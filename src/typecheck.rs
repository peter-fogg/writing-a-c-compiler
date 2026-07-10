// TODO: convert this file so typechecking functions return
// Result<TypedExpression, SomeKindaError>. The file is short enough
// that this will be a good test for the parser.

use std::collections::HashMap;

use crate::CompileError;

use crate::ast::{
    BinaryOperator, BlockItem, CaseInfo, Const, Crement, Declaration, Expression, Fixity, ForInit,
    Function, Program, Statement, StorageClass, Type, UnaryOperator, Var,
};
use crate::interner::{Interner, Symbol};
use crate::semantic_analysis::actual_case_value;

#[derive(Debug, PartialEq, Clone)]
pub enum TypedExpression {
    Constant(Type, Const),
    Unary(Type, UnaryOperator, Box<TypedExpression>),
    Binary(
        Type,
        BinaryOperator,
        Box<TypedExpression>,
        Box<TypedExpression>,
    ),
    Compound(
        Type,
        BinaryOperator,
        Box<TypedExpression>,
        Box<TypedExpression>,
    ),
    Crement(Type, Fixity, Crement, Box<TypedExpression>),
    Var(Type, Symbol),
    Assign(Type, Box<TypedExpression>, Box<TypedExpression>),
    Conditional(
        Type,
        Box<TypedExpression>,
        Box<TypedExpression>,
        Box<TypedExpression>,
    ),
    Call(Type, Symbol, Vec<TypedExpression>),
    Cast(Type, Box<TypedExpression>),
    Deref(Type, Box<TypedExpression>),
    AddrOf(Type, Box<TypedExpression>),
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Attrs {
    Fun { defined: bool, global: bool },
    Static { init: InitValue, global: bool },
    Local,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum InitValue {
    Tentative,
    Initial(StaticInit),
    NoInit,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum StaticInit {
    Int(i32),
    Long(i64),
    UInt(u32),
    ULong(u64),
    Double(f64),
}

pub struct TypeChecker<'a> {
    symbols: HashMap<Symbol, (Type, Attrs)>,
    interner: &'a Interner,
}

pub type CheckResult = (Program<TypedExpression>, HashMap<Symbol, (Type, Attrs)>);

impl<'a> TypeChecker<'a> {
    pub fn check_program(
        program: Vec<Declaration<Expression>>,
        interner: &'a Interner,
    ) -> Result<CheckResult, CompileError> {
        //todo define a type alias here
        let mut type_checker = TypeChecker {
            symbols: HashMap::new(),
            interner,
        };

        let mut typed_decls = Vec::with_capacity(program.len());
        for declaration in program {
            typed_decls.push(match declaration {
                Declaration::Func(function) => {
                    Declaration::Func(type_checker.check_function_decl(function)?)
                }
                Declaration::Var(var) => Declaration::Var(type_checker.check_file_var_decl(var)?),
            });
        }

        Ok((Program(typed_decls), type_checker.symbols))
    }

    fn check_function_decl(
        &mut self,
        Function {
            name,
            params,
            body,
            storage,
            ty,
        }: Function<Expression>,
    ) -> Result<Function<TypedExpression>, CompileError> {
        let mut already_defined = false;
        let mut global = storage != Some(StorageClass::Static);
        if let Some(old_decl) = self.symbols.get(&name) {
            // If this function is already declared
            if let (
                fun_type @ Type::Fun(_, _), // And it's declared as a function
                Attrs::Fun {
                    defined,
                    global: old_global,
                },
            ) = old_decl
            {
                if ty != *fun_type {
                    return Err(CompileError::Check(format!(
                        "Function {} redeclared with different type",
                        self.interner.get_symbol(name)
                    )));
                }
                if *defined && body.is_some() {
                    return Err(CompileError::Check(format!(
                        "Duplicate definition of function {}",
                        self.interner.get_symbol(name)
                    )));
                }
                already_defined = *defined;
                if *old_global && storage == Some(StorageClass::Static) {
                    return Err(CompileError::Check(format!(
                        "Static function declaration {} follows non-static",
                        self.interner.get_symbol(name)
                    )));
                }
                global = *old_global;
            } else {
                return Err(CompileError::Check(format!(
                    "Function {} already defined as variable",
                    self.interner.get_symbol(name)
                )));
            }
        }
        let attrs = Attrs::Fun {
            defined: body.is_some() || already_defined,
            global,
        };

        self.symbols.insert(name, (ty.clone(), attrs));

        let Type::Fun(param_tys, ret_ty) = ty.clone() else {
            unreachable!("")
        };

        let body = match body {
            Some(block_items) => {
                for (param, param_ty) in params.iter().zip(param_tys) {
                    self.symbols
                        .insert(*param, (param_ty.clone(), Attrs::Local));
                }
                Some(self.check_block(&ret_ty, block_items)?)
            }
            None => None,
        };

        Ok(Function {
            name,
            ty,
            params,
            body,
            storage,
        })
    }

    fn check_block(
        &mut self,
        current_fn: &Type,
        block_items: Vec<BlockItem<Expression>>,
    ) -> Result<Vec<BlockItem<TypedExpression>>, CompileError> {
        let mut typed_block_items = Vec::with_capacity(block_items.len());
        for block_item in block_items {
            typed_block_items.push(match block_item {
                BlockItem::D(decl) => BlockItem::D(match decl {
                    Declaration::Var(var) => Declaration::Var(self.check_block_var_decl(var)?),
                    Declaration::Func(func) => Declaration::Func(self.check_function_decl(func)?),
                }),
                BlockItem::S(stmt) => BlockItem::S(self.check_statement(current_fn, stmt)?),
            });
        }
        Ok(typed_block_items)
    }

    fn check_statement(
        &mut self,
        current_fn: &Type,
        stmt: Statement<Expression>,
    ) -> Result<Statement<TypedExpression>, CompileError> {
        Ok(match stmt {
            Statement::Return(expr) => {
                let typed_expr = self.check_expr(expr)?;
                let cast_expr = convert_by_assignment(typed_expr, current_fn)?;
                Statement::Return(cast_expr)
            }
            Statement::Exp(expr) => Statement::Exp(self.check_expr(expr)?),
            Statement::If(cond, if_stmt, else_stmt) => {
                let typed_cond = self.check_expr(cond)?;
                let typed_if = self.check_statement(current_fn, *if_stmt)?;
                let typed_else = match else_stmt {
                    Some(stmt) => Some(Box::new(self.check_statement(current_fn, *stmt)?)),
                    None => None,
                };
                Statement::If(typed_cond, Box::new(typed_if), typed_else)
            }
            Statement::Goto(label) => Statement::Goto(label),
            Statement::Label(label, stmt) => {
                Statement::Label(label, Box::new(self.check_statement(current_fn, *stmt)?))
            }
            Statement::Compound(block_items) => {
                Statement::Compound(self.check_block(current_fn, block_items)?)
            }
            Statement::Break(label) => Statement::Break(label),
            Statement::Continue(label) => Statement::Continue(label),
            Statement::For(label, for_init, cond, post, body) => {
                let typed_init = self.check_for_init(for_init)?;
                let typed_cond = match cond {
                    Some(expr) => Some(self.check_expr(expr)?),
                    None => None,
                };
                let typed_post = match post {
                    Some(expr) => Some(self.check_expr(expr)?),
                    None => None,
                };
                let typed_body = self.check_statement(current_fn, *body)?;
                Statement::For(
                    label,
                    typed_init,
                    typed_cond,
                    typed_post,
                    Box::new(typed_body),
                )
            }
            Statement::While(label, cond, body) => {
                let typed_body = self.check_statement(current_fn, *body)?;
                let typed_cond = self.check_expr(cond)?;
                Statement::While(label, typed_cond, Box::new(typed_body))
            }
            Statement::DoWhile(label, body, cond) => {
                let typed_body = self.check_statement(current_fn, *body)?;
                let typed_cond = self.check_expr(cond)?;
                Statement::DoWhile(label, Box::new(typed_body), typed_cond)
            }
            Statement::Switch {
                expr,
                body,
                label,
                cases,
            } => {
                let expr = self.check_expr(expr)?;
                let expr_ty = get_type(&expr);
                if *expr_ty == Type::Double {
                    return Err(CompileError::Check(String::from(
                        "can't use a double as switch expression",
                    )));
                }
                let body = Box::new(self.check_statement(current_fn, *body)?);
                let cases = convert_cases(&cases, expr_ty)?;
                Statement::Switch {
                    expr,
                    body,
                    label,
                    cases,
                }
            }
            Statement::Case(label, expr, stmt) => {
                let typed_expr = self.check_expr(expr)?;
                let typed_stmt = self.check_statement(current_fn, *stmt)?;
                Statement::Case(label, typed_expr, Box::new(typed_stmt))
            }
            Statement::Default(label, stmt) => {
                Statement::Default(label, Box::new(self.check_statement(current_fn, *stmt)?))
            }
            Statement::Null => Statement::Null,
        })
    }

    fn check_file_var_decl(
        &mut self,
        Var {
            name,
            init,
            storage,
            ty,
        }: Var<Expression>,
    ) -> Result<Var<TypedExpression>, CompileError> {
        let mut init_attr = match init {
            Some(Expression::Constant(const_init)) => {
                InitValue::Initial(cast_init(const_init, &ty)?)
            }
            None => {
                if storage == Some(StorageClass::Extern) {
                    InitValue::NoInit
                } else {
                    InitValue::Tentative
                }
            }
            _ => {
                return Err(CompileError::Check(format!(
                    "Non-constant initialization of variable {}",
                    self.interner.get_symbol(name)
                )));
            }
        };

        let mut global = storage != Some(StorageClass::Static);
        match self.symbols.get(&name) {
            Some((declared_ty, _)) if *declared_ty != ty => {
                return Err(CompileError::Check(format!(
                    "Conflicting declaration of variable {}",
                    self.interner.get_symbol(name)
                )));
            }
            Some((
                _,
                Attrs::Static {
                    init: old_init,
                    global: old_global,
                },
            )) => {
                if storage == Some(StorageClass::Extern) {
                    global = *old_global;
                } else if *old_global != global {
                    return Err(CompileError::Check(format!(
                        "Conflicting linkage of variable {}",
                        self.interner.get_symbol(name)
                    )));
                }
                if let InitValue::Initial(_) = old_init {
                    if let InitValue::Initial(_) = init_attr {
                        return Err(CompileError::Check(format!(
                            "Conflicting file scope definitions of variable {}",
                            self.interner.get_symbol(name)
                        )));
                    }
                    init_attr = *old_init;
                } else if *old_init == InitValue::Tentative
                    && !matches!(init_attr, InitValue::Initial(_))
                {
                    init_attr = InitValue::Tentative;
                }
            }
            _ => (),
        }
        self.symbols.insert(
            name,
            (
                ty.clone(),
                Attrs::Static {
                    init: init_attr,
                    global,
                },
            ),
        );
        let typed_init = match init {
            Some(expr) => Some(self.check_expr(expr)?),
            None => None,
        };
        Ok(Var {
            name,
            storage,
            ty,
            init: typed_init,
        })
    }

    fn check_block_var_decl(
        &mut self,
        Var {
            name,
            init,
            storage,
            ty,
        }: Var<Expression>,
    ) -> Result<Var<TypedExpression>, CompileError> {
        match storage {
            // No storage class means this is a local variable
            None => {
                // Check for an earlier declaration with a different type
                self.check_redeclaration(&ty, &name)?;
                // Insert the type as declared
                self.symbols.insert(name, (ty.clone(), Attrs::Local));

                // Typecheck the init expr if it exists and convert it to the appropriate type

                let typed_init = match init {
                    Some(expr) => Some(convert_by_assignment(self.check_expr(expr)?, &ty)?),
                    None => None,
                };

                Ok(Var {
                    name,
                    init: typed_init,
                    storage,
                    ty,
                })
            }
            Some(StorageClass::Extern) => {
                // Extern variable cannot be initialized because they live in some other translation unit
                if init.is_some() {
                    return Err(CompileError::Check(format!(
                        "Initializer on local extern declaration {}",
                        self.interner.get_symbol(name)
                    )));
                }
                // Check for an earlier declaration with a different type
                if let std::collections::hash_map::Entry::Vacant(e) = self.symbols.entry(name) {
                    e.insert((
                        ty.clone(),
                        Attrs::Static {
                            init: InitValue::NoInit,
                            global: true,
                        },
                    ));
                } else {
                    self.check_redeclaration(&ty, &name)?;
                }
                Ok(Var {
                    name,
                    init: None,
                    storage,
                    ty,
                })
            }
            Some(StorageClass::Static) => {
                // Determine the initializer attribute
                let init_attr = match init {
                    None => match ty {
                        Type::Long => StaticInit::Long(0),
                        Type::Pointer(_) => StaticInit::ULong(0),
                        _ => StaticInit::Int(0),
                    },
                    Some(Expression::Constant(n)) => cast_init(n, &ty)?,
                    _ => {
                        return Err(CompileError::Check(format!(
                            "Non-constant initialization of variable {}",
                            self.interner.get_symbol(name)
                        )));
                    }
                };

                self.symbols.insert(
                    name,
                    (
                        ty.clone(),
                        Attrs::Static {
                            init: InitValue::Initial(init_attr),
                            global: false,
                        },
                    ),
                );
                let typed_init = match init {
                    Some(expr) => Some(self.check_expr(expr)?),
                    None => None,
                };
                Ok(Var {
                    name,
                    init: typed_init,
                    storage,
                    ty,
                })
            }
        }
    }

    fn check_redeclaration(&self, ty: &Type, name: &Symbol) -> Result<(), CompileError> {
        if let Some((declared_ty, _)) = self.symbols.get(name)
            && *declared_ty != *ty
        {
            return Err(CompileError::Check(format!(
                "Variable {} originally declared as {:?} and redeclared as {:?}",
                self.interner.get_symbol(*name),
                declared_ty,
                ty
            )));
        }
        Ok(())
    }

    fn check_for_init(
        &mut self,
        for_init: ForInit<Expression>,
    ) -> Result<ForInit<TypedExpression>, CompileError> {
        Ok(match for_init {
            ForInit::Decl(Var {
                storage: Some(StorageClass::Static),
                name,
                ..
            }) => {
                return Err(CompileError::Check(format!(
                    "Static initializer {} in for loop",
                    self.interner.get_symbol(name)
                )));
            }
            ForInit::Decl(var) => ForInit::Decl(self.check_block_var_decl(var)?),
            ForInit::Exp(expr) => ForInit::Exp(self.check_expr(expr)?),
            ForInit::Null => ForInit::Null,
        })
    }

    // Return a new typed expression after checking this expression.
    fn check_expr(&mut self, expr: Expression) -> Result<TypedExpression, CompileError> {
        Ok(match expr {
            Expression::Var(id) => {
                let v_type = self.symbols.get(&id).unwrap().0.clone(); // unwrap() is safe because we've already resolved symbols
                if let Type::Fun(_, _) = v_type {
                    return Err(CompileError::Check(format!(
                        "Function {} used as variable",
                        self.interner.get_symbol(id)
                    )));
                }
                TypedExpression::Var(v_type, id)
            }
            Expression::Constant(n) => TypedExpression::Constant(const_type(&n), n),
            Expression::Cast(ty, expr) => {
                let typed_expr = self.check_expr(*expr)?;
                if matches!(
                    (&ty, get_type(&typed_expr)),
                    (Type::Double, Type::Pointer(_)) | (Type::Pointer(_), Type::Double)
                ) {
                    return Err(CompileError::Check(format!(
                        "Can't convert {typed_expr:?} to {ty:?}"
                    )));
                }
                TypedExpression::Cast(ty, Box::new(typed_expr))
            }
            Expression::Unary(unop, expr) => {
                let typed_expr = self.check_expr(*expr)?;
                if unop == UnaryOperator::Complement && *get_type(&typed_expr) == Type::Double {
                    return Err(CompileError::Check(String::from(
                        "can't take the bitwise complement of a double",
                    )));
                }
                if matches!(unop, UnaryOperator::Complement | UnaryOperator::Negate)
                    && matches!(get_type(&typed_expr), Type::Pointer(_))
                {
                    return Err(CompileError::Check(format!(
                        "can't apply {unop:?} to pointer type {typed_expr:?}"
                    )));
                }
                let ty = match unop {
                    UnaryOperator::Not => Type::Int,
                    _ => get_type(&typed_expr).clone(),
                };
                TypedExpression::Unary(ty, unop, Box::new(typed_expr))
            }
            Expression::Binary(
                eq_op @ (BinaryOperator::Equal | BinaryOperator::NotEqual),
                lhs,
                rhs,
            ) => {
                let typed_lhs = self.check_expr(*lhs)?;
                let typed_rhs = self.check_expr(*rhs)?;

                let left_type = get_type(&typed_lhs);
                let right_type = get_type(&typed_rhs);

                let common_type = if is_pointer_type(left_type) || is_pointer_type(right_type) {
                    get_common_pointer_type(&typed_lhs, &typed_rhs)?
                } else {
                    get_common_type(left_type, right_type)?
                };

                let cast_lhs = cast(typed_lhs, &common_type);
                let cast_rhs = cast(typed_rhs, &common_type);

                TypedExpression::Binary(Type::Int, eq_op, Box::new(cast_lhs), Box::new(cast_rhs))
            }
            Expression::Binary(binop, lhs, rhs) => {
                let typed_lhs = self.check_expr(*lhs)?;
                let typed_rhs = self.check_expr(*rhs)?;
                if matches!(binop, BinaryOperator::And | BinaryOperator::Or) {
                    return Ok(TypedExpression::Binary(
                        Type::Int,
                        binop,
                        Box::new(typed_lhs),
                        Box::new(typed_rhs),
                    ));
                }

                let left_ty = get_type(&typed_lhs);
                let ty = get_common_type(get_type(&typed_lhs), get_type(&typed_rhs))?;

                check_double_binop(binop, &ty)?;
                check_pointer_binop(binop, &ty)?;

                let cast_lhs = cast(typed_lhs.clone(), &ty);
                let cast_rhs = cast(typed_rhs.clone(), &ty);

                match binop {
                    BinaryOperator::Add
                    | BinaryOperator::Subtract
                    | BinaryOperator::Multiply
                    | BinaryOperator::Divide
                    | BinaryOperator::Remainder
                    | BinaryOperator::BitAnd
                    | BinaryOperator::BitOr
                    | BinaryOperator::BitXOr => {
                        let ty = get_common_type(get_type(&typed_lhs), get_type(&typed_rhs))?;
                        TypedExpression::Binary(ty, binop, Box::new(cast_lhs), Box::new(cast_rhs))
                    }
                    BinaryOperator::ShiftLeft | BinaryOperator::ShiftRight => {
                        TypedExpression::Binary(
                            left_ty.clone(),
                            binop,
                            Box::new(typed_lhs),
                            Box::new(typed_rhs),
                        )
                    }
                    _ => TypedExpression::Binary(
                        Type::Int,
                        binop,
                        Box::new(cast_lhs),
                        Box::new(cast_rhs),
                    ),
                }
            }
            Expression::Assign(lhs, rhs) => {
                // The LHS is guaranteed to be an lvalue after variable resolution
                let typed_lhs = self.check_expr(*lhs)?;
                let typed_rhs = self.check_expr(*rhs)?;
                let left_type = get_type(&typed_lhs);
                let cast_rhs = convert_by_assignment(typed_rhs, left_type)?;

                TypedExpression::Assign(left_type.clone(), Box::new(typed_lhs), Box::new(cast_rhs))
            }
            Expression::Compound(binop, lhs, rhs) => {
                let typed_lhs = self.check_expr(*lhs)?;
                let typed_rhs = self.check_expr(*rhs)?;

                let left_ty = get_type(&typed_lhs);
                let right_ty = get_type(&typed_rhs);

                let common_ty = if is_pointer_type(left_ty) || is_pointer_type(right_ty) {
                    get_common_pointer_type(&typed_lhs, &typed_rhs)?
                } else {
                    get_common_type(left_ty, right_ty)?
                };

                check_double_binop(binop, &common_ty)?;
                check_pointer_binop(binop, &common_ty)?;

                let cast_rhs = cast(typed_rhs.clone(), &common_ty);

                TypedExpression::Compound(
                    left_ty.clone(),
                    binop,
                    Box::new(typed_lhs),
                    Box::new(cast_rhs),
                )
            }
            Expression::Crement(fixity, crement, expr) => {
                let typed_expr = self.check_expr(*expr)?;
                let ty = get_type(&typed_expr);
                TypedExpression::Crement(ty.clone(), fixity, crement, Box::new(typed_expr))
            }
            Expression::Call(name, params) => {
                let fn_type = self.symbols.get(&name).unwrap().0.clone(); // unwrap() is safe because we've already resolved symbols
                match fn_type {
                    Type::Fun(param_tys, ret_ty) => {
                        if param_tys.len() != params.len() {
                            return Err(CompileError::Check(format!(
                                "Function {} expected {} params and got {}",
                                self.interner.get_symbol(name),
                                param_tys.len(),
                                params.len()
                            )));
                        }
                        let mut converted_params = Vec::with_capacity(params.len());
                        for (param_ty, param) in param_tys.iter().zip(params) {
                            let typed_param = self.check_expr(param)?;
                            converted_params.push(convert_by_assignment(typed_param, param_ty)?);
                        }
                        TypedExpression::Call(*ret_ty, name, converted_params)
                    }
                    _ => {
                        return Err(CompileError::Check(format!(
                            "Variable {} used as function name",
                            self.interner.get_symbol(name)
                        )));
                    }
                }
            }
            Expression::Conditional(cond, if_expr, else_expr) => {
                let typed_cond = self.check_expr(*cond)?;
                let typed_if = self.check_expr(*if_expr)?;
                let typed_else = self.check_expr(*else_expr)?;

                let if_type = get_type(&typed_if);
                let else_type = get_type(&typed_else);

                let common_type = if is_pointer_type(if_type) || is_pointer_type(else_type) {
                    get_common_pointer_type(&typed_if, &typed_else)?
                } else {
                    get_common_type(if_type, else_type)?
                };

                let cast_if = cast(typed_if, &common_type);
                let cast_else = cast(typed_else, &common_type);

                TypedExpression::Conditional(
                    common_type,
                    Box::new(typed_cond),
                    Box::new(cast_if),
                    Box::new(cast_else),
                )
            }
            Expression::Deref(expr) => {
                let typed_expr = self.check_expr(*expr)?;
                match get_type(&typed_expr) {
                    Type::Pointer(ref_ty) => {
                        TypedExpression::Deref(*ref_ty.clone(), Box::new(typed_expr))
                    }

                    ty => {
                        return Err(CompileError::Check(format!(
                            "Can't dereference non-pointer type {ty:?}"
                        )));
                    }
                }
            }
            Expression::AddrOf(expr) => {
                if is_lvalue(&expr) {
                    let typed_expr = self.check_expr(*expr)?;
                    let ty = get_type(&typed_expr);
                    TypedExpression::AddrOf(
                        Type::Pointer(Box::new(ty.clone())),
                        Box::new(typed_expr),
                    )
                } else {
                    return Err(CompileError::Check(format!(
                        "Can't take address of non-lvalue {expr:?}"
                    )));
                }
            }
        })
    }
}

// lvalues can be assigned to or have their address taken
pub fn is_lvalue(expr: &Expression) -> bool {
    matches!(expr, Expression::Var(_) | Expression::Deref(_))
}

// Arithmetic types are normal numeric types
fn is_arithmetic_type(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Double | Type::Int | Type::UInt | Type::Long | Type::ULong
    )
}

fn is_pointer_type(ty: &Type) -> bool {
    matches!(ty, Type::Pointer(_))
}

// Pointers can only be compared with integral-type zeroes
fn is_null_pointer(expr: &TypedExpression) -> bool {
    matches!(
        expr,
        TypedExpression::Constant(
            _,
            Const::Int(0) | Const::UInt(0) | Const::Long(0) | Const::ULong(0)
        )
    )
}

// Return the type of a known-typed expression. Simple pattern
// match.
pub fn get_type(expr: &TypedExpression) -> &Type {
    match expr {
        TypedExpression::Assign(ty, _, _) => ty,
        TypedExpression::Binary(ty, _, _, _) => ty,
        TypedExpression::Call(ty, _, _) => ty,
        TypedExpression::Cast(ty, _) => ty,
        TypedExpression::Conditional(ty, _, _, _) => ty,
        TypedExpression::Constant(ty, _) => ty,
        TypedExpression::Crement(ty, _, _, _) => ty,
        TypedExpression::Unary(ty, _, _) => ty,
        TypedExpression::Var(ty, _) => ty,
        TypedExpression::AddrOf(ty, _) => ty,
        TypedExpression::Deref(ty, _) => ty,
        TypedExpression::Compound(ty, _, _, _) => ty,
    }
}

// Returns the type of a constant. We know this info by parsing
// it.
fn const_type(c: &Const) -> Type {
    match c {
        Const::Int(_) => Type::Int,
        Const::Long(_) => Type::Long,
        Const::UInt(_) => Type::UInt,
        Const::ULong(_) => Type::ULong,
        Const::Double(_) => Type::Double,
    }
}

fn check_double_binop(binop: BinaryOperator, ty: &Type) -> Result<(), CompileError> {
    if matches!(
        binop,
        BinaryOperator::Remainder
            | BinaryOperator::BitAnd
            | BinaryOperator::BitOr
            | BinaryOperator::BitXOr
            | BinaryOperator::ShiftLeft
            | BinaryOperator::ShiftRight
    ) && *ty == Type::Double
    {
        Err(CompileError::Check(format!(
            "invalid operation {:?} with double",
            binop,
        )))
    } else {
        Ok(())
    }
}

fn check_pointer_binop(binop: BinaryOperator, ty: &Type) -> Result<(), CompileError> {
    if matches!(
        binop,
        BinaryOperator::BitAnd
            | BinaryOperator::BitOr
            | BinaryOperator::BitXOr
            | BinaryOperator::ShiftLeft
            | BinaryOperator::ShiftRight
            | BinaryOperator::Divide
            | BinaryOperator::Multiply
            | BinaryOperator::Remainder
    ) && is_pointer_type(ty)
    {
        Err(CompileError::Check(format!(
            "invalid operation {binop:?} with pointer",
        )))
    } else {
        Ok(())
    }
}

// Return size in bytes of the given type
pub fn size(t: &Type) -> Result<u8, CompileError> {
    Ok(match t {
        Type::Int | Type::UInt => 4,
        Type::Long | Type::ULong | Type::Double | Type::Pointer(_) => 8,
        Type::Fun(_, _) => {
            return Err(CompileError::Check(String::from(
                "Can't get size of a function type",
            )));
        }
    })
}

pub fn signed(t: &Type) -> bool {
    matches!(t, Type::Int | Type::Long)
}

// Return the usual arithmetic conversion for ensuring expressions
// involving two types have the right result.
pub fn get_common_type(t1: &Type, t2: &Type) -> Result<Type, CompileError> {
    if t1 == t2 {
        return Ok(t1.clone());
    }

    if *t1 == Type::Double || *t2 == Type::Double {
        return Ok(Type::Double);
    }

    if size(t1)? == size(t2)? {
        if signed(t1) {
            return Ok(t2.clone());
        } else {
            return Ok(t1.clone());
        }
    }

    if size(t1)? > size(t2)? {
        Ok(t1.clone())
    } else {
        Ok(t2.clone())
    }
}

// Determine the common pointer type for a comparison. Two pointers
// must have the same type, or one must be a zero.
fn get_common_pointer_type(
    e1: &TypedExpression,
    e2: &TypedExpression,
) -> Result<Type, CompileError> {
    let t1 = get_type(e1);
    let t2 = get_type(e2);
    if t1 == t2 {
        return Ok(t1.clone());
    }
    if is_null_pointer(e1) {
        return Ok(t2.clone());
    }
    if is_null_pointer(e2) {
        return Ok(t1.clone());
    }
    Err(CompileError::Check(format!(
        "Expressions {e1:?} and {e2:?} have incompatible types"
    )))
}

// Return a type converted to another. Either the original type is
// already what we need in which case it just gets returned, or it
// isn't and we cast it appropriately.
fn cast(expr: TypedExpression, ty: &Type) -> TypedExpression {
    if *get_type(&expr) == *ty {
        expr
    } else {
        TypedExpression::Cast(ty.clone(), Box::new(expr))
    }
}

fn convert_by_assignment(
    expr: TypedExpression,
    ty: &Type,
) -> Result<TypedExpression, CompileError> {
    let expr_ty = get_type(&expr);
    if expr_ty == ty {
        Ok(expr)
    } else if (is_arithmetic_type(expr_ty) && is_arithmetic_type(ty))
        || (is_null_pointer(&expr) && is_pointer_type(ty))
    {
        Ok(cast(expr, ty))
    } else {
        Err(CompileError::Check(format!(
            "Can't convert {expr:?} to {ty:?}"
        )))
    }
}

// Convert cases of a switch statement to all have the same type as
// the controlling expression. Additionally check for duplicate cases
// after said conversion
fn convert_cases(cases: &Vec<CaseInfo>, ty: &Type) -> Result<Vec<CaseInfo>, CompileError> {
    let mut new_cases = Vec::with_capacity(cases.len());
    let mut values = Vec::with_capacity(cases.len());
    for case in cases {
        match case {
            CaseInfo::Case { expr, label } => {
                if matches!(expr, Const::Double(_)) {
                    return Err(CompileError::Check(format!("Double case {expr}")));
                }
                let converted = convert_case(expr, ty);
                if values.contains(&converted) {
                    return Err(CompileError::Check(format!(
                        "Duplicate case value after conversion {converted:?}"
                    )));
                }
                values.push(converted);
                new_cases.push(CaseInfo::Case {
                    expr: converted,
                    label: *label,
                })
            }
            c => new_cases.push(c.clone()),
        }
    }

    Ok(new_cases)
}

fn convert_case(expr: &Const, ty: &Type) -> Const {
    match ty {
        Type::Int => Const::Int(actual_case_value(*expr) as i32),
        Type::UInt => Const::UInt(actual_case_value(*expr) as u32),
        Type::Long => Const::Long(actual_case_value(*expr) as i64),
        Type::ULong => Const::ULong(actual_case_value(*expr)),
        Type::Double => unreachable!("double in case expression"),
        Type::Fun(_, _) => unreachable!("function type in case expression"),
        Type::Pointer(_) => unreachable!("pointer type in case expression"),
    }
}

// Convert a constant initializer to its declared type
fn cast_init(const_init: Const, ty: &Type) -> Result<StaticInit, CompileError> {
    Ok(match (const_init, ty) {
        (c, Type::Int) if is_integral(c) => StaticInit::Int(u64_value(c) as i32),
        (c, Type::UInt) if is_integral(c) => StaticInit::UInt(u64_value(c) as u32),
        (c, Type::Long) if is_integral(c) => StaticInit::Long(u64_value(c) as i64),
        (c, Type::ULong) if is_integral(c) => StaticInit::ULong(u64_value(c)),
        (Const::Int(n), Type::Double) => StaticInit::Double(n as f64),
        (Const::UInt(n), Type::Double) => StaticInit::Double(n as f64),
        (Const::Long(n), Type::Double) => StaticInit::Double(n as f64),
        (Const::ULong(n), Type::Double) => StaticInit::Double(n as f64),
        (Const::Double(n), Type::Double) => StaticInit::Double(n),
        (Const::Double(n), Type::Int) => StaticInit::Int(n as i32),
        (Const::Double(n), Type::UInt) => StaticInit::UInt(n as u32),
        (Const::Double(n), Type::Long) => StaticInit::Long(n as i64),
        (Const::Double(n), Type::ULong) => StaticInit::ULong(n as u64),
        (c, Type::Pointer(_)) if is_integral(c) && u64_value(c) == 0 => StaticInit::ULong(0),
        (_, Type::Fun(_, _)) => {
            return Err(CompileError::Check(
                "function type in constant initializer".to_string(),
            ));
        }
        _ => {
            return Err(CompileError::Check(format!(
                "can't convert static initializer {const_init:?} to {ty:?}"
            )));
        }
    })
}

fn u64_value(const_init: Const) -> u64 {
    match const_init {
        Const::Int(n) => n as u64,
        Const::Long(n) => n as u64,
        Const::UInt(n) => n as u64,
        Const::ULong(n) => n,
        Const::Double(n) => n.to_bits(),
    }
}

fn is_integral(const_init: Const) -> bool {
    !matches!(const_init, Const::Double(_))
}
