// TODO: convert this file so typechecking functions return
// Result<TypedExpression, SomeKindaError>. The file is short enough
// that this will be a good test for the parser.

use std::collections::{HashMap, HashSet};

use crate::CompileError;

use crate::ast::{
    BinaryOperator, BlockItem, CaseInfo, CompoundOperator, Const, Crement, Declaration, Expression,
    Fixity, ForInit, Function, Program, Statement, StorageClass, Type, UnaryOperator, Var,
};

use crate::interner::Symbol;

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
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum Attrs {
    Fun { defined: bool, global: bool },
    Static { init: InitValue, global: bool },
    Local,
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum InitValue {
    Tentative,
    Initial(StaticInit),
    NoInit,
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum StaticInit {
    Int(i32),
    Long(i64),
    UInt(u32),
    ULong(u64),
}

pub struct TypeChecker {
    symbols: HashMap<Symbol, (Type, Attrs)>,
}

pub type CheckResult = (Program<TypedExpression>, HashMap<Symbol, (Type, Attrs)>);

impl TypeChecker {
    pub fn check_program(
        program: Vec<Declaration<Expression>>,
    ) -> Result<CheckResult, CompileError> {
        //todo define a type alias here
        let mut type_checker = TypeChecker {
            symbols: HashMap::new(),
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
                        name
                    )));
                }
                if *defined && body.is_some() {
                    return Err(CompileError::Check(format!(
                        "Duplicate definition of function {}",
                        name
                    )));
                }
                already_defined = *defined;
                if *old_global && storage == Some(StorageClass::Static) {
                    return Err(CompileError::Check(format!(
                        "Static function declaration {} follows non-static",
                        name
                    )));
                }
                global = *old_global;
            } else {
                return Err(CompileError::Check(format!(
                    "Function {} already defined as variable",
                    name
                )));
            }
        }
        let attrs = Attrs::Fun {
            defined: body.is_some() || already_defined,
            global,
        };

        self.symbols.insert(name, (ty.clone(), attrs));

        let Type::Fun(_, ret_ty) = ty.clone() else {
            unreachable!("")
        };

        let body = match body {
            Some(block_items) => {
                for (param, param_ty) in &params {
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
                let cast_expr = cast(typed_expr, current_fn);
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
                let body = Box::new(self.check_statement(current_fn, *body)?);
                let cases = if *get_type(&expr) == Type::Long {
                    convert_long_cases(&cases)
                } else {
                    convert_int_cases(&cases)?
                };
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
            Some(Expression::Constant(Const::Int(n))) => InitValue::Initial(StaticInit::Int(n)),
            Some(Expression::Constant(Const::Long(n))) => InitValue::Initial(StaticInit::Long(n)),
            Some(Expression::Constant(Const::UInt(n))) => InitValue::Initial(StaticInit::UInt(n)),
            Some(Expression::Constant(Const::ULong(n))) => InitValue::Initial(StaticInit::ULong(n)),
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
                    name
                )));
            }
        };

        let mut global = storage != Some(StorageClass::Static);
        match self.symbols.get(&name) {
            Some((declared_ty, _)) if *declared_ty != ty => {
                return Err(CompileError::Check(format!(
                    "Conflicting declaration of variable {}",
                    name
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
                        name
                    )));
                }
                if let InitValue::Initial(_) = old_init {
                    if let InitValue::Initial(_) = init_attr {
                        return Err(CompileError::Check(format!(
                            "Conflicting file scope definitions of variable {}",
                            name
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
                    Some(expr) => Some(cast(self.check_expr(expr)?, &ty)),
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
                        name
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
                        _ => StaticInit::Int(0),
                    },
                    Some(Expression::Constant(n)) => Self::compile_time_cast(n, ty.clone()),
                    _ => {
                        return Err(CompileError::Check(format!(
                            "Non-constant initialization of variable {}",
                            name
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

    // Convert a static initializer to its declared type at compile time.
    // This is something of a monstrosity and when I'm feeling less dumb I'll
    // do it a better way.
    fn compile_time_cast(init: Const, declared_ty: Type) -> StaticInit {
        match (init, declared_ty) {
            (Const::Int(n), Type::Int) => StaticInit::Int(n),
            (Const::Long(n), Type::Long) => StaticInit::Long(n),
            (Const::Int(n), Type::Long) => StaticInit::Long(n as i64),
            (Const::Long(n), Type::Int) => StaticInit::Int(n as i32),
            _ => unreachable!("Non-constant type for static variable"),
        }
    }

    fn check_redeclaration(&self, ty: &Type, name: &Symbol) -> Result<(), CompileError> {
        if let Some((declared_ty, _)) = self.symbols.get(name)
            && *declared_ty != *ty
        {
            return Err(CompileError::Check(format!(
                "Variable {} originally declared as {:?} and redeclared as {:?}",
                name, declared_ty, ty
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
                    name
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
                        id
                    )));
                }
                TypedExpression::Var(v_type, id)
            }
            Expression::Constant(n) => TypedExpression::Constant(const_type(&n), n),
            Expression::Cast(ty, expr) => {
                let typed_expr = self.check_expr(*expr)?;
                TypedExpression::Cast(ty, Box::new(typed_expr))
            }
            Expression::Unary(unop, expr) => {
                let typed_expr = self.check_expr(*expr)?;
                let ty = match unop {
                    UnaryOperator::Not => Type::Int,
                    _ => get_type(&typed_expr).clone(),
                };
                TypedExpression::Unary(ty, unop, Box::new(typed_expr))
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
                let typed_lhs = self.check_expr(*lhs)?;
                let typed_rhs = self.check_expr(*rhs)?;
                let left_type = get_type(&typed_lhs);
                let cast_rhs = cast(typed_rhs, left_type);

                TypedExpression::Assign(left_type.clone(), Box::new(typed_lhs), Box::new(cast_rhs))
            }
            Expression::Compound(op, lhs, rhs) => {
                // Desugar compound expressions into an assignment and
                // a binary op. Example: x += 1 desugars to x = x +
                // 1. This is a safe translation because the variable
                // analysis pass has already determined that the LHS
                // of the expression is a variable, and evaluating a
                // variable twice in the rewritten expression doesn't
                // affect the result.

                // Desugaring instead of a straight translation into
                // the typed AST makes this easier because we can
                // separate the LHS type and the RHS type here, while
                // we can't while generating TACKY.
                let typed_lhs = self.check_expr(*lhs)?;
                let typed_rhs = self.check_expr(*rhs)?;

                let lhs_ty = get_type(&typed_lhs).clone();
                let rhs_ty = get_type(&typed_rhs).clone();

                let rhs_expr = match (&lhs_ty, &rhs_ty) {
                    // Variable is a long and operand is an
                    // int. Convert the operand to a long and do the
                    // operation.
                    (Type::Long, Type::Int) => TypedExpression::Binary(
                        Type::Long,
                        convert_compound_op(op),
                        Box::new(typed_lhs.clone()),
                        Box::new(TypedExpression::Cast(
                            Type::Long,
                            Box::new(typed_rhs.clone()),
                        )),
                    ),
                    // Variable is an int and operand is a
                    // long. Convert both to longs, do the operation,
                    // then truncate the result to int.
                    (Type::Int, Type::Long) => TypedExpression::Cast(
                        Type::Int,
                        Box::new(TypedExpression::Binary(
                            Type::Long,
                            convert_compound_op(op),
                            Box::new(TypedExpression::Cast(
                                Type::Long,
                                Box::new(typed_lhs.clone()),
                            )),
                            Box::new(typed_rhs),
                        )),
                    ),
                    // No type conversions
                    _ => TypedExpression::Binary(
                        rhs_ty.clone(), // Both types are the same, doesn't matter which we pick
                        convert_compound_op(op),
                        Box::new(typed_lhs.clone()),
                        Box::new(typed_rhs),
                    ),
                };
                TypedExpression::Assign(lhs_ty, Box::new(typed_lhs.clone()), Box::new(rhs_expr))
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
                                name,
                                param_tys.len(),
                                params.len()
                            )));
                        }
                        let mut converted_params = Vec::with_capacity(params.len());
                        for (param_ty, param) in param_tys.iter().zip(params) {
                            let typed_param = self.check_expr(param)?;
                            converted_params.push(cast(typed_param, param_ty));
                        }
                        TypedExpression::Call(*ret_ty, name, converted_params)
                    }
                    _ => {
                        return Err(CompileError::Check(format!(
                            "Variable {} used as function name",
                            name
                        )));
                    }
                }
            }
            Expression::Conditional(cond, if_expr, else_expr) => {
                let typed_cond = self.check_expr(*cond)?;
                let typed_if = self.check_expr(*if_expr)?;
                let typed_else = self.check_expr(*else_expr)?;

                let ty = get_common_type(get_type(&typed_if), get_type(&typed_else))?;
                TypedExpression::Conditional(
                    ty.clone(),
                    Box::new(typed_cond),
                    Box::new(cast(typed_if, &ty)),
                    Box::new(cast(typed_else, &ty)),
                )
            }
        })
    }
}

fn convert_compound_op(compound_op: CompoundOperator) -> BinaryOperator {
    match compound_op {
        CompoundOperator::Add => BinaryOperator::Add,
        CompoundOperator::Subtract => BinaryOperator::Subtract,
        CompoundOperator::Multiply => BinaryOperator::Multiply,
        CompoundOperator::Divide => BinaryOperator::Divide,
        CompoundOperator::Remainder => BinaryOperator::Remainder,
        CompoundOperator::BitAnd => BinaryOperator::BitAnd,
        CompoundOperator::BitOr => BinaryOperator::BitOr,
        CompoundOperator::BitXOr => BinaryOperator::BitXOr,
        CompoundOperator::ShiftLeft => BinaryOperator::ShiftLeft,
        CompoundOperator::ShiftRight => BinaryOperator::ShiftRight,
    }
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
    }
}

// Return size in bytes of the given type
pub fn size(t: &Type) -> Result<u8, CompileError> {
    Ok(match t {
        Type::Int => 4,
        Type::UInt => 4,
        Type::Long => 8,
        Type::ULong => 8,
        _ => {
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
fn get_common_type(t1: &Type, t2: &Type) -> Result<Type, CompileError> {
    if t1 == t2 {
        return Ok(t1.clone());
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
// If a switch statement's controlling expression is a long, convert all its cases to longs
fn convert_long_cases(cases: &Vec<CaseInfo>) -> Vec<CaseInfo> {
    let mut new_cases = Vec::with_capacity(cases.len());
    for case in cases {
        if let CaseInfo::Case {
            expr: Const::Int(n),
            label,
        } = case
        {
            new_cases.push(CaseInfo::Case {
                expr: Const::Long(*n as i64),
                label: *label,
            });
        } else {
            new_cases.push(case.clone());
        }
    }
    new_cases
}

// If a switch statement's controlling expression is an int, convert all its cases to ints, truncating them
fn convert_int_cases(cases: &Vec<CaseInfo>) -> Result<Vec<CaseInfo>, CompileError> {
    let mut new_cases = Vec::with_capacity(cases.len());
    // Check for duplicate values after conversions
    let mut values = HashSet::with_capacity(cases.len());
    for case in cases {
        match case {
            CaseInfo::Case {
                expr: Const::Long(n),
                label,
            } => {
                if values.contains(&(*n as i32)) {
                    return Err(CompileError::Check(format!(
                        "Duplicate case value after conversion {n}"
                    )));
                } else {
                    values.insert(*n as i32);
                }
                new_cases.push(CaseInfo::Case {
                    expr: Const::Int(*n as i32),
                    label: *label,
                });
            }
            CaseInfo::Case {
                expr: Const::Int(n),
                ..
            } => {
                if values.contains(n) {
                    return Err(CompileError::Check(format!(
                        "Duplicate case value after conversion {n}"
                    )));
                } else {
                    values.insert(*n);
                }
                new_cases.push(case.clone());
            }
            _ => new_cases.push(case.clone()),
        }
    }
    Ok(new_cases)
}
