use std::collections::HashMap;

use crate::interner::Symbol;
use crate::parser::{
    BlockItem, Declaration, Expression, ForInit, Function, Statement, StorageClass, Var,
};

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum Type {
    Int,
    Fun { param_count: u8 },
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
    Initial(i32),
    NoInit,
}

pub struct TypeChecker {
    symbols: HashMap<Symbol, (Type, Attrs)>,
}

impl TypeChecker {
    pub fn check_program(program: &Vec<Declaration>) -> HashMap<Symbol, (Type, Attrs)> {
        let mut type_checker = TypeChecker {
            symbols: HashMap::new(),
        };

        for declaration in program {
            match declaration {
                Declaration::Func(function) => type_checker.check_function_decl(function),
                Declaration::Var(var) => type_checker.check_file_var_decl(var),
            }
        }
        type_checker.symbols
    }

    fn check_function_decl(
        &mut self,
        Function {
            name,
            params,
            body,
            storage,
            ty,
        }: &Function,
    ) {
        let mut already_defined = false;
        let mut global = *storage != Some(StorageClass::Static);
        if let Some(ty) = self.symbols.get(name) {
            if let (
                Type::Fun { param_count },
                Attrs::Fun {
                    defined,
                    global: old_global,
                },
            ) = ty
            {
                if *param_count != params.len() as u8 {
                    panic!(
                        "Incompatible declaration of function {} with first declaration having {} params, second having {}",
                        name,
                        param_count,
                        params.len()
                    );
                }
                if *defined && body.is_some() {
                    panic!("Duplicate definition of function {}", name);
                }
                already_defined = *defined;
                if *old_global && *storage == Some(StorageClass::Static) {
                    panic!("Static function declaration {} follows non-static", name);
                }
                global = *old_global;
            } else {
                panic!("Function {} already defined as variable", name);
            }
        }
        let fun_type = Type::Fun {
            param_count: params.len() as u8,
        };
        let attrs = Attrs::Fun {
            defined: body.is_some() || already_defined,
            global,
        };

        self.symbols.insert(*name, (fun_type, attrs));

        if let Some(block_items) = body {
            for param in params {
                self.symbols.insert(*param, (Type::Int, Attrs::Local));
            }
            self.check_block(block_items);
        }
    }

    fn check_block(&mut self, block_items: &Vec<BlockItem>) {
        for block_item in block_items {
            match block_item {
                BlockItem::D(decl) => match decl {
                    Declaration::Var(var) => self.check_block_var_decl(var),
                    Declaration::Func(func) => self.check_function_decl(func),
                },
                BlockItem::S(stmt) => self.check_statement(stmt),
            }
        }
    }

    fn check_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Return(expr) => self.check_expr(expr),
            Statement::Exp(expr) => self.check_expr(expr),
            Statement::If(cond, if_stmt, else_stmt) => {
                self.check_expr(cond);
                self.check_statement(if_stmt);
                if let Some(else_stmt) = else_stmt.as_ref() {
                    self.check_statement(else_stmt)
                }
            }
            Statement::Goto(_) => (),
            Statement::Label(_, stmt) => self.check_statement(stmt),
            Statement::Compound(block_items) => self.check_block(block_items),
            Statement::Break(_) => (),
            Statement::Continue(_) => (),
            Statement::While(_, cond, body) => {
                self.check_expr(cond);
                self.check_statement(body);
            }
            Statement::For(_, for_init, cond, post, body) => {
                self.check_for_init(for_init);
                if let Some(cond) = cond.as_ref() {
                    self.check_expr(cond)
                }
                if let Some(post) = post.as_ref() {
                    self.check_expr(post)
                }
                self.check_statement(body);
            }
            Statement::DoWhile(_, body, cond) => {
                self.check_statement(body);
                self.check_expr(cond);
            }
            Statement::Switch { expr, body, .. } => {
                self.check_expr(expr);
                self.check_statement(body);
            }
            Statement::Case(_, expr, stmt) => {
                self.check_expr(expr);
                self.check_statement(stmt);
            }
            Statement::Default(_, stmt) => self.check_statement(stmt),
            Statement::Null => (),
        }
    }

    fn check_file_var_decl(
        &mut self,
        Var {
            name,
            init,
            storage,
            ty,
        }: &Var,
    ) {
        let mut init = match init {
            Some(Expression::Constant(n)) => todo!(), //InitValue::Initial(*n),
            None => {
                if *storage == Some(StorageClass::Extern) {
                    InitValue::NoInit
                } else {
                    InitValue::Tentative
                }
            }
            _ => panic!("Non-constant initialization of variable {}", name),
        };

        let mut global = *storage != Some(StorageClass::Static);
        match self.symbols.get(name) {
            Some((Type::Fun { .. }, _)) => {
                panic!("Function {} redeclared as variable", name)
            }
            Some((
                Type::Int,
                Attrs::Static {
                    init: old_init,
                    global: old_global,
                },
            )) => {
                if *storage == Some(StorageClass::Extern) {
                    global = *old_global;
                } else if *old_global != global {
                    panic!("Conflicting linkage of variable {}", name);
                }
                if let InitValue::Initial(_) = old_init {
                    if let InitValue::Initial(_) = init {
                        panic!("Conflicting file scope definitions of variable {}", name);
                    }
                    init = *old_init;
                } else if *old_init == InitValue::Tentative
                    && !matches!(init, InitValue::Initial(_))
                {
                    init = InitValue::Tentative;
                }
            }
            _ => (),
        }
        self.symbols
            .insert(*name, (Type::Int, Attrs::Static { init, global }));
    }

    fn check_block_var_decl(
        &mut self,
        Var {
            name,
            init,
            storage,
            ty,
        }: &Var,
    ) {
        match storage {
            Some(StorageClass::Extern) => {
                if init.is_some() {
                    panic!("Initializer on local extern declaration {}", name);
                }
                if self.symbols.contains_key(name) {
                    if let Some((Type::Fun { .. }, _)) = self.symbols.get(name) {
                        panic!("Function {} redeclared as variable", name);
                    }
                } else {
                    self.symbols.insert(
                        *name,
                        (
                            Type::Int,
                            Attrs::Static {
                                init: InitValue::NoInit,
                                global: true,
                            },
                        ),
                    );
                }
            }
            Some(StorageClass::Static) => {
                let init = match init {
                    Some(Expression::Constant(n)) => todo!(), //InitValue::Initial(*n),
                    None => InitValue::Initial(0),
                    _ => panic!("Non-constant initialization of variable {}", name),
                };
                self.symbols.insert(
                    *name,
                    (
                        Type::Int,
                        Attrs::Static {
                            init,
                            global: false,
                        },
                    ),
                );
            }
            None => {
                self.symbols.insert(*name, (Type::Int, Attrs::Local));
                if let Some(expr) = init {
                    self.check_expr(expr)
                };
            }
        }
    }

    fn check_for_init(&mut self, for_init: &ForInit) {
        match for_init {
            ForInit::Decl(Var {
                storage: Some(StorageClass::Static),
                name,
                ..
            }) => panic!("Static initializer {} in for loop", name),
            ForInit::Decl(var) => self.check_block_var_decl(var),
            ForInit::Exp(expr) => self.check_expr(expr),
            ForInit::Null => (),
        }
    }

    fn check_expr(&mut self, expr: &Expression) {
        match expr {
            Expression::Constant(_) => (),
            Expression::Unary(_, expr) => self.check_expr(expr),
            Expression::Binary(_, lhs, rhs) => {
                self.check_expr(lhs);
                self.check_expr(rhs);
            }
            Expression::Compound(_, lhs, rhs) => {
                self.check_expr(lhs);
                self.check_expr(rhs);
            }
            Expression::Crement(_, _, expr) => self.check_expr(expr),
            Expression::Var(id) => {
                if let Some((Type::Fun { .. }, _)) = self.symbols.get(id) {
                    panic!("Function {} used as variable", id)
                }
            }
            Expression::Assign(lhs, rhs) => {
                self.check_expr(lhs);
                self.check_expr(rhs);
            }
            Expression::Conditional(cond, if_expr, else_expr) => {
                self.check_expr(cond);
                self.check_expr(if_expr);
                self.check_expr(else_expr);
            }
            Expression::Call(name, params) => match self.symbols.get(name) {
                Some((Type::Int, _)) => panic!("Variable {} used as function", name),
                Some((Type::Fun { param_count, .. }, _)) => {
                    if *param_count != params.len() as u8 {
                        panic!(
                            "Mismatched parameter count: declared as {}, called with {}",
                            param_count,
                            params.len()
                        )
                    }
                    for param in params {
                        self.check_expr(param);
                    }
                }
                _ => panic!(
                    "Unreachable: should have resolved function {} already",
                    name
                ),
            },
            Expression::Cast(_, _) => todo!(),
        }
    }
}
