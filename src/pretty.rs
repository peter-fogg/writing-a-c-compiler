use std::fmt;

use crate::ast::{
    BlockItem, Declaration, Expression, ForInit, Function, Program, Statement, StorageClass, Var,
};

use crate::typecheck::TypedExpression;

pub fn print_program<E: fmt::Display>(Program(decls): &Program<E>) {
    for decl in decls {
        println!("{}", decl_str(decl, 0));
    }
}

fn decl_str<E: fmt::Display>(decl: &Declaration<E>, indent: usize) -> String {
    match decl {
        Declaration::Func(func) => func_decl_str(func, indent),
        Declaration::Var(var) => var_decl_str(var, indent),
    }
}

fn func_decl_str<E: fmt::Display>(
    Function {
        name,
        params,
        body,
        storage,
        ty,
    }: &Function<E>,
    indent: usize,
) -> String {
    let header = format!(
        "{}{} {:?} Function {} ({:?})",
        "  ".repeat(indent),
        storage_str(storage),
        ty,
        name,
        params
    );
    let body = match body {
        Some(block_items) => {
            let mut blocks = Vec::with_capacity(block_items.len());
            for block_item in block_items {
                blocks.push(block_item_str(block_item, indent + 1));
            }
            blocks.join("\n")
        }
        None => "".to_string(),
    };
    format!("{}\n{}", header, body)
}

fn block_item_str<E: fmt::Display>(block_item: &BlockItem<E>, indent: usize) -> String {
    match block_item {
        BlockItem::D(decl) => decl_str(decl, indent),
        BlockItem::S(stmt) => statement_str(stmt, indent),
    }
}

fn statement_str<E: fmt::Display>(stmt: &Statement<E>, indent: usize) -> String {
    let indent_str = "  ".repeat(indent);
    let next_indent_str = "  ".repeat(indent + 1);
    let prettied = match stmt {
        Statement::Return(expr) => format!("Return({})", expr),
        Statement::Exp(expr) => format!("ExprStmt({})", expr),
        Statement::If(cond, if_stmt, else_stmt) => {
            let else_str = match else_stmt {
                None => format!("{}()", next_indent_str),
                Some(else_stmt) => statement_str(else_stmt, indent + 1),
            };
            format!(
                "If(\n{}{},\n{}\n{}\n{})",
                next_indent_str,
                cond,
                statement_str(if_stmt, indent + 1),
                else_str,
                indent_str,
            )
        }
        Statement::Goto(id) => format!("Goto({})", id),
        Statement::Break(id) => format!("Break({})", id),
        Statement::Continue(id) => format!("Continue({})", id),
        Statement::Label(id, stmt) => format!("Label({})\n{}", id, statement_str(stmt, indent + 1)),
        Statement::Compound(block_items) => {
            return block_items
                .iter()
                .map(|block_item| block_item_str(block_item, indent))
                .collect::<Vec<_>>()
                .join("\n");
        }
        Statement::While(label, cond, stmt) => {
            format!(
                "While({}, {}\n{}\n{})",
                label,
                cond,
                statement_str(stmt, indent + 1),
                indent_str,
            )
        }
        Statement::DoWhile(label, stmt, cond) => {
            format!(
                "DoWhile({},\n{},\n{}{})",
                label,
                statement_str(stmt, indent + 1),
                next_indent_str,
                cond,
            )
        }
        Statement::For(label, init, incr, post, stmt) => {
            let init = match init {
                ForInit::Decl(var) => var.name.to_string(),
                ForInit::Exp(expr) => format!("{}", expr),
                ForInit::Null => "".to_string(),
            };
            let incr = incr
                .as_ref()
                .map(|expr| format!("{}", expr))
                .unwrap_or("".to_string());
            let post = post
                .as_ref()
                .map(|expr| format!("{}", expr))
                .unwrap_or("".to_string());

            format!(
                "For({} ({}; {}; {}))\n{}",
                label,
                init,
                incr,
                post,
                statement_str(stmt, indent + 1)
            )
        }
        Statement::Switch {
            label,
            expr,
            body,
            cases: _,
        } => {
            format!(
                "Switch({}, ({}))\n{}",
                label,
                expr,
                statement_str(body, indent + 1)
            )
        }
        Statement::Case(label, expr, stmt) => format!(
            "Case({}, {})\n{}",
            label,
            expr,
            statement_str(stmt, indent + 1)
        ),
        Statement::Default(label, stmt) => {
            format!("Default({})\n{}", label, statement_str(stmt, indent + 1))
        }
        Statement::Null => "".to_string(),
    };
    format!("{}{}", indent_str, prettied)
}

fn var_decl_str<E: fmt::Display>(
    Var {
        name,
        init,
        storage,
        ty,
    }: &Var<E>,
    indent: usize,
) -> String {
    format!(
        "{}VarDecl({}, {:?}, {}, {})",
        "  ".repeat(indent),
        storage_str(storage),
        ty,
        name,
        init.as_ref()
            .map(|expr_str| format!("{}", expr_str))
            .unwrap_or("".to_string())
    )
}

impl fmt::Display for TypedExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            TypedExpression::Constant(ty, n) => format!("{:?} {}", ty, n),
            TypedExpression::Unary(_, unop, expr) => format!("{:?}({})", unop, expr),
            TypedExpression::Binary(_, binop, lhs, rhs) => {
                format!("{:?}({}, {})", binop, lhs, rhs)
            }
            TypedExpression::Crement(_, fix, crement, expr) => {
                format!("{:?}({:?}, {})", fix, crement, expr)
            }
            TypedExpression::Var(ty, v) => format!("{:?} {}", ty, v),
            TypedExpression::Assign(_, lhs, rhs) => {
                format!("Assign({}, {})", lhs, rhs)
            }
            TypedExpression::Conditional(_, cond, if_expr, else_expr) => {
                format!("Conditional({}) if {} else {}", cond, if_expr, else_expr)
            }
            TypedExpression::Call(_, func, exprs) => {
                format!("Call({}, {:?})", func, exprs)
            }
            TypedExpression::Cast(ty, expr) => {
                format!("Cast({:?}, {})", ty, expr)
            }
        };
        write!(f, "{}", s)
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Expression::Constant(n) => n.to_string(),
            Expression::Unary(unop, expr) => format!("{:?}({})", unop, expr),
            Expression::Binary(binop, lhs, rhs) => {
                format!("{:?}({}, {})", binop, lhs, rhs)
            }
            Expression::Compound(compound, lhs, rhs) => {
                format!("{:?}({}, {})", compound, lhs, rhs)
            }
            Expression::Crement(fix, crement, expr) => {
                format!("{:?}({:?}, {})", fix, crement, expr)
            }
            Expression::Var(v) => v.to_string(),
            Expression::Assign(lhs, rhs) => {
                format!("Assign({}, {})", lhs, rhs)
            }
            Expression::Conditional(cond, if_expr, else_expr) => {
                format!("Conditional({}) if {} else {}", cond, if_expr, else_expr)
            }
            Expression::Call(func, exprs) => {
                format!("Call({}, {:?})", func, exprs)
            }
            Expression::Cast(ty, expr) => {
                format!("Cast({:?}, {})", ty, expr)
            }
        };
        write!(f, "{}", s)
    }
}

fn storage_str(storage: &Option<StorageClass>) -> String {
    match storage {
        Some(storage_class) => format!("{:?} ", storage_class),
        None => "NoStorage".to_string(),
    }
}
