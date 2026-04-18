use crate::parser::*;

pub fn print_program(Program(decls): &Program) {
    for decl in decls {
        println!("{}", decl_str(decl, 0));
    }
}

fn decl_str(decl: &Declaration, indent: usize) -> String {
    match decl {
        Declaration::Func(func) => func_decl_str(func, indent),
        Declaration::Var(var) => var_decl_str(var, indent),
    }
}

fn func_decl_str(
    Function {
        name,
        params,
        body,
        storage,
    }: &Function,
    indent: usize,
) -> String {
    let header = format!(
        "{}{} Function {} ({:?})",
        "  ".repeat(indent),
        storage_str(storage),
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

fn block_item_str(block_item: &BlockItem, indent: usize) -> String {
    match block_item {
        BlockItem::D(decl) => decl_str(decl, indent),
        BlockItem::S(stmt) => statement_str(stmt, indent),
    }
}

fn statement_str(stmt: &Statement, indent: usize) -> String {
    let indent_str = "  ".repeat(indent);
    let next_indent_str = "  ".repeat(indent + 1);
    let prettied = match stmt {
        Statement::Return(expr) => format!("Return({})", expr_str(expr)),
        Statement::Exp(expr) => format!("ExprStmt({})", expr_str(expr)),
        Statement::If(cond, if_stmt, else_stmt) => {
            let else_str = match else_stmt {
                None => format!("{}()", next_indent_str),
                Some(else_stmt) => statement_str(else_stmt, indent + 1),
            };
            format!(
                "If(\n{}{},\n{}\n{}\n{})",
                next_indent_str,
                expr_str(cond),
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
                expr_str(cond),
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
                expr_str(cond),
            )
        }
        Statement::For(label, init, incr, post, stmt) => {
            let init = match init {
                ForInit::Decl(var) => var.name.clone(),
                ForInit::Exp(expr) => expr_str(expr),
                ForInit::Null => "".to_string(),
            };
            let incr = incr.as_ref().map(expr_str).unwrap_or("".to_string());
            let post = post.as_ref().map(expr_str).unwrap_or("".to_string());

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
                expr_str(expr),
                statement_str(body, indent + 1)
            )
        }
        Statement::Case(label, expr, stmt) => format!(
            "Case({}, {})\n{}",
            label,
            expr_str(expr),
            statement_str(stmt, indent + 1)
        ),
        Statement::Default(label, stmt) => {
            format!("Default({})\n{}", label, statement_str(stmt, indent + 1))
        }
        Statement::Null => "".to_string(),
    };
    format!("{}{}", indent_str, prettied)
}

fn var_decl_str(
    Var {
        name,
        init,
        storage,
    }: &Var,
    indent: usize,
) -> String {
    format!(
        "{}VarDecl({}, {}, {})",
        "  ".repeat(indent),
        storage_str(storage),
        name,
        init.as_ref().map(expr_str).unwrap_or("()".to_string())
    )
}

fn expr_str(expr: &Expression) -> String {
    match expr {
        Expression::Constant(n) => n.to_string(),
        Expression::Unary(unop, expr) => format!("{:?}({})", unop, expr_str(expr)),
        Expression::Binary(binop, lhs, rhs) => {
            format!("{:?}({}, {})", binop, expr_str(lhs), expr_str(rhs))
        }
        Expression::Compound(compound, lhs, rhs) => {
            format!("{:?}({}, {})", compound, expr_str(lhs), expr_str(rhs))
        }
        Expression::Crement(fix, crement, expr) => {
            format!("{:?}({:?}, {})", fix, crement, expr_str(expr))
        }
        Expression::Var(v) => v.to_string(),
        Expression::Assign(lhs, rhs) => {
            format!("Assign({}, {})", expr_str(lhs), expr_str(rhs))
        }
        Expression::Conditional(cond, if_expr, else_expr) => format!(
            "Conditional({}) if {} else {}",
            expr_str(cond),
            expr_str(if_expr),
            expr_str(else_expr)
        ),
        Expression::Call(func, exprs) => {
            format!(
                "Call({}, {:?})",
                func,
                exprs.iter().map(expr_str).collect::<Vec<_>>()
            )
        }
    }
}

fn storage_str(storage: &Option<StorageClass>) -> String {
    match storage {
        Some(storage_class) => format!("{:?} ", storage_class),
        None => "NoStorage".to_string(),
    }
}
