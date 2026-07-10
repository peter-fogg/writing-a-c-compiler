use crate::ast::{
    BlockItem, Declaration, Expression, ForInit, Function, Program, Statement, StorageClass, Var,
};
use crate::interner::Interner;
use crate::typecheck::TypedExpression;

struct Pretty<'a> {
    interner: &'a Interner,
}
pub fn print_program<'a, E: InternerDisplay<'a>>(
    Program(decls): &Program<E>,
    interner: &'a Interner,
) {
    let pretty = Pretty { interner };
    for decl in decls {
        println!("{}", pretty.decl_str(decl, 0));
    }
}

impl<'a> Pretty<'a> {
    fn decl_str<E: InternerDisplay<'a>>(&self, decl: &Declaration<E>, indent: usize) -> String {
        match decl {
            Declaration::Func(func) => self.func_decl_str(func, indent),
            Declaration::Var(var) => self.var_decl_str(var, indent),
        }
    }

    fn block_item_str<E: InternerDisplay<'a>>(
        &self,
        block_item: &BlockItem<E>,
        indent: usize,
    ) -> String {
        match block_item {
            BlockItem::D(decl) => self.decl_str(decl, indent),
            BlockItem::S(stmt) => self.statement_str(stmt, indent),
        }
    }
    fn func_decl_str<E: InternerDisplay<'a>>(
        &self,
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
            self.interner.get_symbol(*name),
            params
        );
        let body = match body {
            Some(block_items) => {
                let mut blocks = Vec::with_capacity(block_items.len());
                for block_item in block_items {
                    blocks.push(self.block_item_str(block_item, indent + 1));
                }
                blocks.join("\n")
            }
            None => "".to_string(),
        };
        format!("{}\n{}", header, body)
    }

    fn statement_str<E: InternerDisplay<'a>>(&self, stmt: &Statement<E>, indent: usize) -> String {
        let indent_str = "  ".repeat(indent);
        let next_indent_str = "  ".repeat(indent + 1);
        let prettied = match stmt {
            Statement::Return(expr) => format!("Return({})", expr.display(self.interner)),
            Statement::Exp(expr) => format!("ExprStmt({})", expr.display(self.interner)),
            Statement::If(cond, if_stmt, else_stmt) => {
                let else_str = match else_stmt {
                    None => format!("{}()", next_indent_str),
                    Some(else_stmt) => self.statement_str(else_stmt, indent + 1),
                };
                format!(
                    "If(\n{}{},\n{}\n{}\n{})",
                    next_indent_str,
                    cond.display(self.interner),
                    self.statement_str(if_stmt, indent + 1),
                    else_str,
                    indent_str,
                )
            }
            Statement::Goto(id) => format!("Goto({})", self.interner.get_symbol(*id)),
            Statement::Break(id) => format!("Break({})", self.interner.get_symbol(*id)),
            Statement::Continue(id) => format!("Continue({})", self.interner.get_symbol(*id)),
            Statement::Label(id, stmt) => {
                format!(
                    "Label({})\n{}",
                    self.interner.get_symbol(*id),
                    self.statement_str(stmt, indent + 1)
                )
            }
            Statement::Compound(block_items) => {
                return block_items
                    .iter()
                    .map(|block_item| self.block_item_str(block_item, indent))
                    .collect::<Vec<_>>()
                    .join("\n");
            }
            Statement::While(label, cond, stmt) => {
                format!(
                    "While({}, {}\n{}\n{})",
                    self.interner.get_symbol(*label),
                    cond.display(self.interner),
                    self.statement_str(stmt, indent + 1),
                    indent_str,
                )
            }
            Statement::DoWhile(label, stmt, cond) => {
                format!(
                    "DoWhile({},\n{},\n{}{})",
                    self.interner.get_symbol(*label),
                    self.statement_str(stmt, indent + 1),
                    next_indent_str,
                    cond.display(self.interner),
                )
            }
            Statement::For(label, init, incr, post, stmt) => {
                let init = match init {
                    ForInit::Decl(var) => self.var_decl_str(var, indent),
                    ForInit::Exp(expr) => expr.display(self.interner).to_string(),
                    ForInit::Null => "".to_string(),
                };
                let incr = incr
                    .as_ref()
                    .map(|expr| expr.display(self.interner).to_string())
                    .unwrap_or("".to_string());
                let post = post
                    .as_ref()
                    .map(|expr| expr.display(self.interner).to_string())
                    .unwrap_or("".to_string());

                format!(
                    "For({} ({}; {}; {}))\n{}",
                    self.interner.get_symbol(*label),
                    init,
                    incr,
                    post,
                    self.statement_str(stmt, indent + 1)
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
                    self.interner.get_symbol(*label),
                    expr.display(self.interner),
                    self.statement_str(body, indent + 1)
                )
            }
            Statement::Case(label, expr, stmt) => format!(
                "Case({}, {})\n{}",
                self.interner.get_symbol(*label),
                expr.display(self.interner),
                self.statement_str(stmt, indent + 1)
            ),
            Statement::Default(label, stmt) => {
                format!(
                    "Default({})\n{}",
                    self.interner.get_symbol(*label),
                    self.statement_str(stmt, indent + 1)
                )
            }
            Statement::Null => "".to_string(),
        };
        format!("{}{}", indent_str, prettied)
    }

    fn var_decl_str<E: InternerDisplay<'a>>(
        &self,
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
            self.interner.get_symbol(*name),
            init.as_ref()
                .map(|expr| expr.display(self.interner))
                .unwrap_or("".to_string())
        )
    }
}

pub trait InternerDisplay<'a> {
    fn display(&self, interner: &'a Interner) -> String;
}

impl<'a> InternerDisplay<'a> for Expression {
    fn display(&self, interner: &Interner) -> String {
        match self {
            Expression::Constant(n) => format!("{}", n),
            Expression::Unary(unop, expr) => format!("{:?}({})", unop, expr.display(interner)),
            Expression::Binary(binop, lhs, rhs) => {
                format!(
                    "{:?}({}, {})",
                    binop,
                    lhs.display(interner),
                    rhs.display(interner)
                )
            }
            Expression::Crement(fix, crement, expr) => {
                format!("{:?}({:?}, {})", fix, crement, expr.display(interner))
            }
            Expression::Var(v) => interner.get_symbol(*v).to_string(),
            Expression::Assign(lhs, rhs) => {
                format!(
                    "Assign({}, {})",
                    lhs.display(interner),
                    rhs.display(interner)
                )
            }
            Expression::Conditional(cond, if_expr, else_expr) => {
                format!(
                    "Conditional({}) if {} else {}",
                    cond.display(interner),
                    if_expr.display(interner),
                    else_expr.display(interner)
                )
            }
            Expression::Call(func, exprs) => {
                format!(
                    "Call({}, {:?})",
                    func,
                    exprs.iter().map(|expr| expr.display(interner))
                )
            }
            Expression::Cast(ty, expr) => {
                format!("Cast({:?}, {})", ty, expr.display(interner))
            }
            Expression::Compound(binop, lhs, rhs) => format!(
                "{:?}Compound({}, {})",
                binop,
                lhs.display(interner),
                rhs.display(interner)
            ),
            Expression::AddrOf(expr) => format!("AddrOf({})", expr.display(interner)),
            Expression::Deref(expr) => format!("Deref({})", expr.display(interner)),
        }
    }
}

impl<'a> InternerDisplay<'a> for TypedExpression {
    fn display(&self, interner: &'a Interner) -> String {
        match self {
            TypedExpression::Constant(ty, n) => format!("{:?} {}", ty, n),
            TypedExpression::Unary(_, unop, expr) => {
                format!("{:?}({})", unop, expr.display(interner))
            }
            TypedExpression::Binary(_, binop, lhs, rhs) => {
                format!(
                    "{:?}({}, {})",
                    binop,
                    lhs.display(interner),
                    rhs.display(interner)
                )
            }
            TypedExpression::Compound(ty, binop, lhs, rhs) => format!(
                "{:?}Compound({ty:?}, {}, {})",
                binop,
                lhs.display(interner),
                rhs.display(interner)
            ),
            TypedExpression::Crement(_, fix, crement, expr) => {
                format!("{:?}({:?}, {})", fix, crement, expr.display(interner))
            }
            TypedExpression::Var(ty, v) => format!("{:?} {}", ty, interner.get_symbol(*v)),
            TypedExpression::Assign(_, lhs, rhs) => {
                format!(
                    "Assign({}, {})",
                    lhs.display(interner),
                    rhs.display(interner)
                )
            }
            TypedExpression::Conditional(_, cond, if_expr, else_expr) => {
                format!(
                    "Conditional({}) if {} else {}",
                    cond.display(interner),
                    if_expr.display(interner),
                    else_expr.display(interner)
                )
            }
            TypedExpression::Call(_, func, exprs) => {
                format!(
                    "Call({}, {:?})",
                    func,
                    exprs.iter().map(|expr| expr.display(interner))
                )
            }
            TypedExpression::Cast(ty, expr) => {
                format!("Cast({:?}, {})", ty, expr.display(interner))
            }
            TypedExpression::AddrOf(ty, expr) => {
                format!("AddrOf({:?}, {})", ty, expr.display(interner))
            }
            TypedExpression::Deref(ty, expr) => {
                format!("Deref({:?}, {})", ty, expr.display(interner))
            }
        }
    }
}

fn storage_str(storage: &Option<StorageClass>) -> String {
    match storage {
        Some(storage_class) => format!("{:?} ", storage_class),
        None => "NoStorage".to_string(),
    }
}
