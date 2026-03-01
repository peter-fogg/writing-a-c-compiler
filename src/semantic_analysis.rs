use std::collections::{HashMap, HashSet};

use crate::parser::{BlockItem, Declaration, Expression, Program, Statement};

struct ResolveState {
    env: HashMap<String, String>,
    count: u8,
}

impl ResolveState {
    pub fn declaration(&mut self, Declaration { name, init }: Declaration) -> Declaration {
        if self.env.contains_key(&name) {
            panic!("Duplicate variable name {}", name);
        }
        let new_name = self.new_temp(name.clone());
        self.env.insert(name, new_name.clone());
        let init = init.map(|exp| self.expression(exp));
        Declaration {
            name: new_name,
            init,
        }
    }

    pub fn statement(&mut self, stmt: Statement) -> Statement {
        match stmt {
            Statement::Null => Statement::Null,
            Statement::Return(expr) => Statement::Return(self.expression(expr)),
            Statement::Exp(expr) => Statement::Exp(self.expression(expr)),
            Statement::If(cond, if_stmt, else_stmt) => {
                let cond = self.expression(cond);
                let if_stmt = self.statement(*if_stmt);
                let else_stmt = else_stmt.map(|else_stmt| Box::new(self.statement(*else_stmt)));
                Statement::If(cond, Box::new(if_stmt), else_stmt)
            }
            Statement::Label(id, stmt) => {
                let stmt = self.statement(*stmt);
                Statement::Label(id, Box::new(stmt))
            }
            Statement::Goto(id) => Statement::Goto(id),
        }
    }

    pub fn expression(&mut self, exp: Expression) -> Expression {
        match exp {
            Expression::Assign(lhs, rhs) => {
                if let Expression::Var(_) = *lhs {
                    Expression::Assign(
                        Box::new(self.expression(*lhs)),
                        Box::new(self.expression(*rhs)),
                    )
                } else {
                    panic!("Assignment to non-lvalue {:?}", lhs);
                }
            }
            Expression::Var(id) => {
                if self.env.contains_key(&id) {
                    Expression::Var(self.env.get(&id).unwrap().to_string())
                } else {
                    panic!("Undeclared variable {:?}", id);
                }
            }
            Expression::Unary(unop, expr) => {
                Expression::Unary(unop, Box::new(self.expression(*expr)))
            }
            Expression::Binary(binop, lhs, rhs) => Expression::Binary(
                binop,
                Box::new(self.expression(*lhs)),
                Box::new(self.expression(*rhs)),
            ),
            Expression::Compound(compound_op, lhs, rhs) => {
                if let Expression::Var(_) = *lhs {
                    Expression::Compound(
                        compound_op,
                        Box::new(self.expression(*lhs)),
                        Box::new(self.expression(*rhs)),
                    )
                } else {
                    panic!("Compound operation on non-value {:?}", lhs)
                }
            }
            Expression::Constant(n) => Expression::Constant(n),
            Expression::Crement(fixity, crement, expr) => {
                if let Expression::Var(_) = *expr {
                    Expression::Crement(fixity, crement, Box::new(self.expression(*expr)))
                } else {
                    panic!("Increment/decrement operation on non-lvalue {:?}", expr);
                }
            }
            Expression::Conditional(cond_expr, if_expr, else_expr) => {
                let cond_expr = self.expression(*cond_expr);
                let if_expr = self.expression(*if_expr);
                let else_expr = self.expression(*else_expr);
                Expression::Conditional(Box::new(cond_expr), Box::new(if_expr), Box::new(else_expr))
            }
        }
    }

    fn new_temp(&mut self, var_name: String) -> String {
        let count = self.count;
        self.count += 1;
        format!("{}.resolved.{}", var_name, count)
    }
}

pub fn resolve_vars(Program::Function(name, block_items): Program) -> Program {
    let mut resolve_state = ResolveState {
        env: HashMap::new(),
        count: 0,
    };
    let mut resolved_items = Vec::new();
    for block_item in block_items {
        match block_item {
            BlockItem::S(stmt) => resolved_items.push(BlockItem::S(resolve_state.statement(stmt))),
            BlockItem::D(decl) => {
                resolved_items.push(BlockItem::D(resolve_state.declaration(decl)))
            }
        }
    }

    Program::Function(name, resolved_items)
}

pub fn check_labels(Program::Function(name, block_items): &Program) {
    let mut label_ids = HashSet::new();
    for block_item in block_items {
        if let BlockItem::S(stmt) = block_item {
            check_label(stmt, &mut label_ids);
        }
    }

    for block_item in block_items {
        if let BlockItem::S(Statement::Goto(id)) = block_item
            && !label_ids.contains(id)
        {
            println!("{:?}", label_ids);
            panic!("Goto to unknown label {:?} in function {:?}", id, name)
        }
    }
}

fn check_label(label: &Statement, label_ids: &mut HashSet<String>) {
    match label {
        Statement::Label(id, stmt) => {
            if label_ids.contains(id) {
                panic!("Duplicate label {:?}", id)
            }
            label_ids.insert(id.to_string());
            check_label(stmt, label_ids);
        }
        Statement::If(_cond, if_stmt, else_stmt) => {
            check_label(if_stmt, label_ids);
            if let Some(stmt) = else_stmt {
                check_label(stmt, label_ids)
            }
        }
        _ => (),
    }
}
