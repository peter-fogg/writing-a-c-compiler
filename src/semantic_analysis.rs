use std::collections::{HashMap, HashSet};

use crate::parser::{BlockItem, Declaration, Expression, ForInit, Program, Statement};

struct ResolveState {
    env: Vec<HashMap<String, String>>,
    count: u8,
}

impl ResolveState {
    pub fn block(&mut self, block_items: Vec<BlockItem>) -> Vec<BlockItem> {
        let mut resolved_items = Vec::new();
        for block_item in block_items {
            match block_item {
                BlockItem::S(stmt) => resolved_items.push(BlockItem::S(self.statement(stmt))),
                BlockItem::D(decl) => resolved_items.push(BlockItem::D(self.declaration(decl))),
            }
        }
        resolved_items
    }

    pub fn declaration(&mut self, Declaration { name, init }: Declaration) -> Declaration {
        if self.env.last().unwrap().contains_key(&name) {
            panic!("Duplicate variable name {}", name);
        }
        let new_name = self.new_temp(name.clone());
        self.put_env(name, new_name.clone());
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
            Statement::Compound(block_items) => {
                self.env.push(HashMap::new());
                let block_items = self.block(block_items);
                self.env.pop();
                Statement::Compound(block_items)
            }
            Statement::Break(id) => Statement::Break(id),
            Statement::Continue(id) => Statement::Continue(id),
            Statement::DoWhile(label, body, cond) => Statement::DoWhile(
                label,
                Box::new(self.statement(*body)),
                self.expression(cond),
            ),
            Statement::While(label, cond, body) => Statement::While(
                label,
                self.expression(cond),
                Box::new(self.statement(*body)),
            ),
            Statement::For(label, init, cond, post, body) => {
                self.env.push(HashMap::new());
                let init = match init {
                    ForInit::Decl(decl) => ForInit::Decl(self.declaration(decl)),
                    ForInit::Exp(expr) => ForInit::Exp(self.expression(expr)),
                    ForInit::Null => ForInit::Null,
                };
                let cond = cond.map(|cond| self.expression(cond));
                let post = post.map(|post| self.expression(post));
                let body = self.statement(*body);
                self.env.pop();
                Statement::For(label, init, cond, post, Box::new(body))
            }
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
                if let Some(var) = self.get_env(&id) {
                    Expression::Var(var)
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

    fn get_env(&self, var_name: &String) -> Option<String> {
        for map in self.env.iter().rev() {
            if map.contains_key(var_name) {
                return Some(map.get(var_name).unwrap().to_string());
            }
        }
        None
    }

    fn put_env(&mut self, var_name: String, resolved: String) {
        self.env.last_mut().unwrap().insert(var_name, resolved);
    }
}

pub fn analyze(program: Program) -> Program {
    let program = resolve_vars(program);
    check_labels(&program);
    label_loops(program)
}

fn resolve_vars(Program::Function(name, block_items): Program) -> Program {
    let mut resolve_state = ResolveState {
        env: vec![HashMap::new()],
        count: 0,
    };
    let resolved_items = resolve_state.block(block_items);
    Program::Function(name, resolved_items)
}

fn check_labels(Program::Function(name, block_items): &Program) {
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

struct Labeller {
    count: u8,
}

#[derive(Clone)]
enum LoopType {
    For,
    While,
    DoWhile,
}

fn label_loops(Program::Function(name, block_items): Program) -> Program {
    Program::Function(name, Labeller::new().label_block(block_items, None))
}

impl Labeller {
    fn new() -> Self {
        Self { count: 0 }
    }

    fn label_block(
        &mut self,
        block_items: Vec<BlockItem>,
        label: Option<String>,
    ) -> Vec<BlockItem> {
        let mut labeled = Vec::with_capacity(block_items.len());
        for block_item in block_items {
            match block_item {
                BlockItem::S(stmt) => {
                    labeled.push(BlockItem::S(self.label_statement(stmt, label.clone())))
                }
                decl => labeled.push(decl),
            }
        }
        labeled
    }

    fn label_statement(&mut self, stmt: Statement, label: Option<String>) -> Statement {
        match stmt {
            stmt @ (Statement::Return(_)
            | Statement::Exp(_)
            | Statement::Goto(_)
            | Statement::Null) => stmt,
            Statement::If(cond, if_stmt, else_stmt) => Statement::If(
                cond,
                Box::new(self.label_statement(*if_stmt, label.clone())),
                else_stmt.map(|stmt| Box::new(self.label_statement(*stmt, label))),
            ),
            Statement::Label(id, stmt) => {
                Statement::Label(id, Box::new(self.label_statement(*stmt, label)))
            }
            Statement::Break(_) if label.is_none() => {
                panic!("Break statement outside of loop")
            }
            Statement::Break(_) => Statement::Break(label.unwrap()),
            Statement::Continue(_) if label.is_none() => {
                panic!("Continue statement outside of loop")
            }
            Statement::Continue(_) => Statement::Continue(label.unwrap()),
            Statement::Compound(block_items) => {
                Statement::Compound(self.label_block(block_items, label))
            }
            Statement::While(_, cond, body) => {
                let new_label = self.new_label(LoopType::While);
                let body = self.label_statement(*body, Some(new_label.clone()));
                Statement::While(new_label, cond, Box::new(body))
            }
            Statement::DoWhile(_, body, cond) => {
                let new_label = self.new_label(LoopType::DoWhile);
                let body = self.label_statement(*body, Some(new_label.clone()));
                Statement::DoWhile(new_label, Box::new(body), cond)
            }
            Statement::For(_, init_decl, cond, post, body) => {
                let new_label = self.new_label(LoopType::For);
                let body = self.label_statement(*body, Some(new_label.clone()));
                Statement::For(new_label, init_decl, cond, post, Box::new(body))
            }
        }
    }

    fn new_label(&mut self, loop_type: LoopType) -> String {
        self.count += 1;
        let loop_str = match loop_type {
            LoopType::For => "for",
            LoopType::While => "while",
            LoopType::DoWhile => "do_while",
        };

        format!("{}_{}", loop_str, self.count)
    }
}
