use std::collections::{HashMap, HashSet};

use crate::parser::{
    BlockItem, CaseInfo, Declaration, Expression, ForInit, Function, Statement, Var,
};

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
enum Linkage {
    External,
    None,
}

#[derive(PartialEq, Eq, Debug, Clone)]
struct ResolutionInfo {
    name: String,
    linkage: Linkage,
}

struct ResolveState {
    env: Vec<HashMap<String, ResolutionInfo>>,
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

    fn var_declaration(&mut self, Var { name, init }: Var) -> Var {
        if self.current_scope_has(&name) {
            panic!("Duplicate variable name {}", name);
        }
        let new_name = self.new_temp(name.clone());
        let res_info = ResolutionInfo {
            name: new_name.clone(),
            linkage: Linkage::None,
        };
        self.put_env(name, res_info);

        let init = init.map(|exp| self.expression(exp));
        Var {
            name: new_name,
            init,
        }
    }

    fn param(&mut self, name: String) -> String {
        if self.current_scope_has(&name) {
            panic!("Duplicate variable name {}", name);
        }
        let new_name = self.new_temp(name.clone());
        let res_info = ResolutionInfo {
            name: new_name.clone(),
            linkage: Linkage::None,
        };
        self.put_env(name, res_info);
        new_name
    }

    fn func_declaration(&mut self, Function { name, params, body }: Function) -> Function {
        if self.current_scope_has(&name)
            && let Some(ResolutionInfo { name, linkage }) = self.get_env(&name)
                && *linkage == Linkage::None
            {
                panic!("Duplicate function declaration {}", name);
            }

        self.put_env(
            name.clone(),
            ResolutionInfo {
                name: name.clone(),
                linkage: Linkage::External,
            },
        );

        self.env.push(HashMap::new());

        let mut new_params = Vec::with_capacity(params.len());
        for param in params {
            new_params.push(self.param(param));
        }

        if body.is_some() && self.env.len() > 2 {
            panic!("Nested function declaration {}", name)
        }

        let body = body.map(|body| self.block(body));

        self.env.pop();

        Function {
            name,
            params: new_params,
            body,
        }
    }

    pub fn declaration(&mut self, decl: Declaration) -> Declaration {
        match decl {
            Declaration::Var(var) => Declaration::Var(self.var_declaration(var)),
            Declaration::Func(func) => Declaration::Func(self.func_declaration(func)),
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
                    ForInit::Decl(decl) => ForInit::Decl(self.var_declaration(decl)),
                    ForInit::Exp(expr) => ForInit::Exp(self.expression(expr)),
                    ForInit::Null => ForInit::Null,
                };
                let cond = cond.map(|cond| self.expression(cond));
                let post = post.map(|post| self.expression(post));
                let body = self.statement(*body);
                self.env.pop();
                Statement::For(label, init, cond, post, Box::new(body))
            }
            Statement::Case(label, expr, body) => Statement::Case(
                label,
                self.expression(expr),
                Box::new(self.statement(*body)),
            ),
            Statement::Default(label, body) => {
                Statement::Default(label, Box::new(self.statement(*body)))
            }
            Statement::Switch {
                label,
                expr,
                body,
                cases,
            } => Statement::Switch {
                label,
                expr: self.expression(expr),
                body: Box::new(self.statement(*body)),
                cases,
            },
        }
    }

    pub fn expression(&mut self, expr: Expression) -> Expression {
        match expr {
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
                if let Some(ResolutionInfo { name, .. }) = self.get_env(&id) {
                    Expression::Var(name.to_string())
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
            Expression::Call(name, args) => {
                if let Some(ResolutionInfo { name, .. }) = self.get_env(&name) {
                    let name = name.to_string();
                    let mut new_args = Vec::with_capacity(args.len());
                    for arg in args {
                        new_args.push(self.expression(arg));
                    }

                    Expression::Call(name, new_args)
                } else {
                    panic!("Undeclared function {}", name);
                }
            }
        }
    }

    fn new_temp(&mut self, var_name: String) -> String {
        let count = self.count;
        self.count += 1;
        format!("{}.resolved.{}", var_name, count)
    }

    fn get_env(&self, var_name: &String) -> Option<&ResolutionInfo> {
        for map in self.env.iter().rev() {
            if map.contains_key(var_name) {
                return Some(map.get(var_name).unwrap());
            }
        }
        None
    }

    fn put_env(&mut self, var_name: String, info: ResolutionInfo) {
        self.env.last_mut().unwrap().insert(var_name, info);
    }

    fn current_scope_has(&self, var_name: &String) -> bool {
        self.env.last().unwrap().contains_key(var_name)
    }

    // fn function(&mut self, Function { name, body, params }: Function) -> Function {
    //     let body = body.map(|body| self.block(body));
    //     Function { name, body, params }
    // }
}

pub fn analyze(functions: Vec<Function>) -> Vec<Function> {
    let mut analyzed = Vec::with_capacity(functions.len());
    let mut resolve_state = ResolveState {
        env: vec![HashMap::new()],
        count: 0,
    };

    for function in functions {
        let function = resolve_state.func_declaration(function);

        check_labels(&function);
        let mut function = label_loops(function);
        let Function {
            name: _,
            body: ref mut block_items,
            ..
        } = function;

        if let Some(b) = block_items {
            gather_block(b, None)
        }

        analyzed.push(function);
    }

    TypeChecker::check_program(&analyzed);

    analyzed
}

fn check_labels(Function { body, .. }: &Function) {
    let mut label_ids = HashSet::new();
    let mut gotos = HashSet::new();
    if let Some(b) = body {
        check_block_label(b, &mut label_ids, &mut gotos);
    }

    for goto in gotos {
        if !label_ids.contains(&goto) {
            panic!("Goto to unknown label {}", goto)
        }
    }
}

fn check_block_label(
    block_items: &Vec<BlockItem>,
    label_ids: &mut HashSet<String>,
    gotos: &mut HashSet<String>,
) {
    for block_item in block_items {
        if let BlockItem::S(stmt) = block_item {
            check_statement_label(stmt, label_ids, gotos);
        }
    }
}

fn check_statement_label(
    label: &Statement,
    label_ids: &mut HashSet<String>,
    gotos: &mut HashSet<String>,
) {
    match label {
        Statement::Label(id, stmt) => {
            if label_ids.contains(id) {
                panic!("Duplicate label {:?}", id)
            }
            label_ids.insert(id.to_string());
            check_statement_label(stmt, label_ids, gotos);
        }
        Statement::If(_cond, if_stmt, else_stmt) => {
            check_statement_label(if_stmt, label_ids, gotos);
            if let Some(stmt) = else_stmt {
                check_statement_label(stmt, label_ids, gotos)
            }
        }
        Statement::Compound(block_items) => check_block_label(block_items, label_ids, gotos),
        Statement::While(_, _, body) => check_statement_label(body, label_ids, gotos),
        Statement::For(_, _, _, _, body) => check_statement_label(body, label_ids, gotos),
        Statement::DoWhile(_, body, _) => check_statement_label(body, label_ids, gotos),
        Statement::Switch { body, .. } => check_statement_label(body, label_ids, gotos),
        Statement::Case(_, _, stmt) => check_statement_label(stmt, label_ids, gotos),
        Statement::Default(_, stmt) => check_statement_label(stmt, label_ids, gotos),
        Statement::Goto(label) => {
            gotos.insert(label.to_string());
        }
        Statement::Break(_)
        | Statement::Continue(_)
        | Statement::Exp(_)
        | Statement::Null
        | Statement::Return(_) => (),
    }
}

struct Labeller {
    count: u8,
}

#[derive(Clone, Copy)]
enum LabelType {
    For,
    While,
    DoWhile,
    Switch,
    Case,
    Default,
}

fn label_loops(Function { name, body, params }: Function) -> Function {
    Function {
        name,
        body: body.map(|body| Labeller::new().label_block(body, None, None)),
        params,
    }
}

impl Labeller {
    fn new() -> Self {
        Self { count: 0 }
    }

    fn label_block(
        &mut self,
        block_items: Vec<BlockItem>,
        break_label: Option<String>,
        continue_label: Option<String>,
    ) -> Vec<BlockItem> {
        let mut labeled = Vec::with_capacity(block_items.len());
        for block_item in block_items {
            match block_item {
                BlockItem::S(stmt) => labeled.push(BlockItem::S(self.label_statement(
                    stmt,
                    break_label.clone(),
                    continue_label.clone(),
                ))),
                decl => labeled.push(decl),
            }
        }
        labeled
    }

    fn label_statement(
        &mut self,
        stmt: Statement,
        break_label: Option<String>,
        continue_label: Option<String>,
    ) -> Statement {
        match stmt {
            stmt @ (Statement::Return(_)
            | Statement::Exp(_)
            | Statement::Goto(_)
            | Statement::Null) => stmt,
            Statement::If(cond, if_stmt, else_stmt) => Statement::If(
                cond,
                Box::new(self.label_statement(
                    *if_stmt,
                    break_label.clone(),
                    continue_label.clone(),
                )),
                else_stmt
                    .map(|stmt| Box::new(self.label_statement(*stmt, break_label, continue_label))),
            ),
            Statement::Label(id, stmt) => Statement::Label(
                id,
                Box::new(self.label_statement(*stmt, break_label, continue_label)),
            ),
            Statement::Break(_) if break_label.is_none() => {
                panic!("Break statement outside of loop or switch")
            }
            Statement::Break(_) => Statement::Break(break_label.unwrap().to_string()),
            Statement::Continue(_) if continue_label.is_none() => {
                panic!("Continue statement outside of loop")
            }
            Statement::Continue(_) => Statement::Continue(continue_label.unwrap().to_string()),
            Statement::Compound(block_items) => {
                Statement::Compound(self.label_block(block_items, break_label, continue_label))
            }
            Statement::While(_, cond, body) => {
                let new_label = self.new_label(LabelType::While);
                let body =
                    self.label_statement(*body, Some(new_label.clone()), Some(new_label.clone()));
                Statement::While(new_label.to_string(), cond, Box::new(body))
            }
            Statement::DoWhile(_, body, cond) => {
                let new_label = self.new_label(LabelType::DoWhile);
                let body =
                    self.label_statement(*body, Some(new_label.clone()), Some(new_label.clone()));
                Statement::DoWhile(new_label.to_string(), Box::new(body), cond)
            }
            Statement::For(_, init_decl, cond, post, body) => {
                let new_label = self.new_label(LabelType::For);
                let body =
                    self.label_statement(*body, Some(new_label.clone()), Some(new_label.clone()));
                Statement::For(new_label.to_string(), init_decl, cond, post, Box::new(body))
            }
            Statement::Case(_, expr, stmt) => {
                let stmt = self.label_statement(*stmt, break_label.clone(), continue_label);
                let l = self.new_label(LabelType::Case);
                Statement::Case(l.to_string(), expr, Box::new(stmt))
            }
            Statement::Default(_, stmt) => {
                let stmt = self.label_statement(*stmt, break_label, continue_label);
                let l = self.new_label(LabelType::Default);
                Statement::Default(l.to_string(), Box::new(stmt))
            }
            Statement::Switch {
                label: _,
                expr,
                body,
                cases,
            } => {
                let new_label = self.new_label(LabelType::Switch);
                let body =
                    Box::new(self.label_statement(*body, Some(new_label.clone()), continue_label));
                Statement::Switch {
                    label: new_label,
                    expr,
                    body,
                    cases,
                }
            }
        }
    }

    fn new_label(&mut self, label_type: LabelType) -> String {
        self.count += 1;
        let label_str = match label_type {
            LabelType::For => "for",
            LabelType::While => "while",
            LabelType::DoWhile => "do_while",
            LabelType::Switch => "switch",
            LabelType::Case => "case",
            LabelType::Default => "default",
        };

        format!("{}_{}", label_str, self.count)
    }
}

fn gather_block(block_items: &mut Vec<BlockItem>, mut cases: Option<&mut Vec<CaseInfo>>) {
    for block_item in block_items {
        if let BlockItem::S(stmt) = block_item {
            gather_statement(stmt, cases.as_deref_mut()); // TODO bad
        }
    }
}

fn gather_statement(stmt: &mut Statement, mut cases: Option<&mut Vec<CaseInfo>>) {
    match stmt {
        Statement::If(_, if_stmt, else_stmt) => {
            gather_statement(if_stmt, cases.as_deref_mut());
            if let Some(stmt) = else_stmt {
                gather_statement(stmt, cases);
            }
        }
        Statement::Label(_, stmt) => gather_statement(stmt, cases),
        Statement::Compound(block_items) => gather_block(block_items, cases),
        Statement::While(_, _, stmt) => gather_statement(stmt, cases),
        Statement::For(_, _, _, _, body) => gather_statement(body, cases),
        Statement::DoWhile(_, body, _) => gather_statement(body, cases),
        Statement::Switch {
            label: _,
            expr: _,
            body,
            cases,
        } => gather_statement(body, Some(cases)),
        Statement::Case(label, expr, stmt) => {
            gather_statement(stmt, cases.as_deref_mut());
            match expr {
                Expression::Constant(n) if cases.is_some() => {
                    let c = cases.unwrap();
                    if c.iter()
                        .any(|ci| matches!(ci, CaseInfo::Case { expr: m, label: _ } if n == m))
                    {
                        panic!("Duplicate case in switch statement");
                    }
                    c.push(CaseInfo::Case {
                        expr: *n,
                        label: label.to_string(),
                    });
                }
                _ if cases.is_some() => panic!("Non-integral expression in case"),
                _ => panic!("Case outside of switch statement"),
            }
        }
        Statement::Default(label, stmt) => {
            gather_statement(stmt, cases.as_deref_mut());
            match cases {
                None => panic!("Default outside of switch statement"),
                Some(c) => {
                    if c.iter()
                        .any(|ci| matches!(ci, CaseInfo::Default { label: _ }))
                    {
                        panic!("Duplicate default inside of switch")
                    }
                    c.push(CaseInfo::Default {
                        label: label.to_string(),
                    });
                }
            }
        }
        _ => (),
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum Type {
    Int,
    Fun { param_count: u8, defined: bool },
}

struct TypeChecker {
    symbols: HashMap<String, Type>,
}

impl TypeChecker {
    fn check_program(program: &Vec<Function>) {
        let mut type_checker = TypeChecker {
            symbols: HashMap::new(),
        };

        for function in program {
            type_checker.check_function_decl(function);
        }
    }

    fn check_function_decl(&mut self, Function { name, params, body }: &Function) {
        let mut already_defined = false;
        if let Some(Type::Fun {
            param_count,
            defined,
        }) = self.symbols.get(name)
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
        }
        let fun_type = Type::Fun {
            param_count: params.len() as u8,
            defined: body.is_some() || already_defined,
        };

        self.symbols.insert(name.to_string(), fun_type);

        if let Some(block_items) = body {
            for param in params {
                self.symbols.insert(param.to_string(), Type::Int);
            }
            self.check_block(block_items);
        }
    }

    fn check_block(&mut self, block_items: &Vec<BlockItem>) {
        for block_item in block_items {
            match block_item {
                BlockItem::D(decl) => match decl {
                    Declaration::Var(var) => self.check_var_decl(var),
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
                if let Some(else_stmt) = else_stmt
                    .as_ref() { self.check_statement(else_stmt) }
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
                if let Some(cond) = cond.as_ref() { self.check_expr(cond) }
                if let Some(post) = post.as_ref() { self.check_expr(post) }
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

    fn check_var_decl(&mut self, Var { name, init }: &Var) {
        self.symbols.insert(name.to_string(), Type::Int);
        if let Some(expr) = init {
            self.check_expr(expr)
        };
    }

    fn check_for_init(&mut self, for_init: &ForInit) {
        match for_init {
            ForInit::Decl(var) => self.check_var_decl(var),
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
            Expression::Var(id) => if let Some(Type::Fun { .. }) = self.symbols.get(id) { panic!("Function {} used as variable", id) },
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
                Some(Type::Int) => panic!("Variable {} used as function", name),
                Some(Type::Fun { param_count, .. }) => {
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
        }
    }
}
