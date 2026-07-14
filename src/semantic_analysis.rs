use std::collections::{HashMap, HashSet};

use crate::CompileError;
use crate::ast::{
    BlockItem, CaseInfo, Const, Declaration, Expression, ForInit, Function, Program, Statement,
    StorageClass, Var,
};
use crate::interner::{Interner, Symbol};
use crate::typecheck::{CheckResult, TypeChecker, is_lvalue};

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
enum Linkage {
    External,
    None,
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
enum DeclScope {
    Block,
    File,
}

#[derive(PartialEq, Eq, Debug, Clone)]
struct ResolutionInfo {
    name: Symbol,
    linkage: Linkage,
}

struct ResolveState<'a> {
    env: Vec<HashMap<Symbol, ResolutionInfo>>,
    count: u8,
    interner: &'a mut Interner,
}

impl ResolveState<'_> {
    pub fn block(&mut self, block_items: Vec<BlockItem<Expression>>) -> Vec<BlockItem<Expression>> {
        let mut resolved_items = Vec::new();
        for block_item in block_items {
            match block_item {
                BlockItem::S(stmt) => resolved_items.push(BlockItem::S(self.statement(stmt))),
                BlockItem::D(decl) => resolved_items.push(BlockItem::D(self.declaration(decl))),
            }
        }
        resolved_items
    }

    fn block_var_declaration(
        &mut self,
        Var {
            name,
            init,
            storage,
            ty,
        }: Var<Expression>,
    ) -> Var<Expression> {
        self.put_env(
            name,
            ResolutionInfo {
                name,
                linkage: Linkage::External,
            },
        );
        Var {
            name,
            init,
            storage,
            ty,
        }
    }

    fn local_var_declaration(
        &mut self,
        Var {
            name,
            init,
            storage,
            ty,
        }: Var<Expression>,
    ) -> Var<Expression> {
        if self.current_scope_has(&name)
            && let Some(ResolutionInfo { name, linkage }) = self.get_env(&name)
            && !(*linkage != Linkage::None && storage == Some(StorageClass::Extern))
        {
            panic!("Duplicate variable name {}", name);
        }
        if storage == Some(StorageClass::Extern) {
            let res_info = ResolutionInfo {
                name,
                linkage: Linkage::External,
            };
            self.put_env(name, res_info);
            Var {
                name,
                storage,
                init,
                ty,
            }
        } else {
            let new_name = self.new_temp(name);
            let res_info = ResolutionInfo {
                name: new_name,
                linkage: Linkage::None,
            };
            self.put_env(name, res_info);

            // let init = init.map(|exp| self.expression(exp));
            Var {
                name: new_name,
                init: todo!(),
                storage,
                ty,
            }
        }
    }

    fn param(&mut self, name: Symbol) -> Symbol {
        if self.current_scope_has(&name) {
            panic!("Duplicate variable name {}", name);
        }
        let new_name = self.new_temp(name);
        let res_info = ResolutionInfo {
            name: new_name,
            linkage: Linkage::None,
        };
        self.put_env(name, res_info);
        new_name
    }

    fn func_declaration(
        &mut self,
        Function {
            name,
            params,
            body,
            storage,
            ty,
        }: Function<Expression>,
        scope: DeclScope,
    ) -> Function<Expression> {
        if scope == DeclScope::Block && storage == Some(StorageClass::Static) {
            panic!(
                "Illegal static function declaration {} at block scope",
                name
            )
        }
        if self.current_scope_has(&name)
            && let Some(ResolutionInfo { name, linkage }) = self.get_env(&name)
            && *linkage == Linkage::None
        {
            panic!("Duplicate function declaration {}", name);
        }

        self.put_env(
            name,
            ResolutionInfo {
                name,
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
            storage,
            ty,
        }
    }

    pub fn declaration(&mut self, decl: Declaration<Expression>) -> Declaration<Expression> {
        match decl {
            Declaration::Var(var) => Declaration::Var(self.local_var_declaration(var)),
            Declaration::Func(func) => {
                Declaration::Func(self.func_declaration(func, DeclScope::Block))
            }
        }
    }

    pub fn statement(&mut self, stmt: Statement<Expression>) -> Statement<Expression> {
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
                    ForInit::Decl(decl) => ForInit::Decl(self.local_var_declaration(decl)),
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
                if is_lvalue(&lhs) {
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
                    Expression::Var(*name)
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
                if is_lvalue(&lhs) {
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
                if is_lvalue(&expr) {
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
                    let name = *name;
                    let mut new_args = Vec::with_capacity(args.len());
                    for arg in args {
                        new_args.push(self.expression(arg));
                    }

                    Expression::Call(name, new_args)
                } else {
                    panic!("Undeclared function {}", name);
                }
            }
            Expression::Cast(ty, expr) => Expression::Cast(ty, Box::new(self.expression(*expr))),
            Expression::AddrOf(expr) => Expression::AddrOf(Box::new(self.expression(*expr))),

            Expression::Deref(expr) => Expression::Deref(Box::new(self.expression(*expr))),
            Expression::Subscript(_, _) => todo!(),
        }
    }

    fn new_temp(&mut self, var: Symbol) -> Symbol {
        let count = self.count;
        self.count += 1;
        let var_name = self.interner.get_symbol(var);
        self.interner
            .intern(format!("{}.resolved.{}", var_name, count))
    }

    fn get_env(&self, var: &Symbol) -> Option<&ResolutionInfo> {
        for map in self.env.iter().rev() {
            if map.contains_key(var) {
                return Some(map.get(var).unwrap());
            }
        }
        None
    }

    fn put_env(&mut self, var: Symbol, info: ResolutionInfo) {
        self.env.last_mut().unwrap().insert(var, info);
    }

    fn current_scope_has(&self, var: &Symbol) -> bool {
        self.env.last().unwrap().contains_key(var)
    }
}

pub fn analyze(
    program: Program<Expression>,
    interner: &mut Interner,
) -> Result<CheckResult, CompileError> {
    let declarations = program.0;
    let mut analyzed = Vec::with_capacity(declarations.len());
    let mut resolve_state = ResolveState {
        env: vec![HashMap::new()],
        count: 0,
        interner,
    };

    for declaration in declarations {
        match declaration {
            Declaration::Func(function) => {
                let function = resolve_state.func_declaration(function, DeclScope::File);

                check_labels(&function);
                let mut function = label_loops(function, &mut *resolve_state.interner);
                let Function {
                    name: _,
                    body: ref mut block_items,
                    ..
                } = function;

                if let Some(b) = block_items {
                    gather_block(b, None)
                }

                analyzed.push(Declaration::Func(function));
            }
            Declaration::Var(var) => {
                let var = resolve_state.block_var_declaration(var);
                analyzed.push(Declaration::Var(var));
            }
        }
    }

    TypeChecker::check_program(analyzed, interner)
}

fn check_labels(Function { body, .. }: &Function<Expression>) {
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
    block_items: &Vec<BlockItem<Expression>>,
    label_ids: &mut HashSet<Symbol>,
    gotos: &mut HashSet<Symbol>,
) {
    for block_item in block_items {
        if let BlockItem::S(stmt) = block_item {
            check_statement_label(stmt, label_ids, gotos);
        }
    }
}

fn check_statement_label(
    label: &Statement<Expression>,
    label_ids: &mut HashSet<Symbol>,
    gotos: &mut HashSet<Symbol>,
) {
    match label {
        Statement::Label(id, stmt) => {
            if label_ids.contains(id) {
                panic!("Duplicate label {:?}", id)
            }
            label_ids.insert(*id);
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
            gotos.insert(*label);
        }
        Statement::Break(_)
        | Statement::Continue(_)
        | Statement::Exp(_)
        | Statement::Null
        | Statement::Return(_) => (),
    }
}

struct Labeller<'a> {
    count: u8,
    interner: &'a mut Interner,
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

fn label_loops(
    Function {
        name,
        body,
        params,
        storage,
        ty,
    }: Function<Expression>,
    interner: &mut Interner,
) -> Function<Expression> {
    Function {
        name,
        body: body.map(|body| Labeller::new(interner).label_block(body, None, None, name)),
        params,
        storage,
        ty,
    }
}

impl<'a> Labeller<'a> {
    fn new(interner: &'a mut Interner) -> Self {
        Self { count: 0, interner }
    }

    fn label_block(
        &mut self,
        block_items: Vec<BlockItem<Expression>>,
        break_label: Option<Symbol>,
        continue_label: Option<Symbol>,
        fn_name: Symbol,
    ) -> Vec<BlockItem<Expression>> {
        let mut labeled = Vec::with_capacity(block_items.len());
        for block_item in block_items {
            match block_item {
                BlockItem::S(stmt) => labeled.push(BlockItem::S(self.label_statement(
                    stmt,
                    break_label,
                    continue_label,
                    fn_name,
                ))),
                decl => labeled.push(decl),
            }
        }
        labeled
    }

    fn label_statement(
        &mut self,
        stmt: Statement<Expression>,
        break_label: Option<Symbol>,
        continue_label: Option<Symbol>,
        fn_name: Symbol,
    ) -> Statement<Expression> {
        match stmt {
            stmt @ (Statement::Return(_) | Statement::Exp(_) | Statement::Null) => stmt,
            /*
             * Add function name (symbol) arg to label_statement
             * Rename goto as format!("{}.{}", function_name, label) -- period is a valid identifier letter
             * Rename all labels in a function too?
             */
            Statement::Goto(id) => Statement::Goto(self.uniquify_label(id, fn_name)),

            Statement::If(cond, if_stmt, else_stmt) => Statement::If(
                cond,
                Box::new(self.label_statement(*if_stmt, break_label, continue_label, fn_name)),
                else_stmt.map(|stmt| {
                    Box::new(self.label_statement(*stmt, break_label, continue_label, fn_name))
                }),
            ),
            Statement::Label(id, stmt) => Statement::Label(
                self.uniquify_label(id, fn_name),
                Box::new(self.label_statement(*stmt, break_label, continue_label, fn_name)),
            ),
            Statement::Break(_) if break_label.is_none() => {
                panic!("Break statement outside of loop or switch")
            }
            Statement::Break(_) => Statement::Break(break_label.unwrap()),
            Statement::Continue(_) if continue_label.is_none() => {
                panic!("Continue statement outside of loop")
            }
            Statement::Continue(_) => Statement::Continue(continue_label.unwrap()),
            Statement::Compound(block_items) => Statement::Compound(self.label_block(
                block_items,
                break_label,
                continue_label,
                fn_name,
            )),
            Statement::While(_, cond, body) => {
                let new_label = self.new_label(LabelType::While);
                let body = self.label_statement(*body, Some(new_label), Some(new_label), fn_name);
                Statement::While(new_label, cond, Box::new(body))
            }
            Statement::DoWhile(_, body, cond) => {
                let new_label = self.new_label(LabelType::DoWhile);
                let body = self.label_statement(*body, Some(new_label), Some(new_label), fn_name);
                Statement::DoWhile(new_label, Box::new(body), cond)
            }
            Statement::For(_, init_decl, cond, post, body) => {
                let new_label = self.new_label(LabelType::For);
                let body = self.label_statement(*body, Some(new_label), Some(new_label), fn_name);
                Statement::For(new_label, init_decl, cond, post, Box::new(body))
            }
            Statement::Case(_, expr, stmt) => {
                let stmt = self.label_statement(*stmt, break_label, continue_label, fn_name);
                let l = self.new_label(LabelType::Case);
                Statement::Case(l, expr, Box::new(stmt))
            }
            Statement::Default(_, stmt) => {
                let stmt = self.label_statement(*stmt, break_label, continue_label, fn_name);
                let l = self.new_label(LabelType::Default);
                Statement::Default(l, Box::new(stmt))
            }
            Statement::Switch {
                label: _,
                expr,
                body,
                cases,
            } => {
                let new_label = self.new_label(LabelType::Switch);
                let body =
                    Box::new(self.label_statement(*body, Some(new_label), continue_label, fn_name));
                Statement::Switch {
                    label: new_label,
                    expr,
                    body,
                    cases,
                }
            }
        }
    }

    // Generate unique labels inside a function. Label names are
    // allowed to be reused in different functions in C, but in the
    // generated code they must be unique. Separate the function name
    // and the original label by a period, since this is guaranteed to
    // not be a valid identifier.
    fn uniquify_label(&mut self, id: Symbol, fn_name: Symbol) -> Symbol {
        let old_label = self.interner.get_symbol(id);
        let fn_name = self.interner.get_symbol(fn_name);
        let new_label = format!("{}.{}", fn_name, old_label);
        self.interner.intern(new_label)
    }

    fn new_label(&mut self, label_type: LabelType) -> Symbol {
        self.count += 1;
        let label_str = match label_type {
            LabelType::For => "for",
            LabelType::While => "while",
            LabelType::DoWhile => "do_while",
            LabelType::Switch => "switch",
            LabelType::Case => "case",
            LabelType::Default => "default",
        };

        self.interner
            .intern(format!("{}_{}", label_str, self.count))
    }
}

fn gather_block(
    block_items: &mut Vec<BlockItem<Expression>>,
    mut cases: Option<&mut Vec<CaseInfo>>,
) {
    for block_item in block_items {
        if let BlockItem::S(stmt) = block_item {
            gather_statement(stmt, cases.as_deref_mut()); // TODO bad
        }
    }
}

// Return the actual compile-time value of a constant, since we need
// to compare these in cases to make sure we don't have
// duplicates. This returns u64 because any int or long must fit into
// that value.
pub fn actual_case_value(c: Const) -> u64 {
    match c {
        Const::Int(n) => n as u64,
        Const::Long(n) => n as u64,
        Const::UInt(n) => n as u64,
        Const::ULong(n) => n,
        Const::Double(n) => n.to_bits(), // TODO: this seems sketch -- should it be a straight bitwise cast?
    }
}

fn gather_statement(stmt: &mut Statement<Expression>, mut cases: Option<&mut Vec<CaseInfo>>) {
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
        Statement::Switch { body, cases, .. } => {
            gather_statement(body, Some(cases));
        }
        Statement::Case(label, expr, stmt) => {
            gather_statement(stmt, cases.as_deref_mut());
            match expr {
                Expression::Constant(n) if cases.is_some() => {
                    let c = cases.unwrap();
                    if c.iter().any(|ci| {
                        matches!(ci, CaseInfo::Case { expr: m, label: _ } if actual_case_value(*n) == actual_case_value(*m)
                        )
                    }) {
                        panic!("Duplicate case in switch statement");
                    }
                    c.push(CaseInfo::Case {
                        expr: *n,
                        label: *label,
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
                    c.push(CaseInfo::Default { label: *label });
                }
            }
        }
        _ => (),
    }
}
