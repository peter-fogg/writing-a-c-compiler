use std::collections::HashMap;

use crate::ast::{
    BinaryOperator, BlockItem, CaseInfo, CompoundOperator, Const, Crement, Declaration, Fixity,
    ForInit, Function, Program, Statement, Type, UnaryOperator, Var,
};
use crate::interner::{Interner, Symbol};
use crate::typecheck::{Attrs, InitValue, StaticInit, TypedExpression, get_type};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    Complement,
    Negate,
    Not,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitAnd,
    BitOr,
    BitXOr,
    ShiftLeft,
    ShiftRight,
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
    Equals,
    NotEquals,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Val {
    Constant(Const),
    Var(Symbol),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Instr {
    Return(Val),
    Unary {
        unop: UnaryOp,
        src: Val,
        dst: Val,
    },
    Binary {
        binop: BinaryOp,
        src1: Val,
        src2: Val,
        dst: Val,
    },
    Copy {
        src: Val,
        dst: Val,
    },
    Jump {
        target: Symbol,
    },
    JumpIfZero {
        condition: Val,
        target: Symbol,
    },
    JumpIfNotZero {
        condition: Val,
        target: Symbol,
    },
    Label(Symbol),
    Call {
        name: Symbol,
        params: Vec<Val>,
        dst: Val,
    },
    SignExtend {
        src: Val,
        dst: Val,
    },
    Truncate {
        src: Val,
        dst: Val,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum TopLevel {
    TackyFunction {
        name: Symbol,
        params: Vec<(Symbol, Type)>,
        instructions: Vec<Instr>,
        global: bool,
    },
    StaticVar {
        name: Symbol,
        global: bool,
        ty: Type,
        init: StaticInit,
    },
}
use TopLevel::*;

pub type Tacky = Vec<TopLevel>;

struct TackifyState<'a> {
    count: u8,
    symbols: &'a mut HashMap<Symbol, (Type, Attrs)>,
    interner: &'a mut Interner,
}

pub fn emit_tacky(
    program: Program<TypedExpression>,
    symbols: &mut HashMap<Symbol, (Type, Attrs)>,
    interner: &mut Interner,
) -> Tacky {
    let declarations = program.0;
    let mut program_tacky = Vec::new();

    let mut tackify_state = TackifyState::new(symbols, interner);

    for declaration in declarations {
        match declaration {
            Declaration::Func(function) => {
                tackify_state.tackify_function(function, &mut program_tacky)
            }
            Declaration::Var(_) => (),
        }
    }

    tackify_state.tackify_symbols(&mut program_tacky);

    program_tacky
}

impl<'a> TackifyState<'a> {
    pub fn new(
        symbols: &'a mut HashMap<Symbol, (Type, Attrs)>,
        interner: &'a mut Interner,
    ) -> Self {
        Self {
            count: 0,
            symbols,
            interner,
        }
    }

    fn tackify_symbols(&mut self, program: &mut Tacky) {
        for (name, (ty, attrs)) in self.symbols.iter() {
            if let Attrs::Static { init, global } = attrs {
                match init {
                    InitValue::Initial(n) => program.push(StaticVar {
                        name: *name,
                        global: *global,
                        init: *n,
                        ty: ty.clone(),
                    }),
                    InitValue::Tentative => program.push(StaticVar {
                        name: *name,
                        global: *global,
                        init: if *ty == Type::Long {
                            StaticInit::Long(0)
                        } else {
                            StaticInit::Int(0)
                        },
                        ty: ty.clone(),
                    }),
                    InitValue::NoInit => (),
                }
            }
        }
    }

    fn tackify_function(
        &mut self,
        Function {
            name, params, body, ..
        }: Function<TypedExpression>,
        program: &mut Tacky,
    ) {
        if let Some(body) = body {
            let mut instructions = Vec::new();
            self.tackify_block(body, &mut instructions);
            instructions.push(Instr::Return(Val::Constant(Const::Int(0))));
            let global = match self.symbols.get(&name) {
                Some((_, Attrs::Fun { global, .. })) => *global,
                _ => false,
            };
            program.push(TackyFunction {
                name,
                params,
                instructions,
                global,
            });
        }
    }

    fn tackify_block(
        &mut self,
        block_items: Vec<BlockItem<TypedExpression>>,
        instrs: &mut Vec<Instr>,
    ) {
        for block_item in block_items {
            match block_item {
                BlockItem::D(decl) => self.tackify_declaration(decl, instrs),
                BlockItem::S(stmt) => self.tackify_statement(stmt, instrs),
            }
        }
    }

    fn tackify_declaration(&mut self, decl: Declaration<TypedExpression>, instrs: &mut Vec<Instr>) {
        match decl {
            Declaration::Var(Var {
                name,
                init,
                storage: None,
                ty: _,
            }) => {
                if let Some(expr) = init {
                    let expr = self.tackify_expr(expr, instrs);
                    instrs.push(Instr::Copy {
                        src: expr,
                        dst: Val::Var(name),
                    });
                }
            }
            Declaration::Var(_) => (),
            Declaration::Func(_) => (),
        }
    }

    fn tackify_statement(&mut self, stmt: Statement<TypedExpression>, instrs: &mut Vec<Instr>) {
        match stmt {
            Statement::Null => (),
            Statement::Return(expr) => {
                let result = Instr::Return(self.tackify_expr(expr, instrs));
                instrs.push(result);
            }
            Statement::Exp(expr) => {
                self.tackify_expr(expr, instrs);
            }
            Statement::If(cond, if_stmt, Some(else_stmt)) => {
                let cond = self.tackify_expr(cond, instrs);
                let else_label = self.new_temp("if_else");
                let end_label = self.new_temp("if_end");
                instrs.push(Instr::JumpIfZero {
                    condition: cond,
                    target: else_label,
                });
                self.tackify_statement(*if_stmt, instrs);
                instrs.push(Instr::Jump { target: end_label });
                instrs.push(Instr::Label(else_label));
                self.tackify_statement(*else_stmt, instrs);
                instrs.push(Instr::Label(end_label));
            }
            Statement::If(cond, if_stmt, None) => {
                let cond = self.tackify_expr(cond, instrs);
                let end_label = self.new_temp("if_end");
                instrs.push(Instr::JumpIfZero {
                    condition: cond,
                    target: end_label,
                });
                self.tackify_statement(*if_stmt, instrs);
                instrs.push(Instr::Label(end_label));
            }
            Statement::Label(id, stmt) => {
                instrs.push(Instr::Label(id));
                self.tackify_statement(*stmt, instrs);
            }
            Statement::Goto(id) => {
                instrs.push(Instr::Jump { target: id });
            }
            Statement::Compound(block_items) => self.tackify_block(block_items, instrs),
            Statement::Break(label) => instrs.push(Instr::Jump {
                target: self.block_label("break", label),
            }),
            Statement::Continue(label) => instrs.push(Instr::Jump {
                target: self.block_label("continue", label),
            }),
            Statement::DoWhile(label, body, cond) => {
                instrs.push(Instr::Label(label));
                self.tackify_statement(*body, instrs);
                instrs.push(Instr::Label(self.block_label("continue", label)));
                let cond = self.tackify_expr(cond, instrs);
                instrs.push(Instr::JumpIfNotZero {
                    condition: cond,
                    target: label,
                });
                instrs.push(Instr::Label(self.block_label("break", label)));
            }
            Statement::While(label, cond, body) => {
                let continue_label = self.block_label("continue", label);
                let break_label = self.block_label("break", label);
                instrs.push(Instr::Label(continue_label));
                let cond = self.tackify_expr(cond, instrs);
                instrs.push(Instr::JumpIfZero {
                    condition: cond,
                    target: break_label,
                });
                self.tackify_statement(*body, instrs);
                instrs.push(Instr::Jump {
                    target: continue_label,
                });
                instrs.push(Instr::Label(break_label));
            }
            Statement::For(label, init, cond, post, body) => {
                match init {
                    ForInit::Decl(decl) => {
                        self.tackify_declaration(Declaration::Var(decl), instrs);
                    }
                    ForInit::Exp(expr) => {
                        self.tackify_expr(expr, instrs);
                    }
                    ForInit::Null => (),
                }
                instrs.push(Instr::Label(label));
                let break_label = self.block_label("break", label);
                if let Some(expr) = cond {
                    let result = self.tackify_expr(expr, instrs);
                    instrs.push(Instr::JumpIfZero {
                        condition: result,
                        target: break_label,
                    });
                }
                self.tackify_statement(*body, instrs);
                instrs.push(Instr::Label(self.block_label("continue", label)));
                if let Some(expr) = post {
                    self.tackify_expr(expr, instrs);
                }
                instrs.extend(vec![
                    Instr::Jump { target: label },
                    Instr::Label(break_label),
                ])
            }
            Statement::Case(label, _expr, stmt) => {
                instrs.push(Instr::Label(label));
                self.tackify_statement(*stmt, instrs);
            }
            Statement::Default(label, stmt) => {
                instrs.push(Instr::Label(label));
                self.tackify_statement(*stmt, instrs);
            }
            Statement::Switch {
                label,
                expr,
                body,
                cases,
            } => {
                let result = self.tackify_expr(expr, instrs);
                let (cases, default): (Vec<_>, Vec<_>) = cases
                    .iter()
                    .partition(|ci| matches!(ci, CaseInfo::Case { expr: _, label: _ }));
                for case in cases {
                    match case {
                        CaseInfo::Case { expr: n, label } => {
                            let val = Val::Constant(*n);
                            let binop = BinaryOp::Equals;
                            let ty = if matches!(n, Const::Int(_)) {
                                Type::Int
                            } else {
                                Type::Long
                            };
                            let dst = self.new_var(ty, "case_tmp");
                            instrs.push(Instr::Binary {
                                binop,
                                src1: val,
                                src2: result,
                                dst,
                            });
                            instrs.push(Instr::JumpIfNotZero {
                                condition: dst,
                                target: *label,
                            })
                        }
                        _ => unreachable!(),
                    }
                }
                if default.len() == 1 {
                    match default[0] {
                        CaseInfo::Default { label } => instrs.push(Instr::Jump { target: *label }),
                        _ => unreachable!(),
                    }
                }
                let break_label = self.block_label("break", label);
                instrs.push(Instr::Jump {
                    target: break_label,
                });
                self.tackify_statement(*body, instrs);
                instrs.push(Instr::Label(break_label));
            }
        }
    }

    fn tackify_expr(&mut self, expr: TypedExpression, instrs: &mut Vec<Instr>) -> Val {
        match expr {
            TypedExpression::Constant(_ty, n) => Val::Constant(n),
            TypedExpression::Unary(ty, un_op, inner) => {
                let src = self.tackify_expr(*inner, instrs);
                let dst = self.new_var(ty, "unop");
                let op = Self::convert_unop(un_op);
                let new_unop = Instr::Unary { unop: op, src, dst };
                instrs.push(new_unop);
                dst
            }
            TypedExpression::Binary(ty, BinaryOperator::And, lhs, rhs) => {
                let end_label = self.new_temp("and_end");
                let false_label = self.new_temp("and_false");
                let ret_val = self.new_var(ty, "and_result");

                let lhs = self.tackify_expr(*lhs, instrs);

                instrs.push(Instr::JumpIfZero {
                    condition: lhs,
                    target: false_label,
                });
                let rhs = self.tackify_expr(*rhs, instrs);
                instrs.extend(vec![
                    Instr::JumpIfZero {
                        condition: rhs,
                        target: false_label,
                    },
                    Instr::Copy {
                        src: Val::Constant(Const::Int(1)),
                        dst: ret_val,
                    },
                    Instr::Jump { target: end_label },
                    Instr::Label(false_label),
                    Instr::Copy {
                        src: Val::Constant(Const::Int(0)),
                        dst: ret_val,
                    },
                    Instr::Label(end_label),
                ]);

                ret_val
            }
            TypedExpression::Binary(ty, BinaryOperator::Or, lhs, rhs) => {
                let end_label = self.new_temp("or_end");
                let true_label = self.new_temp("or_true");
                let ret_val = self.new_var(ty, "or_result");

                let lhs = self.tackify_expr(*lhs, instrs);
                instrs.push(Instr::JumpIfNotZero {
                    condition: lhs,
                    target: true_label,
                });
                let rhs = self.tackify_expr(*rhs, instrs);
                instrs.extend(vec![
                    Instr::JumpIfNotZero {
                        condition: rhs,
                        target: true_label,
                    },
                    Instr::Copy {
                        src: Val::Constant(Const::Int(0)),
                        dst: ret_val,
                    },
                    Instr::Jump { target: end_label },
                    Instr::Label(true_label),
                    Instr::Copy {
                        src: Val::Constant(Const::Int(1)),
                        dst: ret_val,
                    },
                    Instr::Label(end_label),
                ]);

                ret_val
            }
            TypedExpression::Binary(ty, binop, lhs, rhs) => {
                let src1 = self.tackify_expr(*lhs, instrs);
                let src2 = self.tackify_expr(*rhs, instrs);
                let dst = self.new_var(ty, "binop");

                let op = Self::convert_binop(binop);

                let new_binop = Instr::Binary {
                    binop: op,
                    src1,
                    src2,
                    dst,
                };

                instrs.push(new_binop);

                dst
            }
            TypedExpression::Compound(ty, compound_op, lhs, rhs) => {
                let op = Self::convert_compound_op(compound_op);

                let TypedExpression::Var(_, id) = *lhs.clone() else {
                    panic!(
                        "Bad assignment made it through semantic analysis: {:?}",
                        *lhs
                    );
                };

                let src1 = self.tackify_expr(*lhs, instrs);
                let src2 = self.tackify_expr(*rhs, instrs);
                let tmp_dst = self.new_var(ty, "c_tmp");

                instrs.push(Instr::Binary {
                    binop: op,
                    src1,
                    src2,
                    dst: tmp_dst,
                });

                instrs.push(Instr::Copy {
                    src: tmp_dst,
                    dst: Val::Var(id),
                });

                Val::Var(id)
            }
            TypedExpression::Var(_ty, id) => Val::Var(id),
            TypedExpression::Assign(_ty, lhs, expr) => {
                let result = self.tackify_expr(*expr, instrs);

                let TypedExpression::Var(_, id) = *lhs else {
                    panic!(
                        "Bad assignment made it through semantic analysis: {:?}",
                        *lhs
                    )
                };

                instrs.push(Instr::Copy {
                    src: result,
                    dst: Val::Var(id),
                });
                Val::Var(id)
            }
            TypedExpression::Crement(ty, fixity, crement, expr) => {
                let op = Self::convert_crement(crement);

                let name = if crement == Crement::Inc {
                    "inc"
                } else {
                    "dec"
                };
                let tmp_dst = self.new_var(ty, name);

                let src = self.tackify_expr(*expr, instrs);

                instrs.extend(vec![
                    Instr::Copy { src, dst: tmp_dst },
                    Instr::Binary {
                        binop: op,
                        src1: tmp_dst,
                        src2: Val::Constant(Const::Int(1)),
                        dst: src,
                    },
                ]);

                if fixity == Fixity::Pre { src } else { tmp_dst }
            }
            TypedExpression::Conditional(ty, cond_expr, if_expr, else_expr) => {
                let cond_expr = self.tackify_expr(*cond_expr, instrs);
                let end_label = self.new_temp("cond_end");
                let else_label = self.new_temp("cond_else");
                let cond_dst = self.new_var(ty, "cond_result");
                instrs.push(Instr::JumpIfZero {
                    condition: cond_expr,
                    target: else_label,
                });
                let if_expr = self.tackify_expr(*if_expr, instrs);
                instrs.extend(vec![
                    Instr::Copy {
                        src: if_expr,
                        dst: cond_dst,
                    },
                    Instr::Jump { target: end_label },
                    Instr::Label(else_label),
                ]);
                let else_expr = self.tackify_expr(*else_expr, instrs);
                instrs.extend(vec![
                    Instr::Copy {
                        src: else_expr,
                        dst: cond_dst,
                    },
                    Instr::Label(end_label),
                ]);
                cond_dst
            }
            TypedExpression::Call(ty, name, param_exprs) => {
                let mut params = Vec::with_capacity(param_exprs.len());
                for param in param_exprs {
                    params.push(self.tackify_expr(param, instrs));
                }
                let dst = self.new_var(ty, "call");
                instrs.push(Instr::Call { name, params, dst });

                dst
            }
            TypedExpression::Cast(ty, expr) => {
                let src = self.tackify_expr(*expr.clone(), instrs);
                if ty == *get_type(&expr) {
                    return src;
                }
                let dst = self.new_var(ty.clone(), "cast");

                if ty == Type::Long {
                    instrs.push(Instr::SignExtend { src, dst });
                } else {
                    instrs.push(Instr::Truncate { src, dst })
                }
                dst
            }
        }
    }

    fn new_temp(&mut self, var_name: &'static str) -> Symbol {
        let count = self.count;
        self.count += 1;
        self.interner.intern(format!("{}.{}", var_name, count))
    }

    fn new_var(&mut self, ty: Type, var_name: &'static str) -> Val {
        let name = self.new_temp(var_name);
        self.symbols.insert(name, (ty, Attrs::Local));
        Val::Var(name)
    }

    fn block_label(&mut self, label_kind: &'static str, block: Symbol) -> Symbol {
        let block_str = self.interner.get_symbol(block);
        let formatted = format!("{}{}", label_kind, block_str);
        self.interner.intern(formatted)
    }

    fn convert_crement(crement: Crement) -> BinaryOp {
        match crement {
            Crement::Dec => BinaryOp::Subtract,
            Crement::Inc => BinaryOp::Add,
        }
    }

    fn convert_unop(unop: UnaryOperator) -> UnaryOp {
        match unop {
            UnaryOperator::Complement => UnaryOp::Complement,
            UnaryOperator::Negate => UnaryOp::Negate,
            UnaryOperator::Not => UnaryOp::Not,
        }
    }

    fn convert_binop(binop: BinaryOperator) -> BinaryOp {
        match binop {
            BinaryOperator::Add => BinaryOp::Add,
            BinaryOperator::Subtract => BinaryOp::Subtract,
            BinaryOperator::Multiply => BinaryOp::Multiply,
            BinaryOperator::Divide => BinaryOp::Divide,
            BinaryOperator::Remainder => BinaryOp::Remainder,
            BinaryOperator::BitAnd => BinaryOp::BitAnd,
            BinaryOperator::BitOr => BinaryOp::BitOr,
            BinaryOperator::BitXOr => BinaryOp::BitXOr,
            BinaryOperator::ShiftLeft => BinaryOp::ShiftLeft,
            BinaryOperator::ShiftRight => BinaryOp::ShiftRight,
            BinaryOperator::Equal => BinaryOp::Equals,
            BinaryOperator::NotEqual => BinaryOp::NotEquals,
            BinaryOperator::Greater => BinaryOp::GreaterThan,
            BinaryOperator::Less => BinaryOp::LessThan,
            BinaryOperator::GreaterOrEqual => BinaryOp::GreaterThanEquals,
            BinaryOperator::LessOrEqual => BinaryOp::LessThanEquals,
            binop => panic!("Unexpected binary operator {:?}", binop),
        }
    }

    fn convert_compound_op(compound_op: CompoundOperator) -> BinaryOp {
        match compound_op {
            CompoundOperator::Add => BinaryOp::Add,
            CompoundOperator::Subtract => BinaryOp::Subtract,
            CompoundOperator::Multiply => BinaryOp::Multiply,
            CompoundOperator::Divide => BinaryOp::Divide,
            CompoundOperator::Remainder => BinaryOp::Remainder,
            CompoundOperator::BitAnd => BinaryOp::BitAnd,
            CompoundOperator::BitOr => BinaryOp::BitOr,
            CompoundOperator::BitXOr => BinaryOp::BitXOr,
            CompoundOperator::ShiftLeft => BinaryOp::ShiftLeft,
            CompoundOperator::ShiftRight => BinaryOp::ShiftRight,
        }
    }
}
