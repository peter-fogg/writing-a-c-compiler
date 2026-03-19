use crate::parser::{
    BinaryOperator, BlockItem, CaseInfo, CompoundOperator, Crement, Declaration, Expression,
    Fixity, ForInit, Function, Statement, UnaryOperator, Var,
};

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

#[derive(Debug, PartialEq, Clone)]
pub enum Val {
    Constant(i32),
    Var(String),
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
        target: String,
    },
    JumpIfZero {
        condition: Val,
        target: String,
    },
    JumpIfNotZero {
        condition: Val,
        target: String,
    },
    Label(String),
    Call {
        name: String,
        params: Vec<Val>,
        dst: Val,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct TackyFunction {
    pub name: String,
    pub params: Vec<String>,
    pub instructions: Vec<Instr>,
}

pub type Tacky = Vec<TackyFunction>;

struct TackifyState {
    count: u8,
}

pub fn emit_tacky(functions: Vec<Function>) -> Tacky {
    let mut program = Vec::new();

    let mut tackify_state = TackifyState::new();

    for function in functions {
        tackify_state.tackify_function(function, &mut program);
    }

    program
}

impl TackifyState {
    pub fn new() -> Self {
        Self { count: 0 }
    }

    fn tackify_function(&mut self, Function { name, params, body }: Function, program: &mut Tacky) {
        if let Some(body) = body {
            let mut instructions = Vec::new();
            let name = name.clone();
            self.tackify_block(body, &mut instructions);
            instructions.push(Instr::Return(Val::Constant(0)));
            program.push(TackyFunction {
                name,
                params: params,
                instructions,
            });
        }
    }

    fn tackify_block(&mut self, block_items: Vec<BlockItem>, instrs: &mut Vec<Instr>) {
        for block_item in block_items {
            match block_item {
                BlockItem::D(decl) => self.tackify_declaration(decl, instrs),
                BlockItem::S(stmt) => self.tackify_statement(stmt, instrs),
            }
        }
    }

    fn tackify_declaration(&mut self, decl: Declaration, instrs: &mut Vec<Instr>) {
        match decl {
            Declaration::Var(Var { name, init }) => {
                if let Some(expr) = init {
                    let expr = self.tackify_expr(expr, instrs);
                    instrs.push(Instr::Copy {
                        src: expr,
                        dst: Val::Var(name),
                    });
                }
            }
            //Declaration::Func(function) => self.tackify_function(function, instrs),
            Declaration::Func(_) => (),
        }
    }

    fn tackify_statement(&mut self, stmt: Statement, instrs: &mut Vec<Instr>) {
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
                    target: else_label.clone(),
                });
                self.tackify_statement(*if_stmt, instrs);
                instrs.push(Instr::Jump {
                    target: end_label.clone(),
                });
                instrs.push(Instr::Label(else_label));
                self.tackify_statement(*else_stmt, instrs);
                instrs.push(Instr::Label(end_label));
            }
            Statement::If(cond, if_stmt, None) => {
                let cond = self.tackify_expr(cond, instrs);
                let end_label = self.new_temp("if_end");
                instrs.push(Instr::JumpIfZero {
                    condition: cond,
                    target: end_label.clone(),
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
                target: "break".to_owned() + &label,
            }),
            Statement::Continue(label) => instrs.push(Instr::Jump {
                target: "continue".to_owned() + &label,
            }),
            Statement::DoWhile(label, body, cond) => {
                instrs.push(Instr::Label(label.clone()));
                self.tackify_statement(*body, instrs);
                instrs.push(Instr::Label("continue".to_owned() + &label));
                let cond = self.tackify_expr(cond, instrs);
                instrs.push(Instr::JumpIfNotZero {
                    condition: cond,
                    target: label.clone(),
                });
                instrs.push(Instr::Label("break".to_owned() + &label));
            }
            Statement::While(label, cond, body) => {
                instrs.push(Instr::Label("continue".to_owned() + &label));
                let cond = self.tackify_expr(cond, instrs);
                instrs.push(Instr::JumpIfZero {
                    condition: cond,
                    target: "break".to_owned() + &label,
                });
                self.tackify_statement(*body, instrs);
                instrs.push(Instr::Jump {
                    target: "continue".to_owned() + &label,
                });
                instrs.push(Instr::Label("break".to_owned() + &label));
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
                instrs.push(Instr::Label(label.clone()));
                if let Some(expr) = cond {
                    let result = self.tackify_expr(expr, instrs);
                    instrs.push(Instr::JumpIfZero {
                        condition: result,
                        target: "break".to_owned() + &label,
                    });
                }
                self.tackify_statement(*body, instrs);
                instrs.push(Instr::Label("continue".to_owned() + &label));
                if let Some(expr) = post {
                    self.tackify_expr(expr, instrs);
                }
                instrs.extend(vec![
                    Instr::Jump {
                        target: label.clone(),
                    },
                    Instr::Label("break".to_owned() + &label),
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
                            let dst = Val::Var(self.new_temp("case_tmp"));
                            instrs.push(Instr::Binary {
                                binop,
                                src1: val,
                                src2: result.clone(),
                                dst: dst.clone(),
                            });
                            instrs.push(Instr::JumpIfNotZero {
                                condition: dst,
                                target: label.to_string(),
                            })
                        }
                        _ => unreachable!(),
                    }
                }
                if default.len() == 1 {
                    match default[0] {
                        CaseInfo::Default { label } => instrs.push(Instr::Jump {
                            target: label.to_string(),
                        }),
                        _ => unreachable!(),
                    }
                }
                instrs.push(Instr::Jump {
                    target: "break".to_owned() + &label,
                });
                self.tackify_statement(*body, instrs);
                instrs.push(Instr::Label("break".to_owned() + &label));
            }
        }
    }

    fn tackify_expr(&mut self, expr: Expression, instrs: &mut Vec<Instr>) -> Val {
        match expr {
            Expression::Constant(n) => Val::Constant(n),
            Expression::Unary(un_op, inner) => {
                let src = self.tackify_expr(*inner, instrs);
                let dst_name = self.new_temp("tmp");
                let dst = Val::Var(dst_name);
                let op = Self::convert_unop(un_op);
                let new_unop = Instr::Unary {
                    unop: op,
                    src,
                    dst: dst.clone(),
                };
                instrs.push(new_unop);
                dst
            }
            Expression::Binary(BinaryOperator::And, lhs, rhs) => {
                let end_label = self.new_temp("and_end");
                let false_label = self.new_temp("and_false");
                let ret_val = Val::Var(self.new_temp("and_result"));

                let lhs = self.tackify_expr(*lhs, instrs);

                instrs.push(Instr::JumpIfZero {
                    condition: lhs,
                    target: false_label.clone(),
                });
                let rhs = self.tackify_expr(*rhs, instrs);
                instrs.extend(vec![
                    Instr::JumpIfZero {
                        condition: rhs,
                        target: false_label.clone(),
                    },
                    Instr::Copy {
                        src: Val::Constant(1),
                        dst: ret_val.clone(),
                    },
                    Instr::Jump {
                        target: end_label.clone(),
                    },
                    Instr::Label(false_label),
                    Instr::Copy {
                        src: Val::Constant(0),
                        dst: ret_val.clone(),
                    },
                    Instr::Label(end_label),
                ]);

                ret_val
            }
            Expression::Binary(BinaryOperator::Or, lhs, rhs) => {
                let end_label = self.new_temp("or_end");
                let true_label = self.new_temp("or_true");
                let ret_val = Val::Var(self.new_temp("or_result"));

                let lhs = self.tackify_expr(*lhs, instrs);
                instrs.push(Instr::JumpIfNotZero {
                    condition: lhs,
                    target: true_label.clone(),
                });
                let rhs = self.tackify_expr(*rhs, instrs);
                instrs.extend(vec![
                    Instr::JumpIfNotZero {
                        condition: rhs,
                        target: true_label.clone(),
                    },
                    Instr::Copy {
                        src: Val::Constant(0),
                        dst: ret_val.clone(),
                    },
                    Instr::Jump {
                        target: end_label.clone(),
                    },
                    Instr::Label(true_label),
                    Instr::Copy {
                        src: Val::Constant(1),
                        dst: ret_val.clone(),
                    },
                    Instr::Label(end_label),
                ]);

                ret_val
            }
            Expression::Binary(binop, lhs, rhs) => {
                let src1 = self.tackify_expr(*lhs, instrs);
                let src2 = self.tackify_expr(*rhs, instrs);
                let dst = Val::Var(self.new_temp("tmp"));

                let op = Self::convert_binop(binop);

                let new_binop = Instr::Binary {
                    binop: op,
                    src1,
                    src2,
                    dst: dst.clone(),
                };

                instrs.push(new_binop);

                dst
            }
            Expression::Compound(compound_op, lhs, rhs) => {
                let op = Self::convert_compound_op(compound_op);

                let Expression::Var(id) = *lhs.clone() else {
                    panic!(
                        "Bad assignment made it through semantic analysis: {:?}",
                        *lhs
                    );
                };

                let src1 = self.tackify_expr(*lhs, instrs);
                let src2 = self.tackify_expr(*rhs, instrs);
                let tmp_dst = Val::Var(self.new_temp("c_tmp"));

                instrs.push(Instr::Binary {
                    binop: op,
                    src1,
                    src2,
                    dst: tmp_dst.clone(),
                });

                instrs.push(Instr::Copy {
                    src: tmp_dst,
                    dst: Val::Var(id.clone()),
                });

                Val::Var(id)
            }
            Expression::Var(id) => Val::Var(id),
            Expression::Assign(lhs, expr) => {
                let result = self.tackify_expr(*expr, instrs);

                let Expression::Var(id) = *lhs else {
                    panic!(
                        "Bad assignment made it through semantic analysis: {:?}",
                        *lhs
                    )
                };

                instrs.push(Instr::Copy {
                    src: result,
                    dst: Val::Var(id.clone()),
                });
                Val::Var(id)
            }
            Expression::Crement(fixity, crement, expr) => {
                let op = Self::convert_crement(crement);

                let name = if crement == Crement::Inc {
                    "inc"
                } else {
                    "dec"
                };
                let tmp_dst = Val::Var(self.new_temp(name));

                let src = self.tackify_expr(*expr, instrs);

                instrs.extend(vec![
                    Instr::Copy {
                        src: src.clone(),
                        dst: tmp_dst.clone(),
                    },
                    Instr::Binary {
                        binop: op,
                        src1: tmp_dst.clone(),
                        src2: Val::Constant(1),
                        dst: src.clone(),
                    },
                ]);

                if fixity == Fixity::Pre { src } else { tmp_dst }
            }
            Expression::Conditional(cond_expr, if_expr, else_expr) => {
                let cond_expr = self.tackify_expr(*cond_expr, instrs);
                let end_label = self.new_temp("cond_end");
                let else_label = self.new_temp("cond_else");
                let cond_dst = Val::Var(self.new_temp("cond_result"));
                instrs.push(Instr::JumpIfZero {
                    condition: cond_expr,
                    target: else_label.clone(),
                });
                let if_expr = self.tackify_expr(*if_expr, instrs);
                instrs.extend(vec![
                    Instr::Copy {
                        src: if_expr,
                        dst: cond_dst.clone(),
                    },
                    Instr::Jump {
                        target: end_label.clone(),
                    },
                    Instr::Label(else_label),
                ]);
                let else_expr = self.tackify_expr(*else_expr, instrs);
                instrs.extend(vec![
                    Instr::Copy {
                        src: else_expr,
                        dst: cond_dst.clone(),
                    },
                    Instr::Label(end_label),
                ]);
                cond_dst
            }
            Expression::Call(name, param_exprs) => {
                let mut params = Vec::with_capacity(param_exprs.len());
                for param in param_exprs {
                    params.push(self.tackify_expr(param, instrs));
                }
                let dst = Val::Var(self.new_temp("call"));
                instrs.push(Instr::Call {
                    name,
                    params,
                    dst: dst.clone(),
                });

                dst
            }
        }
    }

    fn new_temp(&mut self, var_name: &'static str) -> String {
        let count = self.count;
        self.count += 1;
        format!("{}.{}", var_name, count)
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
