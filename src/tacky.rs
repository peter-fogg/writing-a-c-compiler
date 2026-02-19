use crate::parser::{
    BinaryOperator, BlockItem, CompoundOperator, Declaration, Expression, Program, Statement,
    UnaryOperator,
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
}

#[derive(Debug, PartialEq, Clone)]
pub enum Tacky {
    Function {
        name: String,
        instructions: Vec<Instr>,
    },
}

struct TackifyState {
    count: u8,
}

pub fn emit_tacky(ast: Program) -> Tacky {
    let Program::Function(name, block_items) = ast;
    let mut instructions = Vec::new();

    let mut tackify_state = TackifyState::new();
    for block_item in block_items {
        match block_item {
            BlockItem::D(decl) => tackify_state.tackify_declaration(decl, &mut instructions),
            BlockItem::S(stmt) => tackify_state.tackify_statement(stmt, &mut instructions),
        }
    }
    instructions.push(Instr::Return(Val::Constant(0)));
    Tacky::Function { name, instructions }
}

impl TackifyState {
    pub fn new() -> Self {
        Self { count: 0 }
    }

    fn tackify_declaration(&mut self, decl: Declaration, instrs: &mut Vec<Instr>) {
        if let Some(expr) = decl.init {
            let expr = self.tackify_expr(expr, instrs);
            instrs.push(Instr::Copy {
                src: expr,
                dst: Val::Var(decl.name),
            });
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
        }
    }

    fn new_temp(&mut self, var_name: &'static str) -> String {
        let count = self.count;
        self.count += 1;
        format!("{}.{}", var_name, count)
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
