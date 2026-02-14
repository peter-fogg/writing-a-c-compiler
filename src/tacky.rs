use crate::parser::{self, UnaryOperator};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    Complement,
    Negate,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Val {
    Constant(i32),
    Var(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Instr {
    Return(Val),
    Unary(UnaryOp, Val, Val),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Tacky<'a> {
    Function {
        name: &'a str,
        instructions: Vec<Instr>,
    },
}

struct TackifyState {
    count: u8,
}

pub fn emit_tacky<'a>(ast: parser::ParseOutput<'a>) -> Tacky<'a> {
    let parser::Program::Function(name, statement) = ast;
    let parser::Statement::Return(expr) = statement;
    let mut instructions = Vec::new();
    let mut tackify_state = TackifyState::new();
    let result = tackify_state.tackify_expr(expr, &mut instructions);
    instructions.push(Instr::Return(result));
    Tacky::Function { name, instructions }
}

impl TackifyState {
    pub fn new() -> Self {
        Self { count: 0 }
    }

    fn tackify_expr(&mut self, expr: parser::Expression, instrs: &mut Vec<Instr>) -> Val {
        match expr {
            parser::Expression::Constant(n) => Val::Constant(n),
            parser::Expression::Unary(un_op, inner) => {
                let src = self.tackify_expr(*inner, instrs);
                let dst_name = self.new_temp("tmp");
                let dst = Val::Var(dst_name);
                let op = Self::convert_unop(un_op);
                let new_unop = Instr::Unary(op, src, dst.clone());
                instrs.push(new_unop);
                dst
            }
        }
    }

    fn new_temp(&mut self, var_name: &'static str) -> String {
        let count = self.count;
        self.count += 1;
        format!("{}.{}", var_name, count)
    }

    fn convert_unop(unop: parser::UnaryOperator) -> UnaryOp {
        match unop {
            UnaryOperator::Complement => UnaryOp::Complement,
            UnaryOperator::Negate => UnaryOp::Negate,
        }
    }
}
