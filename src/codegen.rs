use crate::parser;

type Register<'a> = &'a str;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operand<'a> {
    Imm(i32),
    Reg(Register<'a>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Instr<'a> {
    Ret,
    Mov { src: Operand<'a>, dst: Operand<'a> },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Assembly<'a> {
    Function {
        name: &'a str,
        instructions: Vec<Instr<'a>>,
    },
}

pub fn assemble<'a>(ast: parser::ParseOutput<'a>) -> Assembly<'a> {
    let parser::Program::Function(name, stmts) = ast;
    let instrs = assemble_instructions(stmts);
    Assembly::Function {
        name: name,
        instructions: instrs,
    }
}

fn assemble_instructions<'a>(statements: parser::Statement) -> Vec<Instr<'a>> {
    let parser::Statement::Return(parser::Expression::Constant(n)) = statements;
    vec![
        Instr::Mov {
            src: Operand::Imm(n),
            dst: Operand::Reg("%eax"),
        },
        Instr::Ret,
    ]
}
