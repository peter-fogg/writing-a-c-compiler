use std::io::Result;
use std::{fs::File, io::Write};

use crate::codegen::{Assembly, Instr, Operand, Register, UnaryOp};

pub fn emit<'a>(
    Assembly::Function { name, instructions }: Assembly<'a>,
    mut file: File,
) -> Result<()> {
    file.write_all(format!("\t.globl _{}\n", name).as_bytes())?;
    file.write_all(format!("_{}:\n", name).as_bytes())?;
    file.write_all("\tpushq\t%rbp\n".as_bytes())?;
    file.write_all("\tmovq\t%rsp, %rbp\n".as_bytes())?;
    for instr in instructions {
        emit_instr(instr, &mut file)?;
    }
    Ok(())
}

fn emit_instr(instr: Instr, file: &mut File) -> Result<()> {
    match instr {
        Instr::Ret => {
            file.write_all("\tmovq \t%rbp, %rsp\n".as_bytes())?;
            file.write_all("\tpopq\t%rbp\n".as_bytes())?;
            file.write_all("\tret\n".as_bytes())?;
        }
        Instr::Mov { src, dst } => file.write_all(
            format!("\tmovl\t{}, {}\n", write_operand(src), write_operand(dst)).as_bytes(),
        )?,
        Instr::AllocateStack(n) => file.write_all(format!("\tsubq\t${}, %rsp\n", n).as_bytes())?,
        Instr::Unary(unop, operand) => file.write_all(
            format!("\t{}\t{}\n", write_unop(unop), write_operand(operand)).as_bytes(),
        )?,
    }
    Ok(())
}

fn write_unop(unop: UnaryOp) -> String {
    match unop {
        UnaryOp::Neg => "negl",
        UnaryOp::Not => "notl",
    }
    .to_string()
}

fn write_operand(op: Operand) -> String {
    match op {
        Operand::Reg(reg) => write_register(reg),
        Operand::Imm(n) => format!("${}", n),
        Operand::Stack(offset) => format!("-{}(%rbp)", offset),
        Operand::Pseudo(s) => panic!("Pseudo operand {} not replaced", s),
    }
}

fn write_register(reg: Register) -> String {
    match reg {
        Register::AX => "%eax",
        Register::R10 => "%r10d",
    }
    .to_string()
}
