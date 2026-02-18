use std::io::Result;
use std::{fs::File, io::Write};

use crate::codegen::{Assembly, BinaryOp, CondCode, Instr, Operand, Register, UnaryOp};

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
            format!(
                "\tmovl\t{}, {}\n",
                write_operand(src, 4),
                write_operand(dst, 4)
            )
            .as_bytes(),
        )?,
        Instr::AllocateStack(n) => file.write_all(format!("\tsubq\t${}, %rsp\n", n).as_bytes())?,
        Instr::Unary { unop, dst: operand } => file.write_all(
            format!("\t{}\t{}\n", write_unop(unop), write_operand(operand, 4)).as_bytes(),
        )?,
        Instr::Binary { binop, src, dst } => file.write_all(
            format!(
                "\t{}\t{}, {}\n",
                write_binop(binop),
                write_operand(src, 4),
                write_operand(dst, 4)
            )
            .as_bytes(),
        )?,
        Instr::IDiv(operand) => {
            file.write_all(format!("\tidivl\t{}\n", write_operand(operand, 4)).as_bytes())?
        }
        Instr::Cdq => file.write_all("\tcdq\n".as_bytes())?,
        Instr::Cmp { lhs, rhs } => file.write_all(
            format!(
                "\tcmpl\t{}, {}\n",
                write_operand(lhs, 4),
                write_operand(rhs, 4)
            )
            .as_bytes(),
        )?,
        Instr::Jmp(label) => file.write_all(format!("\tjmp\t.L{}\n", label).as_bytes())?,
        Instr::JmpCC(cond_code, label) => {
            file.write_all(format!("\tj{}\t.L{}\n", write_cond_code(cond_code), label).as_bytes())?
        }
        Instr::SetCC(cond_code, operand) => file.write_all(
            format!(
                "\tset{}\t{}\n",
                write_cond_code(cond_code),
                write_operand(operand, 1)
            )
            .as_bytes(),
        )?,
        Instr::Label(label) => file.write_all(format!(".L{}:\n", label).as_bytes())?,
    }
    Ok(())
}

fn write_cond_code(code: CondCode) -> String {
    match code {
        CondCode::E => "e",
        CondCode::NE => "ne",
        CondCode::LE => "le",
        CondCode::GE => "ge",
        CondCode::L => "l",
        CondCode::G => "g",
    }
    .to_string()
}

fn write_unop(unop: UnaryOp) -> String {
    match unop {
        UnaryOp::Neg => "negl",
        UnaryOp::Not => "notl",
    }
    .to_string()
}

fn write_binop(binop: BinaryOp) -> String {
    match binop {
        BinaryOp::Add => "addl",
        BinaryOp::Sub => "subl",
        BinaryOp::Mult => "imull",
        BinaryOp::BitAnd => "andl",
        BinaryOp::BitOr => "orl",
        BinaryOp::BitXOr => "xorl",
        BinaryOp::ShiftLeft => "shll",
        BinaryOp::ShiftRight => "sarl",
    }
    .to_string()
}

fn write_operand(op: Operand, bytes: u8) -> String {
    match op {
        Operand::Reg(reg) => write_register(reg, bytes),
        Operand::Imm(n) => format!("${}", n),
        Operand::Stack(offset) => format!("-{}(%rbp)", offset),
        Operand::Pseudo(s) => panic!("Pseudo operand {} not replaced", s),
    }
}

fn write_register(reg: Register, bytes: u8) -> String {
    match reg {
        Register::AX => {
            if bytes == 4 {
                "%eax"
            } else {
                "%al"
            }
        }
        Register::DX => {
            if bytes == 4 {
                "%edx"
            } else {
                "%dl"
            }
        }
        Register::R10 => {
            if bytes == 4 {
                "%r10d"
            } else {
                "%r10b"
            }
        }
        Register::R11 => {
            if bytes == 4 {
                "%r11d"
            } else {
                "%r11b"
            }
        }
        Register::CX => "%ecx",
        Register::CL => "%cl",
    }
    .to_string()
}
