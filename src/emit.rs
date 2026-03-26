use std::io::Result;
use std::{fs::File, io::Write};

use crate::codegen::{
    AsmTopLevel, Assembly, BinaryOp, CondCode, Instr, Operand, Register, UnaryOp,
};

pub fn emit(asm: Assembly, mut file: File) -> Result<()> {
    for top_level in asm {
        emit_top_level(top_level, &mut file)?
    }
    Ok(())
}

fn emit_top_level(top_level: AsmTopLevel, file: &mut File) -> Result<()> {
    match top_level {
        AsmTopLevel::AsmFunction {
            name,
            instructions,
            global,
        } => {
            if global {
                file.write_all(format!("\t.globl _{}\n", name).as_bytes())?;
            }
            file.write_all(format!("_{}:\n", name).as_bytes())?;
            file.write_all("\tpushq\t%rbp\n".as_bytes())?;
            file.write_all("\tmovq\t%rsp, %rbp\n".as_bytes())?;
            for instr in instructions {
                emit_instr(instr, file)?;
            }
        }
        AsmTopLevel::AsmStatic { name, global, init } => {
            if global {
                file.write_all(format!("\t.globl _{}\n", name).as_bytes())?;
            }
            if init == 0 {
                file.write_all("\t.bss\n".as_bytes())?;
            } else {
                file.write_all("\t.data\n".as_bytes())?;
            }
            file.write_all("\t.balign 4\n".as_bytes())?;
            file.write_all(format!("_{}:\n", name).as_bytes())?;
            if init == 0 {
                file.write_all("\t.zero 4\n".as_bytes())?;
            } else {
                file.write_all(format!("\t.long {}\n", init).as_bytes())?;
            }
        }
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
        Instr::DeallocateStack(n) => {
            file.write_all(format!("\taddq\t${}, %rsp\n", n).as_bytes())?
        }
        Instr::Unary { unop, dst: operand } => file.write_all(
            format!("\t{}\t{}\n", write_unop(unop), write_operand(operand, 4)).as_bytes(),
        )?,
        Instr::Binary { binop, src, dst } => file.write_all(
            format!(
                "\t{}\t{}, {}\n",
                write_binop(binop),
                if matches!(binop, BinaryOp::ShiftLeft | BinaryOp::ShiftRight) {
                    write_operand(src, 1)
                } else {
                    write_operand(src, 4)
                },
                write_operand(dst, 4),
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
        Instr::Call(name) => file.write_all(format!("\tcall _{}\n", name).as_bytes())?,

        Instr::Push(operand) => {
            file.write_all(format!("\tpushq {}\n", write_operand(operand, 8)).as_bytes())?
        }
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
        Operand::Stack(offset) => format!("{}(%rbp)", offset),
        Operand::Pseudo(s) => panic!("Pseudo operand {} not replaced", s),
        Operand::Data(var) => format!("_{}(%rip)", var),
    }
}

fn write_register(reg: Register, bytes: u8) -> String {
    match reg {
        Register::AX | Register::CX | Register::DX => write_x_register(reg, bytes),
        Register::R8 | Register::R9 | Register::R10 | Register::R11 => {
            write_numeric_register(reg, bytes)
        }
        Register::DI | Register::SI => write_i_register(reg, bytes),
    }
}

fn write_numeric_register(reg: Register, bytes: u8) -> String {
    let suffix = match bytes {
        8 => "",
        4 => "d",
        1 => "b",
        n => panic!("Bad number of bytes for register {:?}, {}", reg, n),
    };
    let num = match reg {
        Register::R8 => 8,
        Register::R9 => 9,
        Register::R10 => 10,
        Register::R11 => 11,
        r => panic!("Bad numeric register {:?}", r),
    };
    format!("%r{}{}", num, suffix)
}

fn write_x_register(reg: Register, bytes: u8) -> String {
    let (prefix, suffix) = match bytes {
        8 => ("r", "x"),
        4 => ("e", "x"),
        1 => ("", "l"),
        n => panic!("Bad number of bytes for register {:?}, {}", reg, n),
    };

    let letter = match reg {
        Register::AX => "a",
        Register::CX => "c",
        Register::DX => "d",
        r => panic!("Bad x register {:?}", r),
    };

    format!("%{}{}{}", prefix, letter, suffix)
}

fn write_i_register(reg: Register, bytes: u8) -> String {
    let (prefix, suffix) = match bytes {
        8 => ("r", ""),
        4 => ("e", ""),
        1 => ("", "l"),
        _ => panic!("Bad number of bytes for register {:?}, {}", reg, bytes),
    };

    let letter = match reg {
        Register::DI => "di",
        Register::SI => "si",
        r => panic!("Bad i register {:?}", r),
    };

    format!("%{}{}{}", prefix, letter, suffix)
}
