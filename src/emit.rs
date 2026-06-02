use std::io::Error;
use std::io::Result;
use std::{fs::File, io::Write};

use crate::codegen::{
    AsmTopLevel, AsmType, Assembly, BinaryOp, CondCode, Instr, Operand, Register, UnaryOp,
};
use crate::error::CompileError;
use crate::interner::Interner;
use crate::typecheck::StaticInit;

pub fn emit(asm: Assembly, interner: &Interner, mut file: File) -> Result<()> {
    for top_level in asm {
        emit_top_level(top_level, interner, &mut file)?
    }
    Ok(())
}

fn emit_top_level(top_level: AsmTopLevel, interner: &Interner, file: &mut File) -> Result<()> {
    match top_level {
        AsmTopLevel::AsmFunction {
            name,
            instructions,
            global,
        } => {
            if global {
                file.write_all(format!("\t.globl _{}\n", interner.get_symbol(name)).as_bytes())?;
            }
            file.write_all(format!("_{}:\n", interner.get_symbol(name)).as_bytes())?;
            file.write_all("\tpushq\t%rbp\n".as_bytes())?;
            file.write_all("\tmovq\t%rsp, %rbp\n".as_bytes())?;
            for instr in instructions {
                emit_instr(instr, interner, file)?;
            }
        }
        AsmTopLevel::AsmStatic {
            alignment,
            name,
            global,
            init,
        } => {
            if global {
                file.write_all(format!("\t.globl _{}\n", interner.get_symbol(name)).as_bytes())?;
            }
            if matches!(
                init,
                StaticInit::Int(0)
                    | StaticInit::Long(0)
                    | StaticInit::UInt(0)
                    | StaticInit::ULong(0)
            ) {
                file.write_all("\t.bss\n".as_bytes())?;
            } else {
                file.write_all("\t.data\n".as_bytes())?;
            }
            file.write_all(format!("\t.balign {alignment}\n").as_bytes())?;
            file.write_all(format!("_{}:\n", interner.get_symbol(name)).as_bytes())?;
            file.write_all(format!("\t{}\n", format_static_init(init)).as_bytes())?;
        }
    }
    Ok(())
}

fn format_static_init(init: StaticInit) -> String {
    match init {
        StaticInit::Int(0) => String::from(".zero 4"),
        StaticInit::UInt(0) => String::from(".zero 4"),
        StaticInit::Int(n) => format!(".long {n}"),
        StaticInit::UInt(n) => format!(".long {n}"),
        StaticInit::Long(0) => String::from(".zero 8"),
        StaticInit::ULong(0) => String::from(".zero 8"),
        StaticInit::Long(n) => format!(".quad {n}"),
        StaticInit::ULong(n) => format!(".quad {n}"),
    }
}

fn emit_instr(instr: Instr, interner: &Interner, file: &mut File) -> Result<()> {
    match instr {
        Instr::Ret => {
            file.write_all("\tmovq \t%rbp, %rsp\n".as_bytes())?;
            file.write_all("\tpopq\t%rbp\n".as_bytes())?;
            file.write_all("\tret\n".as_bytes())?;
        }
        Instr::Mov { ty, src, dst } => file.write_all(
            format!(
                "\tmov{}\t{}, {}\n",
                type_suffix(ty),
                write_operand(src, bytes(ty), interner),
                write_operand(dst, bytes(ty), interner)
            )
            .as_bytes(),
        )?,
        Instr::Unary {
            ty,
            unop,
            dst: operand,
        } => file.write_all(
            format!(
                "\t{}\t{}\n",
                write_unop(unop, ty),
                write_operand(operand, bytes(ty), interner)
            )
            .as_bytes(),
        )?,
        Instr::Binary {
            ty,
            binop,
            src,
            dst,
        } => file.write_all(
            format!(
                "\t{}\t{}, {}\n",
                write_binop(binop, ty),
                if matches!(binop, BinaryOp::ShiftLeft | BinaryOp::ShiftRight) {
                    write_operand(src, 1, interner)
                } else {
                    write_operand(src, bytes(ty), interner)
                },
                write_operand(dst, bytes(ty), interner),
            )
            .as_bytes(),
        )?,
        Instr::IDiv(ty, operand) => file.write_all(
            format!(
                "\tidiv{}\t{}\n",
                type_suffix(ty),
                write_operand(operand, bytes(ty), interner)
            )
            .as_bytes(),
        )?,
        Instr::Div(ty, operand) => file.write_all(
            format!(
                "\tdiv{}\t{}\n",
                type_suffix(ty),
                write_operand(operand, bytes(ty), interner)
            )
            .as_bytes(),
        )?,
        Instr::Cdq(AsmType::Longword) => file.write_all("\tcdq\n".as_bytes())?,
        Instr::Cdq(AsmType::Quadword) => file.write_all("\tcqo\n".as_bytes())?,
        Instr::Cmp { ty, lhs, rhs } => file.write_all(
            format!(
                "\tcmp{}\t{}, {}\n",
                type_suffix(ty),
                write_operand(lhs, bytes(ty), interner),
                write_operand(rhs, bytes(ty), interner)
            )
            .as_bytes(),
        )?,
        Instr::Jmp(label) => {
            file.write_all(format!("\tjmp\t.L{}\n", interner.get_symbol(label)).as_bytes())?
        }
        Instr::JmpCC(cond_code, label) => file.write_all(
            format!(
                "\tj{}\t.L{}\n",
                write_cond_code(cond_code),
                interner.get_symbol(label)
            )
            .as_bytes(),
        )?,
        Instr::SetCC(cond_code, operand) => file.write_all(
            format!(
                "\tset{}\t{}\n",
                write_cond_code(cond_code),
                write_operand(operand, 1, interner)
            )
            .as_bytes(),
        )?,
        Instr::Label(label) => {
            file.write_all(format!(".L{}:\n", interner.get_symbol(label)).as_bytes())?
        }
        Instr::Call(name) => {
            file.write_all(format!("\tcall\t_{}\n", interner.get_symbol(name)).as_bytes())?
        }

        Instr::Push(operand) => file
            .write_all(format!("\tpushq\t{}\n", write_operand(operand, 8, interner)).as_bytes())?,
        Instr::Movsx { src, dst } => file.write_all(
            format!(
                "\tmovslq\t{}, {}\n",
                write_operand(src, 4, interner),
                write_operand(dst, 8, interner)
            )
            .as_bytes(),
        )?,
        Instr::MovZeroExtend { .. } => {
            return Err(CompileError::Emit(String::from(
                "MovZeroExtend not fixed up",
            )))
            .map_err(Error::other);
        }
    }
    Ok(())
}

fn bytes(ty: AsmType) -> u8 {
    match ty {
        AsmType::Longword => 4,
        AsmType::Quadword => 8,
    }
}

fn write_cond_code(code: CondCode) -> String {
    match code {
        CondCode::E => "e",
        CondCode::NE => "ne",
        CondCode::LE => "le",
        CondCode::GE => "ge",
        CondCode::L => "l",
        CondCode::G => "g",
        CondCode::A => "a",
        CondCode::AE => "ae",
        CondCode::B => "b",
        CondCode::BE => "be",
    }
    .to_string()
}

fn write_unop(unop: UnaryOp, ty: AsmType) -> String {
    let instr = match unop {
        UnaryOp::Neg => "neg",
        UnaryOp::Not => "not",
    }
    .to_string();
    let suffix = type_suffix(ty);
    format!("{instr}{suffix}")
}

fn type_suffix(ty: AsmType) -> &'static str {
    match ty {
        AsmType::Longword => "l",
        AsmType::Quadword => "q",
    }
}

fn write_binop(binop: BinaryOp, ty: AsmType) -> String {
    let instr = match binop {
        BinaryOp::Add => "add",
        BinaryOp::Sub => "sub",
        BinaryOp::Mult => "imul",
        BinaryOp::BitAnd => "and",
        BinaryOp::BitOr => "or",
        BinaryOp::BitXOr => "xor",
        BinaryOp::ShiftLeft => "shl",
        BinaryOp::ShiftRight => "sar",
    }
    .to_string();
    let suffix = type_suffix(ty);
    format!("{instr}{suffix}")
}

fn write_operand(op: Operand, bytes: u8, interner: &Interner) -> String {
    match op {
        Operand::Reg(reg) => write_register(reg, bytes),
        Operand::Imm(n) => format!("${}", n),
        Operand::Stack(offset) => format!("{}(%rbp)", offset),
        Operand::Pseudo(s) => panic!("Pseudo operand {} not replaced", s),
        Operand::Data(var) => format!("_{}(%rip)", interner.get_symbol(var)),
    }
}

fn write_register(reg: Register, bytes: u8) -> String {
    match reg {
        Register::AX | Register::CX | Register::DX => write_x_register(reg, bytes),
        Register::R8 | Register::R9 | Register::R10 | Register::R11 => {
            write_numeric_register(reg, bytes)
        }
        Register::DI | Register::SI => write_i_register(reg, bytes),
        Register::SP => String::from("%rsp"),
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
