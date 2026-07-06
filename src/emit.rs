use std::collections::HashMap;
use std::io::Error;
use std::io::Result;
use std::{fs::File, io::Write};

use crate::codegen::{
    AsmEntry, AsmTopLevel, AsmType, Assembly, BinaryOp, CondCode, Instr, Operand, Register, UnaryOp,
};
use crate::error::CompileError;
use crate::interner::{Interner, Symbol};
use crate::typecheck::StaticInit;

pub fn emit(
    asm: Assembly,
    symbols: &HashMap<Symbol, AsmEntry>,
    interner: &Interner,
    mut file: File,
) -> Result<()> {
    for top_level in asm {
        emit_top_level(top_level, symbols, interner, &mut file)?
    }
    Ok(())
}

fn emit_top_level(
    top_level: AsmTopLevel,
    symbols: &HashMap<Symbol, AsmEntry>,
    interner: &Interner,
    file: &mut File,
) -> Result<()> {
    match top_level {
        AsmTopLevel::Function {
            name,
            instructions,
            global,
            stack_size,
        } => {
            if global {
                file.write_all(format!("\t.globl _{}\n", interner.get_symbol(name)).as_bytes())?;
            }
            file.write_all(format!("_{}:\n", interner.get_symbol(name)).as_bytes())?;
            file.write_all("\tpushq\t%rbp\n".as_bytes())?;
            file.write_all("\tmovq\t%rsp, %rbp\n".as_bytes())?;
            // TODO allocate stack space here by subtracting stack offset from RSP
            file.write_all(format!("\tsubq\t${stack_size},\t%rsp\n").as_bytes())?;
            for instr in instructions {
                emit_instr(instr, interner, symbols, file)?;
            }
        }
        AsmTopLevel::StaticVar {
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
                // Nonzero integer or any double
                file.write_all("\t.data\n".as_bytes())?;
            }
            file.write_all(format!("\t.balign {alignment}\n").as_bytes())?;
            file.write_all(format!("_{}:\n", interner.get_symbol(name)).as_bytes())?;
            file.write_all(format!("\t{}\n", format_static_init(init)).as_bytes())?;
        }
        AsmTopLevel::StaticConst {
            name,
            init,
            alignment,
        } => {
            file.write_all(format!("\t.literal{alignment}\n").as_bytes())?;
            file.write_all(format!("\t.balign {alignment}\n").as_bytes())?;
            file.write_all(
                format!(
                    "{}_{}:\n",
                    format_local(symbols, name),
                    interner.get_symbol(name)
                )
                .as_bytes(),
            )?;
            file.write_all(format!("\t{}\n", format_static_init(init)).as_bytes())?;
            if alignment == 16 {
                file.write_all("\t.quad 0\n".as_bytes())?;
            }
        }
    }
    Ok(())
}

fn format_local(symbols: &HashMap<Symbol, AsmEntry>, name: Symbol) -> &'static str {
    if matches!(
        symbols.get(&name),
        Some(AsmEntry::Obj {
            is_constant: true,
            ..
        })
    ) {
        "L"
    } else {
        ""
    }
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
        StaticInit::Double(n) => format!(".quad {:#x} # {n}", n.to_bits()),
    }
}

fn emit_instr(
    instr: Instr,
    interner: &Interner,
    symbols: &HashMap<Symbol, AsmEntry>,
    file: &mut File,
) -> Result<()> {
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
                write_operand(src, bytes(ty), interner, symbols),
                write_operand(dst, bytes(ty), interner, symbols)
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
                write_operand(operand, bytes(ty), interner, symbols)
            )
            .as_bytes(),
        )?,
        Instr::Binary {
            ty: AsmType::Double,
            binop: BinaryOp::BitXOr,
            src,
            dst,
        } => file.write_all(
            format!(
                "\txorpd\t{}, {}\n",
                write_operand(src, bytes(AsmType::Double), interner, symbols),
                write_operand(dst, bytes(AsmType::Double), interner, symbols)
            )
            .as_bytes(),
        )?,
        Instr::Binary {
            ty: AsmType::Double,
            binop: BinaryOp::Mult,
            src,
            dst,
        } => file.write_all(
            format!(
                "\tmulsd\t{}, {}\n",
                write_operand(src, bytes(AsmType::Double), interner, symbols),
                write_operand(dst, bytes(AsmType::Double), interner, symbols)
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
                if matches!(
                    binop,
                    BinaryOp::ShiftLeft | BinaryOp::ShiftRight | BinaryOp::ShiftRightLogical
                ) {
                    write_operand(src, 1, interner, symbols)
                } else {
                    write_operand(src, bytes(ty), interner, symbols)
                },
                write_operand(dst, bytes(ty), interner, symbols),
            )
            .as_bytes(),
        )?,
        Instr::IDiv(ty, operand) => file.write_all(
            format!(
                "\tidiv{}\t{}\n",
                type_suffix(ty),
                write_operand(operand, bytes(ty), interner, symbols)
            )
            .as_bytes(),
        )?,
        Instr::Div(ty, operand) => file.write_all(
            format!(
                "\tdiv{}\t{}\n",
                type_suffix(ty),
                write_operand(operand, bytes(ty), interner, symbols)
            )
            .as_bytes(),
        )?,
        Instr::Cdq(AsmType::Longword) => file.write_all("\tcdq\n".as_bytes())?,
        Instr::Cdq(AsmType::Quadword) => file.write_all("\tcqo\n".as_bytes())?,
        Instr::Cdq(AsmType::Double) => unreachable!(),
        Instr::Cmp {
            ty: AsmType::Double,
            lhs,
            rhs,
        } => file.write_all(
            format!(
                "\tcomisd\t{}, {}\n",
                write_operand(lhs, bytes(AsmType::Double), interner, symbols),
                write_operand(rhs, bytes(AsmType::Double), interner, symbols)
            )
            .as_bytes(),
        )?,
        Instr::Cmp { ty, lhs, rhs } => file.write_all(
            format!(
                "\tcmp{}\t{}, {}\n",
                type_suffix(ty),
                write_operand(lhs, bytes(ty), interner, symbols),
                write_operand(rhs, bytes(ty), interner, symbols)
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
                write_operand(operand, 1, interner, symbols)
            )
            .as_bytes(),
        )?,
        Instr::Label(label) => {
            file.write_all(format!(".L{}:\n", interner.get_symbol(label)).as_bytes())?
        }
        Instr::Call(name) => {
            file.write_all(format!("\tcall\t_{}\n", interner.get_symbol(name)).as_bytes())?
        }

        Instr::Push(operand) => file.write_all(
            format!(
                "\tpushq\t{}\n",
                write_operand(operand, 8, interner, symbols)
            )
            .as_bytes(),
        )?,
        Instr::Movsx { src, dst } => file.write_all(
            format!(
                "\tmovslq\t{}, {}\n",
                write_operand(src, 4, interner, symbols),
                write_operand(dst, 8, interner, symbols)
            )
            .as_bytes(),
        )?,
        Instr::MovZeroExtend { .. } => {
            return Err(Error::other(CompileError::Emit(String::from(
                "MovZeroExtend not fixed up",
            ))));
        }
        Instr::Cvtsi2sd { ty, src, dst } => file.write_all(
            format!(
                "\tcvtsi2sd{}\t{}, {}\n",
                type_suffix(ty),
                write_operand(src, bytes(ty), interner, symbols),
                write_operand(dst, bytes(ty), interner, symbols)
            )
            .as_bytes(),
        )?,
        Instr::Cvttsd2si { ty, src, dst } => file.write_all(
            format!(
                "\tcvttsd2si{}\t{}, {}\n",
                type_suffix(ty),
                write_operand(src, bytes(ty), interner, symbols),
                write_operand(dst, bytes(ty), interner, symbols)
            )
            .as_bytes(),
        )?,
    }
    Ok(())
}

fn bytes(ty: AsmType) -> u8 {
    match ty {
        AsmType::Longword => 4,
        AsmType::Quadword => 8,
        AsmType::Double => 8,
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
        CondCode::P => "p",
        CondCode::NP => "np",
    }
    .to_string()
}

fn write_unop(unop: UnaryOp, ty: AsmType) -> String {
    let instr = match unop {
        UnaryOp::Neg => "neg",
        UnaryOp::Not => "not",
        UnaryOp::Shr => "shr", // TODO unify this with binary shr?
    }
    .to_string();
    let suffix = type_suffix(ty);
    format!("{instr}{suffix}")
}

fn type_suffix(ty: AsmType) -> &'static str {
    match ty {
        AsmType::Longword => "l",
        AsmType::Quadword => "q",
        AsmType::Double => "sd",
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
        BinaryOp::ShiftRightLogical => "shr",
        BinaryOp::DivDouble => "div",
    }
    .to_string();
    let suffix = type_suffix(ty);
    format!("{instr}{suffix}")
}

fn write_operand(
    op: Operand,
    bytes: u8,
    interner: &Interner,
    symbols: &HashMap<Symbol, AsmEntry>,
) -> String {
    match op {
        Operand::Reg(reg) => write_register(reg, bytes),
        Operand::Imm(n) => format!("${}", n),
        Operand::Stack(offset) => format!("{}(%rbp)", offset),
        Operand::Pseudo(s) => panic!("Pseudo operand {} not replaced", s),
        Operand::Data(var) => {
            format!(
                "{}_{}(%rip)",
                format_local(symbols, var),
                interner.get_symbol(var)
            )
        }
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
        Register::XMM0
        | Register::XMM1
        | Register::XMM2
        | Register::XMM3
        | Register::XMM4
        | Register::XMM5
        | Register::XMM6
        | Register::XMM7
        | Register::XMM14
        | Register::XMM15 => write_fp_register(reg),
    }
}

fn write_fp_register(reg: Register) -> String {
    let suffix = match reg {
        Register::XMM0 => "0",
        Register::XMM1 => "1",
        Register::XMM2 => "2",
        Register::XMM3 => "3",
        Register::XMM4 => "4",
        Register::XMM5 => "5",
        Register::XMM6 => "6",
        Register::XMM7 => "7",
        Register::XMM14 => "7",
        Register::XMM15 => "15",
        r => panic!("Bad floating-point register {r:?}"),
    };
    format!("%xmm{suffix}")
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
