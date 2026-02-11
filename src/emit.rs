use std::io::Result;
use std::{fs::File, io::Write};

use crate::codegen::{Assembly, Instr, Operand};

pub fn emit<'a>(
    Assembly::Function { name, instructions }: Assembly<'a>,
    mut file: File,
) -> Result<()> {
    file.write_all(format!("\t.globl _{}\n", name).as_bytes())?;
    file.write_all(format!("_{}:\n", name).as_bytes())?;
    for instr in instructions {
        emit_instr(instr, &mut file)?;
    }
    Ok(())
}

fn emit_instr<'a>(instr: Instr<'a>, file: &mut File) -> Result<()> {
    match instr {
        Instr::Ret => file.write_all("\tret\n".as_bytes())?,
        Instr::Mov { src, dst } => file.write_all(
            format!("\tmovl\t{}, {}\n", write_operand(src), write_operand(dst)).as_bytes(),
        )?,
    }
    Ok(())
}

fn write_operand<'a>(op: Operand<'a>) -> String {
    match op {
        Operand::Reg(reg) => reg.to_string(),
        Operand::Imm(n) => format!("${}", n),
    }
}
