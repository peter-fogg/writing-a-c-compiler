use std::collections::HashMap;

use crate::tacky;

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Imm(i32),
    Reg(Register),
    Pseudo(String),
    Stack(u8),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Register {
    AX,
    R10,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Instr {
    Ret,
    Mov { src: Operand, dst: Operand },
    Unary(UnaryOp, Operand),
    AllocateStack(u8),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Assembly<'a> {
    Function {
        name: &'a str,
        instructions: Vec<Instr>,
    },
}

struct ReplaceState {
    offsets: HashMap<String, u8>,
    max_offset: u8,
}

pub fn assemble<'a>(tacky: tacky::Tacky<'a>) -> Assembly<'a> {
    let tacky::Tacky::Function { name, instructions } = tacky;
    let mut assembled = assemble_instructions(instructions);
    let stack_offset = replace_pseudo(&mut assembled);

    assembled.insert(0, Instr::AllocateStack(stack_offset));

    let fixed = fixup_instructions(assembled);

    Assembly::Function {
        name,
        instructions: fixed,
    }
}

fn assemble_instructions(instructions: Vec<tacky::Instr>) -> Vec<Instr> {
    let mut assembly = Vec::new();
    for instr in instructions {
        match instr {
            tacky::Instr::Return(val) => {
                assembly.push(Instr::Mov {
                    src: assemble_val(val),
                    dst: Operand::Reg(Register::AX),
                });
                assembly.push(Instr::Ret);
            }
            tacky::Instr::Unary(unop, src, dst) => {
                let dst = assemble_val(dst);
                assembly.push(Instr::Mov {
                    src: assemble_val(src),
                    dst: dst.clone(),
                });
                assembly.push(Instr::Unary(assemble_op(unop), dst));
            }
        }
    }
    assembly
}

fn assemble_op(unop: tacky::UnaryOp) -> UnaryOp {
    match unop {
        tacky::UnaryOp::Complement => UnaryOp::Not,
        tacky::UnaryOp::Negate => UnaryOp::Neg,
    }
}

fn assemble_val(val: tacky::Val) -> Operand {
    match val {
        tacky::Val::Constant(n) => Operand::Imm(n),
        tacky::Val::Var(s) => Operand::Pseudo(s),
    }
}

fn replace_pseudo(instrs: &mut [Instr]) -> u8 {
    let stack_map = HashMap::new();
    let mut replace_state = ReplaceState {
        offsets: stack_map,
        max_offset: 0,
    };
    for instr in instrs {
        match instr {
            Instr::Unary(_, _) => {
                let unary = std::mem::replace(instr, Instr::Ret);
                let Instr::Unary(unop, operand) = unary else {
                    panic!("unreachable")
                };
                let new_operand = replace_op(operand, &mut replace_state);
                *instr = Instr::Unary(unop, new_operand);
            }
            Instr::Mov { src: _, dst: _ } => {
                let mov = std::mem::replace(instr, Instr::Ret);
                let Instr::Mov { src, dst } = mov else {
                    panic!("unreachable")
                };
                let new_src = replace_op(src, &mut replace_state);
                let new_dst = replace_op(dst, &mut replace_state);
                *instr = Instr::Mov {
                    src: new_src,
                    dst: new_dst,
                };
            }
            _ => (),
        }
    }
    replace_state.max_offset
}

fn replace_op(op: Operand, state: &mut ReplaceState) -> Operand {
    let stack_map = &mut state.offsets;
    match op {
        Operand::Pseudo(var) => {
            let offset = stack_map.entry(var).or_insert_with(|| {
                state.max_offset += 4;
                state.max_offset
            });
            Operand::Stack(*offset)
        }
        op => op,
    }
}

fn fixup_instructions(instrs: Vec<Instr>) -> Vec<Instr> {
    let mut fixed = Vec::new();
    for instr in instrs {
        match instr {
            Instr::Mov {
                src: Operand::Stack(src_off),
                dst: Operand::Stack(dst_off),
            } => {
                fixed.push(Instr::Mov {
                    src: Operand::Stack(src_off),
                    dst: Operand::Reg(Register::R10),
                });
                fixed.push(Instr::Mov {
                    src: Operand::Reg(Register::R10),
                    dst: Operand::Stack(dst_off),
                });
            }
            i => fixed.push(i),
        }
    }
    fixed
}
