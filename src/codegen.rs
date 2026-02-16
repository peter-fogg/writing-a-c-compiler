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
pub enum BinaryOp {
    Add,
    Sub,
    Mult,
    BitAnd,
    BitOr,
    BitXOr,
    ShiftLeft,
    ShiftRight,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Register {
    AX,
    DX,
    R10,
    R11,
    CL,
    CX,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Instr {
    Ret,
    Mov {
        src: Operand,
        dst: Operand,
    },
    Unary {
        unop: UnaryOp,
        dst: Operand,
    },
    Binary {
        binop: BinaryOp,
        src: Operand,
        dst: Operand,
    },
    IDiv(Operand),
    Cdq,
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
            tacky::Instr::Unary { unop, src, dst } => {
                let dst = assemble_val(dst);
                assembly.push(Instr::Mov {
                    src: assemble_val(src),
                    dst: dst.clone(),
                });
                assembly.push(Instr::Unary {
                    unop: assemble_unop(unop),
                    dst,
                });
            }
            tacky::Instr::Binary {
                binop: binop @ (tacky::BinaryOp::Divide | tacky::BinaryOp::Remainder),
                src1,
                src2,
                dst,
            } => {
                let dst = assemble_val(dst);
                let src1 = assemble_val(src1);
                let src2 = assemble_val(src2);
                let out_reg = if binop == tacky::BinaryOp::Divide {
                    Register::AX
                } else {
                    Register::DX
                };
                assembly.extend(vec![
                    Instr::Mov {
                        src: src1,
                        dst: Operand::Reg(Register::AX),
                    },
                    Instr::Cdq,
                    Instr::IDiv(src2),
                    Instr::Mov {
                        src: Operand::Reg(out_reg),
                        dst,
                    },
                ]);
            }
            tacky::Instr::Binary {
                binop: binop @ (tacky::BinaryOp::ShiftLeft | tacky::BinaryOp::ShiftRight),
                src1,
                src2,
                dst,
            } => {
                let binop = match binop {
                    tacky::BinaryOp::ShiftLeft => BinaryOp::ShiftLeft,
                    tacky::BinaryOp::ShiftRight => BinaryOp::ShiftRight,
                    _ => panic!("unreachable"),
                };
                let dst = assemble_val(dst);
                assembly.extend(vec![
                    Instr::Mov {
                        src: assemble_val(src2),
                        dst: Operand::Reg(Register::CX),
                    },
                    Instr::Mov {
                        src: assemble_val(src1),
                        dst: dst.clone(),
                    },
                    Instr::Binary {
                        binop,
                        src: Operand::Reg(Register::CL),
                        dst,
                    },
                ])
            }
            tacky::Instr::Binary {
                binop,
                src1,
                src2,
                dst,
            } => {
                let binop = match binop {
                    tacky::BinaryOp::Add => BinaryOp::Add,
                    tacky::BinaryOp::Subtract => BinaryOp::Sub,
                    tacky::BinaryOp::Multiply => BinaryOp::Mult,
                    tacky::BinaryOp::BitAnd => BinaryOp::BitAnd,
                    tacky::BinaryOp::BitOr => BinaryOp::BitOr,
                    tacky::BinaryOp::BitXOr => BinaryOp::BitXOr,
                    _ => panic!(
                        "Expected add, subtract, multiply, or bitwise op, got {:?}",
                        binop
                    ),
                };
                let dst = assemble_val(dst);
                assembly.extend(vec![
                    Instr::Mov {
                        src: assemble_val(src1),
                        dst: dst.clone(),
                    },
                    Instr::Binary {
                        binop,
                        src: assemble_val(src2),
                        dst,
                    },
                ]);
            }
        }
    }
    assembly
}

fn assemble_unop(unop: tacky::UnaryOp) -> UnaryOp {
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
            Instr::Unary { unop: _, dst: _ } => {
                let unary = std::mem::replace(instr, Instr::Ret);
                let Instr::Unary { unop, dst: operand } = unary else {
                    panic!("unreachable")
                };
                let new_operand = replace_op(operand, &mut replace_state);
                *instr = Instr::Unary {
                    unop,
                    dst: new_operand,
                };
            }
            Instr::Binary {
                binop: _,
                src: _,
                dst: _,
            } => {
                let binary = std::mem::replace(instr, Instr::Ret);
                let Instr::Binary { binop, src, dst } = binary else {
                    panic!("unreachable");
                };
                let new_src = replace_op(src, &mut replace_state);
                let new_dst = replace_op(dst, &mut replace_state);
                *instr = Instr::Binary {
                    binop,
                    src: new_src,
                    dst: new_dst,
                };
            }
            Instr::IDiv(_) => {
                let idiv = std::mem::replace(instr, Instr::Ret);
                let Instr::IDiv(op) = idiv else {
                    panic!("unreachable")
                };
                let op = replace_op(op, &mut replace_state);
                *instr = Instr::IDiv(op);
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
                fixed.extend(vec![
                    Instr::Mov {
                        src: Operand::Stack(src_off),
                        dst: Operand::Reg(Register::R10),
                    },
                    Instr::Mov {
                        src: Operand::Reg(Register::R10),
                        dst: Operand::Stack(dst_off),
                    },
                ]);
            }
            Instr::Binary {
                binop:
                    binop @ (BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::BitAnd
                    | BinaryOp::BitOr
                    | BinaryOp::BitXOr),
                src: Operand::Stack(src_off),
                dst: Operand::Stack(dst_off),
            } => {
                fixed.extend(vec![
                    Instr::Mov {
                        src: Operand::Stack(src_off),
                        dst: Operand::Reg(Register::R10),
                    },
                    Instr::Binary {
                        binop,
                        src: Operand::Reg(Register::R10),
                        dst: Operand::Stack(dst_off),
                    },
                ]);
            }
            Instr::Binary {
                binop: BinaryOp::Mult,
                src,
                dst: Operand::Stack(dst_off),
            } => fixed.extend(vec![
                Instr::Mov {
                    src: Operand::Stack(dst_off),
                    dst: Operand::Reg(Register::R11),
                },
                Instr::Binary {
                    binop: BinaryOp::Mult,
                    src,
                    dst: Operand::Reg(Register::R11),
                },
                Instr::Mov {
                    src: Operand::Reg(Register::R11),
                    dst: Operand::Stack(dst_off),
                },
            ]),
            Instr::IDiv(Operand::Imm(n)) => fixed.extend(vec![
                Instr::Mov {
                    src: Operand::Imm(n),
                    dst: Operand::Reg(Register::R10),
                },
                Instr::IDiv(Operand::Reg(Register::R10)),
            ]),
            i => fixed.push(i),
        }
    }
    fixed
}
