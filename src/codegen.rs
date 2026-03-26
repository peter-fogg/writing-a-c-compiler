use std::collections::HashMap;

use crate::semantic_analysis::{Attrs, Type};
use crate::tacky::{self, Tacky, TopLevel};

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Imm(i32),
    Reg(Register),
    Pseudo(String),
    Stack(i16),
    Data(String),
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
    CX,
    DX,
    DI,
    SI,
    R8,
    R9,
    R10,
    R11,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
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
    AllocateStack(u16),
    Jmp(String),
    JmpCC(CondCode, String),
    SetCC(CondCode, Operand),
    Label(String),
    Cmp {
        lhs: Operand,
        rhs: Operand,
    },
    DeallocateStack(u16),
    Push(Operand),
    Call(String),
}
#[derive(Debug, PartialEq, Clone)]
pub enum AsmTopLevel {
    AsmFunction {
        name: String,
        instructions: Vec<Instr>,
        global: bool,
    },
    AsmStatic {
        name: String,
        global: bool,
        init: i32,
    },
}

pub type Assembly = Vec<AsmTopLevel>;

struct ReplaceState<'a> {
    offsets: HashMap<String, u16>,
    max_offset: u16,
    symbols: &'a HashMap<String, (Type, Attrs)>,
}

pub fn assemble(top_levels: Tacky, symbols: &HashMap<String, (Type, Attrs)>) -> Assembly {
    let mut asm_top_levels = Vec::with_capacity(top_levels.len());
    for top_level in top_levels {
        asm_top_levels.push(assemble_top_level(top_level, symbols));
    }
    asm_top_levels
}

fn assemble_top_level(
    top_level: TopLevel,
    symbols: &HashMap<String, (Type, Attrs)>,
) -> AsmTopLevel {
    match top_level {
        TopLevel::TackyFunction {
            name,
            instructions,
            params,
            global,
        } => {
            let mut assembly = vec![];
            let mut stack_offset = 16;
            for stack_param in params.iter().skip(6) {
                assembly.push(Instr::Mov {
                    src: Operand::Stack(stack_offset),
                    dst: Operand::Pseudo(stack_param.to_string()),
                });
                stack_offset += 8;
            }

            let reg_arg_locations = [
                Operand::Reg(Register::DI),
                Operand::Reg(Register::SI),
                Operand::Reg(Register::DX),
                Operand::Reg(Register::CX),
                Operand::Reg(Register::R8),
                Operand::Reg(Register::R9),
            ]
            .into_iter();

            for (param, src) in params.iter().zip(reg_arg_locations) {
                let dst = Operand::Pseudo(param.to_string());
                assembly.push(Instr::Mov {
                    src: src.clone(),
                    dst,
                });
            }

            let body = assemble_instructions(instructions);

            assembly.extend(body);

            let stack_size = replace_pseudo(&mut assembly, symbols);

            let rounded = match stack_size % 16 {
                0 => stack_size,
                n => stack_size + (16 - n),
            };

            assembly.insert(0, Instr::AllocateStack(rounded));

            let fixed = fixup_instructions(assembly);

            AsmTopLevel::AsmFunction {
                name,
                instructions: fixed,
                global,
            }
        }
        TopLevel::StaticVar { name, global, init } => AsmTopLevel::AsmStatic { name, global, init },
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
            tacky::Instr::Jump { target } => assembly.push(Instr::Jmp(target)),
            tacky::Instr::Copy { src, dst } => assembly.push(Instr::Mov {
                src: assemble_val(src),
                dst: assemble_val(dst),
            }),
            tacky::Instr::Label(id) => assembly.push(Instr::Label(id)),
            tacky::Instr::Unary {
                unop: tacky::UnaryOp::Not,
                src,
                dst,
            } => assembly.extend(vec![
                Instr::Cmp {
                    lhs: Operand::Imm(0),
                    rhs: assemble_val(src),
                },
                Instr::Mov {
                    src: Operand::Imm(0),
                    dst: assemble_val(dst.clone()),
                },
                Instr::SetCC(CondCode::E, assemble_val(dst)),
            ]),
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
                        src: Operand::Reg(Register::CX),
                        dst,
                    },
                ])
            }
            tacky::Instr::Binary {
                binop,
                src1,
                src2,
                dst,
            } if is_comparison(binop) => {
                let code = match binop {
                    tacky::BinaryOp::Equals => CondCode::E,
                    tacky::BinaryOp::NotEquals => CondCode::NE,
                    tacky::BinaryOp::GreaterThan => CondCode::G,
                    tacky::BinaryOp::GreaterThanEquals => CondCode::GE,
                    tacky::BinaryOp::LessThan => CondCode::L,
                    tacky::BinaryOp::LessThanEquals => CondCode::LE,
                    _ => unreachable!(),
                };
                assembly.extend(vec![
                    Instr::Cmp {
                        lhs: assemble_val(src2),
                        rhs: assemble_val(src1),
                    },
                    Instr::Mov {
                        src: Operand::Imm(0),
                        dst: assemble_val(dst.clone()),
                    },
                    Instr::SetCC(code, assemble_val(dst)),
                ]);
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
            tacky::Instr::JumpIfZero { condition, target } => assembly.extend(vec![
                Instr::Cmp {
                    lhs: Operand::Imm(0),
                    rhs: assemble_val(condition),
                },
                Instr::JmpCC(CondCode::E, target),
            ]),
            tacky::Instr::JumpIfNotZero { condition, target } => assembly.extend(vec![
                Instr::Cmp {
                    lhs: Operand::Imm(0),
                    rhs: assemble_val(condition),
                },
                Instr::JmpCC(CondCode::NE, target),
            ]),
            tacky::Instr::Call { name, params, dst } => {
                let arg_registers = [
                    Register::DI,
                    Register::SI,
                    Register::DX,
                    Register::CX,
                    Register::R8,
                    Register::R9,
                ];

                let (first_six, rest) =
                    if let Some((first_six, rest)) = params.split_first_chunk::<6>() {
                        (first_six.as_slice(), rest)
                    } else {
                        (params.as_slice(), [].as_slice())
                    };

                let stack_padding = if rest.len() % 2 == 0 { 0 } else { 8 };
                if stack_padding != 0 {
                    assembly.push(Instr::AllocateStack(stack_padding));
                }

                for (reg_index, tacky_param) in first_six.iter().enumerate() {
                    let reg = arg_registers[reg_index];
                    let asm_param = assemble_val(tacky_param.clone());
                    assembly.push(Instr::Mov {
                        src: asm_param,
                        dst: Operand::Reg(reg),
                    })
                }

                for tacky_param in rest.iter().rev() {
                    let asm_param = assemble_val(tacky_param.clone());
                    if matches!(asm_param, Operand::Imm(_) | Operand::Reg(_)) {
                        assembly.push(Instr::Push(asm_param));
                    } else {
                        assembly.extend(vec![
                            Instr::Mov {
                                src: asm_param,
                                dst: Operand::Reg(Register::AX),
                            },
                            Instr::Push(Operand::Reg(Register::AX)),
                        ]);
                    }
                }

                assembly.push(Instr::Call(name));

                let bytes_to_pop = 8 * rest.len() as u16 + stack_padding;
                if bytes_to_pop != 0 {
                    assembly.push(Instr::DeallocateStack(bytes_to_pop));
                }

                let dst = assemble_val(dst);
                assembly.push(Instr::Mov {
                    src: Operand::Reg(Register::AX),
                    dst,
                })
            }
        }
    }
    assembly
}

fn is_comparison(binop: tacky::BinaryOp) -> bool {
    matches!(
        binop,
        tacky::BinaryOp::Equals
            | tacky::BinaryOp::GreaterThan
            | tacky::BinaryOp::GreaterThanEquals
            | tacky::BinaryOp::LessThan
            | tacky::BinaryOp::LessThanEquals
            | tacky::BinaryOp::NotEquals
    )
}

fn assemble_unop(unop: tacky::UnaryOp) -> UnaryOp {
    match unop {
        tacky::UnaryOp::Complement => UnaryOp::Not,
        tacky::UnaryOp::Negate => UnaryOp::Neg,
        unop => panic!("Can't assemble {:?}", unop),
    }
}

fn assemble_val(val: tacky::Val) -> Operand {
    match val {
        tacky::Val::Constant(n) => Operand::Imm(n),
        tacky::Val::Var(s) => Operand::Pseudo(s),
    }
}

fn replace_pseudo(instrs: &mut [Instr], symbols: &HashMap<String, (Type, Attrs)>) -> u16 {
    let stack_map = HashMap::new();
    let mut replace_state = ReplaceState {
        offsets: stack_map,
        max_offset: 0,
        symbols,
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
            Instr::Cmp { lhs: _, rhs: _ } => {
                let cmp = std::mem::replace(instr, Instr::Ret);
                let Instr::Cmp { lhs, rhs } = cmp else {
                    unreachable!()
                };
                let new_lhs = replace_op(lhs, &mut replace_state);
                let new_rhs = replace_op(rhs, &mut replace_state);
                *instr = Instr::Cmp {
                    lhs: new_lhs,
                    rhs: new_rhs,
                };
            }
            Instr::SetCC(_, _) => {
                let setcc = std::mem::replace(instr, Instr::Ret);
                let Instr::SetCC(cond_code, operand) = setcc else {
                    unreachable!();
                };
                let operand = replace_op(operand, &mut replace_state);
                *instr = Instr::SetCC(cond_code, operand);
            }
            Instr::Push(_) => {
                let push = std::mem::replace(instr, Instr::Ret);
                let Instr::Push(operand) = push else {
                    unreachable!();
                };
                let operand = replace_op(operand, &mut replace_state);
                *instr = Instr::Push(operand);
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
            if let Some((_, Attrs::Static { .. })) = state.symbols.get(&var) {
                Operand::Data(var)
            } else {
                let offset = stack_map.entry(var).or_insert_with(|| {
                    state.max_offset += 4;
                    state.max_offset
                });
                Operand::Stack(-(*offset as i16))
            }
        }
        op => op,
    }
}

fn is_memory(op: &Operand) -> bool {
    matches!(op, Operand::Data(_) | Operand::Stack(_))
}

fn fixup_instructions(instrs: Vec<Instr>) -> Vec<Instr> {
    let mut fixed = Vec::new();
    for instr in instrs {
        match instr {
            Instr::Mov { src: s, dst: d } if is_memory(&s) || is_memory(&d) => {
                fixed.extend(vec![
                    Instr::Mov {
                        src: s,
                        dst: Operand::Reg(Register::R10),
                    },
                    Instr::Mov {
                        src: Operand::Reg(Register::R10),
                        dst: d,
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
                src: s,
                dst: d,
            } if is_memory(&s) || is_memory(&d) => {
                fixed.extend(vec![
                    Instr::Mov {
                        src: s,
                        dst: Operand::Reg(Register::R10),
                    },
                    Instr::Binary {
                        binop,
                        src: Operand::Reg(Register::R10),
                        dst: d,
                    },
                ]);
            }
            Instr::Binary {
                binop: BinaryOp::Mult,
                src,
                dst: d,
            } if is_memory(&d) => fixed.extend(vec![
                Instr::Mov {
                    src: d.clone(),
                    dst: Operand::Reg(Register::R11),
                },
                Instr::Binary {
                    binop: BinaryOp::Mult,
                    src,
                    dst: Operand::Reg(Register::R11),
                },
                Instr::Mov {
                    src: Operand::Reg(Register::R11),
                    dst: d,
                },
            ]),
            Instr::IDiv(Operand::Imm(n)) => fixed.extend(vec![
                Instr::Mov {
                    src: Operand::Imm(n),
                    dst: Operand::Reg(Register::R10),
                },
                Instr::IDiv(Operand::Reg(Register::R10)),
            ]),
            Instr::Cmp {
                lhs,
                rhs: Operand::Imm(n),
            } => fixed.extend(vec![
                Instr::Mov {
                    src: Operand::Imm(n),
                    dst: Operand::Reg(Register::R11),
                },
                Instr::Cmp {
                    lhs,
                    rhs: Operand::Reg(Register::R11),
                },
            ]),
            Instr::Cmp { lhs: l, rhs: r } if is_memory(&l) || is_memory(&r) => fixed.extend(vec![
                Instr::Mov {
                    src: l,
                    dst: Operand::Reg(Register::R10),
                },
                Instr::Cmp {
                    lhs: Operand::Reg(Register::R10),
                    rhs: r,
                },
            ]),
            i => fixed.push(i),
        }
    }
    fixed
}
