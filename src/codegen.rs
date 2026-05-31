use std::collections::HashMap;

use crate::ast::{Const, Type};
use crate::interner::Symbol;
use crate::tacky::{self, Tacky, TopLevel, Val};
use crate::typecheck::{Attrs, StaticInit};

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Imm(i64),
    Reg(Register),
    Pseudo(Symbol),
    Stack(i16),
    Data(Symbol),
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
    SP,
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum AsmType {
    Longword,
    Quadword,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Instr {
    Ret,
    Mov {
        ty: AsmType,
        src: Operand,
        dst: Operand,
    },
    Movsx {
        src: Operand,
        dst: Operand,
    },
    Unary {
        ty: AsmType,
        unop: UnaryOp,
        dst: Operand,
    },
    Binary {
        ty: AsmType,
        binop: BinaryOp,
        src: Operand,
        dst: Operand,
    },
    IDiv(AsmType, Operand),
    Cdq(AsmType),
    Jmp(Symbol),
    JmpCC(CondCode, Symbol),
    SetCC(CondCode, Operand),
    Label(Symbol),
    Cmp {
        ty: AsmType,
        lhs: Operand,
        rhs: Operand,
    },
    Push(Operand),
    Call(Symbol),
}
#[derive(Debug, PartialEq, Clone)]
pub enum AsmTopLevel {
    AsmFunction {
        name: Symbol,
        instructions: Vec<Instr>,
        global: bool,
    },
    AsmStatic {
        name: Symbol,
        global: bool,
        init: StaticInit,
        alignment: u8,
    },
}

pub type Assembly = Vec<AsmTopLevel>;

struct ReplaceState<'a> {
    offsets: HashMap<Symbol, u16>,
    max_offset: u16,
    symbols: &'a HashMap<Symbol, AsmEntry>,
}

pub fn assemble(top_levels: Tacky, symbols: &HashMap<Symbol, (Type, Attrs)>) -> Assembly {
    let mut asm_top_levels = Vec::with_capacity(top_levels.len());
    for top_level in top_levels {
        asm_top_levels.push(assemble_top_level(top_level, symbols));
    }
    asm_top_levels
}

fn alignment(ty: &Type) -> u8 {
    match ty {
        Type::Int => 4,
        Type::Long => 8,
        Type::Fun(_, _) => unreachable!("alignment of function type"),
        _ => todo!("unsigned"),
    }
}

fn val_asm_type(val: &Val, symbols: &HashMap<Symbol, (Type, Attrs)>) -> AsmType {
    match val {
        Val::Constant(Const::Int(_)) => AsmType::Longword,
        Val::Constant(Const::Long(_)) => AsmType::Quadword,
        Val::Var(name) => match symbols.get(name) {
            None => unreachable!("unresolved symbol {}", name),
            Some((ty, _)) => ty_asm_type(ty),
        },
        _ => todo!("unsigned"),
    }
}

fn ty_asm_type(ty: &Type) -> AsmType {
    match ty {
        Type::Int => AsmType::Longword,
        Type::Long => AsmType::Quadword,
        Type::Fun(_, _) => {
            unreachable!("function used as variable post-typechecking")
        }
        _ => todo!("unsigned"),
    }
}

fn assemble_top_level(
    top_level: TopLevel,
    symbols: &HashMap<Symbol, (Type, Attrs)>,
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
            for (stack_param, ty) in params.iter().skip(6) {
                assembly.push(Instr::Mov {
                    ty: ty_asm_type(ty),
                    src: Operand::Stack(stack_offset),
                    dst: Operand::Pseudo(*stack_param),
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

            for ((param, ty), src) in params.iter().zip(reg_arg_locations) {
                let dst = Operand::Pseudo(*param);
                assembly.push(Instr::Mov {
                    ty: ty_asm_type(ty),
                    src: src.clone(),
                    dst,
                });
            }

            let body = assemble_instructions(instructions, symbols);

            assembly.extend(body);

            let symbols = convert_symbols(symbols);

            let stack_size = replace_pseudo(&mut assembly, &symbols);

            let rounded = match stack_size % 16 {
                0 => stack_size,
                n => stack_size + (16 - n),
            };

            // Allocate space on the stack for instructions
            // TODO: inserting at the front of the vec is inefficient and can be optimized
            assembly.insert(
                0,
                Instr::Binary {
                    ty: AsmType::Quadword,
                    binop: BinaryOp::Sub,
                    src: Operand::Imm(rounded.into()),
                    dst: Operand::Reg(Register::SP),
                },
            );

            let fixed = fixup_instructions(assembly);

            AsmTopLevel::AsmFunction {
                name,
                instructions: fixed,
                global,
            }
        }
        TopLevel::StaticVar {
            name,
            global,
            init,
            ty,
        } => AsmTopLevel::AsmStatic {
            name,
            global,
            init,
            alignment: alignment(&ty),
        },
    }
}

fn assemble_instructions(
    instructions: Vec<tacky::Instr>,
    symbols: &HashMap<Symbol, (Type, Attrs)>,
) -> Vec<Instr> {
    let mut assembly = Vec::new();
    for instr in instructions {
        match instr {
            tacky::Instr::Return(val) => {
                assembly.push(Instr::Mov {
                    ty: val_asm_type(&val, symbols),
                    src: assemble_val(val),
                    dst: Operand::Reg(Register::AX),
                });
                assembly.push(Instr::Ret);
            }
            tacky::Instr::Jump { target } => assembly.push(Instr::Jmp(target)),
            tacky::Instr::Copy { src, dst } => assembly.push(Instr::Mov {
                ty: val_asm_type(&src, symbols),
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
                    ty: val_asm_type(&src, symbols),
                    lhs: Operand::Imm(0),
                    rhs: assemble_val(src),
                },
                Instr::Mov {
                    ty: val_asm_type(&src, symbols),
                    src: Operand::Imm(0),
                    dst: assemble_val(dst),
                },
                Instr::SetCC(CondCode::E, assemble_val(dst)),
            ]),
            tacky::Instr::Unary { unop, src, dst } => {
                let dst_ty = val_asm_type(&dst, symbols);
                let dst = assemble_val(dst);
                assembly.push(Instr::Mov {
                    ty: val_asm_type(&src, symbols),
                    src: assemble_val(src),
                    dst: dst.clone(),
                });
                assembly.push(Instr::Unary {
                    ty: dst_ty,
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
                let ty = val_asm_type(&src1, symbols);
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
                        ty,
                        src: src1,
                        dst: Operand::Reg(Register::AX),
                    },
                    Instr::Cdq(ty),
                    Instr::IDiv(ty, src2),
                    Instr::Mov {
                        ty,
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
                let ty = val_asm_type(&src1, symbols);
                assembly.extend(vec![
                    Instr::Mov {
                        ty,
                        src: assemble_val(src2),
                        dst: Operand::Reg(Register::CX),
                    },
                    Instr::Mov {
                        ty,
                        src: assemble_val(src1),
                        dst: dst.clone(),
                    },
                    Instr::Binary {
                        ty,
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
                        ty: val_asm_type(&src1, symbols),
                        lhs: assemble_val(src2),
                        rhs: assemble_val(src1),
                    },
                    Instr::Mov {
                        ty: val_asm_type(&dst, symbols),
                        src: Operand::Imm(0),
                        dst: assemble_val(dst),
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
                        ty: val_asm_type(&src1, symbols),
                        src: assemble_val(src1),
                        dst: dst.clone(),
                    },
                    Instr::Binary {
                        ty: val_asm_type(&src1, symbols),
                        binop,
                        src: assemble_val(src2),
                        dst,
                    },
                ]);
            }
            tacky::Instr::JumpIfZero { condition, target } => assembly.extend(vec![
                Instr::Cmp {
                    ty: val_asm_type(&condition, symbols),
                    lhs: Operand::Imm(0),
                    rhs: assemble_val(condition),
                },
                Instr::JmpCC(CondCode::E, target),
            ]),
            tacky::Instr::JumpIfNotZero { condition, target } => assembly.extend(vec![
                Instr::Cmp {
                    ty: val_asm_type(&condition, symbols),
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
                    assembly.push(Instr::Binary {
                        ty: AsmType::Quadword,
                        binop: BinaryOp::Sub,
                        src: Operand::Imm(stack_padding),
                        dst: Operand::Reg(Register::SP),
                    });
                }

                for (reg_index, tacky_param) in first_six.iter().enumerate() {
                    let reg = arg_registers[reg_index];
                    let asm_param = assemble_val(*tacky_param);
                    assembly.push(Instr::Mov {
                        ty: val_asm_type(tacky_param, symbols),
                        src: asm_param,
                        dst: Operand::Reg(reg),
                    })
                }

                for tacky_param in rest.iter().rev() {
                    let asm_param = assemble_val(*tacky_param);
                    if matches!(asm_param, Operand::Imm(_) | Operand::Reg(_))
                        || val_asm_type(tacky_param, symbols) == AsmType::Quadword
                    {
                        assembly.push(Instr::Push(asm_param));
                    } else {
                        assembly.extend(vec![
                            Instr::Mov {
                                ty: AsmType::Longword,
                                src: asm_param,
                                dst: Operand::Reg(Register::AX),
                            },
                            Instr::Push(Operand::Reg(Register::AX)),
                        ]);
                    }
                }

                assembly.push(Instr::Call(name));

                let bytes_to_pop = (8 * rest.len()) as i64 + stack_padding;
                if bytes_to_pop != 0 {
                    assembly.push(Instr::Binary {
                        ty: AsmType::Quadword,
                        binop: BinaryOp::Add,
                        src: Operand::Imm(bytes_to_pop),
                        dst: Operand::Reg(Register::SP),
                    });
                }
                let dst_ty = val_asm_type(&dst, symbols);
                let dst = assemble_val(dst);
                assembly.push(Instr::Mov {
                    ty: dst_ty,
                    src: Operand::Reg(Register::AX),
                    dst,
                })
            }
            tacky::Instr::SignExtend { src, dst } => assembly.push(Instr::Movsx {
                src: assemble_val(src),
                dst: assemble_val(dst),
            }),
            tacky::Instr::Truncate { src, dst } => assembly.push(Instr::Mov {
                ty: AsmType::Longword,
                src: assemble_val(src),
                dst: assemble_val(dst),
            }),
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

fn assemble_val(val: Val) -> Operand {
    match val {
        Val::Constant(Const::Int(n)) => Operand::Imm(n.into()),
        Val::Constant(Const::Long(n)) => Operand::Imm(n),
        Val::Var(s) => Operand::Pseudo(s),
        _ => todo!("unsigned"),
    }
}

fn replace_pseudo(instrs: &mut [Instr], symbols: &HashMap<Symbol, AsmEntry>) -> u16 {
    let stack_map = HashMap::new();
    let mut replace_state = ReplaceState {
        offsets: stack_map,
        max_offset: 0,
        symbols,
    };
    for instr in instrs {
        match instr {
            Instr::Unary { .. } => {
                let unary = std::mem::replace(instr, Instr::Ret);
                let Instr::Unary {
                    ty,
                    unop,
                    dst: operand,
                } = unary
                else {
                    unreachable!()
                };
                let new_operand = replace_op(operand, &mut replace_state);
                *instr = Instr::Unary {
                    ty,
                    unop,
                    dst: new_operand,
                };
            }
            Instr::Binary { .. } => {
                let binary = std::mem::replace(instr, Instr::Ret);
                let Instr::Binary {
                    ty,
                    binop,
                    src,
                    dst,
                } = binary
                else {
                    unreachable!();
                };
                let new_src = replace_op(src, &mut replace_state);
                let new_dst = replace_op(dst, &mut replace_state);
                *instr = Instr::Binary {
                    ty,
                    binop,
                    src: new_src,
                    dst: new_dst,
                };
            }
            Instr::IDiv(_, _) => {
                let idiv = std::mem::replace(instr, Instr::Ret);
                let Instr::IDiv(ty, op) = idiv else {
                    panic!("unreachable")
                };
                let op = replace_op(op, &mut replace_state);
                *instr = Instr::IDiv(ty, op);
            }
            Instr::Mov { .. } => {
                let mov = std::mem::replace(instr, Instr::Ret);
                let Instr::Mov { ty, src, dst } = mov else {
                    panic!("unreachable")
                };
                let new_src = replace_op(src, &mut replace_state);
                let new_dst = replace_op(dst, &mut replace_state);
                *instr = Instr::Mov {
                    ty,
                    src: new_src,
                    dst: new_dst,
                };
            }
            Instr::Movsx { .. } => {
                let movsx = std::mem::replace(instr, Instr::Ret);
                let Instr::Movsx { src, dst } = movsx else {
                    panic!("unreachable")
                };
                let new_src = replace_op(src, &mut replace_state);
                let new_dst = replace_op(dst, &mut replace_state);
                *instr = Instr::Movsx {
                    src: new_src,
                    dst: new_dst,
                };
            }
            Instr::Cmp { .. } => {
                let cmp = std::mem::replace(instr, Instr::Ret);
                let Instr::Cmp { ty, lhs, rhs } = cmp else {
                    unreachable!()
                };
                let new_lhs = replace_op(lhs, &mut replace_state);
                let new_rhs = replace_op(rhs, &mut replace_state);
                *instr = Instr::Cmp {
                    ty,
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

// Replace pseudo operands with a real stack offset.
fn replace_op(op: Operand, state: &mut ReplaceState) -> Operand {
    let stack_map = &mut state.offsets;
    match op {
        Operand::Pseudo(var) => {
            if let Some(AsmEntry::Obj {
                is_static: true, ..
            }) = state.symbols.get(&var)
            {
                Operand::Data(var) // This is a static variable so it doesn't live on the stack
            } else {
                let offset = stack_map.entry(var).or_insert_with(|| {
                    // Get the asm type of the variable
                    let ty = match state.symbols.get(&var) {
                        Some(AsmEntry::Obj { ty, .. }) => ty,
                        _ => unreachable!("wrong type for var {}", var),
                    };
                    // Quadwords get 8 bytes of stack space, 4 otherwise
                    let is_quadword = *ty == AsmType::Quadword;
                    let stack_size = if is_quadword { 8 } else { 4 };
                    state.max_offset += stack_size;
                    // Ensure proper alignment by adding another 4
                    // bytes if a quadword would not be at an offset
                    // which is a multiple of 8
                    if is_quadword && !state.max_offset.is_multiple_of(8) {
                        state.max_offset += 4;
                    }
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

// Replace instructions if their operands are in the wrong places.
fn fixup_instructions(instrs: Vec<Instr>) -> Vec<Instr> {
    let mut fixed = Vec::new();
    for instr in instrs {
        match instr {
            Instr::Mov { ty, src: s, dst: d } if is_memory(&s) || is_memory(&d) => {
                fixed.extend(vec![
                    Instr::Mov {
                        ty,
                        src: s,
                        dst: Operand::Reg(Register::R10),
                    },
                    Instr::Mov {
                        ty,
                        src: Operand::Reg(Register::R10),
                        dst: d,
                    },
                ]);
            }
            Instr::Mov {
                ty: ty @ AsmType::Quadword,
                src: Operand::Imm(n),
                dst,
            } if !in_int_range(n) && is_memory(&dst) => fixed.extend(vec![
                // mov can't have an immediate outside of i32 range moved into memory, but can move it into a register
                Instr::Mov {
                    ty,
                    src: Operand::Imm(n),
                    dst: Operand::Reg(Register::R10),
                },
                Instr::Mov {
                    ty,
                    src: Operand::Reg(Register::R10),
                    dst,
                },
            ]),
            Instr::Mov {
                ty: ty @ AsmType::Longword,
                src: Operand::Imm(n),
                dst,
            } if !in_int_range(n) => fixed.push(Instr::Mov {
                ty,
                src: Operand::Imm((n as i32).into()),
                dst,
            }),
            Instr::Binary {
                ty: ty @ AsmType::Quadword,
                binop: binop @ (BinaryOp::Add | BinaryOp::Sub),
                src: src @ Operand::Imm(n),
                dst,
            } if !in_int_range(n) => {
                fixed.extend(vec![
                    // addq and subq can't have an immediate operand outside of i32 range
                    Instr::Mov {
                        ty,
                        src,
                        dst: Operand::Reg(Register::R10),
                    },
                    Instr::Binary {
                        ty,
                        binop,
                        src: Operand::Reg(Register::R10),
                        dst,
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
                ty,
            } if is_memory(&s) || is_memory(&d) => {
                fixed.extend(vec![
                    Instr::Mov {
                        ty,
                        src: s,
                        dst: Operand::Reg(Register::R10),
                    },
                    Instr::Binary {
                        ty,
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
                ty,
            } if is_memory(&d) => {
                // mulq cannot have a large constant src -- move this into r10 first
                let needs_src_rewrite =
                    ty == AsmType::Quadword && matches!(src, Operand::Imm(n) if !in_int_range(n));
                let mut new_src = src.clone();
                if needs_src_rewrite {
                    new_src = Operand::Reg(Register::R10);
                    fixed.push(Instr::Mov {
                        ty: AsmType::Quadword,
                        src,
                        dst: new_src.clone(),
                    });
                }
                fixed.extend(vec![
                    Instr::Mov {
                        ty,
                        src: d.clone(),
                        dst: Operand::Reg(Register::R11),
                    },
                    Instr::Binary {
                        ty,
                        binop: BinaryOp::Mult,
                        src: new_src,
                        dst: Operand::Reg(Register::R11),
                    },
                    Instr::Mov {
                        ty,
                        src: Operand::Reg(Register::R11),
                        dst: d,
                    },
                ])
            }
            Instr::IDiv(ty, Operand::Imm(n)) => fixed.extend(vec![
                Instr::Mov {
                    ty,
                    src: Operand::Imm(n),
                    dst: Operand::Reg(Register::R10),
                },
                Instr::IDiv(ty, Operand::Reg(Register::R10)),
            ]),
            Instr::Cmp {
                ty,
                lhs,
                rhs: Operand::Imm(n),
            } => {
                let needs_lhs_rewrite =
                    ty == AsmType::Quadword && matches!(lhs, Operand::Imm(n) if !in_int_range(n));
                let mut new_lhs = lhs.clone();
                if needs_lhs_rewrite {
                    new_lhs = Operand::Reg(Register::R10);
                    fixed.push(Instr::Mov {
                        ty: AsmType::Quadword,
                        src: lhs,
                        dst: new_lhs.clone(),
                    });
                }
                fixed.extend(vec![
                    Instr::Mov {
                        ty,
                        src: Operand::Imm(n),
                        dst: Operand::Reg(Register::R11),
                    },
                    Instr::Cmp {
                        ty,
                        lhs: new_lhs,
                        rhs: Operand::Reg(Register::R11),
                    },
                ])
            }
            Instr::Cmp { ty, lhs: l, rhs: r } if is_memory(&l) || is_memory(&r) => {
                fixed.extend(vec![
                    Instr::Mov {
                        ty,
                        src: l,
                        dst: Operand::Reg(Register::R10),
                    },
                    Instr::Cmp {
                        ty,
                        lhs: Operand::Reg(Register::R10),
                        rhs: r,
                    },
                ])
            }
            Instr::Push(Operand::Imm(n)) if !in_int_range(n) => fixed.extend(vec![
                Instr::Mov {
                    ty: AsmType::Quadword,
                    src: Operand::Imm(n),
                    dst: Operand::Reg(Register::R10),
                },
                Instr::Push(Operand::Reg(Register::R10)),
            ]),
            Instr::Movsx { src, dst } => {
                let mut new_src = src.clone();
                // movsx can't have an immediate as it's source
                if matches!(src, Operand::Imm(_)) {
                    fixed.push(Instr::Mov {
                        ty: AsmType::Longword,
                        src,
                        dst: Operand::Reg(Register::R10),
                    });
                    new_src = Operand::Reg(Register::R10);
                }
                let mut post_instr = vec![];
                let mut new_dst = dst.clone();
                // And can't have a memory location as its dst
                if is_memory(&dst) {
                    post_instr = vec![Instr::Mov {
                        ty: AsmType::Quadword,
                        src: Operand::Reg(Register::R11),
                        dst,
                    }];
                    new_dst = Operand::Reg(Register::R11);
                }
                // Insert the movsx with (potentially) corrected src and dst
                fixed.push(Instr::Movsx {
                    src: new_src,
                    dst: new_dst,
                });
                fixed.extend(post_instr);
            }
            i => fixed.push(i),
        }
    }
    fixed
}

fn in_int_range(n: i64) -> bool {
    n <= i32::MAX.into()
}

pub enum AsmEntry {
    Obj { ty: AsmType, is_static: bool },
    Fun { _defined: bool },
}

fn convert_symbols(symbols: &HashMap<Symbol, (Type, Attrs)>) -> HashMap<Symbol, AsmEntry> {
    let mut backend_symbols = HashMap::with_capacity(symbols.len());
    for (name, (ty, attrs)) in symbols.iter() {
        let entry = match ty {
            Type::Fun(_, _) => {
                let defined = match attrs {
                    Attrs::Fun { defined, .. } => *defined,
                    _ => false,
                };
                AsmEntry::Fun { _defined: defined }
            }
            ty => {
                let is_static = matches!(attrs, Attrs::Static { .. });
                AsmEntry::Obj {
                    ty: ty_asm_type(ty),
                    is_static,
                }
            }
        };
        backend_symbols.insert(*name, entry);
    }
    backend_symbols
}
