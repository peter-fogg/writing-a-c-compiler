use std::collections::HashMap;

use crate::ast::{Const, Type};
use crate::interner::{Interner, Symbol};
use crate::tacky::{self, Tacky, TopLevel, Val};
use crate::typecheck::{Attrs, StaticInit};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operand {
    Imm(i64),
    Reg(Register),
    Pseudo(Symbol),
    Memory(Register, i16),
    Data(Symbol),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    Neg,
    Not,
    Shr, // TODO consider renaming binary operator versions for consistency
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mult,
    DivDouble,
    BitAnd,
    BitOr,
    BitXOr,
    ShiftLeft,
    ShiftRight,
    ShiftRightLogical,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Register {
    AX,
    CX,
    DX,
    DI,
    SI,
    SP,
    BP,
    R8,
    R9,
    R10,
    R11,
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    XMM14,
    XMM15,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
    A,
    AE,
    B,
    BE,
    P,
    NP,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum AsmType {
    Longword,
    Quadword,
    Double,
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
    MovZeroExtend {
        src: Operand,
        dst: Operand,
    },
    Lea {
        src: Operand,
        dst: Operand,
    },
    Cvttsd2si {
        ty: AsmType,
        src: Operand,
        dst: Operand,
    },
    Cvtsi2sd {
        ty: AsmType,
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
    Div(AsmType, Operand),
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
    Function {
        name: Symbol,
        instructions: Vec<Instr>,
        global: bool,
        stack_size: u16,
    },
    StaticVar {
        name: Symbol,
        global: bool,
        init: StaticInit,
        alignment: u8,
    },
    StaticConst {
        name: Symbol,
        init: StaticInit,
        alignment: u8,
    },
}

pub type Assembly = Vec<AsmTopLevel>;

type ClassifiedParams = (
    Vec<(AsmType, Operand)>,
    Vec<(AsmType, Operand)>,
    Vec<(AsmType, Operand)>,
);

struct ReplaceState<'a> {
    offsets: HashMap<Symbol, u16>,
    max_offset: u16,
    symbols: &'a HashMap<Symbol, AsmEntry>,
}

struct AssembleState<'a> {
    interner: &'a mut Interner,
    count: u8,
    constants: HashMap<(u64, u8), AsmTopLevel>,
}

pub fn assemble(
    top_levels: Tacky,
    symbols: &HashMap<Symbol, (Type, Attrs)>,
    interner: &mut Interner,
) -> (Assembly, HashMap<Symbol, AsmEntry>) {
    let mut asm_top_levels = Vec::with_capacity(top_levels.len());
    let mut state = AssembleState {
        count: 0,
        constants: HashMap::new(),
        interner,
    };
    // Assemble the TACKY instructions
    for top_level in top_levels {
        asm_top_levels.push(state.assemble_top_level(top_level, symbols));
    }
    let mut backend_symbols = convert_symbols(symbols);
    for top_level in &mut asm_top_levels {
        if let AsmTopLevel::Function {
            instructions,
            stack_size,
            ..
        } = top_level
        {
            let new_stack_size = replace_pseudo(instructions, &backend_symbols);

            let fixed = fixup_instructions(instructions.to_vec());

            let rounded = match new_stack_size % 16 {
                0 => new_stack_size,
                n => new_stack_size + (16 - n),
            };

            *stack_size = rounded;
            *instructions = fixed;
        }
    }
    // Add floating-point constants to both the symbol table and the
    // top levels so that we can use local labels for them
    for (_, constant) in state.constants.drain() {
        if let AsmTopLevel::StaticConst { name, .. } = constant {
            backend_symbols.insert(
                name,
                AsmEntry::Obj {
                    ty: AsmType::Double,
                    is_static: true,
                    is_constant: true,
                },
            );
        }

        asm_top_levels.push(constant)
    }

    (asm_top_levels, backend_symbols)
}

fn alignment(ty: &Type) -> u8 {
    match ty {
        Type::Int | Type::UInt => 4,
        Type::Long | Type::ULong | Type::Double | Type::Pointer(_) => 8,
        Type::Fun(_, _) => unreachable!("alignment of function type"),
        Type::Array(_, _) => todo!(),
    }
}

fn val_asm_type(val: &Val, symbols: &HashMap<Symbol, (Type, Attrs)>) -> AsmType {
    match val_tacky_type(val, symbols) {
        Type::Int | Type::UInt => AsmType::Longword,
        Type::Long | Type::ULong | Type::Pointer(_) => AsmType::Quadword,
        Type::Double => AsmType::Double,
        Type::Fun(_, _) => unreachable!("function used as a variable post-typechecking"),
        Type::Array(_, _) => todo!(),
    }
}

fn ty_asm_type(ty: &Type) -> AsmType {
    match ty {
        Type::Int | Type::UInt => AsmType::Longword,
        Type::Long | Type::ULong | Type::Pointer(_) => AsmType::Quadword,
        Type::Fun(_, _) => {
            unreachable!("function used as variable post-typechecking")
        }
        Type::Double => AsmType::Double,
        Type::Array(_, _) => todo!(),
    }
}

fn val_tacky_type(val: &Val, symbols: &HashMap<Symbol, (Type, Attrs)>) -> Type {
    match val {
        Val::Constant(c) => const_type(c),
        Val::Var(name) => match symbols.get(name) {
            None => unreachable!("unresolved symbol {name}"),
            Some((ty, _)) => ty.clone(),
        },
    }
}

fn const_type(constant: &Const) -> Type {
    match constant {
        Const::Double(_) => Type::Double,
        Const::Int(_) => Type::Int,
        Const::Long(_) => Type::Long,
        Const::UInt(_) => Type::UInt,
        Const::ULong(_) => Type::ULong,
    }
}

fn is_unsigned_type(val: &Val, symbols: &HashMap<Symbol, (Type, Attrs)>) -> bool {
    matches!(val_tacky_type(val, symbols), Type::UInt | Type::ULong)
}

impl<'a> AssembleState<'a> {
    fn assemble_top_level(
        &mut self,
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

                let params = params.iter().map(|p| Val::Var(*p)).collect();

                let (int_reg_params, double_reg_params, stack_params) =
                    self.classify_params(params, symbols);

                let int_arg_registers = [
                    Operand::Reg(Register::DI),
                    Operand::Reg(Register::SI),
                    Operand::Reg(Register::DX),
                    Operand::Reg(Register::CX),
                    Operand::Reg(Register::R8),
                    Operand::Reg(Register::R9),
                ]
                .into_iter();

                let double_arg_registers = [
                    Operand::Reg(Register::XMM0),
                    Operand::Reg(Register::XMM1),
                    Operand::Reg(Register::XMM2),
                    Operand::Reg(Register::XMM3),
                    Operand::Reg(Register::XMM4),
                    Operand::Reg(Register::XMM5),
                    Operand::Reg(Register::XMM6),
                    Operand::Reg(Register::XMM7),
                ]
                .into_iter();

                for ((ty, dst), src) in int_reg_params.iter().zip(int_arg_registers) {
                    assembly.push(Instr::Mov {
                        ty: *ty,
                        src,
                        dst: *dst,
                    });
                }

                for ((ty, dst), src) in double_reg_params.iter().zip(double_arg_registers) {
                    assembly.push(Instr::Mov {
                        ty: *ty,
                        src,
                        dst: *dst,
                    });
                }

                let mut stack_offset = 16;
                for (ty, dst) in stack_params {
                    assembly.push(Instr::Mov {
                        ty,
                        src: Operand::Memory(Register::BP, stack_offset),
                        dst,
                    });
                    stack_offset += 8;
                }

                let body = self.assemble_instructions(instructions, symbols);

                assembly.extend(body);

                AsmTopLevel::Function {
                    name,
                    instructions: assembly,
                    global,
                    stack_size: 0, // will get updated later after pseudoregister replacement
                }
            }
            TopLevel::StaticVar {
                name,
                global,
                init,
                ty,
            } => AsmTopLevel::StaticVar {
                name,
                global,
                init,
                alignment: alignment(&ty),
            },
        }
    }

    fn assemble_instructions(
        &mut self,
        instructions: Vec<tacky::Instr>,
        symbols: &HashMap<Symbol, (Type, Attrs)>,
    ) -> Vec<Instr> {
        let mut assembly = Vec::new();
        for instr in instructions {
            match instr {
                tacky::Instr::Return(val) => {
                    let ty = val_asm_type(&val, symbols);
                    let reg = if ty == AsmType::Double {
                        Register::XMM0
                    } else {
                        Register::AX
                    };
                    assembly.push(Instr::Mov {
                        ty,
                        src: self.assemble_val(val),
                        dst: Operand::Reg(reg),
                    });
                    assembly.push(Instr::Ret);
                }
                tacky::Instr::Jump { target } => assembly.push(Instr::Jmp(target)),
                tacky::Instr::Copy { src, dst } => assembly.push(Instr::Mov {
                    ty: val_asm_type(&src, symbols),
                    src: self.assemble_val(src),
                    dst: self.assemble_val(dst),
                }),
                tacky::Instr::Label(id) => assembly.push(Instr::Label(id)),
                tacky::Instr::Unary {
                    unop: tacky::UnaryOp::Not,
                    src,
                    dst,
                } if val_asm_type(&src, symbols) == AsmType::Double => {
                    let ty = AsmType::Double;
                    assembly.extend(vec![
                        // zero out XMM0
                        Instr::Binary {
                            binop: BinaryOp::BitXOr,
                            ty,
                            src: Operand::Reg(Register::XMM0),
                            dst: Operand::Reg(Register::XMM0),
                        },
                        // compare src to 0
                        Instr::Cmp {
                            ty,
                            lhs: self.assemble_val(src),
                            rhs: Operand::Reg(Register::XMM0),
                        },
                        // zero out destination, eax, and ecx
                        Instr::Mov {
                            ty: AsmType::Longword,
                            src: Operand::Imm(0),
                            dst: self.assemble_val(dst),
                        },
                        Instr::Mov {
                            ty: AsmType::Longword,
                            src: Operand::Imm(0),
                            dst: Operand::Reg(Register::AX),
                        },
                        Instr::Mov {
                            ty: AsmType::Longword,
                            src: Operand::Imm(0),
                            dst: Operand::Reg(Register::CX),
                        },
                        Instr::SetCC(CondCode::E, Operand::Reg(Register::AX)),
                        Instr::SetCC(CondCode::NP, Operand::Reg(Register::CX)),
                        // we need the src to equal 0
                        // AND the parity flag to not be set
                        Instr::Binary {
                            ty: AsmType::Longword,
                            binop: BinaryOp::BitAnd,
                            src: Operand::Reg(Register::AX),
                            dst: Operand::Reg(Register::CX),
                        },
                        Instr::Mov {
                            ty: AsmType::Longword,
                            src: Operand::Reg(Register::CX),
                            dst: self.assemble_val(dst),
                        },
                        // set dst to result of comparison
                        //Instr::SetCC(CondCode::E, self.assemble_val(dst)),
                    ])
                }
                tacky::Instr::Unary {
                    unop: tacky::UnaryOp::Not,
                    src,
                    dst,
                } => assembly.extend(vec![
                    Instr::Cmp {
                        ty: val_asm_type(&src, symbols),
                        lhs: Operand::Imm(0),
                        rhs: self.assemble_val(src),
                    },
                    Instr::Mov {
                        ty: val_asm_type(&src, symbols),
                        src: Operand::Imm(0),
                        dst: self.assemble_val(dst),
                    },
                    Instr::SetCC(CondCode::E, self.assemble_val(dst)),
                ]),
                tacky::Instr::Unary {
                    // Negate doubles by xoring with -0.0
                    unop: tacky::UnaryOp::Negate,
                    src,
                    dst,
                } if val_asm_type(&src, symbols) == AsmType::Double => {
                    let label = self.add_static_constant(-0.0f64, 16);
                    let src = self.assemble_val(src);
                    let dst = self.assemble_val(dst);
                    let ty = AsmType::Double;
                    assembly.extend(vec![
                        Instr::Mov { ty, src, dst },
                        Instr::Binary {
                            binop: BinaryOp::BitXOr,
                            ty,
                            src: Operand::Data(label),
                            dst,
                        },
                    ]);
                }
                tacky::Instr::Unary { unop, src, dst } => {
                    let dst_ty = val_asm_type(&dst, symbols);
                    let dst = self.assemble_val(dst);
                    assembly.push(Instr::Mov {
                        ty: val_asm_type(&src, symbols),
                        src: self.assemble_val(src),
                        dst,
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
                } if is_unsigned_type(&src1, symbols) => {
                    let ty = val_asm_type(&src1, symbols);
                    let dst = self.assemble_val(dst);
                    let src1 = self.assemble_val(src1);
                    let src2 = self.assemble_val(src2);
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
                        Instr::Mov {
                            ty,
                            src: Operand::Imm(0),
                            dst: Operand::Reg(Register::DX),
                        },
                        Instr::Div(ty, src2),
                        Instr::Mov {
                            ty,
                            src: Operand::Reg(out_reg),
                            dst,
                        },
                    ])
                }
                tacky::Instr::Binary {
                    // Double division uses div instruction
                    binop: tacky::BinaryOp::Divide,
                    src1,
                    src2,
                    dst,
                } if val_asm_type(&src1, symbols) == AsmType::Double => {
                    let dst = self.assemble_val(dst);
                    let src1 = self.assemble_val(src1);
                    let src2 = self.assemble_val(src2);
                    let ty = AsmType::Double;
                    assembly.extend(vec![
                        Instr::Mov { ty, src: src1, dst },
                        Instr::Binary {
                            ty,
                            binop: BinaryOp::DivDouble,
                            src: src2,
                            dst,
                        },
                    ]);
                }
                tacky::Instr::Binary {
                    binop: binop @ (tacky::BinaryOp::Divide | tacky::BinaryOp::Remainder),
                    src1,
                    src2,
                    dst,
                } => {
                    let ty = val_asm_type(&src1, symbols);
                    let dst = self.assemble_val(dst);
                    let src1 = self.assemble_val(src1);
                    let src2 = self.assemble_val(src2);
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
                        tacky::BinaryOp::ShiftRight => {
                            if is_unsigned_type(&src1, symbols) {
                                BinaryOp::ShiftRightLogical
                            } else {
                                BinaryOp::ShiftRight
                            }
                        }
                        _ => panic!("unreachable"),
                    };
                    let dst = self.assemble_val(dst);
                    let ty = val_asm_type(&src1, symbols);
                    assembly.extend(vec![
                        Instr::Mov {
                            ty,
                            src: self.assemble_val(src2),
                            dst: Operand::Reg(Register::CX),
                        },
                        Instr::Mov {
                            ty,
                            src: self.assemble_val(src1),
                            dst,
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
                    let is_double = val_tacky_type(&src1, symbols) == Type::Double;
                    let unsigned_or_double = is_unsigned_type(&src1, symbols) || is_double;
                    let code = match binop {
                        tacky::BinaryOp::Equals => CondCode::E,
                        tacky::BinaryOp::NotEquals => CondCode::NE,
                        tacky::BinaryOp::GreaterThan => {
                            if unsigned_or_double {
                                CondCode::A
                            } else {
                                CondCode::G
                            }
                        }
                        tacky::BinaryOp::GreaterThanEquals => {
                            if unsigned_or_double {
                                CondCode::AE
                            } else {
                                CondCode::GE
                            }
                        }
                        tacky::BinaryOp::LessThan => {
                            if unsigned_or_double {
                                CondCode::B
                            } else {
                                CondCode::L
                            }
                        }
                        tacky::BinaryOp::LessThanEquals => {
                            if unsigned_or_double {
                                CondCode::BE
                            } else {
                                CondCode::LE
                            }
                        }
                        _ => unreachable!(),
                    };
                    assembly.extend(vec![
                        Instr::Cmp {
                            ty: val_asm_type(&src1, symbols),
                            lhs: self.assemble_val(src2),
                            rhs: self.assemble_val(src1),
                        },
                        Instr::Mov {
                            // TODO maybe this should just be longword, or maybe long for doubles? check pg 328
                            ty: val_asm_type(&dst, symbols),
                            src: Operand::Imm(0),
                            dst: self.assemble_val(dst),
                        },
                    ]);
                    if is_double {
                        let (parity_code, op) = if binop == tacky::BinaryOp::NotEquals {
                            (CondCode::P, BinaryOp::BitOr)
                        } else {
                            (CondCode::NP, BinaryOp::BitAnd)
                        };
                        assembly.extend(vec![
                            // zero out eax and ecx
                            Instr::Mov {
                                ty: AsmType::Longword,
                                src: Operand::Imm(0),
                                dst: Operand::Reg(Register::AX),
                            },
                            Instr::Mov {
                                ty: AsmType::Longword,
                                src: Operand::Imm(0),
                                dst: Operand::Reg(Register::CX),
                            },
                            // set eax and ecx to results of both the
                            // comparison and the parity flag, which
                            // is set if the comparison is unordered
                            Instr::SetCC(code, Operand::Reg(Register::AX)),
                            Instr::SetCC(parity_code, Operand::Reg(Register::CX)),
                            // we need the comparison to return true
                            // AND the parity flag to not be set
                            Instr::Binary {
                                ty: AsmType::Longword,
                                binop: op,
                                src: Operand::Reg(Register::AX),
                                dst: Operand::Reg(Register::CX),
                            },
                            Instr::Mov {
                                ty: AsmType::Longword,
                                src: Operand::Reg(Register::CX),
                                dst: self.assemble_val(dst),
                            },
                        ])
                    } else {
                        assembly.push(Instr::SetCC(code, self.assemble_val(dst)));
                    }
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
                    let dst = self.assemble_val(dst);
                    assembly.extend(vec![
                        Instr::Mov {
                            ty: val_asm_type(&src1, symbols),
                            src: self.assemble_val(src1),
                            dst,
                        },
                        Instr::Binary {
                            ty: val_asm_type(&src1, symbols),
                            binop,
                            src: self.assemble_val(src2),
                            dst,
                        },
                    ]);
                }
                jump @ (tacky::Instr::JumpIfZero { condition, target }
                | tacky::Instr::JumpIfNotZero { condition, target }) => {
                    let jump_if_zero = matches!(jump, tacky::Instr::JumpIfZero { .. });
                    let code = if jump_if_zero {
                        CondCode::E
                    } else {
                        CondCode::NE
                    };
                    let ty = val_asm_type(&condition, symbols);
                    let condition = self.assemble_val(condition);
                    if ty == AsmType::Double {
                        assembly.extend(vec![
                            // Doubles use XMM0 for comparison, zero it out
                            Instr::Binary {
                                binop: BinaryOp::BitXOr,
                                ty,
                                src: Operand::Reg(Register::XMM0),
                                dst: Operand::Reg(Register::XMM0),
                            },
                            Instr::Cmp {
                                ty,
                                lhs: condition,
                                rhs: Operand::Reg(Register::XMM0),
                            },
                        ]);
                        if jump_if_zero {
                            let unordered_label = self.new_label("unordered");
                            assembly.extend(vec![
                                // Skip over zero check if it's unordered
                                Instr::JmpCC(CondCode::P, unordered_label),
                                // Jump to the target if it's equal to 0
                                Instr::JmpCC(CondCode::E, target),
                                Instr::Label(unordered_label),
                            ]);
                        } else {
                            assembly.extend(vec![
                                // Jump to the target if it's unorder since NaN is nonzero
                                Instr::JmpCC(CondCode::P, target),
                                // Jump to the target if nonzero
                                Instr::JmpCC(CondCode::NE, target),
                            ])
                        }
                    } else {
                        assembly.extend(vec![
                            Instr::Cmp {
                                ty,
                                lhs: Operand::Imm(0),
                                rhs: condition,
                            },
                            Instr::JmpCC(code, target),
                        ]);
                    }
                }
                tacky::Instr::Call { name, params, dst } => {
                    let int_registers = [
                        Register::DI,
                        Register::SI,
                        Register::DX,
                        Register::CX,
                        Register::R8,
                        Register::R9,
                    ];
                    let double_registers = [
                        Register::XMM0,
                        Register::XMM1,
                        Register::XMM2,
                        Register::XMM3,
                        Register::XMM4,
                        Register::XMM5,
                        Register::XMM6,
                        Register::XMM7,
                    ];

                    let (int_args, double_args, stack_args) = self.classify_params(params, symbols);

                    let stack_padding = if stack_args.len() % 2 == 0 { 0 } else { 8 };
                    if stack_padding != 0 {
                        assembly.push(Instr::Binary {
                            ty: AsmType::Quadword,
                            binop: BinaryOp::Sub,
                            src: Operand::Imm(stack_padding),
                            dst: Operand::Reg(Register::SP),
                        });
                    }

                    for (reg_index, (ty, param)) in int_args.iter().enumerate() {
                        let reg = int_registers[reg_index];
                        assembly.push(Instr::Mov {
                            ty: *ty,
                            src: *param,
                            dst: Operand::Reg(reg),
                        })
                    }

                    for (reg_index, (ty, param)) in double_args.iter().enumerate() {
                        let reg = double_registers[reg_index];
                        assembly.push(Instr::Mov {
                            ty: *ty,
                            src: *param,
                            dst: Operand::Reg(reg),
                        })
                    }

                    for (ty, param) in stack_args.iter().rev() {
                        if matches!(param, Operand::Imm(_) | Operand::Reg(_))
                            || matches!(ty, AsmType::Quadword | AsmType::Double)
                        {
                            assembly.push(Instr::Push(*param));
                        } else {
                            assembly.extend(vec![
                                Instr::Mov {
                                    ty: *ty,
                                    src: *param,
                                    dst: Operand::Reg(Register::AX),
                                },
                                Instr::Push(Operand::Reg(Register::AX)),
                            ]);
                        }
                    }

                    assembly.push(Instr::Call(name));

                    let bytes_to_pop = (8 * stack_args.len()) as i64 + stack_padding;
                    if bytes_to_pop != 0 {
                        assembly.push(Instr::Binary {
                            ty: AsmType::Quadword,
                            binop: BinaryOp::Add,
                            src: Operand::Imm(bytes_to_pop),
                            dst: Operand::Reg(Register::SP),
                        });
                    }
                    let dst_ty = val_asm_type(&dst, symbols);
                    let dst = self.assemble_val(dst);
                    if dst_ty == AsmType::Double {
                        assembly.push(Instr::Mov {
                            ty: AsmType::Double,
                            src: Operand::Reg(Register::XMM0),
                            dst,
                        })
                    } else {
                        assembly.push(Instr::Mov {
                            ty: dst_ty,
                            src: Operand::Reg(Register::AX),
                            dst,
                        })
                    }
                }
                tacky::Instr::SignExtend { src, dst } => assembly.push(Instr::Movsx {
                    src: self.assemble_val(src),
                    dst: self.assemble_val(dst),
                }),
                tacky::Instr::Truncate { src, dst } => assembly.push(Instr::Mov {
                    ty: AsmType::Longword,
                    src: self.assemble_val(src),
                    dst: self.assemble_val(dst),
                }),
                tacky::Instr::ZeroExtend { src, dst } => assembly.push(Instr::MovZeroExtend {
                    src: self.assemble_val(src),
                    dst: self.assemble_val(dst),
                }),
                tacky::Instr::DoubleToInt { src, dst } => assembly.push(Instr::Cvttsd2si {
                    ty: val_asm_type(&dst, symbols),
                    src: self.assemble_val(src),
                    dst: self.assemble_val(dst),
                }),
                tacky::Instr::IntToDouble { src, dst } => assembly.push(Instr::Cvtsi2sd {
                    ty: val_asm_type(&src, symbols),
                    src: self.assemble_val(src),
                    dst: self.assemble_val(dst),
                }),
                tacky::Instr::UIntToDouble { src, dst }
                    if val_tacky_type(&src, symbols) == Type::UInt =>
                {
                    // Unsigned int conversion
                    assembly.extend(vec![
                        Instr::MovZeroExtend {
                            src: self.assemble_val(src),
                            dst: Operand::Reg(Register::AX),
                        },
                        Instr::Cvtsi2sd {
                            ty: AsmType::Quadword,
                            src: Operand::Reg(Register::AX),
                            dst: self.assemble_val(dst),
                        },
                    ])
                }
                tacky::Instr::UIntToDouble { src, dst } => {
                    // Unsigned long conversion
                    let src = self.assemble_val(src);
                    let dst = self.assemble_val(dst);
                    let out_of_range = self.new_label("out_of_range");
                    let end = self.new_label("cvt_end");
                    let r1 = Operand::Reg(Register::AX);
                    let r2 = Operand::Reg(Register::DX);
                    assembly.extend(vec![
                        // Check if integer is > 0
                        Instr::Cmp {
                            ty: AsmType::Quadword,
                            lhs: Operand::Imm(0),
                            rhs: src,
                        },
                        Instr::JmpCC(CondCode::L, out_of_range),
                        // Do the conversion if so
                        Instr::Cvtsi2sd {
                            ty: AsmType::Quadword,
                            src,
                            dst,
                        },
                        Instr::Jmp(end),
                        Instr::Label(out_of_range),
                        // Get the src into a register so it can be shifted to divide by 2
                        Instr::Mov {
                            ty: AsmType::Quadword,
                            src,
                            dst: r1,
                        },
                        Instr::Mov {
                            ty: AsmType::Quadword,
                            src: r1,
                            dst: r2,
                        },
                        // Half the src
                        Instr::Unary {
                            ty: AsmType::Quadword,
                            unop: UnaryOp::Shr,
                            dst: r2,
                        },
                        // Round up to odd
                        Instr::Binary {
                            ty: AsmType::Quadword,
                            binop: BinaryOp::BitAnd,
                            src: Operand::Imm(1),
                            dst: r1,
                        },
                        Instr::Binary {
                            ty: AsmType::Quadword,
                            binop: BinaryOp::BitOr,
                            src: r1,
                            dst: r2,
                        },
                        // Do the conversion
                        Instr::Cvtsi2sd {
                            ty: AsmType::Quadword,
                            src: r2,
                            dst,
                        },
                        // Undo the halving
                        Instr::Binary {
                            ty: AsmType::Double,
                            binop: BinaryOp::Add,
                            src: dst,
                            dst,
                        },
                        Instr::Label(end),
                    ]);
                }
                tacky::Instr::DoubleToUInt { src, dst }
                    if val_tacky_type(&src, symbols) == Type::UInt =>
                {
                    // Unsigned int conversion
                    assembly.extend(vec![
                        Instr::Cvttsd2si {
                            ty: AsmType::Quadword,
                            src: self.assemble_val(src),
                            dst: Operand::Reg(Register::AX),
                        },
                        Instr::MovZeroExtend {
                            src: Operand::Reg(Register::AX),
                            dst: self.assemble_val(dst),
                        },
                    ])
                }
                tacky::Instr::DoubleToUInt { src, dst } => {
                    // Unsigned long conversion
                    let upper_bound =
                        Operand::Data(self.add_static_constant(9223372036854775808.0, 8));
                    let src = self.assemble_val(src);
                    let dst = self.assemble_val(dst);
                    let out_of_range = self.new_label("out_of_range");
                    let end = self.new_label("end");
                    let x_reg = Operand::Reg(Register::XMM1);
                    let r_reg = Operand::Reg(Register::DX);
                    assembly.extend(vec![
                        Instr::Cmp {
                            ty: AsmType::Double,
                            lhs: upper_bound,
                            rhs: src,
                        },
                        Instr::JmpCC(CondCode::AE, out_of_range),
                        Instr::Cvttsd2si {
                            ty: AsmType::Quadword,
                            src,
                            dst,
                        },
                        Instr::Jmp(end),
                        Instr::Label(out_of_range),
                        Instr::Mov {
                            ty: AsmType::Double,
                            src,
                            dst: x_reg,
                        },
                        Instr::Binary {
                            ty: AsmType::Double,
                            binop: BinaryOp::Sub,
                            src: upper_bound,
                            dst: x_reg,
                        },
                        Instr::Cvttsd2si {
                            ty: AsmType::Quadword,
                            src: x_reg,
                            dst,
                        },
                        Instr::Mov {
                            ty: AsmType::Quadword,
                            // Out of bounds number can't fit in an i64 of course, but this gets the right bits
                            src: Operand::Imm(((i64::MAX as u64) + 1) as i64),
                            dst: r_reg,
                        },
                        Instr::Binary {
                            ty: AsmType::Quadword,
                            binop: BinaryOp::Add,
                            src: r_reg,
                            dst,
                        },
                        Instr::Label(end),
                    ]);
                }
                tacky::Instr::GetAddress { src, dst } => assembly.push(Instr::Lea {
                    src: self.assemble_val(src),
                    dst: self.assemble_val(dst),
                }),
                tacky::Instr::Load { ptr, dst } => assembly.extend(vec![
                    Instr::Mov {
                        ty: AsmType::Quadword,
                        src: self.assemble_val(ptr),
                        dst: Operand::Reg(Register::AX),
                    },
                    Instr::Mov {
                        ty: val_asm_type(&dst, symbols),
                        src: Operand::Memory(Register::AX, 0),
                        dst: self.assemble_val(dst),
                    },
                ]),
                tacky::Instr::Store { src, ptr } => assembly.extend(vec![
                    Instr::Mov {
                        ty: AsmType::Quadword,
                        src: self.assemble_val(ptr),
                        dst: Operand::Reg(Register::AX),
                    },
                    Instr::Mov {
                        ty: val_asm_type(&src, symbols),
                        src: self.assemble_val(src),
                        dst: Operand::Memory(Register::AX, 0),
                    },
                ]),
            }
        }
        assembly
    }

    fn classify_params(
        &mut self,
        values: Vec<Val>,
        symbols: &HashMap<Symbol, (Type, Attrs)>,
    ) -> ClassifiedParams {
        let mut int_reg_args = Vec::new();
        let mut double_reg_args = Vec::new();
        let mut stack_args = Vec::new();

        for v in values {
            let ty = val_asm_type(&v, symbols);
            let v = self.assemble_val(v);
            let typed_operand = (ty, v);
            if ty == AsmType::Double {
                if double_reg_args.len() < 8 {
                    double_reg_args.push(typed_operand);
                } else {
                    stack_args.push(typed_operand);
                }
            } else {
                if int_reg_args.len() < 6 {
                    int_reg_args.push(typed_operand);
                } else {
                    stack_args.push(typed_operand);
                }
            }
        }

        (int_reg_args, double_reg_args, stack_args)
    }

    fn assemble_val(&mut self, val: Val) -> Operand {
        match val {
            Val::Constant(Const::Int(n)) => Operand::Imm(n.into()),
            Val::Constant(Const::Long(n)) => Operand::Imm(n),
            Val::Constant(Const::UInt(n)) => Operand::Imm(n.into()),
            Val::Constant(Const::ULong(n)) => Operand::Imm(n as i64),
            Val::Constant(Const::Double(n)) => {
                let label = self.add_static_constant(n, 8);
                Operand::Data(label)
            }
            Val::Var(s) => Operand::Pseudo(s),
        }
    }

    fn add_static_constant(&mut self, value: f64, alignment: u8) -> Symbol {
        // Use bits as key to handle both -0.0 and 0.0 distinctly
        let bits = value.to_bits();

        if let Some(AsmTopLevel::StaticConst { name, .. }) =
            // Use the existing constant if we have it
            self.constants.get(&(bits, alignment))
        {
            *name
        } else {
            // Generate a new constants
            let label = self.new_label("double");
            self.constants.insert(
                (bits, alignment),
                AsmTopLevel::StaticConst {
                    name: label,
                    alignment,
                    init: StaticInit::Double(value),
                },
            );
            label
        }
    }

    fn new_label(&mut self, name: &'static str) -> Symbol {
        let count = self.count;
        self.count += 1;
        self.interner.intern(format!("{name}.{count}"))
    }
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
            Instr::Div(_, _) => {
                let div = std::mem::replace(instr, Instr::Ret);
                let Instr::Div(ty, op) = div else {
                    panic!("unreachable")
                };
                let op = replace_op(op, &mut replace_state);
                *instr = Instr::Div(ty, op);
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
            Instr::MovZeroExtend { .. } => {
                let movzx = std::mem::replace(instr, Instr::Ret);
                let Instr::MovZeroExtend { src, dst } = movzx else {
                    panic!("unreachable")
                };
                let new_src = replace_op(src, &mut replace_state);
                let new_dst = replace_op(dst, &mut replace_state);
                *instr = Instr::MovZeroExtend {
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
            Instr::Cvtsi2sd { .. } => {
                let cvtsi2sd = std::mem::replace(instr, Instr::Ret);
                let Instr::Cvtsi2sd { ty, src, dst } = cvtsi2sd else {
                    unreachable!()
                };
                let src = replace_op(src, &mut replace_state);
                let dst = replace_op(dst, &mut replace_state);
                *instr = Instr::Cvtsi2sd { ty, src, dst };
            }
            Instr::Cvttsd2si { .. } => {
                let cvtsd2si = std::mem::replace(instr, Instr::Ret);
                let Instr::Cvttsd2si { ty, src, dst } = cvtsd2si else {
                    unreachable!()
                };
                let src = replace_op(src, &mut replace_state);
                let dst = replace_op(dst, &mut replace_state);
                *instr = Instr::Cvttsd2si { ty, src, dst };
            }
            Instr::Lea { .. } => {
                let lea = std::mem::replace(instr, Instr::Ret);
                let Instr::Lea { src, dst } = lea else {
                    unreachable!()
                };
                let src = replace_op(src, &mut replace_state);
                let dst = replace_op(dst, &mut replace_state);
                *instr = Instr::Lea { src, dst };
            }
            Instr::JmpCC(_, _)
            | Instr::Label(_)
            | Instr::Jmp(_)
            | Instr::Call(_)
            | Instr::Ret
            | Instr::Cdq(_) => (),
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
                    // Quadwords and doubles get 8 bytes of stack space, 4 otherwise
                    let eight_bytes = matches!(*ty, AsmType::Quadword | AsmType::Double);
                    let stack_size = if eight_bytes { 8 } else { 4 };
                    state.max_offset += stack_size;
                    // Ensure proper alignment by adding another 4
                    // bytes if a quadword would not be at an offset
                    // which is a multiple of 8
                    if eight_bytes && !state.max_offset.is_multiple_of(8) {
                        state.max_offset += 4;
                    }
                    state.max_offset
                });
                Operand::Memory(Register::BP, -(*offset as i16))
            }
        }
        op => op,
    }
}

fn is_memory(op: &Operand) -> bool {
    matches!(op, Operand::Data(_) | Operand::Memory(_, _))
}

// Replace instructions if their operands are in the wrong places.
fn fixup_instructions(instrs: Vec<Instr>) -> Vec<Instr> {
    let mut fixed = Vec::new();
    for instr in instrs {
        match instr {
            // Mov can't have both operands in memory. Select a scratch register based on type
            Instr::Mov { ty, src: s, dst: d } if is_memory(&s) || is_memory(&d) => {
                let reg = get_scratch_reg(ty);
                fixed.extend(vec![
                    Instr::Mov {
                        ty,
                        src: s,
                        dst: reg,
                    },
                    Instr::Mov {
                        ty,
                        src: reg,
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
            // addsd, subsd, mulsd, divsd, xorpd must have a register
            // destination. move their destination into XMM15, do the
            // operation, then move XMM15 into the original destination
            Instr::Binary {
                ty: AsmType::Double,
                binop:
                    binop @ (BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mult
                    | BinaryOp::BitXOr
                    | BinaryOp::DivDouble),
                src,
                dst,
            } if matches!(src, Operand::Imm(_)) || is_memory(&dst) => {
                let new_src = if matches!(src, Operand::Imm(_)) {
                    fixed.extend(vec![
                        Instr::Mov {
                            ty: AsmType::Longword,
                            src,
                            dst: Operand::Reg(Register::R10),
                        },
                        Instr::Cvtsi2sd {
                            ty: AsmType::Longword,
                            src: Operand::Reg(Register::R10),
                            dst: Operand::Reg(Register::XMM14),
                        },
                    ]);
                    Operand::Reg(Register::XMM14)
                } else {
                    src
                };
                fixed.extend(vec![
                    Instr::Mov {
                        ty: AsmType::Double,
                        src: dst,
                        dst: Operand::Reg(Register::XMM15),
                    },
                    Instr::Binary {
                        ty: AsmType::Double,
                        binop,
                        src: new_src,
                        dst: Operand::Reg(Register::XMM15),
                    },
                    Instr::Mov {
                        ty: AsmType::Double,
                        src: Operand::Reg(Register::XMM15),
                        dst,
                    },
                ])
            }

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
                let reg = get_scratch_reg(ty);
                fixed.extend(vec![
                    Instr::Mov {
                        ty,
                        src: s,
                        dst: reg,
                    },
                    Instr::Binary {
                        ty,
                        binop,
                        src: reg,
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
                let mut new_src = src;
                if needs_src_rewrite {
                    new_src = Operand::Reg(Register::R11);
                    fixed.push(Instr::Mov {
                        ty: AsmType::Quadword,
                        src,
                        dst: new_src,
                    });
                }
                let reg = get_scratch_reg(ty);

                fixed.extend(vec![
                    Instr::Mov {
                        ty,
                        src: d,
                        dst: reg,
                    },
                    Instr::Binary {
                        ty,
                        binop: BinaryOp::Mult,
                        src: new_src,
                        dst: reg,
                    },
                    Instr::Mov {
                        ty,
                        src: reg,
                        dst: d,
                    },
                ])
            }
            // Idiv can't use an immediate as its destination so move into a register
            Instr::IDiv(ty, Operand::Imm(n)) => fixed.extend(vec![
                Instr::Mov {
                    ty,
                    src: Operand::Imm(n),
                    dst: Operand::Reg(Register::R10),
                },
                Instr::IDiv(ty, Operand::Reg(Register::R10)),
            ]),
            // div can't use an immediate as its destination so move into a register
            Instr::Div(ty, Operand::Imm(n)) => fixed.extend(vec![
                Instr::Mov {
                    ty,
                    src: Operand::Imm(n),
                    dst: Operand::Reg(Register::R10),
                },
                Instr::Div(ty, Operand::Reg(Register::R10)),
            ]),
            // Comisd must have a register for its rhs
            Instr::Cmp {
                ty: AsmType::Double,
                lhs,
                rhs,
            } if is_memory(&rhs) => fixed.extend(vec![
                Instr::Mov {
                    ty: AsmType::Double,
                    src: rhs,
                    dst: Operand::Reg(Register::XMM15),
                },
                Instr::Cmp {
                    ty: AsmType::Double,
                    lhs,
                    rhs: Operand::Reg(Register::XMM15),
                },
            ]),
            Instr::Cmp {
                ty,
                lhs,
                rhs: Operand::Imm(n),
            } => {
                let needs_lhs_rewrite =
                    ty == AsmType::Quadword && matches!(lhs, Operand::Imm(n) if !in_int_range(n));
                let mut new_lhs = lhs;
                if needs_lhs_rewrite {
                    new_lhs = Operand::Reg(Register::R10);
                    fixed.push(Instr::Mov {
                        ty: AsmType::Quadword,
                        src: lhs,
                        dst: new_lhs,
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
            Instr::Cmp { ty, lhs: l, rhs: r }
                if ty != AsmType::Double && (is_memory(&l) || is_memory(&r)) =>
            {
                // cmp can't use memory locations for integer types
                // TODO: pretty sure this should be that BOTH lhs and rhs are in memory, not either
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
            Instr::Push(Operand::Reg(reg)) if is_xmm_reg(reg) => fixed.extend(vec![
                Instr::Binary {
                    ty: AsmType::Quadword,
                    binop: BinaryOp::Sub,
                    src: Operand::Imm(8),
                    dst: Operand::Reg(Register::SP),
                },
                Instr::Mov {
                    ty: AsmType::Double,
                    src: Operand::Reg(reg),
                    dst: Operand::Memory(Register::SP, 0),
                },
            ]),
            // lea can't have a non-register destination
            Instr::Lea { src, dst } if !matches!(dst, Operand::Reg(_)) => fixed.extend(vec![
                Instr::Lea {
                    src,
                    dst: Operand::Reg(Register::R10),
                },
                Instr::Mov {
                    ty: AsmType::Quadword,
                    src: Operand::Reg(Register::R10),
                    dst,
                },
            ]),
            Instr::Movsx { src, dst } => {
                let mut new_src = src;
                // movsx can't have an immediate as its source
                if matches!(src, Operand::Imm(_)) {
                    fixed.push(Instr::Mov {
                        ty: AsmType::Longword,
                        src,
                        dst: Operand::Reg(Register::R10),
                    });
                    new_src = Operand::Reg(Register::R10);
                }
                let mut post_instr = vec![];
                let mut new_dst = dst;
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
            Instr::MovZeroExtend {
                src,
                dst: Operand::Reg(reg),
            } =>
            // Zero extending by moving into a register
            {
                fixed.push(Instr::Mov {
                    ty: AsmType::Longword,
                    src,
                    dst: Operand::Reg(reg),
                })
            }
            // Move into a register then back to memory
            Instr::MovZeroExtend { src, dst } if is_memory(&dst) => fixed.extend(vec![
                Instr::Mov {
                    ty: AsmType::Longword,
                    src,
                    dst: Operand::Reg(Register::R11),
                },
                Instr::Mov {
                    ty: AsmType::Quadword,
                    src: Operand::Reg(Register::R11),
                    dst,
                },
            ]),
            // Cvttsd2si must have a non-constant source and register destination
            Instr::Cvttsd2si { ty, src, dst } if is_memory(&dst) => fixed.extend(vec![
                Instr::Cvttsd2si {
                    ty,
                    src,
                    dst: Operand::Reg(Register::R11),
                },
                Instr::Mov {
                    ty,
                    src: Operand::Reg(Register::R11),
                    dst,
                },
            ]),
            // Cvttsi2sd must have a non-constant source and register destination
            Instr::Cvtsi2sd { ty, src, dst }
                if is_memory(&dst) || matches!(src, Operand::Imm(_)) =>
            {
                let src_reg = if matches!(src, Operand::Imm(_)) {
                    let reg = Operand::Reg(Register::R10);
                    fixed.push(Instr::Mov { ty, src, dst: reg });
                    reg
                } else {
                    src
                };
                if is_memory(&dst) {
                    let dst_reg = Operand::Reg(Register::XMM15);
                    fixed.extend(vec![
                        Instr::Cvtsi2sd {
                            ty,
                            src: src_reg,
                            dst: dst_reg,
                        },
                        Instr::Mov {
                            ty: AsmType::Double,
                            src: dst_reg,
                            dst,
                        },
                    ])
                } else {
                    fixed.push(Instr::Cvtsi2sd {
                        ty,
                        src: src_reg,
                        dst,
                    })
                }
            }
            i => fixed.push(i),
        }
    }
    fixed
}

fn get_scratch_reg(ty: AsmType) -> Operand {
    Operand::Reg(if ty == AsmType::Double {
        Register::XMM15
    } else {
        Register::R10
    })
}

fn is_xmm_reg(reg: Register) -> bool {
    matches!(
        reg,
        Register::XMM0
            | Register::XMM1
            | Register::XMM2
            | Register::XMM3
            | Register::XMM4
            | Register::XMM5
            | Register::XMM6
            | Register::XMM7
            | Register::XMM14
            | Register::XMM15
    )
}

// Determine if a constant is in the 32-bit range. In order to avoid
// issues comparing signed and unsigned numbers, we bitcast both to u64
fn in_int_range(n: i64) -> bool {
    n as u64 <= i32::MAX as u64
}

pub enum AsmEntry {
    Obj {
        ty: AsmType,
        is_static: bool,
        is_constant: bool,
    },
    Fun {
        _defined: bool,
    },
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
                    is_constant: false,
                }
            }
        };
        backend_symbols.insert(*name, entry);
    }
    backend_symbols
}
