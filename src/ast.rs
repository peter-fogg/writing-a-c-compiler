use std::fmt::{Display, Error, Formatter};

use crate::interner::Symbol;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitAnd,
    BitOr,
    BitXOr,
    ShiftLeft,
    ShiftRight,
    And,
    Or,
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Conditional,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Fixity {
    Pre,
    Post,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Crement {
    Inc,
    Dec,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Const {
    Int(i32),
    Long(i64),
    UInt(u32),
    ULong(u64),
    Double(f64),
}

impl Display for Const {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Const::Int(n) => write!(f, "{}", n),
            Const::Long(n) => write!(f, "{}", n),
            Const::UInt(n) => write!(f, "{}", n),
            Const::ULong(n) => write!(f, "{}", n),
            Const::Double(n) => write!(f, "{}", n),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Constant(Const),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Compound(BinaryOperator, Box<Expression>, Box<Expression>),
    Crement(Fixity, Crement, Box<Expression>),
    Var(Symbol),
    Assign(Box<Expression>, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    Call(Symbol, Vec<Expression>),
    Cast(Type, Box<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<E> {
    Return(E),
    Exp(E),
    If(E, Box<Statement<E>>, Option<Box<Statement<E>>>),
    Goto(Symbol),
    Label(Symbol, Box<Statement<E>>),
    Compound(Vec<BlockItem<E>>),
    Break(Symbol),
    Continue(Symbol),
    While(Symbol, E, Box<Statement<E>>),
    For(Symbol, ForInit<E>, Option<E>, Option<E>, Box<Statement<E>>),
    DoWhile(Symbol, Box<Statement<E>>, E),
    Switch {
        label: Symbol,
        expr: E,
        body: Box<Statement<E>>,
        cases: Vec<CaseInfo>,
    },
    Case(Symbol, E, Box<Statement<E>>),
    Default(Symbol, Box<Statement<E>>),
    Null,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CaseInfo {
    Case { expr: Const, label: Symbol },
    Default { label: Symbol },
}

#[derive(Debug, PartialEq, Clone)]
pub enum ForInit<E> {
    Decl(Var<E>),
    Exp(E),
    Null,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration<E> {
    Var(Var<E>),
    Func(Function<E>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum BlockItem<E> {
    S(Statement<E>),
    D(Declaration<E>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Long,
    UInt,
    ULong,
    Double,
    Fun(Vec<Type>, Box<Type>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Var<E> {
    pub name: Symbol,
    pub init: Option<E>,
    pub storage: Option<StorageClass>,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function<E> {
    pub name: Symbol,
    pub params: Vec<Symbol>,
    pub body: Option<Vec<BlockItem<E>>>,
    pub storage: Option<StorageClass>,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum StorageClass {
    Static,
    Extern,
}

#[derive(Debug, PartialEq, PartialOrd, Copy, Clone)]
pub enum Prec {
    Bottom,
    Assign,
    Cond,
    Expr,
    Or,
    And,
    BitOr,
    BitXOr,
    BitAnd,
    Equals,
    Comparison,
    Shift,
    AddSub,
    MultDiv,
    Unary,
    Postfix,
    Top,
}

pub struct Program<E>(pub Vec<Declaration<E>>);
