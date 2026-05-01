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
pub enum CompoundOperator {
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
}

impl Display for Const {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Const::Int(n) => write!(f, "{}", n),
            Const::Long(n) => write!(f, "{}", n),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Constant(Const),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Compound(CompoundOperator, Box<Expression>, Box<Expression>),
    Crement(Fixity, Crement, Box<Expression>),
    Var(Symbol),
    Assign(Box<Expression>, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    Call(Symbol, Vec<Expression>),
    Cast(Type, Box<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Return(Expression),
    Exp(Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Goto(Symbol),
    Label(Symbol, Box<Statement>),
    Compound(Vec<BlockItem>),
    Break(Symbol),
    Continue(Symbol),
    While(Symbol, Expression, Box<Statement>),
    For(
        Symbol,
        ForInit,
        Option<Expression>,
        Option<Expression>,
        Box<Statement>,
    ),
    DoWhile(Symbol, Box<Statement>, Expression),
    Switch {
        label: Symbol,
        expr: Expression,
        body: Box<Statement>,
        cases: Vec<CaseInfo>,
    },
    Case(Symbol, Expression, Box<Statement>),
    Default(Symbol, Box<Statement>),
    Null,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CaseInfo {
    Case { expr: Const, label: Symbol },
    Default { label: Symbol },
}

#[derive(Debug, PartialEq, Clone)]
pub enum ForInit {
    Decl(Var),
    Exp(Expression),
    Null,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration {
    Var(Var),
    Func(Function),
}

#[derive(Debug, PartialEq, Clone)]
pub enum BlockItem {
    S(Statement),
    D(Declaration),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Long,
    Fun(Vec<Type>, Box<Type>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Var {
    pub name: Symbol,
    pub init: Option<Expression>,
    pub storage: Option<StorageClass>,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub name: Symbol,
    pub params: Vec<Symbol>,
    pub body: Option<Vec<BlockItem>>,
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

pub struct Program(pub Vec<Declaration>);
