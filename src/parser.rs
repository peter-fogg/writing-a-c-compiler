use std::iter::Peekable;

use crate::lexer::{Lexer, Token};

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
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Constant(i32),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Var(String),
    Assign(Box<Expression>, Box<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Return(Expression),
    Exp(Expression),
    Null,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Declaration {
    pub name: String,
    pub init: Option<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BlockItem {
    S(Statement),
    D(Declaration),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Program {
    Function(String, Vec<BlockItem>),
}

#[derive(Debug, PartialEq, PartialOrd, Copy, Clone)]
enum Prec {
    Bottom,
    Assign,
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
    Top,
}

type TokenStream<'a> = Peekable<Lexer<'a>>;

pub struct Parser<'a> {
    tokens: TokenStream<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: TokenStream<'a>) -> Self {
        Self { tokens }
    }

    pub fn parse(&mut self) -> Program {
        self.consume(Token::Int); // only int returns for now
        let fn_name = match self.tokens.next() {
            Some(Token::Id(s)) => s,
            _ => {
                panic!("Bad parse");
            }
        };
        self.consume(Token::LParen);
        self.consume(Token::Void);
        self.consume(Token::RParen);
        self.consume(Token::LBrace);

        let mut block_items = Vec::new();

        while self.tokens.peek() != Some(&Token::RBrace) {
            let item = self.block_item();
            block_items.push(item);
        }

        self.consume(Token::RBrace);

        let program = Program::Function(fn_name.to_string(), block_items);

        match self.tokens.peek() {
            None => (),
            Some(t) => panic!("Extra junk at end: {:?}", t),
        }

        program
    }

    fn declaration(&mut self) -> Declaration {
        self.consume(Token::Int);
        let name = match self.tokens.next() {
            Some(Token::Id(id)) => id.to_string(),
            t => panic!("Expected identifier, got {:?}", t),
        };

        let init = match self.tokens.peek() {
            Some(Token::Equals) => {
                self.tokens.next();
                Some(self.expression(Prec::Bottom))
            }
            Some(Token::Semicolon) => None,
            Some(t) => panic!("Expected assignment or ;, got {:?}", t),
            None => None,
        };

        self.consume(Token::Semicolon);
        Declaration { name, init }
    }

    fn block_item(&mut self) -> BlockItem {
        match self.tokens.peek() {
            Some(Token::Int) => BlockItem::D(self.declaration()),
            Some(_) => BlockItem::S(self.statement()),
            None => panic!("Unexpected end of input parsing block item"),
        }
    }

    fn statement(&mut self) -> Statement {
        match self.tokens.peek() {
            Some(Token::Return) => {
                self.consume(Token::Return);

                let expr = self.expression(Prec::Bottom);

                self.consume(Token::Semicolon);

                Statement::Return(expr)
            }
            Some(Token::Semicolon) => {
                self.consume(Token::Semicolon);
                Statement::Null
            }
            Some(_) => {
                let expr = Statement::Exp(self.expression(Prec::Bottom));
                self.consume(Token::Semicolon);
                expr
            }
            None => panic!("Unexpected end of input parsing statement"),
        }
    }

    fn constant(&mut self) -> Expression {
        let n_str = match self.tokens.next() {
            Some(Token::Constant(n_str)) => n_str,
            err => panic!("bad numeric parse: {:?}", err),
        };

        let n = match n_str.parse::<i32>() {
            Ok(n) => n,
            err => panic!("bad numeric parse: {:?}", err),
        };

        Expression::Constant(n)
    }

    fn get_prec(t: Token) -> Prec {
        match t {
            Token::Constant(_) => Prec::Expr,
            Token::Equals => Prec::Assign,
            Token::Plus | Token::Minus => Prec::AddSub,
            Token::Percent | Token::Star | Token::Slash => Prec::MultDiv,
            Token::Pipe => Prec::BitOr,
            Token::Ampersand => Prec::BitAnd,
            Token::Caret => Prec::BitXOr,
            Token::DoubleLAngle | Token::DoubleRAngle => Prec::Shift,
            Token::DoubleEquals | Token::BangEquals => Prec::Equals,
            Token::LAngleEquals | Token::LAngle | Token::RAngleEquals | Token::RAngle => {
                Prec::Comparison
            }
            Token::DoubleAmpersand => Prec::And,
            Token::DoublePipe => Prec::Or,
            _ => Prec::Bottom,
        }
    }

    fn expression(&mut self, prec: Prec) -> Expression {
        let mut lhs = self.factor();
        let mut next = *self
            .tokens
            .peek()
            .unwrap_or_else(|| panic!("Ran out of tokens while parsing expression"));
        while Self::is_binary_op(&next) && Self::get_prec(next) >= prec {
            let next_prec = Self::get_prec(next);
            if next == Token::Equals {
                self.consume(Token::Equals);
                let rhs = self.expression(next_prec);
                lhs = Expression::Assign(Box::new(lhs), Box::new(rhs));
            } else {
                let binop = self.binary_op();
                let rhs = self.expression(Self::increment_prec(&next_prec));
                lhs = Expression::Binary(binop, Box::new(lhs), Box::new(rhs));
            }
            next = *self
                .tokens
                .peek()
                .unwrap_or_else(|| panic!("Ran out of tokens while parsing expression"));
        }

        lhs
    }

    fn increment_prec(prec: &Prec) -> Prec {
        match prec {
            Prec::Bottom => Prec::Assign,
            Prec::Assign => Prec::Expr,
            Prec::Expr => Prec::Or,
            Prec::Or => Prec::And,
            Prec::And => Prec::BitOr,
            Prec::BitOr => Prec::BitXOr,
            Prec::BitXOr => Prec::BitAnd,
            Prec::BitAnd => Prec::Equals,
            Prec::Equals => Prec::Comparison,
            Prec::Comparison => Prec::Shift,
            Prec::Shift => Prec::AddSub,
            Prec::AddSub => Prec::MultDiv,
            _ => Prec::Top,
        }
    }

    fn is_binary_op(token: &Token) -> bool {
        [
            Token::Plus,
            Token::Minus,
            Token::Star,
            Token::Slash,
            Token::Percent,
            Token::Ampersand,
            Token::Pipe,
            Token::Caret,
            Token::DoubleLAngle,
            Token::DoubleRAngle,
            Token::BangEquals,
            Token::DoubleEquals,
            Token::DoubleAmpersand,
            Token::DoublePipe,
            Token::RAngle,
            Token::RAngleEquals,
            Token::LAngle,
            Token::LAngleEquals,
            Token::Equals,
        ]
        .contains(token)
    }

    fn factor(&mut self) -> Expression {
        match self.tokens.peek() {
            Some(Token::Constant(_)) => self.constant(),
            Some(Token::LParen) => {
                self.tokens.next();
                let sub_expr = self.expression(Prec::Bottom);
                self.consume(Token::RParen);
                sub_expr
            }
            Some(Token::Tilde | Token::Minus | Token::Bang) => {
                let un_op = self.unary_op();
                let inner_expr = self.factor();
                Expression::Unary(un_op, Box::new(inner_expr))
            }
            Some(Token::Id(id)) => {
                let id = id.to_string();
                self.tokens.next();
                Expression::Var(id)
            }
            t => panic!("Unexpected token {:?}", t),
        }
    }

    fn binary_op(&mut self) -> BinaryOperator {
        match self.tokens.next() {
            None => panic!("Ran out of tokens while parsing expression"),
            Some(Token::Plus) => BinaryOperator::Add,
            Some(Token::Minus) => BinaryOperator::Subtract,
            Some(Token::Star) => BinaryOperator::Multiply,
            Some(Token::Slash) => BinaryOperator::Divide,
            Some(Token::Percent) => BinaryOperator::Remainder,
            Some(Token::Ampersand) => BinaryOperator::BitAnd,
            Some(Token::Pipe) => BinaryOperator::BitOr,
            Some(Token::Caret) => BinaryOperator::BitXOr,
            Some(Token::DoubleLAngle) => BinaryOperator::ShiftLeft,
            Some(Token::DoubleRAngle) => BinaryOperator::ShiftRight,
            Some(Token::DoubleAmpersand) => BinaryOperator::And,
            Some(Token::DoublePipe) => BinaryOperator::Or,
            Some(Token::DoubleEquals) => BinaryOperator::Equal,
            Some(Token::BangEquals) => BinaryOperator::NotEqual,
            Some(Token::RAngle) => BinaryOperator::Greater,
            Some(Token::RAngleEquals) => BinaryOperator::GreaterOrEqual,
            Some(Token::LAngle) => BinaryOperator::Less,
            Some(Token::LAngleEquals) => BinaryOperator::LessOrEqual,
            Some(t) => panic!("Expected binary operator, got {:?}", t),
        }
    }

    fn unary_op(&mut self) -> UnaryOperator {
        match self.tokens.next() {
            Some(Token::Tilde) => UnaryOperator::Complement,
            Some(Token::Minus) => UnaryOperator::Negate,
            Some(Token::Bang) => UnaryOperator::Not,
            _ => panic!("unreachable"),
        }
    }

    fn consume(&mut self, token: Token) {
        let next_token = self.tokens.peek();
        match next_token {
            Some(t) if *t == token => {
                self.tokens.next();
            }
            t => panic!("Expected {:?}, got {:?}", token, t),
        }
    }
}
