use std::iter::Peekable;

use crate::lexer::{Lexer, Token};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOperator {
    Complement,
    Negate,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Constant(i32),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Program<'a> {
    Function(&'a str, Statement),
}

#[derive(Debug, PartialEq, PartialOrd, Copy, Clone)]
enum Prec {
    Bottom,
    Expr,
    AddSub,
    MultDiv,
    Top,
}

type TokenStream<'a> = Peekable<Lexer<'a>>;

pub type ParseOutput<'a> = Program<'a>;

pub struct Parser<'a> {
    tokens: TokenStream<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: TokenStream<'a>) -> Self {
        Self { tokens }
    }

    pub fn parse(&mut self) -> ParseOutput<'a> {
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

        let body = self.statement();

        self.consume(Token::RBrace);

        let program = Program::Function(fn_name, body);

        match self.tokens.peek() {
            None => (),
            Some(t) => panic!("Extra junk at end: {:?}", t),
        }

        program
    }

    fn statement(&mut self) -> Statement {
        self.consume(Token::Return);

        let expr = self.expression(Prec::Bottom);

        self.consume(Token::Semicolon);

        Statement::Return(expr)
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
            Token::Plus | Token::Minus => Prec::AddSub,
            Token::Percent | Token::Star | Token::Slash => Prec::MultDiv,
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
            let binop = self.binary_op();
            let next_prec = Self::get_prec(next);
            let rhs = self.expression(Self::increment_prec(&next_prec));
            lhs = Expression::Binary(binop, Box::new(lhs), Box::new(rhs));
            next = *self
                .tokens
                .peek()
                .unwrap_or_else(|| panic!("Ran out of tokens while parsing expression"));
        }

        lhs
    }

    fn increment_prec(prec: &Prec) -> Prec {
        match prec {
            Prec::Bottom => Prec::Expr,
            Prec::Expr => Prec::AddSub,
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
        ]
        .contains(token)
    }

    fn factor(&mut self) -> Expression {
        match self.tokens.peek() {
            Some(Token::Constant(_)) => self.constant(),
            Some(Token::LParen) => {
                self.tokens.next();
                let sub_expr = self.expression(Prec::Expr);
                self.consume(Token::RParen);
                sub_expr
            }
            Some(Token::Tilde | Token::Minus) => {
                let un_op = self.unary_op();
                let inner_expr = self.factor();
                Expression::Unary(un_op, Box::new(inner_expr))
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
            Some(t) => panic!("Expected binary operator, got {:?}", t),
        }
    }

    fn unary_op(&mut self) -> UnaryOperator {
        match self.tokens.next() {
            Some(Token::Tilde) => UnaryOperator::Complement,
            Some(Token::Minus) => UnaryOperator::Negate,
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
