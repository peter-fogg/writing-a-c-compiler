use std::iter::Peekable;

use crate::lexer::{Lexer, Token};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOperator {
    Complement,
    Negate,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Constant(i32),
    Unary(UnaryOperator, Box<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Program<'a> {
    Function(&'a str, Statement),
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

        let expr = self.expression();

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

    fn expression(&mut self) -> Expression {
        match self.tokens.peek() {
            Some(Token::Constant(_)) => return self.constant(),
            Some(Token::LParen) => {
                self.tokens.next();
                let sub_expr = self.expression();
                self.consume(Token::RParen);
                return sub_expr;
            }
            Some(Token::Tilde | Token::Minus) => {
                let un_op = self.unary_op();
                let inner_expr = self.expression();
                return Expression::Unary(un_op, Box::new(inner_expr));
            }
            _ => panic!(),
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
