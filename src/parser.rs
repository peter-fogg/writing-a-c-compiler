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

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Constant(i32),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Compound(CompoundOperator, Box<Expression>, Box<Expression>),
    Crement(Fixity, Crement, Box<Expression>),
    Var(String),
    Assign(Box<Expression>, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Return(Expression),
    Exp(Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Goto(String),
    Label(String),
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

pub struct Parser<'a> {
    tokens: Lexer<'a>,
    current: Option<Token<'a>>,
    next: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(mut tokens: Lexer<'a>) -> Self {
        let current = tokens.next();
        let next = tokens.next();
        Self {
            tokens,
            current,
            next,
        }
    }

    pub fn advance(&mut self) -> Option<Token<'a>> {
        self.current = self.next;
        self.next = self.tokens.next();
        self.current
    }

    fn consume(&mut self, token: Token) {
        match self.current {
            Some(t) if t == token => {
                self.advance();
            }
            t => panic!("Expected {:?}, got {:?}", token, t),
        }
    }

    pub fn parse(&mut self) -> Program {
        self.consume(Token::Int); // only int returns for now
        let fn_name = match self.current {
            Some(Token::Id(s)) => s,
            _ => {
                panic!("Bad parse");
            }
        };
        self.advance();
        self.consume(Token::LParen);
        self.consume(Token::Void);
        self.consume(Token::RParen);
        self.consume(Token::LBrace);

        let mut block_items = Vec::new();

        while self.current != Some(Token::RBrace) {
            let item = self.block_item();
            match item {
                BlockItem::S(Statement::Label(_)) => {
                    let next_stmt = self.statement();
                    block_items.push(item);
                    block_items.push(BlockItem::S(next_stmt));
                }
                _ => block_items.push(item),
            };
        }

        self.consume(Token::RBrace);

        let program = Program::Function(fn_name.to_string(), block_items);

        match self.current {
            None => (),
            Some(t) => panic!("Extra junk at end: {:?}", t),
        }

        program
    }

    fn declaration(&mut self) -> Declaration {
        self.consume(Token::Int);
        let name = match self.current {
            Some(Token::Id(id)) => {
                self.advance();
                id.to_string()
            }
            t => panic!("Expected identifier, got {:?}", t),
        };

        let init = match self.current {
            Some(Token::Equals) => {
                self.consume(Token::Equals);
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
        match self.current {
            Some(Token::Int) => BlockItem::D(self.declaration()),
            Some(_) => BlockItem::S(self.statement()),
            None => panic!("Unexpected end of input parsing block item"),
        }
    }

    fn statement(&mut self) -> Statement {
        match self.current {
            Some(Token::If) => {
                self.consume(Token::If);
                self.consume(Token::LParen);
                let condition = self.expression(Prec::Bottom);
                self.consume(Token::RParen);
                let if_stmt = self.statement();
                let else_stmt = match self.current {
                    Some(Token::Else) => {
                        self.consume(Token::Else);
                        let else_stmt = self.statement();
                        Some(Box::new(else_stmt))
                    }
                    _ => None,
                };
                Statement::If(condition, Box::new(if_stmt), else_stmt)
            }
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
            Some(Token::Id(id)) if self.next == Some(Token::Colon) => {
                self.advance();
                self.consume(Token::Colon);
                Statement::Label(id.to_string())
            }
            Some(Token::Goto) => {
                self.consume(Token::Goto);
                match self.current {
                    Some(Token::Id(id)) => {
                        self.advance();
                        self.consume(Token::Semicolon);
                        Statement::Goto(id.to_string())
                    }
                    Some(t) => panic!("Expected identifier after goto, got {:?}", t),
                    None => panic!("Unexpected end of input parsing goto"),
                }
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
        let n_str = match self.current {
            Some(Token::Constant(n_str)) => n_str,
            err => panic!("bad numeric parse: {:?}", err),
        };

        let n = match n_str.parse::<i32>() {
            Ok(n) => n,
            err => panic!("bad numeric parse: {:?}", err),
        };
        self.advance();
        Expression::Constant(n)
    }

    fn get_prec(t: Token) -> Prec {
        match t {
            Token::Constant(_) => Prec::Expr,
            Token::Equals
            | Token::PlusEquals
            | Token::MinusEquals
            | Token::StarEquals
            | Token::SlashEquals
            | Token::PercentEquals
            | Token::AmpersandEquals
            | Token::PipeEquals
            | Token::CaretEquals
            | Token::DoubleLAngleEquals
            | Token::DoubleRAngleEquals => Prec::Assign,
            Token::Huh => Prec::Cond,
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
            Token::DoublePlus | Token::DoubleMinus => Prec::Postfix,
            _ => Prec::Bottom,
        }
    }

    fn expression(&mut self, prec: Prec) -> Expression {
        let mut lhs = self.factor();
        let mut next = self
            .current
            .unwrap_or_else(|| panic!("Ran out of tokens while parsing expression"));

        while (Self::is_binary_op(&next) || Self::is_compound_op(&next))
            && Self::get_prec(next) >= prec
        {
            let next_prec = Self::get_prec(next);
            if next == Token::Equals {
                self.consume(Token::Equals);
                let rhs = self.expression(next_prec);
                lhs = Expression::Assign(Box::new(lhs), Box::new(rhs));
            } else if next == Token::Huh {
                self.consume(Token::Huh);
                let if_expr = self.expression(Prec::Bottom);
                self.consume(Token::Colon);
                let else_expr = self.expression(next_prec);
                lhs =
                    Expression::Conditional(Box::new(lhs), Box::new(if_expr), Box::new(else_expr));
            } else if Self::is_compound_op(&next) {
                let compound_op = self.compound_op();
                let rhs = self.expression(next_prec);
                lhs = Expression::Compound(compound_op, Box::new(lhs), Box::new(rhs));
            } else {
                let binop = self.binary_op();
                let rhs = self.expression(Self::increment_prec(&next_prec));
                lhs = Expression::Binary(binop, Box::new(lhs), Box::new(rhs));
            }
            next = self
                .current
                .unwrap_or_else(|| panic!("Ran out of tokens while parsing expression"));
        }
        while Self::is_postfix_op(&next) {
            match next {
                Token::DoublePlus => {
                    self.consume(Token::DoublePlus);
                    lhs = Expression::Crement(Fixity::Post, Crement::Inc, Box::new(lhs));
                }
                Token::DoubleMinus => {
                    self.consume(Token::DoubleMinus);
                    lhs = Expression::Crement(Fixity::Post, Crement::Dec, Box::new(lhs));
                }
                _ => (),
            }
            next = self
                .current
                .unwrap_or_else(|| panic!("Ran out of tokens while parsing expression"));
        }
        lhs
    }

    fn increment_prec(prec: &Prec) -> Prec {
        match prec {
            Prec::Bottom => Prec::Assign,
            Prec::Assign => Prec::Cond,
            Prec::Cond => Prec::Expr,
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
            Prec::MultDiv => Prec::Unary,
            Prec::Unary => Prec::Postfix,
            _ => Prec::Top,
        }
    }

    fn is_postfix_op(token: &Token) -> bool {
        [Token::DoublePlus, Token::DoubleMinus].contains(token)
    }

    fn is_compound_op(token: &Token) -> bool {
        [
            Token::PlusEquals,
            Token::MinusEquals,
            Token::StarEquals,
            Token::SlashEquals,
            Token::PercentEquals,
            Token::AmpersandEquals,
            Token::PipeEquals,
            Token::CaretEquals,
            Token::DoubleLAngleEquals,
            Token::DoubleRAngleEquals,
        ]
        .contains(token)
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
            Token::Huh,
        ]
        .contains(token)
    }

    fn factor(&mut self) -> Expression {
        match self.current {
            Some(Token::Constant(_)) => self.constant(),
            Some(Token::LParen) => {
                self.consume(Token::LParen);
                let sub_expr = self.expression(Prec::Bottom);
                self.consume(Token::RParen);
                sub_expr
            }
            Some(Token::Tilde | Token::Minus | Token::Bang) => {
                let un_op = self.unary_op();
                let inner_expr = self.expression(Prec::Unary);
                Expression::Unary(un_op, Box::new(inner_expr))
            }
            Some(Token::Id(id)) => {
                let id = id.to_string();
                self.advance();
                Expression::Var(id)
            }
            Some(Token::DoublePlus | Token::DoubleMinus) => {
                let crement = match self.current {
                    Some(Token::DoublePlus) => Crement::Inc,
                    Some(Token::DoubleMinus) => Crement::Dec,
                    _ => unreachable!(),
                };
                self.advance();
                let inner_expr = self.factor();
                Expression::Crement(Fixity::Pre, crement, Box::new(inner_expr))
            }
            t => panic!("Unexpected token {:?}", t),
        }
    }

    fn compound_op(&mut self) -> CompoundOperator {
        let compound = match self.current {
            None => panic!("Ran out of tokens while parsing expression"),
            Some(Token::PlusEquals) => CompoundOperator::Add,
            Some(Token::MinusEquals) => CompoundOperator::Subtract,
            Some(Token::StarEquals) => CompoundOperator::Multiply,
            Some(Token::SlashEquals) => CompoundOperator::Divide,
            Some(Token::PercentEquals) => CompoundOperator::Remainder,
            Some(Token::AmpersandEquals) => CompoundOperator::BitAnd,
            Some(Token::PipeEquals) => CompoundOperator::BitOr,
            Some(Token::CaretEquals) => CompoundOperator::BitXOr,
            Some(Token::DoubleLAngleEquals) => CompoundOperator::ShiftLeft,
            Some(Token::DoubleRAngleEquals) => CompoundOperator::ShiftRight,
            Some(t) => panic!("Expected compound operator, got {:?}", t),
        };
        self.advance();
        compound
    }

    fn binary_op(&mut self) -> BinaryOperator {
        let binop = match self.current {
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
            Some(Token::Huh) => BinaryOperator::Conditional,
            Some(t) => panic!("Expected binary operator, got {:?}", t),
        };
        self.advance();
        binop
    }

    fn unary_op(&mut self) -> UnaryOperator {
        let unop = match self.current {
            Some(Token::Tilde) => UnaryOperator::Complement,
            Some(Token::Minus) => UnaryOperator::Negate,
            Some(Token::Bang) => UnaryOperator::Not,
            _ => panic!("unreachable"),
        };
        self.advance();
        unop
    }
}
