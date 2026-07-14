use std::collections::HashSet;

use crate::CompileError;
use crate::ast::*;
use crate::interner::{Interner, Symbol};
use crate::lexer::{Lexer, Token, TokenKind};

#[derive(Debug, PartialEq, Clone)]
enum Declarator {
    Ident(Symbol),
    Pointer(Box<Declarator>),
    Array(Box<Declarator>, u32),
    Fun(Vec<Param>, Box<Declarator>),
}

#[derive(Debug, PartialEq, Clone)]
enum AbstractDeclarator {
    Pointer(Box<AbstractDeclarator>),
    Array(Box<AbstractDeclarator>, u32),
    Base,
}

#[derive(Debug, PartialEq, Clone)]
struct Param(Type, Declarator);

pub struct Parser<'a> {
    tokens: Lexer<'a>,
    last_token: Option<Token<'a>>,
    current_token: Option<Token<'a>>,
    next_token: Option<Token<'a>>,
    interner: &'a mut Interner,
}

const UNLABELLED: &str = "unlabelled";

type ParseResult<T> = Result<T, CompileError>;

impl<'a> Parser<'a> {
    pub fn new(mut tokens: Lexer<'a>, interner: &'a mut Interner) -> ParseResult<Self> {
        let current_token = Self::next_or_fail(&mut tokens)?;
        let next_token = Self::next_or_fail(&mut tokens)?;
        interner.intern(UNLABELLED.into());
        Ok(Self {
            last_token: None,
            tokens,
            current_token,
            next_token,
            interner,
        })
    }

    pub fn advance(&mut self) -> ParseResult<Token<'a>> {
        self.last_token = self.current_token;
        self.current_token = self.next_token;
        self.next_token = Self::next_or_fail(&mut self.tokens)?;
        Ok(self.current_token.unwrap_or(Self::eof_token()))
    }

    fn next_or_fail(tokens: &mut Lexer<'a>) -> ParseResult<Option<Token<'a>>> {
        match tokens.next() {
            Some(Err(err)) => Err(err),
            Some(Ok(t)) => Ok(Some(t)),
            None => Ok(None),
        }
    }

    pub fn current(&self) -> Token<'a> {
        match self.current_token {
            None => Self::eof_token(),
            Some(t) => t,
        }
    }

    pub fn next(&self) -> Token<'a> {
        match self.next_token {
            None => Self::eof_token(),
            Some(t) => t,
        }
    }

    fn eof_token() -> Token<'a> {
        Token {
            kind: TokenKind::Eof,
            start: 0,
            end: 0,
            line: 0,
        }
    }

    fn consume(&mut self, kind: TokenKind<'a>) -> ParseResult<()> {
        match self.current() {
            t if t.kind == kind => {
                self.advance()?;
                Ok(())
            }
            t => Err(self.report_error(format!("Expected {:?}, got {:?}", kind, t.kind))),
        }
    }

    pub fn parse(&mut self) -> ParseResult<Program<Expression>> {
        let mut decls = vec![];
        while self.current().kind != TokenKind::Eof {
            decls.push(self.declaration()?)
        }

        Ok(Program(decls))
    }

    fn block(&mut self) -> ParseResult<Vec<BlockItem<Expression>>> {
        self.consume(TokenKind::LBrace)?;

        let mut block_items = Vec::new();

        while self.current().kind != TokenKind::RBrace {
            let item = self.block_item()?;
            block_items.push(item);
        }

        self.consume(TokenKind::RBrace)?;
        Ok(block_items)
    }

    fn declaration(&mut self) -> ParseResult<Declaration<Expression>> {
        let mut storage_and_type = vec![];
        while Self::is_specifier(self.current()) {
            storage_and_type.push(self.current());
            self.advance()?;
        }

        let (ty, storage) = self.type_and_storage_class(storage_and_type)?;
        let declarator = self.declarator()?;
        let (name, decl_type, params) = self.process_declarator(declarator, ty)?;
        Ok(if matches!(decl_type, Type::Fun(_, _)) {
            Declaration::Func(self.func_declaration(decl_type, name, params, storage)?)
        } else {
            Declaration::Var(self.var_declaration(decl_type, name, storage)?)
        })
    }

    fn func_declaration(
        &mut self,
        ty: Type,
        name: Symbol,
        params: Vec<Symbol>,
        storage: Option<StorageClass>,
    ) -> ParseResult<Function<Expression>> {
        let body = if self.current().kind == TokenKind::LBrace {
            Some(self.block()?)
        } else {
            self.consume(TokenKind::Semicolon)?;
            None
        };

        Ok(Function {
            name,
            body,
            params,
            storage,
            ty,
        })
    }

    fn var_declaration(
        &mut self,
        ty: Type,
        name: Symbol,
        storage: Option<StorageClass>,
    ) -> ParseResult<Var<Expression>> {
        let init = match self.current().kind {
            TokenKind::Equals => {
                self.consume(TokenKind::Equals)?;
                Some(self.initializer()?)
            }
            TokenKind::Semicolon => None,
            kind => {
                return Err(self.report_error(format!("Expected assignment or ;, got {:?}", kind)));
            }
        };

        self.consume(TokenKind::Semicolon)?;
        Ok(Var {
            name,
            init,
            storage,
            ty,
        })
    }

    fn initializer(&mut self) -> ParseResult<Initializer<Expression>> {
        match self.current().kind {
            TokenKind::LBrace => {
                self.consume(TokenKind::LBrace)?;

                let mut inits = vec![];

                inits.push(self.initializer()?);

                while self.current().kind == TokenKind::Comma {
                    self.consume(TokenKind::Comma)?;
                    if self.current().kind == TokenKind::RBrace {
                        break;
                    }
                    inits.push(self.initializer()?);
                }
                // // Consume trailing comma if it's there
                // if self.current().kind == TokenKind::Comma {
                //     let _ = self.consume(TokenKind::Comma);
                // }

                self.consume(TokenKind::RBrace)?;

                Ok(Initializer::Compound(inits))
            }
            _ => Ok(Initializer::Single(self.expression(Prec::Bottom)?)),
        }
    }

    // Either an identifier or a parenthesized declarator
    fn simple_declarator(&mut self) -> ParseResult<Declarator> {
        match self.current().kind {
            TokenKind::Id(id) => {
                self.advance()?;
                Ok(Declarator::Ident(self.interner.intern(id.into())))
            }
            TokenKind::LParen => {
                self.consume(TokenKind::LParen)?;
                let declarator = self.declarator()?;
                self.consume(TokenKind::RParen)?;
                Ok(declarator)
            }
            kind => Err(self.report_error(format!("Expected id or parenthesis, found {kind:?}"))),
        }
    }

    // A simple declarator, or a function declarator
    fn direct_declarator(&mut self) -> ParseResult<Declarator> {
        let simple = self.simple_declarator()?;
        if matches!(self.current().kind, TokenKind::LParen) {
            self.consume(TokenKind::LParen)?;
            if self.current().kind == TokenKind::Void {
                self.consume(TokenKind::Void)?;
                self.consume(TokenKind::RParen)?;
                return Ok(Declarator::Fun(vec![], Box::new(simple)));
            }
            let mut params = vec![];
            while {
                let ty = self.type_specifier()?;
                let declarator = self.declarator()?;

                params.push(Param(ty, declarator));

                let comma = self.current().kind == TokenKind::Comma;
                if comma {
                    self.consume(TokenKind::Comma)?;
                }
                comma
            } {} // This is a sneaky hack for a do-while loop
            self.consume(TokenKind::RParen)?;

            return Ok(Declarator::Fun(params, Box::new(simple)));
        } else if matches!(self.current().kind, TokenKind::LSquare) {
            let mut declarator = simple;
            while matches!(self.current().kind, TokenKind::LSquare) {
                self.consume(TokenKind::LSquare)?;

                let size = self.array_size()?;

                self.consume(TokenKind::RSquare)?;
                declarator = Declarator::Array(Box::new(declarator), size);
            }
            return Ok(declarator);
        }
        Ok(simple)
    }

    fn array_size(&mut self) -> ParseResult<u32> {
        let c = self.constant()?;
        let size = match c {
            Expression::Constant(Const::Double(n)) => {
                return Err(
                    self.report_error(format!("Can't use double constant {n} as array dimension"))
                );
            }
            Expression::Constant(Const::Int(n)) => n as u32,
            Expression::Constant(Const::UInt(n)) => n,
            Expression::Constant(Const::Long(n)) => n as u32,
            Expression::Constant(Const::ULong(n)) => n as u32,
            expr => return Err(self.report_error(format!("Bad array size {expr:?}"))),
        };
        Ok(size)
    }

    fn declarator(&mut self) -> ParseResult<Declarator> {
        match self.current().kind {
            TokenKind::Star => {
                self.consume(TokenKind::Star)?;
                let declarator = self.declarator()?;
                Ok(Declarator::Pointer(Box::new(declarator)))
            }
            _ => self.direct_declarator(),
        }
    }

    fn abstract_declarator(&mut self) -> ParseResult<AbstractDeclarator> {
        match self.current().kind {
            TokenKind::Star => {
                self.consume(TokenKind::Star)?;

                let declarator = if self.current().kind == TokenKind::Star {
                    self.abstract_declarator()?
                } else {
                    self.direct_abstract_declarator()?
                };
                Ok(AbstractDeclarator::Pointer(Box::new(declarator)))
            }
            _ => self.direct_abstract_declarator(),
        }
    }

    fn direct_abstract_declarator(&mut self) -> ParseResult<AbstractDeclarator> {
        match self.current().kind {
            TokenKind::LParen => {
                self.consume(TokenKind::LParen)?;

                let mut declarator = self.abstract_declarator()?;
                self.consume(TokenKind::RParen)?;
                while self.current().kind == TokenKind::LSquare {
                    self.consume(TokenKind::LSquare)?;

                    let size = self.array_size()?;

                    self.consume(TokenKind::RSquare)?;
                    declarator = AbstractDeclarator::Array(Box::new(declarator), size);
                }
                Ok(declarator)
            }
            TokenKind::LSquare => {
                self.consume(TokenKind::LSquare)?;

                let size = self.array_size()?;

                self.consume(TokenKind::RSquare)?;

                let mut declarator =
                    AbstractDeclarator::Array(Box::new(AbstractDeclarator::Base), size);
                while self.current().kind == TokenKind::LSquare {
                    self.consume(TokenKind::LSquare)?;

                    let size = self.array_size()?;

                    self.consume(TokenKind::RSquare)?;
                    declarator = AbstractDeclarator::Array(Box::new(declarator), size);
                }
                Ok(declarator)
            }
            _ => Ok(AbstractDeclarator::Base),
        }
    }

    fn type_and_storage_class(
        &self,
        specifiers: Vec<Token<'a>>,
    ) -> ParseResult<(Type, Option<StorageClass>)> {
        let mut storage_classes = vec![];
        let mut types = vec![];
        for specifier in specifiers {
            match specifier.kind {
                kind @ (TokenKind::Int
                | TokenKind::Long
                | TokenKind::Unsigned
                | TokenKind::Signed
                | TokenKind::Double) => types.push(kind),
                TokenKind::Static | TokenKind::Extern => storage_classes.push(specifier.kind),
                _ => {
                    return Err(
                        self.report_error(format!("Bad declaration specifier {:?}", specifier))
                    );
                }
            }
        }

        let ty = self.consolidate_type_specifier(types)?;
        let storage_class = match &storage_classes[..] {
            [] => None,
            [TokenKind::Extern] => Some(StorageClass::Extern),
            [TokenKind::Static] => Some(StorageClass::Static),
            l => return Err(self.report_error(format!("Too many storage classes {:?}", l))),
        };

        Ok((ty, storage_class))
    }

    fn consolidate_type_specifier(&self, types: Vec<TokenKind<'a>>) -> ParseResult<Type> {
        if types.is_empty() {
            return Err(self.report_error(String::from("Empty type specifier list")));
        }

        if types.contains(&TokenKind::Double) {
            if types.len() > 1 {
                return Err(self.report_error(format!(
                    "double combined with other type specs: {:?}",
                    types
                )));
            }
            return Ok(Type::Double);
        }

        if types.contains(&TokenKind::Signed) && types.contains(&TokenKind::Unsigned) {
            return Err(self.report_error(String::from(
                "Type specifier list contains both signed and unsigned",
            )));
        }

        let mut seen = HashSet::new();
        for type_spec in &types {
            if seen.contains(type_spec) {
                return Err(self.report_error(format!(
                    "Type specifier list contains specificer {type_spec:?} twice"
                )));
            }
            seen.insert(*type_spec);
        }

        if types.contains(&TokenKind::Long) && types.contains(&TokenKind::Unsigned) {
            return Ok(Type::ULong);
        }

        if types.contains(&TokenKind::Unsigned) {
            return Ok(Type::UInt);
        }

        if types.contains(&TokenKind::Long) {
            return Ok(Type::Long);
        }

        Ok(Type::Int)
    }

    fn type_specifier(&mut self) -> ParseResult<Type> {
        let mut types = vec![];
        while Self::is_type_specifier(self.current()) {
            match self.current().kind {
                kind @ (TokenKind::Int
                | TokenKind::Long
                | TokenKind::Unsigned
                | TokenKind::Signed
                | TokenKind::Double) => types.push(kind),
                _ => unreachable!(),
            }
            self.advance()?;
        }
        self.consolidate_type_specifier(types)
    }

    fn process_declarator(
        &self,
        declarator: Declarator,
        base_type: Type,
    ) -> ParseResult<(Symbol, Type, Vec<Symbol>)> {
        match declarator {
            Declarator::Ident(name) => Ok((name, base_type, vec![])),
            Declarator::Pointer(d) => {
                let derived_type = Type::Pointer(Box::new(base_type));
                self.process_declarator(*d, derived_type)
            }
            Declarator::Array(d, size) => {
                let derived_type = Type::Array(Box::new(base_type), size);
                self.process_declarator(*d, derived_type)
            }
            Declarator::Fun(params, d) => {
                if let Declarator::Ident(name) = *d {
                    let mut param_names = Vec::with_capacity(params.len());
                    let mut param_types = Vec::with_capacity(params.len());

                    for Param(p_base_ty, p_declarator) in params {
                        let (param_name, param_ty, _) =
                            self.process_declarator(p_declarator, p_base_ty)?;
                        if matches!(param_ty, Type::Fun(_, _)) {
                            return Err(self.report_error(
                                "Can't use a function pointer in parameters".to_string(),
                            ));
                        }
                        param_names.push(param_name);
                        param_types.push(param_ty);
                    }

                    let derived_type = Type::Fun(param_types, Box::new(base_type));
                    Ok((name, derived_type, param_names))
                } else {
                    Err(self.report_error(format!(
                        "Can't apply additional type derivation {params:?} {d:?} to function type"
                    )))
                }
            }
        }
    }

    fn process_abstract_declarator(declarator: AbstractDeclarator, base_type: Type) -> Type {
        match declarator {
            AbstractDeclarator::Base => base_type,
            AbstractDeclarator::Pointer(declarator) => {
                Self::process_abstract_declarator(*declarator, Type::Pointer(Box::new(base_type)))
            }
            AbstractDeclarator::Array(declarator, size) => Self::process_abstract_declarator(
                *declarator,
                Type::Array(Box::new(base_type), size),
            ),
        }
    }

    fn block_item(&mut self) -> ParseResult<BlockItem<Expression>> {
        Ok(match self.current() {
            t if Self::is_specifier(t) => BlockItem::D(self.declaration()?),
            Token {
                kind: TokenKind::Eof,
                ..
            } => {
                return Err(
                    self.report_error("Unexpected end of input parsing block item".to_string())
                );
            }
            _ => BlockItem::S(self.statement()?),
        })
    }

    fn statement(&mut self) -> ParseResult<Statement<Expression>> {
        Ok(match self.current().kind {
            TokenKind::If => {
                self.consume(TokenKind::If)?;
                self.consume(TokenKind::LParen)?;
                let condition = self.expression(Prec::Bottom)?;
                self.consume(TokenKind::RParen)?;
                let if_stmt = self.statement()?;
                let else_stmt = match self.current().kind {
                    TokenKind::Else => {
                        self.consume(TokenKind::Else)?;
                        let else_stmt = self.statement()?;
                        Some(Box::new(else_stmt))
                    }
                    _ => None,
                };
                Statement::If(condition, Box::new(if_stmt), else_stmt)
            }
            TokenKind::Return => {
                self.consume(TokenKind::Return)?;
                let expr = self.expression(Prec::Bottom)?;

                self.consume(TokenKind::Semicolon)?;

                Statement::Return(expr)
            }
            TokenKind::Semicolon => {
                self.consume(TokenKind::Semicolon)?;
                Statement::Null
            }
            TokenKind::Id(id) if self.next().kind == TokenKind::Colon => {
                self.advance()?;
                self.consume(TokenKind::Colon)?;
                let stmt = self.statement()?;
                let id = self.interner.intern(id.into());
                Statement::Label(id, Box::new(stmt))
            }
            TokenKind::Goto => {
                self.consume(TokenKind::Goto)?;
                match self.current().kind {
                    TokenKind::Id(id) => {
                        self.advance()?;
                        self.consume(TokenKind::Semicolon)?;
                        let id = self.interner.intern(id.into());
                        Statement::Goto(id)
                    }
                    kind => {
                        return Err(self.report_error(format!(
                            "Expected identifier after goto, got {:?}",
                            kind
                        )));
                    }
                }
            }
            TokenKind::LBrace => Statement::Compound(self.block()?),
            TokenKind::Break => {
                self.advance()?;
                let stmt = Statement::Break(self.interner.get_str(UNLABELLED));
                self.consume(TokenKind::Semicolon)?;
                stmt
            }
            TokenKind::Continue => {
                self.advance()?;
                let stmt = Statement::Continue(self.interner.get_str(UNLABELLED));
                self.consume(TokenKind::Semicolon)?;
                stmt
            }
            TokenKind::While => {
                self.consume(TokenKind::While)?;
                self.consume(TokenKind::LParen)?;
                let cond = self.expression(Prec::Bottom)?;
                self.consume(TokenKind::RParen)?;
                let body = self.statement()?;
                Statement::While(self.interner.get_str(UNLABELLED), cond, Box::new(body))
            }
            TokenKind::Do => {
                self.consume(TokenKind::Do)?;
                let body = self.statement()?;
                self.consume(TokenKind::While)?;
                self.consume(TokenKind::LParen)?;
                let cond = self.expression(Prec::Bottom)?;
                self.consume(TokenKind::RParen)?;
                self.consume(TokenKind::Semicolon)?;
                Statement::DoWhile(self.interner.get_str(UNLABELLED), Box::new(body), cond)
            }
            TokenKind::For => {
                self.consume(TokenKind::For)?;
                self.consume(TokenKind::LParen)?;
                let init = match self.current() {
                    t if Self::is_specifier(t) => match self.declaration()? {
                        Declaration::Func(_) => {
                            return Err(self.report_error(
                                "Function declaration in for loop init".to_string(),
                            ));
                        }
                        Declaration::Var(var) => ForInit::Decl(var),
                    },
                    Token {
                        kind: TokenKind::Semicolon,
                        ..
                    } => {
                        self.consume(TokenKind::Semicolon)?;
                        ForInit::Null
                    }
                    _ => {
                        let expr = ForInit::Exp(self.expression(Prec::Bottom)?);
                        self.consume(TokenKind::Semicolon)?;
                        expr
                    }
                };
                let cond = if self.current().kind != TokenKind::Semicolon {
                    let expr = Some(self.expression(Prec::Bottom)?);
                    self.consume(TokenKind::Semicolon)?;
                    expr
                } else {
                    self.consume(TokenKind::Semicolon)?;
                    None
                };
                let post = if self.current().kind != TokenKind::RParen {
                    let expr = Some(self.expression(Prec::Bottom)?);
                    self.consume(TokenKind::RParen)?;
                    expr
                } else {
                    self.consume(TokenKind::RParen)?;
                    None
                };
                let body = self.statement()?;

                Statement::For(
                    self.interner.get_str(UNLABELLED),
                    init,
                    cond,
                    post,
                    Box::new(body),
                )
            }
            TokenKind::Switch => {
                self.consume(TokenKind::Switch)?;
                self.consume(TokenKind::LParen)?;
                let expr = self.expression(Prec::Bottom)?;
                self.consume(TokenKind::RParen)?;
                let body = Box::new(self.statement()?);
                Statement::Switch {
                    label: self.interner.get_str(UNLABELLED),
                    expr,
                    body,
                    cases: vec![],
                }
            }
            TokenKind::Case => {
                self.consume(TokenKind::Case)?;
                let expr = self.expression(Prec::Bottom)?;
                self.consume(TokenKind::Colon)?;
                let stmt = self.statement()?;
                Statement::Case(self.interner.get_str(UNLABELLED), expr, Box::new(stmt))
            }
            TokenKind::Default => {
                self.consume(TokenKind::Default)?;
                self.consume(TokenKind::Colon)?;
                let stmt = self.statement()?;
                Statement::Default(self.interner.get_str(UNLABELLED), Box::new(stmt))
            }
            TokenKind::Eof => {
                return Err(
                    self.report_error("Unexpected end of input parsing statement".to_string())
                );
            }
            _ => {
                let expr = Statement::Exp(self.expression(Prec::Bottom)?);
                self.consume(TokenKind::Semicolon)?;
                expr
            }
        })
    }

    fn constant(&mut self) -> ParseResult<Expression> {
        let c = match self.current().kind {
            TokenKind::Constant(n) => match n.parse::<i32>() {
                Ok(n) => Const::Int(n),
                Err(_) => Const::Long(n.parse::<i64>().map_err(|_| {
                    CompileError::Parse(format!(
                        "Promoted signed {n} from 32 to 64 bits and it still doesn't work!",
                    ))
                })?),
            },
            TokenKind::UnsignedConstant(n) => match n.parse::<u32>() {
                Ok(n) => Const::UInt(n),
                Err(_) => Const::ULong(n.parse::<u64>().map_err(|_| {
                    CompileError::Parse(format!(
                        "Promoted unsigned {n} from 32 to 64 bits and it still doesn't work!",
                    ))
                })?),
            },
            TokenKind::LongConstant(n) => Const::Long(
                n.parse::<i64>()
                    .map_err(|_| CompileError::Parse(format!("Error parsing 64-bit int {n}")))?,
            ),
            TokenKind::UnsignedLongConstant(n) => Const::ULong(n.parse::<u64>().map_err(|_| {
                CompileError::Parse(format!("Error parsing 64-bit unsigned int {n}"))
            })?),
            TokenKind::DoubleConstant(n) => Const::Double(
                n.parse::<f64>()
                    .map_err(|_| CompileError::Parse(format!("Error parsing double {n}")))?,
            ),
            err => return Err(self.report_error(format!("bad numeric parse: {:?}", err))),
        };
        self.advance()?;
        Ok(Expression::Constant(c))
    }

    fn get_prec(t: Token) -> Prec {
        match t.kind {
            TokenKind::Constant(_) => Prec::Expr,
            TokenKind::Equals
            | TokenKind::PlusEquals
            | TokenKind::MinusEquals
            | TokenKind::StarEquals
            | TokenKind::SlashEquals
            | TokenKind::PercentEquals
            | TokenKind::AmpersandEquals
            | TokenKind::PipeEquals
            | TokenKind::CaretEquals
            | TokenKind::DoubleLAngleEquals
            | TokenKind::DoubleRAngleEquals => Prec::Assign,
            TokenKind::Huh => Prec::Cond,
            TokenKind::Plus | TokenKind::Minus => Prec::AddSub,
            TokenKind::Percent | TokenKind::Star | TokenKind::Slash => Prec::MultDiv,
            TokenKind::Pipe => Prec::BitOr,
            TokenKind::Ampersand => Prec::BitAnd,
            TokenKind::Caret => Prec::BitXOr,
            TokenKind::DoubleLAngle | TokenKind::DoubleRAngle => Prec::Shift,
            TokenKind::DoubleEquals | TokenKind::BangEquals => Prec::Equals,
            TokenKind::LAngleEquals
            | TokenKind::LAngle
            | TokenKind::RAngleEquals
            | TokenKind::RAngle => Prec::Comparison,
            TokenKind::DoubleAmpersand => Prec::And,
            TokenKind::DoublePipe => Prec::Or,
            TokenKind::DoublePlus | TokenKind::DoubleMinus => Prec::Postfix,
            _ => Prec::Bottom,
        }
    }

    fn expression(&mut self, prec: Prec) -> ParseResult<Expression> {
        let mut lhs = self.factor()?;
        let mut next = self.current();

        while (Self::is_binary_op(&next)
            || Self::is_compound_op(&next)
            || Self::is_postfix_op(&next))
            && Self::get_prec(next) >= prec
        {
            let next_prec = Self::get_prec(next);
            if next.kind == TokenKind::Equals {
                self.consume(TokenKind::Equals)?;
                let rhs = self.expression(next_prec)?;
                lhs = Expression::Assign(Box::new(lhs), Box::new(rhs));
            } else if next.kind == TokenKind::Huh {
                self.consume(TokenKind::Huh)?;
                let if_expr = self.expression(Prec::Bottom)?;
                self.consume(TokenKind::Colon)?;
                let else_expr = self.expression(next_prec)?;
                lhs =
                    Expression::Conditional(Box::new(lhs), Box::new(if_expr), Box::new(else_expr));
            } else if Self::is_compound_op(&next) {
                let compound_op = self.compound_op()?;
                let rhs = self.expression(next_prec)?;
                lhs = Expression::Compound(compound_op, Box::new(lhs), Box::new(rhs));
            } else if Self::is_postfix_op(&next) {
                match next.kind {
                    TokenKind::DoublePlus => {
                        self.consume(TokenKind::DoublePlus)?;
                        lhs = Expression::Crement(Fixity::Post, Crement::Inc, Box::new(lhs));
                    }
                    TokenKind::DoubleMinus => {
                        self.consume(TokenKind::DoubleMinus)?;
                        lhs = Expression::Crement(Fixity::Post, Crement::Dec, Box::new(lhs));
                    }
                    TokenKind::LSquare => {
                        self.consume(TokenKind::LSquare)?;
                        let subscript = self.expression(Prec::Bottom)?;
                        self.consume(TokenKind::RSquare)?;
                        lhs = Expression::Subscript(Box::new(lhs), Box::new(subscript))
                    }
                    _ => (),
                }
            } else {
                let binop = self.binary_op()?;
                let rhs = self.expression(Self::increment_prec(&next_prec))?;
                lhs = Expression::Binary(binop, Box::new(lhs), Box::new(rhs));
            }
            next = self.current();
        }
        Ok(lhs)
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
        matches!(
            &token.kind,
            TokenKind::DoublePlus | TokenKind::DoubleMinus | TokenKind::LSquare
        )
    }

    fn is_compound_op(token: &Token) -> bool {
        matches!(
            &token.kind,
            TokenKind::PlusEquals
                | TokenKind::MinusEquals
                | TokenKind::StarEquals
                | TokenKind::SlashEquals
                | TokenKind::PercentEquals
                | TokenKind::AmpersandEquals
                | TokenKind::PipeEquals
                | TokenKind::CaretEquals
                | TokenKind::DoubleLAngleEquals
                | TokenKind::DoubleRAngleEquals
        )
    }

    fn is_binary_op(token: &Token) -> bool {
        matches!(
            &token.kind,
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::Percent
                | TokenKind::Ampersand
                | TokenKind::Pipe
                | TokenKind::Caret
                | TokenKind::DoubleLAngle
                | TokenKind::DoubleRAngle
                | TokenKind::BangEquals
                | TokenKind::DoubleEquals
                | TokenKind::DoubleAmpersand
                | TokenKind::DoublePipe
                | TokenKind::RAngle
                | TokenKind::RAngleEquals
                | TokenKind::LAngle
                | TokenKind::LAngleEquals
                | TokenKind::Equals
                | TokenKind::Huh
        )
    }

    fn factor(&mut self) -> ParseResult<Expression> {
        Ok(match self.current().kind {
            TokenKind::UnsignedConstant(_)
            | TokenKind::UnsignedLongConstant(_)
            | TokenKind::LongConstant(_)
            | TokenKind::Constant(_)
            | TokenKind::DoubleConstant(_) => self.constant()?,
            TokenKind::LParen => {
                self.consume(TokenKind::LParen)?;
                if Self::is_type_specifier(self.current()) {
                    let base_ty = self.type_specifier()?;
                    let abstract_declarator = self.abstract_declarator()?;
                    println!("{abstract_declarator:?}");
                    self.consume(TokenKind::RParen)?;
                    let ty = Self::process_abstract_declarator(abstract_declarator, base_ty);
                    println!("{ty:?}");
                    let expr = self.expression(Prec::Postfix)?;
                    Expression::Cast(ty, Box::new(expr))
                } else {
                    let sub_expr = self.expression(Prec::Bottom)?;
                    self.consume(TokenKind::RParen)?;
                    sub_expr
                }
            }
            TokenKind::Tilde | TokenKind::Minus | TokenKind::Bang => {
                let un_op = self.unary_op()?;
                let inner_expr = self.expression(Prec::Unary)?;
                Expression::Unary(un_op, Box::new(inner_expr))
            }
            TokenKind::Star => {
                self.consume(TokenKind::Star)?;
                let inner_expr = self.expression(Prec::Unary)?;
                Expression::Deref(Box::new(inner_expr))
            }
            TokenKind::Ampersand => {
                self.consume(TokenKind::Ampersand)?;
                let inner_expr = self.expression(Prec::Unary)?;
                Expression::AddrOf(Box::new(inner_expr))
            }
            TokenKind::Id(id) => {
                self.advance()?;
                let id = self.interner.intern(id.into());
                if self.current().kind == TokenKind::LParen {
                    self.consume(TokenKind::LParen)?;
                    let mut params = vec![];
                    if self.current().kind == TokenKind::RParen {
                        self.consume(TokenKind::RParen)?;
                    } else {
                        while {
                            let expr = self.expression(Prec::Bottom)?;
                            params.push(expr);
                            let comma = self.current().kind == TokenKind::Comma;
                            if comma {
                                self.consume(TokenKind::Comma)?;
                            }
                            comma
                        } {}
                        self.consume(TokenKind::RParen)?;
                    }
                    Expression::Call(id, params)
                } else {
                    Expression::Var(id)
                }
            }
            TokenKind::DoublePlus | TokenKind::DoubleMinus => {
                let crement = match self.current().kind {
                    TokenKind::DoublePlus => Crement::Inc,
                    TokenKind::DoubleMinus => Crement::Dec,
                    _ => unreachable!(),
                };
                self.advance()?;
                let inner_expr = self.factor()?;
                Expression::Crement(Fixity::Pre, crement, Box::new(inner_expr))
            }
            t => return Err(self.report_error(format!("Unexpected token {:?}", t))),
        })
    }

    fn compound_op(&mut self) -> ParseResult<BinaryOperator> {
        let compound = match self.current().kind {
            TokenKind::Eof => {
                return Err(
                    self.report_error("Ran out of tokens while parsing expression".to_string())
                );
            }
            TokenKind::PlusEquals => BinaryOperator::Add,
            TokenKind::MinusEquals => BinaryOperator::Subtract,
            TokenKind::StarEquals => BinaryOperator::Multiply,
            TokenKind::SlashEquals => BinaryOperator::Divide,
            TokenKind::PercentEquals => BinaryOperator::Remainder,
            TokenKind::AmpersandEquals => BinaryOperator::BitAnd,
            TokenKind::PipeEquals => BinaryOperator::BitOr,
            TokenKind::CaretEquals => BinaryOperator::BitXOr,
            TokenKind::DoubleLAngleEquals => BinaryOperator::ShiftLeft,
            TokenKind::DoubleRAngleEquals => BinaryOperator::ShiftRight,
            kind => {
                return Err(
                    self.report_error(format!("Expected compound operator, got {:?}", kind))
                );
            }
        };
        self.advance()?;
        Ok(compound)
    }

    fn binary_op(&mut self) -> ParseResult<BinaryOperator> {
        let binop = match self.current().kind {
            TokenKind::Eof => {
                return Err(
                    self.report_error("Ran out of tokens while parsing expression".to_string())
                );
            }
            TokenKind::Plus => BinaryOperator::Add,
            TokenKind::Minus => BinaryOperator::Subtract,
            TokenKind::Star => BinaryOperator::Multiply,
            TokenKind::Slash => BinaryOperator::Divide,
            TokenKind::Percent => BinaryOperator::Remainder,
            TokenKind::Ampersand => BinaryOperator::BitAnd,
            TokenKind::Pipe => BinaryOperator::BitOr,
            TokenKind::Caret => BinaryOperator::BitXOr,
            TokenKind::DoubleLAngle => BinaryOperator::ShiftLeft,
            TokenKind::DoubleRAngle => BinaryOperator::ShiftRight,
            TokenKind::DoubleAmpersand => BinaryOperator::And,
            TokenKind::DoublePipe => BinaryOperator::Or,
            TokenKind::DoubleEquals => BinaryOperator::Equal,
            TokenKind::BangEquals => BinaryOperator::NotEqual,
            TokenKind::RAngle => BinaryOperator::Greater,
            TokenKind::RAngleEquals => BinaryOperator::GreaterOrEqual,
            TokenKind::LAngle => BinaryOperator::Less,
            TokenKind::LAngleEquals => BinaryOperator::LessOrEqual,
            TokenKind::Huh => BinaryOperator::Conditional,
            kind => {
                return Err(self.report_error(format!("Expected binary operator, got {:?}", kind)));
            }
        };
        self.advance()?;
        Ok(binop)
    }

    fn unary_op(&mut self) -> ParseResult<UnaryOperator> {
        let unop = match self.current().kind {
            TokenKind::Tilde => UnaryOperator::Complement,
            TokenKind::Minus => UnaryOperator::Negate,
            TokenKind::Bang => UnaryOperator::Not,
            _ => unreachable!(),
        };
        self.advance()?;
        Ok(unop)
    }

    fn is_specifier(t: Token<'a>) -> bool {
        matches!(
            t.kind,
            TokenKind::Long
                | TokenKind::Int
                | TokenKind::Signed
                | TokenKind::Unsigned
                | TokenKind::Extern
                | TokenKind::Static
                | TokenKind::Double
        )
    }

    fn is_type_specifier(t: Token<'a>) -> bool {
        matches!(
            t.kind,
            TokenKind::Long
                | TokenKind::Int
                | TokenKind::Signed
                | TokenKind::Unsigned
                | TokenKind::Double
        )
    }

    fn report_error(&self, message: String) -> CompileError {
        let last_token = self.last_token.unwrap();
        let line = last_token.line;
        let first = format!("Error [line {}]: {}", line, message);
        let second = "Error seems to be around here...".to_string();
        let lines = self.print_enclosing_lines(last_token.start, self.current_token.unwrap().end);
        CompileError::Parse(format!(
            "Aborting due to error:\n{first}\n{second}\n{lines}"
        ))
    }

    fn print_enclosing_lines(&self, error_start: usize, error_end: usize) -> String {
        let mut line_start = error_start;
        while self.tokens.source.get(line_start..line_start + 1) != Some("\n") {
            line_start -= 1;
        }
        line_start += 1;

        let start_offset = error_start.saturating_sub(line_start);

        let mut line_end = error_end;
        while self.tokens.source.get(line_end..line_end + 1) != Some("\n") {
            line_end += 1;
        }
        let end_offset = line_end.saturating_sub(error_end);

        let mut error_lines = self
            .tokens
            .source
            .get(line_start..line_end)
            .unwrap()
            .lines();

        let first_line = error_lines.next().unwrap();
        let first = first_line.to_string();
        let second = format!(
            "{}{}",
            str::repeat(" ", start_offset),
            str::repeat("^", first_line.len() - start_offset)
        );
        let third = if let Some(next_line) = error_lines.next() {
            format!(
                "{next_line}\n{}{}",
                str::repeat("^", next_line.len() - end_offset),
                str::repeat(" ", end_offset)
            )
        } else {
            String::from("")
        };
        format!("{first}\n{second}\n{third}\n")
    }
}
