use std::iter::Peekable;
use std::str::CharIndices;

use crate::CompileError;

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub enum TokenKind<'a> {
    Eof,
    Id(&'a str),
    Void,
    Int,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Return,
    Constant(&'a str),
    LongConstant(&'a str),
    UnsignedConstant(&'a str),
    UnsignedLongConstant(&'a str),
    Signed,
    Unsigned,
    Semicolon,
    Tilde,
    Plus,
    DoublePlus,
    Minus,
    DoubleMinus,
    Star,
    Slash,
    Percent,
    Ampersand,
    DoubleAmpersand,
    Pipe,
    DoublePipe,
    RAngle,
    LAngle,
    DoubleRAngle,
    DoubleLAngle,
    Caret,
    Bang,
    BangEquals,
    Equals,
    DoubleEquals,
    RAngleEquals,
    LAngleEquals,
    PlusEquals,
    MinusEquals,
    StarEquals,
    SlashEquals,
    PercentEquals,
    AmpersandEquals,
    PipeEquals,
    CaretEquals,
    DoubleLAngleEquals,
    DoubleRAngleEquals,
    If,
    Else,
    Huh,
    Colon,
    Goto,
    While,
    Do,
    For,
    Break,
    Continue,
    Switch,
    Case,
    Default,
    Comma,
    Static,
    Extern,
    Long,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub start: usize,
    pub end: usize,
    pub line: u16,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    pub source: &'a str,
    line: u16,
    chars: Peekable<CharIndices<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source,
            line: 0,
            chars: source.char_indices().peekable(),
        }
    }

    pub fn constant(&mut self) -> Result<Token<'a>, CompileError> {
        let (mut end, mut c) = *self.peek().unwrap();
        let start = end - 1;

        while Self::is_digit(c) {
            self.next_char();
            (end, c) = *self.peek().unwrap();
        }
        let mut suffix_end = end;
        while "ul".contains(c.to_ascii_lowercase()) {
            self.next_char();
            (suffix_end, c) = *self.peek().unwrap();
        }
        let mut suffix = self
            .source
            .get(end..suffix_end)
            .unwrap() // Unwrap is safe here because the end..suffix_end range must exist in the source string
            .to_lowercase()
            .into_bytes();
        suffix.sort();
        // Unwrap is safe here because this range has just been proven as only made of digits
        let digits = self.source.get(start..end).unwrap();
        Ok(match suffix[..] {
            [b'l', b'u'] => Token {
                kind: TokenKind::UnsignedLongConstant(digits),
                start,
                end: end + 2,
                line: self.line,
            },
            [b'l'] => Token {
                kind: TokenKind::LongConstant(digits),
                start,
                end: end + 1,
                line: self.line,
            },
            [b'u'] => Token {
                kind: TokenKind::UnsignedConstant(digits),
                start,
                end: end + 1,
                line: self.line,
            },
            [] => Token {
                kind: TokenKind::Constant(digits),
                start,
                end,
                line: self.line,
            },
            _ => return Err(CompileError::Lex(format!("Bad numeric suffix {suffix:?}"))),
        })
    }

    pub fn identifier(&mut self) -> Token<'a> {
        let (mut end, mut c) = *self.peek().unwrap();
        let start = end - 1;
        while Self::is_alpha(c) || Self::is_digit(c) {
            self.next_char();
            (end, c) = *self.peek().unwrap();
        }

        let id = self.source.get(start..end).unwrap();

        let kind = match id {
            "return" => TokenKind::Return,
            "int" => TokenKind::Int,
            "void" => TokenKind::Void,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "goto" => TokenKind::Goto,
            "do" => TokenKind::Do,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "switch" => TokenKind::Switch,
            "case" => TokenKind::Case,
            "default" => TokenKind::Default,
            "static" => TokenKind::Static,
            "extern" => TokenKind::Extern,
            "long" => TokenKind::Long,
            "signed" => TokenKind::Signed,
            "unsigned" => TokenKind::Unsigned,
            _ => TokenKind::Id(id),
        };

        Token {
            kind,
            start,
            end,
            line: self.line,
        }
    }

    fn peek(&mut self) -> Option<&(usize, char)> {
        self.chars.peek()
    }

    fn next_char(&mut self) -> Option<(usize, char)> {
        self.chars.next()
    }

    pub fn is_digit(c: char) -> bool {
        "0123456789".contains(c)
    }

    pub fn is_whitespace(c: char) -> bool {
        " \t\n".contains(c)
    }

    pub fn is_alpha(c: char) -> bool {
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_".contains(c)
    }

    pub fn check_next_char(
        &mut self,
        next_char: char,
        present: TokenKind<'a>,
        absent: TokenKind<'a>,
        start: usize,
    ) -> Token<'a> {
        let (end, kind) = if let Some(&(_, c)) = self.peek()
            && c == next_char
        {
            self.next_char();
            (start + 2, present)
        } else {
            (start + 1, absent)
        };
        Token {
            kind,
            start,
            end,
            line: self.line,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, CompileError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (start, c) = self.next_char()?;

            match c {
                '\n' => {
                    self.line += 1;
                    continue;
                }
                c if Self::is_whitespace(c) => {
                    continue;
                }
                c if Self::is_digit(c) => {
                    let number = self.constant();
                    if let Some((_, next_c)) = self.peek()
                        && Self::is_alpha(*next_c)
                    {
                        return Some(Err(CompileError::Lex("Bad token".to_string())));
                    }
                    return Some(number);
                }
                c if Self::is_alpha(c) => {
                    return Some(Ok(self.identifier()));
                }
                '-' => {
                    if let Some(&(end, '-')) = self.peek() {
                        self.next_char();
                        return Some(Ok(Token {
                            kind: TokenKind::DoubleMinus,
                            start,
                            end,
                            line: self.line,
                        }));
                    } else {
                        return Some(Ok(self.check_next_char(
                            '=',
                            TokenKind::MinusEquals,
                            TokenKind::Minus,
                            start,
                        )));
                    }
                }
                '<' => {
                    if let Some(&(_, '<')) = self.peek() {
                        self.next_char();
                        return Some(Ok(self.check_next_char(
                            '=',
                            TokenKind::DoubleLAngleEquals,
                            TokenKind::DoubleLAngle,
                            start,
                        )));
                    } else {
                        return Some(Ok(self.check_next_char(
                            '=',
                            TokenKind::LAngleEquals,
                            TokenKind::LAngle,
                            start,
                        )));
                    }
                }
                '>' => {
                    if let Some((_, '>')) = self.peek() {
                        self.next_char();
                        return Some(Ok(self.check_next_char(
                            '=',
                            TokenKind::DoubleRAngleEquals,
                            TokenKind::DoubleRAngle,
                            start,
                        )));
                    } else {
                        return Some(Ok(self.check_next_char(
                            '=',
                            TokenKind::RAngleEquals,
                            TokenKind::RAngle,
                            start,
                        )));
                    }
                }
                '&' => {
                    if let Some((_, '&')) = self.peek() {
                        self.next_char();
                        return Some(Ok(Token {
                            kind: TokenKind::DoubleAmpersand,
                            start,
                            end: start + 2,
                            line: self.line,
                        }));
                    } else {
                        return Some(Ok(self.check_next_char(
                            '=',
                            TokenKind::AmpersandEquals,
                            TokenKind::Ampersand,
                            start,
                        )));
                    }
                }
                '|' => {
                    if let Some((_, '|')) = self.peek() {
                        self.next_char();
                        return Some(Ok(Token {
                            kind: TokenKind::DoublePipe,
                            start,
                            end: start + 2,
                            line: self.line,
                        }));
                    } else {
                        return Some(Ok(self.check_next_char(
                            '=',
                            TokenKind::PipeEquals,
                            TokenKind::Pipe,
                            start,
                        )));
                    }
                }
                '=' => {
                    return Some(Ok(self.check_next_char(
                        '=',
                        TokenKind::DoubleEquals,
                        TokenKind::Equals,
                        start,
                    )));
                }
                '!' => {
                    return Some(Ok(self.check_next_char(
                        '=',
                        TokenKind::BangEquals,
                        TokenKind::Bang,
                        start,
                    )));
                }
                '~' => {
                    return Some(Ok(Token {
                        kind: TokenKind::Tilde,
                        start,
                        end: start + 1,
                        line: self.line,
                    }));
                }
                '(' => {
                    return Some(Ok(Token {
                        kind: TokenKind::LParen,
                        start,
                        end: start + 1,
                        line: self.line,
                    }));
                }
                ')' => {
                    return Some(Ok(Token {
                        kind: TokenKind::RParen,
                        start,
                        end: start + 1,
                        line: self.line,
                    }));
                }
                '{' => {
                    return Some(Ok(Token {
                        kind: TokenKind::LBrace,
                        start,
                        end: start + 1,
                        line: self.line,
                    }));
                }
                '}' => {
                    return Some(Ok(Token {
                        kind: TokenKind::RBrace,
                        start,
                        end: start + 1,
                        line: self.line,
                    }));
                }
                ';' => {
                    return Some(Ok(Token {
                        kind: TokenKind::Semicolon,
                        start,
                        end: start + 1,
                        line: self.line,
                    }));
                }
                '+' => {
                    if let Some((_, '+')) = self.peek() {
                        self.next_char();
                        return Some(Ok(Token {
                            kind: TokenKind::DoublePlus,
                            start,
                            end: start + 2,
                            line: self.line,
                        }));
                    } else {
                        return Some(Ok(self.check_next_char(
                            '=',
                            TokenKind::PlusEquals,
                            TokenKind::Plus,
                            start,
                        )));
                    }
                }
                '/' => {
                    return Some(Ok(self.check_next_char(
                        '=',
                        TokenKind::SlashEquals,
                        TokenKind::Slash,
                        start,
                    )));
                }
                '%' => {
                    return Some(Ok(self.check_next_char(
                        '=',
                        TokenKind::PercentEquals,
                        TokenKind::Percent,
                        start,
                    )));
                }
                '*' => {
                    return Some(Ok(self.check_next_char(
                        '=',
                        TokenKind::StarEquals,
                        TokenKind::Star,
                        start,
                    )));
                }
                '^' => {
                    return Some(Ok(self.check_next_char(
                        '=',
                        TokenKind::CaretEquals,
                        TokenKind::Caret,
                        start,
                    )));
                }
                '?' => {
                    return Some(Ok(Token {
                        kind: TokenKind::Huh,
                        start,
                        end: start + 1,
                        line: self.line,
                    }));
                }
                ':' => {
                    return Some(Ok(Token {
                        kind: TokenKind::Colon,
                        start,
                        end: start + 1,
                        line: self.line,
                    }));
                }
                ',' => {
                    return Some(Ok(Token {
                        kind: TokenKind::Comma,
                        start,
                        end: start + 1,
                        line: self.line,
                    }));
                }
                c => return Some(Err(CompileError::Lex(format!("Bad token {}", c)))),
            };
        }
    }
}
