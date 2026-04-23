use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug, PartialEq, Clone, Copy)]
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

    pub fn constant(&mut self) -> Token<'a> {
        let &(mut end, mut c) = self.peek().unwrap();
        let start = end - 1;

        while Self::is_digit(c) {
            (end, c) = self.next_char().unwrap();
        }

        Token {
            kind: TokenKind::Constant(self.source.get(start..end).unwrap()),
            start,
            end,
            line: self.line,
        }
    }

    pub fn identifier(&mut self) -> Token<'a> {
        let &(mut end, mut c) = self.peek().unwrap();
        let start = end - 1;

        while Self::is_alpha(c) || Self::is_digit(c) {
            (end, c) = self.next_char().unwrap();
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
    type Item = Token<'a>;

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
                        panic!("Bad token");
                    }
                    return Some(number);
                }
                c if Self::is_alpha(c) => {
                    return Some(self.identifier());
                }
                '-' => {
                    if let Some(&(end, '-')) = self.peek() {
                        self.next_char();
                        return Some(Token {
                            kind: TokenKind::DoubleMinus,
                            start,
                            end,
                            line: self.line,
                        });
                    } else {
                        return Some(self.check_next_char(
                            '=',
                            TokenKind::MinusEquals,
                            TokenKind::Minus,
                            start,
                        ));
                    }
                }
                '<' => {
                    if let Some(&(_, '<')) = self.peek() {
                        self.next_char();
                        return Some(self.check_next_char(
                            '=',
                            TokenKind::DoubleLAngleEquals,
                            TokenKind::DoubleLAngle,
                            start,
                        ));
                    } else {
                        return Some(self.check_next_char(
                            '=',
                            TokenKind::LAngleEquals,
                            TokenKind::LAngle,
                            start,
                        ));
                    }
                }
                '>' => {
                    if let Some((_, '>')) = self.peek() {
                        self.next_char();
                        return Some(self.check_next_char(
                            '=',
                            TokenKind::DoubleRAngleEquals,
                            TokenKind::DoubleRAngle,
                            start,
                        ));
                    } else {
                        return Some(self.check_next_char(
                            '=',
                            TokenKind::RAngleEquals,
                            TokenKind::RAngle,
                            start,
                        ));
                    }
                }
                '&' => {
                    if let Some((_, '&')) = self.peek() {
                        self.next_char();
                        return Some(Token {
                            kind: TokenKind::DoubleAmpersand,
                            start,
                            end: start + 2,
                            line: self.line,
                        });
                    } else {
                        return Some(self.check_next_char(
                            '=',
                            TokenKind::AmpersandEquals,
                            TokenKind::Ampersand,
                            start,
                        ));
                    }
                }
                '|' => {
                    if let Some((_, '|')) = self.peek() {
                        self.next_char();
                        return Some(Token {
                            kind: TokenKind::DoublePipe,
                            start,
                            end: start + 2,
                            line: self.line,
                        });
                    } else {
                        return Some(self.check_next_char(
                            '=',
                            TokenKind::PipeEquals,
                            TokenKind::Pipe,
                            start,
                        ));
                    }
                }
                '=' => {
                    return Some(self.check_next_char(
                        '=',
                        TokenKind::DoubleEquals,
                        TokenKind::Equals,
                        start,
                    ));
                }
                '!' => {
                    return Some(self.check_next_char(
                        '=',
                        TokenKind::BangEquals,
                        TokenKind::Bang,
                        start,
                    ));
                }
                '~' => {
                    return Some(Token {
                        kind: TokenKind::Tilde,
                        start,
                        end: start + 1,
                        line: self.line,
                    });
                }
                '(' => {
                    return Some(Token {
                        kind: TokenKind::LParen,
                        start,
                        end: start + 1,
                        line: self.line,
                    });
                }
                ')' => {
                    return Some(Token {
                        kind: TokenKind::RParen,
                        start,
                        end: start + 1,
                        line: self.line,
                    });
                }
                '{' => {
                    return Some(Token {
                        kind: TokenKind::LBrace,
                        start,
                        end: start + 1,
                        line: self.line,
                    });
                }
                '}' => {
                    return Some(Token {
                        kind: TokenKind::RBrace,
                        start,
                        end: start + 1,
                        line: self.line,
                    });
                }
                ';' => {
                    return Some(Token {
                        kind: TokenKind::Semicolon,
                        start,
                        end: start + 1,
                        line: self.line,
                    });
                }
                '+' => {
                    if let Some((_, '+')) = self.peek() {
                        self.next_char();
                        return Some(Token {
                            kind: TokenKind::DoublePlus,
                            start,
                            end: start + 2,
                            line: self.line,
                        });
                    } else {
                        return Some(self.check_next_char(
                            '=',
                            TokenKind::PlusEquals,
                            TokenKind::Plus,
                            start,
                        ));
                    }
                }
                '/' => {
                    return Some(self.check_next_char(
                        '=',
                        TokenKind::SlashEquals,
                        TokenKind::Slash,
                        start,
                    ));
                }
                '%' => {
                    return Some(self.check_next_char(
                        '=',
                        TokenKind::PercentEquals,
                        TokenKind::Percent,
                        start,
                    ));
                }
                '*' => {
                    return Some(self.check_next_char(
                        '=',
                        TokenKind::StarEquals,
                        TokenKind::Star,
                        start,
                    ));
                }
                '^' => {
                    return Some(self.check_next_char(
                        '=',
                        TokenKind::CaretEquals,
                        TokenKind::Caret,
                        start,
                    ));
                }
                '?' => {
                    return Some(Token {
                        kind: TokenKind::Huh,
                        start,
                        end: start + 1,
                        line: self.line,
                    });
                }
                ':' => {
                    return Some(Token {
                        kind: TokenKind::Colon,
                        start,
                        end: start + 1,
                        line: self.line,
                    });
                }
                ',' => {
                    return Some(Token {
                        kind: TokenKind::Comma,
                        start,
                        end: start + 1,
                        line: self.line,
                    });
                }
                c => panic!("Bad token {}", c),
            };
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use TokenKind::*;

    #[test]
    fn whitespace() {
        let tokens = Lexer::new(" \t      \n\n  \n").collect::<Vec<_>>();
        assert_eq!(tokens, vec![]);
    }

    #[test]
    fn numbers() {
        let tokens = Lexer::new("1124\n").collect::<Vec<_>>();
        assert_eq!(tokens, vec![Constant("1124")]);
    }

    #[test]
    fn punctuation() {
        let tokens = Lexer::new("; ( ) { } \n").collect::<Vec<_>>();
        assert_eq!(tokens, vec![Semicolon, LParen, RParen, LBrace, RBrace]);
    }

    #[test]
    fn identifiers() {
        let tokens = Lexer::new("return int void ").collect::<Vec<_>>();
        assert_eq!(tokens, vec![Return, Int, Void]);
    }
}
