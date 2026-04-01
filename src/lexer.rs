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

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a str,
    position: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source,
            position: 0,
        }
    }

    pub fn constant(&mut self) -> Token<'a> {
        let start_index = self.position - 1;

        while Self::is_digit(self.peek().unwrap_or("_")) {
            self.position += 1;
        }

        Token {
            kind: TokenKind::Constant(self.source.get(start_index..self.position).unwrap()),
            start: start_index,
            end: self.position,
        }
    }

    pub fn identifier(&mut self) -> Token<'a> {
        let start_index = self.position - 1;

        while Self::is_alpha(self.peek().unwrap_or(" "))
            || Self::is_digit(self.peek().unwrap_or(" "))
        {
            self.position += 1;
        }

        let id = self.source.get(start_index..self.position).unwrap();

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
            start: start_index,
            end: self.position,
        }
    }

    fn peek(&self) -> Option<&'a str> {
        if self.position >= self.source.len() {
            None
        } else {
            self.source.get(self.position..self.position + 1)
        }
    }

    fn next_char(&mut self) -> Option<&'a str> {
        if self.position >= self.source.len() {
            None
        } else {
            self.position += 1;
            self.source.get(self.position - 1..self.position)
        }
    }

    pub fn is_digit(s: &'a str) -> bool {
        "0123456789".contains(s)
    }

    pub fn is_whitespace(s: &'a str) -> bool {
        " \t\n".contains(s)
    }

    pub fn is_alpha(s: &'a str) -> bool {
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_".contains(s)
    }

    pub fn check_next_char(
        &mut self,
        next_char: &'static str,
        present: TokenKind<'a>,
        absent: TokenKind<'a>,
        start: usize,
    ) -> Token<'a> {
        let kind = if let Some(c) = self.peek()
            && c == next_char
        {
            self.next_char();
            present
        } else {
            absent
        };
        Token {
            kind,
            start,
            end: self.position,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.position;
        loop {
            let c = self.next_char()?;

            match c {
                c if Self::is_whitespace(c) => {
                    continue;
                }
                c if Self::is_digit(c) => {
                    let number = self.constant();
                    if let Some(next_c) = self.peek()
                        && Self::is_alpha(next_c)
                    {
                        panic!("Bad token");
                    }
                    return Some(number);
                }
                c if Self::is_alpha(c) => {
                    return Some(self.identifier());
                }
                "-" => {
                    if let Some("-") = self.peek() {
                        self.next_char();
                        return Some(Token {
                            kind: TokenKind::DoubleMinus,
                            start,
                            end: self.position,
                        });
                    } else {
                        return Some(self.check_next_char(
                            "=",
                            TokenKind::MinusEquals,
                            TokenKind::Minus,
                            start,
                        ));
                    }
                }
                "<" => {
                    if let Some("<") = self.peek() {
                        self.next_char();
                        return Some(self.check_next_char(
                            "=",
                            TokenKind::DoubleLAngleEquals,
                            TokenKind::DoubleLAngle,
                            start,
                        ));
                    } else {
                        return Some(self.check_next_char(
                            "=",
                            TokenKind::LAngleEquals,
                            TokenKind::LAngle,
                            start,
                        ));
                    }
                }
                ">" => {
                    if let Some(">") = self.peek() {
                        self.next_char();
                        return Some(self.check_next_char(
                            "=",
                            TokenKind::DoubleRAngleEquals,
                            TokenKind::DoubleRAngle,
                            start,
                        ));
                    } else {
                        return Some(self.check_next_char(
                            "=",
                            TokenKind::RAngleEquals,
                            TokenKind::RAngle,
                            start,
                        ));
                    }
                }
                "&" => {
                    if let Some("&") = self.peek() {
                        self.next_char();
                        return Some(Token {
                            kind: TokenKind::DoubleAmpersand,
                            start,
                            end: self.position,
                        });
                    } else {
                        return Some(self.check_next_char(
                            "=",
                            TokenKind::AmpersandEquals,
                            TokenKind::Ampersand,
                            start,
                        ));
                    }
                }
                "|" => {
                    if let Some("|") = self.peek() {
                        self.next_char();
                        return Some(Token {
                            kind: TokenKind::DoublePipe,
                            start,
                            end: self.position,
                        });
                    } else {
                        return Some(self.check_next_char(
                            "=",
                            TokenKind::PipeEquals,
                            TokenKind::Pipe,
                            start,
                        ));
                    }
                }
                "=" => {
                    return Some(self.check_next_char(
                        "=",
                        TokenKind::DoubleEquals,
                        TokenKind::Equals,
                        start,
                    ));
                }
                "!" => {
                    return Some(self.check_next_char(
                        "=",
                        TokenKind::BangEquals,
                        TokenKind::Bang,
                        start,
                    ));
                }
                "~" => {
                    return Some(Token {
                        kind: TokenKind::Tilde,
                        start,
                        end: self.position,
                    });
                }
                "(" => {
                    return Some(Token {
                        kind: TokenKind::LParen,
                        start,
                        end: self.position,
                    });
                }
                ")" => {
                    return Some(Token {
                        kind: TokenKind::RParen,
                        start,
                        end: self.position,
                    });
                }
                "{" => {
                    return Some(Token {
                        kind: TokenKind::LBrace,
                        start,
                        end: self.position,
                    });
                }
                "}" => {
                    return Some(Token {
                        kind: TokenKind::RBrace,
                        start,
                        end: self.position,
                    });
                }
                ";" => {
                    return Some(Token {
                        kind: TokenKind::Semicolon,
                        start,
                        end: self.position,
                    });
                }
                "+" => {
                    if let Some("+") = self.peek() {
                        self.next_char();
                        return Some(Token {
                            kind: TokenKind::DoublePlus,
                            start,
                            end: self.position,
                        });
                    } else {
                        return Some(self.check_next_char(
                            "=",
                            TokenKind::PlusEquals,
                            TokenKind::Plus,
                            start,
                        ));
                    }
                }
                "/" => {
                    return Some(self.check_next_char(
                        "=",
                        TokenKind::SlashEquals,
                        TokenKind::Slash,
                        start,
                    ));
                }
                "%" => {
                    return Some(self.check_next_char(
                        "=",
                        TokenKind::PercentEquals,
                        TokenKind::Percent,
                        start,
                    ));
                }
                "*" => {
                    return Some(self.check_next_char(
                        "=",
                        TokenKind::StarEquals,
                        TokenKind::Star,
                        start,
                    ));
                }
                "^" => {
                    return Some(self.check_next_char(
                        "=",
                        TokenKind::CaretEquals,
                        TokenKind::Caret,
                        start,
                    ));
                }
                "?" => {
                    return Some(Token {
                        kind: TokenKind::Huh,
                        start,
                        end: self.position,
                    });
                }
                ":" => {
                    return Some(Token {
                        kind: TokenKind::Colon,
                        start,
                        end: self.position,
                    });
                }
                "," => {
                    return Some(Token {
                        kind: TokenKind::Comma,
                        start,
                        end: self.position,
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
