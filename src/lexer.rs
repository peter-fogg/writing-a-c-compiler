#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token<'a> {
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
    Minus,
    Star,
    Slash,
    Percent,
    DoubleMinus,
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

        Token::Constant(self.source.get(start_index..self.position).unwrap())
    }

    pub fn identifier(&mut self) -> Token<'a> {
        let start_index = self.position - 1;

        while Self::is_alpha(self.peek().unwrap_or(" "))
            || Self::is_digit(self.peek().unwrap_or(" "))
        {
            self.position += 1;
        }

        let id = self.source.get(start_index..self.position).unwrap();

        match id {
            "return" => Token::Return,
            "int" => Token::Int,
            "void" => Token::Void,
            _ => Token::Id(id),
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
        present: Token<'a>,
        absent: Token<'a>,
    ) -> Token<'a> {
        if let Some(c) = self.peek()
            && c == next_char
        {
            self.next_char();
            present
        } else {
            absent
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
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
                        return Some(Token::DoubleMinus);
                    } else {
                        return Some(self.check_next_char("=", Token::MinusEquals, Token::Minus));
                    }
                }
                "<" => {
                    if let Some("<") = self.peek() {
                        self.next_char();
                        return Some(self.check_next_char(
                            "=",
                            Token::DoubleLAngleEquals,
                            Token::DoubleLAngle,
                        ));
                    } else {
                        return Some(self.check_next_char("=", Token::LAngleEquals, Token::LAngle));
                    }
                }
                ">" => {
                    if let Some(">") = self.peek() {
                        self.next_char();
                        return Some(self.check_next_char(
                            "=",
                            Token::DoubleRAngleEquals,
                            Token::DoubleRAngle,
                        ));
                    } else {
                        return Some(self.check_next_char("=", Token::RAngleEquals, Token::RAngle));
                    }
                }
                "&" => {
                    if let Some("&") = self.peek() {
                        self.next_char();
                        return Some(Token::DoubleAmpersand);
                    } else {
                        return Some(self.check_next_char(
                            "=",
                            Token::AmpersandEquals,
                            Token::Ampersand,
                        ));
                    }
                }
                "|" => {
                    if let Some("|") = self.peek() {
                        self.next_char();
                        return Some(Token::DoublePipe);
                    } else {
                        return Some(self.check_next_char("=", Token::PipeEquals, Token::Pipe));
                    }
                }
                "=" => {
                    return Some(self.check_next_char("=", Token::DoubleEquals, Token::Equals));
                }
                "!" => {
                    return Some(self.check_next_char("=", Token::BangEquals, Token::Bang));
                }
                "~" => return Some(Token::Tilde),
                "(" => return Some(Token::LParen),
                ")" => return Some(Token::RParen),
                "{" => return Some(Token::LBrace),
                "}" => return Some(Token::RBrace),
                ";" => return Some(Token::Semicolon),
                "+" => return Some(self.check_next_char("=", Token::PlusEquals, Token::Plus)),
                "/" => return Some(self.check_next_char("=", Token::SlashEquals, Token::Slash)),
                "%" => {
                    return Some(self.check_next_char("=", Token::PercentEquals, Token::Percent));
                }
                "*" => return Some(self.check_next_char("=", Token::StarEquals, Token::Star)),
                "^" => return Some(self.check_next_char("=", Token::CaretEquals, Token::Caret)),
                c => panic!("Bad token {}", c),
            };
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use Token::*;

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
