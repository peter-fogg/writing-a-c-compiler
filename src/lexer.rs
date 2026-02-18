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

        while Self::is_alpha(self.peek().unwrap_or(" ")) {
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
                        return Some(Token::Minus);
                    }
                }
                "<" => {
                    if let Some("<") = self.peek() {
                        self.next_char();
                        return Some(Token::DoubleLAngle);
                    } else if let Some("=") = self.peek() {
                        self.next_char();
                        return Some(Token::LAngleEquals);
                    } else {
                        return Some(Token::LAngle);
                    }
                }
                ">" => {
                    if let Some(">") = self.peek() {
                        self.next_char();
                        return Some(Token::DoubleRAngle);
                    } else if let Some("=") = self.peek() {
                        self.next_char();
                        return Some(Token::RAngleEquals);
                    } else {
                        return Some(Token::RAngle);
                    }
                }
                "&" => {
                    if let Some("&") = self.peek() {
                        self.next_char();
                        return Some(Token::DoubleAmpersand);
                    } else {
                        return Some(Token::Ampersand);
                    }
                }
                "|" => {
                    if let Some("|") = self.peek() {
                        self.next_char();
                        return Some(Token::DoublePipe);
                    } else {
                        return Some(Token::Pipe);
                    }
                }
                "=" => {
                    if let Some("=") = self.peek() {
                        self.next_char();
                        return Some(Token::DoubleEquals);
                    } else {
                        return Some(Token::Equals);
                    }
                }
                "!" => {
                    if let Some("=") = self.peek() {
                        self.next_char();
                        return Some(Token::BangEquals);
                    } else {
                        return Some(Token::Bang);
                    }
                }
                "~" => return Some(Token::Tilde),
                "(" => return Some(Token::LParen),
                ")" => return Some(Token::RParen),
                "{" => return Some(Token::LBrace),
                "}" => return Some(Token::RBrace),
                ";" => return Some(Token::Semicolon),
                "+" => return Some(Token::Plus),
                "/" => return Some(Token::Slash),
                "%" => return Some(Token::Percent),
                "*" => return Some(Token::Star),
                "^" => return Some(Token::Caret),
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
