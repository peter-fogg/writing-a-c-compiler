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
    Semicolon
}

#[derive(Debug)]
pub struct Scanner<'a> {
    source: &'a str,
    position: usize,
}

impl<'a> Scanner<'a > {
    pub fn new(source: &'a str) -> Self {
        Scanner {
            source,
            position: 0,
        }
    }

    pub fn constant(&mut self) -> Option<Token<'a>> {
        let start_index = self.position - 1;

        while Self::is_digit(self.peek().unwrap_or("_")) {
            self.position += 1;
        }

        Some(Token::Constant(self.source.get(start_index..self.position)?))
    }

    pub fn identifier(&mut self) -> Option<Token<'a>> {
        let start_index = self.position - 1;

        while Self::is_alpha(self.peek().unwrap_or(" ")) {
            self.position += 1;
        }

        let id = self.source.get(start_index..self.position)?;

        let token = match id {
            "return" => Token::Return,
            "int" => Token::Int,
            "void" => Token::Void,
            _ => Token::Id(id),
        };

        Some(token)
    }

    fn peek(&self) -> Option<&'a str> {
        if self.position >= self.source.len() {
            None
        } else {
            self.source.get(self.position..self.position+1)
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

impl<'a> Iterator for Scanner<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let c = self.next_char()?;
            match c {
                c if Self::is_whitespace(c) => { continue; },
                c if Self::is_digit(c) => {
                    return self.constant();
                },
                c if Self::is_alpha(c) => {
                    return self.identifier();
                }
                "(" => return Some(Token::LParen),
                ")" => return Some(Token::RParen),
                "{" => return Some(Token::LBrace),
                "}" => return Some(Token::RBrace),
                ";" => return Some(Token::Semicolon),
                _ => return None
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use Token::*;

    #[test]
    fn whitespace() {
        let tokens = Scanner::new(" \t      \n\n  \n").collect::<Vec<_>>();
        assert_eq!(tokens, vec![]);
    }

    #[test]
    fn numbers() {
        let tokens = Scanner::new("1124\n").collect::<Vec<_>>();
        assert_eq!(tokens, vec![Token::Constant("1124")]);
    }

    #[test]
    fn punctuation() {
        let tokens = Scanner::new("; ( ) { } \n").collect::<Vec<_>>();
        assert_eq!(tokens, vec![Semicolon, LParen, RParen, LBrace, RBrace]);
    }

    #[test]
    fn identifiers() {
        let tokens = Scanner::new("return int void ").collect::<Vec<_>>();
        assert_eq!(tokens, vec![Return, Int, Void]);
    }
}
