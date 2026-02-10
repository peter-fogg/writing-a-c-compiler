use std::{env, fs, process};

mod lexer;

fn main() {
    let args = env::args().collect::<Vec<String>>();
    let path = &args[1];
    let data = fs::read_to_string(path);
    match data {
        Ok(text) => parse_file(text),
        Err(err) => println!("Error reading source file: [{}]", err),
    }
}

fn parse_file(text: String) {
    let tokens = lexer::Lexer::new(&text).peekable().collect::<Vec<_>>();
    print!("{:?}", tokens);
}
