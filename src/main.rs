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
    let token_result = lexer::Lexer::new(&text)
        .peekable()
        .collect::<Result<Vec<_>, _>>();

    match token_result {
        Ok(tokens) => {
            print!("{:?}", tokens);
        }
        Err(err) => {
            println!("{:?}", err);
            process::exit(1);
        }
    }
}
