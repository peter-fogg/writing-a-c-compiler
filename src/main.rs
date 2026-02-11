use std::{env, fs, path};

use parser::Parser;

mod codegen;
mod emit;
mod lexer;
mod parser;

fn main() {
    let args = env::args().collect::<Vec<String>>();
    let path = &args[1];
    let data = fs::read_to_string(path);
    match data {
        Ok(text) => parse_file(text, path),
        Err(err) => println!("Error reading source file: [{}]", err),
    }
}

fn parse_file(text: String, path_str: &str) {
    let tokens = lexer::Lexer::new(&text).peekable();
    let parsed = Parser::new(tokens).parse();
    let assembled = codegen::assemble(parsed);
    let path = path::Path::new(path_str);
    let assembly_path = path.with_extension("S");
    let result = emit::emit(
        assembled,
        fs::File::create(assembly_path).expect("Error opening .s file"),
    );
    match result {
        Ok(_) => (),
        Err(err) => {
            panic!("{:?}", err);
        }
    }
}
