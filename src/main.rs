use std::{env, fs, path};

use parser::Parser;

mod codegen;
mod emit;
mod lexer;
mod parser;
mod tacky;

fn main() {
    let mut args = env::args().collect::<Vec<String>>();
    let path = &args[1].clone();
    let data = fs::read_to_string(path);
    match data {
        Ok(text) => parse_file(text, path, args.drain(2..).collect()),
        Err(err) => println!("Error reading source file: [{}]", err),
    }
}

fn parse_file(text: String, path_str: &str, rest_args: Vec<String>) {
    let lexed = lexer::Lexer::new(&text).peekable();
    if rest_args.iter().any(|s| s == "--show-lexed") {
        println!("{:?}", lexed);
        std::process::exit(0);
    }
    let parsed = Parser::new(lexed).parse();
    if rest_args.iter().any(|s| s == "--show-parsed") {
        println!("{:?}", parsed);
        std::process::exit(0);
    }
    let tackified = tacky::emit_tacky(parsed);
    if rest_args.iter().any(|s| s == "--show-tackified") {
        println!("{:?}", tackified);
        std::process::exit(0);
    }
    let assembled = codegen::assemble(tackified);
    if rest_args.iter().any(|s| s == "--show-assembled") {
        println!("{:?}", assembled);
        std::process::exit(0);
    }
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
