use std::{env, fs, path};

use parser::Parser;

mod codegen;
mod emit;
mod lexer;
mod parser;
mod tacky;

fn main() {
    let mut args = env::args().collect::<Vec<String>>();
    let c_path = &args.pop().unwrap().clone();
    let path = path::Path::new(c_path);
    let i_path = path.with_extension("i");
    std::process::Command::new("gcc")
        .args(["-E", "-P", c_path, "-o", i_path.to_str().unwrap()])
        .output()
        .expect("Failed to preprocess .c file");
    let data = fs::read_to_string(i_path);
    let s_path = path.with_extension("s");
    match data {
        Ok(text) => compile_file(text, &s_path, args),
        Err(err) => println!("Error reading source file: [{}]", err),
    }
    let out_path = path.with_extension("");
    std::process::Command::new("arch")
        .args([
            "-x86_64",
            "gcc",
            s_path.to_str().unwrap(),
            "-o",
            out_path.to_str().unwrap(),
        ])
        .output()
        .expect("Failed to assemble .s file");
    std::process::Command::new(out_path.to_str().unwrap())
        .output()
        .expect("Failed to run executable");
}

fn compile_file(text: String, assembly_path: &path::Path, rest_args: Vec<String>) {
    let lexed = lexer::Lexer::new(&text).peekable();
    if rest_args.iter().any(|s| s == "--lex") {
        println!("{:?}", lexed.collect::<Vec<_>>());
        std::process::exit(0);
    }
    let parsed = Parser::new(lexed).parse();
    if rest_args.iter().any(|s| s == "--parse") {
        println!("{:?}", parsed);
        std::process::exit(0);
    }
    let tackified = tacky::emit_tacky(parsed);
    if rest_args.iter().any(|s| s == "--tackify") {
        println!("{:?}", tackified);
        std::process::exit(0);
    }
    let assembled = codegen::assemble(tackified);
    if rest_args.iter().any(|s| s == "--codegen") {
        println!("{:?}", assembled);
        std::process::exit(0);
    }
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
