use std::{env, fs, path};

use error::CompileError;
use parser::Parser;
use semantic_analysis::analyze;

mod ast;
mod codegen;
mod emit;
mod error;
mod interner;
mod lexer;
mod parser;
mod pretty;
mod semantic_analysis;
mod tacky;
mod typecheck;

fn main() -> Result<(), CompileError> {
    let mut args = env::args().collect::<Vec<String>>();
    let library = args.contains(&"-c".to_string());
    let c_path = &args.pop().unwrap().clone();
    let path = path::Path::new(c_path);
    let i_path = path.with_extension("i");
    std::process::Command::new("gcc")
        .args(["-E", "-P", c_path, "-o", i_path.to_str().unwrap()])
        .output()
        .map_err(|_| CompileError::Preprocess(String::from("Failed to preprocess .c file")))?;
    let data = fs::read_to_string(i_path);
    let s_path = path.with_extension("s");
    match data {
        Ok(text) => compile_file(text, &s_path, &args)?,
        Err(err) => {
            return Err(CompileError::FileRead(format!(
                "Error reading source file: [{}]",
                err
            )));
        }
    }
    let out_path = if library {
        path.with_extension("o")
    } else {
        path.with_extension("")
    };
    std::process::Command::new("arch")
        .args([
            "-x86_64",
            "gcc",
            if library { "-c" } else { "" },
            s_path.to_str().unwrap(),
            "-o",
            out_path.to_str().unwrap(),
        ])
        .output()
        .map_err(|_| CompileError::Assemble(String::from("Failed to assemble .s file")))?;
    if !library {
        std::process::Command::new(out_path.to_str().unwrap())
            .output()
            .map_err(|_| CompileError::Execute(String::from("Failed to run executable")))?;
    }
    Ok(())
}

fn compile_file(
    text: String,
    assembly_path: &path::Path,
    rest_args: &[String],
) -> Result<(), CompileError> {
    let mut interner = interner::Interner::new();
    let lexed = lexer::Lexer::new(&text);
    if rest_args.iter().any(|s| s == "--lex") {
        println!("{:?}", lexed.collect::<Result<Vec<_>, CompileError>>()?);
        std::process::exit(0);
    }
    let parsed = Parser::new(lexed, &mut interner)?.parse()?;
    if rest_args.iter().any(|s| s == "--parse") {
        println!("{:?}", pretty::print_program(&parsed));
        std::process::exit(0);
    }
    let (analyzed, mut symbols) = analyze(parsed, &mut interner)?;
    if rest_args.iter().any(|s| s == "--validate") {
        println!("{:?}", analyzed.0);
        std::process::exit(0);
    }
    let tackified = tacky::emit_tacky(analyzed, &mut symbols, &mut interner);
    if rest_args.iter().any(|s| s == "--tacky") {
        println!("{:?}", tackified);
        std::process::exit(0);
    }
    let assembled = codegen::assemble(tackified, &symbols);
    if rest_args.iter().any(|s| s == "--codegen") {
        println!("{:?}", assembled);
        std::process::exit(0);
    }
    let result = emit::emit(
        assembled,
        &interner,
        fs::File::create(assembly_path).expect("Error opening .s file"),
    );
    match result {
        Ok(_) => Ok(()),
        Err(err) => Err(CompileError::Emit(format!("{:?}", err))),
    }
}
