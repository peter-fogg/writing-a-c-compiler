use std::error::Error;
use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum CompileError {
    Check(String),
    Emit(String),
    Assemble(String),
    Execute(String),
    Preprocess(String),
    FileRead(String),
    Parse(String),
    Lex(String),
}

impl Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let contents = match self {
            CompileError::Assemble(s) => s,
            CompileError::Check(s) => s,
            CompileError::Emit(s) => s,
            CompileError::Execute(s) => s,
            CompileError::Preprocess(s) => s,
            CompileError::Parse(s) => s,
            CompileError::Lex(s) => s,
            CompileError::FileRead(s) => s,
        };
        write!(f, "Compilation error: {contents}")
    }
}

impl Error for CompileError {}
