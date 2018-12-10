#[macro_use] extern crate failure;
extern crate byteorder;

use self::types::*;
use self::lex::*;
use self::parse::*;
use self::emit::*;

pub mod types;
mod lex;
mod parse;
mod emit;

fn string_err(err: CompileError) -> String {
    format!("{:?}", err)
}

fn compile_module(src: String) -> Result<Vec<u8>, CompileError> {
    let mut lex = Lexer::new(&src)?;
    let module = parse_module(&mut lex)?;
    let wasm = emit_module(module)?;
    Ok(wasm)
}

pub fn compile(src: String) -> Result<Vec<u8>, String> {
    Ok(compile_module(src).map_err(string_err)?)
}