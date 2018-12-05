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

// od -t d1 it.wasm

// Add the other primitive types.
// Do typechecking and pick operators.
// Add all the rest of the operators based on types.
// Add transform operators too.
// control structures
// better error messages
// Then I can think about structs and arrays and stuff, but I need that base first.

// That will be v1, all the wasm primitives with the 4 wasm types.
// then for v2 I'll play with memory management, references, structs and arrays

// If I actually do this language I will be so happy!

// How can I make working on this not suck?
// I should make a feature for making it a webassembly library.
// Then I can disable that feature and have a binary that's a test program that lets me just run a program and debug it.
// If that's a good idea it could help me out A TON with debugging this.

// That could let me debug this in clion too which might be better since I can view rust values.

// I still can't fully debug everything so I really need printing which is why I need a binary I can debug that isn't a test.
// Gonna set up a new project that has a debugable binary, has tests that use a spec interpreter and then feature flags that
// set up the wasm stuff so I can still publish to npm.

// I think I guess I need to really learn how to use vscode too.