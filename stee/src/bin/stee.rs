// Command line compiler.

extern crate stee;

use std::io::prelude::*;
use std::fs::File;
use std::io::BufWriter;

fn main() -> std::io::Result<()> {
    let src = r#"
       // comments work!
        func main() : i32 { 
            var x: i32;
            x = 5;
            loop {
                if x == 0 {
                    break;
                }
                x = x - 1;
            }
            return x;
        }
    "#.to_string();
    let module = stee::compile(src).expect("compile error");
    println!("{:?}", module);
    let file = File::create("debug.wasm")?;
    let mut w = BufWriter::new(file);
    w.write(&module)?;
    Ok(())
    //println!("{:?}", module);
}