// Command line compiler.

extern crate stee;

use std::io::prelude::*;
use std::fs::File;
use std::io::BufWriter;

fn main() -> std::io::Result<()> {
    let src = r#"
        // comments work!
        var x : i32;
        func main() : i32 { 
            x = 10;
            return x;
        }
    "#.to_string();
    let module = stee::compile(src);
    if let Err(err)  = module {
        println!("{:?}", err);
        return Ok(());
    }
    let module = module.unwrap();
    println!("{:?}", module);
    let file = File::create("debug.wasm")?;
    let mut w = BufWriter::new(file);
    w.write(&module)?;
    Ok(())
}