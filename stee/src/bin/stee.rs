// Command line compiler.

extern crate stee;

use std::io::prelude::*;
use std::fs::File;
use std::io::BufWriter;

fn main() -> std::io::Result<()> {
    let src = r#"
    import func foo(x: i32) : i32;
    export func main() : i32 {
      return foo(5);
    }"#.to_string();
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

// so in the example, table has 2 1's   (table 1 1 anyfunc), and it's just a call instead of a call_indirect.