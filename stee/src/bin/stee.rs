// @TODO: Command line compiler.

extern crate stee;

use std::io::prelude::*;
use std::fs::File;
use std::io::BufWriter;

// @NOTE: Right now I just use this to debug. Eventuially this will be a cli for the compiler.
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