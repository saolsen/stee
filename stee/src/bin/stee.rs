// Command line compiler.

extern crate stee;

use std::io::prelude::*;
use std::fs::File;
use std::io::BufWriter;

fn main() -> std::io::Result<()> {
    let src = r#"
    // comments work!
    func main() : i32 { 
        var y: i64;
        var x: i32;
        x = !y;
        //y = y & y | y ^ !y + y / y * y - y + -y;
        //var x: i32;
        //x = x & x | x ^ !x;
        //return 10 / 11 * 12 + 12 - 14 % x;
        return 5;
    }"#.to_string();
    let module = stee::compile(src).expect("compile error");
    let file = File::create("debug.wasm")?;
    let mut w = BufWriter::new(file);
    w.write(&module)?;
    Ok(())
    //println!("{:?}", module);
}