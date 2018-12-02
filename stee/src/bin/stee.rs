// Command line compiler.

extern crate stee;

fn main() {
    let src = r#"
    func main(a: i32, b: i32) : i32 {
        return add(a,b);
    }
    "#.to_string();
    let module = stee::compile(src);
    println!("{:?}", module);
}