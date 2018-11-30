// Command line compiler.

extern crate stee;

fn main() {
    let src = r#"
    func main() : i32 {
        return 123;
    }
    "#.to_string();
    let module = stee::compile(src);
    println!("{:?}", module);
}