// Command line compiler.

extern crate stee;

fn main() {
    let src = r#"
    func main() : f32 {
        return 123.45;
    }
    "#.to_string();
    let module = stee::compile(src);
    println!("{:?}", module);
}