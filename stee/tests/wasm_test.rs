extern crate stee;
extern crate wasmi;

use wasmi::{ModuleInstance, ImportsBuilder, NopExternals, RuntimeValue};

fn test_program(src: &str, func: &str, args: &[RuntimeValue], result: RuntimeValue) {
    let wasm_binary = stee::compile(src.to_string()).expect("failed to compile");
    let module = wasmi::Module::from_buffer(&wasm_binary).expect("failed to load wasm");
    let instance = ModuleInstance::new(
        &module,
        &ImportsBuilder::default()
    ).expect("failed to instantiate wasm module").assert_no_start();
    assert_eq!(
        instance.invoke_export(func, args, &mut NopExternals).expect("failed to execute export"),
        Some(result)
    )
}

// Unit tests for all the language features.
// @TODO: I really need comments!
#[test]
fn test_lots_of_stuff() {
    test_program(r#"
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

        }"#,
        "main",
        &[],
        RuntimeValue::I32(5)
    );
}

#[test]
fn test_it_runs() {
    test_program(r#"
        func main() : i32 { 
            return 0;
        }"#,
        "main",
        &[],
        RuntimeValue::I32(0)
    );
}

#[test]
fn it_still_tests() {
    test_program(r#"
        func steveadd(a: i32, b: i32) : i32 {
            return add(a,b);
        }
        func main(a: i32, b: i32) : i32 { 
            return steveadd(a,b);
        }"#,
        "main",
        &[RuntimeValue::I32(1),RuntimeValue::I32(2)],
        RuntimeValue::I32(3)
    );
}

#[test]
fn it_tests() {
    let wasm_binary: Vec<u8> = stee::compile(r#"
    func steveadd(a: i32, b: i32) : i32 {
        return add(a,b);
    }
    func main(a: i32, b: i32) : i32 { 
        return steveadd(a,b);
    }
    "#.to_string()).expect("failed to compile");
    let module = wasmi::Module::from_buffer(&wasm_binary).expect("failed to load wasm");
    let instance = ModuleInstance::new(
        &module,
        &ImportsBuilder::default()
    ).expect("failed to instantiate wasm module").assert_no_start();
    assert_eq!(
        instance.invoke_export(
            "main",
            &[RuntimeValue::I32(1),RuntimeValue::I32(2)],
            &mut NopExternals,
        ).expect("failed to execute export"),
        Some(RuntimeValue::I32(3)));
}