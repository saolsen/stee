extern crate stee;
extern crate wasmi;

use wasmi::{ModuleInstance, ImportsBuilder, NopExternals, RuntimeValue};

#[test]
fn it_tests() {
    let wasm_binary: Vec<u8> = stee::compile(r#"
    func main() : i32 { 
        return 1;
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
            &[],
            &mut NopExternals,
        ).expect("failed to execute export"),
        Some(RuntimeValue::I32(1)));
}