extern crate cfg_if;
extern crate wasm_bindgen;
extern crate js_sys;
extern crate stee;

mod utils;

use cfg_if::cfg_if;
use wasm_bindgen::prelude::*;
use js_sys::WebAssembly;

cfg_if! {
    // When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
    // allocator.
    if #[cfg(feature = "wee_alloc")] {
        extern crate wee_alloc;
        #[global_allocator]
        static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
    }
}

#[wasm_bindgen]
pub fn compile(src: String, import_object: js_sys::Object) -> Result<js_sys::Promise, JsValue> {
    let compiled_buf = stee::compile(src)?;
    Ok(WebAssembly::instantiate_buffer(&compiled_buf, &import_object))
}