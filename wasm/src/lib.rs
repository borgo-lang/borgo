use compiler::codegen;
use compiler::fs::WasmFS;
use compiler::global_state::Module;
use compiler::infer;
use compiler::type_::ModuleId;

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

extern crate console_error_panic_hook;

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
pub struct State {
    // io/fs => ("fs.brg" => contents)
    packages: HashMap<String, HashMap<String, String>>,
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
pub struct Output {
    error: Option<String>,
    emitted: Vec<codegen::EmittedFile>,
}

#[wasm_bindgen]
pub fn compile_wasm(source: &str, val: JsValue) -> JsValue {
    console_error_panic_hook::set_once();

    let std: State = serde_wasm_bindgen::from_value(val).unwrap();

    let filesystem = Box::new(WasmFS::new(std.packages));
    let mut instance = infer::Infer::new(filesystem);

    instance.init_std();

    {
        let m = Module::user();
        instance.gs.set_module(m.id.clone());
        instance.gs.modules.insert(m.id.clone(), m.clone());

        let test_file = "test.brg";
        instance.file_from_source(test_file, &source);

        // run inference
        instance.declare_module(&m.id);
        instance.infer_module(&m.id);
    }

    let error = instance.first_error();

    if error.is_some() {
        return serde_wasm_bindgen::to_value(&Output {
            error,
            emitted: vec![],
        })
        .unwrap();
    }

    let std = instance.gs.get_module(&ModuleId::from_str("std")).unwrap();
    let user = instance.gs.get_module(&ModuleId::from_str("user")).unwrap();

    let mut gen = codegen::Codegen::new(instance);

    // TODO asdf this shouldn't return an EmittedFile but rather the full source
    // with imports

    let mut emitted = gen.compile_module(&std);
    emitted.extend(gen.compile_module(&user));

    return serde_wasm_bindgen::to_value(&Output { error, emitted }).unwrap();
}
