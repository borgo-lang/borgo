use compiler::ast::UnparsedFile;
use compiler::codegen;
use compiler::infer;
use compiler::project;

use wasm_bindgen::prelude::*;

extern crate console_error_panic_hook;

#[wasm_bindgen]
pub fn compile_wasm(contents: &str, std_source: &str) -> Result<String, String> {
    console_error_panic_hook::set_once();

    let mut instance = infer::Infer::new();
    let mut project = project::Project::new();

    let files = vec![UnparsedFile {
        filename: "app.brg".to_string(),
        contents: contents.to_string(),
    }];

    let std_files = vec![UnparsedFile {
        filename: "std.brg".to_string(),
        contents: std_source.to_string(),
    }];

    project.compile(project::Project::std(), &mut instance, std_files);
    project.compile(project::Project::user(), &mut instance, files);

    // TODO no need to turn it into json, see main.rs
    if let Some(err) = project.to_json().first_error {
        return Err(err.msg);
    }

    let mut gen = codegen::Codegen::new(&instance.gs);
    project.build(&mut gen);

    let json = serde_json::to_string(&project.output).unwrap();
    Ok(json)
}
