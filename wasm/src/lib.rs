use compiler::ast::UnparsedFile;
use compiler::infer;
use compiler::project::{self, Project};


use wasm_bindgen::prelude::*;

extern crate console_error_panic_hook;

#[wasm_bindgen]
pub fn compile_wasm(contents: &str, std_source: &str) -> Result<Vec<u8>, String> {
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

    let encoded = bincode::serialize(&project).unwrap();
    Ok(encoded)
}

#[wasm_bindgen]
pub fn on_hover(bytes: Vec<u8>, line: u32, character: u32) -> Option<String> {
    let project: Project = bincode::deserialize(&bytes).unwrap();

    let span = compiler::ast::Span::from_position(line, character);

    let pkg = project.user_package().unwrap();
    let file = pkg.files.first().unwrap();
    let expr = file.find_expr_at_position(&span);

    if expr.is_none() {
        eprintln!("hover, can't find expr at {:?}", span);
        return None;
    }

    let ty = expr.unwrap().get_type();

    let contents = format!("{}", ty);

    Some(contents)
}
