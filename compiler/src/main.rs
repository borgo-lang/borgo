use compiler::ast::{Expr, FileId};
use compiler::codegen::EmittedFile;
use compiler::global_state::Module;
use compiler::type_::ModuleId;
use compiler::{codegen, infer};
use compiler::{fs, prelude};

use serde::Deserialize;

#[derive(Deserialize, Debug)]
enum Input {
    InferExpr(String),
    InferPackage(String),
}

fn build_project() {
    let filesystem = Box::new(fs::LocalFS::new("."));
    let mut instance = infer::Infer::new(filesystem);

    instance.init_std();

    let m = Module::user();

    let import = compiler::global_state::ModuleImport {
        path: "*current*".to_string(),
        name: m.id.0.to_string(),
    };

    instance.module_from_folder(import, infer::DeclareMode::Full);

    if let Some(err) = instance.first_error() {
        eprintln!("{}", err);
        std::process::exit(1);
    }

    {
        let std = instance.gs.get_module(&ModuleId::from_str("std")).unwrap();
        let user = instance.gs.get_module(&ModuleId::from_str("user")).unwrap();

        let mut gen = codegen::Codegen::new(instance);

        // TODO should output multiple files, as many as in module.files
        emit_files(gen.compile_module(&std));
        emit_files(gen.compile_module(&user));
    }
}

fn emit_files(files: Vec<EmittedFile>) {
    for file in files {
        std::fs::write(&file.name, &file.render_source()).unwrap();
    }
}

fn help() {
    println!(
        "
Commands:

  build         compile all .brg files into .go files

"
    );
}

fn run_tests(input: &str) {
    let input: Input = serde_json::from_str(input).unwrap();

    match input {
        Input::InferExpr(source) => {
            let mut ast = compiler::ast::Ast::new();

            let filesystem = Box::new(fs::NoopFS {});
            let mut instance = infer::Infer::new(filesystem);

            prelude::init(&mut instance).unwrap();

            // This shouldn't be necessary,
            // but better do it anyway just to make sure there's no left-over state
            instance.reset_scope();

            let file = FileId(1);
            ast.set_file(&file);

            let e = Expr::from_source(&source);
            let expr = ast.from_expr(e).unwrap();

            // import builtin module
            instance.import_module(None, &compiler::type_::ModuleId("std".to_string()));

            let (new_expr, errors, typ) = instance.infer_expr_with_error(&expr);

            let err = errors
                .first()
                .map(|e| e.stringify(Some(&source)))
                .unwrap_or_else(|| "No errors.".to_string());

            let json = serde_json::to_string_pretty(&new_expr).unwrap();
            println!("{}\n---\n{}\n---\n{}", typ, err, json);
        }

        Input::InferPackage(source) => {
            let filesystem = Box::new(fs::LocalFS::new(".."));
            let mut instance = infer::Infer::new(filesystem);

            instance.init_std();

            let m = Module::user();
            instance.gs.set_module(m.id.clone());
            instance.gs.modules.insert(m.id.clone(), m.clone());

            let test_file = "test.brg";
            instance.file_from_source(test_file, &source);

            // run inference
            instance.declare_module(&m.id);
            instance.infer_module(&m.id);

            let err = instance
                .first_error()
                .unwrap_or_else(|| "No errors.".to_string());

            let decls = instance
                .gs
                .get_module(&m.id)
                .unwrap()
                .get_file(test_file)
                .unwrap()
                .decls;

            let expr = decls.last().cloned().unwrap_or_else(|| Expr::Noop);
            let typ = expr.get_type();

            let json = serde_json::to_string_pretty(&decls).unwrap();
            println!("{}\n---\n{}\n---\n{}", typ, err, json);
        }
    }
}

fn main() {
    // TODO use a proper parsing lib
    let cmd = std::env::args().nth(1);

    if cmd.is_none() {
        help();
        return;
    }

    let cmd = cmd.unwrap();

    if cmd == "build" {
        build_project();
        println!("done");
        return;
    }

    if cmd == "test" {
        let input = std::env::args().nth(2).unwrap();
        run_tests(&input);
        return;
    }

    help();
}
