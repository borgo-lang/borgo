use compiler::ast::{Expr, FileId};
use compiler::codegen::EmittedFile;
use compiler::global_state::Module;
use compiler::type_::ModuleId;
use compiler::{codegen, fs, infer, lexer, parser, prelude};

use serde::Deserialize;

#[derive(Deserialize, Debug)]
enum Input {
    InferExpr(String),
    InferPackage(String),
    ParseExpr(String),
    ParseFile(String),
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

fn parse_source(input: Input) {
    let source = match input {
        Input::ParseExpr(ref source) => source,
        Input::ParseFile(ref source) => source,
        _ => unreachable!(),
    };

    let mut lex = lexer::Lexer::new(source);

    let tokens = lex.tokens();

    let file = FileId(1);
    let mut p = parser::Parser::new(tokens.clone(), file);

    let parsed = match input {
        Input::ParseExpr(_) => vec![p.parse_expr()],
        Input::ParseFile(_) => p.parse_file(),
        _ => unreachable!(),
    };

    let json = if parsed.len() == 1 {
        let expr = parsed.first().unwrap();
        serde_json::to_string_pretty(&expr)
    } else {
        serde_json::to_string_pretty(&parsed)
    };

    let mut err = p.errors.first().cloned().map(|e| format!("{:#?}", e));

    if err.is_none() && !p.eof() {
        err = Some(format!(
            "Leftover input from parser:\n{}",
            lexer::stringify_tokens(&tokens[p.current..])
        ))
    }

    let err = err.unwrap_or_else(|| "No errors.".to_string());

    let tokens_str = lexer::stringify_tokens(&tokens);

    println!("{}\n---\n{}\n---\n{}", tokens_str, err, json.unwrap());
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
            let filesystem = Box::new(fs::NoopFS {});
            let mut instance = infer::Infer::new(filesystem);

            prelude::init(&mut instance).unwrap();

            // This shouldn't be necessary,
            // but better do it anyway just to make sure there's no left-over state
            instance.reset_scope();

            let file = FileId(1);
            let mut lex = lexer::Lexer::new(&source);
            let mut p = parser::Parser::new(lex.tokens(), file);

            let expr = p.parse_expr();

            // import builtin module
            instance.import_module(None, &compiler::type_::ModuleId("std".to_string()));

            let (new_expr, errors, typ) = instance.infer_expr_with_error(&expr);

            let mut err = p.errors.first().cloned().map(|e| format!("{:#?}", e));

            if err.is_none() {
                err = errors.first().map(|e| e.stringify(Some(&source)));
            }

            let err = err.unwrap_or_else(|| "No errors.".to_string());

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

        Input::ParseExpr(_) | Input::ParseFile(_) => parse_source(input),
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
