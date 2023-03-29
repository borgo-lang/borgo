use std::vec;

use compiler::ast::{Expr, Span, UnparsedFile};
use compiler::codegen;
use compiler::infer;
use compiler::prelude;
use compiler::project::{self, Package, Project};
use compiler::type_::{self, Type};

use serde::Deserialize;

#[derive(Deserialize, Debug)]
enum Input {
    InferExpr(String),
    InferPackage(String),
}

fn scan_folder(name: &str, folder: &str) -> Package {
    let files = std::fs::read_dir(folder)
        .unwrap()
        .filter_map(|entry| {
            let path = entry
                .unwrap()
                .path()
                .canonicalize()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string();

            if path.ends_with(".brg") {
                let contents = std::fs::read_to_string(&path).unwrap();
                return Some(UnparsedFile {
                    filename: std::path::Path::new(&path)
                        .file_name()
                        .unwrap()
                        .to_os_string()
                        .into_string()
                        .unwrap(),
                    contents,
                });
            }

            None
        })
        .collect();

    Package::from_file_contents(name.to_string(), files)
}

fn init_project() -> Project {
    let std = scan_folder("std", "./std");
    let user = scan_folder("user", ".");

    Project::from_packages(vec![std, user])
}

fn build_project() -> project::Project {
    let mut project = init_project();
    let mut instance = infer::Infer::new();

    project.infer(&mut instance);

    if let Some(err) = project.to_json().first_error {
        eprintln!("{}", err.msg);
        std::process::exit(1);
    }

    let mut gen = codegen::Codegen::new(&instance.gs);
    project.build(&mut gen);
    project
}

fn emit_files(project: Project) {
    project.output.iter().for_each(|(_, file)| {
        std::fs::write(&file.name, &file.source).unwrap();
    });
}

fn emit_typed_project(project: Project) {
    let encoded = bincode::serialize(&project.packages).unwrap();
    std::fs::write("project.bin", encoded).unwrap();
}

fn main() {
    let args = std::env::args().last().unwrap();

    if args == "build" {
        let project = build_project();
        emit_files(project);
        println!("done");
        return;
    }

    if args == "dump-binary-ast" {
        let project = build_project();
        emit_typed_project(project);
        println!("done");
        return;
    }

    let input: Input = serde_json::from_str(&args).unwrap();

    match input {
        Input::InferExpr(source) => {
            let e = Expr::from_source(&source);
            let expr = Expr::from_expr(e).unwrap();

            let mut instance = infer::Infer::new();

            prelude::init(&mut instance);

            // Set the error type explicitely, as there's no package to grab it from
            let err_ty = Type::Con {
                name: "ErrFoo".to_string(),
                args: vec![],
            };

            let (new_expr, errors, typ) = instance.infer_expr_with_error(&expr, err_ty);

            let err = errors
                .first()
                .map(|e| e.stringify(Some(&source)))
                .unwrap_or_else(|| "No errors.".to_string());

            let fx = match typ {
                type_::Type::Fun { ref fx, .. } => {
                    // convert to a set so that duplicate entries can be removed
                    let fx = fx.iter().collect::<std::collections::HashSet<_>>();

                    // then sort
                    let mut fx = fx.iter().collect::<Vec<_>>();
                    fx.sort();

                    format!("{:?}", fx)
                }
                _ => "[]".to_string(), // empty vec
            };

            let json = serde_json::to_string_pretty(&new_expr).unwrap();
            println!("{}\n---\n{}\n---\n{}\n---\n{}", typ, err, fx, json);
        }

        Input::InferPackage(source) => {
            let mut instance = infer::Infer::new();

            let files = vec![UnparsedFile {
                filename: "test.brg".to_string(),
                contents: source,
            }];

            let std = scan_folder("std", "../runtime/std");
            let pkg = Package::from_file_contents(project::Project::user(), files);

            let mut project = Project::from_packages(vec![std, pkg]);

            project.infer(&mut instance);

            let new_pkg = project
                .packages
                .get(&project::Project::user())
                .cloned()
                .unwrap();

            let json = project::PackageJSON(new_pkg.clone());
            let json = serde_json::to_string_pretty(&json).unwrap();

            let expr = if new_pkg.first_error().is_none() {
                new_pkg.files.first().unwrap().decls.last().unwrap().clone()
            } else {
                Expr::Unit {
                    span: Span::dummy(),
                }
            };

            let typ = expr.get_type();
            let err = new_pkg
                .first_error()
                .map(|(file, e)| e.stringify(Some(&file.source)))
                .unwrap_or_else(|| "No errors.".to_string());

            println!("{}\n---\n{}\n---\n{}", typ, err, json);
        }
    }
}
