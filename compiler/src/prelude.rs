use crate::{ast::FileId, error, global_state::Visibility, infer, parser};

/// Initialize types and functions only used in tests

pub fn init(instance: &mut infer::Infer) -> Result<(), error::Error> {
    let source = include_str!("../test/prelude.brg");
    let file = FileId(22);

    let res = parser::Parser::lex_and_parse_file(source, file);

    if let Err(err) = res {
        panic!("parse error in prelude {:#?}", err);
    }

    let decls = res.unwrap();

    instance.declare_stmts(&decls, &Visibility::TopLevel);

    let errors = &instance.errors;
    if !errors.is_empty() {
        panic!(
            "Got error while initializing prelude {:#?}",
            errors.first().unwrap(),
        );
    }

    Ok(())
}
