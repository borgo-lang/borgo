use crate::{
    error,
    global_state::{Visibility},
    infer,
};

/// Initialize types and functions only used in tests

pub fn init(instance: &mut infer::Infer) -> Result<(), error::Error> {
    let source = include_str!("../test/prelude.brg");
    // let source = format!("{{ {} }}", source); // turn it into a block
    let parsed = syn::parse_str::<syn::File>(source).unwrap();

    let ast = crate::ast::Ast::new();
    let file = ast
        .from_file("prelude.brg".to_string(), source.to_string(), parsed)
        .unwrap();

    // Add any in scope
    // let any_ty = instance.get_type("any").unwrap();
    // instance.add_value("EXT", any_ty, &Span::dummy());

    instance.declare_stmts(&file.decls, &Visibility::TopLevel);

    // let m = instance.gs.get_module(&ModuleId("std".to_string()));
    // dbg!(m);

    // let (_, errors, _) = instance.infer_expr_with_error(&expr);

    let errors = &instance.errors;
    if !errors.is_empty() {
        panic!(
            "Got error while initializing prelude {:#?}",
            errors.first().unwrap(),
        );
    }

    Ok(())
}

/*
pub fn populate_prelude(gs: &mut global_state::GlobalState) {
    let gen_t = Type::generic("T");
    let gen_y = Type::generic("Y");

    gs.add_builtin_type("int".into(), Type::int().to_bounded());
    gs.add_builtin_type("bool".into(), Type::bool().to_bounded());
    gs.add_builtin_type("string".into(), Type::string().to_bounded());

    gs.add_builtin_value(
        "inc".into(),
        Type::Fun {
            args: vec![Type::int()],
            bounds: vec![],
            ret: Type::int().into(),
            id: TypeId::unset(),
        }
        .to_bounded(),
    );

    gs.add_builtin_value(
        "dec".into(),
        Type::Fun {
            args: vec![Type::int()],
            bounds: vec![],
            ret: Type::int().into(),
            id: TypeId::unset(),
        }
        .to_bounded(),
    );

    gs.add_builtin_value(
        "sum".into(),
        Type::Fun {
            args: vec![Type::int(), Type::int()],
            bounds: vec![],
            ret: Type::int().into(),
            id: TypeId::unset(),
        }
        .to_bounded(),
    );

    gs.add_builtin_value(
        "hello".into(),
        Type::Fun {
            args: vec![],
            bounds: vec![],
            ret: Type::string().into(),
            id: TypeId::unset(),
        }
        .to_bounded(),
    );

    gs.add_builtin_value(
        "list_push".into(),
        Type::Fun {
            args: vec![Type::slice(gen_t.clone()), gen_t.clone()],
            bounds: vec![],
            ret: Type::slice(gen_t.clone()).into(),
            id: TypeId::unset(),
        }
        .to_bounded_with_generics(vec!["T".to_string()]),
    );

    let map_fn = Type::Fun {
        args: vec![gen_t.clone()],
        bounds: vec![],
        ret: gen_y.clone().into(),
        id: TypeId::unset(),
    };

    gs.add_builtin_value(
        "list_map".into(),
        Type::Fun {
            args: vec![Type::slice(gen_t.clone()), map_fn],
            bounds: vec![],
            ret: Type::slice(gen_y.clone()).into(),
            id: TypeId::unset(),
        }
        .to_bounded_with_generics(vec!["T".to_string(), "Y".to_string()]),
    );

    gs.add_builtin_value(
        "list_head".into(),
        Type::Fun {
            args: vec![Type::slice(gen_t.clone())],
            bounds: vec![],
            ret: gen_t.clone().into(),
            id: TypeId::unset(),
        }
        .to_bounded_with_generics(vec!["T".to_string()]),
    );

    gs.add_builtin_value(
        "int_to_string".into(),
        Type::Fun {
            args: vec![Type::int()],
            bounds: vec![],
            ret: Type::string().into(),
            id: TypeId::unset(),
        }
        .to_bounded(),
    );

    gs.add_builtin_value(
        "str_concat".into(),
        Type::Fun {
            args: vec![Type::string(), Type::string()],
            bounds: vec![],
            ret: Type::string().into(),
            id: TypeId::unset(),
        }
        .to_bounded(),
    );

    gs.add_builtin_value(
        "abs".into(),
        Type::Fun {
            args: vec![Type::int()],
            bounds: vec![],
            ret: Type::int().into(),
            id: TypeId::unset(),
        }
        .to_bounded(),
    );

    gs.add_builtin_value(
        "fst".into(),
        Type::Fun {
            args: vec![Type::tuple2(gen_t.clone(), gen_y.clone())],
            bounds: vec![],
            ret: gen_t.clone().into(),
            id: TypeId::unset(),
        }
        .to_bounded_with_generics(vec!["T".to_string(), "Y".to_string()]),
    );

    gs.add_builtin_value(
        "snd".into(),
        Type::Fun {
            args: vec![Type::tuple2(gen_t, gen_y.clone())],
            bounds: vec![],
            ret: gen_y.into(),
            id: TypeId::unset(),
        }
        .to_bounded_with_generics(vec!["T".to_string(), "Y".to_string()]),
    );

    gs.add_builtin_value(
        "print".into(),
        Type::Fun {
            args: vec![Type::string()],
            bounds: vec![],
            ret: Type::unit().into(),
            id: TypeId::unset(),
        }
        .to_bounded(),
    );

    gs.add_builtin_value(
        "rand".into(),
        Type::Fun {
            args: vec![],
            bounds: vec![],
            ret: Type::int().into(),
            id: TypeId::unset(),
        }
        .to_bounded(),
    );

    gs.add_builtin_value(
        "print_and_stuff".into(),
        Type::Fun {
            args: vec![],
            bounds: vec![],
            ret: Type::int().into(),
            id: TypeId::unset(),
        }
        .to_bounded(),
    );
}
*/
