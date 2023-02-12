use crate::ast;
use crate::global_state;
use crate::infer;
use crate::type_::Type;
use std::collections::HashSet;

/// Initialize types and functions only used in tests

pub fn init(instance: &mut infer::Infer) {
    let source = include_str!("../test/prelude.txt");
    let source = format!("{{ {} }}", source); // turn it into a block
    let e = syn::parse_str::<syn::Expr>(&source).unwrap();
    let expr = ast::Expr::from_expr(e).unwrap();

    populate_prelude(&mut instance.gs);
    let (_, errors, _) = instance.infer_expr_with_error(&expr, Type::unit());

    if !errors.is_empty() {
        panic!(
            "Got error while initializing prelude {:#?}",
            errors.first().unwrap(),
        );
    }
}

pub fn populate_prelude(gs: &mut global_state::GlobalState) {
    let gen_t = Type::generic("T");
    let gen_y = Type::generic("Y");

    // junk
    gs.add_effect("print".into());
    gs.add_effect("rand".into());

    gs.add_builtin_value(
        "inc".into(),
        Type::Fun {
            args: vec![Type::int()],
            bounds: vec![],
            ret: Type::int().into(),
            fx: Default::default(),
        }
        .to_bounded(),
    );

    gs.add_builtin_value(
        "dec".into(),
        Type::Fun {
            args: vec![Type::int()],
            bounds: vec![],
            ret: Type::int().into(),
            fx: Default::default(),
        }
        .to_bounded(),
    );

    gs.add_builtin_value(
        "sum".into(),
        Type::Fun {
            args: vec![Type::int(), Type::int()],
            bounds: vec![],
            ret: Type::int().into(),
            fx: Default::default(),
        }
        .to_bounded(),
    );

    gs.add_builtin_value(
        "hello".into(),
        Type::Fun {
            args: vec![],
            bounds: vec![],
            ret: Type::string().into(),
            fx: Default::default(),
        }
        .to_bounded(),
    );

    gs.add_builtin_value(
        "list_push".into(),
        Type::Fun {
            args: vec![Type::list(gen_t.clone()), gen_t.clone()],
            bounds: vec![],
            ret: Type::list(gen_t.clone()).into(),
            fx: Default::default(),
        }
        .to_bounded_with_generics(vec!["T".to_string()]),
    );

    let map_fn = Type::Fun {
        args: vec![gen_t.clone()],
        bounds: vec![],
        ret: gen_y.clone().into(),
        fx: Default::default(),
    };

    gs.add_builtin_value(
        "list_map".into(),
        Type::Fun {
            args: vec![Type::list(gen_t.clone()), map_fn],
            bounds: vec![],
            ret: Type::list(gen_y.clone()).into(),
            fx: Default::default(),
        }
        .to_bounded_with_generics(vec!["T".to_string(), "Y".to_string()]),
    );

    gs.add_builtin_value(
        "list_head".into(),
        Type::Fun {
            args: vec![Type::list(gen_t.clone())],
            bounds: vec![],
            ret: gen_t.clone().into(),
            fx: Default::default(),
        }
        .to_bounded_with_generics(vec!["T".to_string()]),
    );

    gs.add_builtin_value(
        "int_to_string".into(),
        Type::Fun {
            args: vec![Type::int()],
            bounds: vec![],
            ret: Type::string().into(),
            fx: Default::default(),
        }
        .to_bounded(),
    );

    gs.add_builtin_value(
        "str_concat".into(),
        Type::Fun {
            args: vec![Type::string(), Type::string()],
            bounds: vec![],
            ret: Type::string().into(),
            fx: Default::default(),
        }
        .to_bounded(),
    );

    gs.add_builtin_value(
        "abs".into(),
        Type::Fun {
            args: vec![Type::int()],
            bounds: vec![],
            ret: Type::int().into(),
            fx: Default::default(),
        }
        .to_bounded(),
    );

    gs.add_builtin_value(
        "fst".into(),
        Type::Fun {
            args: vec![Type::tuple2(gen_t.clone(), gen_y.clone())],
            bounds: vec![],
            ret: gen_t.clone().into(),
            fx: Default::default(),
        }
        .to_bounded_with_generics(vec!["T".to_string(), "Y".to_string()]),
    );

    gs.add_builtin_value(
        "snd".into(),
        Type::Fun {
            args: vec![Type::tuple2(gen_t, gen_y.clone())],
            bounds: vec![],
            ret: gen_y.into(),
            fx: Default::default(),
        }
        .to_bounded_with_generics(vec!["T".to_string(), "Y".to_string()]),
    );

    gs.add_builtin_value(
        "print".into(),
        Type::Fun {
            args: vec![Type::string()],
            bounds: vec![],
            ret: Type::unit().into(),
            fx: Default::default(),
        }
        .to_bounded(),
    );

    gs.add_builtin_value(
        "rand".into(),
        Type::Fun {
            args: vec![],
            bounds: vec![],
            ret: Type::int().into(),
            fx: Default::default(),
        }
        .to_bounded(),
    );

    {
        let mut fx = HashSet::new();
        fx.insert("print".to_string());
        fx.insert("rand".to_string());

        gs.add_builtin_value(
            "print_and_stuff".into(),
            Type::Fun {
                args: vec![],
                bounds: vec![],
                ret: Type::int().into(),
                fx,
            }
            .to_bounded(),
        );
    }
}
