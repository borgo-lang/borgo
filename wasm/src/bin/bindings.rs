/// Generate serializer/deserializer for `Expr` and other data types so they can be used in Go.
use serde_reflection::{Tracer, TracerConfig};
use std::io::Write;

use compiler::ast;
use compiler::error;
use compiler::project;
use compiler::type_;

fn main() {
    let mut tracer = Tracer::new(TracerConfig::default());

    tracer.trace_simple_type::<ast::DebugKind>().unwrap();
    tracer.trace_simple_type::<ast::ExternKind>().unwrap();
    tracer.trace_simple_type::<ast::FunctionKind>().unwrap();
    tracer.trace_simple_type::<ast::Literal>().unwrap();
    tracer.trace_simple_type::<ast::Operator>().unwrap();
    tracer.trace_simple_type::<ast::TypeAst>().unwrap();
    tracer.trace_simple_type::<ast::UnOp>().unwrap();
    tracer.trace_simple_type::<ast::Pat>().unwrap();
    tracer.trace_simple_type::<ast::Expr>().unwrap();
    tracer.trace_simple_type::<ast::File>().unwrap();
    tracer.trace_simple_type::<ast::Loop>().unwrap();
    tracer.trace_simple_type::<ast::LoopFlow>().unwrap();

    tracer.trace_simple_type::<type_::Type>().unwrap();

    tracer.trace_simple_type::<project::Project>().unwrap();
    tracer.trace_simple_type::<project::Package>().unwrap();
    tracer.trace_simple_type::<error::Error>().unwrap();

    let registry = tracer.registry().unwrap();

    let mut source = Vec::new();
    let config = serde_generate::CodeGeneratorConfig::new("borgo".to_string())
        .with_encodings(vec![serde_generate::Encoding::Bincode]);

    let generator = serde_generate::golang::CodeGenerator::new(&config);

    generator.output(&mut source, &registry).unwrap();

    // Save bindings file
    let filename = "./runtime/runtime/bindings.go";
    let mut file = std::fs::File::create(filename).unwrap();
    file.write_all(&source).unwrap();

    // Run gofmt
    std::process::Command::new("go")
        .arg("fmt")
        .arg(filename)
        .output()
        .unwrap();
}
