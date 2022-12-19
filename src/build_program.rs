use std::{fs::File, io::Read, path::Path, rc::Rc};

use crate::{
    cli::CLIOptions,
    evaluator,
    frontend::{parser::Parser, semantic_analysis::AnnotatedAst},
    utils::{diagnostics::DiagnosticContext, tree_pretty_printer::AstPrinter},
};
fn compile(options: &CLIOptions, path: &Path) -> AnnotatedAst {
    //FIXME: hmm mabye not leak the memory.
    let source = &*Box::leak(read_file(path).into_boxed_str());
    let diagnostics = Rc::new(DiagnosticContext::new(source));

    let ast = Parser::new(source).parse_file_ast();

    let ast = match ast {
        Ok(ast) => ast,
        Err(err) => diagnostics.report_parse_error(err),
    };
    if options.dump_ast {
        println!(
            "{}",
            AstPrinter::from_node(
                &ast,
                ast.identifiers.clone(),
                ast.strings.clone(),
                diagnostics.clone()
            )
        );
    }

    ast.annotate()
}

pub fn check(options: &CLIOptions, path: &Path) {
    compile(options, path);
}

pub fn run(options: &CLIOptions, path: &Path) {
    let ast = compile(options, path);

    evaluator::evaluate(ast).unwrap();
}

fn read_file(path: &Path) -> String {
    let mut file = File::open(path).expect("Could not find file");
    let mut source = String::new();
    file.read_to_string(&mut source).unwrap();
    source
}
