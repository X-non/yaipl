use std::{fs::File, io::Read, path::Path, rc::Rc};

use crate::{
    cli::CLIOptions,
    evaluator,
    frontend::parser::{self, ast::Ast, Parser},
    utils::{
        diagnostics::{resolve_span_from_src, DiagnosticContext},
        tree_pretty_printer::AstPrinter,
    },
};

// pub fn check(options: &CLIOptions, path: &Path) -> Result<(), ()> {}

pub fn run(options: &CLIOptions, path: &Path) {
    let source = &*Box::leak(read_file(path).into_boxed_str());

    //FIXME: hmm mabye not leak the memory.
    let diagnostics = Rc::new(DiagnosticContext::new(source));

    let ast = Parser::new(source).parse_file_ast();

    let ast = match ast {
        Ok(ast) => ast,
        Err(err) => diagnostics.report_parse_error(err),
    };

    let ast = ast.annotate();
    if options.dump_ast {
        println!(
            "{}",
            AstPrinter::from_node(
                &ast.ast,
                ast.ast.identifiers.clone(),
                ast.ast.strings.clone(),
                diagnostics.clone()
            )
        );
    }

    evaluator::evaluate(ast).unwrap();
}

fn read_file(path: &Path) -> String {
    let mut file = File::open(path).expect("Could not find file");
    let mut source = String::new();
    file.read_to_string(&mut source).unwrap();
    source
}
