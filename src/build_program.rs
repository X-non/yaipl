use std::{fs::File, io::Read, path::Path, rc::Rc};

use crate::{
    cli::CLIOptions,
    evaluator,
    frontend::{lexer::Token, parser::Parser, semantic_analysis::AnnotatedAst},
    utils::{diagnostics::DiagnosticContext, tree_pretty_printer::AstPrinter},
};
fn compile(options: &CLIOptions, path: &Path) -> AnnotatedAst {
    //FIXME: hmm mabye not leak the memory.
    let source = &*Box::leak(read_file(path).into_boxed_str());
    let diagnostics = Rc::new(DiagnosticContext::new(path, source));
    let mut token_trace = Vec::new();
    let mut parser = Parser::new(source);

    if options.flags.dump_tokens {
        parser.trace_tokens(&mut token_trace);
    }
    let ast = parser.parse_file_ast();

    if options.flags.dump_tokens {
        dump_tokens(&diagnostics, token_trace);
    }

    let ast = ast.unwrap_or_else(|err| diagnostics.report_parse_error(err));

    if options.flags.dump_ast {
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

fn dump_tokens(diagnostics: &DiagnosticContext, token_trace: Vec<Token>) {
    let filepath = diagnostics.filepath();
    let filepath = std::env::current_dir().map_or(filepath, |base| {
        filepath.strip_prefix(base).unwrap_or(filepath)
    });
    let filename = filepath.display();
    for token in token_trace {
        println!(
            "[ {:033}\t @ {}{} ]",
            format!("{:?}", token.kind),
            filename,
            diagnostics.resolve_span(token.span)
        );
    }
    println!();
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
