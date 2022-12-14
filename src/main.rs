use clap::Parser as CLIParser;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::rc::Rc;

use yaipl::cli::{CLIOptions, Command};
use yaipl::evaluator;
use yaipl::utils::diagnostics::{resolve_span_from_src, DiagnosticContext};

use yaipl::frontend::parser::{self, Parser};
use yaipl::utils::tree_pretty_printer::AstPrinter;
use yaipl::{self, frontend::parser::ast::Ast};

fn main() {
    let cli_options = CLIOptions::parse();
    match &cli_options.command {
        Command::Run { path } => run(&cli_options, path),
    }
}

fn run(options: &CLIOptions, path: &Path) {
    //FIXME: hmm mabye not leak the memory.
    let source = &*Box::leak(read_file(path).into_boxed_str());

    let diagnostics = Rc::new(DiagnosticContext::new(source));
    let mut parser = Parser::new(source);
    let parse_root_module = parser.parse_root_module();
    let (idents, strings) = parser.into_interners();

    match parse_root_module {
        Ok(module) => {
            let ast = Ast::new(module, idents, strings).annotate();

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

            println!("{:#?}", evaluator::evaluate(ast));

            // println!("{:#?}", ast.table);
        }
        Err(err) => match err {
            parser::ParseError::UnexpectedToken(r) => {
                eprintln!(
                    "UnexpectedToken: {:?} @ {:?}",
                    &source[r.into_src_range()],
                    resolve_span_from_src(&source, r)
                )
            }
            rest => eprintln!("{rest:?}"),
        },
    }
    drop(diagnostics);
}

fn read_file(path: &Path) -> String {
    let mut file = File::open(path).expect("Could not find file");
    let mut source = String::new();
    file.read_to_string(&mut source).unwrap();
    source
}
