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

pub fn run(options: &CLIOptions, path: &Path) -> Result<(), String> {
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
            Ok(())
        }
        Err(err) => match err {
            parser::ParseError::UnexpectedToken(r) => Err(format!(
                "UnexpectedToken: {:?} @ {:?}",
                &source[r.into_src_range()],
                resolve_span_from_src(&source, r)
            )),
            rest => Err(format!("{rest:?}")),
        },
    }
}

fn read_file(path: &Path) -> String {
    let mut file = File::open(path).expect("Could not find file");
    let mut source = String::new();
    file.read_to_string(&mut source).unwrap();
    source
}
