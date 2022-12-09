use std::rc::Rc;

use yaipl::evaluator;
use yaipl::utils::diagnostics::{resolve_span_from_src, DiagnosticContext};

use yaipl::utils::tree_pretty_printer::AstPrinter;
use yaipl::{self, frontend::parser::ast::Ast};

use yaipl::frontend::parser::{self, Parser};

fn main() {
    // let args = Args::parse();
    // let mut file = match File::open(args.file) {
    //     Ok(a) => a,
    //     Err(err) => panic!("{}", err),
    // };
    // let mut text = String::new();
    // file.read_to_string(&mut text).unwrap();
    let text = r#"
        fn main {
            let a = "outer";
            { 
                let a = "inner";
                print(a);
            }
            print(a);
        }
        "#;

    let mut parser = Parser::new(text);
    let parse_root_module = parser.parse_root_module();
    let (idents, strings) = parser.into_interners();
    let diagnostics = Rc::new(DiagnosticContext::new(text));
    println!("{:#?}", strings);
    match parse_root_module {
        Ok(module) => {
            let ast = Ast::new(module, idents, strings);
            println!(
                "{}",
                AstPrinter::from_node(
                    &ast,
                    ast.identifiers.clone(),
                    ast.strings.clone(),
                    diagnostics.clone()
                )
            );
            let ast = ast.annotate();
            println!("{:#?}", ast.ast.identifiers);
            println!("{:#?}", ast.ast.strings);

            println!("{:#?}", evaluator::evaluate(ast));

            // println!("{:#?}", ast.table);
        }
        Err(err) => match err {
            parser::ParseError::UnexpectedToken(r) => {
                eprintln!(
                    "UnexpectedToken: {:?} @ {:?}",
                    &text[r.into_src_range()],
                    resolve_span_from_src(text, r)
                )
            }
            rest => eprintln!("{rest:?}"),
        },
    }
}
