use yaipl::evaluator;
use yaipl::utils::diagnostics::resolve_span_from_src;

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
            let condition = true;
            if condition {
                let a = print(condition);
                let v = print("hello");
            } else if false {
                let a = 213; 
            } else{
                
            }
        }
        "#;
    let mut parser = Parser::new(text);
    let parse_root_module = parser.parse_root_module();
    let (idents, strings) = parser.into_interners();
    println!("{:#?}", strings);
    match parse_root_module {
        Ok(module) => {
            println!("{:#?}", module);
            let ast = Ast::new(module, idents, strings).annotate();

            println!("{:?}", ast.ast.identifiers);

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
