use yaipl::evaluator;
use yaipl::utils::interner::Interner;
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
                // let a = "hello";
            } else if false {
                
            } else{
                
            }
        }
        "#;
    let interner = Interner::new();
    match Parser::new(text, &interner).parse_root_module() {
        Ok(module) => {
            println!("{:#?}", module);
            let ast = Ast::new(module, interner).annotate();

            println!("{:?}", ast.ast.interner);

            println!("{:#?}", evaluator::evaluate(ast));

            // println!("{:#?}", ast.table);
        }
        Err(err) => match err {
            parser::ParseError::UnexpectedToken(r) => {
                eprintln!("Error: {:?} @ {:?}", &text[r.clone()], r)
            }
            rest => eprintln!("{rest:?}"),
        },
    }
}
