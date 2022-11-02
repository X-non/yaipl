#![feature(new_uninit)]
#![feature(associated_type_defaults)]
use args::Args;

use frontend::parser;
use utils::interner::Interner;

use crate::frontend::parser::ast::Ast;

mod args;
mod evaluator;
pub mod frontend;

mod utils;

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
            let condition = 2;
            if condition {
                let a = "hello";
            } else if "Hello" {
                
            } else{
                
            }
        }
        "#;
    let interner = Interner::new();
    match parser::Parser::new(text, &interner).parse_root_module() {
        Ok(module) => {
            println!("{:#?}", module);
            let ast = Ast::new(module, interner).annotate();
            evaluator::evaluate(ast);
            // println!("{:#?}", ast.table);
            // ast.ast.interner.debug_dump_strs()
        }
        Err(err) => match err {
            parser::ParseError::UnexpectedToken(r) => {
                eprintln!("Error: {:?} @ {:?}", &text[r.clone()], r)
            }
            rest => eprintln!("{rest:?}"),
        },
    }
}
