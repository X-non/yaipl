#![feature(new_uninit)]
use args::Args;
use clap::Parser;
use frontend::parser;
use utils::interner::Interner;

mod args;
mod frontend;
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
        let condition = 0;
        if condition {
            let a = "hello";
        } else {

        }
        "#;
    let interner = Interner::new();
    match parser::Parser::new(text, &interner).parse_program() {
        Ok(module) => {
            println!("{:#?}", module);
        }
        Err(err) => match err {
            parser::ParseError::UnexpectedToken(r) => {
                eprintln!("Error: {:?} @ {:?}", &text[r.clone()], r)
            }
            rest => eprintln!("{rest:?}"),
        },
    }
}
