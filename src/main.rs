use args::Args;
use clap::Parser;
use frontend::parser;

mod args;
mod frontend;

fn main() {
    // let args = Args::parse();
    // let mut file = match File::open(args.file) {
    //     Ok(a) => a,
    //     Err(err) => panic!("{}", err),
    // };
    // let mut text = String::new();
    // file.read_to_string(&mut text).unwrap();
    let text = "if 1 {} else {}";

    match parser::Parser::new(text).parse_program() {
        Ok(module) => {
            println!("{:#?}", module)
        }
        Err(err) => {
            eprintln!("Error");
            match err {
                parser::ParseError::UnexpectedToken(r) => {
                    eprintln!("{:?} @ {:?}", &text[r.clone()], r)
                }
                rest => eprintln!("{rest:?}"),
            }
        }
    }
}
