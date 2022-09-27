use std::{fs::File, io::Read};

use args::Args;
use clap::Parser;

mod args;
mod frontend;

fn main() {
    let args = Args::parse();
    let mut file = match File::open(args.file) {
        Ok(a) => a,
        Err(err) => panic!("{}", err),
    };
    let mut text = String::new();
    file.read_to_string(&mut text).unwrap();
    println!("{}", text);
}
