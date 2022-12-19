use clap::Parser as CLIParser;

use yaipl::cli::CLIOptions;

use yaipl;

fn main() {
    let options = CLIOptions::parse();
    yaipl::build_program(options)
}
