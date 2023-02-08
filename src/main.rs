use clap::Parser as CLIParser;

use yaipl::cli::CLIOptions;

fn main() {
    let options = CLIOptions::parse();
    yaipl::build_program(&options)
}
