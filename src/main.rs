use clap::Parser as CLIParser;
use std::process::ExitCode;

use yaipl::cli::CLIOptions;

use yaipl;

fn main() -> ExitCode {
    let options = CLIOptions::parse();
    match yaipl::build_program(options) {
        Ok(_) => ExitCode::SUCCESS,
        Err(message) => {
            eprintln!("{message}");
            ExitCode::FAILURE
        }
    }
}
