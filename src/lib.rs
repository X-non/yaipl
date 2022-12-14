#![feature(new_uninit)]
#![feature(associated_type_defaults)]

use cli::CLIOptions;

mod build_program;
pub mod cli;
pub mod evaluator;
pub mod frontend;
pub mod utils;

pub fn build_program(options: CLIOptions) -> Result<(), String> {
    match &options.command {
        cli::Command::Run { path } => build_program::run(&options, path),
    }
}
