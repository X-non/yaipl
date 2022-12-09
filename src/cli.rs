use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser, Debug)]
pub struct CLIOptions {
    #[arg(long)]
    pub dump_ast: bool,

    #[command(cubcommand)]
    comman: Command,
}

#[derive(Subcommand)]
pub enum Command {
    Run { pub file: PathBuf },
}
