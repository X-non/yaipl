use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
pub struct CLIOptions {
    #[arg(long)]
    pub dump_ast: bool,

    #[command(subcommand)]
    pub command: Command,
}

#[derive(Subcommand)]
pub enum Command {
    Run { path: PathBuf },
}
