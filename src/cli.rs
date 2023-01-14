use clap::Subcommand;
use std::path::PathBuf;

#[derive(clap::Parser)]
pub struct CLIOptions {
    #[command(flatten)]
    pub flags: DebugFlags,

    #[command(subcommand)]
    pub command: Command,
}
#[derive(clap::Args)]
pub struct DebugFlags {
    #[arg(long)]
    pub dump_ast: bool,
    #[arg(long)]
    pub dump_tokens: bool,
}
#[derive(Subcommand)]
pub enum Command {
    Run { path: PathBuf },
    Check { path: PathBuf },
}
