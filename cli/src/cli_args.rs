use clap::Parser;
use std::path::PathBuf;

#[derive(Parser)]
#[command(author,version, about, long_about = None)]
pub struct Arguments {
    #[arg(short, long, default_value_t = false)]
    pub run: bool,

    #[arg(short, long)]
    pub out_file: Option<PathBuf>,

    #[arg(long = "llvm", default_value_t = true)]
    pub output_llvm: bool,

    pub file: PathBuf,
}
