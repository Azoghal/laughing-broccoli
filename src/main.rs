use clap::Parser;
use tracing::{error, info, Level};
use tracing_subscriber::FmtSubscriber;

mod ast;
mod codegen;
mod parser;

#[derive(Parser)]
#[command(author, version, about, long_about=None)]
// #[command(propagate_version = true)]
struct Args {
    #[arg(default_value_t = 10)]
    arg_num: u8,
}

fn main() {
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::TRACE)
        .finish();
    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");

    let _args = Args::parse();

    let prog_string = "define bob() as {b of int = 2 + 1;}";

    info!("Parsing {prog_string}");

    match parser::parse_function(prog_string) {
        Err(err) => {
            error!("Failed to parse: {err}");
        }
        Ok(func) => {
            info!("Codegen-ing");
            if let Err(err) = codegen::emit(*func) {
                error!("Failed to code gen {err}");
            };
        }
    };
}
