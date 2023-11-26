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

    info!("Doing some parsing");
    if let Err(err) = parser::parse_expr("1 + 2") {
        error!("Done failed to parse an expression {err}");
    };

    info!("Doing some codegen-ing");
    if let Err(err) = codegen::codeo_geneo() {
        error!("Done failed to code gen {err}");
    };
}
