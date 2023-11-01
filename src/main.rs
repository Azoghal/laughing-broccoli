use clap::Parser;
use tracing::{error, info, Level};
use tracing_subscriber::FmtSubscriber;

use lalrpop_util::lalrpop_mod;

mod ast;
lalrpop_mod!(pub bassoon);

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

    let args = Args::parse();
    info!("Starting to do things, e.g. with arg_num: {}", args.arg_num);
    // Do things
    error!("Didn't do anything");
}

// #[cfg(test)]
// fn parse_expr_and_check(input: &str, output: &str) {
//     let mut errors = Vec::new();
//     assert_eq!(
//         &format!(
//             "{:?}",
//             bassoon::ExprParser::new()
//                 .parse(&mut errors, input)
//                 .unwrap()
//         ),
//         output
//     );
// }

// #[test]
// fn expr_factor_ops() {
//     parse_expr_and_check("1 + 2", "(1 + 2)");
//     parse_expr_and_check("(1 + 2)", "(1 + 2)");
//     parse_expr_and_check("3 * (1 + 2)", "(3 * (1 + 2))");
//     parse_expr_and_check("(1 + 2) * 3", "((1 + 2) * 3)");
//     parse_expr_and_check("3 * 1 + 2", "((3 * 1) + 2)");
//     parse_expr_and_check("(1 + 3 * 2)", "(1 + (3 * 2))");
//     parse_expr_and_check("1 + \"abc def\"", "(1 + \"abc def\")");
// }

// #[test]
// fn errorworks() {
//     // use lalrpop_util::ParseError;
//     let mut errors = Vec::new();
//     let expr = bassoon::ExprParser::new().parse(&mut errors, "2147483648");
//     println!("{:?}", expr);
//     assert!(expr.is_err());
//     // assert_eq!(
//     //     expr.unwrap_err(),
//     //     ParseError::User {
//     //         error: PracticeError::OutOfRange
//     //     }
//     // );
// }

// #[test]
// fn multierror() {
//     parse_expr_and_check("22 + + 3", "((22 + error) + 3)");
//     parse_expr_and_check("22 23 + 3", "(error + 3)");
// }

// #[test]
// fn string() {
//     let mut errors = Vec::new();
//     let expr = bassoon::ExprParser::new().parse(&mut errors, r#""abc def""#);
//     println!("{:?}", expr);
//     assert_eq!(&format!("{:?}", expr.unwrap()), "\"abc def\"");
// }
