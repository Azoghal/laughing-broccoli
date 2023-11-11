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

#[cfg(test)]
fn parse_expr_and_check(input: &str, output: &str) {
    let mut errors = Vec::new();
    assert_eq!(
        &format!(
            "{:?}",
            bassoon::ExprParser::new()
                .parse(&mut errors, input)
                .unwrap()
        ),
        output
    );
}

#[test]
fn expr_factor_ops() {
    parse_expr_and_check("1 + 2", "(1 + 2)");
    parse_expr_and_check("(1 + 2)", "(1 + 2)");
    parse_expr_and_check("3 * (1 + 2)", "(3 * (1 + 2))");
    parse_expr_and_check("(1 + 2) * 3", "((1 + 2) * 3)");
    parse_expr_and_check("3 * 1 + 2", "((3 * 1) + 2)");
    parse_expr_and_check("(1 + 3 * 2)", "(1 + (3 * 2))");
    parse_expr_and_check("1 + \"abc def\"", "(1 + \"abc def\")");
}

#[test]
fn errorworks() {
    // use lalrpop_util::ParseError;
    let mut errors = Vec::new();
    let expr = bassoon::ExprParser::new().parse(&mut errors, "2147483648");
    println!("{:?}", expr);
    assert!(expr.is_err());
    // assert_eq!(
    //     expr.unwrap_err(),
    //     ParseError::User {
    //         error: PracticeError::OutOfRange
    //     }
    // );
}

#[test]
fn multierror() {
    parse_expr_and_check("22 + + 3", "((22 + error) + 3)");
    parse_expr_and_check("22 23 + 3", "(error + 3)");
}

#[test]
fn string() {
    let mut errors = Vec::new();
    let expr = bassoon::ExprParser::new().parse(&mut errors, r#""abc def""#);
    println!("{:?}", expr);
    assert_eq!(&format!("{:?}", expr.unwrap()), "\"abc def\"");
}

#[test]
fn unary() {
    parse_expr_and_check("1", "1");
    parse_expr_and_check("!1", "!(1)");
    parse_expr_and_check("1!", "(1)!");
    parse_expr_and_check("5-1!", "(5 - (1)!)");
    parse_expr_and_check("!1!", "(!(1))!");
}

#[cfg(test)]
fn parse_statement_and_check(input: &str, output: &str) {
    let mut errors = Vec::new();
    assert_eq!(
        &format!(
            "{:?}",
            bassoon::StatementParser::new()
                .parse(&mut errors, input)
                .unwrap()
        ),
        output
    );
}

#[test]
fn statement() {
    // TODO extend these as more statements added
    parse_statement_and_check("bob = 3;", "bob = 3;")
}

#[cfg(test)]
fn parse_statements_and_check(input: &str, output: &str) {
    let mut errors = Vec::new();
    assert_eq!(
        &format!(
            "{:?}",
            bassoon::StatementsParser::new()
                .parse(&mut errors, input)
                .unwrap()
        ),
        output
    );
}

#[test]
fn statements() {
    // TODO add tests that poorly formatted sequence of statements fails.
    parse_statements_and_check("", "[]");
    parse_statements_and_check("bob = 3;", "[bob = 3;]");
    parse_statements_and_check("bob = 3; bill = 4;", "[bob = 3;, bill = 4;]");
}

#[cfg(test)]
fn parse_codeblock_and_check(input: &str, output: &str) {
    let mut errors = Vec::new();
    assert_eq!(
        &format!(
            "{:?}",
            bassoon::CodeBlockParser::new()
                .parse(&mut errors, input)
                .unwrap()
        ),
        output
    );
}
#[test]
fn codeblocks() {
    // TODO impl fmt for ASTCodeBlock
    parse_codeblock_and_check("{bob = 1; bill=2;}", "{ [bob = 1;, bill = 2;] }")
}

#[test]
fn ifs() {
    parse_statement_and_check("if 2 {bob=3;}", "if 2 { [bob = 3;] } else None");
    parse_statement_and_check(
        "if 2 {bob=3;} else {bill=4;}",
        "if 2 { [bob = 3;] } else Some({ [bill = 4;] })",
    );
    parse_statement_and_check(
        "if 2 {bob=3;} else if 3 {bill=4;} else {jill=4;}",
        "if 2 { [bob = 3;] } else if 3 { [bill = 4;] } else Some({ [jill = 4;] })",
    );
    parse_statement_and_check(
        "if 2 {bob=3;} else if 3 {bill=4;} else if 4 {jill=4;} else {jerome=5;}",
        "if 2 { [bob = 3;] } else if 3 { [bill = 4;] } else if 4 { [jill = 4;] } else Some({ [jerome = 5;] })",
    );
    parse_statement_and_check(
        "if 2 {bob=3;} else if 3 {bill=4;} else if 4 {jill=4;}",
        "if 2 { [bob = 3;] } else if 3 { [bill = 4;] } else if 4 { [jill = 4;] } else None",
    );
}

#[test]
fn whiles() {
    parse_statement_and_check("while 3 { bob = 4; }", "while (3) { [bob = 4;] }")
}

#[test]
fn fors() {
    parse_statement_and_check(
        "for (i=3; i<5; i=i+1) { bob = 4; }",
        "for ( i = 3; ; (i < 5) ; i = (i + 1); ) { [bob = 4;] }",
    )
}
