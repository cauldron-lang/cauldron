use std::env;
use std::io;
use std::io::stdout;
use std::io::Write;

use crate::eval::Environment;

pub mod eval;
pub mod lexer;
pub mod parser;

enum SubCommand {
    ReadLexPrintLoop,
    ReadParsePrintLoop,
    ReadEvaluatePrintLoop,
}

fn main() {
    let mut args = env::args();
    let subcommand = args.nth(1).expect("Missing subcommand!");

    match subcommand.as_str() {
        "rlpl" => print_loop(SubCommand::ReadLexPrintLoop),
        "rppl" => print_loop(SubCommand::ReadParsePrintLoop),
        "repl" => print_loop(SubCommand::ReadEvaluatePrintLoop),
        _ => println!("Unknown subcommand provided {}", subcommand),
    }
}

fn print_loop(subcommand: SubCommand) {
    let mut environment = Environment::new();

    loop {
        print!("> ");
        stdout().flush().expect("Failed to write to stdout");

        let mut code = String::new();

        io::stdin()
            .read_line(&mut code)
            .expect("rlpl: Failed to read from stdin");
        let tokens = lexer::tokenize(code.as_str().trim_end());

        match subcommand {
            SubCommand::ReadLexPrintLoop => println!("{:?}", tokens),
            SubCommand::ReadParsePrintLoop => println!("{:?}", parser::parse(tokens)),
            SubCommand::ReadEvaluatePrintLoop => {
                println!("{:?}", eval::eval(parser::parse(tokens), &mut environment))
            }
        }
    }
}
