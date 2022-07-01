use std::env;
use std::io;
use std::io::stdout;
use std::io::Write;

pub mod lexer;
pub mod parser;

fn main() {
    let tokens = lexer::tokenize("let a = 1");
    let program = parser::parse(tokens);
    println!("PROGRAM: {:?}", program);

    let mut args = env::args();
    let subcommand = args.nth(1).expect("Missing subcommand!");

    match subcommand.as_str() {
        "rlpl" => rlpl(),
        "rppl" => rppl(),
        _ => println!("Unknown subcommand provided {}", subcommand),
    }
}

fn rlpl() {
    let mut code = String::new();

    loop {
        print!("> ");
        stdout().flush().expect("Failed to write to stdout");
        io::stdin()
            .read_line(&mut code)
            .expect("rlpl: Failed to read from stdin");
        let tokens = lexer::tokenize(code.as_str());
        print!("{:?}", tokens);
    }
}

fn rppl() {
    let mut code = String::new();

    loop {
        print!("> ");
        stdout().flush().expect("Failed to write to stdout");
        io::stdin()
            .read_line(&mut code)
            .expect("rppl: Failed to read from stdin");
        match code {
            _ => println!("rppl: Failed to parse code from stdin"),
        }
    }
}
