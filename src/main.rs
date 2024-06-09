use std::env;
use std::fs;
use std::io;
use std::io::stdout;
use std::io::Write;
use std::path::PathBuf;

use crate::eval::env::Environment;
use crate::eval::object::{Object, Result};

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

    /*
     * Move argument pointer off the first argument since it's the path of
     * the Cauldron executable https://doc.rust-lang.org/std/env/fn.args.html
     */
    args.next();
    let subcommand = args.next().expect("Missing subcommand!");

    match subcommand.as_str() {
        "rlpl" => print_loop(SubCommand::ReadLexPrintLoop),
        "rppl" => print_loop(SubCommand::ReadParsePrintLoop),
        "repl" => print_loop(SubCommand::ReadEvaluatePrintLoop),
        "parse" => {
            let filepath = args.next().expect("Missing path to file!");

            parse_file(String::from(filepath));
        }
        "lex" => {
            let filepath = args.next().expect("Missing path to file!");

            lex_file(String::from(filepath));
        }
        "run" => {
            let filepath = args.next().expect("Missing path to file!");

            interpret_file(String::from(filepath))
        }
        subcommand => panic!("Unknown subcommand: {:?}", subcommand),
    }
}

fn interpret(code: String, mut environment: Environment) {
    let tokens = lexer::tokenize(code.as_str().trim_end());
    let ast = parser::parse(tokens);
    let result = eval::eval(ast, &mut environment);

    match result {
        Result::Void => {}
        Result::Object(Object::Error(error_message)) => panic!("{:?}", error_message),
        Result::Object(_) => {}
    };
}

fn interpret_file(file_path: String) {
    match env::current_dir() {
        Ok(cwd) => match fs::read_to_string(file_path.clone()) {
            Ok(code) => {
                let mut source_context = PathBuf::from(cwd);
                source_context.push(file_path.clone());

                let environment = Environment::new(source_context.parent().unwrap().to_path_buf());

                interpret(code, environment);
            }
            Err(error) => panic!("Unable to read file due to error: {:?}", error),
        },
        Err(error) => panic!(
            "Cannot access current working directory due to error: {:?}",
            error
        ),
    }
}

fn parse_file(file_path: String) {
    match env::current_dir() {
        Ok(cwd) => match fs::read_to_string(file_path.clone()) {
            Ok(code) => {
                let mut source_context = PathBuf::from(cwd);
                source_context.push(file_path.clone());

                println!("{:#?}", parser::parse(lexer::tokenize(code.as_str().trim_end())))
            }
            Err(error) => panic!("Unable to read file due to error: {:?}", error),
        },
        Err(error) => panic!(
            "Cannot access current working directory due to error: {:?}",
            error
        ),
    }
}

fn lex_file(file_path: String) {
    match env::current_dir() {
        Ok(cwd) => match fs::read_to_string(file_path.clone()) {
            Ok(code) => {
                let mut source_context = PathBuf::from(cwd);
                source_context.push(file_path.clone());

                println!("{:#?}", lexer::tokenize(code.as_str().trim_end()));
            }
            Err(error) => panic!("Unable to read file due to error: {:?}", error),
        },
        Err(error) => panic!(
            "Cannot access current working directory due to error: {:?}",
            error
        ),
    }
}

fn print_loop(subcommand: SubCommand) {
    match env::current_dir() {
        Ok(cwd) => {
            let mut environment = Environment::new(cwd);

            loop {
                print!("> ");
                stdout().flush().expect("Failed to write to stdout");

                let mut code = String::new();

                io::stdin()
                    .read_line(&mut code)
                    .expect("rlpl: Failed to read from stdin");
                let tokens = lexer::tokenize(code.as_str().trim_end());

                match subcommand {
                    SubCommand::ReadLexPrintLoop => println!("{:#?}", tokens),
                    SubCommand::ReadParsePrintLoop => println!("{:#?}", parser::parse(tokens)),
                    SubCommand::ReadEvaluatePrintLoop => {
                        println!("{:#?}", eval::eval(parser::parse(tokens), &mut environment))
                    }
                }
            }
        }
        Err(error) => panic!(
            "Cannot access current working directory due to error: {:?}",
            error
        ),
    }
}
