use std::slice::Iter;

use crate::lexer;

#[derive(Debug, PartialEq, Clone)]
enum Token {
    Integer(String),
}

#[derive(Debug, PartialEq, Clone)]
enum Statement {
    Assignment(String, Box<Statement>),
    Expression(Token),
    InvalidExpression,
}

#[derive(Debug, PartialEq, Clone)]
struct Error {
    message: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    statements: Vec<Statement>,
    errors: Vec<Error>,
}

impl Program {
    fn new(statements: Vec<Statement>, errors: Vec<Error>) -> Self {
        Self { statements, errors }
    }
}

pub struct Parser<'a> {
    errors: Vec<Error>,
    statements: Vec<Statement>,
    tokens: Iter<'a, lexer::Token>,
}

impl<'a> Parser<'a> {
    fn new(tokens: Iter<'a, lexer::Token>) -> Self {
        Self {
            errors: Vec::new(),
            statements: Vec::new(),
            tokens: tokens,
        }
    }

    fn parse_statements(&mut self) {
        loop {
            match self.tokens.next() {
                Some(current_token) => self.parse_statement(current_token),
                None => break,
            };
        }
    }

    fn parse_statement(&mut self, current_token: &lexer::Token) {
        let next_token = self.tokens.next();

        match current_token {
            lexer::Token::Keyword(keyword) if keyword == "let" => {
                self.parse_assignment(next_token);
            }
            _ => {
                let expression = self.parse_expression(next_token);
                self.statements.push(expression);
            }
        };
    }

    fn parse_assignment(&mut self, current_token: Option<&lexer::Token>) {
        match current_token {
            Some(lexer::Token::Identifier(identifier)) => {
                self.parse_assignment_operator(identifier.clone())
            }
            Some(token) => self.error(format!(
                "Expected identifier after 'let' keyword in assignment statement recieved: {:?}",
                token
            )),
            None => self.error(String::from(
                "Missing identifier after 'let' keyword in assignment statement",
            )),
        };
    }

    fn parse_assignment_operator(&mut self, identifier: String) {
        match self.tokens.next() {
            Some(lexer::Token::Operator('=')) => {
                let next_token = self.tokens.next();

                self.parse_assignment_body(identifier, next_token);
            }
            Some(token) => self.error(format!(
                "Expected assignment operator after identifier in assignment \
            statement, recieved: {:?}",
                token
            )),
            None => self.error(String::from(
                "Missing assignment operator after identifier in assignment statement",
            )),
        }
    }

    fn parse_assignment_body(&mut self, identifier: String, current_token: Option<&lexer::Token>) {
        match current_token {
            None | Some(lexer::Token::Delimiter(_)) => self.error(String::from(
                "Expected expression after assignment operator in assignment statement",
            )),
            _ => {
                let expression = self.parse_expression(current_token);
                let assignment_statement = Statement::Assignment(identifier, Box::new(expression));

                self.statements.push(assignment_statement);
            }
        }
    }

    fn parse_expression(&mut self, current_token: Option<&lexer::Token>) -> Statement {
        match current_token {
            Some(lexer::Token::Integer(integer)) => {
                Statement::Expression(Token::Integer(integer.clone()))
            }
            Some(token) => {
                self.error(format!(
                    "Invalid token while parsing expression: {:?}",
                    token
                ));
                Statement::InvalidExpression
            }
            None => {
                self.error(String::from("Missing token while parsing expression"));
                Statement::InvalidExpression
            }
        }
    }

    fn error(&mut self, message: String) {
        self.errors.push(Error { message });
        self.seek_delimiter();
    }

    fn seek_delimiter(&mut self) {
        loop {
            match self.tokens.next() {
                Some(lexer::Token::Delimiter(_)) => break,
                None => break,
                _ => continue,
            }
        }
    }
}

pub fn parse(tokens: lexer::Tokens) -> Program {
    let mut parser = Parser::new(tokens.iter());
    parser.parse_statements();

    Program::new(parser.statements, parser.errors)
}

#[cfg(test)]
mod tests {
    use super::{parse, Program, Statement, Token};
    use crate::lexer::tokenize;

    #[test]
    fn it_parses_simple_assignment() {
        let actual = parse(tokenize("let a = 1"));
        let statement = Statement::Assignment(
            String::from("a"),
            Box::new(Statement::Expression(Token::Integer(String::from("1")))),
        );
        let expected = Program::new(vec![statement], Vec::new());

        assert_eq!(actual, expected);
    }
}
