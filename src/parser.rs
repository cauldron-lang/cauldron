use std::{slice::Iter, thread::current};

use crate::lexer;

#[derive(Debug, PartialEq, Clone)]
struct Error {
    message: String,
}

#[derive(Debug, PartialEq, Clone)]
enum Expression {
    Integer(String),
    Invalid(Error),
}

#[derive(Debug, PartialEq, Clone)]
enum Condition {
    Expression(Expression),
    Invalid(Error),
}

#[derive(Debug, PartialEq, Clone)]
enum Block {
    Statements(Vec<Statement>),
    Invalid(Error),
}

#[derive(Debug, PartialEq, Clone)]
enum Statement {
    Assignment(String, Expression),
    Block(Block),
    Expression(Expression),
    Conditional(Condition, Block),
    Invalid(Error),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    fn new(statements: Vec<Statement>) -> Self {
        Self { statements }
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

    fn parse_statements(&mut self) -> Vec<Statement> {
        let mut statements: Vec<Statement> = Vec::new();

        loop {
            match self.tokens.next() {
                Some(current_token) => statements.push(self.parse_statement(current_token)),
                None => break,
            };
        }

        statements
    }

    fn parse_statement(&mut self, current_token: &lexer::Token) -> Statement {
        let next_token = self.tokens.next();
        let statement = match (current_token, next_token) {
            (lexer::Token::Identifier(name), Some(lexer::Token::Operator('='))) => {
                let next_token = self.tokens.next();

                self.parse_assignment_statement(name.clone(), next_token)
            }
            (lexer::Token::Delimiter('{'), _) => self.parse_block_statement(next_token),
            (lexer::Token::Keyword(_if), _) if _if == "if" => self.parse_if_statement(next_token),
            _ => Statement::Expression(self.parse_expression(next_token)),
        };

        statement
    }

    fn parse_block_statement(&mut self, current_token: Option<&lexer::Token>) -> Statement {
        Statement::Block(self.parse_block(current_token))
    }

    fn parse_block(&mut self, current_token: Option<&lexer::Token>) -> Block {
        let mut statements: Vec<Statement> = Vec::new();
        let mut token = current_token;

        loop {
            match token {
                Some(lexer::Token::Delimiter('}')) => break,
                Some(lexer::Token::Delimiter(';')) => {}
                Some(token) => {
                    let statement = self.parse_statement(token);
                    statements.push(statement);
                }
                None => {
                    statements.push(self.error_statement(String::from("Missing remaining block")));
                    break;
                }
            };

            token = self.tokens.next();
        }

        Block::Statements(statements)
    }

    fn parse_if_statement(&mut self, current_token: Option<&lexer::Token>) -> Statement {
        match current_token {
            Some(lexer::Token::Delimiter('(')) => {
                let next_token = self.tokens.next();
                let condition = Condition::Expression(self.parse_expression(next_token));
                let next_token = self.tokens.next();
                let consequence = match next_token {
                    Some(lexer::Token::Delimiter('{')) => {
                        let next_token = self.tokens.next();

                        self.parse_block(next_token)
                    }
                    Some(invalid_token) => self.error_block(format!(
                        "Invalid token while parsing 'if' statement's consequence: {:?}",
                        invalid_token
                    )),
                    None => self.error_block(String::from("Missing 'if' statement's consequence")),
                };

                Statement::Conditional(condition, consequence)
            }
            Some(invalid_token) => self.error_statement(format!(
                "Invalid token while parsing 'if' statement's condition: {:?}",
                invalid_token
            )),
            None => self.error_statement(String::from(
                "Missing condition and consequence of 'if' statement",
            )),
        }
    }

    fn parse_assignment_statement(
        &mut self,
        identifier: String,
        current_token: Option<&lexer::Token>,
    ) -> Statement {
        match current_token {
            None => self.error_statement(String::from(
                "Expected expression after assignment operator in assignment statement",
            )),
            _ => {
                let expression = self.parse_expression(current_token);
                Statement::Assignment(identifier, expression)
            }
        }
    }

    fn parse_expression(&mut self, current_token: Option<&lexer::Token>) -> Expression {
        match current_token {
            Some(lexer::Token::Integer(integer)) => Expression::Integer(integer.clone()),
            Some(token) => self.error_expression(format!(
                "Invalid token while parsing expression: {:?}",
                token
            )),
            None => self.error_expression(String::from("Missing token while parsing expression")),
        }
    }

    fn error_statement(&mut self, message: String) -> Statement {
        self.seek_delimiter();

        Statement::Invalid(Error { message })
    }

    fn error_block(&mut self, message: String) -> Block {
        self.seek_delimiter();

        Block::Invalid(Error { message })
    }

    fn error_expression(&mut self, message: String) -> Expression {
        self.seek_delimiter();

        Expression::Invalid(Error { message })
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
    let statements = parser.parse_statements();

    Program::new(statements)
}

#[cfg(test)]
mod tests {
    use super::{parse, Block, Program, Statement};
    use crate::{lexer::tokenize, parser::Expression};

    #[test]
    fn it_parses_simple_assignment() {
        let actual = parse(tokenize("a = 1"));
        let statement =
            Statement::Assignment(String::from("a"), Expression::Integer(String::from("1")));
        let expected = Program::new(vec![statement]);

        assert_eq!(actual, expected);
    }

    #[test]
    fn it_parses_simple_blocks() {
        let actual = parse(tokenize("{ a = 1 }"));
        let actual_with_semicolon = parse(tokenize("{ a = 1; }"));
        let assignment_statement =
            Statement::Assignment(String::from("a"), Expression::Integer(String::from("1")));
        let block_statement = Statement::Block(Block::Statements(vec![assignment_statement]));
        let expected = Program::new(vec![block_statement]);

        assert_eq!(actual, expected);
        assert_eq!(actual_with_semicolon, expected);
    }
}
