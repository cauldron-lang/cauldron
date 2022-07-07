use std::{iter::Peekable, slice::Iter};

use crate::lexer;

#[derive(Debug, PartialEq, Clone)]
struct Error {
    message: String,
}

#[derive(Debug, PartialEq, Clone)]
enum PrefixOperator {
    Minus,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum InfixOperator {
    Minus,
}

#[derive(Debug, PartialEq, Clone)]
enum Expression {
    Integer(String),
    Prefix(PrefixOperator, Box<Expression>),
    Infix(InfixOperator, Box<Expression>, Box<Expression>),
    Invalid(Error),
}

#[derive(Debug, PartialEq, Clone)]
enum Condition {
    Expression(Expression),
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
    tokens: Peekable<Iter<'a, lexer::Token>>,
}

impl<'a> Parser<'a> {
    fn new(tokens: Iter<'a, lexer::Token>) -> Self {
        Self {
            tokens: tokens.peekable(),
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
        let peek_token = self.tokens.peek();
        let statement = match (current_token, peek_token) {
            (lexer::Token::Identifier(name), Some(&&lexer::Token::Operator('='))) => {
                self.tokens.next();
                let next_token = self.tokens.next();

                self.parse_assignment_statement(name.clone(), next_token)
            }
            (lexer::Token::Delimiter('{'), _) => {
                let next_token = self.tokens.next();

                self.parse_block_statement(next_token)
            }
            (lexer::Token::Keyword(_if), _) if _if == "if" => {
                let next_token = self.tokens.next();

                self.parse_if_statement(next_token)
            }
            // (lexer::Token::Operator('-'), _) => Statement::Expression(
            //     self.parse_prefix_expression(PrefixOperator::Minus, next_token),
            // ),
            _ => Statement::Expression(self.parse_expression(Some(current_token))),
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

    fn parse_prefix_expression(
        &mut self,
        prefix_operator: PrefixOperator,
        current_token: Option<&lexer::Token>,
    ) -> Expression {
        Expression::Prefix(
            prefix_operator,
            Box::new(self.parse_expression(current_token)),
        )
    }

    fn get_precedence(&self, infix_operator: InfixOperator) -> u8 {
        // LOWEST
        // EQUALS
        // LESSGREATER
        // SUM
        // PRODUCT
        // PREFIX
        // CALL
        match infix_operator {
            InfixOperator::Minus => 3,
            _ => 0,
        }
    }

    fn parse_infix_expression(
        &mut self,
        left_expression: Expression,
        infix_operator: InfixOperator,
    ) -> Expression {
        let precedence = self.get_precedence(infix_operator);
        let next_token = self.tokens.next();
        let right_expression = match next_token {
            Some(token) => self.parse_expression_with_precedence(token, precedence),
            None => {
                self.error_expression(String::from("Missing token while parsing infix expression"))
            }
        };

        Expression::Infix(
            infix_operator,
            Box::new(left_expression),
            Box::new(right_expression),
        )
    }

    fn is_peek_semicolon(&mut self) -> bool {
        self.tokens.peek() != Some(&&lexer::Token::Delimiter(';'))
    }

    fn peek_precedence(&mut self) -> u8 {
        let token = self.tokens.peek();

        match token {
            Some(&&lexer::Token::Operator('-')) => 3,
            _ => 0,
        }
    }

    fn parse_expression_with_precedence(
        &mut self,
        current_token: &lexer::Token,
        precedence: u8,
    ) -> Expression {
        let mut left_expression = match current_token {
            lexer::Token::Operator('-') => {
                let next_token = self.tokens.next();
                let right_expression = match next_token {
                    Some(token) => self.parse_expression_with_precedence(token, 3),
                    None => self
                        .error_expression(String::from("Missing token while parsing expression")),
                };

                Expression::Prefix(PrefixOperator::Minus, Box::new(right_expression))
            }
            lexer::Token::Integer(integer) => Expression::Integer(integer.clone()),
            lexer::Token::Operator(_) => todo!(),
            lexer::Token::Delimiter(_) => todo!(),
            lexer::Token::Identifier(_) => todo!(),
            lexer::Token::Keyword(_) => todo!(),
            lexer::Token::Illegal => todo!(),
        };

        while self.is_peek_semicolon() && precedence < self.peek_precedence() {
            let peek_token = self.tokens.peek();

            match peek_token {
                Some(&&lexer::Token::Operator('-')) => {
                    self.tokens.next();
                    left_expression =
                        self.parse_infix_expression(left_expression, InfixOperator::Minus);
                }
                _ => break,
            };
        }

        left_expression
    }

    fn parse_expression(&mut self, current_token: Option<&lexer::Token>) -> Expression {
        match current_token {
            Some(token) => self.parse_expression_with_precedence(token, 0),
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
    use crate::{
        lexer::tokenize,
        parser::{Expression, InfixOperator, PrefixOperator},
    };

    #[test]
    fn it_parses_simple_prefix_operator_expressions() {
        let actual = parse(tokenize("-100"));
        let statement = Statement::Expression(Expression::Prefix(
            PrefixOperator::Minus,
            Box::new(Expression::Integer(String::from("100"))),
        ));
        let expected = Program::new(vec![statement]);

        assert_eq!(actual, expected);
    }

    #[test]
    fn it_parses_simple_infix_operator_expressions() {
        let actual = parse(tokenize("100 - 50"));
        let statement = Statement::Expression(Expression::Infix(
            InfixOperator::Minus,
            Box::new(Expression::Integer(String::from("100"))),
            Box::new(Expression::Integer(String::from("50"))),
        ));
        let expected = Program::new(vec![statement]);

        assert_eq!(actual, expected);
    }

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
