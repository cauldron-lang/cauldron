use std::{iter::Peekable, slice::Iter};

use crate::lexer;

// LOWEST
// EQUALS ==
// LESSGREATER > or <
// SUM + or -
// PRODUCT *
// PREFIX -X or !X
// CALL my_function(X)
const PRECEDENCE_LOWEST: u8 = 0;
const PRECEDENCE_EQUALS: u8 = 1;
const PRECEDENCE_LESSGREATER: u8 = 2;
const PRECEDENCE_SUM: u8 = 3;
const PRECEDENCE_PRODUCT: u8 = 4;
const PRECEDENCE_PREFIX: u8 = 5;
const PRECEDENCE_CALL: u8 = 6;

#[derive(Debug, PartialEq, Clone)]
struct Error {
    message: String,
}

#[derive(Debug, PartialEq, Clone)]
enum PrefixOperator {
    Minus,
    Bang,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum InfixOperator {
    Minus,
}

#[derive(Debug, PartialEq, Clone)]
enum Expression {
    Call(Box<Expression>, Vec<Expression>),
    Integer(String),
    Identifier(String),
    Boolean(bool),
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

    fn get_precedence(&self, infix_operator: InfixOperator) -> u8 {
        match infix_operator {
            InfixOperator::Minus => PRECEDENCE_SUM,
        }
    }

    fn peek_precedence(&mut self) -> u8 {
        let token = self.tokens.peek();

        match token {
            Some(&&lexer::Token::Operator('-')) => PRECEDENCE_SUM,
            Some(&&lexer::Token::Delimiter('(')) => PRECEDENCE_CALL,
            _ => PRECEDENCE_LOWEST,
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

    fn parse_call_expression(&mut self, block_identifier: Expression) -> Expression {
        let peek_token = self.tokens.peek();
        let mut arguments: Vec<Expression> = Vec::new();

        if peek_token == Some(&&lexer::Token::Delimiter(')')) {
            self.tokens.next();
            return Expression::Call(Box::new(block_identifier), arguments);
        }

        let current_token = self.tokens.next();

        match current_token {
            Some(token) => {
                arguments.push(self.parse_expression_with_precedence(token, PRECEDENCE_LOWEST));

                while self.tokens.peek() == Some(&&lexer::Token::Delimiter(',')) {
                    self.tokens.next();
                    let token_after_comma = self.tokens.next();

                    match token_after_comma {
                        Some(token_after_comma) => {
                            arguments.push(self.parse_expression_with_precedence(
                                token_after_comma,
                                PRECEDENCE_LOWEST,
                            ));
                        }
                        None => {
                            return self.error_expression(String::from(
                                "Unexpected end of arguments in call",
                            ))
                        }
                    }
                }

                let closing_brace = self.tokens.next();

                if closing_brace != Some(&&lexer::Token::Delimiter(')')) {
                    return self.error_expression(String::from(
                        "Missing closing bracket when calling block",
                    ));
                }

                Expression::Call(Box::new(block_identifier), arguments)
            }
            None => {
                self.error_expression(String::from("Error parsing arguments when calling block"))
            }
        }
    }

    fn is_peek_semicolon(&mut self) -> bool {
        self.tokens.peek() != Some(&&lexer::Token::Delimiter(';'))
    }

    fn parse_expression_with_precedence(
        &mut self,
        current_token: &lexer::Token,
        precedence: u8,
    ) -> Expression {
        let mut left_expression = match current_token {
            lexer::Token::Operator(operator) if *operator == '-' || *operator == '!' => {
                let next_token = self.tokens.next();
                let right_expression = match next_token {
                    Some(token) => self.parse_expression_with_precedence(token, PRECEDENCE_PREFIX),
                    None => self
                        .error_expression(String::from("Missing token while parsing expression")),
                };

                if *operator == '-' {
                    return Expression::Prefix(PrefixOperator::Minus, Box::new(right_expression));
                }

                Expression::Prefix(PrefixOperator::Bang, Box::new(right_expression))
            }
            lexer::Token::Integer(integer) => Expression::Integer(integer.clone()),
            lexer::Token::Operator(_) => todo!(),
            lexer::Token::Delimiter(_) => todo!(),
            lexer::Token::Identifier(identifier) => Expression::Identifier(identifier.clone()),
            lexer::Token::Keyword(_) => todo!(),
            lexer::Token::Boolean(boolean) => Expression::Boolean(*boolean),
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
                Some(&&lexer::Token::Delimiter('(')) => {
                    self.tokens.next();
                    left_expression = self.parse_call_expression(left_expression);
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
    fn it_parses_prefix_operator_expressions() {
        let expectations = [
            (
                "-100",
                Expression::Prefix(
                    PrefixOperator::Minus,
                    Box::new(Expression::Integer(String::from("100"))),
                ),
            ),
            (
                "!false",
                Expression::Prefix(PrefixOperator::Bang, Box::new(Expression::Boolean(false))),
            ),
            (
                "!!true",
                Expression::Prefix(
                    PrefixOperator::Bang,
                    Box::new(Expression::Prefix(
                        PrefixOperator::Bang,
                        Box::new(Expression::Boolean(true)),
                    )),
                ),
            ),
        ];

        for (code, expression) in expectations {
            let actual = parse(tokenize(code));
            let expected = Program::new(vec![Statement::Expression(expression)]);

            assert_eq!(actual, expected);
        }
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
    fn it_parses_calling_block_expressions() {
        let expectations = [(
            "print(1)",
            Expression::Call(
                Box::new(Expression::Identifier(String::from("print"))),
                vec![Expression::Integer(String::from("1"))],
            ),
        )];

        for (code, expression) in expectations {
            let actual = parse(tokenize(code));
            let expected = Program::new(vec![Statement::Expression(expression)]);

            assert_eq!(actual, expected);
        }
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
