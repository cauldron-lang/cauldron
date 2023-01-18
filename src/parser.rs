use std::{collections::HashMap, iter::Peekable, slice::Iter};

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
pub struct ParseError {
    pub message: String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrefixOperator {
    Minus,
    Bang,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum InfixOperator {
    Minus,
    Plus,
    Equals,
    NotEquals,
    GreaterThan,
    LessThan,
    Multiply,
    Divide,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub arguments: Arguments,
    pub block: Block,
}

impl Function {
    fn new(arguments: Arguments, block: Block) -> Self {
        Self { arguments, block }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Callable {
    Reference(Identifier),
    Literal(Function),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ADT {
    Product(Identifier, Arguments),
    Sum(Vec<ADT>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum MatchPattern {
    Any,
}

#[derive(Debug, PartialEq, Clone)]
pub struct MatchArm {
    match_pattern: MatchPattern,
    expression: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Type(String),
    Illegal(ParseError),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Call(Box<Expression>, Vec<Expression>),
    Access(Box<Expression>, Box<Expression>),
    Integer(String),
    Identifier(Identifier),
    String(String),
    Boolean(bool),
    Prefix(PrefixOperator, Box<Expression>),
    Infix(InfixOperator, Box<Expression>, Box<Expression>),
    Invalid(Vec<ParseError>),
    Function(Function),
    Block(Block),
    Vector(Vector),
    Map(Map),
    Import(String, String),
    Export(Box<Expression>),
    ADT(Type, ADT),
    Match(Box<Expression>, Vec<MatchArm>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Condition {
    Expression(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Block {
    Statements(Vec<Statement>),
    Invalid(ParseError),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Vector {
    Expressions(Vec<Expression>),
    Invalid(ParseError),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Map {
    Expressions(HashMap<Identifier, Expression>),
    Invalid(ParseError),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Arguments {
    Arguments(Vec<Identifier>),
    Invalid(ParseError),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Assignment(String, Expression),
    ReAssignment(String, Expression),
    Expression(Expression),
    Conditional(Condition, Block),
    Loop(Condition, Block),
    Invalid(ParseError),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
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
            (
                lexer::Token::Identifier(name),
                Some(&&lexer::Token::Operator(lexer::Operator::Assignment)),
            ) => {
                self.tokens.next();
                let next_token = self.tokens.next();

                self.parse_assignment_statement(
                    name.clone(),
                    next_token,
                    lexer::Operator::Assignment,
                )
            }
            (
                lexer::Token::Identifier(name),
                Some(&&lexer::Token::Operator(lexer::Operator::ReAssignment)),
            ) => {
                self.tokens.next();
                let next_token = self.tokens.next();

                self.parse_assignment_statement(
                    name.clone(),
                    next_token,
                    lexer::Operator::ReAssignment,
                )
            }
            (lexer::Token::Delimiter('{'), _) => {
                let next_token = self.tokens.next();

                self.parse_block_statement(next_token)
            }
            (lexer::Token::Keyword(_if), _) if _if == "if" => {
                let next_token = self.tokens.next();

                self.parse_if_statement(next_token)
            }
            (lexer::Token::Keyword(_while), _) if _while == "while" => {
                let next_token = self.tokens.next();

                self.parse_loop_statement(next_token)
            }
            _ => Statement::Expression(self.parse_expression(Some(current_token))),
        };

        let peek_token = self.tokens.peek();

        if peek_token == Some(&&lexer::Token::Delimiter(';')) {
            self.tokens.next();
        }

        statement
    }

    fn parse_block_statement(&mut self, current_token: Option<&lexer::Token>) -> Statement {
        Statement::Expression(Expression::Block(self.parse_block(current_token)))
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
                let peek_token = self.tokens.peek();

                if peek_token != Some(&&lexer::Token::Delimiter(')')) {
                    return self.error_statement(String::from(
                        "Missing close parenthesis for containing condition in conditional",
                    ));
                }

                self.tokens.next();
                let peek_token = self.tokens.peek();

                if peek_token != Some(&&lexer::Token::Delimiter('{')) {
                    return self.error_statement(String::from(
                        "Missing open curly brace for containing consequence in conditional",
                    ));
                }

                self.tokens.next();
                let next_token = self.tokens.next();
                let consequence = self.parse_block(next_token);

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

    fn parse_loop_statement(&mut self, current_token: Option<&lexer::Token>) -> Statement {
        match current_token {
            Some(lexer::Token::Delimiter('(')) => {
                let next_token = self.tokens.next();
                let condition = Condition::Expression(self.parse_expression(next_token));
                let peek_token = self.tokens.peek();

                if peek_token != Some(&&lexer::Token::Delimiter(')')) {
                    return self.error_statement(String::from(
                        "Missing close parenthesis for containing condition in while loop",
                    ));
                }

                self.tokens.next();
                let peek_token = self.tokens.peek();

                if peek_token != Some(&&lexer::Token::Delimiter('{')) {
                    return self.error_statement(String::from(
                        "Missing open curly brace for containing consequence in while loop",
                    ));
                }

                self.tokens.next();
                let next_token = self.tokens.next();
                let loop_body = self.parse_block(next_token);

                Statement::Loop(condition, loop_body)
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
        assignment_token: lexer::Operator,
    ) -> Statement {
        match current_token {
            None => self.error_statement(String::from(
                "Expected expression after assignment operator in assignment statement",
            )),
            _ => {
                let expression = self.parse_expression(current_token);

                if assignment_token == lexer::Operator::Assignment {
                    Statement::Assignment(identifier, expression)
                } else {
                    Statement::ReAssignment(identifier, expression)
                }
            }
        }
    }

    fn get_precedence(&self, infix_operator: InfixOperator) -> u8 {
        match infix_operator {
            InfixOperator::Minus => PRECEDENCE_SUM,
            InfixOperator::Plus => PRECEDENCE_SUM,
            InfixOperator::Equals => PRECEDENCE_EQUALS,
            InfixOperator::NotEquals => PRECEDENCE_EQUALS,
            InfixOperator::GreaterThan => PRECEDENCE_LESSGREATER,
            InfixOperator::LessThan => PRECEDENCE_LESSGREATER,
            InfixOperator::Multiply => PRECEDENCE_PRODUCT,
            InfixOperator::Divide => PRECEDENCE_PRODUCT,
        }
    }

    fn peek_precedence(&mut self) -> u8 {
        let token = self.tokens.peek();

        match token {
            Some(&&lexer::Token::Operator(lexer::Operator::Minus))
            | Some(&&lexer::Token::Operator(lexer::Operator::Plus)) => PRECEDENCE_SUM,
            Some(&&lexer::Token::Delimiter('(')) => PRECEDENCE_CALL,
            Some(&&lexer::Token::Operator(lexer::Operator::Equals))
            | Some(&&lexer::Token::Operator(lexer::Operator::NotEquals)) => PRECEDENCE_EQUALS,
            Some(&&lexer::Token::Operator(lexer::Operator::Multiply))
            | Some(&&lexer::Token::Operator(lexer::Operator::Divide)) => PRECEDENCE_PRODUCT,
            Some(&&lexer::Token::Operator(lexer::Operator::GreaterThan))
            | Some(&&lexer::Token::Operator(lexer::Operator::LessThan)) => PRECEDENCE_LESSGREATER,
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

    fn parse_map_expression(&mut self) -> Expression {
        let peek_token = self.tokens.peek();
        let mut map: HashMap<Identifier, Expression> = HashMap::new();

        if peek_token == Some(&&lexer::Token::Delimiter(']')) {
            self.tokens.next();
            return Expression::Map(Map::Expressions(map));
        }

        let current_token = self.tokens.next();
        let next_token = self.tokens.next();
        let next_next_token = self.tokens.next();

        match (current_token, next_token, next_next_token) {
            (
                Some(lexer::Token::Identifier(identifier)),
                Some(lexer::Token::MapKeySuffix(':')),
                Some(token),
            ) => {
                let expression = self.parse_expression_with_precedence(token, PRECEDENCE_LOWEST);

                map.insert(
                    Identifier {
                        name: identifier.clone(),
                    },
                    expression,
                );

                while self.tokens.peek() == Some(&&lexer::Token::Delimiter(',')) {
                    self.tokens.next();
                    let token_after_comma = self.tokens.next();
                    let next_token_after_comma = self.tokens.next();
                    let next_next_token_after_comma = self.tokens.next();

                    match (
                        token_after_comma,
                        next_token_after_comma,
                        next_next_token_after_comma,
                    ) {
                        (
                            Some(lexer::Token::Identifier(identifier)),
                            Some(lexer::Token::MapKeySuffix(':')),
                            Some(token),
                        ) => {
                            let expression =
                                self.parse_expression_with_precedence(token, PRECEDENCE_LOWEST);

                            map.insert(
                                Identifier {
                                    name: identifier.clone(),
                                },
                                expression,
                            );
                        }
                        (Some(token), _, _) => {
                            let message = format!(
                                "Identifier expected after map literal initializer, received: {:?}",
                                token
                            );

                            return self.error_expression(message);
                        }
                        (None, _, _) => {
                            return self.error_expression(String::from("Unexpected end of map"));
                        }
                    }
                }

                let closing_brace = self.tokens.next();

                if closing_brace != Some(&&lexer::Token::Delimiter(']')) {
                    return self.error_expression(String::from(
                        "Missing closing bracket after items in vector",
                    ));
                }

                Expression::Map(Map::Expressions(map))
            }
            (Some(token), _, _) => {
                let message = format!(
                    "Identifier expected after map literal initializer, received: {:?}",
                    token
                );

                self.error_expression(message)
            }
            (None, _, _) => {
                self.error_expression(String::from("Error parsing arguments when calling block"))
            }
        }
    }

    fn parse_vector_expression(&mut self) -> Expression {
        let peek_token = self.tokens.peek();
        let mut items: Vec<Expression> = Vec::new();

        if peek_token == Some(&&lexer::Token::Delimiter(']')) {
            self.tokens.next();
            return Expression::Vector(Vector::Expressions(items));
        }

        let current_token = self.tokens.next();

        match current_token {
            Some(token) => {
                items.push(self.parse_expression_with_precedence(token, PRECEDENCE_LOWEST));

                while self.tokens.peek() == Some(&&lexer::Token::Delimiter(',')) {
                    self.tokens.next();
                    let token_after_comma = self.tokens.next();

                    match token_after_comma {
                        Some(token_after_comma) => {
                            items.push(self.parse_expression_with_precedence(
                                token_after_comma,
                                PRECEDENCE_LOWEST,
                            ));
                        }
                        None => {
                            return self.error_expression(String::from(
                                "Unexpected end of items in vector",
                            ))
                        }
                    }
                }

                let closing_brace = self.tokens.next();

                if closing_brace != Some(&&lexer::Token::Delimiter(']')) {
                    return self.error_expression(String::from(
                        "Missing closing bracket after items in vector",
                    ));
                }

                Expression::Vector(Vector::Expressions(items))
            }
            None => {
                self.error_expression(String::from("Error parsing arguments when calling block"))
            }
        }
    }

    fn parse_call_expression(&mut self, block_identifier: Expression) -> Expression {
        let peek_token = self.tokens.peek();
        let mut arguments: Vec<Expression> = Vec::new();

        if peek_token == Some(&&lexer::Token::Delimiter(')')) {
            self.tokens.next();

            return match block_identifier {
                Expression::Identifier(Identifier { name }) if name.as_str() == "import" => self
                    .error_expression(String::from(
                        "Import expressions must include argument containing name of module",
                    )),
                Expression::Identifier(Identifier { name }) if name.as_str() == "export" => self
                    .error_expression(String::from(
                        "Export expressions must include argument containing name of module",
                    )),
                _ => Expression::Call(Box::new(block_identifier), arguments),
            };
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

                match block_identifier {
                    Expression::Identifier(Identifier { name }) if name.as_str() == "import" => {
                        // unwrapping the first argument because the zero arguments case is being handled above
                        let import_argument = arguments.first().unwrap();

                        match import_argument {
                            Expression::String(import_argument) => {
                                let module_path = import_argument.split(':').collect::<Vec<&str>>();

                                match module_path.len() {
                                    1 => Expression::Import(
                                        String::from(""),
                                        String::from(*module_path.first().unwrap()),
                                    ),
                                    2 => Expression::Import(
                                        String::from(*module_path.first().unwrap()),
                                        String::from(*module_path.get(1).unwrap()),
                                    ),
                                    _ => self.error_expression(String::from(format!("Expected module path to follow the template <prefix:path> and received {:?}", module_path)))
                                }
                            }
                            other => self.error_expression(String::from(format!("Only string literals can be passed to import statements, instead received: {:?}", other))),
                        }
                    }
                    Expression::Identifier(Identifier { name }) if name.as_str() == "export" => {
                        // unwrapping the first argument because the zero arguments case is being handled above
                        let export_argument = arguments.first().unwrap();

                        match arguments.len() {
                            1 => Expression::Export(Box::new(export_argument.clone())),
                            num_args => self.error_expression(String::from(format!(
                                "Expected only one argument to export function. Received {:?}",
                                num_args
                            ))),
                        }
                    }
                    _ => Expression::Call(Box::new(block_identifier), arguments),
                }
            }
            None => {
                self.error_expression(String::from("Error parsing arguments when calling block"))
            }
        }
    }

    fn parse_access_expression(&mut self, block_identifier: Expression) -> Expression {
        let current_token = self.tokens.next();

        match current_token {
            Some(current_token) => {
                let key = self.parse_expression_with_precedence(current_token, PRECEDENCE_LOWEST);

                Expression::Access(Box::new(block_identifier), Box::new(key))
            }
            None => Expression::Invalid(vec![ParseError {
                message: String::from("Unexpected EOF while parsing access expression"),
            }]),
        }
    }

    fn is_peek_not_semicolon(&mut self) -> bool {
        self.tokens.peek() != Some(&&lexer::Token::Delimiter(';'))
    }

    fn parse_arguments(&mut self) -> Arguments {
        let mut identifiers = vec![];
        let mut peek_token = self.tokens.peek();

        while peek_token != Some(&&lexer::Token::Delimiter(')')) {
            let current_token = self.tokens.next();
            peek_token = self.tokens.peek();

            match current_token {
                Some(lexer::Token::Identifier(identifier)) => {
                    identifiers.push(Identifier {
                        name: identifier.clone(),
                    });
                }
                Some(lexer::Token::Delimiter(',')) => {}
                _ => {
                    return Arguments::Invalid(ParseError {
                        message: String::from("Invalid Arguments"),
                    });
                }
            };
        }

        self.tokens.next();
        Arguments::Arguments(identifiers)
    }

    fn parse_match_arm_expression(&mut self) -> Result<Expression, Vec<ParseError>> {
        let current_token = self.tokens.next();

        match current_token {
            Some(token) => {
                let parsed_expression =
                    self.parse_expression_with_precedence(token, PRECEDENCE_LOWEST);

                match parsed_expression {
                    Expression::Invalid(parse_errors) => Err(parse_errors),
                    expression => Ok(expression),
                }
            }
            None => self
                .parse_result_error(String::from("Unexpected EOF in match arm"))
                .map_err(|parse_error| vec![parse_error]),
        }
    }

    fn parse_match_arm(
        &mut self,
        current_token: &lexer::Token,
    ) -> Result<MatchArm, Vec<ParseError>> {
        match current_token {
            lexer::Token::Identifier(identifier) if identifier == "_" => {
                let current_token = self.tokens.next();

                if current_token != Some(&lexer::Token::Operator(lexer::Operator::MatchArrow)) {
                    return self
                        .parse_result_error(format!(
                            "Unexpected token in match arm: {:?}",
                            current_token
                        ))
                        .map_err(|parse_error| vec![parse_error]);
                }

                let parsed_expression = self.parse_match_arm_expression();

                parsed_expression.map(|expression| MatchArm {
                    match_pattern: MatchPattern::Any,
                    expression: expression,
                })
            }
            token => self
                .parse_result_error(format!("Unexpected token in match arm: {:?}", token))
                .map_err(|parse_error| vec![parse_error]),
        }
    }

    fn parse_match_arms(&mut self) -> Result<Vec<MatchArm>, Vec<ParseError>> {
        let current_token = self.tokens.next();

        if current_token != Some(&lexer::Token::Delimiter('{')) {
            return self
                .parse_result_error(format!(
                    "Expected open bracket token to start match arms but received: {:?}",
                    current_token
                ))
                .map_err(|parse_error| vec![parse_error]);
        }

        let mut match_arms: Vec<MatchArm> = Vec::new();
        let mut parse_errors: Vec<ParseError> = Vec::new();

        loop {
            let current_token = self.tokens.next();

            match current_token {
                Some(&lexer::Token::Delimiter('}')) => {
                    break;
                }
                Some(lexer::Token::Delimiter('|')) => {
                    continue;
                }
                Some(token) => {
                    let parsed_match_arm = self.parse_match_arm(token);

                    match parsed_match_arm {
                        Ok(match_arm) => match_arms.push(match_arm),
                        Err(other_parse_errors) => other_parse_errors
                            .iter()
                            .for_each(|parse_error| parse_errors.push(parse_error.clone())),
                    }
                }
                None => {
                    parse_errors.push(ParseError {
                        message: String::from("Unexpected EOF in match expression"),
                    });
                    break;
                }
            };
        }

        if parse_errors.len() > 0 {
            return Err(parse_errors);
        }

        Ok(match_arms)
    }

    fn parse_match_expression(&mut self) -> Result<Expression, Vec<ParseError>> {
        let current_token = self.tokens.next();

        match current_token {
            Some(lexer::Token::Delimiter('(')) => {
                let current_token = self.tokens.next();

                match current_token {
                    Some(current_token) => {
                        let delimited_expression =
                            self.parse_expression_with_precedence(current_token, PRECEDENCE_LOWEST);
                        let peek_token = self.tokens.peek();

                        match peek_token {
                            Some(&&lexer::Token::Delimiter(')')) => {
                                self.tokens.next();

                                let parsed_match_arms = self.parse_match_arms();

                                parsed_match_arms.map(|match_arms| {
                                    Expression::Match(Box::new(delimited_expression), match_arms)
                                })
                            }
                            _ => self
                                .parse_result_error(String::from(
                                    "Unexpected token after group expression",
                                ))
                                .map_err(|parse_error| vec![parse_error]),
                        }
                    }
                    None => self
                        .parse_result_error(String::from("Unexpected EOF after 'match ('"))
                        .map_err(|parse_error| vec![parse_error]),
                }
            }
            token => self
                .parse_result_error(format!(
                    "Expected open parenthesis after 'match' keyword but received: {:?}",
                    token
                ))
                .map_err(|parse_error| vec![parse_error]),
        }
    }

    fn parse_prefix_expression(&mut self, current_token: &lexer::Token) -> Expression {
        match current_token {
            lexer::Token::String(string) => Expression::String(string.clone()),
            lexer::Token::Operator(operator)
                if *operator == lexer::Operator::Minus || *operator == lexer::Operator::Bang =>
            {
                let next_token = self.tokens.next();
                let right_expression = match next_token {
                    Some(token) => self.parse_expression_with_precedence(token, PRECEDENCE_PREFIX),
                    None => self
                        .error_expression(String::from("Missing token while parsing expression")),
                };

                if *operator == lexer::Operator::Minus {
                    return Expression::Prefix(PrefixOperator::Minus, Box::new(right_expression));
                }

                Expression::Prefix(PrefixOperator::Bang, Box::new(right_expression))
            }
            lexer::Token::Integer(integer) => Expression::Integer(integer.clone()),
            lexer::Token::Operator(operator) => Expression::Invalid(vec![ParseError {
                message: format!("Unexpected operator: {:?}", operator),
            }]),
            lexer::Token::Delimiter('(') => {
                let current_token = self.tokens.next();

                match current_token {
                    Some(current_token) => {
                        let delimited_expression =
                            self.parse_expression_with_precedence(current_token, PRECEDENCE_LOWEST);
                        let peek_token = self.tokens.peek();

                        match peek_token {
                            Some(&&lexer::Token::Delimiter(')')) => {
                                self.tokens.next();
                                delimited_expression
                            }
                            _ => self.error_expression(String::from(
                                "Unexpected token after group expression",
                            )),
                        }
                    }
                    None => self.error_expression(String::from(
                        "Unexpected EOF after start of grouped expression",
                    )),
                }
            }
            lexer::Token::MapInitializer('%') => {
                let current_token = self.tokens.next();

                match current_token {
                    Some(lexer::Token::Delimiter('[')) => self.parse_map_expression(),
                    Some(token) => {
                        let message =
                            format!("Unexpected token after map initializer: {:?}", token);

                        self.error_expression(message)
                    }
                    None => {
                        self.error_expression(String::from("Missing token after map initializer"))
                    }
                }
            }
            lexer::Token::Delimiter('[') => self.parse_vector_expression(),
            lexer::Token::Identifier(identifier) => {
                let peek_token = self.tokens.peek();

                match peek_token {
                    Some(&&lexer::Token::Delimiter('[')) => {
                        self.tokens.next();

                        let current_token = self.tokens.next();

                        match current_token {
                            Some(current_token) => {
                                let delimited_expression = self.parse_expression_with_precedence(
                                    current_token,
                                    PRECEDENCE_LOWEST,
                                );
                                let peek_token = self.tokens.peek();

                                match peek_token {
                                    Some(&&lexer::Token::Delimiter(']')) => {
                                        self.tokens.next();

                                        Expression::Access(
                                            Box::new(Expression::Identifier(Identifier {
                                                name: identifier.clone(),
                                            })),
                                            Box::new(delimited_expression),
                                        )
                                    }
                                    _ => self.error_expression(String::from(
                                        "Unexpected token after group expression",
                                    )),
                                }
                            }
                            None => self.error_expression(String::from(
                                "Missing token after open bracket during collection access",
                            )),
                        }
                    }
                    _ => Expression::Identifier(Identifier {
                        name: identifier.clone(),
                    }),
                }
            }
            lexer::Token::Keyword(_fn) if _fn == "fn" => {
                let peek_token = self.tokens.peek();

                match peek_token {
                    Some(&&lexer::Token::Delimiter('(')) => {
                        self.tokens.next();

                        let arguments = self.parse_arguments();
                        let peek_token = self.tokens.peek();

                        if peek_token != Some(&&lexer::Token::Delimiter('{')) {
                            return self.error_expression(String::from(
                                "Missing open curly brace for containing consequence in conditional",
                            ));
                        }

                        self.tokens.next();
                        let next_token = self.tokens.next();
                        let block = self.parse_block(next_token);

                        Expression::Function(Function::new(arguments, block))
                    }
                    Some(invalid_token) => {
                        let token = *invalid_token;

                        self.error_expression(format!(
                            "Invalid token while parsing 'fn' expression's arguments: {:?}",
                            token
                        ))
                    }
                    None => self.error_expression(String::from(
                        "Missing condition and consequence of 'fn' expression",
                    )),
                }
            }
            lexer::Token::Keyword(keyword) if keyword == "adt" => {
                let current_token = self.tokens.next();
                let _type = match current_token {
                    Some(lexer::Token::Type(_type)) => Type::Type(_type.clone()),
                    Some(token) => Type::Illegal(ParseError {
                        message: format!(
                            "Expected type after keyword 'adt' instead received {:?}",
                            token
                        ),
                    }),
                    None => Type::Illegal(ParseError {
                        message: String::from("Unexpected EOF after keyword 'adt'"),
                    }),
                };

                let current_token = self.tokens.next();

                match current_token {
                    Some(lexer::Token::Delimiter('{')) => {
                        let mut adts = vec![];

                        loop {
                            let current_token = self.tokens.next();

                            match current_token {
                                Some(lexer::Token::Identifier(identifier)) => {
                                    let peek_token = self.tokens.peek();

                                    match peek_token {
                                        Some(lexer::Token::Delimiter('(')) => {
                                            self.tokens.next();
                                            let arguments = self.parse_arguments();

                                            adts.push(ADT::Product(
                                                Identifier {
                                                    name: identifier.clone(),
                                                },
                                                arguments,
                                            ));
                                        }
                                        _ => adts.push(ADT::Product(
                                            Identifier {
                                                name: identifier.clone(),
                                            },
                                            Arguments::Arguments(vec![]),
                                        )),
                                    }
                                }
                                Some(lexer::Token::Delimiter('|')) => continue,
                                Some(lexer::Token::Delimiter('}')) => break,
                                Some(token) => {
                                    return self.error_expression(format!(
                                        "Unexpected token in 'adt' declaration: {:?}",
                                        token
                                    ))
                                }
                                None => {
                                    return self.error_expression(String::from(
                                        "Unexpected EOF while parsing 'adt' declaration",
                                    ))
                                }
                            };
                        }

                        match adts.len() {
                            0 => self.error_expression(String::from(
                                "Must include sum or product type in ADT declaration",
                            )),
                            1 => {
                                let product = adts.first().unwrap();

                                Expression::ADT(_type, product.clone())
                            }
                            _ => Expression::ADT(_type, ADT::Sum(adts)),
                        }
                    }
                    Some(invalid_token) => {
                        let token = invalid_token;

                        self.error_expression(format!(
                            "Expected block following 'adt' keyword instead given: {:?}",
                            token
                        ))
                    }
                    None => self.error_expression(String::from(
                        "Missing condition and consequence of 'adt' expression",
                    )),
                }
            }
            lexer::Token::Keyword(keyword) if keyword == "match" => {
                let parsed_match_expression = self.parse_match_expression();

                match parsed_match_expression {
                    Ok(expression) => expression,
                    Err(parse_errors) => Expression::Invalid(parse_errors),
                }
            }
            lexer::Token::Keyword(_) => todo!(),
            lexer::Token::Boolean(boolean) => Expression::Boolean(*boolean),
            lexer::Token::Illegal => todo!(),
            lexer::Token::Delimiter(_) => todo!(),
            lexer::Token::MapInitializer(_) => todo!(),
            lexer::Token::MapKeySuffix(_) => todo!(),
            lexer::Token::Type(_) => todo!(),
        }
    }

    fn parse_expression_with_precedence(
        &mut self,
        current_token: &lexer::Token,
        precedence: u8,
    ) -> Expression {
        let mut left_expression = self.parse_prefix_expression(current_token);

        while self.is_peek_not_semicolon() && precedence < self.peek_precedence() {
            let peek_token = self.tokens.peek();

            match peek_token {
                Some(&&lexer::Token::Operator(lexer::Operator::Minus)) => {
                    self.tokens.next();
                    left_expression =
                        self.parse_infix_expression(left_expression, InfixOperator::Minus);
                }
                Some(&&lexer::Token::Operator(lexer::Operator::Plus)) => {
                    self.tokens.next();
                    left_expression =
                        self.parse_infix_expression(left_expression, InfixOperator::Plus)
                }
                Some(&&lexer::Token::Operator(lexer::Operator::Multiply)) => {
                    self.tokens.next();
                    left_expression =
                        self.parse_infix_expression(left_expression, InfixOperator::Multiply)
                }
                Some(&&lexer::Token::Operator(lexer::Operator::Divide)) => {
                    self.tokens.next();
                    left_expression =
                        self.parse_infix_expression(left_expression, InfixOperator::Divide)
                }
                Some(&&lexer::Token::Operator(lexer::Operator::Equals)) => {
                    self.tokens.next();
                    left_expression =
                        self.parse_infix_expression(left_expression, InfixOperator::Equals);
                }
                Some(&&lexer::Token::Operator(lexer::Operator::NotEquals)) => {
                    self.tokens.next();
                    left_expression =
                        self.parse_infix_expression(left_expression, InfixOperator::NotEquals);
                }
                Some(&&lexer::Token::Operator(lexer::Operator::GreaterThan)) => {
                    self.tokens.next();
                    left_expression =
                        self.parse_infix_expression(left_expression, InfixOperator::GreaterThan);
                }
                Some(&&lexer::Token::Operator(lexer::Operator::LessThan)) => {
                    self.tokens.next();
                    left_expression =
                        self.parse_infix_expression(left_expression, InfixOperator::LessThan);
                }
                Some(&&lexer::Token::Delimiter('(')) => {
                    self.tokens.next();
                    left_expression = self.parse_call_expression(left_expression);
                }
                Some(&&lexer::Token::Delimiter('[')) => {
                    self.tokens.next();
                    left_expression = self.parse_access_expression(left_expression);
                }
                _ => break,
            };
        }

        left_expression
    }

    fn parse_expression(&mut self, current_token: Option<&lexer::Token>) -> Expression {
        match current_token {
            Some(token) => self.parse_expression_with_precedence(token, PRECEDENCE_LOWEST),
            None => self.error_expression(String::from("Missing token while parsing expression")),
        }
    }

    fn error_statement(&mut self, message: String) -> Statement {
        self.seek_delimiter();

        Statement::Invalid(ParseError { message })
    }

    fn error_expression(&mut self, message: String) -> Expression {
        self.seek_delimiter();

        Expression::Invalid(vec![ParseError { message }])
    }

    fn parse_result_error<T>(&mut self, message: String) -> Result<T, ParseError> {
        self.seek_delimiter();

        let parse_error = ParseError {
            message: String::from(message),
        };

        Err(parse_error)
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
    use std::collections::HashMap;

    use super::{
        parse, Arguments, Block, Condition, Function, Identifier, Map, MatchArm, MatchPattern,
        Program, Statement, Type, Vector, ADT,
    };
    use crate::{
        lexer::tokenize,
        parser::{Expression, InfixOperator, PrefixOperator},
    };

    fn assert_expression_eq(code: &str, expression: Expression) {
        let actual = parse(tokenize(code));
        let expected = Program::new(vec![Statement::Expression(expression)]);

        assert_eq!(actual, expected);
    }

    fn assert_statement_eq(code: &str, statement: Statement) {
        let actual = parse(tokenize(code));
        let expected = Program::new(vec![statement]);

        assert_eq!(actual, expected);
    }

    #[test]
    fn it_parses_grouped_expression_1() {
        assert_expression_eq(
            "10 * (4 + 1)",
            Expression::Infix(
                InfixOperator::Multiply,
                Box::new(Expression::Integer(String::from("10"))),
                Box::new(Expression::Infix(
                    InfixOperator::Plus,
                    Box::new(Expression::Integer(String::from("4"))),
                    Box::new(Expression::Integer(String::from("1"))),
                )),
            ),
        );
    }

    #[test]
    fn it_parses_grouped_expression_2() {
        assert_expression_eq(
            "(100 + 50) * 5",
            Expression::Infix(
                InfixOperator::Multiply,
                Box::new(Expression::Infix(
                    InfixOperator::Plus,
                    Box::new(Expression::Integer(String::from("100"))),
                    Box::new(Expression::Integer(String::from("50"))),
                )),
                Box::new(Expression::Integer(String::from("5"))),
            ),
        )
    }

    #[test]
    fn it_parses_negative_number_expression() {
        assert_expression_eq(
            "-100",
            Expression::Prefix(
                PrefixOperator::Minus,
                Box::new(Expression::Integer(String::from("100"))),
            ),
        );
    }

    #[test]
    fn it_parses_bang_expression_1() {
        assert_expression_eq(
            "!false",
            Expression::Prefix(PrefixOperator::Bang, Box::new(Expression::Boolean(false))),
        );
    }

    #[test]
    fn it_parse_bang_expression_2() {
        assert_expression_eq(
            "!!true",
            Expression::Prefix(
                PrefixOperator::Bang,
                Box::new(Expression::Prefix(
                    PrefixOperator::Bang,
                    Box::new(Expression::Boolean(true)),
                )),
            ),
        );
    }

    #[test]
    fn it_parses_subtraction_expression() {
        assert_expression_eq(
            "100 - 50",
            Expression::Infix(
                InfixOperator::Minus,
                Box::new(Expression::Integer(String::from("100"))),
                Box::new(Expression::Integer(String::from("50"))),
            ),
        );
    }

    #[test]
    fn it_parses_addition_expression() {
        assert_expression_eq(
            "100 + 50",
            Expression::Infix(
                InfixOperator::Plus,
                Box::new(Expression::Integer(String::from("100"))),
                Box::new(Expression::Integer(String::from("50"))),
            ),
        );
    }

    #[test]
    fn it_parses_call_expression() {
        assert_expression_eq(
            "print(1)",
            Expression::Call(
                Box::new(Expression::Identifier(Identifier {
                    name: String::from("print"),
                })),
                vec![Expression::Integer(String::from("1"))],
            ),
        );
    }

    #[test]
    fn it_parses_greater_than_expression() {
        assert_expression_eq(
            "1 > 2",
            Expression::Infix(
                InfixOperator::GreaterThan,
                Box::new(Expression::Integer(String::from("1"))),
                Box::new(Expression::Integer(String::from("2"))),
            ),
        );
    }

    #[test]
    fn it_parses_less_than_expression() {
        assert_expression_eq(
            "1 < 2",
            Expression::Infix(
                InfixOperator::LessThan,
                Box::new(Expression::Integer(String::from("1"))),
                Box::new(Expression::Integer(String::from("2"))),
            ),
        );
    }

    #[test]
    fn it_parses_assignment_statement() {
        assert_statement_eq(
            "a := 1",
            Statement::Assignment(String::from("a"), Expression::Integer(String::from("1"))),
        );
    }

    #[test]
    fn it_parses_assignment_in_block_statement() {
        assert_statement_eq(
            "{ a := 1 }",
            Statement::Expression(Expression::Block(Block::Statements(vec![
                Statement::Assignment(String::from("a"), Expression::Integer(String::from("1"))),
            ]))),
        );
    }

    #[test]
    fn it_parses_functions_with_arguments() {
        assert_statement_eq(
            "fn(arg1, arg2) { print(arg1, arg2) }",
            Statement::Expression(Expression::Function(Function::new(
                Arguments::Arguments(vec![
                    Identifier {
                        name: String::from("arg1"),
                    },
                    Identifier {
                        name: String::from("arg2"),
                    },
                ]),
                Block::Statements(vec![Statement::Expression(Expression::Call(
                    Box::new(Expression::Identifier(Identifier {
                        name: String::from("print"),
                    })),
                    vec![
                        Expression::Identifier(Identifier {
                            name: String::from("arg1"),
                        }),
                        Expression::Identifier(Identifier {
                            name: String::from("arg2"),
                        }),
                    ],
                ))]),
            ))),
        );
    }

    #[test]
    fn it_parses_call_in_conditional() {
        assert_statement_eq(
            "if (1) { print(1) }",
            Statement::Conditional(
                Condition::Expression(Expression::Integer(String::from("1"))),
                Block::Statements(vec![Statement::Expression(Expression::Call(
                    Box::new(Expression::Identifier(Identifier {
                        name: String::from("print"),
                    })),
                    vec![Expression::Integer(String::from("1"))],
                ))]),
            ),
        );
    }

    #[test]
    fn it_parses_string_assignment() {
        assert_statement_eq(
            "foo := \"bar\"",
            Statement::Assignment(String::from("foo"), Expression::String(String::from("bar"))),
        )
    }

    #[test]
    fn it_parses_empty_vector() {
        assert_statement_eq(
            "[]",
            Statement::Expression(Expression::Vector(Vector::Expressions(vec![]))),
        )
    }

    #[test]
    fn it_parses_vector_of_integers() {
        assert_statement_eq(
            "[1, 2, 3, 4]",
            Statement::Expression(Expression::Vector(Vector::Expressions(vec![
                Expression::Integer(String::from("1")),
                Expression::Integer(String::from("2")),
                Expression::Integer(String::from("3")),
                Expression::Integer(String::from("4")),
            ]))),
        )
    }

    #[test]
    fn it_parses_vector_of_strings() {
        assert_statement_eq(
            "[\"foo\", \"bar\"]",
            Statement::Expression(Expression::Vector(Vector::Expressions(vec![
                Expression::String(String::from("foo")),
                Expression::String(String::from("bar")),
            ]))),
        )
    }

    #[test]
    fn it_parses_vector_of_arithmetic_expressions() {
        assert_statement_eq(
            "[1 + 1, 3 - 1]",
            Statement::Expression(Expression::Vector(Vector::Expressions(vec![
                Expression::Infix(
                    InfixOperator::Plus,
                    Box::new(Expression::Integer(String::from("1"))),
                    Box::new(Expression::Integer(String::from("1"))),
                ),
                Expression::Infix(
                    InfixOperator::Minus,
                    Box::new(Expression::Integer(String::from("3"))),
                    Box::new(Expression::Integer(String::from("1"))),
                ),
            ]))),
        )
    }

    #[test]
    fn it_parses_map_of_strings() {
        assert_statement_eq(
            "%[foo: \"bar\"]",
            Statement::Expression(Expression::Map(Map::Expressions(HashMap::from([(
                Identifier {
                    name: String::from("foo"),
                },
                Expression::String(String::from("bar")),
            )])))),
        )
    }

    #[test]
    fn it_parses_call_in_loop() {
        assert_statement_eq(
            "while (true) { print(1) }",
            Statement::Loop(
                Condition::Expression(Expression::Boolean(true)),
                Block::Statements(vec![Statement::Expression(Expression::Call(
                    Box::new(Expression::Identifier(Identifier {
                        name: String::from("print"),
                    })),
                    vec![Expression::Integer(String::from("1"))],
                ))]),
            ),
        );
    }

    #[test]
    fn it_parses_access_with_integer() {
        assert_statement_eq(
            "f[0]",
            Statement::Expression(Expression::Access(
                Box::new(Expression::Identifier(Identifier {
                    name: String::from("f"),
                })),
                Box::new(Expression::Integer(String::from("0"))),
            )),
        )
    }

    #[test]
    fn it_parses_exports() {
        assert_statement_eq(
            "export(1)",
            Statement::Expression(Expression::Export(Box::new(Expression::Integer(
                String::from("1"),
            )))),
        )
    }

    #[test]
    fn it_parses_imports() {
        assert_statement_eq(
            "import(\"bifs:io\")",
            Statement::Expression(Expression::Import(String::from("bifs"), String::from("io"))),
        )
    }

    #[test]
    fn it_parses_adts() {
        assert_statement_eq(
            "adt Maybe { some(a) | none }",
            Statement::Expression(Expression::ADT(
                Type::Type(String::from("Maybe")),
                ADT::Sum(vec![
                    ADT::Product(
                        Identifier {
                            name: String::from("some"),
                        },
                        Arguments::Arguments(vec![Identifier {
                            name: String::from("a"),
                        }]),
                    ),
                    ADT::Product(
                        Identifier {
                            name: String::from("none"),
                        },
                        Arguments::Arguments(vec![]),
                    ),
                ]),
            )),
        )
    }

    #[test]
    fn it_parses_matches() {
        assert_statement_eq(
            "match(m) { _ -> 0 }",
            Statement::Expression(Expression::Match(
                Box::new(Expression::Identifier(Identifier {
                    name: String::from("m"),
                })),
                vec![MatchArm {
                    match_pattern: MatchPattern::Any,
                    expression: Expression::Integer(String::from("0")),
                }],
            )),
        );
    }
}
