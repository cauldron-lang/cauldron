use std::collections::HashMap;

use crate::parser::{
    self, Arguments, Block, Function, Identifier, InfixOperator, PrefixOperator, Statement,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Void,
    Integer(i32),
    Error(String),
    Boolean(bool),
    // FIXME: Arguments should be renamed to Parameters here
    Function(Arguments, Block, Environment),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Result {
    Void,
    Object(Object),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    variables: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    fn get(&self, key: &Identifier) -> Option<&Object> {
        self.variables.get(&key.name)
    }

    fn set(&mut self, key: String, object: Object) {
        self.variables.insert(key, object);
    }
}

fn eval_expression(expression: parser::Expression, environment: &mut Environment) -> Object {
    match expression {
        parser::Expression::Call(identifier, arguments) => match *identifier {
            parser::Expression::Identifier(identifier) => {
                match environment.clone().get(&identifier) {
                    Some(object) => match object {
                        Object::Function(parameters, block, enclosed_environment) => {
                            match parameters {
                                Arguments::Arguments(parameters) => {
                                    let environment_variables =
                                        parameters.iter().zip(arguments.iter());
                                    let mut new_enclosed_environment = enclosed_environment.clone();

                                    for (_i, (identifier, expression)) in
                                        environment_variables.enumerate()
                                    {
                                        new_enclosed_environment.set(
                                            identifier.clone().name,
                                            eval_expression(expression.clone(), environment),
                                        )
                                    }

                                    match eval_block(block.clone(), &mut new_enclosed_environment) {
                                        Result::Object(retval) => retval,
                                        Result::Void => Object::Void,
                                    }
                                }
                                Arguments::Invalid(error) => Object::Error(format!(
                                    "Cannot call function with invalid parameters: {:?}",
                                    error
                                )),
                            }
                        }
                        other => Object::Error(format!("Object is not callable: {:?}", other)),
                    },
                    None => Object::Error(format!(
                        "Cannot call function that doesn't exist with identifier: {:?}",
                        identifier
                    )),
                }
            }
            other => {
                // FIXME: This should not be possible assuming the parser is working properly.
                // Can the parser be changed to reduce the need for this extra logic.
                Object::Error(format!(
                    "Parsed the following non-identifier token as callable: {:?}",
                    other
                ))
            }
        },
        parser::Expression::Integer(integer) => {
            let integer = integer.parse::<i32>();

            match integer {
                Ok(integer) => Object::Integer(integer),
                Err(parse_error) => Object::Error(format!("{:?}", parse_error)),
            }
        }
        parser::Expression::Identifier(identifier) => match environment.get(&identifier) {
            Some(object) => object.clone(),
            None => Object::Error(format!(
                "Environment missing identifier {:?}",
                identifier.clone()
            )),
        },
        parser::Expression::Boolean(boolean) => Object::Boolean(boolean),
        parser::Expression::Prefix(prefix_operator, expression) => {
            let right = eval_expression(*expression, environment);

            match prefix_operator {
                PrefixOperator::Minus => todo!(),
                PrefixOperator::Bang => match right {
                    Object::Boolean(true) => Object::Boolean(false),
                    Object::Boolean(false) => Object::Boolean(true),
                    _ => Object::Error(String::from(
                        "Bang operator can only precede boolean expressions",
                    )),
                },
            }
        }
        parser::Expression::Infix(infix_operator, left_expression, right_expression) => {
            let left_object = eval_expression(*left_expression, environment);
            let right_object = eval_expression(*right_expression, environment);

            match (left_object.clone(), right_object.clone()) {
                (Object::Integer(left_integer), Object::Integer(right_integer)) => {
                    match infix_operator {
                        InfixOperator::Minus => Object::Integer(left_integer - right_integer),
                        InfixOperator::Plus => Object::Integer(left_integer + right_integer),
                        InfixOperator::Equals => Object::Boolean(left_integer == right_integer),
                        InfixOperator::NotEquals => Object::Boolean(left_integer != right_integer),
                        InfixOperator::GreaterThan => Object::Boolean(left_integer > right_integer),
                        InfixOperator::LessThan => Object::Boolean(left_integer < right_integer),
                        InfixOperator::Multiply => Object::Integer(left_integer * right_integer),
                        InfixOperator::Divide => Object::Integer(left_integer / right_integer),
                    }
                }
                _ => Object::Error(format!(
                    "Cannot subtract '{:?}' from '{:?}'",
                    left_object, right_object
                )),
            }
        }
        parser::Expression::Invalid(_) => todo!(),
        parser::Expression::Function(Function { arguments, block }) => {
            Object::Function(arguments, block, environment.clone())
        }
        parser::Expression::Block(_) => todo!(),
    }
}

fn eval_block(block: parser::Block, environment: &mut Environment) -> Result {
    match block {
        Block::Statements(statements) => {
            let mut retval = Result::Void;

            for statement in statements {
                match eval_statement(statement, environment) {
                    Result::Object(Object::Error(error)) => {
                        return Result::Object(Object::Error(String::from(format!(
                            "Error occurred while evaluating statement in block: {:?}",
                            error
                        ))))
                    }
                    Result::Object(object) => {
                        retval = Result::Object(object);
                        continue;
                    }
                    Result::Void => {
                        retval = Result::Void;
                        continue;
                    }
                }
            }

            retval
        }
        Block::Invalid(error) => Result::Object(Object::Error(String::from(format!(
            "Cannot eval block due to parse error: {:?}",
            error.message
        )))),
    }
}

fn eval_statement(statement: parser::Statement, environment: &mut Environment) -> Result {
    match statement {
        parser::Statement::Assignment(identifier, expression) => {
            let object = eval_expression(expression, environment);

            environment.set(identifier, object);
            Result::Void
        }
        parser::Statement::Expression(expression) => {
            Result::Object(eval_expression(expression, environment))
        }
        parser::Statement::Conditional(
            parser::Condition::Expression(condition_expression),
            consequence,
        ) => match eval_expression(condition_expression, environment) {
            Object::Boolean(true) => {
                eval_block(consequence, environment);
                Result::Void
            }
            Object::Boolean(false) => Result::Void,
            object => Result::Object(Object::Error(String::from(format!(
                "Condition expression must evalate to boolean, instead received {:?}",
                object
            )))),
        },
        parser::Statement::Invalid(_) => todo!(),
    }
}

fn eval_statements(statements: Vec<parser::Statement>, environment: &mut Environment) -> Result {
    let mut object = Result::Void;

    for statement in statements {
        object = eval_statement(statement, environment);
    }

    object
}

pub fn eval(program: parser::Program, environment: &mut Environment) -> Result {
    eval_statements(program.statements, environment)
}

#[cfg(test)]
mod tests {
    use super::{eval, Environment, Object, Result};
    use crate::{lexer::tokenize, parser::parse};

    fn assert_evaluated_object(code: &str, object: Object) {
        let parsed = parse(tokenize(code));
        let mut environment = Environment::new();
        let actual = eval(parsed, &mut environment);

        assert_eq!(actual, Result::Object(object));
    }

    #[test]
    fn it_evaluates_integer_expressions() {
        assert_evaluated_object("1", Object::Integer(1));
    }

    #[test]
    fn it_evaluates_true_boolean_expressions() {
        assert_evaluated_object("true", Object::Boolean(true));
    }

    #[test]
    fn it_evaluates_false_boolean_expressions() {
        assert_evaluated_object("false", Object::Boolean(false));
    }

    #[test]
    fn it_evaluates_bang_true_boolean_expressions() {
        assert_evaluated_object("!true", Object::Boolean(false));
    }

    #[test]
    fn it_evaluates_bang_false_boolean_expressions() {
        assert_evaluated_object("!false", Object::Boolean(true));
    }

    #[test]
    fn it_evaluates_bang_bang_true_boolean_expressions() {
        assert_evaluated_object("!!true", Object::Boolean(true));
    }

    #[test]
    fn it_evaluates_subtraction_expressions() {
        assert_evaluated_object("1 - 1", Object::Integer(0));
    }

    #[test]
    fn it_evaluates_addition_expressions() {
        assert_evaluated_object("1 + 1", Object::Integer(2));
    }

    #[test]
    fn it_evaluates_consecutive_subtraction_expressions() {
        assert_evaluated_object("5 - 1 - 2", Object::Integer(2));
    }

    #[test]
    fn it_evaluates_consecutive_addition_expressions() {
        assert_evaluated_object("1 + 1 + 1 + 1 + 1 + 1 + 1 + 1", Object::Integer(8));
    }

    #[test]
    fn it_evaluates_addition_before_multiplication_expressions() {
        assert_evaluated_object("5 + 5 * 5", Object::Integer(30));
    }

    #[test]
    fn it_evaluates_grouped_addition_before_multiplication_expressions() {
        assert_evaluated_object("(5 + 5) * 5", Object::Integer(50));
    }

    #[test]
    fn it_evaluates_complex_grouped_arithmetic_expressions() {
        assert_evaluated_object("10 / ((3 - 1) * 2 + 1)", Object::Integer(2));
    }

    #[test]
    fn it_evaluates_integer_equality_expressions() {
        assert_evaluated_object("1 == 1", Object::Boolean(true));
    }

    #[test]
    fn it_evaluates_integer_inequality_expressions() {
        assert_evaluated_object("1 != 1", Object::Boolean(false));
    }

    #[test]
    fn it_evaluates_assignment_statements() {
        assert_evaluated_object("a = 1; a", Object::Integer(1));
    }

    #[test]
    fn it_evaluates_conditional_statements_truthy() {
        let code = "if (true) { a = 1; b = 2 }";
        let parsed = parse(tokenize(code));
        let mut environment = Environment::new();
        let actual = eval(parsed, &mut environment);
        let mut expected_environment = Environment::new();

        expected_environment.set(String::from("a"), Object::Integer(1));
        expected_environment.set(String::from("b"), Object::Integer(2));

        assert_eq!(actual, Result::Void);
        assert_eq!(environment, expected_environment);
    }

    #[test]
    fn it_evaluates_conditional_statements_falsey() {
        let code = "if (false) { a = 1; b = 2 }";
        let parsed = parse(tokenize(code));
        let mut environment = Environment::new();
        let actual = eval(parsed, &mut environment);
        let expected_environment = Environment::new();

        assert_eq!(actual, Result::Void);
        assert_eq!(environment, expected_environment);
    }

    #[test]
    fn it_evaluates_function_application() {
        let code = "add = fn(a, b) { a + b }; add(1, 1)";

        assert_evaluated_object(code, Object::Integer(2));
    }

    #[test]
    fn it_evaluates_passing_functions_as_arguments() {
        let code = "ap = fn(f, i) { f(i) }; ap(fn(a) { a + 1 }, 1)";

        assert_evaluated_object(code, Object::Integer(2));
    }
}
