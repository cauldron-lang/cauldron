use std::collections::HashMap;

use crate::parser::{self, Arguments, Block, Identifier, InfixOperator, PrefixOperator, Statement};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Void,
    Integer(i32),
    Error(String),
    Boolean(bool),
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
    fn new() -> Self {
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

struct Evaluator {
    environment: Environment,
}

impl Evaluator {
    fn new(environment: Environment) -> Self {
        Self { environment }
    }

    fn eval_expression(&mut self, expression: parser::Expression) -> Object {
        match expression {
            parser::Expression::Call(_, _) => todo!(),
            parser::Expression::Integer(integer) => {
                let integer = integer.parse::<i32>();

                match integer {
                    Ok(integer) => Object::Integer(integer),
                    Err(parse_error) => Object::Error(format!("{:?}", parse_error)),
                }
            }
            parser::Expression::Identifier(identifier) => match self.environment.get(&identifier) {
                Some(object) => object.clone(),
                None => Object::Error(format!(
                    "Environment missing identifier {:?}",
                    identifier.clone()
                )),
            },
            parser::Expression::Boolean(boolean) => Object::Boolean(boolean),
            parser::Expression::Prefix(prefix_operator, expression) => {
                let right = self.eval_expression(*expression);

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
                let left_object = self.eval_expression(*left_expression);
                let right_object = self.eval_expression(*right_expression);

                match (left_object.clone(), right_object.clone()) {
                    (Object::Integer(left_integer), Object::Integer(right_integer)) => {
                        match infix_operator {
                            InfixOperator::Minus => Object::Integer(left_integer - right_integer),
                            InfixOperator::Plus => Object::Integer(left_integer + right_integer),
                            InfixOperator::Equals => Object::Boolean(left_integer == right_integer),
                            InfixOperator::NotEquals => {
                                Object::Boolean(left_integer != right_integer)
                            }
                            InfixOperator::GreaterThan => {
                                Object::Boolean(left_integer > right_integer)
                            }
                            InfixOperator::LessThan => {
                                Object::Boolean(left_integer < right_integer)
                            }
                            InfixOperator::Multiply => {
                                Object::Integer(left_integer * right_integer)
                            }
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
            parser::Expression::Function(arguments, block) => {
                Object::Function(arguments, block, Environment::new())
            }
            parser::Expression::Block(_) => todo!(),
        }
    }

    fn eval_block(&mut self, block: parser::Block) -> Result {
        Result::Void
    }

    fn eval_statement(&mut self, statement: parser::Statement) -> Result {
        match statement {
            parser::Statement::Assignment(identifier, expression) => {
                let object = self.eval_expression(expression);

                self.environment.set(identifier, object);
                Result::Void
            }
            parser::Statement::Expression(expression) => {
                Result::Object(self.eval_expression(expression))
            }
            parser::Statement::Conditional(
                parser::Condition::Expression(condition_expression),
                consequence,
            ) => match self.eval_expression(condition_expression) {
                Object::Boolean(true) => self.eval_block(consequence),
                object => Result::Object(Object::Error(String::from(format!(
                    "Condition expression must evalate to boolean, instead received {:?}",
                    object
                )))),
            },
            parser::Statement::Invalid(_) => todo!(),
        }
    }

    fn eval_statements(&mut self, statements: Vec<parser::Statement>) -> Result {
        let mut object = Result::Void;

        for statement in statements {
            object = self.eval_statement(statement);
        }

        object
    }
}

pub fn eval(program: parser::Program) -> Result {
    let mut environment = Environment::new();
    let mut evaluator = Evaluator::new(environment);

    evaluator.eval_statements(program.statements)
}

#[cfg(test)]
mod tests {
    use super::{eval, Object, Result};
    use crate::{lexer::tokenize, parser::parse};

    fn assert_evaluated_object(code: &str, object: Object) {
        let actual = eval(parse(tokenize(code)));

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
}
