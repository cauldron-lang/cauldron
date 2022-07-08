use std::collections::HashMap;

use crate::parser;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Void,
    Integer(i32),
    Error(String),
}

struct Environment {
    variables: HashMap<String, Object>,
}

impl Environment {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
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
            parser::Expression::Identifier(_) => todo!(),
            parser::Expression::Boolean(_) => todo!(),
            parser::Expression::Prefix(_, _) => todo!(),
            parser::Expression::Infix(_, _, _) => todo!(),
            parser::Expression::Invalid(_) => todo!(),
        }
    }

    fn eval_statement(&mut self, statement: parser::Statement) -> Object {
        match statement {
            parser::Statement::Assignment(_, _) => todo!(),
            parser::Statement::Block(_) => todo!(),
            parser::Statement::Expression(expression) => self.eval_expression(expression),
            parser::Statement::Conditional(_, _) => todo!(),
            parser::Statement::Invalid(_) => todo!(),
        }
    }

    fn eval_statements(&mut self, statements: Vec<parser::Statement>) -> Object {
        let mut object = Object::Void;

        for statement in statements {
            object = self.eval_statement(statement);
        }

        object
    }
}

pub fn eval(program: parser::Program) -> Object {
    let mut environment = Environment::new();

    environment.eval_statements(program.statements)
}

#[cfg(test)]
mod tests {
    use super::{eval, Object};
    use crate::{lexer::tokenize, parser::parse};

    #[test]
    fn it_evaluates_expressions() {
        let expectations = [("1", Object::Integer(1))];

        for (code, expected) in expectations {
            let actual = eval(parse(tokenize(code)));

            assert_eq!(actual, expected);
        }
    }
}
