use crate::parser;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Void,
}

pub fn eval(program: parser::Program) -> Object {
    Object::Void
}
