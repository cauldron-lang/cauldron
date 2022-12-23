use crate::parser::{Arguments, Block};
use crate::Environment;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct MapKey {
    pub name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Void,
    Integer(i32),
    String(String),
    Error(String),
    Boolean(bool),
    // FIXME: Arguments should be renamed to Parameters here
    Function(Arguments, Block, Environment),
    Vector(Vec<Object>),
    Map(HashMap<MapKey, Box<Object>>),
    BIF(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Result {
    Void,
    Object(Object),
}
