use crate::parser::Identifier;
use crate::Object;
use std::collections::HashMap;

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

    pub fn get(&self, key: &Identifier) -> Option<&Object> {
        self.variables.get(&key.name)
    }

    pub fn set(&mut self, key: String, object: Object) {
        self.variables.insert(key, object);
    }
}
