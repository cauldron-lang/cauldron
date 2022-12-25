use crate::parser::Identifier;
use crate::Object;
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    variables: HashMap<String, Object>,
    pub export: Option<Box<Object>>,
    pub source_context: PathBuf,
}

impl Environment {
    pub fn new(source_context: PathBuf) -> Self {
        Self {
            variables: HashMap::new(),
            export: None,
            source_context,
        }
    }

    pub fn get(&self, key: &Identifier) -> Option<&Object> {
        self.variables.get(&key.name)
    }

    pub fn set(&mut self, key: String, object: Object) {
        self.variables.insert(key, object);
    }

    pub fn set_export(&mut self, export: Option<Box<Object>>) {
        self.export = export;
    }
}
