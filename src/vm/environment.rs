use super::interpreter::{Structure, Value};
use super::value_type::Type;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Environment {
    pub structures: HashMap<String, Structure>,
    pub functions: HashMap<String, Func>,
}

#[derive(Debug)]
pub struct Func {
    pub parameters: Vec<Type>,
    pub execution: fn(Vec<Value>) -> Option<Value>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            structures: HashMap::new(),
            functions: HashMap::new(),
        }
    }
}
