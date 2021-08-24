use crate::interpreter::Value;
use crate::lexer::*;
use crate::parser::Structure;

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Serialize, Deserialize, Debug, Clone, Eq, PartialEq)]
pub enum Literal {
    Null,
    String(String),
    Number(usize),
    Boolean(bool),
}

#[derive(Serialize, Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    String,
    Number,
    Boolean,
    Any,
    Structure(String),
    Array(Box<Type>),
    LibraryType(String, Box<Type>)
}

impl Type {
    pub fn get_type(structures: &HashMap<String, Structure>, value: &TokenValue) -> Option<Type> {
        let _type = match value.token {
            Token::ValNumber => Type::Number,
            Token::ValString => Type::String,
            Token::True | Token::False => Type::Boolean,
            Token::Identifier => match value.value.as_str() {
                "string" => Type::String,
                "number" => Type::Number,
                "bool" => Type::Boolean,
                _ => {
                    if structures.contains_key(&value.value) {
                        return Some(Type::Structure(value.value.clone()));
                    }

                    return None;
                }
            },
            _ => return None,
        };

        Some(_type)
    }

    pub fn get_type_of_value(value: &Value) -> Type {
        match &value {
            Value::Literal(l) => match l {
                Literal::Null => {
                    panic!("Expected a not-null value or a type");
                }
                Literal::Boolean(_) => Type::Boolean,
                Literal::Number(_) => Type::Number,
                Literal::String(_) => Type::String,
            },
            Value::Array(values) => {
                if values.len() > 0 {
                    return Type::Array(Box::new(Type::get_type_of_value(&values[0])));
                }

                panic!("Expected at least one value to determine Array type!")
            }
            Value::Structure(struct_type, _) => struct_type.clone()
                //Type::Structure(name.clone()),
        }
    }
}
