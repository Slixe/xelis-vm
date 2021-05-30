use super::lexer::*;
use super::parser::Structure;

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone, Eq, PartialEq)]
pub enum Literal {
    Null,
    String(String),
    Number(usize),
    Boolean(bool),
}

#[derive(Serialize, Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
pub enum Type { //TODO Implement "Any" Properly
    String,
    Number,
    Boolean,
    Structure(String),
    Array(Box<Type>),
}

impl Type {
    pub fn get_type(structures: &Vec<Structure>, value: &TokenValue) -> Option<Type> {
        let _type = match value.token {
            Token::ValNumber => Type::Number,
            Token::ValString => Type::String,
            Token::True | Token::False => Type::Boolean,
            Token::Identifier => match value.value.as_str() {
                "string" => Type::String,
                "number" => Type::Number,
                "bool" => Type::Boolean,
                _ => {
                    for structure in structures {
                        if structure.name == value.value {
                            return Some(Type::Structure(value.value.clone()));
                        }
                    }

                    return None;
                }
            },
            _ => return None,
        };

        Some(_type)
    }
}
