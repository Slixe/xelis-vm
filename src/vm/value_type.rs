use super::lexer::*;
use super::parser::Structure;

#[derive(Debug, Clone)]
pub enum Literal {
    Null,
    String(String),
    Number(usize),
    Boolean(bool),
}

#[derive(Debug)]
pub enum Type {
    String,
    Number,
    Boolean,
    Struct(String),
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
                            return Some(Type::Struct(value.value.clone()));
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
