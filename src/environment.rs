use crate::value_type::{Literal, Type};
use crate::interpreter::{Scope, Value, FunctionType, FuncType, Func};
use crate::parser::Structure;

use std::collections::HashMap;
use num_bigint::{ToBigInt, BigInt};

#[derive(Clone)]
pub struct Environment {
    pub scope: Scope,
    pub structures: HashMap<String, Structure>,
    pub functions: HashMap<String, FunctionType>,
    pub type_functions: HashMap<Type, HashMap<String, FunctionType>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            scope: Scope::new(),
            structures: HashMap::new(),
            functions: HashMap::new(),
            type_functions: HashMap::new(),
        }
    }

    pub fn default() -> Environment {
        let mut env = Environment::new();

        env.bind_native_function(String::from("println"), println_func, vec![Type::Any]); //print in terminal a string
        env.bind_native_function_on_type(
            Type::Array(Box::new(Type::Any)),
            String::from("len"),
            array_len,
            vec![],
        ); //return the len of array
        env.bind_native_function_on_type(
            Type::Array(Box::new(Type::Any)),
            String::from("push"),
            array_push,
            vec![Type::Any],
        ); //add value in array
        env.bind_native_function("bigInt".into(), bigInt, vec![Type::Any]);

        env
    }

    pub fn bind_native_function(
        &mut self,
        name: String,
        function: fn(Vec<Value>) -> Option<Value>,
        parameters: Vec<Type>,
    ) {
        if let Some(_) = self.functions.insert(
            name,
            FunctionType::Builtin(Func {
                parameters: parameters,
                execution: function,
            }),
        ) {
            panic!("Function already exist!");
        }
    }

    pub fn bind_native_function_on_type(
        &mut self,
        value_type: Type,
        name: String,
        function: fn(&mut Value, Vec<Value>) -> Option<Value>,
        parameters: Vec<Type>,
    ) {
        let map: &mut HashMap<String, FunctionType> = match self.type_functions.get_mut(&value_type)
        {
            Some(v) => v,
            None => {
                self.type_functions
                    .insert(value_type.clone(), HashMap::new());
                match self.type_functions.get_mut(&value_type) {
                    Some(v) => v,
                    None => panic!("How is it possible ? Map is inserted on previous line!"),
                }
            }
        };
        if let Some(_) = map.insert(
            name,
            FunctionType::Type(FuncType {
                parameters: parameters,
                execution: function,
            }),
        ) {
            panic!("Function already exist!");
        }
    }
}

fn println_func(values: Vec<Value>) -> Option<Value> {
    println!("{}", values[0]);
    None
}

fn array_len(current_value: &mut Value, _: Vec<Value>) -> Option<Value> {
    match current_value {
        Value::Array(values) => Some(Value::Literal(Literal::Number(values.len() as u64))),
        _ => panic!("Should not happen!"),
    }
}

fn array_push(current_value: &mut Value, mut parameters: Vec<Value>) -> Option<Value> {
    let value = parameters.remove(0);
    let current_type = match Type::get_type_of_value(current_value) {
        Type::Array(v) => v,
        _ => panic!("Unexpected type? How is it possible!"),
    };
    let value_type = Type::get_type_of_value(&value);
    if *current_type != value_type {
        panic!(
            "Type is not valid for value, expected {} found {}",
            current_type, value_type
        );
    }

    if let Value::Array(ref mut values) = current_value {
        values.push(value);
    }
    None
}

fn bigInt(parameters: Vec<Value>) -> Option<Value> {
    let value = &parameters[0];
    let res = match value {
        Value::Literal(l) => match l {
            Literal::Number(n) => Value::Literal(Literal::BigInt(n.to_bigint().unwrap())),
            Literal::String(s) => Value::Literal(Literal::BigInt(s.parse::<BigInt>().unwrap())),
            _ => panic!("Invalid parameter, only number is supported")
        }
        _ => panic!("Invalid parameter, only number supported")
    };

    Some(res)
}