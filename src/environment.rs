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
        env.bind_native_function("bigInt".into(), big_int, vec![Type::Any]); //transform string/number to bigInt

        //Map implementation
        env.bind_native_function(String::from("map"), map_new, vec![]);
        env.bind_native_function_on_type(Type::Map, String::from("put"), map_put, vec![Type::String, Type::Any]);
        env.bind_native_function_on_type(Type::Map, String::from("has"), map_has, vec![Type::String]);
        env.bind_native_function_on_type(Type::Map, String::from("get"), map_get, vec![Type::String]);
        env.bind_native_function_on_type(Type::Map, String::from("remove"), map_remove, vec![Type::String]);
        env.bind_native_function_on_type(Type::Map, String::from("keys"), map_keys, vec![]);
        env.bind_native_function_on_type(Type::Map, String::from("len"), map_len, vec![]);

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

fn big_int(parameters: Vec<Value>) -> Option<Value> {
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

fn get_map_from_value(current_value: &mut Value) -> &mut HashMap<String, Value> {
    if let Value::Literal(l) = current_value {
        if let Literal::Map(ref mut map) = l {
            return map
        }
    }

    panic!("Map not found!")
}

fn get_string_from_value(value: Value) -> String {
    match value {
        Value::Literal(l) => match l {
            Literal::String(s) => s,
            _ => panic!("Invalid map key! Expected a string")
        },
        _ => panic!("Invalid map key! Expected a literal!")
    }
}

fn map_new(_: Vec<Value>) -> Option<Value> {
    Some(Value::Literal(Literal::Map(HashMap::new())))
}

fn map_put(current_value: &mut Value, mut parameters: Vec<Value>) -> Option<Value> {
    let key = get_string_from_value(parameters.remove(0));
    let value = parameters.remove(0);

    let map = get_map_from_value(current_value);
    map.insert(key, value)
}

fn map_has(current_value: &mut Value, mut parameters: Vec<Value>) -> Option<Value> {
    let map = get_map_from_value(current_value);
    let key = get_string_from_value(parameters.remove(0));

    Some(Value::Literal(Literal::Boolean(map.contains_key(&key))))
}

fn map_get(current_value: &mut Value, mut parameters: Vec<Value>) -> Option<Value> {
    let map = get_map_from_value(current_value);
    let key = get_string_from_value(parameters.remove(0));

    match map.get(&key) {
        Some(value) => Some(value.clone()),
        None => Some(Value::Literal(Literal::Null))
    }
}

fn map_remove(current_value: &mut Value, mut parameters: Vec<Value>) -> Option<Value> {
    let map = get_map_from_value(current_value);
    let key = get_string_from_value(parameters.remove(0));

    match map.remove(&key) {
        Some(value) => Some(value),
        None => Some(Value::Literal(Literal::Null))
    }
}

fn map_keys(current_value: &mut Value, _: Vec<Value>) -> Option<Value> {
    let map = get_map_from_value(current_value);

    let mut keys: Vec<Value> = vec![];
    for key in map.keys() {
        keys.push(Value::Literal(Literal::String(key.clone())));
    }

    Some(Value::Array(keys))
}

fn map_len(current_value: &mut Value, _: Vec<Value>) -> Option<Value> {
    let map = get_map_from_value(current_value);

    Some(Value::Literal(Literal::Number(map.len() as u64)))
}