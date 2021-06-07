mod vm;

use std::fs;
use vm::interpreter::*;
use vm::lexer::*;
use vm::parser::*;
use vm::value_type::{Type, Literal};

fn println_func(values: Vec<Value>) -> Option<Value> {
    println!("{:?}", values);
    None
}

fn array_len(current_value: &mut Value, _: Vec<Value>) -> Option<Value> {
    match current_value {
        Value::Array(values) => Some(Value::Literal(Literal::Number(values.len()))),
        _ => panic!("Should not happen!")
    }
}

fn array_push(current_value: &mut Value, parameters: Vec<Value>) -> Option<Value> {
    let value = &parameters[0];
    let current_type = match Type::get_type_of_value(current_value) {
        Type::Array(v) => v,
        _ => panic!("Unexpected type? How is it possible!")
    };
    let value_type = Type::get_type_of_value(value);
    if  *current_type != value_type {
        panic!("Type is not valid for value, expected {:?} found {:?}", current_type, value_type);
    }

    if let Value::Array(ref mut values) = current_value {
        values.push(value.clone());
    }
    None
}

fn say_hello(_: &mut Value, _: Vec<Value>) -> Option<Value> {
    println!("Hello World from 'say_hello' on Test structure instance");
    None
}

fn main() {
    let code: String =
        fs::read_to_string("tests.xel").expect("Something went wrong reading the file");
    let lexer = Lexer::new(code);
    let tokens: Vec<TokenValue> = lexer.get();

    let parser = Parser::new(tokens);
    let program = match parser.build_program() {
        Ok(program) => program,
        Err(err) => panic!("Error while building program: {:?}", err),
    };

    println!("{}", serde_json::to_string_pretty(&program).unwrap());

    let mut env = Environment::new();
    env.bind_native_function(String::from("println"), println_func, vec![Type::Any]); //print in terminal a string
    env.bind_native_function_on_type(Type::Array(Box::new(Type::Any)), String::from("len"), array_len, vec![]); //return the len of array
    env.bind_native_function_on_type(Type::Array(Box::new(Type::Any)), String::from("push"), array_push, vec![Type::Any]); //add value in array
    env.bind_native_function_on_type(Type::Structure(String::from("Test")), String::from("say_hello"), say_hello, vec![]);

    for func in vec!["for_each", "compute_with_constant", "while_test", "struct_example", "function_call_example", "condition_example", "func_on_struct", "null_example"] {
        println!("executing entrypoint: {}", func);
        let interpreter = Interpreter::new(program.clone(), env.clone());
        println!(
            "Value returned {:?}",
            interpreter.run_function(func.to_string(), vec![])
        );
        println!("-------------------");
    }
}
