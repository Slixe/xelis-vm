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
    if let Value::Array(ref mut values) = current_value {
        values.push(parameters[0].clone());
    }
    None
}

fn main() {
    let code: String =
        fs::read_to_string("priority.xel").expect("Something went wrong reading the file");
    let lexer = Lexer::new(code);
    let tokens: Vec<TokenValue> = lexer.get();

    let parser = Parser::new(tokens);
    let program = match parser.build_program() {
        Ok(program) => program,
        Err(err) => panic!("Error while building program: {:?}", err),
    };

    println!("{}", serde_json::to_string_pretty(&program).unwrap());

    let mut env = Environment::new();
    env.bind_native_function(String::from("println"), println_func, vec![Type::String]); //print in terminal a string
    env.bind_native_function_on_type(Type::Array(Box::new(Type::Number)), String::from("len"), array_len, vec![]); //return the len of array
    env.bind_native_function_on_type(Type::Array(Box::new(Type::Number)), String::from("push"), array_push, vec![Type::Number]); //add value in array

    let interpreter = Interpreter::new(program.clone(), env);
    println!(
        "{:?}",
        interpreter.run_function(String::from("main"), vec![])
    );
}
