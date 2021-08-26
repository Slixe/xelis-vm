mod interpreter;
mod lexer;
mod parser;
mod value_type;
mod operator;

use std::fs;
use interpreter::*;
use lexer::*;
use parser::*;
use value_type::{Literal, Type};

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

fn array_push(current_value: &mut Value, parameters: Vec<Value>) -> Option<Value> {
    let value = &parameters[0];
    let current_type = match Type::get_type_of_value(current_value) {
        Type::Array(v) => v,
        _ => panic!("Unexpected type? How is it possible!"),
    };
    let value_type = Type::get_type_of_value(value);
    if *current_type != value_type {
        panic!(
            "Type is not valid for value, expected {} found {}",
            current_type, value_type
        );
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

fn load_library(path: String) -> Option<Library> {
    let program = build_program(&format!("examples/{}", &path));
    Some(Library::new(path, program))
}

fn build_program(path: &String) -> Program {
    let code: String =
    fs::read_to_string(path).expect("Something went wrong reading the file");
    let lexer = Lexer::new(code);
    let parser = Parser::new(lexer.get());
    let program = match parser.build_program(load_library) {
        Ok(program) => program,
        Err(err) => panic!("Error while building program: {:?}", err),
    };

    program
}

fn main() {
    let program = build_program(&"examples/factorial.xel".into());
    let mut env = Environment::new();
    {
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
        env.bind_native_function_on_type(
            Type::Structure(String::from("Test")),
            String::from("say_hello"),
            say_hello,
            vec![],
        );
    } //End environnement

    //Testing
    println!("{}", serde_json::to_string_pretty(&program).unwrap());
    let interpreter = Interpreter::new(program, env, load_library);
    for func in vec![
        "main",
        "lib_test",
        "for_each",
        "compute_with_constant",
        "while_test",
        "struct_example",
        "function_call_example",
        "condition_example",
        "func_on_struct",
        "null_example",
        "scope_example",
        "variable_declaration",
        "semicolon_example",
    ] {
        println!("executing entrypoint: {}", func);
        println!(
            "Value returned {}",
            interpreter.run_function(func.to_string(), vec![]).unwrap()
        );
        println!("-------------------");
    }
}
