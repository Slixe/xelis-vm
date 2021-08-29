mod interpreter;
mod lexer;
mod parser;
mod value_type;
mod operator;
mod environment;

use std::fs;
use interpreter::*;
use lexer::*;
use parser::*;
use environment::Environment;

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
    let program = build_program(&"examples/map.xel".into());
    println!("{}", serde_json::to_string_pretty(&program).unwrap());
    let interpreter = Interpreter::new(program, Environment::default(), load_library);
    for func in vec![
        "main",
        /*"lib_test",
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
        "semicolon_example",*/
    ] {
        println!("executing entrypoint: {}", func);
        println!(
            "Value returned {}",
            interpreter.run_function(func.to_string(), vec![]).unwrap()
        );
        println!("-------------------");
    }
}
