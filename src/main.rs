mod vm;

use std::fs;
use vm::environment::*;
use vm::interpreter::*;
use vm::lexer::*;
use vm::parser::*;
use vm::value_type::Type;

fn println_func(values: Vec<Value>) -> Option<Value> {
    println!("{:?}", values);
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

    let mut env = Environment::new();
    env.functions.insert(
        String::from("println"),
        Func {
            parameters: vec![Type::String],
            execution: println_func,
        },
    );

    let interpreter = Interpreter::new(program.clone(), env);
    println!(
        "{:?}",
        interpreter.run_function(String::from("main"), vec![])
    );
}
