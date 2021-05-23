mod vm;

use std::fs;
use vm::interpreter::*;
use vm::lexer::*;
use vm::parser::*;

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

    println!("{:?}", program);

    let interpreter = Interpreter::new(program);
    println!(
        "{:?}",
        interpreter.run_function(
            String::from("main"),
            vec![/*Literal::Number(1), Literal::Number(2)*/]
        )
    );
}
