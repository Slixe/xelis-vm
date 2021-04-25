mod vm;

use std::fs;
use vm::interpreter::*;
use vm::lexer::*;
use vm::parser::*;
use vm::value_type::*;

fn main() {
    let code: String =
        fs::read_to_string("program.xel").expect("Something went wrong reading the file");
    let lexer = Lexer::new(code);
    let tokens: Vec<TokenValue> = lexer.get();

    let parser = Parser::new(tokens);
    let program = match parser.build_program() {
        Ok(program) => program,
        Err(err) => panic!("Error while building program: {:?}", err),
    };

    println!("{:?}", program);

    let interpreter = Interpreter::new(program);
    interpreter.test();
    println!(
        "{:?}",
        interpreter.run_function(
            String::from("bar"),
            vec![Literal::Number(1), Literal::Number(2)]
        )
    );
}
