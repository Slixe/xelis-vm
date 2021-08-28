# Xelis VM

Xelis is an interpreted language developed in Rust. It supports constants, functions, while/foreach loops, arrays and structures. The syntax is strongly inspired by the Rust programming language.

The different primitive types present in the language are:
- String
- Number (represents an unsigned 64-bit number)
- BigInt
- Boolean
- Map

File extension is .xel

## Roadmap

### Main Parts
 - [x] Lexer
 - [x] Parser
 - [x] Interpreter 
 - [ ] Documentation

### Ideas 
- Implement operator priority logic
- Make debug messages prettier
- Implement the following statements:
  - for / for each
  - break
  - continue
  - scope

- Make sure that variables added in one scope cannot be used in another.
- Optimize the code to avoid cloning values.
- Realize the standard library
- Improve the way to implement the builtin functions (automatic parsing from the Value enum to the native rust values and vice versa?)

## How it works

First of all, in order for the code you write to work, it must pass through several stages before being executed.

The code goes through a [lexer](https://github.com/Slixe/xelis-vm/blob/master/src/lexer.rs), which transforms a string into a list of tokens. This list of tokens allows us to generate statements and/or expressions using the [parser](https://github.com/Slixe/xelis-vm/blob/master/src/parser.rs).

Then, once our program is created with the different instructions, we have to check their validity, (example: that a variable/function name is not already used, type checking, a if statement before a else statement, return a value only if the function allows it...)

Once these tests are passed, we can execute the code using the [interpreter](https://github.com/Slixe/xelis-vm/blob/master/src/interpreter.rs) which will take care of executing the different instructions.


## Testing

If you want to test the code from [tests.xel](https://github.com/Slixe/xelis-vm/blob/master/examples/tests.xel) make sure you have Cargo installed.
```bash
cargo run
```

## Benchmark

No benchmark yet