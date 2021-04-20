use super::parser::*;
use super::operator::*;
use super::value_type::*;

use std::collections::HashMap;

pub struct Interpreter {
    program: Program,
}

pub struct Scope {
    variables: HashMap<String, Literal>
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            variables: HashMap::new(),
        }
    }

    pub fn get_variable_value(&self, name: &str) -> Option<&Literal> {
        self.variables.get(name)
    }

    pub fn set_variable_value(&mut self, name: &str, value: Literal) {
        self.variables.insert(name.to_string(), value);
    }
}

impl Interpreter {
    pub fn new(program: Program) -> Interpreter {
        return Interpreter {
            program: program,
        }
    }

    pub fn run_function(&self, entry: String, parameters: Vec<Literal>) -> Option<Literal> {
        let func: &Function = match self.get_function(&entry) {
            Some(func) => func,
            None => {
                panic!("Func not found");
            },
        };

        if parameters.len() != func.parameters.len() {
            panic!("Not enough parameters");
        }

        let scope = Scope::new();
        self.execute_statements(&func.statements, &scope)
    }

    fn get_function(&self, entry: &String) -> Option<&Function> {
        return self.program.functions.iter().filter(|f| f.name == *entry).nth(0);
    }

    fn execute_statements(&self, statements: &Vec<Statement>, scope: &Scope) -> Option<Literal> {
        for statement in statements {
            self.execute_statement(statement, &scope);
        }
        None
    }

    fn execute_statement(&self, statement: &Statement, scope: &Scope) -> Option<Literal> {
        match statement {
            Statement::Expression(exp) => {
                self.execute_expression(&exp, &scope);
            },
            Statement::If(value) => {
                if let Some(result) = self.execute_expression(&value.condition, &scope) {
                    let r: bool = match result {
                        Literal::Boolean(r) => r,
                        _ => panic!("Expected boolean result for this condition")
                    };

                    if r {
                        self.execute_statements(&value.body, scope); //TODO copy scope into a new one
                    }
                }
            },
            _ => {
                panic!("How is it possible?");
            }
        };

        None
    }

    fn execute_expression(&self, expr: &Expression, scope: &Scope) -> Option<Literal> {
        match expr {
            Expression::Value(_) => None,//Some(val),
            Expression::SubExpression(sub) => self.execute_expression(sub, scope),
            _ => None,
        }
    }
}