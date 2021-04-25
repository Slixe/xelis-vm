use super::operator::*;
use super::parser::*;
use super::value_type::*;

use std::collections::HashMap;

pub struct Interpreter {
    program: Program,
}

#[derive(Debug)]
pub struct Scope {
    variables: HashMap<String, Literal>,
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
        return Interpreter { program: program };
    }

    pub fn test(&self) {
        let scope = Scope::new();

        let value1 = Expression::Value(Literal::Number(10));
        let value2 = Expression::Value(Literal::Number(2));
        let value3 = Expression::Value(Literal::Number(5));

        // 10 * (2+5)
        let expr = Expression::Operator(Operator::OperatorMultiply(
            Box::new(value1),
            Box::new(Expression::Operator(Operator::OperatorPlus(
                Box::new(value2),
                Box::new(value3),
            ))),
        ));

        let value = self.execute_expression(&expr, &scope);
        println!("{:?}", value);
    }

    pub fn run_function(&self, entry: String, parameters: Vec<Literal>) -> Option<Literal> {
        let func: &Function = match self.get_function(&entry) {
            Some(func) => func,
            None => {
                panic!("Func not found");
            }
        };

        if !func.entry {
            panic!("Not entrypoint!");
        }

        let scope = Scope::new();
        self.execute_function(func, parameters, scope)
    }

    fn get_function(&self, entry: &String) -> Option<&Function> {
        return self
            .program
            .functions
            .iter()
            .filter(|f| f.name == *entry)
            .nth(0);
    }

    fn execute_function_and_params(
        &self,
        name: &String,
        parameters: &Vec<Expression>,
    ) -> Option<Literal> {
        let mut values: Vec<Literal> = vec![];
        let scope = Scope::new();

        let func = self.get_function(name)?;

        for e in parameters {
            values.push(self.execute_expression(&e, &scope)?);
        }

        self.execute_function(&func, values, scope)
    }

    fn execute_function(
        &self,
        func: &Function,
        mut parameters: Vec<Literal>,
        mut scope: Scope,
    ) -> Option<Literal> {
        if parameters.len() != func.parameters.len() {
            panic!("Parameters / values length mismatch");
        }

        let mut i = 0;
        while i <= parameters.len() {
            scope.set_variable_value(&func.parameters[i].name, parameters.remove(0));
            i += 1;
        }

        self.execute_statements(&func.statements, &scope)
    }

    fn execute_statements(&self, statements: &Vec<Statement>, scope: &Scope) -> Option<Literal> {
        for statement in statements {
            if let Some(value) = self.execute_statement(statement, &scope) {
                return Some(value);
            }
        }
        None
    }

    fn execute_statement(&self, statement: &Statement, scope: &Scope) -> Option<Literal> {
        match statement {
            Statement::Expression(exp) => {
                self.execute_expression(&exp, &scope);
            }
            Statement::If(value) => {
                if let Some(result) = self.execute_expression(&value.condition, &scope) {
                    let r: bool = match result {
                        Literal::Boolean(r) => r,
                        _ => panic!("Expected boolean result for this condition"),
                    };

                    if r {
                        return self.execute_statements(&value.body, scope); //TODO copy scope into a new one
                    }
                }
            }
            Statement::Return(value) => {
                match value {
                    Some(value) => return Some(self.execute_expression(value, scope)?),
                    None => return None,
                };
            }
            _ => {
                panic!("How is it possible?");
            }
        };

        None
    }

    fn get_numbers(
        &self,
        left: &Expression,
        right: &Expression,
        scope: &Scope,
    ) -> Option<(usize, usize)> {
        if let (Literal::Number(left_val), Literal::Number(right_val)) = (
            self.execute_expression(left, scope)?,
            self.execute_expression(right, scope)?,
        ) {
            return Some((left_val, right_val));
        } else {
            panic!("Only number are allowed for multiply operator!");
        }
    }

    fn get_booleans(
        &self,
        left: &Expression,
        right: &Expression,
        scope: &Scope,
    ) -> Option<(bool, bool)> {
        if let (Literal::Boolean(left_val), Literal::Boolean(right_val)) = (
            self.execute_expression(left, scope)?,
            self.execute_expression(right, scope)?,
        ) {
            return Some((left_val, right_val));
        } else {
            panic!("Only boolean are allowed!");
        }
    }

    fn execute_expression(&self, expr: &Expression, scope: &Scope) -> Option<Literal> {
        match expr {
            Expression::Value(val) => Some(val.clone()),
            Expression::Variable(val) => match scope.get_variable_value(val) {
                Some(val) => Some(val.clone()),
                None => panic!(format!("Variable '{}' not found. {:?}", val, scope)),
            }, //TODO: return value of variable
            Expression::ArrayCall(val, index) => None, //TODO:
            Expression::ArrayConstructor(expressions) => {
                let mut values: Vec<Literal> = vec![];
                for e in expressions {
                    values.push(self.execute_expression(e, scope)?);
                }

                None
                //Some(Literal::Array(values))
            }
            Expression::FunctionCall(func_name, params) => {
                self.execute_function_and_params(func_name, params)
            }
            Expression::Operator(operator) => match operator {
                Operator::Dot(left, right) => None,
                Operator::OperatorAssign(left, right) => None,
                Operator::OperatorPlusAssign(left, right) => None,
                Operator::OperatorMinusAssign(left, right) => None,
                Operator::OperatorDivideAssign(left, right) => None,
                Operator::OperatorMultiplyAssign(left, right) => None,
                Operator::OperatorAnd(left, right) => {
                    if let Some((left, right)) = self.get_booleans(left, right, scope) {
                        Some(Literal::Boolean(left && right))
                    } else {
                        None
                    }
                }
                Operator::OperatorOr(left, right) => {
                    if let Some((left, right)) = self.get_booleans(left, right, scope) {
                        Some(Literal::Boolean(left || right))
                    } else {
                        None
                    }
                }
                Operator::OperatorEquals(left, right) => {
                    if let Some((left, right)) = self.get_booleans(left, right, scope) {
                        Some(Literal::Boolean(left == right))
                    } else {
                        None
                    }
                }
                Operator::OperatorNotEquals(left, right) => {
                    if let Some((left, right)) = self.get_booleans(left, right, scope) {
                        Some(Literal::Boolean(left != right))
                    } else {
                        None
                    }
                }
                Operator::OperatorGreaterThan(left, right) => {
                    if let Some((left, right)) = self.get_booleans(left, right, scope) {
                        Some(Literal::Boolean(left > right))
                    } else {
                        None
                    }
                }
                Operator::OperatorGreaterOrEqual(left, right) => {
                    if let Some((left, right)) = self.get_booleans(left, right, scope) {
                        Some(Literal::Boolean(left >= right))
                    } else {
                        None
                    }
                }
                Operator::OperatorLessThan(left, right) => {
                    if let Some((left, right)) = self.get_booleans(left, right, scope) {
                        Some(Literal::Boolean(left < right))
                    } else {
                        None
                    }
                }
                Operator::OperatorLessOrEqual(left, right) => {
                    if let Some((left, right)) = self.get_booleans(left, right, scope) {
                        Some(Literal::Boolean(left <= right))
                    } else {
                        None
                    }
                }
                Operator::OperatorModulo(left, right) => {
                    if let Some((left, right)) = self.get_numbers(left, right, scope) {
                        Some(Literal::Number(left % right))
                    } else {
                        None
                    }
                }
                Operator::OperatorBitwiseLeft(left, right) => {
                    if let Some((left, right)) = self.get_numbers(left, right, scope) {
                        Some(Literal::Number(left << right))
                    } else {
                        None
                    }
                }
                Operator::OperatorBitwiseRight(left, right) => {
                    if let Some((left, right)) = self.get_numbers(left, right, scope) {
                        Some(Literal::Number(left >> right))
                    } else {
                        None
                    }
                }
                Operator::OperatorMultiply(left, right) => {
                    if let Some((left, right)) = self.get_numbers(left, right, scope) {
                        Some(Literal::Number(left * right))
                    } else {
                        None
                    }
                }
                Operator::OperatorDivide(left, right) => {
                    if let Some((left, right)) = self.get_numbers(left, right, scope) {
                        Some(Literal::Number(left / right))
                    } else {
                        None
                    }
                }
                Operator::OperatorPlus(left, right) => {
                    let left = self.execute_expression(left, scope)?;
                    let right = self.execute_expression(right, scope)?;

                    match left {
                        Literal::Number(left_val) => match right {
                            Literal::Number(val) => Some(Literal::Number(left_val + val)),
                            Literal::String(val) => {
                                Some(Literal::String(format!("{}{}", left_val, val)))
                            }
                            _ => panic!("Error! Invalid type for this operator"),
                        },
                        Literal::String(left_val) => match right {
                            Literal::String(val) => Some(Literal::String(left_val + &val)),
                            Literal::Number(val) => {
                                Some(Literal::String(format!("{}{}", left_val, val)))
                            }
                            Literal::Boolean(val) => {
                                Some(Literal::String(format!("{}{}", left_val, val)))
                            }
                            Literal::Null => {
                                Some(Literal::String(format!("{}{}", left_val, "null")))
                            }
                        },
                        _ => panic!("Error! Invalid type for + operator"),
                    }
                }
                Operator::OperatorMinus(left, right) => {
                    if let Some((left, right)) = self.get_numbers(left, right, scope) {
                        Some(Literal::Number(left - right))
                    } else {
                        None
                    }
                }
            },
            Expression::SubExpression(sub) => self.execute_expression(sub, scope),
        }
    }
}
