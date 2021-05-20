use super::operator::*;
use super::parser::*;
use super::value_type::*;

use std::collections::HashMap;

pub struct Interpreter { //TODO HashMap<String, Structure>
    program: Program,
}

#[derive(Debug, Clone)]
pub enum Value {
    Array(Vec<Value>),
    Literal(Literal),
    Structure(String, HashMap<String, Value>),
}

#[derive(Debug)]
pub struct Variable {
    pub value: Value,
    pub value_type: Type,
}

#[derive(Debug)]
pub struct Scope {
    variables: HashMap<String, Variable>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            variables: HashMap::new(),
        }
    }

    pub fn get_variable(&self, name: &str) -> Option<&Variable> {
        self.variables.get(name)
    }

    pub fn register_variable(&mut self, name: &str, value: Variable) {
        if let Some(v) = self.variables.insert(name.to_string(), value) {
            panic!("Variable '{}' already exist with value: {:?}", name, v);
        }
    }

    pub fn set_variable_value(&mut self, name: &str, value: Value) {
        let var: &mut Variable = match self.variables.get_mut(name) {
            Some(v) => v,
            None => panic!("Trying to set value on a inexistant variable")
        };

        var.value = value;
    }
}

impl Interpreter {
    pub fn new(program: Program) -> Interpreter {
        return Interpreter { program: program };
    }

    pub fn run_function(&self, entry: String, parameters: Vec<Literal>) -> Option<Value> {
        let func: &Function = match self.get_function(&entry) {
            Some(func) => func,
            None => {
                panic!(format!("Entrypoint '{}' not found", entry));
            }
        };

        if !func.entry {
            panic!(format!("Function '{}' is not an entrypoint!", entry));
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
    ) -> Option<Value> {
        let mut values: Vec<Literal> = vec![];
        let mut scope = Scope::new();

        let func = self.get_function(name)?;

        for e in parameters {
            values.push(self.execute_expression_and_expect_literal(&e, &mut scope)?);
        }

        self.execute_function(&func, values, scope)
    }

    fn execute_function(
        &self,
        func: &Function,
        parameters: Vec<Literal>,
        mut scope: Scope,
    ) -> Option<Value> {
        if parameters.len() != func.parameters.len() {
            panic!("Parameters / values length mismatch");
        }

        let mut i = 0;
        for param in parameters {
            scope.set_variable_value(&func.parameters[i].name, Value::Literal(param));
            i += 1;
        }

        self.execute_statements(&func.statements, &mut scope)
    }

    fn execute_statements(&self, statements: &Vec<Statement>, scope: &mut Scope) -> Option<Value> {
        for statement in statements {
            if let Some(value) = self.execute_statement(statement, scope) {
                return Some(value);
            }
        }
        None
    }

    fn execute_statement(&self, statement: &Statement, scope: &mut Scope) -> Option<Value> {
        match statement {
            Statement::Expression(exp) => {
                self.execute_expression(&exp, scope);
            }
            Statement::If(value) => {
                if let Some(result) = self.execute_expression(&value.condition, scope) {
                    let r: bool = match result {
                        Value::Literal(l) => match l {
                            Literal::Boolean(r) => r,
                            _ => panic!("Expected boolean result for this condition")
                        }
                        _ => panic!("Expected a literal"),
                    };

                    if r {
                        return self.execute_statements(&value.body, scope); //TODO copy scope into a new one
                    }
                }
            }
            Statement::Variable(var) => {
                let value: Value;
                let value_type: Type;

                if let Some(result) = &var.value {
                    value = self.execute_expression(result, scope)?;
                } else {
                    if let Some(result) = &var.value_type {
                        value = match result {
                            Type::Number => Value::Literal(Literal::Number(0)),
                            _ => Value::Literal(Literal::Null)
                        };
                    }
                    else {
                        panic!("Expected an expression or a type");
                    }
                }

                value_type = match &value {
                    Value::Literal(l) => match l {
                        Literal::Null => {
                            if let Some(value) = &var.value_type {
                                value.clone()
                            } else {
                                panic!("Expected a not-null value or a type");
                            }
                        }
                        Literal::Boolean(_) => Type::Boolean,
                        Literal::Number(_) => Type::Number,
                        Literal::String(_) => Type::String
                    }
                    Value::Array(_) => Type::Array(Box::new(Type::Number)), //TODO type
                    Value::Structure(name, _) => Type::Struct(name.clone()), //TODO better structure
                };

                scope.register_variable(&var.name, Variable {
                    value: value,
                    value_type
                });
            }
            Statement::Return(value) => {
                match value {
                    Some(value) => return Some(self.execute_expression(value, scope)?),
                    None => return None,
                };
            }
            _ => {
                panic!(format!("How is it possible?\n {:?}", statement));
            }
        };

        None
    }

    fn get_numbers(
        &self,
        left: &Expression,
        right: &Expression,
        scope: &mut Scope,
    ) -> Option<(usize, usize)> {
        if let (Literal::Number(left_val), Literal::Number(right_val)) = (
            self.execute_expression_and_expect_literal(left, scope)?,
            self.execute_expression_and_expect_literal(right, scope)?,
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
        scope: &mut Scope,
    ) -> Option<(bool, bool)> {
        if let (Literal::Boolean(left_val), Literal::Boolean(right_val)) = (
            self.execute_expression_and_expect_literal(left, scope)?,
            self.execute_expression_and_expect_literal(right, scope)?,
        ) {
            return Some((left_val, right_val));
        } else {
            panic!("Only boolean are allowed!");
        }
    }

    fn execute_expression_and_expect_literal(&self, expr: &Expression, scope: &mut Scope) -> Option<Literal> {
        match self.execute_expression(expr, scope)? {
            Value::Literal(v) => Some(v),
            _ => panic!("Only literal are allowed!")
        }
    }

    fn execute_expression(&self, expr: &Expression, scope: &mut Scope) -> Option<Value> {
        match expr {
            Expression::Value(val) => Some(Value::Literal(val.clone())),
            Expression::Variable(val) => match scope.get_variable(val) {
                Some(val) => Some(val.value.clone()),
                None => panic!(format!("Variable '{}' not found. {:?}", val, scope)),
            },
            Expression::ArrayCall(val, index) => {
                match self.execute_expression(val, scope)? {
                    Value::Array(values) => {
                        let i: usize = match self.execute_expression(index, scope)? {
                            Value::Literal(l) => match l {
                                Literal::Number(n) => n,
                                _ => panic!("Expected number")
                            },
                            _ => panic!("Expected literal")
                        };

                        match values.get(i) {
                            Some(v) => Some(v.clone()),
                            None => Some(Value::Literal(Literal::Null))
                        }
                    },
                    _ => None
                }
            },
            Expression::ArrayConstructor(expressions) => {
                let mut values: Vec<Value> = vec![];
                for e in expressions {
                    values.push(self.execute_expression(e, scope)?);
                }

                Some(Value::Array(values))
            }
            Expression::FunctionCall(func_name, params) => {
                self.execute_function_and_params(func_name, params)
            }
            Expression::Operator(operator) => match operator {
                Operator::Dot(left, right) => {
                    match self.execute_expression(left, scope)? {
                        Value::Structure(_, fields) => fields.get(match self.execute_expression_and_expect_literal(right, scope)? {
                            Literal::String(ref s) => s,
                            _ => panic!("Expected a field name")
                        }),
                        _ => panic!("not a structure, where are you trying to use dot operator ?")
                    };

                    None
                },
                Operator::OperatorAssign(left, right) => {
                    let variable = self.execute_expression(left, scope)?;
                    let right_val = self.execute_expression(right, scope)?;

                    println!("assign old value: {:?} to {:?}", variable, right_val);

                    None
                },
                Operator::OperatorPlusAssign(left, right) => None,
                Operator::OperatorMinusAssign(left, right) => None,
                Operator::OperatorDivideAssign(left, right) => None,
                Operator::OperatorMultiplyAssign(left, right) => None,
                Operator::OperatorAnd(left, right) => {
                    if let Some((left, right)) = self.get_booleans(left, right, scope) {
                        Some(Value::Literal(Literal::Boolean(left && right)))
                    } else {
                        None
                    }
                }
                Operator::OperatorOr(left, right) => {
                    if let Some((left, right)) = self.get_booleans(left, right, scope) {
                        Some(Value::Literal(Literal::Boolean(left || right)))
                    } else {
                        None
                    }
                }
                Operator::OperatorEquals(left, right) => {
                    if let Some((left, right)) = self.get_booleans(left, right, scope) {
                        Some(Value::Literal(Literal::Boolean(left == right)))
                    } else {
                        None
                    }
                }
                Operator::OperatorNotEquals(left, right) => {
                    if let Some((left, right)) = self.get_booleans(left, right, scope) {
                        Some(Value::Literal(Literal::Boolean(left != right)))
                    } else {
                        None
                    }
                }
                Operator::OperatorGreaterThan(left, right) => {
                    if let Some((left, right)) = self.get_booleans(left, right, scope) {
                        Some(Value::Literal(Literal::Boolean(left > right)))
                    } else {
                        None
                    }
                }
                Operator::OperatorGreaterOrEqual(left, right) => {
                    if let Some((left, right)) = self.get_booleans(left, right, scope) {
                        Some(Value::Literal(Literal::Boolean(left >= right)))
                    } else {
                        None
                    }
                }
                Operator::OperatorLessThan(left, right) => {
                    if let Some((left, right)) = self.get_booleans(left, right, scope) {
                        Some(Value::Literal(Literal::Boolean(left < right)))
                    } else {
                        None
                    }
                }
                Operator::OperatorLessOrEqual(left, right) => {
                    if let Some((left, right)) = self.get_booleans(left, right, scope) {
                        Some(Value::Literal(Literal::Boolean(left <= right)))
                    } else {
                        None
                    }
                }
                Operator::OperatorModulo(left, right) => {
                    if let Some((left, right)) = self.get_numbers(left, right, scope) {
                        Some(Value::Literal(Literal::Number(left % right)))
                    } else {
                        None
                    }
                }
                Operator::OperatorBitwiseLeft(left, right) => {
                    if let Some((left, right)) = self.get_numbers(left, right, scope) {
                        Some(Value::Literal(Literal::Number(left << right)))
                    } else {
                        None
                    }
                }
                Operator::OperatorBitwiseRight(left, right) => {
                    if let Some((left, right)) = self.get_numbers(left, right, scope) {
                        Some(Value::Literal(Literal::Number(left >> right)))
                    } else {
                        None
                    }
                }
                Operator::OperatorMultiply(left, right) => {
                    if let Some((left, right)) = self.get_numbers(left, right, scope) {
                        Some(Value::Literal(Literal::Number(left * right)))
                    } else {
                        None
                    }
                }
                Operator::OperatorDivide(left, right) => {
                    if let Some((left, right)) = self.get_numbers(left, right, scope) {
                        Some(Value::Literal(Literal::Number(left / right)))
                    } else {
                        None
                    }
                }
                Operator::OperatorPlus(left, right) => {
                    let left = self.execute_expression_and_expect_literal(left, scope)?;
                    let right = self.execute_expression_and_expect_literal(right, scope)?;

                    match left {
                        Literal::Number(left_val) => match right {
                            Literal::Number(val) => Some(Value::Literal(Literal::Number(left_val + val))),
                            Literal::String(val) => {
                                Some(Value::Literal(Literal::String(format!("{}{}", left_val, val))))
                            }
                            _ => panic!("Error! Invalid type for this operator"),
                        },
                        Literal::String(left_val) => match right {
                            Literal::String(val) => Some(Value::Literal(Literal::String(left_val + &val))),
                            Literal::Number(val) => {
                                Some(Value::Literal(Literal::String(format!("{}{}", left_val, val))))
                            }
                            Literal::Boolean(val) => {
                                Some(Value::Literal(Literal::String(format!("{}{}", left_val, val))))
                            }
                            Literal::Null => {
                                Some(Value::Literal(Literal::String(format!("{}{}", left_val, "null"))))
                            }
                        },
                        _ => panic!("Error! Invalid type for + operator"),
                    }
                }
                Operator::OperatorMinus(left, right) => {
                    if let Some((left, right)) = self.get_numbers(left, right, scope) {
                        Some(Value::Literal(Literal::Number(left - right)))
                    } else {
                        None
                    }
                }
            },
            Expression::SubExpression(sub) => self.execute_expression(sub, scope),
        }
    }

    fn unbox<T>(&self, value: Box<T>) -> T {
        *value
    }
}
