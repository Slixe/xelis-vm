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
}

#[derive(Debug, Clone)]
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

    pub fn get_mut_variable(&mut self, name: &str) -> Option<&mut Variable> {
        self.variables.get_mut(name)
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

        self.execute_function(func, parameters)
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
        scope: &Scope,
    ) -> Option<Value> {
        let mut values: Vec<Literal> = vec![];

        let func = self.get_function(name)?;
        for e in parameters {
            values.push(self.execute_expression_and_expect_literal(&e, scope)?);
        }

        self.execute_function(&func, values)
    }

    fn execute_function(
        &self,
        func: &Function,
        values: Vec<Literal>,
    ) -> Option<Value> {
        if values.len() != func.parameters.len() {
            panic!("Parameters / values length mismatch");
        }

        let mut scope = Scope::new();
        let mut i = 0;
        for value in values {
            let param = &func.parameters[i];
            let val = Value::Literal(value.clone());
            if value != Literal::Null && self.get_type_of_value(&val) != param.value_type {
                panic!("Invalid value type for parameter {} expected {:?} found {:?}!", param.name, param.value_type, value);
            }
            scope.register_variable(&param.name, Variable {
                value_type: param.value_type.clone(),
                value: val
            });
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
                self.execute_expression(&exp, scope)
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

                None
            }
            Statement::Variable(var) => {
                let mut value: Option<Value> = None;
                let mut value_type: Option<Type> = None;

                if let Some(result) = &var.value {
                    value = self.execute_expression(result, scope);
                }

                if let Some(result) = &var.value_type {
                    value_type = var.value_type.clone();

                    if value.is_none() {
                        value = Some(match result {
                            Type::Number => Value::Literal(Literal::Number(0)),
                            _ => Value::Literal(Literal::Null)
                        });
                    }
                }

                if value_type.is_none() {
                    if let Some(v) = &value {
                        println!("variable {} with value {:?}", var.name, v);
                        value_type = Some(self.get_type_of_value(&v));
                    }
                }

                scope.register_variable(&var.name, Variable {
                    value: value.unwrap(),
                    value_type: value_type.unwrap()
                });

                None
            }
            Statement::Return(value) => {
                match value {
                    Some(value) => self.execute_expression(value, scope),
                    None => None,
                }
            }
            Statement::Assign(value) => self.assign_value(&value.variable, &value.expression, scope),
            _ => {
                panic!(format!("Statement not implemented: {:?}", statement));
            }
        }
    }

    fn get_type_of_value(&self, value: &Value) -> Type {
        match &value {
            Value::Literal(l) => match l {
                Literal::Null => {
                    panic!("Expected a not-null value or a type");
                }
                Literal::Boolean(_) => Type::Boolean,
                Literal::Number(_) => Type::Number,
                Literal::String(_) => Type::String
            }
            Value::Array(_) => Type::Array(Box::new(Type::Number)), //TODO type
        }
    }
    fn assign_value(&self, var: &VariableAssign, value: &Expression, scope: &mut Scope) -> Option<Value> {
        match var {
            VariableAssign::Variable(v) => {
                let val = self.execute_expression(value, scope)?;
                scope.set_variable_value(v, val);
            }
            VariableAssign::Array(v, index) => {
                let i = match self.execute_expression_and_expect_literal(index, scope)? {
                    Literal::Number(val) => val,
                    _ => panic!("Invalid index")
                };
                let val = self.execute_expression(value, scope)?;
                match &**v {
                    VariableAssign::Variable(v) => {
                        match scope.get_mut_variable(&v).unwrap().value {
                            Value::Array(ref mut values) => {
                                drop(std::mem::replace(&mut values[i], val))
                            },
                            _ => panic!("wtf bro")
                        };
                    }
                    _ => panic!("no variable")
                };
            }
            _ => {}
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
        scope: &Scope,
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

    fn execute_expression_and_expect_literal(&self, expr: &Expression, scope: &Scope) -> Option<Literal> {
        match self.execute_expression(expr, scope)? {
            Value::Literal(v) => Some(v),
            _ => panic!("Only literal are allowed!")
        }
    }

    fn execute_expression(&self, expr: &Expression, scope: &Scope) -> Option<Value> {
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
                self.execute_function_and_params(func_name, params, scope)
            }
            Expression::Operator(operator) => match operator {
                Operator::Dot(left, right) => {
                    match self.execute_expression(left, scope)? {
                        _ => panic!("not a structure, where are you trying to use dot operator ?")
                    };
                },
                /*Operator::OperatorAssign(left, right) => {
                    let value = self.execute_expression(right, scope)?;
                    self.update_value_of(left, value, scope)
                },
                Operator::OperatorPlusAssign(left, right) => {
                    let value = self.execute_expression(right, scope)?;
                    self.update_value_of(left, value, scope)
                },
                Operator::OperatorMinusAssign(left, right) => None,
                Operator::OperatorDivideAssign(left, right) => None,
                Operator::OperatorMultiplyAssign(left, right) => None,*/
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
}