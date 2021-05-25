use super::operator::*;
use super::parser::*;
use super::value_type::*;

use std::collections::HashMap;
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Interpreter {
    constants: Scope,
    functions: HashMap<String, Function>,
    structures: HashMap<String, Structure>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Structure {
    pub fields: HashMap<String, Type>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Value {
    Array(Vec<Value>),
    Literal(Literal),
    Structure(String, Scope),
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Variable {
    pub value: Value,
    pub value_type: Type,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
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

    pub fn get_type(&self, name: &str) -> Option<&Type> {
        Some(&self.variables.get(name)?.value_type)
    }
    pub fn register_variable(&mut self, name: &str, value: Variable) {
        if let Some(v) = self.variables.insert(name.to_string(), value) {
            panic!("Variable '{}' already exist with value: {:?}", name, v);
        }
    }

    pub fn set_variable_value(&mut self, name: &str, value: Value) {
        let var: &mut Variable = match self.variables.get_mut(name) {
            Some(v) => v,
            None => panic!("Trying to set value on a inexistant variable"),
        };

        var.value = value;
    }
}

impl Interpreter {
    pub fn new(program: Program) -> Interpreter {
        let mut interpreter = Interpreter {
            constants: Scope::new(),
            functions: HashMap::new(),
            structures: HashMap::new(),
        };

        for constant in program.constants {
            let value =
                match interpreter.execute_expression(&constant.value, &interpreter.constants) {
                    Some(v) => v,
                    None => panic!("No value found for this constant"),
                };

            let variable = Variable {
                value_type: interpreter.get_type_of_value(&value),
                value,
            };

            interpreter
                .constants
                .register_variable(&constant.name, variable);
        }

        for function in program.functions {
            interpreter
                .functions
                .insert(function.name.clone(), function);
        }

        for structure in program.structures {
            let mut fields = HashMap::new();
            for param in structure.parameters {
                fields.insert(param.name, param.value_type);
            }
            interpreter
                .structures
                .insert(structure.name, Structure { fields });
        }


        println!("{}", serde_json::to_string(&interpreter).unwrap());

        return interpreter;
    }

    pub fn run_function(&self, entry: String, parameters: Vec<Value>) -> Option<Value> {
        let func: &Function = match self.functions.get(&entry) {
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

    fn execute_function_and_params(
        &self,
        name: &String,
        parameters: &Vec<Expression>,
        scope: &Scope,
    ) -> Option<Value> {
        let mut values: Vec<Value> = vec![];

        let func = self.functions.get(name)?;
        for e in parameters {
            values.push(self.execute_expression(&e, scope)?);
        }

        self.execute_function(&func, values)
    }

    fn execute_function(&self, func: &Function, values: Vec<Value>) -> Option<Value> {
        if values.len() != func.parameters.len() {
            panic!("Parameters / values length mismatch");
        }

        let mut scope = self.constants.clone();
        let mut i = 0;
        for value in values {
            let param = &func.parameters[i];
            if self.get_type_of_value(&value) != param.value_type {
                panic!(
                    "Invalid value type for parameter {} expected {:?} found {:?}!",
                    param.name, param.value_type, value
                );
            }
            scope.register_variable(
                &param.name,
                Variable {
                    value_type: param.value_type.clone(),
                    value,
                },
            );
            i += 1;
        }

        self.execute_statements(&func.statements, &mut scope)
    }

    fn execute_statements(&self, statements: &Vec<Statement>, scope: &mut Scope) -> Option<Value> {
        let mut accept_else = false;
        for statement in statements {
            match statement {
                Statement::Expression(exp) => {
                    self.execute_expression(&exp, scope);
                }
                Statement::If(value) => {
                    if let Some(result) =
                        self.execute_expression_and_expect_literal(&value.condition, scope)
                    {
                        let condition: bool = match result {
                            Literal::Boolean(value) => value,
                            _ => panic!("Expected boolean result for this condition"),
                        };

                        if condition {
                            if let Some(res) = self.execute_statements(&value.body, scope) {
                                return Some(res);
                            }
                        } else {
                            accept_else = true;
                        }
                    }
                }
                Statement::Else(value) => {
                    if accept_else {
                        if let Some(res) = self.execute_statements(&value.body, scope) {
                            return Some(res);
                        }
                    }
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
                                _ => Value::Literal(Literal::Null),
                            });
                        }
                    }

                    if value_type.is_none() {
                        if let Some(v) = &value {
                            value_type = Some(self.get_type_of_value(&v));
                        }
                    }

                    scope.register_variable(
                        &var.name,
                        Variable {
                            value: value.unwrap(),
                            value_type: value_type.unwrap(),
                        },
                    );
                }
                Statement::Return(value) => match value {
                    Some(value) => return self.execute_expression(value, scope),
                    None => {}
                },
                Statement::Assign(value) => {
                    let expr_scope = scope.clone(); //TODO search another way
                    self.assign_value(&value.variable, &value.expression, scope, &expr_scope, false);
                }
                _ => {
                    panic!(format!("Statement not implemented: {:?}", statement));
                }
            };

            match statement {
                Statement::If(_) => {}
                _ => {
                    accept_else = false;
                }
            };
        }
        None
    }

    fn get_type_of_value(&self, value: &Value) -> Type {
        match &value {
            Value::Literal(l) => match l {
                Literal::Null => {
                    panic!("Expected a not-null value or a type");
                }
                Literal::Boolean(_) => Type::Boolean,
                Literal::Number(_) => Type::Number,
                Literal::String(_) => Type::String,
            },
            Value::Array(values) => {
                if values.len() > 0 {
                    return Type::Array(Box::new(self.get_type_of_value(&values[0])));
                }

                panic!("Expected at least one value to determine Array type!")
            }
            Value::Structure(name, _) => Type::Structure(name.clone()),
        }
    }

    //TODO rework this function
    fn assign_value<'a>(
        &self,
        var: &VariableAssign,
        value: &Expression,
        scope: &'a mut Scope,
        expr_scope: &Scope,
        return_var: bool,
    ) -> Option<(&'a mut Value, &'a Type)> {
        match var {
            VariableAssign::Variable(v) => {
                if return_var {
                    let var = scope.get_mut_variable(v)?;
                    return Some((&mut var.value, &var.value_type))
                }
                let val =
                    self.execute_expression_and_validate_type(value, expr_scope, scope.get_type(&v)?)?;
                scope.set_variable_value(v, val);
            }
            VariableAssign::Array(v, index) => {
                let i = match self.execute_expression_and_expect_literal(index, expr_scope)? {
                    Literal::Number(val) => val,
                    _ => panic!("Invalid index for array call"),
                };

                let var = self.assign_value(v, value, scope, expr_scope, true)?;
                match var.0 {
                    Value::Array(ref mut values) => {
                        let value_type = match var.1 {
                            Type::Array(ref v) => v,
                            _ => panic!("Invalid type for array call"),
                        };

                        if return_var {
                            return Some((&mut values[i], value_type))
                        }

                        let val =
                            self.execute_expression_and_validate_type(value, expr_scope, value_type)?;

                        std::mem::replace(&mut values[i], val)
                    }
                    _ => panic!("How is it possible ? Expected a Array value"),
                };
            }
            VariableAssign::SubVariable(left, right) => {
                let left_var = match self.assign_value(left, value, scope, expr_scope, true) {
                    Some(v) => v,
                    None => panic!("Variable not found! '{:?}'", left),
                };

                match left_var.0 {
                    Value::Structure(_, ref mut values) => {
                        return self.assign_value(right, value, values, expr_scope, return_var)
                    }
                    _ => panic!("Expected a Structure value, how is it possible ?"),
                }
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

    fn execute_expression_and_expect_literal(
        &self,
        expr: &Expression,
        scope: &Scope,
    ) -> Option<Literal> {
        match self.execute_expression(expr, scope)? {
            Value::Literal(v) => Some(v),
            _ => panic!("Only literal are allowed! Expression: {}", serde_json::to_string_pretty(&expr).unwrap()),
        }
    }

    fn execute_expression_and_validate_type(
        &self,
        expr: &Expression,
        scope: &Scope,
        value_type: &Type,
    ) -> Option<Value> {
        let value = self.execute_expression(expr, scope)?;
        let _type = self.get_type_of_value(&value);
        if _type != *value_type {
            panic!(
                "Invalid value type got {:?} for value {:?} but expected type {:?}",
                _type, value, value_type
            );
        }

        Some(value)
    }

    fn execute_expression(&self, expr: &Expression, scope: &Scope) -> Option<Value> {
        match expr {
            Expression::Value(val) => Some(Value::Literal(val.clone())),
            Expression::Variable(val) => match scope.get_variable(val) {
                Some(val) => Some(val.value.clone()),
                None => panic!(format!("Variable '{}' not found. {:?}", val, scope)),
            },
            Expression::Structure(name, expressions) => {
                let structure = self.structures.get(name)?;
                if structure.fields.len() != expressions.len() {
                    panic!("Mismatch amount of fields for this structure");
                }

                let mut values = Scope::new();
                for (field, value_type) in &structure.fields {
                    let expr = match expressions.get(field) {
                        Some(v) => v,
                        None => panic!("Field {} not found in Structure {}", field, name),
                    };
                    let value =
                        self.execute_expression_and_validate_type(expr, scope, value_type)?;
                    values.register_variable(
                        &field,
                        Variable {
                            value,
                            value_type: value_type.clone(),
                        },
                    );
                }

                Some(Value::Structure(name.clone(), values))
            }
            Expression::ArrayCall(val, index) => match self.execute_expression(val, scope)? {
                Value::Array(values) => {
                    let i: usize = match self.execute_expression(index, scope)? {
                        Value::Literal(l) => match l {
                            Literal::Number(n) => n,
                            _ => panic!("Expected number"),
                        },
                        _ => panic!("Expected literal"),
                    };

                    match values.get(i) {
                        Some(v) => Some(v.clone()),
                        None => Some(Value::Literal(Literal::Null)),
                    }
                }
                _ => None,
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
            Expression::Operator(operator) => {
                match operator {
                    Operator::Dot(left, right) => match self.execute_expression(left, scope)? {
                        Value::Structure(_, values) => self.execute_expression(right, &values),
                        _ => panic!("not a structure, where are you trying to use dot operator ?"),
                    },
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
                        let left = self.execute_expression_and_expect_literal(&left, scope)?;
                        let right = self.execute_expression_and_expect_literal(&right, scope)?;

                        Some(Value::Literal(Literal::Boolean(left == right)))
                    }
                    Operator::OperatorNotEquals(left, right) => {
                        let left = self.execute_expression_and_expect_literal(&left, scope)?;
                        let right = self.execute_expression_and_expect_literal(&right, scope)?;

                        Some(Value::Literal(Literal::Boolean(left != right)))
                    }
                    Operator::OperatorGreaterThan(left, right) => {
                        if let Some((left, right)) = self.get_numbers(left, right, scope) {
                            Some(Value::Literal(Literal::Boolean(left > right)))
                        } else {
                            panic!("Expected two numbers");
                        }
                    }
                    Operator::OperatorGreaterOrEqual(left, right) => {
                        if let Some((left, right)) = self.get_numbers(left, right, scope) {
                            Some(Value::Literal(Literal::Boolean(left >= right)))
                        } else {
                            panic!("Expected two numbers");
                        }
                    }
                    Operator::OperatorLessThan(left, right) => {
                        if let Some((left, right)) = self.get_numbers(left, right, scope) {
                            Some(Value::Literal(Literal::Boolean(left < right)))
                        } else {
                            panic!("Expected two numbers");
                        }
                    }
                    Operator::OperatorLessOrEqual(left, right) => {
                        if let Some((left, right)) = self.get_numbers(left, right, scope) {
                            Some(Value::Literal(Literal::Boolean(left <= right)))
                        } else {
                            panic!("Expected two numbers");
                        }
                    }
                    Operator::OperatorModulo(left, right) => {
                        if let Some((left, right)) = self.get_numbers(left, right, scope) {
                            Some(Value::Literal(Literal::Number(left % right)))
                        } else {
                            panic!("Expected two numbers");
                        }
                    }
                    Operator::OperatorBitwiseLeft(left, right) => {
                        if let Some((left, right)) = self.get_numbers(left, right, scope) {
                            Some(Value::Literal(Literal::Number(left << right)))
                        } else {
                            panic!("Expected two numbers");
                        }
                    }
                    Operator::OperatorBitwiseRight(left, right) => {
                        if let Some((left, right)) = self.get_numbers(left, right, scope) {
                            Some(Value::Literal(Literal::Number(left >> right)))
                        } else {
                            panic!("Expected two numbers");
                        }
                    }
                    Operator::OperatorMultiply(left, right) => {
                        if let Some((left, right)) = self.get_numbers(left, right, scope) {
                            Some(Value::Literal(Literal::Number(left * right)))
                        } else {
                            panic!("Expected two numbers");
                        }
                    }
                    Operator::OperatorDivide(left, right) => {
                        if let Some((left, right)) = self.get_numbers(left, right, scope) {
                            Some(Value::Literal(Literal::Number(left / right)))
                        } else {
                            panic!("Expected two numbers");
                        }
                    }
                    Operator::OperatorPlus(left, right) => {
                        let left = self.execute_expression_and_expect_literal(left, scope)?;
                        let right = self.execute_expression_and_expect_literal(right, scope)?;

                        match left {
                            Literal::Number(left_val) => match right {
                                Literal::Number(val) => {
                                    Some(Value::Literal(Literal::Number(left_val + val)))
                                }
                                Literal::String(val) => Some(Value::Literal(Literal::String(
                                    format!("{}{}", left_val, val),
                                ))),
                                _ => panic!("Error! Invalid type for this operator"),
                            },
                            Literal::String(left_val) => match right {
                                Literal::String(val) => {
                                    Some(Value::Literal(Literal::String(left_val + &val)))
                                }
                                Literal::Number(val) => Some(Value::Literal(Literal::String(
                                    format!("{}{}", left_val, val),
                                ))),
                                Literal::Boolean(val) => Some(Value::Literal(Literal::String(
                                    format!("{}{}", left_val, val),
                                ))),
                                Literal::Null => Some(Value::Literal(Literal::String(format!(
                                    "{}{}",
                                    left_val, "null"
                                )))),
                            },
                            _ => panic!("Error! Invalid type for + operator"),
                        }
                    }
                    Operator::OperatorMinus(left, right) => {
                        if let Some((left, right)) = self.get_numbers(left, right, scope) {
                            Some(Value::Literal(Literal::Number(left - right)))
                        } else {
                            panic!("Expected two numbers");
                        }
                    }
                }
            }
            Expression::SubExpression(sub) => self.execute_expression(sub, scope),
        }
    }
}
