use crate::value_type::*;
use crate::parser::*;
use crate::operator::*;
use crate::environment::Environment;

use std::collections::HashMap;
use num_bigint::{BigInt, ToBigInt};
use num_traits::identities::Zero;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug)]
pub struct Func {
    pub parameters: Vec<Type>,
    pub execution: fn(Vec<Value>) -> Option<Value>,
}

#[derive(Clone)]
pub struct FuncType {
    pub parameters: Vec<Type>,
    pub execution: fn(&mut Value, Vec<Value>) -> Option<Value>,
}

#[derive(Clone)]
pub enum FunctionType {
    Custom(Function),
    Builtin(Func),
    Type(FuncType),
}

impl FunctionType {
    pub fn is_entry(&self) -> bool {
        match self {
            FunctionType::Custom(f) => f.entry,
            _ => false,
        }
    }

    pub fn get_parameters_type(&self) -> Vec<&Type> {
        match self {
            FunctionType::Custom(f) => f.parameters.iter().map(|p| &p.value_type).collect(),
            FunctionType::Builtin(f) => f.parameters.iter().map(|p| p).collect(),
            FunctionType::Type(f) => f.parameters.iter().map(|p| p).collect(),
        }
    }
}

pub struct Interpreter {
    constants: Scope,
    functions: HashMap<String, FunctionType>,
    structures: HashMap<String, Structure>,
    type_fuctions: HashMap<Type, HashMap<String, FunctionType>>,
    libraries: HashMap<String, Interpreter>
}

#[derive(Serialize, Deserialize, Debug, Clone, Eq)]
pub enum Value {
    Array(Vec<Value>),
    Literal(Literal),
    Structure(Type, Scope)
}

pub enum InterpreterError {
    VariableAlreadyRegistered(String),
    VariableNotFound(String),
    LibraryNotFound(String),
    NoValueReturned,
    NoValueForConstant,
    FunctionNotEntryPoint(String),
    FunctionIsEntryPoint(String),
    FunctionNotFound(String),
    InvalidValueType,
    FunctionAlreadyRegistered(String),
    StructureAlreadyRegistered(String),
    MismatchFunctionReturn,
    InvalidFunctionType,
    MismatchFunctionParameters,
    ExpectedBoolean,
    ExpectedArray,
    ExpectedLiteral,
    ExpectedNumber,
}

pub type InterpreterResult<T> = Result<T, InterpreterError>;

impl PartialEq<Value> for Value {
    fn eq(&self, other: &Value) -> bool {
        match self {
            Value::Array(values) => match other {
                Value::Array(other_values) => {
                    if values.len() != other_values.len() {
                        return false
                    }

                    for i in 0..values.len() - 1 {
                        if values[i] != other_values[i] {
                            return false
                        }
                    }

                    true
                },
                _ => false
            },
            Value::Literal(l) => match other {
                Value::Literal(o) => l == o,
                _ => false
            },
            Value::Structure(s, s_scope) => match other {
                Value::Structure(o, o_scope) => {
                    if s != o {
                        return false
                    }

                    s_scope == o_scope
                },
                _ => false
            }
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Eq)]
pub struct Variable {
    pub value: Value,
    pub value_type: Type,
}

impl PartialEq<Variable> for Variable {
    fn eq(&self, other: &Variable) -> bool {
        self.value_type != other.value_type || self.value != other.value
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Eq)]
pub struct Scope {
    variables: HashMap<String, Variable>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            variables: HashMap::new(),
        }
    }

    pub fn get_mut_variable(&mut self, name: &str) -> InterpreterResult<&mut Variable> {
        match self.variables.get_mut(name) {
            Some(v) => Ok(v),
            None => return Err(InterpreterError::VariableNotFound(name.into()))
        }
    }

    pub fn get_variable(&self, name: &str) -> Option<&Variable> {
        self.variables.get(name)
    }

    pub fn register_variable(&mut self, name: &str, value: Variable) -> InterpreterResult<()> {
        if self.variables.insert(name.to_string(), value).is_some() {
            //panic!("Variable '{}' already exist with value: {:?}", name, v);
            return Err(InterpreterError::VariableAlreadyRegistered(name.into()))
        }

        Ok(())
    }

    pub fn update_scope(&mut self, mut scope: Scope) -> InterpreterResult<()> {
        for (key, val) in self.variables.iter_mut() {
            *val = match scope.variables.remove(key) {
                Some(v) => v,
                None => return Err(InterpreterError::VariableNotFound(key.clone()))
            };
        }

        Ok(())
    }
}

impl PartialEq<Scope> for Scope {
    fn eq(&self, other: &Scope) -> bool {
        if self.variables.len() != other.variables.len() {
            return false
        }

        for (key, value) in self.variables.iter() {
            match other.variables.get(key) {
                Some(v) => {
                    if value != v {
                        return false
                    }
                },
                None => return false
            }
        }

        true
    }
}

impl Interpreter {
    pub fn new(program: Program, environment: Environment, load_library: fn(String) -> Option<Library>) -> InterpreterResult<Interpreter> {
        let mut libraries: HashMap<String, Interpreter> = HashMap::new();
        for (name, path) in program.libraries {
            match load_library(path) {
                Some(lib) => {
                    libraries.insert(name, Interpreter::new(lib.get_program(), environment.clone(), load_library)?);
                },
                None => return Err(InterpreterError::LibraryNotFound(name))//panic!(format!("Error, library imported as '{}' was not found!", name))
            };
        }

        //Environment have priority over program declaration
        let mut interpreter = Interpreter {
            constants: environment.scope,
            functions: environment.functions,
            structures: environment.structures,
            type_fuctions: environment.type_functions,
            libraries: libraries
        };

        for (name, constant) in program.constants {
            let value =
                match interpreter.execute_expression(&constant.value, &interpreter.constants)? {
                    Some(v) => v,
                    None => return Err(InterpreterError::NoValueForConstant) //panic!("No value found for this constant"),
                };

            let variable = Variable {
                value_type: Type::get_type_of_value(&value),
                value,
            };

            interpreter.constants.register_variable(&name, variable)?;
        }

        for (name, function) in program.functions {
            if interpreter.functions.contains_key(&name) {
                return Err(InterpreterError::FunctionAlreadyRegistered(name.clone()))
                //panic!("Function '{}' already registered!", name);
            }
            interpreter
                .functions
                .insert(name, FunctionType::Custom(function));
        }

        for (name, structure) in program.structures {
            if interpreter.structures.contains_key(&name) {
                return Err(InterpreterError::StructureAlreadyRegistered(name.clone()))
                //panic!("Structure '{}' already registered!", name);
            }

            interpreter.structures.insert(name, structure);
        }

        Ok(interpreter)
    }

    pub fn run_function(&self, entry: String, parameters: Vec<Value>) -> InterpreterResult<Option<Value>> {
        let func: &FunctionType = match self.functions.get(&entry) {
            Some(func) => func,
            None => return Err(InterpreterError::FunctionNotFound(entry.clone()))
        };

        if !func.is_entry() {
            return Err(InterpreterError::FunctionNotEntryPoint(entry.clone()))
            //panic!(format!("Function '{}' is not an entrypoint!", entry));
        }

        self.execute_function(func, parameters)
    }

    fn execute_function_params(&self, parameters: &Vec<Expression>, scope: &Scope) -> InterpreterResult<Vec<Value>> {
        let mut values: Vec<Value> = vec![];
        for e in parameters {
            let value = match self.execute_expression(&e, scope)? {
                Some(v) => v,
                None => return Err(InterpreterError::NoValueReturned) //panic!("Expression returned no value!")
            };
            values.push(value);
        }

        Ok(values)
    }

    fn execute_function_with_params(
        &self,
        name: &String,
        parameters: Vec<Value>,
    ) -> InterpreterResult<Option<Value>> {
        let func = match self.functions.get(name) {
            Some(v) => v,
            None => return Err(InterpreterError::FunctionNotFound(name.clone()))//panic!("Function {} not found!", name)
        };
        if func.is_entry() { // Code cannot call entrypoint function, thats only for user!
            return Err(InterpreterError::FunctionIsEntryPoint(name.clone()))
            //panic!(format!("Function '{}' is an entrypoint!", name.clone()));
        }

        self.execute_function(&func, parameters)
    }

    fn execute_function(&self, func: &FunctionType, values: Vec<Value>) -> InterpreterResult<Option<Value>> {
        let types = func.get_parameters_type();
        if values.len() != types.len() {
            return Err(InterpreterError::MismatchFunctionParameters)
            //panic!("Parameters / values length mismatch");
        }

        let mut i = 0;
        while i < types.len() {
            let value_type = types[i];
            let value = &values[i];
            if Type::get_type_of_value(value) != *value_type && *value_type != Type::Any {
                return Err(InterpreterError::InvalidValueType)
                //panic!("Invalid value type for parameter {} expected {:?} found {:?}!", i, value_type, value);
            }
            i = i + 1;
        }

        match func {
            FunctionType::Builtin(f) => Ok((f.execution)(values)),
            FunctionType::Custom(f) => {
                let mut scope = self.constants.clone();
                let mut i = 0;
                for value in values {
                    let param = &f.parameters[i];
                    scope.register_variable(
                        &param.name,
                        Variable {
                            value_type: param.value_type.clone(),
                            value,
                        },
                    )?;
                    i += 1;
                }

                let result = self.execute_statements(&f.statements, &f.ret_value, &mut scope)?;
                if result.is_some() != f.ret_value.is_some() {
                    return Err(InterpreterError::MismatchFunctionReturn)
                    //panic!("Error on function, mismatch on returned value and function return type");
                }
                Ok(result)
            }
            _ => Err(InterpreterError::InvalidFunctionType),
        }
    }

    fn get_function_type(&self, value_type: &Type) -> Option<&HashMap<String, FunctionType>> {
        match self.type_fuctions.get(value_type) {
            Some(v) => Some(v),
            None => match value_type {
                Type::Array(t) => match t.as_ref() {
                    Type::Any => None,
                    _ => self.get_function_type(&Type::Array(Box::new(Type::Any))),
                },
                Type::Any => None,
                _ => self.get_function_type(&Type::Any),
            },
        }
    }

    fn execute_function_type(
        &self,
        val: &mut Value,
        function_name: &String,
        parameters: &Vec<Expression>,
        scope: &Scope,
    ) -> InterpreterResult<Option<Value>> {
        let value_type = Type::get_type_of_value(&val);
        match self.get_function_type(&value_type) {
            Some(map) => {
                match map.get(function_name) {
                    Some(v) => match v {
                        FunctionType::Type(f) => {
                            let mut values: Vec<Value> = vec![];
                            if f.parameters.len() != parameters.len() {
                                return Err(InterpreterError::MismatchFunctionParameters)
                                //panic!("Parameters / values length mismatch");
                            }

                            let mut i = 0;
                            while i < f.parameters.len() {
                                let value = self.execute_expression_expect_value(&parameters[i], scope)?;
                                let value_type = Type::get_type_of_value(&value);
                                let param_type = &f.parameters[i];
                                if *param_type != Type::Any && value_type != *param_type {
                                    return Err(InterpreterError::InvalidValueType)
                                    //panic!("Invalid value type for parameter {}, expected {:?} found {:?}", i, f.parameters[i], value_type);
                                }
                                values.push(value);
                                i += 1;
                            }

                            Ok((f.execution)(val, values))
                        }
                        _ => Err(InterpreterError::InvalidFunctionType) //panic!("Invalid function type"),
                    },
                    None => Err(InterpreterError::FunctionNotFound(function_name.clone())) //panic!("No function named '{}' found for type {:?} !", function_name, value_type),
                }
            }
            None => Err(InterpreterError::FunctionNotFound(function_name.clone())) //panic!("No function found for type {:?} !", value_type),
        }
    }

    fn execute_statements(
        &self,
        statements: &Vec<Statement>,
        return_type: &Option<Type>,
        scope: &mut Scope,
    ) -> InterpreterResult<Option<Value>> {
        let mut accept_else = false;
        for statement in statements {
            match statement {
                Statement::Expression(exp) => {
                    self.execute_expression(&exp, scope)?;
                }
                Statement::If(value) => {
                    let condition: bool = match self.execute_expression_and_expect_literal(&value.condition, scope)? {
                        Literal::Boolean(value) => value,
                        _ => return Err(InterpreterError::ExpectedBoolean) //panic!("Expected boolean result for this if condition"),
                    };

                    if condition {
                        let mut cloned_scope = scope.clone();
                        let res = self.execute_statements(&value.body, return_type, &mut cloned_scope)?;
                        if res.is_some() {
                            return Ok(res)
                        }
                        scope.update_scope(cloned_scope)?;
                    } else {
                        accept_else = true;
                    }
                }
                Statement::ElseIf(value) => {
                    if accept_else {
                        let result = self.execute_expression_and_expect_literal(&value.condition, scope)?;
                        let condition: bool = match result {
                            Literal::Boolean(value) => value,
                            _ => return Err(InterpreterError::ExpectedBoolean) //panic!("Expected boolean result for this else if condition"),
                        };

                        if condition {
                            accept_else = false;
                            let mut cloned_scope = scope.clone();
                            let res = self.execute_statements(&value.body, return_type, &mut cloned_scope)?;
                            if res.is_some() {
                                return Ok(res)
                            }
                            scope.update_scope(cloned_scope)?;
                        } else {
                            accept_else = true;
                        }
                    }
                },
                Statement::Else(value) => {
                    if accept_else {
                        accept_else = false;
                        let mut cloned_scope = scope.clone();
                        let res = self.execute_statements(&value.body, return_type, &mut cloned_scope)?;
                        if res.is_some() {
                            return Ok(res)
                        }
                        scope.update_scope(cloned_scope)?;
                    }
                }
                Statement::Variable(var) => {
                    let mut value: Option<Value> = None;
                    let mut value_type: Option<Type> = None;

                    if let Some(result) = &var.value {
                        value = self.execute_expression(result, scope)?;
                    }

                    if let Some(result) = &var.value_type {
                        value_type = var.value_type.clone();

                        if value.is_none() {
                            value = Some(match result {
                                Type::Number => Value::Literal(Literal::Number(0)),
                                Type::BigInt => Value::Literal(Literal::BigInt(0.to_bigint().unwrap())),
                                _ => Value::Literal(Literal::Null),
                            });
                        }
                    }

                    if let Some(v) = &value {
                        match &value_type {
                            Some(t) => {
                                if *t != Type::get_type_of_value(v) {
                                    return Err(InterpreterError::InvalidValueType)
                                    //panic!("Invalid value for type {}", t)
                                }
                            }
                            None => {
                                value_type = Some(Type::get_type_of_value(&v));
                            }
                        }
                    }
                    scope.register_variable(
                        &var.name,
                        Variable {
                            value: value.unwrap(),
                            value_type: value_type.unwrap(),
                        },
                    )?;
                }
                Statement::Return(value) => match return_type {
                    Some(ret_type) => match value {
                        Some(v) => return Ok(Some(self.execute_expression_and_validate_type(v, scope, ret_type)?)),
                        None => return Err(InterpreterError::MismatchFunctionReturn)
                    },
                    None => {
                        if value.is_some() {
                            return Err(InterpreterError::MismatchFunctionReturn)
                            //panic!("Returned value, but function doesn't accept it")
                        }

                        return Ok(None)
                    }
                },
                Statement::Assign(value) => {
                    let expr_scope = scope.clone(); //TODO search another way
                    self.assign_value(&value.variable, &value.expression, scope, &expr_scope)?;
                }
                Statement::While(value) => {
                    //TODO implement break/continue statements
                    let mut cloned_scope = scope.clone();
                    while match self.execute_expression_and_expect_literal(
                        &value.condition,
                        &mut cloned_scope,
                    )? {
                        Literal::Boolean(v) => v,
                        _ => return Err(InterpreterError::ExpectedBoolean)//panic!("Expected a valid condition"),
                    } {
                        let res = self.execute_statements(&value.body, return_type, &mut cloned_scope)?;
                        if res.is_some()
                        {
                            return Ok(res)
                        }
                    }
                    scope.update_scope(cloned_scope)?;
                }
                Statement::Scope(value) => {
                    let mut cloned_scope = scope.clone();
                    let res = self.execute_statements(&value.body, return_type, &mut cloned_scope)?;
                    if res.is_some() {
                        return Ok(res)
                    }
                    scope.update_scope(cloned_scope)?;
                }
                Statement::For(value) => {
                    //TODO implement break/continue statements
                    let val = self.execute_expression_expect_value(&value.values, scope)?;
                    let value_type = Type::get_type_of_value(&val);
                    let mut cloned_scope = scope.clone();
                    cloned_scope.register_variable(
                        &value.variable,
                        Variable {
                            value_type,
                            value: Value::Literal(Literal::Null),
                        },
                    )?;
                    match val {
                        Value::Array(values) => {
                            for v in values {
                                cloned_scope.get_mut_variable(&value.variable)?.value = v;
                                let res = self.execute_statements(
                                    &value.body,
                                    return_type,
                                    &mut cloned_scope,
                                )?;
                                if res.is_some() {
                                    return Ok(res)
                                }
                            }

                            scope.update_scope(cloned_scope)?;
                        }
                        _ => return Err(InterpreterError::ExpectedArray) //panic!("Expected a array value for iteration"),
                    }
                }
                Statement::Continue | Statement::Break => {
                    panic!("This statement is not allowed here!");
                }
            };

            match statement {
                Statement::If(_) | Statement::ElseIf(_) => {}
                _ => {
                    accept_else = false;
                }
            };
        }

        Ok(None)
    }

    fn execute_expression_and_expect_number(
        &self,
        expression: &Expression,
        scope: &Scope,
    ) -> InterpreterResult<u64> {
        match self.execute_expression_and_expect_literal(expression, scope)? {
            Literal::Number(n) => Ok(n),
            _ => Err(InterpreterError::ExpectedNumber),
        }
    }

    fn get_variable<'a>(
        &self,
        path: &VariablePath,
        scope: &'a mut Scope,
    ) -> InterpreterResult<(&'a mut Value, &'a Type)> {
        match path {
            VariablePath::Variable(v) => {
                let var = scope.get_mut_variable(v)?;
                Ok((&mut var.value, &var.value_type))
            }
            VariablePath::SubVariable(left, right) => {
                let left_var = self.get_variable(left, scope)?;
                match left_var.0 {
                    Value::Structure(_, ref mut values) => self.get_variable(right, values),
                    _ => panic!("Expected a Structure value, how is it possible ?"),
                }
            }
            VariablePath::Array(v, index) => {
                let i = self.execute_expression_and_expect_number(index, scope)?;
                let tuple = self.get_variable(v, scope)?;

                match tuple.0 {
                    Value::Array(ref mut values) => {
                        let value_type = match tuple.1 {
                            Type::Array(ref v) => v,
                            _ => panic!("Invalid type for array call"),
                        };
                        return Ok((&mut values[i as usize], value_type)) //TODO verify if type is tuple.1 is same as values[i]
                    }
                    val => panic!("Expected a array value but got {:?}", val),
                }
            }
        }
    }

    fn assign_value(
        &self,
        path: &VariablePath,
        expression_value: &Expression,
        scope: &mut Scope,
        expression_scope: &Scope,
    ) -> InterpreterResult<()> {
        let value = match self.execute_expression(&expression_value, expression_scope)? {
            Some(v) => v,
            None => panic!("No value returned from this expression!"),
        };
        let (var_value, var_type) = self.get_variable(path, scope)?;
        let value_type = Type::get_type_of_value(&value);
        if *var_type != value_type {
            return Err(InterpreterError::InvalidValueType)
            //panic!("Invalid type of value for this variable. Got {:?} but expected {:?}", value_type, var_type);
        }

        *var_value = value;

        Ok(())
    }

    /*fn get_numbers(
        &self,
        left: &Expression,
        right: &Expression,
        scope: &Scope,
    ) -> Option<(u64, u64)> {
        if let (Literal::Number(left_val), Literal::Number(right_val)) = (
            self.execute_expression_and_expect_literal(left, scope)?,
            self.execute_expression_and_expect_literal(right, scope)?,
        ) {
            Some((left_val, right_val))
        } else {
            None
        }
    }*/

    /*fn get_bigints(
        &self,
        left: &Expression,
        right: &Expression,
        scope: &Scope,
    ) -> Option<(BigInt, BigInt)> {
        if let (Literal::BigInt(left_val), Literal::BigInt(right_val)) = (
            self.execute_expression_and_expect_literal(left, scope)?,
            self.execute_expression_and_expect_literal(right, scope)?,
        ) {
            Some((left_val, right_val))
        } else {
            None
        }
    }*/

    fn get_booleans(
        &self,
        left: &Expression,
        right: &Expression,
        scope: &Scope,
    ) -> InterpreterResult<(bool, bool)> {
        if let (Literal::Boolean(left_val), Literal::Boolean(right_val)) = (
            self.execute_expression_and_expect_literal(left, scope)?,
            self.execute_expression_and_expect_literal(right, scope)?,
        ) {
            Ok((left_val, right_val))
        } else {
            Err(InterpreterError::ExpectedLiteral)
        }
    }

    fn execute_expression_and_expect_literal(
        &self,
        expr: &Expression,
        scope: &Scope,
    ) -> InterpreterResult<Literal> {
        match self.execute_expression(expr, scope)? {
            Some(v) => match v {
                Value::Literal(v) => Ok(v),
                _ =>  return Err(InterpreterError::ExpectedLiteral) //panic!("Only literal are allowed! Expression: {}", serde_json::to_string_pretty(&expr).unwrap()),    
            },
            None => return Err(InterpreterError::ExpectedLiteral)
        }
    }

    fn execute_expression_and_validate_type(
        &self,
        expr: &Expression,
        scope: &Scope,
        value_type: &Type,
    ) -> InterpreterResult<Value> {
        let value = match self.execute_expression(expr, scope)? {
            Some(v) => v,
            None => return Err(InterpreterError::NoValueReturned) //panic!("Expected a value, but nothing is returned!"),
        };
        let _type = Type::get_type_of_value(&value);
        if _type != *value_type {
            return Err(InterpreterError::InvalidValueType)
            //panic!("Invalid value type got {:?} for value {:?} but expected type {:?}", _type, value, value_type);
        }

        Ok(value)
    }

    fn execute_expression_expect_value(&self, expr: &Expression, scope: &Scope) -> InterpreterResult<Value> {
        match self.execute_expression(expr, scope)? {
            Some(v) => Ok(v),
            None => return Err(InterpreterError::NoValueReturned)
        }
    }

    fn execute_expression(&self, expr: &Expression, scope: &Scope) -> InterpreterResult<Option<Value>> {
        match expr {
            Expression::Value(val) => Ok(Some(Value::Literal(val.clone()))),
            Expression::Variable(val) => match scope.get_variable(val) {
                Some(val) => Ok(Some(val.value.clone())),
                None => panic!(format!("Variable '{}' not found. {:?}", val, scope)),
            },
            Expression::LibraryCall(val) => panic!(format!("Library call on {} not authorized.", val)),
            Expression::Structure(struct_type, expressions) => {
                let structure: &Structure = match struct_type {
                    Type::LibraryType(lib_name, t) => match self.libraries.get(lib_name) {
                        Some(lib) => match t.as_ref() {
                            Type::Structure(name) => match lib.structures.get(name) {
                                Some(s) => s,
                                None => panic!(format!("Structure type with name {} was not found in library {}!", name, lib_name))
                            },
                            _ => panic!("Invalid Type on library!")
                        },
                        None => panic!(format!("Library {} not found!", lib_name))
                    },
                    Type::Structure(name) => match self.structures.get(name) {
                        Some(v) => v,
                        None => panic!(format!("Structure with name {} not found", name))
                    },
                    _ => panic!("Only structure accepted!")
                };
                //let structure = self.structures.get(name)?;
                if structure.fields.len() != expressions.len() {
                    panic!("Mismatch amount of fields for this structure");
                }

                let mut values = Scope::new();
                for (field, value_type) in &structure.fields {
                    let expr = match expressions.get(field) {
                        Some(v) => v,
                        None => panic!(format!("Field {} not found in Structure", field)),
                    };
                    let value =
                        self.execute_expression_and_validate_type(expr, scope, value_type)?;
                    values.register_variable(
                        &field,
                        Variable {
                            value,
                            value_type: value_type.clone(),
                        },
                    )?;
                }

                Ok(Some(Value::Structure(struct_type.clone(), values)))
            }
            Expression::ArrayCall(val, index) => match self.execute_expression_expect_value(val, scope)? {
                Value::Array(values) => {
                    let i = self.execute_expression_and_expect_number(index, scope)?;
                    match values.get(i as usize) {
                        Some(v) => Ok(Some(v.clone())),
                        None => Ok(Some(Value::Literal(Literal::Null))),
                    }
                }
                _ => Ok(None),
            },
            Expression::ArrayConstructor(expressions) => {
                let mut values: Vec<Value> = vec![];
                for e in expressions {
                    values.push(self.execute_expression_expect_value(e, scope)?);
                }

                Ok(Some(Value::Array(values)))
            }
            Expression::FunctionCall(func_name, params) => {
                self.execute_function_with_params(func_name, self.execute_function_params(params, scope)?)
            }
            Expression::Operator(operator) => {
                match operator {
                    Operator::Dot(left, right) => match left.as_ref() {
                        Expression::LibraryCall(lib) => {
                            match self.libraries.get(lib) {
                                Some(lib_interpreter) => match right.as_ref() {
                                    Expression::FunctionCall(func_name, params) => {
                                        let values = self.execute_function_params(params, scope)?;
                                        let mut wrapped_values = vec![];
                                        for v in values {
                                            wrapped_values.push(self.unwrap_lib_value(v, &lib));
                                        }

                                        let opt_ret_value = lib_interpreter.execute_function_with_params(func_name, wrapped_values)?;
                                        match opt_ret_value {
                                            Some(ret_value) => Ok(Some(self.wrap_lib_value(ret_value, lib.clone()))),
                                            None => Ok(None)
                                        }
                                    }, //TODO structure in parser, if lib.Vector -> LibraryType(...) not LibCall(LibraryType!!)
                                    _ => panic!("not possible")
                                },
                                None => panic!(format!("Library {} called but wasn't found in libraries loaded!", lib))
                            }
                        },
                        _ => match self.execute_expression_expect_value(left, scope)? {
                            Value::Structure(_, values) => match right.as_ref() {
                                Expression::FunctionCall(name, params) => {
                                    let path = match VariablePath::get_path_for_variable(*left.clone()) {
                                        Ok(v) => v,
                                        Err(err) => panic!("No dynamic variable path found for this expression, error: {:?}", err)
                                    };
                                    unsafe {
                                        let tuple = self.get_variable(&path, &mut *(scope as *const _ as *mut _))?;
                                        self.execute_function_type(tuple.0, name, params, &values)
                                    }
                                }
                                right => self.execute_expression(right, &values),
                            },
                            val => match right.as_ref() {
                                Expression::FunctionCall(name, params) => {
                                    let path = match VariablePath::get_path_for_variable(*left.clone()) {
                                        Ok(v) => v,
                                        Err(err) => panic!("No dynamic variable path found for this expression, error: {:?}", err)
                                    };
                                    unsafe {
                                        let tuple = self.get_variable(&path, &mut *(scope as *const _ as *mut _))?;
                                        self.execute_function_type(tuple.0, name, params, scope)
                                    }
                                }
                                v => panic!(
                                    "Got '{:?}' called on '{:?}', what is it supposed to do ?",
                                    v, val
                                ),
                            },
                        },
                    }
                    Operator::OperatorAnd(left, right) => {
                        let (left, right) = self.get_booleans(left, right, scope)?;
                        Ok(Some(Value::Literal(Literal::Boolean(left && right))))
                    }
                    Operator::OperatorOr(left, right) => {
                        let (left, right) = self.get_booleans(left, right, scope)?;
                        Ok(Some(Value::Literal(Literal::Boolean(left || right))))
                    }
                    Operator::OperatorEquals(left, right) => {
                        let left = self.execute_expression_and_expect_literal(&left, scope)?;
                        let right = self.execute_expression_and_expect_literal(&right, scope)?;

                        Ok(Some(Value::Literal(Literal::Boolean(left == right))))
                    }
                    Operator::OperatorNotEquals(left, right) => {
                        let left = self.execute_expression_and_expect_literal(&left, scope)?;
                        let right = self.execute_expression_and_expect_literal(&right, scope)?;

                        Ok(Some(Value::Literal(Literal::Boolean(left != right))))
                    }
                    Operator::OperatorGreaterThan(left, right) => {
                        match self.execute_expression_and_expect_literal(&left, scope)? {
                            Literal::BigInt(l) => match self.execute_expression_and_expect_literal(right, scope)? {
                                Literal::BigInt(r) => Ok(Some(Value::Literal(Literal::Boolean(l > r)))),
                                _ => panic!("expected two bigInts!")
                            }
                            Literal::Number(l) => match self.execute_expression_and_expect_literal(right, scope)? {
                                Literal::Number(r) => Ok(Some(Value::Literal(Literal::Boolean(l > r)))),
                                _ => panic!("expected two numbers!")
                            },
                            _ => panic!("Expected number or bigint")
                        }
                    }
                    Operator::OperatorGreaterOrEqual(left, right) => {
                        match self.execute_expression_and_expect_literal(&left, scope)? {
                            Literal::BigInt(l) => match self.execute_expression_and_expect_literal(right, scope)? {
                                Literal::BigInt(r) => Ok(Some(Value::Literal(Literal::Boolean(l >= r)))),
                                _ => panic!("expected two bigInts!")
                            }
                            Literal::Number(l) => match self.execute_expression_and_expect_literal(right, scope)? {
                                Literal::Number(r) => Ok(Some(Value::Literal(Literal::Boolean(l >= r)))),
                                _ => panic!("expected two numbers!")
                            },
                            _ => panic!("Expected number or bigint")
                        }
                    }
                    Operator::OperatorLessThan(left, right) => {
                        match self.execute_expression_and_expect_literal(&left, scope)? {
                            Literal::BigInt(l) => match self.execute_expression_and_expect_literal(right, scope)? {
                                Literal::BigInt(r) => Ok(Some(Value::Literal(Literal::Boolean(l < r)))),
                                _ => panic!("expected two bigInts!")
                            }
                            Literal::Number(l) => match self.execute_expression_and_expect_literal(right, scope)? {
                                Literal::Number(r) => Ok(Some(Value::Literal(Literal::Boolean(l < r)))),
                                _ => panic!("expected two numbers!")
                            },
                            _ => panic!("Expected number or bigint")
                        }
                    }
                    Operator::OperatorLessOrEqual(left, right) => {
                        match self.execute_expression_and_expect_literal(&left, scope)? {
                            Literal::BigInt(l) => match self.execute_expression_and_expect_literal(right, scope)? {
                                Literal::BigInt(r) => Ok(Some(Value::Literal(Literal::Boolean(l <= r)))),
                                _ => panic!("expected two bigInts!")
                            }
                            Literal::Number(l) => match self.execute_expression_and_expect_literal(right, scope)? {
                                Literal::Number(r) => Ok(Some(Value::Literal(Literal::Boolean(l <= r)))),
                                _ => panic!("expected two numbers!")
                            },
                            _ => panic!("Expected number or bigint")
                        }
                    }
                    Operator::OperatorModulo(left, right) => {
                        match self.execute_expression_and_expect_literal(&left, scope)? {
                            Literal::BigInt(l) => match self.execute_expression_and_expect_literal(right, scope)? {
                                Literal::BigInt(r) => Ok(Some(Value::Literal(Literal::BigInt(l % r)))),
                                _ => panic!("expected two bigInts!")
                            }
                            Literal::Number(l) => match self.execute_expression_and_expect_literal(right, scope)? {
                                Literal::Number(r) => Ok(Some(Value::Literal(Literal::Number(l % r)))),
                                _ => panic!("expected two numbers!")
                            },
                            _ => panic!("Expected number or bigint")
                        }
                    }
                    Operator::OperatorBitwiseLeft(left, right) => {
                        match self.execute_expression_and_expect_literal(&left, scope)? {
                            Literal::BigInt(l) => match self.execute_expression_and_expect_literal(right, scope)? {
                                Literal::BigInt(r) => Ok(Some(Value::Literal(Literal::BigInt(/*l << r*/ BigInt::zero())))), //TODO
                                _ => panic!("expected two bigInts!")
                            }
                            Literal::Number(l) => match self.execute_expression_and_expect_literal(right, scope)? {
                                Literal::Number(r) => Ok(Some(Value::Literal(Literal::Number(l << r)))),
                                _ => panic!("expected two numbers!")
                            },
                            _ => panic!("Expected number or bigint")
                        }
                    }
                    Operator::OperatorBitwiseRight(left, right) => {
                        match self.execute_expression_and_expect_literal(&left, scope)? {
                            Literal::BigInt(l) => match self.execute_expression_and_expect_literal(right, scope)? {
                                Literal::BigInt(r) => Ok(Some(Value::Literal(Literal::BigInt(/*l >> r*/ BigInt::zero())))), //TODO
                                _ => panic!("expected two bigInts!")
                            }
                            Literal::Number(l) => match self.execute_expression_and_expect_literal(right, scope)? {
                                Literal::Number(r) => Ok(Some(Value::Literal(Literal::Number(l >> r)))),
                                _ => panic!("expected two numbers!")
                            },
                            _ => panic!("Expected number or bigint")
                        }
                    }
                    Operator::OperatorMultiply(left, right) => {
                        match self.execute_expression_and_expect_literal(&left, scope)? {
                            Literal::BigInt(l) => match self.execute_expression_and_expect_literal(right, scope)? {
                                Literal::BigInt(r) => Ok(Some(Value::Literal(Literal::BigInt(l * r)))),
                                _ => panic!("expected two bigInts!")
                            }
                            Literal::Number(l) => match self.execute_expression_and_expect_literal(right, scope)? {
                                Literal::Number(r) => Ok(Some(Value::Literal(Literal::Number(l * r)))),
                                _ => panic!("expected two numbers!")
                            },
                            _ => panic!("Expected number or bigint")
                        }
                    }
                    Operator::OperatorDivide(left, right) => {
                        match self.execute_expression_and_expect_literal(&left, scope)? {
                            Literal::BigInt(l) => match self.execute_expression_and_expect_literal(right, scope)? {
                                Literal::BigInt(r) => Ok(Some(Value::Literal(Literal::BigInt(l / r)))),
                                _ => panic!("expected two bigInts!")
                            }
                            Literal::Number(l) => match self.execute_expression_and_expect_literal(right, scope)? {
                                Literal::Number(r) => Ok(Some(Value::Literal(Literal::Number(l / r)))),
                                _ => panic!("expected two numbers!")
                            },
                            _ => panic!("Expected number or bigint")
                        }
                    }
                    Operator::OperatorPlus(left, right) => {
                        let left = self.execute_expression_and_expect_literal(left, scope)?;
                        let right = self.execute_expression_and_expect_literal(right, scope)?;

                        match left {
                            Literal::Number(left_val) => match right {
                                Literal::Number(val) => {
                                    Ok(Some(Value::Literal(Literal::Number(left_val + val))))
                                }
                                Literal::String(val) => Ok(Some(Value::Literal(Literal::String(
                                    format!("{}{}", left_val, val),
                                )))),
                                _ => panic!("Error! Invalid type for this operator"),
                            },
                            Literal::String(left_val) => match right {
                                Literal::String(val) => {
                                    Ok(Some(Value::Literal(Literal::String(left_val + &val))))
                                }
                                Literal::Number(val) => Ok(Some(Value::Literal(Literal::String(
                                    format!("{}{}", left_val, val),
                                )))),
                                Literal::Boolean(val) => Ok(Some(Value::Literal(Literal::String(
                                    format!("{}{}", left_val, val),
                                )))),
                                Literal::BigInt(val) => Ok(Some(Value::Literal(Literal::String(
                                    format!("{}{}", left_val, val)
                                )))),
                                Literal::Map(_) => panic!("Can't do operations on Map!"),
                                Literal::Null => Ok(Some(Value::Literal(Literal::String(format!(
                                    "{}{}",
                                    left_val, "null"
                                ))))),
                            },
                            Literal::Null => match right {
                                Literal::String(val) => Ok(Some(Value::Literal(Literal::String(
                                    format!("{}{}", "null", val),
                                )))),
                                _ => panic!("Error, only string is authorized on null"),
                            },
                            Literal::BigInt(left_val) => match right {
                                Literal::BigInt(val) => Ok(Some(Value::Literal(Literal::BigInt(
                                    left_val + val
                                )))),
                                _ => panic!("Only BigInt authorized")
                            }
                            Literal::Map(_) => panic!("Can't do operations on Map!"),
                            Literal::Boolean(b) => match right {
                                Literal::String(s) => Ok(Some(Value::Literal(Literal::String(
                                    format!("{}{}", b, s)
                                )))),
                                _ => panic!("Error! Invalid type for + operator")
                            }
                        }
                    }
                    Operator::OperatorMinus(left, right) => {
                        match self.execute_expression_and_expect_literal(&left, scope)? {
                            Literal::BigInt(l) => match self.execute_expression_and_expect_literal(right, scope)? {
                                Literal::BigInt(r) => Ok(Some(Value::Literal(Literal::BigInt(l - r)))),
                                _ => panic!("expected two bigInts!")
                            }
                            Literal::Number(l) => match self.execute_expression_and_expect_literal(right, scope)? {
                                Literal::Number(r) => Ok(Some(Value::Literal(Literal::Number(l - r)))),
                                _ => panic!("expected two numbers!")
                            },
                            _ => panic!("Expected number or bigint")
                        }
                    }
                }
            }
            Expression::SubExpression(sub) => self.execute_expression(sub, scope),
        }
    }

    fn wrap_lib_value(&self, value: Value, lib: String) -> Value {
        match value {
            Value::Structure(struct_type, scope) => Value::Structure(Type::LibraryType(lib, Box::new(struct_type)), scope),
            Value::Array(values) => {
                let mut vec: Vec<Value> = vec![];
                for val in values {
                    vec.push(self.wrap_lib_value(val, lib.clone()))
                }
                Value::Array(vec)
            }
            _ => value
        }
    }

    fn unwrap_lib_value(&self, value: Value, lib: &String) -> Value {
        match value {
            Value::Structure(struct_type, scope) => match struct_type {
                Type::LibraryType(lib_name, value_type) => {
                    if lib_name != *lib {
                        panic!(format!("Not same lib! Got {} expected {}", lib_name, lib));
                    }
                    Value::Structure(*value_type, scope)
                }
                _ => panic!("Unwrap lib value: error, not supported!")
            },
            Value::Array(values) => {
                let mut vec: Vec<Value> = vec![];
                for val in values {
                    vec.push(self.unwrap_lib_value(val, lib))
                }
                Value::Array(vec)
            }
            _ => value
        }
    }
}

use std::fmt;

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Literal(l) => write!(f, "{}", l),
            Value::Array(values) => {
                write!(f, "[")?;

                for value in values {
                    write!(f, "{}, ", value)?;
                }

                write!(f, "]")
            }
            Value::Structure(struct_type, scope) => {
                write!(f, "{} {{ ", struct_type)?;
                for (key, value) in &scope.variables {
                    write!(f, "{}: {}, ", key, value.value)?;
                }
                write!(f, "}}")
            }
        }
    }
}