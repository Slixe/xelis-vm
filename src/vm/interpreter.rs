use super::operator::*;
use super::parser::*;
use super::value_type::*;

use std::collections::HashMap;

pub struct Environment {
    pub scope: Scope,
    pub structures: HashMap<String, Structure>,
    pub functions: HashMap<String, FunctionType>,
    pub type_functions: HashMap<Type, HashMap<String, FunctionType>>
}

#[derive(Debug)]
pub struct Func {
    pub parameters: Vec<Type>,
    pub execution: fn(Vec<Value>) -> Option<Value>,
}

pub struct FuncType {
    pub parameters: Vec<Type>,
    pub execution: fn(&mut Value, Vec<Value>) -> Option<Value>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            scope: Scope::new(),
            structures: HashMap::new(),
            functions: HashMap::new(),
            type_functions: HashMap::new(),
        }
    }

    pub fn bind_native_function(&mut self, name: String, function: fn(Vec<Value>) -> Option<Value>,  parameters: Vec<Type>) {
        if let Some(_) = self.functions.insert(name, FunctionType::Builtin(Func {
            parameters: parameters,
            execution: function
        })) {
            panic!("Function already exist!");
        }
    }

    pub fn bind_native_function_on_type(&mut self, value_type: Type, name: String, function: fn(&mut Value, Vec<Value>) -> Option<Value>,  parameters: Vec<Type>) {
        let map: &mut HashMap<String, FunctionType> = match self.type_functions.get_mut(&value_type) {
            Some(v) => v,
            None => {
                self.type_functions.insert(value_type.clone(), HashMap::new());
                match self.type_functions.get_mut(&value_type) {
                    Some(v) => v,
                    None => panic!("How is it possible ? Map is inserted on previous line!")
                }
            }
        };
        if let Some(_) = map.insert(name, FunctionType::Type(FuncType {
            parameters: parameters,
            execution: function
        })) {
            panic!("Function already exist!");
        }
    }
}

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
            FunctionType::Type(f) => f.parameters.iter().map(|p| p).collect()
        }
    }
}

pub struct Interpreter {
    constants: Scope,
    functions: HashMap<String, FunctionType>,
    structures: HashMap<String, Structure>,
    type_fuctions: HashMap<Type, HashMap<String, FunctionType>>
}

#[derive(Debug)]
pub struct Structure {
    pub fields: HashMap<String, Type>,
}

#[derive(Debug, Clone)]
pub enum Value {
    Array(Vec<Value>),
    Literal(Literal),
    Structure(String, Scope),
}
#[derive(Debug, Clone)]
pub struct Variable {
    pub value: Value,
    pub value_type: Type,
}

#[derive(Debug, Clone)]
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
    pub fn new(program: Program, environment: Environment) -> Interpreter {
        let mut interpreter = Interpreter {
            constants: environment.scope,
            functions: environment.functions,
            structures: environment.structures,
            type_fuctions: environment.type_functions
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
            if interpreter.functions.contains_key(&function.name) {
                panic!("Function '{}' already registered!", function.name);
            }
            interpreter
                .functions
                .insert(function.name.clone(), FunctionType::Custom(function));
        }

        for structure in program.structures {
            if interpreter.functions.contains_key(&structure.name) {
                panic!("Structure '{}' already registered!", structure.name);
            }

            let mut fields = HashMap::new();
            for param in structure.parameters {
                fields.insert(param.name, param.value_type);
            }
            interpreter
                .structures
                .insert(structure.name, Structure { fields });
        }

        return interpreter;
    }

    pub fn run_function(&self, entry: String, parameters: Vec<Value>) -> Option<Value> {
        let func: &FunctionType = match self.functions.get(&entry) {
            Some(func) => func,
            None => {
                panic!(format!("Function '{}' not found", entry));
            }
        };

        if !func.is_entry() {
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
        for e in parameters {
            values.push(self.execute_expression(&e, scope)?);
        }

        let func = self.functions.get(name)?;
        self.execute_function(&func, values)
    }

    fn execute_function(&self, func: &FunctionType, values: Vec<Value>) -> Option<Value> {
        let types = func.get_parameters_type();
        if values.len() != types.len() {
            panic!("Parameters / values length mismatch");
        }

        let mut i = 0;
        while i < types.len() {
            let value_type = types[i];
            let value = &values[i];
            if self.get_type_of_value(value) != *value_type {
                panic!(
                    "Invalid value type for parameter {} expected {:?} found {:?}!",
                    i, value_type, value
                );
            }
            i = i + 1;
        }

        match func {
            FunctionType::Builtin(f) => (f.execution)(values),
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
                    );
                    i += 1;
                }

                self.execute_statements(&f.statements, &mut scope)
            }
            _ => panic!("Invalid function type")
        }
    }

    fn execute_function_type(&self, val: &mut Value, function_name: &String, parameters: &Vec<Expression>, scope: &Scope) -> Option<Value> {
        let value_type = self.get_type_of_value(&val);
        match self.type_fuctions.get(&value_type) {
            Some(map) => match map.get(function_name) {
                Some(v) => match v {
                        FunctionType::Type(f) => {
                            let mut values: Vec<Value> = vec![];
                            if f.parameters.len() != parameters.len() {
                                panic!("Parameters / values length mismatch");
                            }

                            let mut i = 0;
                            while i < f.parameters.len() {
                                let value = self.execute_expression(&parameters[i], scope)?;
                                let value_type = self.get_type_of_value(&value);
                                if value_type != f.parameters[i] {
                                    panic!("Invalid value type for parameter {}, expected {:?} found {:?}", i, f.parameters[i], value_type);
                                }
                                values.push(value);
                                i += 1;
                            }

                            (f.execution)(val, values)
                        }
                        _ => panic!("Invalid function type")
                },
                None => panic!("No function named '{}' found for type {:?} !", function_name, value_type)
            },
            None => panic!("No function found for type {:?} !", value_type)
        }
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
                    self.assign_value(
                        &value.variable,
                        &value.expression,
                        scope,
                        &expr_scope
                    );
                }
                Statement::While(value) => {
                    while match self
                        .execute_expression_and_expect_literal(&value.condition, scope)?
                    {
                        Literal::Boolean(v) => v,
                        _ => panic!("Expected a valid condition"),
                    } {
                        if let Some(v) = self.execute_statements(&value.body, scope) {
                            return Some(v);
                        }
                    }
                }
                Statement::Scope(value) => {
                    if let Some(v) = self.execute_statements(&value.body, scope) {
                        return Some(v);
                    }
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

    fn execute_expression_and_expect_number(&self, expression: &Expression, scope: &Scope) -> usize {
        match self.execute_expression_and_expect_literal(expression, scope) {
            Some(v) => match v {
                Literal::Number(n) => n,
                literal => panic!("Expected a number but got {:?}", literal)
            },
            None => panic!("No value found! Expected a number!")
        }
    }

    fn get_variable<'a>(&self, path: &VariablePath, scope: &'a mut Scope) -> Option<(&'a mut Value, &'a Type)> {
        match path {
            VariablePath::Variable(v) => {
                let var = scope.get_mut_variable(v)?;
                Some((&mut var.value, &var.value_type))
            },
            VariablePath::SubVariable(left, right) => {
                let left_var = self.get_variable(left, scope)?;
                match left_var.0 {
                    Value::Structure(_, ref mut values) => {
                        self.get_variable(right, values)
                    }
                    _ => panic!("Expected a Structure value, how is it possible ?"),
                }
            }
            VariablePath::Array(v, index) => {
                let i = self.execute_expression_and_expect_number(index, scope);
                let tuple = self.get_variable(v, scope)?;

                match tuple.0 {
                    Value::Array(ref mut values) => {
                        return Some((&mut values[i], tuple.1)) //TODO verify if type is tuple.1 is same as values[i]
                    }
                    _ => {}
                };
                None
            }
        }
    }

    fn assign_value(&self, path: &VariablePath, expression_value: &Expression, scope: &mut Scope, expression_scope: &Scope) {
        let value = match self.execute_expression(&expression_value, expression_scope) {
            Some(v) => v,
            None => panic!("No value returned from this expression!")
        };
        let (var_value, var_type) = match self.get_variable(path, scope) {
            Some(v) => v,
            None => panic!("No variable found for this path: {:?}", path)
        };
        *var_value = value;
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
            _ => panic!(
                "Only literal are allowed! Expression: {}",
                serde_json::to_string_pretty(&expr).unwrap()
            ),
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
                        ref mut val => match right.as_ref() {
                            Expression::FunctionCall(name, params) => {
                                let path = VariablePath::get_path_for_variable(*left.clone());
                                self.execute_function_type(val, name, params, scope) //TODO update current value
                            }
                            v => panic!("Got '{:?}' called on '{:?}', what is it supposed to do ?", v, val),
                        }
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
