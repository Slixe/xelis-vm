use crate::lexer::*;
use crate::operator::*;
use crate::value_type::*;

use serde::{Deserialize, Serialize};

macro_rules! next_token {
    ($self: expr) => {{
        let i = $self.cursor.get();
        let v = $self.tokens.get(i);
        $self.cursor.set(i + 1);
        match v {
            Some(value) => value,
            None => return Err(ParserError::NoTokenFound),
        }
    }};
    ($self: expr, $token: ident) => {{
        let v = next_token!($self);
        if (v.token != Token::$token) {
            return Err(ParserError::UnexpectedToken(
                v.clone(),
                String::from(stringify!($token)),
            ));
        }
        v
    }};
}
use std::collections::HashMap;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Expression {
    Operator(Operator),
    Value(Literal),
    Variable(String),
    LibraryCall(String),
    FunctionCall(String, Vec<Expression>),
    ArrayCall(Box<Expression>, Box<Expression>),
    ArrayConstructor(Vec<Expression>),
    SubExpression(Box<Expression>),
    Structure(Type, HashMap<String, Expression>),
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum ExpressionHelper {
    Left,
    Operator,
    Right,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct IfStatement {
    pub condition: Expression,
    pub body: Vec<Statement>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ElseStatement {
    pub body: Vec<Statement>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Vec<Statement>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ForStatement {
    pub variable: String,
    pub values: Expression,
    pub body: Vec<Statement>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ScopeStatement {
    pub body: Vec<Statement>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct DeclarationStatement {
    pub name: String,
    pub value_type: Option<Type>,
    pub value: Option<Expression>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct AssignStatement {
    pub variable: VariablePath,
    pub expression: Expression,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum VariablePath {
    Variable(String),
    Array(Box<VariablePath>, Expression),
    SubVariable(Box<VariablePath>, Box<VariablePath>),
}

impl VariablePath {
    pub fn get_path_for_variable(expression: Expression) -> ParserResult<VariablePath> {
        let res = match expression {
            Expression::Variable(v) => VariablePath::Variable(v),
            Expression::ArrayCall(val, index) => {
                VariablePath::Array(Box::new(VariablePath::get_path_for_variable(*val)?), *index)
            }
            Expression::Operator(op) => match op {
                Operator::Dot(left, right) => {
                    let left_var = VariablePath::get_path_for_variable(*left)?;
                    let right_var = VariablePath::get_path_for_variable(*right)?;
                    VariablePath::SubVariable(Box::new(left_var), Box::new(right_var))
                }
                _ => {
                    return Err(ParserError::InvalidExpression(
                        "Expected a dot operator".to_string(),
                    ))
                }
            },
            _ => {
                return Err(ParserError::InvalidExpression(
                    "Expected a variable/array call".to_string(),
                ))
            }
        };

        Ok(res)
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Statement {
    If(IfStatement),
    Else(ElseStatement),
    ElseIf(IfStatement),
    While(WhileStatement),
    For(ForStatement),
    Expression(Expression),
    Return(Option<Expression>),
    Scope(ScopeStatement),
    Break,
    Continue,
    Variable(DeclarationStatement),
    Assign(AssignStatement),
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub statements: Vec<Statement>,
    pub ret_value: Option<Type>,
    pub entry: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Structure {
    pub name: String,
    pub fields: HashMap<String, Type>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub value_type: Type,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Constant {
    pub name: String,
    pub value_type: Type,
    pub value: Expression,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Program {
    pub libraries: HashMap<String, String>,
    pub structures: HashMap<String, Structure>,
    pub constants: HashMap<String, Constant>,
    pub functions: HashMap<String, Function>,
}

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(TokenValue, String),
    InvalidExpression(String),
    NoTokenFound,
    NoTypeOrValueFound(TokenValue),
    AlreadyRegistered(String),
    UnknownFieldInStructure(String, String),
    LibraryNameAlreadyRegistered(String),
    LibraryNotFound(String),
}

pub type ParserResult<T> = Result<T, ParserError>;

use std::cell::Cell;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Library {
    path: String,
    program: Program //TODO optimize
}

impl Library {
    pub fn new(path: String, program: Program) -> Self {
        Library { path, program }
    }

    pub fn get_path(self) -> String {
        self.path
    }

    pub fn get_program(self) -> Program {
        self.program
    }
}

pub struct Parser {
    cursor: Cell<usize>,
    tokens: Vec<TokenValue>,
    functions: HashMap<String, Function>,
    constants: HashMap<String, Constant>,
    structures: HashMap<String, Structure>,
    libraries: HashMap<String, Library>,
}

impl Parser {
    pub fn new(tokens: Vec<TokenValue>) -> Parser {
        Parser {
            cursor: Cell::new(0),
            tokens: tokens,
            functions: HashMap::new(),
            constants: HashMap::new(),
            structures: HashMap::new(),
            libraries: HashMap::new()
        }
    }

    pub fn build_program(mut self, load_library: fn(String) -> Option<Library>) -> ParserResult<Program> {
        while self.has_current() {
            let first = next_token!(self);
            match first.token {
                Token::Import => {
                    let from = next_token!(self, ValString);
                    next_token!(self, As);
                    let name = next_token!(self, Identifier).value.clone();
                    if self.libraries.contains_key(&name) {
                        return Err(ParserError::LibraryNameAlreadyRegistered(name))
                    }
                    match load_library(from.value.clone()) {
                        Some(lib) => self.libraries.insert(name, lib),
                        None => return Err(ParserError::LibraryNotFound(from.value.clone()))
                    };
                },
                Token::Function | Token::Entry => {
                    let entry = first.token == Token::Entry;
                    let function = self.read_function(entry)?;
                    if let Some(v) = self.functions.insert(function.name.clone(), function) {
                        return Err(ParserError::AlreadyRegistered(format!(
                            "Function '{}' is already registered! Function name should be unique!",
                            v.name
                        )));
                    }
                }
                Token::Struct => {
                    let identifier = next_token!(self, Identifier);
                    if let Some(v) = Type::get_native_type(&identifier) {
                        return Err(ParserError::AlreadyRegistered(format!("Invalid structure name '{}' is already registered! Structure name should be unique!", identifier.value))); 
                    }
                    next_token!(self, BraceOpen);
                    let parameters: Vec<Parameter> = self.read_parameters()?;
                    let mut map = HashMap::new();
                    for param in parameters.into_iter() {
                        map.insert(param.name, param.value_type);
                    }

                    if self.libraries.contains_key(&identifier.value) {
                        return Err(ParserError::LibraryNameAlreadyRegistered(identifier.value.clone()))
                    }

                    if let Some(v) = self.structures.insert(
                        identifier.value.clone(),
                        Structure {
                            name: identifier.value.clone(),
                            fields: map,
                        },
                    ) {
                        return Err(ParserError::AlreadyRegistered(format!("Structure '{}' is already registered! Structure name should be unique!", v.name)));
                    }
                }
                Token::Const => {
                    let name = next_token!(self, Identifier);
                    let value_type: Type;
                    let value: Expression;
                    let mut next = next_token!(self);

                    if next.token == Token::Colon {
                        value_type = self.get_type()?;
                        next = next_token!(self);
                    } else {
                        return Err(ParserError::UnexpectedToken(
                            next.clone(),
                            String::from("Value Type is required for constant"),
                        ));
                    }

                    if next.token == Token::OperatorAssign {
                        value = self.read_expression()?;
                    } else {
                        return Err(ParserError::UnexpectedToken(
                            next.clone(),
                            String::from("Constant require a value"),
                        ));
                    }

                    if self.libraries.contains_key(&name.value) {
                        return Err(ParserError::LibraryNameAlreadyRegistered(name.value.clone()))
                    }

                    if let Some(v) = self.constants.insert(
                        name.value.clone(),
                        Constant {
                            name: name.value.clone(),
                            value_type: value_type,
                            value: value,
                        },
                    ) {
                        return Err(ParserError::AlreadyRegistered(format!("Constant with name '{}' is already registered! Constant name should be unique!", v.name)));
                    }
                }
                _ => {
                    return Err(ParserError::UnexpectedToken(
                        first.clone(),
                        String::from("What should we do with that ?"),
                    ))
                }
            }
        }

        let mut libraries = HashMap::new();
        for (name, lib) in self.libraries.into_iter() {
            libraries.insert(name, lib.get_path());
        }

        return Ok(Program {
            libraries,
            structures: self.structures,
            constants: self.constants,
            functions: self.functions,
        });
    }

    fn has_current(&self) -> bool {
        return self.cursor.get() < self.tokens.len();
    }

    fn view_current(&self) -> ParserResult<&TokenValue> {
        return match self.tokens.get(self.cursor.get()) {
            Some(value) => Ok(value),
            None => return Err(ParserError::NoTokenFound),
        };
    }

    fn read_expression(&self) -> ParserResult<Expression> {
        let mut last_expression: Option<Expression> = None;
        let mut operator: Option<&Token> = None;
        let mut state: ExpressionHelper = ExpressionHelper::Left;
        let mut opt_library: Option<&String> = None;
        let mut require_operator = false;
        let mut token = self.view_current()?;
        while require_operator == token.token.is_operator() {
            next_token!(self);
            if require_operator {
                match state {
                    ExpressionHelper::Operator => {
                        state = ExpressionHelper::Right;
                        operator = Some(&token.token);
                    }
                    _ => {
                        return Err(ParserError::UnexpectedToken(
                            token.clone(),
                            String::from("State is not operator!"),
                        ))
                    }
                }
            } else {
                let current_expression: Expression = match token.token {
                    Token::ValString => Expression::Value(Literal::String(token.value.clone())),
                    Token::ValNumber => {
                        let value = match token.value.parse() {
                            Ok(value) => value,
                            Err(_) => {
                                return Err(ParserError::UnexpectedToken(
                                    token.clone(),
                                    String::from("Error while parsing number"),
                                ))
                            }
                        };
                        Expression::Value(Literal::Number(value))
                    }
                    Token::Null => Expression::Value(Literal::Null),
                    Token::True => Expression::Value(Literal::Boolean(true)),
                    Token::False => Expression::Value(Literal::Boolean(false)),
                    Token::Identifier => {
                        let next = self.view_current()?;
                        match next.token {
                            Token::ParenthesisOpen => {
                                next_token!(self);
                                let function_name = token.value.clone();
                                let mut expressions: Vec<Expression> = vec![];
                                token = self.view_current()?;
                                while token.token != Token::ParenthesisClose {
                                    let ex = self.read_expression()?;
                                    expressions.push(ex);
                                    token = self.view_current()?;
                                    if token.token == Token::Comma {
                                        next_token!(self);
                                        token = self.view_current()?;
                                    }
                                }

                                let mut expr = Expression::FunctionCall(function_name, expressions);
                                next_token!(self, ParenthesisClose);

                                if self.view_current()?.token == Token::BracketOpen {
                                    expr = self.read_array_call(expr)?;
                                }

                                expr
                            }
                            Token::BracketOpen => {
                                self.read_array_call(Expression::Variable(token.value.clone()))?
                            }
                            Token::BraceOpen => {
                                let opt_struct: Option<&Structure> = match opt_library {
                                    Some(v) => self.libraries.get(v).unwrap().program.structures.get(&token.value),
                                    None => self.structures.get(&token.value)
                                };
                                match opt_struct {
                                    Some(structure) => {
                                        next_token!(self);
                                        let mut params = HashMap::new();
                                        let mut current = self.view_current()?;
                                        while current.token != Token::BraceClose {
                                            let field = next_token!(self, Identifier).value.clone();
                                            if !structure.fields.contains_key(&field) {
                                                return Err(ParserError::UnknownFieldInStructure(structure.name.clone(), field));
                                            }
                                            current = self.view_current()?;
                                            if current.token == Token::Colon {
                                                next_token!(self);
                                                let value = self.read_expression()?;
                                                params.insert(field, value);
                                            } else { //we assume the variable name is same as field name
                                                let var = Expression::Variable(field.clone());
                                                params.insert(field, var);
                                            }

                                            current = self.view_current()?;
                                            if current.token == Token::Comma {
                                                next_token!(self);
                                                current = self.view_current()?;
                                            }
                                        }
                                        next_token!(self, BraceClose);

                                        let value_type = Type::Structure(token.value.clone());
                                        match opt_library {
                                            Some(lib) => {
                                                state = ExpressionHelper::Left;
                                                Expression::Structure(Type::LibraryType(String::from(lib), Box::new(value_type)), params)
                                            }
                                            None => Expression::Structure(value_type, params)
                                        }
                                    },
                                    None => {
                                        Expression::Variable(token.value.clone())
                                    }
                                }
                            }
                            _ => { 
                                if self.libraries.contains_key(&token.value) {
                                    opt_library = Some(&token.value);
                                    Expression::LibraryCall(token.value.clone())
                                } else {
                                    Expression::Variable(token.value.clone())
                                }
                            }
                        }
                    }
                    Token::ParenthesisOpen => {
                        let part: Expression =
                            Expression::SubExpression(Box::new(self.read_expression()?));
                        next_token!(self, ParenthesisClose);
                        part
                    }
                    Token::BracketOpen => self.read_array_values()?,
                    _ => break,
                };

                match state {
                    ExpressionHelper::Left => {
                        //only used the first time
                        state = ExpressionHelper::Operator;
                        last_expression = Some(current_expression);
                    }
                    ExpressionHelper::Operator => {
                        return Err(ParserError::InvalidExpression(String::from(
                            "How is it possible ? Operator!",
                        )))
                    }
                    ExpressionHelper::Right => {
                        state = ExpressionHelper::Operator;
                        if let Some(value) = operator {
                            let op = Operator::value_of(
                                value,
                                Box::new(
                                    last_expression.take().expect("Expecting a left expression"),
                                ),
                                Box::new(current_expression),
                            )
                            .expect("No Operator found");
                            last_expression = Some(Expression::Operator(op));
                        } else {
                            return Err(ParserError::InvalidExpression(String::from(
                                "How is it possible ? No operator found!",
                            )));
                        }
                    }
                }
            }

            require_operator = !require_operator;
            token = self.view_current()?;
        }

        match last_expression {
            Some(value) => Ok(value),
            None => Err(ParserError::UnexpectedToken(
                token.clone(),
                String::from("Unexpected token while parsing expression"),
            )),
        }
    }

    fn read_function(&self, entry: bool) -> ParserResult<Function> {
        let identifier = next_token!(self, Identifier);
        next_token!(self, ParenthesisOpen);
        let parameters = self.read_parameters()?;
        let current = self.view_current()?;
        let opt_ret_value: Option<Type>;

        if current.token == Token::Colon {
            next_token!(self);
            opt_ret_value = match self.get_type() {
                Ok(val) => Some(val),
                Err(e) => {
                    return Err(e)
                }
            };
        } else if current.token == Token::BraceOpen {
            if entry {
                opt_ret_value = Some(Type::Number);
            } else {
                opt_ret_value = None;
            }
        } else {
            return Err(ParserError::UnexpectedToken(
                current.clone(),
                String::from("Error while parsing function returned value"),
            ));
        }

        let statements: Vec<Statement> = self.read_body()?;

        Ok(Function {
            name: identifier.value.clone(),
            parameters: parameters,
            statements: statements,
            ret_value: opt_ret_value,
            entry: entry,
        })
    }

    fn read_body(&self) -> ParserResult<Vec<Statement>> {
        next_token!(self, BraceOpen);
        let mut statements: Vec<Statement> = vec![];
        let mut current = self.view_current()?;

        while current.token != Token::BraceClose {
            let statement = self.read_statement(&mut statements)?;
            statements.push(statement);
            current = self.view_current()?;
        }

        next_token!(self, BraceClose);

        return Ok(statements);
    }

    fn read_statement(&self, statements: &mut Vec<Statement>) -> ParserResult<Statement> {
        let res: Statement = match self.view_current()?.token {
            Token::For => self.read_statement_for()?,
            Token::While => self.read_statement_while()?,
            Token::If => self.read_statement_if()?,
            Token::Else => self.read_statement_else()?,
            Token::Break => Statement::Break,
            Token::Continue => Statement::Continue,
            Token::BraceOpen => self.read_statement_scope()?,
            Token::Return => self.read_statement_return()?,
            Token::Let => self.read_statement_let()?,
            Token::OperatorPlusAssign => {
                self.read_statement_assign_with(Token::OperatorPlus, statements)?
            }
            Token::OperatorMinusAssign => {
                self.read_statement_assign_with(Token::OperatorMinus, statements)?
            }
            Token::OperatorMultiplyAssign => {
                self.read_statement_assign_with(Token::OperatorMultiply, statements)?
            }
            Token::OperatorDivideAssign => {
                self.read_statement_assign_with(Token::OperatorDivide, statements)?
            }
            Token::OperatorAssign => self.read_statement_assign(statements)?,
            _ => Statement::Expression(self.read_expression()?),
        };

        Ok(res)
    }

    fn read_statement_for(&self) -> ParserResult<Statement> {
        next_token!(self, For);
        let variable = next_token!(self, Identifier).value.clone();
        next_token!(self, In);
        let values = self.read_expression()?;
        let statements = self.read_body()?;

        Ok(Statement::For(ForStatement {
            variable,
            values,
            body: statements,
        }))
    }

    fn read_statement_while(&self) -> ParserResult<Statement> {
        next_token!(self, While);
        let condition = self.read_expression()?;
        let statements = self.read_body()?;

        Ok(Statement::While(WhileStatement {
            condition: condition,
            body: statements,
        }))
    }

    fn read_statement_if(&self) -> ParserResult<Statement> {
        next_token!(self, If);
        let condition = self.read_expression()?;
        let statements = self.read_body()?;

        Ok(Statement::If(IfStatement {
            condition: condition,
            body: statements,
        }))
    }

    fn read_statement_else(&self) -> ParserResult<Statement> {
        next_token!(self, Else);
        if self.view_current()?.token == Token::If {
            next_token!(self);
            let condition = self.read_expression()?;
            let statements = self.read_body()?;
    
            Ok(Statement::ElseIf(IfStatement {
                condition: condition,
                body: statements,
            }))
        } else {
            let statements = self.read_body()?;
            Ok(Statement::Else(ElseStatement { body: statements }))
        }
    }

    fn read_statement_scope(&self) -> ParserResult<Statement> {
        let statements = self.read_body()?;

        Ok(Statement::Scope(ScopeStatement { body: statements }))
    }

    fn read_statement_return(&self) -> ParserResult<Statement> {
        //TODO verify optional expression
        next_token!(self, Return);
        let expression = match self.read_expression() {
            Ok(value) => Some(value),
            Err(_) => None,
        };

        Ok(Statement::Return(expression))
    }

    fn read_statement_let(&self) -> ParserResult<Statement> {
        next_token!(self, Let);
        let identifier = next_token!(self, Identifier);
        let mut token = self.view_current()?;
        let var_type: Option<Type>;
        let _value: Option<Expression>;

        if token.token == Token::Colon {
            next_token!(self);
            var_type = Some(self.get_type()?);
            token = self.view_current()?;
        } else {
            var_type = None;
        }

        if token.token == Token::OperatorAssign {
            next_token!(self);
            _value = match self.read_expression() {
                Ok(value) => Some(value),
                Err(err) => return Err(err),
            };
        } else {
            _value = None;
        }

        if _value.is_none() && var_type.is_none() {
            //if we don't have a value or a type, then we don't accept it.
            return Err(ParserError::NoTypeOrValueFound(token.clone()));
        }

        Ok(Statement::Variable(DeclarationStatement {
            name: identifier.value.clone(),
            value_type: var_type,
            value: _value,
        }))
    }

    fn read_statement_assign(&self, statements: &mut Vec<Statement>) -> ParserResult<Statement> {
        next_token!(self, OperatorAssign);
        let last_statement = statements.remove(statements.len() - 1);
        let variable = match last_statement {
            Statement::Expression(exp) => VariablePath::get_path_for_variable(exp)?,
            _ => {
                return Err(ParserError::InvalidExpression(
                    "Unexpected statement before assignation!".to_string(),
                ))
            }
        };

        let expression = self.read_expression()?;

        Ok(Statement::Assign(AssignStatement {
            variable,
            expression,
        }))
    }

    fn read_statement_assign_with(
        &self,
        token: Token,
        statements: &mut Vec<Statement>,
    ) -> ParserResult<Statement> {
        self.cursor.set(self.cursor.get() + 1);
        let last_statement = statements.remove(statements.len() - 1);
        let variable_expression: Expression;
        let variable = match last_statement {
            Statement::Expression(exp) => {
                variable_expression = exp.clone();
                VariablePath::get_path_for_variable(exp)?
            }
            _ => {
                return Err(ParserError::InvalidExpression(
                    "Unexpected statement before assignation!".to_string(),
                ))
            }
        };

        let expression = match Operator::value_of(
            &token,
            Box::new(variable_expression),
            Box::new(self.read_expression()?),
        ) {
            Some(value) => Expression::Operator(value),
            None => return Err(ParserError::NoTokenFound),
        };

        Ok(Statement::Assign(AssignStatement {
            variable,
            expression,
        }))
    }

    fn read_parameters(&self) -> ParserResult<Vec<Parameter>> {
        let mut parameters = vec![];
        let mut current = next_token!(self);
        while current.token != Token::BraceClose && current.token != Token::ParenthesisClose {
            let name = current;
            next_token!(self, Colon);

            parameters.push(Parameter {
                name: name.value.clone(),
                value_type: self.get_type()?,
            });

            current = next_token!(self);
            if current.token == Token::Comma {
                current = next_token!(self);
            }
        }

        Ok(parameters)
    }

    fn read_array_values(&self) -> ParserResult<Expression> {
        let mut values: Vec<Expression> = vec![];
        let mut current = self.view_current()?;

        if current.token != Token::BracketClose {
            loop {
                let ex = self.read_expression()?;
                values.push(ex);
                current = self.view_current()?;
                if current.token == Token::Comma {
                    next_token!(self);
                } else {
                    break;
                }
            }
        }

        next_token!(self, BracketClose);

        Ok(Expression::ArrayConstructor(values))
    }

    fn read_array_call(&self, expr: Expression) -> ParserResult<Expression> {
        let mut final_expr: Expression = expr;
        loop {
            next_token!(self, BracketOpen);
            let index = self.read_expression()?;
            next_token!(self, BracketClose);

            final_expr = Expression::ArrayCall(Box::new(final_expr), Box::new(index));

            if self.view_current()?.token != Token::BracketOpen {
                break;
            }
        }

        Ok(final_expr)
    }
    fn get_type(&self) -> ParserResult<Type> {
        let current = next_token!(self);
        let mut _type: ParserResult<Type>;
        if current.token == Token::BracketOpen {
            next_token!(self, BracketClose);
            _type = match self.get_type() {
                Ok(v) => Ok(Type::Array(Box::new(v))),
                Err(v) => Err(v),
            };
        } else {
            _type = match Type::get_type(&self.structures, current) {
                Some(value) => Ok(value),
                None => match self.libraries.get(&current.value) { //last chance: lib Type
                    Some(lib) => {
                        next_token!(self, Dot);
                        let next = next_token!(self, Identifier);
                        match Type::get_type(&lib.program.structures, next) {
                            Some(v) => Ok(Type::LibraryType(current.value.clone(), Box::new(v))),
                            None => return Err(ParserError::UnexpectedToken(
                                current.clone(),
                                format!("Type '{}' from library '{}' not found!", next.value.clone(), current.value.clone()),
                            ))
                        }
                    },
                    None => return Err(ParserError::UnexpectedToken(
                        current.clone(),
                        String::from("Expected a valid type"),
                    ))
                }
            }
        }

        _type
    }
}