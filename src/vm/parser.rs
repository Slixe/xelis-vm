use super::lexer::*;
use super::operator::*;
use super::value_type::*;

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

#[derive(Debug, Clone)]
pub enum Expression {
    Operator(Operator),
    Value(Literal),
    Variable(String),
    FunctionCall(String, Vec<Expression>),
    ArrayCall(Box<Expression>, Box<Expression>),
    ArrayConstructor(Vec<Expression>),
    SubExpression(Box<Expression>),
    Structure(String, HashMap<String, Expression>),
}

#[derive(Debug, Clone)]
pub enum ExpressionHelper {
    Left,
    Operator,
    Right,
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub condition: Expression,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct ElseStatement {
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct ForStatement {
    pub condition: Expression,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct ScopeStatement {
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct DeclarationStatement {
    pub name: String,
    pub value_type: Option<Type>,
    pub value: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct AssignStatement {
    pub variable: VariableAssign,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub enum VariableAssign {
    Variable(String),
    Array(Box<VariableAssign>, Expression),
    //SubVariable(Box<VariableAssign>, Box<VariableAssign>)
}

#[derive(Debug, Clone)]
pub enum Statement {
    If(IfStatement),
    Else(ElseStatement),
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

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub statements: Vec<Statement>,
    pub ret_value: Option<Type>,
    pub entry: bool,
}

#[derive(Debug, Clone)]
pub struct Structure {
    pub name: String,
    pub parameters: Vec<Parameter>,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub value_type: Type,
}

#[derive(Debug, Clone)]
pub struct Constant {
    pub name: String,
    pub value_type: Type,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub structures: Vec<Structure>,
    pub constants: Vec<Constant>,
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(TokenValue, String),
    InvalidExpression(String),
    NoTokenFound,
    NoTypeOrValueFound(TokenValue),
}

pub type ParserResult<T> = Result<T, ParserError>;

use std::cell::Cell;

pub struct Parser {
    cursor: Cell<usize>,
    tokens: Vec<TokenValue>,
    functions: Vec<Function>,
    constants: Vec<Constant>,
    structures: Vec<Structure>,
}

impl Parser {
    pub fn new(tokens: Vec<TokenValue>) -> Parser {
        Parser {
            cursor: Cell::new(0),
            tokens: tokens,
            functions: vec![],
            constants: vec![],
            structures: vec![],
        }
    }

    pub fn build_program(mut self) -> ParserResult<Program> {
        while self.has_current() {
            let first = next_token!(self);
            match first.token {
                Token::Function | Token::Entry => {
                    let entry = first.token == Token::Entry;
                    let function = self.read_function(entry)?;
                    self.functions.push(function);
                }
                Token::Struct => {
                    let identifier = next_token!(self, Identifier);
                    next_token!(self, BraceOpen);
                    let parameters: Vec<Parameter> = self.read_parameters()?;
                    self.structures.push(Structure {
                        name: identifier.value.clone(),
                        parameters: parameters,
                    });
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

                    self.constants.push(Constant {
                        name: name.value.clone(),
                        value_type: value_type,
                        value: value,
                    });
                }
                _ => {
                    return Err(ParserError::UnexpectedToken(
                        first.clone(),
                        String::from("What should we do with that ?"),
                    ))
                }
            }
        }

        return Ok(Program {
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
        let mut expr: Option<Expression> = None;
        let mut operator: Option<&Token> = None;
        let mut state: ExpressionHelper = ExpressionHelper::Left;

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
                let expression: Expression = match token.token {
                    Token::ValString => Expression::Value(Literal::String(token.value.clone())),
                    Token::ValNumber => {
                        let value: usize = match token.value.parse() {
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
                                    next_token!(self);
                                    let ex = self.read_expression()?;
                                    next_token!(self, BracketClose);

                                    require_operator = !require_operator;
                                    expr = Expression::ArrayCall(Box::new(expr), Box::new(ex));
                                }
                                expr
                            }
                            Token::BracketOpen => {
                                next_token!(self);
                                let ex = self.read_expression()?;
                                next_token!(self, BracketClose);

                                Expression::ArrayCall(
                                    Box::new(Expression::Variable(token.value.clone())),
                                    Box::new(ex),
                                )
                            }
                            Token::BraceOpen => {
                                next_token!(self);
                                let mut params = HashMap::new();
                                let mut current = self.view_current()?;
                                while current.token != Token::BraceClose {
                                    let field = next_token!(self, Identifier).value.clone();
                                    next_token!(self, Colon);
                                    let value = self.read_expression()?;
                                    params.insert(field, value);
                                    current = self.view_current()?;
                                    if current.token == Token::Comma {
                                        next_token!(self);
                                        current = self.view_current()?;
                                    }
                                }
                                next_token!(self, BraceClose);
                                Expression::Structure(token.value.clone(), params)
                            },
                            _ => Expression::Variable(token.value.clone()),
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
                        expr = Some(expression);
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
                                Box::new(expr.take().expect("Expecting a left expression")),
                                Box::new(expression),
                            )
                            .expect("No Operator found");
                            expr = Some(Expression::Operator(op));
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

        match expr {
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
                Err(_) => None,
            };
        } else if current.token == Token::BraceOpen {
            opt_ret_value = None;
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
            Token::OperatorAssign => self.read_statement_assign(statements)?,
            _ => Statement::Expression(self.read_expression()?),
        };

        Ok(res)
    }

    fn read_statement_for(&self) -> ParserResult<Statement> {
        next_token!(self, For);
        let condition = self.read_expression()?;
        let statements = self.read_body()?;

        Ok(Statement::For(ForStatement {
            condition: condition,
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
        let statements = self.read_body()?;

        Ok(Statement::Else(ElseStatement { body: statements }))
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
            Statement::Expression(exp) => self.get_path_for_variable(exp)?, /*match exp {
            Expression::Variable(var) => var,
            _ => return Err(ParserError::InvalidExpression(format!("Expected a variable before assignation: {:?}", exp)))
            },*/
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

    fn get_path_for_variable(&self, expression: Expression) -> ParserResult<VariableAssign> {
        let res = match expression {
            Expression::Variable(v) => VariableAssign::Variable(v),
            Expression::ArrayCall(val, index) => {
                VariableAssign::Array(Box::new(self.get_path_for_variable(*val)?), *index)
            }
            _ => {
                return Err(ParserError::InvalidExpression(
                    "Expected a variable/array call".to_string(),
                ))
            }
        };

        Ok(res)
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
                None => {
                    return Err(ParserError::UnexpectedToken(
                        current.clone(),
                        String::from("Expected a valid type"),
                    ))
                }
            }
        }

        _type
    }
}
