use super::lexer::Token;
use super::parser::Expression;

#[derive(Debug)]
pub enum Operator {
    OperatorAssign(Box<Expression>, Box<Expression>),
    OperatorEquals(Box<Expression>, Box<Expression>),
    OperatorNotEquals(Box<Expression>, Box<Expression>),
    OperatorAnd(Box<Expression>, Box<Expression>),
    OperatorOr(Box<Expression>, Box<Expression>),
    OperatorGreaterThan(Box<Expression>, Box<Expression>),
    OperatorLessThan(Box<Expression>, Box<Expression>),
    OperatorGreaterOrEqual(Box<Expression>, Box<Expression>),
    OperatorLessOrEqual(Box<Expression>, Box<Expression>),
    OperatorPlus(Box<Expression>, Box<Expression>),
    OperatorMinus(Box<Expression>, Box<Expression>),
    OperatorMultiply(Box<Expression>, Box<Expression>),
    OperatorDivide(Box<Expression>, Box<Expression>),
    OperatorModulo(Box<Expression>, Box<Expression>),
    OperatorPlusAssign(Box<Expression>, Box<Expression>),
    OperatorMinusAssign(Box<Expression>, Box<Expression>),
    OperatorMultiplyAssign(Box<Expression>, Box<Expression>),
    OperatorDivideAssign(Box<Expression>, Box<Expression>),
    OperatorBitwiseLeft(Box<Expression>, Box<Expression>),
    OperatorBitwiseRight(Box<Expression>, Box<Expression>),

    Dot(Box<Expression>, Box<Expression>),
}

use Operator::*;
impl Operator {
    pub fn value_of(
        token: &Token,
        left: Box<Expression>,
        right: Box<Expression>,
    ) -> Option<Operator> {
        let value = match token {
            Token::OperatorAssign => OperatorAssign(left, right),
            Token::OperatorEquals => OperatorEquals(left, right),
            Token::OperatorNotEquals => OperatorNotEquals(left, right),
            Token::OperatorAnd => OperatorAnd(left, right),
            Token::OperatorOr => OperatorOr(left, right),
            Token::OperatorGreaterThan => OperatorGreaterThan(left, right),
            Token::OperatorLessThan => OperatorLessThan(left, right),
            Token::OperatorGreaterOrEqual => OperatorGreaterOrEqual(left, right),
            Token::OperatorLessOrEqual => OperatorLessOrEqual(left, right),
            Token::OperatorPlus => OperatorPlus(left, right),
            Token::OperatorMinus => OperatorMinus(left, right),
            Token::OperatorMultiply => OperatorMultiply(left, right),
            Token::OperatorDivide => OperatorDivide(left, right),
            Token::OperatorModulo => OperatorModulo(left, right),
            Token::OperatorPlusAssign => OperatorPlusAssign(left, right),
            Token::OperatorMinusAssign => OperatorMinusAssign(left, right),
            Token::OperatorMultiplyAssign => OperatorMultiplyAssign(left, right),
            Token::OperatorDivideAssign => OperatorDivideAssign(left, right),
            Token::OperatorBitwiseLeft => OperatorBitwiseLeft(left, right),
            Token::OperatorBitwiseRight => OperatorBitwiseRight(left, right),
            Token::Dot => Dot(left, right),

            _ => return None,
        };

        Some(value)
    }
}
