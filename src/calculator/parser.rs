use crate::calculator::token::Token;
use crate::calculator::ast::{AST, Operator};
use crate::calculator::calculator::CalculatorError;

pub(crate) fn parse_expression(tokens: &[Token]) -> Result<(AST, &[Token]), CalculatorError> {
    let (mut lhs, mut rest) = parse_term(tokens)?;
    while let Some(token) = rest.first() {
        match token {
            Token::Plus | Token::Minus => {
                let (rhs, next_tokens) = parse_term(&rest[1..])?;
                lhs = AST::BinOp(
                    Box::new(lhs),
                    if matches!(token, Token::Plus) { Operator::Add } else { Operator::Sub },
                    Box::new(rhs),
                );
                rest = next_tokens;
            },
            _ => break,
        }
    }
    Ok((lhs, rest))
}

fn parse_term(tokens: &[Token]) -> Result<(AST, &[Token]), CalculatorError> {
    let (mut lhs, mut rest) = parse_factor(tokens)?;
    while let Some(token) = rest.first() {
        match token {
            Token::Multiply | Token::Divide => {
                let (rhs, next_tokens) = parse_factor(&rest[1..])?;
                lhs = AST::BinOp(
                    Box::new(lhs),
                    if matches!(token, Token::Multiply) { Operator::Mul } else { Operator::Div },
                    Box::new(rhs),
                );
                rest = next_tokens;
            },
            _ => break,
        }
    }
    Ok((lhs, rest))
}

fn parse_factor(tokens: &[Token]) -> Result<(AST, &[Token]), CalculatorError> {
    match tokens.first() {
        Some(Token::Number(n)) => Ok((AST::Num(*n), &tokens[1..])),
        Some(Token::Minus) => {
            let (rhs, rest) = parse_factor(&tokens[1..])?;
            let lhs = AST::Num(-1.0);
            Ok((AST::BinOp(Box::new(lhs), Operator::Mul, Box::new(rhs)), rest))
        },
        Some(Token::Variable(name)) => Ok((AST::Var(name.clone()), &tokens[1..])),
        Some(Token::LeftParenthesis) => {
            let (expr, rest) = parse_expression(&tokens[1..])?;
            match rest.first() {
                Some(Token::RightParenthesis) => Ok((expr, &rest[1..])),
                _ => Err(CalculatorError::MissingRightParenthesis),
            }
        },
        _ => Err(CalculatorError::UnexpectedToken),
    }
}
