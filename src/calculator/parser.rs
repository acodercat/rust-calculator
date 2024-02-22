use std::collections::VecDeque;
use crate::calculator::token::Token;
use crate::calculator::ast::{AST, Operator, Function};
use crate::calculator::calculator::CalculatorError;

fn check_parentheses(tokens: &[Token]) -> Result<(), CalculatorError> {
    let mut stack: VecDeque<Token> = VecDeque::new();

    for token in tokens {
        match token {
            Token::LeftParenthesis => stack.push_back(Token::LeftParenthesis),
            Token::RightParenthesis => {
                if stack.pop_back().is_none() {
                    return Err(CalculatorError::UnmatchedRightParenthesis);
                }
            },
            _ => (),
        }
    }

    if !stack.is_empty() {
        return Err(CalculatorError::UnmatchedLeftParenthesis);
    }

    Ok(())
}
pub(crate) fn parse(tokens: &[Token]) -> Result<(AST, &[Token]), CalculatorError> {
    check_parentheses(tokens)?;
    let (ast, rest) = parse_expression(tokens)?;

    if !rest.is_empty() {
        return Err(CalculatorError::ExtraTokensDetected);
    }

    Ok((ast, rest))
}

fn parse_expression(tokens: &[Token]) -> Result<(AST, &[Token]), CalculatorError> {

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
                _ => Err(CalculatorError::InvalidExpression),
            }
        },
        Some(Token::Sin) | Some(Token::Cos) | Some(Token::Ctan) | Some(Token::Tan) | Some(Token::Ln) | Some(Token::Log) => {
            let func_token = tokens.first().unwrap();
            if tokens.get(1) != Some(&Token::LeftParenthesis) {
                return Err(CalculatorError::InvalidExpression);
            }
            let (expr, rest) = parse_expression(&tokens[2..])?;
            if rest.get(0) != Some(&Token::RightParenthesis) {
                return Err(CalculatorError::InvalidExpression);
            }
            let func = match func_token {
                Token::Sin => Function::Sin,
                Token::Cos => Function::Cos,
                Token::Tan => Function::Tan,
                Token::Ln => Function::Ln,
                Token::Log => Function::Log,
                Token::Ctan => Function::Ctan,
                _ => return Err(CalculatorError::UnexpectedToken),
            };
            Ok((AST::Func(func, Box::new(expr)), &rest[1..]))
        },
        Some(Token::Pi) => Ok((AST::Const(std::f64::consts::PI), &tokens[1..])),
        Some(Token::E) => Ok((AST::Const(std::f64::consts::E), &tokens[1..])),
        Some(Token::LogBase(base)) => {
            if tokens.get(1) != Some(&Token::LeftParenthesis) {
                return Err(CalculatorError::InvalidExpression);
            }
            let (expr, rest) = parse_expression(&tokens[2..])?;
            match rest.first() {
                Some(Token::RightParenthesis) => Ok((AST::LogBase(*base, Box::new(expr)), &rest[1..])),
                _ => Err(CalculatorError::InvalidExpression),
            }
        },

        _ => Err(CalculatorError::UnexpectedToken),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::calculator::ast::{AST, Operator};
    use crate::calculator::token::Token;

    #[test]
    fn test_parse_simple_expression() {
        let tokens = vec![
            Token::Number(3.0),
            Token::Plus,
            Token::Number(2.0),
        ];
        let expected_ast = AST::BinOp(
            Box::new(AST::Num(3.0)),
            Operator::Add,
            Box::new(AST::Num(2.0)),
        );
        assert_eq!(parse(&tokens), Ok((expected_ast, &[] as &[Token])));
    }

}


