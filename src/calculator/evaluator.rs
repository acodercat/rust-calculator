use crate::calculator::ast::{AST, Operator, Function};
use crate::calculator::calculator::CalculatorError;
use crate::calculator::token::Token;
use crate::calculator::parser::parse;

fn extract_coefficients_and_constants(ast: &AST) -> Result<(f64, f64), CalculatorError> {
    match ast {
        AST::Num(n) => Ok((0.0, *n)),
        AST::Var(_) => Ok((1.0, 0.0)),
        AST::BinOp(lhs, op, rhs) => {
            let (lhs_coeff, lhs_const) = extract_coefficients_and_constants(lhs)?;
            let (rhs_coeff, rhs_const) = extract_coefficients_and_constants(rhs)?;

            match op {
                Operator::Add => Ok((lhs_coeff + rhs_coeff, lhs_const + rhs_const)),
                Operator::Sub => Ok((lhs_coeff - rhs_coeff, lhs_const - rhs_const)),
                Operator::Mul => {
                    if lhs_coeff == 0.0 {
                        Ok((rhs_coeff * lhs_const, rhs_const * lhs_const))
                    } else if rhs_coeff == 0.0 {
                        Ok((lhs_coeff * rhs_const, lhs_const * rhs_const))
                    } else {
                        Err(CalculatorError::InvalidExpression)
                    }
                },
                Operator::Div => {
                    if rhs_coeff != 0.0 {
                        Err(CalculatorError::InvalidExpression)
                    } else if rhs_const == 0.0 {
                        Err(CalculatorError::DivisionByZero)
                    } else {
                        Ok((lhs_coeff / rhs_const, lhs_const / rhs_const))
                    }
                },
            }
        },
        _ => return Err(CalculatorError::UnexpectedToken),
    }
}

pub(crate) fn solve_equation(tokens: &[Token]) -> Result<f64, CalculatorError> {
    let equal_pos = tokens.iter().position(|t| *t == Token::Equal)
        .ok_or(CalculatorError::ParseError)?;

    let (left_tokens, right_tokens) = tokens.split_at(equal_pos);
    let right_tokens = &right_tokens[1..];

    let (left_ast, _) = parse(left_tokens)?;
    let (right_ast, _) = parse(right_tokens)?;

    let (left_coefficient, left_constant) = extract_coefficients_and_constants(&left_ast)?;
    let (right_coefficient, right_constant) = extract_coefficients_and_constants(&right_ast)?;

    let a = left_coefficient - right_coefficient;
    let b = right_constant - left_constant;

    if a == 0.0 {
        return Err(CalculatorError::InvalidExpression);
    }

    Ok(b / a)
}


pub(crate) fn evaluate_infix(ast: &AST) -> Result<f64, CalculatorError> {
    match ast {
        AST::Num(n) => Ok(*n),
        AST::Var(_) => Err(CalculatorError::InvalidExpression),
        AST::BinOp(lhs, op, rhs) => {
            let lhs_val = evaluate_infix(lhs)?;
            let rhs_val = evaluate_infix(rhs)?;
            match op {
                Operator::Add => Ok(lhs_val + rhs_val),
                Operator::Sub => Ok(lhs_val - rhs_val),
                Operator::Mul => Ok(lhs_val * rhs_val),
                Operator::Div => {
                    if rhs_val == 0.0 {
                        Err(CalculatorError::DivisionByZero)
                    } else {
                        Ok(lhs_val / rhs_val)
                    }
                },
            }
        },
        AST::Func(func, arg) => {
            let arg_val = evaluate_infix(arg)?;
            match func {
                Function::Log => Ok(arg_val.log10()),
                Function::Ln => Ok(arg_val.ln()),
                Function::Sin => Ok(arg_val.sin()),
                Function::Cos => Ok(arg_val.cos()),
                Function::Tan => Ok(arg_val.tan()),
                Function::Ctan => Ok(1.0 / arg_val.tan()),
            }
        },
        AST::Const(c) => Ok(*c),
        AST::LogBase(base, expr) => {
            let base_val = *base;
            let expr_val = evaluate_infix(expr)?;
            Ok(expr_val.log(base_val))
        },
    }
}

pub(crate) fn evaluate_postfix(tokens: &[Token]) -> Result<f64, CalculatorError> {
    let mut stack: Vec<f64> = Vec::new();

    for token in tokens {
        match token {
            Token::Number(n) => stack.push(*n),
            Token::Plus | Token::Minus | Token::Multiply | Token::Divide => {
                if stack.len() < 2 {
                    return Err(CalculatorError::InvalidExpression);
                }
                let rhs = stack.pop().unwrap();
                let lhs = stack.pop().unwrap();
                let result = match token {
                    Token::Plus => lhs + rhs,
                    Token::Minus => lhs - rhs,
                    Token::Multiply => lhs * rhs,
                    Token::Divide => {
                        if rhs == 0.0 {
                            return Err(CalculatorError::DivisionByZero);
                        }
                        lhs / rhs
                    },
                    _ => unreachable!(),
                };
                stack.push(result);
            },
            _ => return Err(CalculatorError::UnexpectedToken),
        }
    }

    if stack.len() != 1 {
        return Err(CalculatorError::InvalidExpression);
    }

    stack.pop().ok_or(CalculatorError::InvalidExpression)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::calculator::parser::parse;

    #[test]
    fn test_solve_simple_equation() {
        let tokens = vec![Token::Variable("x".to_string()), Token::Plus, Token::Number(40.0), Token::Equal, Token::Number(42.0)];
        let result = solve_equation(&tokens);
        assert_eq!(result, Ok(2.0));
    }

    #[test]
    fn test_evaluate_infix_simple() {
        let tokens = vec![Token::Number(2.0), Token::Plus, Token::Number(2.0)];
        let (ast, _) = parse(&tokens).unwrap();
        let result = evaluate_infix(&ast);
        assert_eq!(result, Ok(4.0));
    }

    #[test]
    fn test_evaluate_postfix_simple() {
        let tokens = vec![Token::Number(2.0), Token::Number(2.0), Token::Plus];
        let result = evaluate_postfix(&tokens);
        assert_eq!(result, Ok(4.0));
    }
}
