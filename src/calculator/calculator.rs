use crate::calculator::lexer::lex;
use crate::calculator::{evaluator::{evaluate_infix, evaluate_postfix}};
use crate::calculator::token::Token;
use crate::calculator::evaluator::solve_equation;
use crate::calculator::parser::parse;

#[derive(Debug, PartialEq)]
pub enum CalculatorError {
    DivisionByZero,
    ParseError,
    UnexpectedToken,
    InvalidExpression,
    MultipleVariables,
    EmptyExpression,
    ExtraTokensDetected,
    UnmatchedRightParenthesis,
    UnmatchedLeftParenthesis,
}
pub fn process_expression(input: &str) -> Result<String, CalculatorError> {
    let tokens = lex(input);
    if tokens.is_empty() {
        return Err(CalculatorError::EmptyExpression);
    }
    let contains_equal = tokens.iter().any(|t| *t == Token::Equal);
    let mut seen_variable = None;

    for token in &tokens {
        if let Token::Variable(name) = token {
            match seen_variable {
                None => seen_variable = Some(name.clone()),
                Some(ref seen_name) if seen_name != name => {
                    return Err(CalculatorError::MultipleVariables);
                },
                _ => {}
            }
        }
    }

    match seen_variable {
        Some(variable_name) if contains_equal => {
            let solution = solve_equation(&tokens)?;
            Ok(format!("{}={}", variable_name, solution))
        },
        _ => {
            if is_postfix_expression(&tokens) {
                let result = evaluate_postfix(&tokens)?;
                Ok(round_result(result).to_string())
            } else {
                let (ast, _) = parse(&tokens)?;
                let result = evaluate_infix(&ast)?;
                Ok(round_result(result).to_string())
            }
        }
    }
}

fn round_result(result: f64) -> f64 {
    (result * 100000000.0).round() / 100000000.0
}

fn is_postfix_expression(tokens: &[Token]) -> bool {

    let contains_parentheses_or_equal = tokens.iter().any(|t| matches!(t, Token::LeftParenthesis | Token::RightParenthesis | Token::Equal));
    if contains_parentheses_or_equal {
        return false;
    }

    let mut last_was_number = false;
    let mut number_count = 0;
    let mut operator_count = 0;

    for token in tokens {
        match token {
            Token::Number(_) => {
                number_count += 1;
                last_was_number = true;
            },
            Token::Plus | Token::Minus | Token::Multiply | Token::Divide => {
                operator_count += 1;
                if last_was_number && number_count - operator_count == 1 {
                    return true;
                }
                last_was_number = false;
            },
            _ => last_was_number = false,
        }
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn evaluate_simple_expression() {
        let input = "(3+(4-1))*5";
        let result = process_expression(input);
        assert_eq!(result, Ok("30".to_string()));
    }

    #[test]
    fn solve_linear_equation() {
        let input = "2 * x + 0.5 = 1";
        let result = process_expression(input);
        assert_eq!(result, Ok("x=0.25".to_string()));
    }

    #[test]
    fn solve_equation_with_variables_on_both_sides() {
        let input = "2 * x + 1 = 2 * (1 - x)";
        let result = process_expression(input);
        assert_eq!(result, Ok("x=0.25".to_string()));
    }

    #[test]
    fn test_log_base_10_of_10() {
        let input = "log(10)";
        assert_eq!(process_expression(input), Ok("1".to_string()));

        let input = "log10";
        assert_eq!(process_expression(input), Ok("1".to_string()));
    }

    #[test]
    fn test_log_base_100_of_10() {
        let input = "log100(10)";
        assert_eq!(process_expression(input), Ok("0.5".to_string()));
    }

    #[test]
    fn test_sin_of_pi() {
        let input = "sin(pi)";
        assert_eq!(process_expression(input), Ok("0".to_string()));

        let input = "sinpi";
        assert_eq!(process_expression(input), Ok("0".to_string()));
    }

    #[test]
    fn test_sin_of_1_5_pi() {
        let input = "sin(1.5pi)";
        assert_eq!(process_expression(input), Ok("-1".to_string()));

        let input = "sin(1.5*pi)";
        assert_eq!(process_expression(input), Ok("-1".to_string()));
    }

}
