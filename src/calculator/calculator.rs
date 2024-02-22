use crate::calculator::lexer::lex;
use crate::calculator::{evaluator::{evaluate_infix, evaluate_postfix}, parser::parse_expression};
use crate::calculator::token::Token;
use crate::calculator::evaluator::solve_equation;

#[derive(Debug)]
pub enum CalculatorError {
    DivisionByZero,
    ParseError,
    UnexpectedToken,
    InvalidExpression,
    MissingRightParenthesis,
    MultipleVariables,
}
pub fn process_expression(input: &str) -> Result<String, CalculatorError> {
    let tokens = lex(input);
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
                Ok(result.to_string())
            } else {
                let (ast, _) = parse_expression(&tokens)?;
                let result = evaluate_infix(&ast)?;
                Ok(result.to_string())
            }
        }
    }
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