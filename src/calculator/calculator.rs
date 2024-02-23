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

/// Processes a given mathematical expression in string format and returns the result as a string.
///
/// This function supports both infix and postfix notations, simple linear equations with a single variable,
/// and evaluates expressions involving basic arithmetic operations, trigonometric functions, and logarithms.
/// It handles known constants such as Pi and Euler's number e, and rounds the result to a fixed precision for display.
///
/// # Errors
///
/// Returns an error if the expression is empty, contains multiple variables, or if any other parsing or evaluation error occurs.
///
/// # Examples
///
/// Basic arithmetic:
///
/// ```
/// let result = process_expression("(3+(4-1))*5").unwrap();
/// assert_eq!(result, "30");
/// ```
///
/// Solving a linear equation:
///
/// ```
/// let result = process_expression("2 * x + 0.5 = 1").unwrap();
/// assert_eq!(result, "x=0.25");
/// ```
///
/// Using trigonometric functions and constants:
///
/// ```
/// let result = process_expression("sin(pi)").unwrap();
/// assert_eq!(result, "0");
/// ```
pub(crate) fn process_expression(input: &str) -> Result<String, CalculatorError> {

    // Analyze the input string to convert it into a sequence of tokens
    let tokens = lex(input);

    // Return an error if the input string does not contain any valid tokens
    if tokens.is_empty() {
        return Err(CalculatorError::EmptyExpression);
    }

    // Check if the expression contains an equals sign, indicating a possible equation
    let contains_equal = tokens.iter().any(|t| *t == Token::Equal);

    // Initialize a variable to keep track of the variable name if one is encountered
    let mut seen_variable = None;

    // Iterate over the tokens to check for variable tokens and ensure only one variable is used in the expression
    for token in &tokens {
        if let Token::Variable(name) = token {
            match seen_variable {
                // If no variable has been seen before, store the current variable name
                None => seen_variable = Some(name.clone()),
                // If a different variable name is encountered, return an error indicating multiple variables
                Some(ref seen_name) if seen_name != name => {
                    return Err(CalculatorError::MultipleVariables);
                },
                _ => {}
            }
        }
    }

    // Process the expression based on whether a variable has been seen and whether it's an equation
    match seen_variable {
        // If a variable is present and the expression is an equation, solve the equation
        Some(variable_name) if contains_equal => {
            let result = solve_equation(&tokens)?;
            // Format the result as "variable=result" and round the result for display
            Ok(format!("{}={}", variable_name, round_result(result)))
        },
        // If there's no equation or variable, evaluate the expression based on its notation
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

/// Rounds a floating-point number to a fixed precision.
///
/// # Arguments
///
/// * `result` - The floating-point number to be rounded.
///
/// # Returns
///
/// * The rounded number with up to 8 decimal places of precision.
///
/// # Examples
///
/// ```
/// let rounded = round_result(3.141592653589793);
/// assert_eq!(rounded, 3.14159265);
///
/// let rounded = round_result(2.718281828459045);
/// assert_eq!(rounded, 2.71828183);
/// ```
fn round_result(result: f64) -> f64 {
    (result * 100000000.0).round() / 100000000.0
}

/// Determines if a sequence of tokens represents a postfix (Reverse Polish Notation) expression.
///
/// # Arguments
///
/// * `tokens` - A slice of tokens representing the mathematical expression to be checked.
///
/// # Returns
///
/// * `true` if the tokens represent a valid postfix expression, `false` otherwise.
///
/// # Examples
///
/// A valid postfix expression:
///
/// ```
/// let tokens = [Token::Number(3.0), Token::Number(4.0), Token::Plus];
/// assert!(is_postfix_expression(&tokens));
/// ```
///
/// An invalid postfix expression (due to the presence of parentheses):
///
/// ```
/// let tokens = [Token::LeftParenthesis, Token::Number(3.0), Token::Number(4.0), Token::Plus, Token::RightParenthesis];
/// assert!(!is_postfix_expression(&tokens));
/// ```
///
/// # Notes
///
/// The function first checks for the presence of parentheses or an equal sign, which automatically
/// disqualifies the expression from being considered postfix. It then counts the number of numeric
/// tokens and operator tokens, ensuring that operators follow the postfix notation rule. If at any
/// point the sequence of tokens breaks the postfix rules, the function returns `false`.
fn is_postfix_expression(tokens: &[Token]) -> bool {
    // Check for disqualifying characters: parentheses or the equal sign
    let contains_parentheses_or_equal = tokens.iter().any(|t| matches!(t, Token::LeftParenthesis | Token::RightParenthesis | Token::Equal));
    if contains_parentheses_or_equal {
        return false;
    }

    let mut last_was_number = false; // Tracks if the last token was a number
    let mut number_count = 0;        // Counts the number of number tokens
    let mut operator_count = 0;      // Counts the number of operator tokens

    // Iterate through each token to count numbers and operators and to verify postfix order
    for token in tokens {
        match token {
            Token::Number(_) => {
                number_count += 1;
                last_was_number = true;
            },
            Token::Plus | Token::Minus | Token::Multiply | Token::Divide => {
                // In postfix, there should be exactly one more operand than operators at any point
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
    fn test_basic_operations() {
        assert_eq!(process_expression("1 + 1"), Ok("2".to_string()));
        assert_eq!(process_expression("2 - 1"), Ok("1".to_string()));
        assert_eq!(process_expression("2 * 3"), Ok("6".to_string()));
        assert_eq!(process_expression("8 / 4"), Ok("2".to_string()));
    }

    #[test]
    fn test_complex_expressions() {
        assert_eq!(process_expression("2 * (3 + 4)"), Ok("14".to_string()));
        assert_eq!(process_expression("(2 + 3) * (4 - 1)"), Ok("15".to_string()));
    }

    #[test]
    fn test_trigonometric_functions() {
        assert_eq!(process_expression("cos(0)"), Ok("1".to_string()));
        assert_eq!(process_expression("tan(pi/4)"), Ok("1".to_string()));
    }

    #[test]
    fn test_logarithmic_functions() {
        assert_eq!(process_expression("ln(e)"), Ok("1".to_string()));
        assert_eq!(process_expression("log(100)"), Ok("2".to_string()));
    }

    #[test]
    fn test_error_handling() {
        assert!(process_expression("2 / 0").is_err());
        assert!(process_expression("2 * (3 + 4").is_err());
        assert!(process_expression("sin(90").is_err());
    }

    #[test]
    fn test_constants_and_variables() {
        assert_eq!(process_expression("pi"), Ok("3.14159265".to_string()));
        assert_eq!(process_expression("e"), Ok("2.71828183".to_string()));
        assert_eq!(process_expression("2 * x + 1 = 3"), Ok("x=1".to_string()));
    }


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

    #[test]
    fn test_postfix_expression() {
        assert_eq!(process_expression("3 4 + 2 *"), Ok("14".to_string()));
        assert_eq!(process_expression("10 2 8 * + 3 -"), Ok("23".to_string()));
    }


}
