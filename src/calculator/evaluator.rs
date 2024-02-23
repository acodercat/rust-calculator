use crate::calculator::ast::{AST, Operator, Function};
use crate::calculator::calculator::CalculatorError;
use crate::calculator::token::Token;
use crate::calculator::parser::parse;

/// Extracts coefficients and constants from an AST representing a linear expression.
///
/// This function traverses an AST to identify and separate the coefficients (associated with variables)
/// and constants in a linear expression. It handles basic arithmetic operations: addition, subtraction,
/// multiplication, and division, under the assumption that the expression is linear (i.e., each variable
/// appears with a degree of 1). Non-linear operations or variables with degrees other than 1 result in an error.
///
/// # Arguments
///
/// * `ast` - A reference to the AST node representing the part of the expression being analyzed.
///
/// # Returns
///
/// * `Ok((f64, f64))` where the first `f64` is the coefficient and the second `f64` is the constant part of the expression.
/// * `Err(CalculatorError::InvalidExpression)` for non-linear expressions or other invalid structures.
/// * `Err(CalculatorError::DivisionByZero)` if a division by zero is encountered.
/// * `Err(CalculatorError::UnexpectedToken)` for unexpected AST nodes.
///
/// # Examples
///
/// Extracting from a simple number:
///
/// ```
/// let ast = AST::Num(5.0);
/// assert_eq!(extract_coefficients_and_constants(&ast), Ok((0.0, 5.0)));
/// ```
///
/// Extracting from a variable:
///
/// ```
/// let ast = AST::Var("x".to_string());
/// assert_eq!(extract_coefficients_and_constants(&ast), Ok((1.0, 0.0)));
/// ```
///
/// Extracting from a linear binomial:
///
/// ```
/// let ast = AST::BinOp(Box::new(AST::Var("x".to_string())), Operator::Add, Box::new(AST::Num(2.0)));
/// assert_eq!(extract_coefficients_and_constants(&ast), Ok((1.0, 2.0)));
/// ```
///
/// Handling non-linear terms:
///
/// ```
/// let ast = AST::BinOp(Box::new(AST::Var("x".to_string())), Operator::Mul, Box::new(AST::Var("x".to_string())));
/// assert_eq!(extract_coefficients_and_constants(&ast), Err(CalculatorError::InvalidExpression));
/// ```
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

/// Solves a simple linear equation represented by a sequence of tokens.
///
/// This function identifies the position of the equality token (`=`) to separate the tokens
/// into left-hand side (LHS) and right-hand side (RHS) expressions. It then parses both sides
/// into abstract syntax trees (ASTs) and extracts coefficients and constants. Using these values,
/// the function solves the equation for the variable (assumed to be 'x' or 'y') by rearranging the
/// equation into the form `ax + b = 0` and solving for `x`.
///
/// # Arguments
///
/// * `tokens` - A slice of tokens representing the linear equation to be solved.
///
/// # Returns
///
/// * `Ok(f64)` with the solution to the equation.
/// * `Err(CalculatorError::ParseError)` if the equation does not contain an equality token.
/// * `Err(CalculatorError::InvalidExpression)` if the equation is not linear or solvable.
///
/// # Examples
///
/// Solving a simple equation:
///
/// ```
/// let tokens = vec![Token::Number(2.0), Token::Multiply, Token::Variable("x".to_string()), Token::Plus, Token::Number(1.0), Token::Equal, Token::Number(3.0)];
/// assert_eq!(solve_equation(&tokens), Ok(1.0)); // Solves 2x + 1 = 3 for x
/// ```
///
/// Handling an unsolvable equation:
///
/// ```
/// let tokens = vec![Token::Number(2.0), Token::Multiply, Token::Variable("x".to_string()), Token::Equal, Token::Number(2.0), Token::Multiply, Token::Variable("x".to_string())];
/// assert_eq!(solve_equation(&tokens), Err(CalculatorError::InvalidExpression)); // 2x = 2x has no solution
/// ```
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

/// Evaluates an infix expression represented as an Abstract Syntax Tree (AST).
///
/// This recursive function traverses the AST, evaluating numerical operations, function calls, and constant values.
/// It supports basic arithmetic operations (addition, subtraction, multiplication, division), as well as common
/// mathematical functions (logarithms, trigonometric functions) and known constants (e.g., Pi, Euler's number).
/// Variables within the expression lead to an `InvalidExpression` error, as this function does not support variable substitution.
///
/// # Arguments
///
/// * `ast` - A reference to the AST node representing the current part of the expression being evaluated.
///
/// # Returns
///
/// * `Ok(f64)` with the evaluated result of the expression.
/// * `Err(CalculatorError::DivisionByZero)` if a division by zero is attempted.
/// * `Err(CalculatorError::InvalidExpression)` if the expression contains variables or is otherwise invalid.
///
/// # Examples
///
/// Evaluating a simple arithmetic expression:
///
/// ```
/// let ast = AST::BinOp(Box::new(AST::Num(2.0)), Operator::Add, Box::new(AST::Num(3.0)));
/// assert_eq!(evaluate_infix(&ast), Ok(5.0)); // Evaluates 2 + 3
/// ```
///
/// Evaluating a function call:
///
/// ```
/// let ast = AST::Func(Function::Sin, Box::new(AST::Const(std::f64::consts::PI)));
/// assert!(evaluate_infix(&ast).unwrap().abs() < 1e-10); // Evaluates sin(PI), which is approximately 0
/// ```
///
/// Handling division by zero:
///
/// ```
/// let ast = AST::BinOp(Box::new(AST::Num(1.0)), Operator::Div, Box::new(AST::Num(0.0)));
/// assert_eq!(evaluate_infix(&ast), Err(CalculatorError::DivisionByZero)); // Attempts division by zero
/// ```
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

/// Evaluates a postfix (Reverse Polish Notation) expression represented as a sequence of tokens.
///
/// This function iterates through the tokens, pushing numbers onto a stack and applying operators to the
/// top two numbers on the stack when encountered. It supports basic arithmetic operations: addition, subtraction,
/// multiplication, and division. The function ensures that division by zero and invalid expressions (e.g., insufficient
/// operands for an operator) are appropriately handled by returning errors.
///
/// # Arguments
///
/// * `tokens` - A slice of tokens representing the postfix expression to be evaluated.
///
/// # Returns
///
/// * `Ok(f64)` with the evaluated result of the expression.
/// * `Err(CalculatorError::DivisionByZero)` if a division by zero is attempted.
/// * `Err(CalculatorError::InvalidExpression)` if the expression is malformed or cannot be evaluated.
/// * `Err(CalculatorError::UnexpectedToken)` if an unexpected token is encountered during evaluation.
///
/// # Examples
///
/// Evaluating a simple postfix expression:
///
/// ```
/// let tokens = vec![Token::Number(2.0), Token::Number(3.0), Token::Plus];
/// assert_eq!(evaluate_postfix(&tokens), Ok(5.0)); // Evaluates to 2 + 3
/// ```
///
/// Handling division by zero:
///
/// ```
/// let tokens = vec![Token::Number(1.0), Token::Number(0.0), Token::Divide];
/// assert_eq!(evaluate_postfix(&tokens), Err(CalculatorError::DivisionByZero)); // Attempts 1 / 0
/// ```
///
/// Handling an invalid expression:
///
/// ```
/// let tokens = vec![Token::Number(1.0), Token::Plus]; // Not enough operands for '+'
/// assert_eq!(evaluate_postfix(&tokens), Err(CalculatorError::InvalidExpression));
/// ```
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
    use crate::calculator::lexer::lex;
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

    #[test]
    fn extract_from_number() {
        let ast = AST::Num(5.0);
        assert_eq!(extract_coefficients_and_constants(&ast), Ok((0.0, 5.0)));
    }

    #[test]
    fn extract_from_variable() {
        let ast = AST::Var("x".to_string());
        assert_eq!(extract_coefficients_and_constants(&ast), Ok((1.0, 0.0)));
    }

    #[test]
    fn extract_from_linear_binomial() {
        let ast = AST::BinOp(Box::new(AST::Var("x".to_string())), Operator::Add, Box::new(AST::Num(2.0)));
        assert_eq!(extract_coefficients_and_constants(&ast), Ok((1.0, 2.0)));
    }

    #[test]
    fn handling_non_linear_terms() {
        let ast = AST::BinOp(Box::new(AST::Var("x".to_string())), Operator::Mul, Box::new(AST::Var("x".to_string())));
        assert_eq!(extract_coefficients_and_constants(&ast), Err(CalculatorError::InvalidExpression));
    }

    // Tests for solve_equation
    #[test]
    fn solve_simple_equation() {
        let tokens = vec![Token::Number(2.0), Token::Multiply, Token::Variable("x".to_string()), Token::Plus, Token::Number(1.0), Token::Equal, Token::Number(3.0)];
        assert_eq!(solve_equation(&tokens), Ok(1.0));
    }

    #[test]
    fn unsolvable_equation() {
        let tokens = vec![Token::Number(2.0), Token::Multiply, Token::Variable("x".to_string()), Token::Equal, Token::Number(2.0), Token::Multiply, Token::Variable("x".to_string())];
        assert_eq!(solve_equation(&tokens), Err(CalculatorError::InvalidExpression));
    }

    // Tests for evaluate_infix
    #[test]
    fn evaluate_simple_arithmetic_expression() {
        let ast = AST::BinOp(Box::new(AST::Num(2.0)), Operator::Add, Box::new(AST::Num(3.0)));
        assert_eq!(evaluate_infix(&ast), Ok(5.0));
    }

    #[test]
    fn evaluate_function_call() {
        let ast = AST::Func(Function::Sin, Box::new(AST::Const(std::f64::consts::PI)));
        assert!(evaluate_infix(&ast).unwrap().abs() < 1e-10);
    }

    #[test]
    fn division_by_zero_in_infix() {
        let ast = AST::BinOp(Box::new(AST::Num(1.0)), Operator::Div, Box::new(AST::Num(0.0)));
        assert_eq!(evaluate_infix(&ast), Err(CalculatorError::DivisionByZero));
    }

    // Tests for evaluate_postfix
    #[test]
    fn evaluate_simple_postfix_expression() {
        let tokens = vec![Token::Number(2.0), Token::Number(3.0), Token::Plus];
        assert_eq!(evaluate_postfix(&tokens), Ok(5.0));
    }

    #[test]
    fn division_by_zero_in_postfix() {
        let tokens = vec![Token::Number(1.0), Token::Number(0.0), Token::Divide];
        assert_eq!(evaluate_postfix(&tokens), Err(CalculatorError::DivisionByZero));
    }

    #[test]
    fn invalid_postfix_expression() {
        let tokens = vec![Token::Number(1.0), Token::Plus]; // Not enough operands for '+'
        assert_eq!(evaluate_postfix(&tokens), Err(CalculatorError::InvalidExpression));
    }

    #[test]
    fn test_extract_coefficients_and_constants() {
        let test_cases = vec![
            ("x", 1.0, 0.0),
            ("-x", -1.0, 0.0),
            ("2*x", 2.0, 0.0),
            ("2*x + 3", 2.0, 3.0),
            ("-3*x - 5", -3.0, -5.0),
            ("3 + 2*x", 2.0, 3.0),
            ("4 - x", -1.0, 4.0),
            ("5 - 2*x", -2.0, 5.0),
            ("2*(x + 3)", 2.0, 6.0),
            ("3*(2 - x)", -3.0, 6.0),
            ("4 + 2*(x - 1)", 2.0, 2.0),
            ("5 - 3*(1 - x)", 3.0, 2.0),
            ("(x + 2) + (3 - x)", 0.0, 5.0),
            ("2*(x + 1) - 3*(x - 1)", -1.0, 5.0),
            ("(3*x + 4) - (x - 2)", 2.0, 6.0),
            ("7", 0.0, 7.0),
            ("-4", 0.0, -4.0),
            ("-5*x", -5.0, 0.0),
            ("x + 10", 1.0, 10.0),
            ("3*x - 2", 3.0, -2.0),
            ("4 + (x - 1)", 1.0, 3.0),
            ("(2 - x) + 5", -1.0, 7.0),
            ("-(2*x - 3)", -2.0, 3.0),
            ("2*(x + 3) - 4*x", -2.0, 6.0),
            ("0.5*x - 1.5", 0.5, -1.5),
            ("x - 8", 1.0, -8.0),
            ("9 + x", 1.0, 9.0),
            ("-x - 7", -1.0, -7.0),
            ("x + x + x", 3.0, 0.0),
            ("5 - 2 + x", 1.0, 3.0),
            ("(x + 4) + (3 - x)", 0.0, 7.0),
            ("1/2*x + 3", 0.5, 3.0),
            ("-3*x + 5 - 2*x", -5.0, 5.0),
            ("2*(x + 1) + 3*x", 5.0, 2.0),
            ("2*(3 + x) - (x - 1) + 4", 1.0, 11.0),
        ];

        for (input, expected_coefficient, expected_constant) in test_cases {
            let tokens = lex(input);
            let (ast, _) = parse(&tokens).unwrap();
            let result = extract_coefficients_and_constants(&ast);

            assert!(result.is_ok(), "Failed to extract coefficients and constants for input: {}", input);
            let (coefficient, constant) = result.unwrap();

            assert_eq!(coefficient, expected_coefficient, "Coefficient mismatch for input: {}", input);
            assert_eq!(constant, expected_constant, "Constant mismatch for input: {}", input);
        }
    }
}
