use std::collections::VecDeque;
use crate::calculator::token::Token;
use crate::calculator::ast::{AST, Operator, Function};
use crate::calculator::calculator::CalculatorError;

/// Checks for balanced parentheses in a sequence of tokens.
///
/// This function iterates through the provided tokens, using a stack to keep track of
/// left parentheses. When a right parenthesis is encountered, it attempts to pop a left
/// parenthesis from the stack. If the stack is empty (indicating no matching left parenthesis),
/// or if there are left parentheses remaining in the stack after processing all tokens (indicating
/// unmatched left parentheses), an error is returned.
///
/// # Arguments
///
/// * `tokens` - A slice of tokens to be checked for balanced parentheses.
///
/// # Returns
///
/// * `Ok(())` if the parentheses are balanced.
/// * `Err(CalculatorError::UnmatchedRightParenthesis)` if a right parenthesis has no matching left parenthesis.
/// * `Err(CalculatorError::UnmatchedLeftParenthesis)` if one or more left parentheses have no matching right parenthesis.
///
/// # Examples
///
/// Balanced parentheses:
///
/// ```
/// let tokens = vec![Token::LeftParenthesis, Token::Number(1.0), Token::RightParenthesis];
/// assert_eq!(check_parentheses(&tokens), Ok(()));
/// ```
///
/// Unmatched right parenthesis:
///
/// ```
/// let tokens = vec![Token::RightParenthesis, Token::Number(1.0)];
/// assert_eq!(check_parentheses(&tokens), Err(CalculatorError::UnmatchedRightParenthesis));
/// ```
///
/// Unmatched left parenthesis:
///
/// ```
/// let tokens = vec![Token::LeftParenthesis, Token::Number(1.0)];
/// assert_eq!(check_parentheses(&tokens), Err(CalculatorError::UnmatchedLeftParenthesis));
/// ```
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

/// Parses a sequence of tokens into an Abstract Syntax Tree (AST).
///
/// This function first checks for balanced parentheses in the tokens. It then attempts to construct an AST.
///
/// # Arguments
///
/// * `tokens` - A slice of tokens representing the mathematical expression to be parsed.
///
/// # Returns
///
/// * `Ok((AST, &[Token]))` where `AST` is the root of the constructed abstract syntax tree and `&[Token]` is a slice
///   of the remaining tokens (expected to be empty if the expression is valid).
/// * `Err(CalculatorError::UnmatchedRightParenthesis)` if there's an unmatched right parenthesis.
/// * `Err(CalculatorError::UnmatchedLeftParenthesis)` if there's an unmatched left parenthesis.
/// * `Err(CalculatorError::ExtraTokensDetected)` if extra tokens are detected after parsing the expression.
/// * `Err(CalculatorError::UnexpectedToken)` if an unexpected token is encountered during parsing.
///
/// # Examples
///
/// Parsing a simple expression:
///
/// ```
/// let tokens = vec![Token::Number(1.0), Token::Plus, Token::Number(2.0)];
/// let (ast, rest) = parse(&tokens).unwrap();
/// assert!(rest.is_empty()); // No extra tokens should be present
/// // `ast` should represent the AST for the expression `1 + 2`
/// ```
///
/// Parsing with unexpected tokens:
///
/// ```
/// let tokens = vec![Token::Number(1.0), Token::Plus, Token::Number(2.0), Token::Multiply];
/// assert_eq!(parse(&tokens), Err(CalculatorError::UnexpectedToken));
/// ```
pub(crate) fn parse(tokens: &[Token]) -> Result<(AST, &[Token]), CalculatorError> {
    check_parentheses(tokens)?;
    let (ast, rest) = parse_expression(tokens)?;

    if !rest.is_empty() {
        return Err(CalculatorError::ExtraTokensDetected);
    }

    Ok((ast, rest))
}

/// Parses an expression from a sequence of tokens into an Abstract Syntax Tree (AST) node.
///
/// This function constructs an AST for binary operations by recursively parsing terms and combining them
/// based on the presence of addition or subtraction operators.
///
/// # Arguments
///
/// * `tokens` - A slice of tokens to be parsed into an expression.
///
/// # Returns
///
/// * `Ok((AST, &[Token]))` where `AST` is the constructed abstract syntax tree representing the parsed expression,
///   and `&[Token]` is a slice of any remaining tokens after the expression has been parsed.
/// * `Err(CalculatorError)` if an error occurs during parsing, such as an invalid token sequence.
///
/// # Examples
///
/// Parsing a simple addition:
///
/// ```
/// let tokens = vec![Token::Number(1.0), Token::Plus, Token::Number(2.0)];
/// let (ast, rest) = parse_expression(&tokens).unwrap();
/// assert!(rest.is_empty()); // Should be no extra tokens
/// assert_eq!(ast, AST::BinOp(Box::new(AST::Num(1.0)), Operator::Add, Box::new(AST::Num(2.0))));
/// ```
///
/// Parsing an addition followed by extra tokens:
///
/// ```
/// let tokens = vec![Token::Number(1.0), Token::Plus, Token::Number(2.0), Token::Multiply];
/// let (ast, rest) = parse_expression(&tokens).unwrap();
/// assert_eq!(rest, &[Token::Multiply]); // Remaining tokens after parsing the expression
/// ```
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

/// Parses a term from a sequence of tokens into an Abstract Syntax Tree (AST) node.
///
/// This function constructs an AST for multiplication and division operations within an expression. It
/// starts by parsing a single factor and then checks for any subsequent multiplication or division operators,
/// combining parsed factors into a larger term. The process repeats until no more multiplication or division
/// operators are found, resulting in the final term and any remaining tokens.
///
/// # Arguments
///
/// * `tokens` - A slice of tokens representing part of an expression to be parsed into a term.
///
/// # Returns
///
/// * `Ok((AST, &[Token]))` where `AST` is the AST node representing the parsed term, and `&[Token]` is
///   a slice of any remaining tokens after the term has been parsed.
/// * `Err(CalculatorError)` for any parsing errors encountered, such as invalid token sequences.
///
/// # Examples
///
/// Parsing a simple multiplication:
///
/// ```
/// let tokens = vec![Token::Number(2.0), Token::Multiply, Token::Number(3.0)];
/// let (ast, rest) = parse_term(&tokens).unwrap();
/// assert!(rest.is_empty()); // Expect no remaining tokens
/// assert_eq!(ast, AST::BinOp(Box::new(AST::Num(2.0)), Operator::Mul, Box::new(AST::Num(3.0))));
/// ```
///
/// Parsing a multiplication with remaining tokens:
///
/// ```
/// let tokens = vec![Token::Number(2.0), Token::Multiply, Token::Number(3.0), Token::Plus];
/// let (ast, rest) = parse_term(&tokens).unwrap();
/// assert_eq!(rest, &[Token::Plus]); // Remaining tokens after parsing the term
/// ```
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

/// Parses a factor from a sequence of tokens into an Abstract Syntax Tree (AST) node.
///
/// Factors are the basic units in an expression, including numbers, variables, and expressions
/// within parentheses. This function handles unary minus by treating it as multiplication by -1,
/// parses variables, numbers, and also handles function calls and constants like Pi and Euler's number (e).
///
/// # Arguments
///
/// * `tokens` - A slice of tokens representing part of an expression to be parsed into a factor.
///
/// # Returns
///
/// * `Ok((AST, &[Token]))` where `AST` is the AST node representing the parsed factor, and `&[Token]`
///   is a slice of any remaining tokens after the factor has been parsed.
/// * `Err(CalculatorError)` for any parsing errors encountered, such as invalid token sequences or
///   unexpected tokens.
///
/// # Examples
///
/// Parsing a simple number:
///
/// ```
/// let tokens = vec![Token::Number(2.0)];
/// let (ast, rest) = parse_factor(&tokens).unwrap();
/// assert!(rest.is_empty()); // Expect no remaining tokens
/// assert_eq!(ast, AST::Num(2.0));
/// ```
///
/// Parsing a unary minus expression:
///
/// ```
/// let tokens = vec![Token::Minus, Token::Number(3.0)];
/// let (ast, rest) = parse_factor(&tokens).unwrap();
/// // Expect an AST representing -1 * 3
/// assert_eq!(ast, AST::BinOp(Box::new(AST::Num(-1.0)), Operator::Mul, Box::new(AST::Num(3.0))));
/// assert!(rest.is_empty());
/// ```
///
/// Parsing a function call with a constant:
///
/// ```
/// let tokens = vec![Token::Sin, Token::LeftParenthesis, Token::Pi, Token::RightParenthesis];
/// let (ast, rest) = parse_factor(&tokens).unwrap();
/// // Expect an AST representing sin(pi)
/// assert_eq!(ast, AST::Func(Function::Sin, Box::new(AST::Const(std::f64::consts::PI))));
/// assert!(rest.is_empty());
/// ```
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

    #[test]
    fn parse_single_number() {
        let tokens = vec![Token::Number(42.0)];
        let (ast, _) = parse(&tokens).unwrap();
        assert_eq!(ast, AST::Num(42.0));
    }

    #[test]
    fn parse_simple_addition() {
        let tokens = vec![Token::Number(1.0), Token::Plus, Token::Number(2.0)];
        let (ast, _) = parse(&tokens).unwrap();
        match ast {
            AST::BinOp(lhs, op, rhs) => {
                assert_eq!(*lhs, AST::Num(1.0));
                assert_eq!(op, Operator::Add);
                assert_eq!(*rhs, AST::Num(2.0));
            },
            _ => panic!("Expected BinOp AST node"),
        }
    }

    #[test]
    fn parse_error_unmatched_left_parenthesis() {
        let tokens = vec![Token::LeftParenthesis, Token::Number(1.0)];
        assert!(parse(&tokens).is_err());
    }

    #[test]
    fn parse_error_unmatched_right_parenthesis() {
        let tokens = vec![Token::Number(1.0), Token::RightParenthesis];
        assert!(parse(&tokens).is_err());
    }

    #[test]
    fn parse_error_unexpected_token() {
        let tokens = vec![Token::Plus];
        assert!(parse(&tokens).is_err());
    }

    #[test]
    fn test_check_parentheses_balanced() {
        let tokens = vec![Token::LeftParenthesis, Token::Number(1.0), Token::RightParenthesis];
        assert_eq!(check_parentheses(&tokens), Ok(()));
    }

    #[test]
    fn test_check_parentheses_unmatched_right() {
        let tokens = vec![Token::RightParenthesis, Token::Number(1.0)];
        assert_eq!(check_parentheses(&tokens), Err(CalculatorError::UnmatchedRightParenthesis));
    }

    #[test]
    fn test_check_parentheses_unmatched_left() {
        let tokens = vec![Token::LeftParenthesis, Token::Number(1.0)];
        assert_eq!(check_parentheses(&tokens), Err(CalculatorError::UnmatchedLeftParenthesis));
    }

    #[test]
    fn test_parse_simple_expression() {
        let tokens = vec![Token::Number(1.0), Token::Plus, Token::Number(2.0)];
        let result = parse(&tokens).unwrap();
        // You might need a custom assert function or macro to compare AST nodes
        assert!(matches!(result.0, AST::BinOp(_, Operator::Add, _)));
        assert!(result.1.is_empty());
    }

    #[test]
    fn test_parse_expression_with_extra_tokens() {
        let tokens = vec![Token::Number(1.0), Token::Plus, Token::Number(2.0), Token::Multiply];
        assert_eq!(parse(&tokens), Err(CalculatorError::UnexpectedToken));
    }

    #[test]
    fn test_parse_term_simple_multiplication() {
        let tokens = vec![Token::Number(2.0), Token::Multiply, Token::Number(3.0)];
        let result = parse_term(&tokens).unwrap();
        assert!(matches!(result.0, AST::BinOp(_, Operator::Mul, _)));
        assert!(result.1.is_empty());
    }
}