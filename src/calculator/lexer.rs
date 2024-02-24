use crate::calculator::calculator::CalculatorError;
use crate::calculator::token::Token;

/// Performs lexical analysis on a mathematical expression, converting it into a sequence of tokens.
///
/// This function sequentially processes each character of the input string, categorizing them into tokens
/// representing numbers, arithmetic operators, parentheses, and identifiers for variables, functions, or constants.
///
/// # Arguments
///
/// * `input` - A string slice containing the mathematical expression to be tokenized.
///
/// # Returns
///
/// * `Ok(Vec<Token>)` - A vector containing the tokens derived from the input expression, representing the
///   structured components such as numeric values, operators, and parentheses.
/// * `Err(CalculatorError)` - An error if the lexing process encounters an unrecognized pattern or invalid syntax.
///
/// # Examples
///
/// Tokenizing basic arithmetic operations:
///
/// ```
/// let tokens = lex("3 + 4.5").expect("Failed to lex expression");
/// assert_eq!(tokens, vec![Token::Number(3.0), Token::Plus, Token::Number(4.5)]);
/// ```
///
/// Tokenizing expressions involving constants and functions:
///
/// ```
/// let tokens = lex("sin(pi) + ln(e)").expect("Failed to lex expression");
/// assert_eq!(tokens, vec![
///     Token::Sin, Token::LeftParenthesis, Token::Pi, Token::RightParenthesis,
///     Token::Plus,
///     Token::Ln, Token::LeftParenthesis, Token::E, Token::RightParenthesis
/// ]);
/// ```
///
/// The lexer is designed to accurately parse and tokenize each character, skipping over any unrecognized characters,
/// and properly assembling tokens to reflect the intended mathematical operations and expressions within the given context.
pub(crate) fn lex(input: &str) -> Result<Vec<Token>, CalculatorError> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&c) = chars.peek() {
        match c {
            '0'..='9' | '.' => {
                // Parse the number
                let number = parse_number(&mut chars);
                // Handle any constants that might immediately follow the number
                handle_number_constant_combination(&mut chars, &mut tokens, number);
            },
            '+' => { chars.next(); tokens.push(Token::Plus); },
            '-' => { chars.next(); tokens.push(Token::Minus); },
            '*' => { chars.next(); tokens.push(Token::Multiply); },
            '/' => { chars.next(); tokens.push(Token::Divide); },
            '(' => { chars.next(); tokens.push(Token::LeftParenthesis); },
            ')' => { chars.next(); tokens.push(Token::RightParenthesis); },
            '=' => { chars.next(); tokens.push(Token::Equal); },
            _ if c.is_alphabetic() => {
                // Parse and add tokens for constants, functions, or variables
                let name = parse_identifier(&mut chars);
                if ["pi", "e"].contains(&name.as_str()) {
                    handle_constant(&name, &mut tokens)?;
                } else if name.ends_with("pi") || name.ends_with("e") {
                    handle_function_with_constant(&name, &mut tokens)?;
                } else {
                    handle_function(&name, &mut chars, &mut tokens);
                }
            },
            _ => {
                return Err(CalculatorError::UnexpectedToken);
            }
        }
    }

    Ok(tokens)
}

/// Parses a sequence of characters into a floating-point number.
///
/// Iterates over a peekable iterator of characters, collecting digits and decimal points to form a number.
///
/// # Arguments
///
/// * `chars` - A peekable iterator over the characters of the input string.
///
/// # Returns
///
/// The parsed floating-point number.
///
/// # Examples
///
/// ```
/// let mut input = "123.456".chars().peekable();
/// assert_eq!(parse_number(&mut input), 123.456);
///
/// let mut input = "3.14pi".chars().peekable();
/// assert_eq!(parse_number(&mut input), 3.14); // Stops parsing at 'pi'
/// ```
fn parse_number(chars: &mut std::iter::Peekable<std::str::Chars>) -> f64 {
    let mut number = String::new();
    while let Some(&c) = chars.peek() {
        if c.is_digit(10) || c == '.' {
            number.push(c);
            chars.next();
        } else {
            break;
        }
    }
    number.parse().unwrap() // Assume valid number for simplicity
}

/// Parses logarithmic expressions, including those with a specified base, and adds the corresponding tokens.
///
/// This function checks for a digit or '.' immediately following the log keyword to determine if a base is present.
/// If there's no base specified, it simply adds a `Log` token, assuming the common logarithm (base 10).
///
/// # Arguments
///
/// * `chars` - A peekable iterator over the characters of the input string, positioned at the character immediately following 'log'.
/// * `tokens` - A mutable reference to the vector of tokens where the parsed tokens will be pushed.
///
/// # Examples
///
/// Parsing a log with an explicit base and parentheses:
///
/// ```
/// let mut chars = "100(10)".chars().peekable();
/// let mut tokens = Vec::new();
/// parse_log(&mut chars, &mut tokens);
/// // This will result in tokens for LogBase(10) with base 100
/// assert_eq!(tokens, vec![Token::LogBase(100.0)]);
/// ```
///
/// Parsing a log with an implicit base:
///
/// ```
/// let mut chars = "".chars().peekable(); // Assuming we've already consumed 'log'
/// let mut tokens = Vec::new();
/// parse_log(&mut chars, &mut tokens);
/// // This will result in a single Log token, assuming base 10
/// assert_eq!(tokens, vec![Token::Log]);
/// ```
fn parse_log(chars: &mut std::iter::Peekable<std::str::Chars>, tokens: &mut Vec<Token>) {
    // Directly check for a digit or '.' to determine if a base is present.
    if chars.peek().map_or(false, |c| c.is_digit(10) || *c == '.') {
        let base = parse_number(chars);

        // After parsing the base, check if the next character is an opening parenthesis.
        match chars.peek() {
            Some(&'(') => {
                // If there's an opening parenthesis, use the LogBase token.
                tokens.push(Token::LogBase(base));
            },
            _ => {
                // If there's no opening parenthesis, treat it as Log(base) with implicit parentheses.
                tokens.push(Token::Log);
                tokens.push(Token::LeftParenthesis);
                tokens.push(Token::Number(base));
                tokens.push(Token::RightParenthesis);
            }
        }
    } else {
        // If there's no base, just push the Log token.
        tokens.push(Token::Log);
    }
}

/// Parses a sequence of alphabetic characters from a peekable character iterator into a string.
///
/// This function accumulates consecutive alphabetic characters into a single string identifier,
/// stopping when it encounters a non-alphabetic character. It's typically used to parse variable
/// names, function names, or other alphabetic tokens within an expression.
///
/// # Arguments
///
/// * `chars` - A peekable iterator over the characters of the input string.
///
/// # Returns
///
/// The parsed identifier as a string.
///
/// # Examples
///
/// Parsing a simple identifier:
///
/// ```
/// let mut chars = "variable123".chars().peekable();
/// let identifier = parse_identifier(&mut chars);
/// assert_eq!(identifier, "variable");
/// // Note: Parsing stops at '1' since it's not an alphabetic character.
/// ```
fn parse_identifier(chars: &mut std::iter::Peekable<std::str::Chars>) -> String {
    let mut name = String::new();
    // Collect consecutive alphabetic characters
    while let Some(&c) = chars.peek() {
        if c.is_alphabetic() {
            name.push(c);
            chars.next();
        } else {
            break; // Stop at the first non-alphabetic character
        }
    }
    name
}

/// Processes mathematical constant identifiers, adding corresponding tokens to a vector.
///
/// This function examines the given identifier name to determine if it matches recognized
/// mathematical constants ('pi' or 'e'). If a match is found, the appropriate token is added
/// to the provided tokens vector.
///
/// # Arguments
///
/// * `name` - A string slice representing the identifier to be checked for constant values.
/// * `tokens` - A mutable reference to a vector of tokens where the recognized constant token will be appended.
///
/// # Returns
///
/// * `Ok(())` if a recognized constant is processed successfully.
/// * `Err(CalculatorError::UnexpectedToken)` if the identifier does not match any recognized constants.
///
/// # Examples
///
/// Adding a token for the recognized 'pi' constant:
///
/// ```
/// let mut tokens = Vec::new();
/// assert!(handle_constant("pi", &mut tokens).is_ok());
/// assert_eq!(tokens, vec![Token::Pi]); // 'pi' token successfully added
/// ```
///
/// Attempting to process an unrecognized identifier:
///
/// ```
/// let mut tokens = Vec::new();
/// assert!(handle_constant("unknown", &mut tokens).is_err()); // 'unknown' is not a recognized constant
/// assert!(tokens.is_empty()); // No tokens added for unrecognized identifiers
/// ```
fn handle_constant(name: &str, tokens: &mut Vec<Token>) -> Result<(), CalculatorError> {
    match name {
        "pi" => {
            tokens.push(Token::Pi);
            Ok(())
        },
        "e" => {
            tokens.push(Token::E);
            Ok(())
        },
        _ => Err(CalculatorError::UnexpectedToken),
    }
}

/// Processes function names and adds the corresponding tokens to the tokens vector.
///
/// Identifies common mathematical functions (log, sin, cos, tan, ctan, ln) by their names
/// and adds the appropriate token to the tokens vector. If the name doesn't match any known
/// function, it's treated as a variable name and a `Variable` token is added instead.
///
/// # Arguments
///
/// * `name` - The function or variable name as a string slice.
/// * `chars` - A peekable iterator over the characters following the function name, used for further parsing if needed.
/// * `tokens` - A mutable reference to the vector of tokens where the function or variable token will be pushed.
///
/// # Examples
///
/// Handling a recognized function 'sin':
///
/// ```
/// let mut chars = "(".chars().peekable(); // Example input following 'sin'
/// let mut tokens = Vec::new();
/// handle_function("sin", &mut chars, &mut tokens);
/// assert_eq!(tokens, vec![Token::Sin]); // 'sin' function token is added
/// ```
///
/// Handling an unrecognized name as a variable:
///
/// ```
/// let mut chars = "(".chars().peekable(); // Example input following the identifier
/// let mut tokens = Vec::new();
/// handle_function("unknownFunction", &mut chars, &mut tokens);
/// assert_eq!(tokens, vec![Token::Variable("unknownFunction".to_string())]); // Treated as a variable
/// ```
fn handle_function(name: &str, chars: &mut std::iter::Peekable<std::str::Chars>, tokens: &mut Vec<Token>) {
    match name {
        "log" => parse_log(chars, tokens),
        "sin" => tokens.push(Token::Sin),
        "cos" => tokens.push(Token::Cos),
        "tan" => tokens.push(Token::Tan),
        "ctan" => tokens.push(Token::Ctan),
        "ln" => tokens.push(Token::Ln),
        _ => tokens.push(Token::Variable(name.to_string())),
    }
}

/// Parses and tokenizes expressions that combine functions with constants, such as "sinpi".
///
/// This function separates the input string into function and constant segments based on standard naming conventions
/// (e.g., 'sin' for sine function and 'pi' for π). It then generates tokens for the identified function and constant,
/// appropriately structuring them in the tokens vector with the constant wrapped in parentheses.
///
/// # Arguments
///
/// * `name` - A string slice representing the concatenated function name and constant (e.g., "sinpi").
/// * `tokens` - A mutable reference to a vector of tokens where the new tokens will be appended.
///
/// # Returns
///
/// * `Ok(())` when tokens are successfully added for recognized function-constant combinations.
/// * `Err(CalculatorError::UnexpectedToken)` if the function name is unrecognized.
///
/// # Examples
///
/// Successful parsing of a recognized function-constant combination "sinpi":
///
/// ```
/// let mut tokens = Vec::new();
/// assert!(handle_function_with_constant("sinpi", &mut tokens).is_ok());
/// assert_eq!(tokens, vec![Token::Sin, Token::LeftParenthesis, Token::Pi, Token::RightParenthesis]);
/// ```
///
/// Handling unrecognized function names combined with a constant:
///
/// ```
/// let mut tokens = Vec::new();
/// assert!(handle_function_with_constant("unknownpi", &mut tokens).is_err());
/// assert!(tokens.is_empty()); // No tokens added due to unrecognized function name
/// ```
fn handle_function_with_constant(name: &str, tokens: &mut Vec<Token>) -> Result<(), CalculatorError> {
    let (func, const_part) = name.split_at(name.len() - 2);

    match func {
        "sin" => tokens.push(Token::Sin),
        "cos" => tokens.push(Token::Cos),
        "tan" => tokens.push(Token::Tan),
        "ctan" => tokens.push(Token::Ctan),
        _ => return Err(CalculatorError::UnexpectedToken),
    }

    tokens.push(Token::LeftParenthesis);

    handle_constant(const_part, tokens)?;

    tokens.push(Token::RightParenthesis);

    Ok(())
}

/// Handles cases where a number is immediately followed by a constant ('pi' or 'e'), adding necessary tokens.
///
/// After adding a token for the parsed number, this function checks the subsequent characters to see if they
/// form a recognized constant. If so, it inserts a multiplication token followed by the constant token, effectively
/// handling expressions like '2pi' or '3e' as '2*pi' or '3*e'.
///
/// # Arguments
///
/// * `chars` - A peekable iterator over the characters following the number.
/// * `tokens` - A mutable reference to the vector of tokens where the new tokens will be pushed.
/// * `number` - The numeric value preceding the potential constant.
///
/// # Examples
///
/// Handling a number followed by 'pi':
///
/// ```
/// let mut chars = "pi+".chars().peekable(); // Example input following the number
/// let mut tokens = Vec::new();
/// handle_number_constant_combination(&mut chars, &mut tokens, 2.0);
/// // Results in tokens for '2', '*', and 'pi'
/// assert_eq!(tokens, vec![Token::Number(2.0), Token::Multiply, Token::Pi]);
/// ```
///
/// Ignoring characters that do not form a recognized constant:
///
/// ```
/// let mut chars = "x+".chars().peekable(); // Example input following the number
/// let mut tokens = Vec::new();
/// handle_number_constant_combination(&mut chars, &mut tokens, 3.0);
/// // Results in a single token for '3', as 'x' is not a recognized constant following a number
/// assert_eq!(tokens, vec![Token::Number(3.0)]);
/// ```
fn handle_number_constant_combination(chars: &mut std::iter::Peekable<std::str::Chars>, tokens: &mut Vec<Token>, number: f64) {
    // Push the parsed number token
    tokens.push(Token::Number(number));

    // Check what character immediately follows the number
    match chars.peek() {
        Some('p') => {
            // Attempt to consume 'pi'
            chars.next(); // Skip 'p'
            if chars.peek() == Some(&'i') {
                chars.next(); // Skip 'i'
                // Add multiplication and Pi tokens
                tokens.push(Token::Multiply);
                tokens.push(Token::Pi);
            }
        },
        Some('e') => {
            chars.next(); // Skip 'e'
            // Add multiplication and E tokens
            tokens.push(Token::Multiply);
            tokens.push(Token::E);
        },
        _ => (), // Do nothing if the following character is not 'pi' or 'e'
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_basic_arithmetic() {
        let tokens = lex("3 + 4.5").expect("Failed to lex basic arithmetic");
        assert_eq!(tokens, vec![Token::Number(3.0), Token::Plus, Token::Number(4.5)]);
    }

    #[test]
    fn test_lex_constants_and_functions() {
        let tokens = lex("sin(pi) + ln(e)").expect("Failed to lex constants and functions");
        assert_eq!(tokens, vec![Token::Sin, Token::LeftParenthesis, Token::Pi, Token::RightParenthesis, Token::Plus, Token::Ln, Token::LeftParenthesis, Token::E, Token::RightParenthesis]);
    }

    #[test]
    fn test_parse_number() {
        let mut input = "123.456".chars().peekable();
        assert_eq!(parse_number(&mut input), 123.456);

        let mut input = "3.14pi".chars().peekable();
        assert_eq!(parse_number(&mut input), 3.14); // Stops parsing at 'pi'
    }

    #[test]
    fn test_parse_log_with_explicit_base() {
        let mut chars = "100(10)".chars().peekable();
        let mut tokens = Vec::new();
        parse_log(&mut chars, &mut tokens);
        println!("tokens{:?}", tokens);
        assert_eq!(tokens, vec![Token::LogBase(100.0)]);
    }

    #[test]
    fn test_parse_log_with_implicit_base() {
        let mut chars = "".chars().peekable();
        let mut tokens = Vec::new();
        parse_log(&mut chars, &mut tokens);
        assert_eq!(tokens, vec![Token::Log]);
    }

    #[test]
    fn test_handle_constant_pi() {
        let mut tokens = Vec::new();
        handle_constant("pi", &mut tokens).expect("Failed to handle constant");
        assert_eq!(tokens, vec![Token::Pi]);
    }

    #[test]
    fn test_handle_function_sin() {
        let mut chars = "(".chars().peekable();
        let mut tokens = Vec::new();
        handle_function("sin", &mut chars, &mut tokens);
        assert_eq!(tokens, vec![Token::Sin]);
    }

    #[test]
    fn test_handle_function_with_constant_sinpi() {
        let mut tokens = Vec::new();
        handle_function_with_constant("sinpi", &mut tokens).expect("Failed to handle function with constant");
        assert_eq!(tokens, vec![Token::Sin, Token::LeftParenthesis, Token::Pi, Token::RightParenthesis]);
    }

    #[test]
    fn test_handle_number_constant_combination_with_pi() {
        let mut chars = "pi+".chars().peekable();
        let mut tokens = Vec::new();
        handle_number_constant_combination(&mut chars, &mut tokens, 2.0);
        assert_eq!(tokens, vec![Token::Number(2.0), Token::Multiply, Token::Pi]);
    }

}




