use crate::calculator::token::Token;

/// Lexically analyzes a mathematical expression and converts it into a sequence of tokens.
///
/// This function iterates through the input string character by character, grouping them into
/// meaningful tokens such as numbers, operators, parentheses, and identifiers for variables or functions.
///
/// # Arguments
///
/// * `input` - The mathematical expression as a string slice.
///
/// # Returns
///
/// A vector of `Token` representing the parsed elements of the input expression. This can include
/// numeric literals, arithmetic operators, parentheses, and identifiers for functions or variables.
///
/// # Examples
///
/// Basic arithmetic operations:
///
/// ```
/// let tokens = lex("3 + 4.5");
/// assert_eq!(tokens, vec![Token::Number(3.0), Token::Plus, Token::Number(4.5)]);
/// ```
///
/// Handling constants and functions:
///
/// ```
/// let tokens = lex("sin(pi) + ln(e)");
/// assert_eq!(tokens, vec![Token::Sin, Token::LeftParenthesis, Token::Pi, Token::RightParenthesis, Token::Plus, Token::Ln, Token::LeftParenthesis, Token::E, Token::RightParenthesis]);
/// ```
///
/// The lexer ensures that each character contributes to forming a valid token, skipping over
/// unrecognized characters, and appropriately groups characters into tokens based on the context
/// (e.g., differentiating between the minus sign and negative numbers).
pub(crate) fn lex(input: &str) -> Vec<Token> {
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
                    handle_constant(&name, &mut tokens);
                } else if name.ends_with("pi") || name.ends_with("e") {
                    handle_function_with_constant(&name, &mut tokens);
                } else {
                    handle_function(&name, &mut chars, &mut tokens);
                }
            },
            _ => { chars.next(); },
        }
    }

    tokens
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

/// Adds tokens for recognized mathematical constants to the tokens vector.
///
/// This function checks the identifier name for known constants ('pi' and 'e') and adds
/// the corresponding token to the tokens vector. Unrecognized names are ignored, assuming
/// the calling context handles them appropriately.
///
/// # Arguments
///
/// * `name` - The identifier name as a string slice, potentially representing a mathematical constant.
/// * `tokens` - A mutable reference to the vector of tokens where the constant token will be pushed if recognized.
///
/// # Examples
///
/// Handling a recognized constant 'pi':
///
/// ```
/// let mut tokens = Vec::new();
/// handle_constant("pi", &mut tokens);
/// assert_eq!(tokens, vec![Token::Pi]);
/// ```
///
/// Ignoring an unrecognized identifier:
///
/// ```
/// let mut tokens = Vec::new();
/// handle_constant("unknown", &mut tokens);
/// assert!(tokens.is_empty()); // No token is added for unrecognized identifiers
/// ```
fn handle_constant(name: &str, tokens: &mut Vec<Token>) {
    match name {
        "pi" => tokens.push(Token::Pi),
        "e" => tokens.push(Token::E),
        _ => {} // Handle error or ignore
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

/// Handles functions combined with constants (e.g., sinpi) by splitting the input into function and constant parts.
///
/// This function identifies the function part (e.g., 'sin', 'cos') and the constant part (e.g., 'pi', 'e')
/// from the concatenated function-constant name. It then adds tokens for the function, wraps the constant
/// within parentheses, and adds the corresponding constant token.
///
/// # Arguments
///
/// * `name` - The concatenated function and constant name as a string slice.
/// * `tokens` - A mutable reference to the vector of tokens where the parsed tokens will be pushed.
///
/// # Examples
///
/// Handling a function combined with a constant 'sinpi':
///
/// ```
/// let mut tokens = Vec::new();
/// handle_function_with_constant("sinpi", &mut tokens);
/// // Results in tokens for 'sin', followed by '(', 'pi', and ')'
/// assert_eq!(tokens, vec![Token::Sin, Token::LeftParenthesis, Token::Pi, Token::RightParenthesis]);
/// ```
///
/// Ignoring an unrecognized combination:
///
/// ```
/// let mut tokens = Vec::new();
/// handle_function_with_constant("unknownpi", &mut tokens);
/// // No tokens are added for unrecognized function names
/// assert!(tokens.is_empty());
/// ```
fn handle_function_with_constant(name: &str, tokens: &mut Vec<Token>) {
    let (func, const_part) = name.split_at(name.len() - 2);

    match func {
        "sin" => tokens.push(Token::Sin),
        "cos" => tokens.push(Token::Cos),
        "tan" => tokens.push(Token::Tan),
        "ctan" => tokens.push(Token::Ctan),
        _ => {}
    }

    tokens.push(Token::LeftParenthesis);

    match const_part {
        "pi" => tokens.push(Token::Pi),
        "e" => tokens.push(Token::E),
        _ => {}
    }

    tokens.push(Token::RightParenthesis);
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
        let tokens = lex("3 + 4.5");
        assert_eq!(tokens, vec![Token::Number(3.0), Token::Plus, Token::Number(4.5)]);
    }

    #[test]
    fn test_lex_constants_and_functions() {
        let tokens = lex("sin(pi) + ln(e)");
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
        handle_constant("pi", &mut tokens);
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
        handle_function_with_constant("sinpi", &mut tokens);
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




