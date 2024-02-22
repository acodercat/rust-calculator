use crate::calculator::token::Token;

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

fn parse_identifier(chars: &mut std::iter::Peekable<std::str::Chars>) -> String {
    let mut name = String::new();
    while let Some(&c) = chars.peek() {
        if c.is_alphabetic() {
            name.push(c);
            chars.next();
        } else {
            break;
        }
    }
    name
}

fn handle_constant(name: &str, tokens: &mut Vec<Token>) {
    match name {
        "pi" => tokens.push(Token::Pi),
        "e" => tokens.push(Token::E),
        _ => {} // Handle error or ignore
    }
}

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

fn handle_function_with_constant(name: &str, tokens: &mut Vec<Token>) {
    let (func, const_part) = name.split_at(name.len() - 2);

    match func {
        "sin" => tokens.push(Token::Sin),
        "cos" => tokens.push(Token::Cos),
        "tan" => tokens.push(Token::Tan),
        "ctan" => tokens.push(Token::Ctan),
        _ => {} // Ignore or handle error
    }

    tokens.push(Token::LeftParenthesis);

    match const_part {
        "pi" => tokens.push(Token::Pi),
        "e" => tokens.push(Token::E),
        _ => {} // Ignore or handle error
    }

    tokens.push(Token::RightParenthesis);
}

// Encapsulates the logic for handling combinations of numbers followed by constants (e.g., 3pi)
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
    use crate::calculator::token::Token;

    #[test]
    fn test_lex_simple() {
        let input = "3 + 2";
        let expected_tokens = vec![
            Token::Number(3.0),
            Token::Plus,
            Token::Number(2.0),
        ];
        assert_eq!(lex(input), expected_tokens);
    }
}




