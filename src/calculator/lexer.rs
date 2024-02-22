use crate::calculator::token::Token;

pub(crate) fn lex(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&c) = chars.peek() {
        if c >= '0' && c <= '9' || c == '.' {
            let mut number = String::new();
            while let Some(&c) = chars.peek() {
                if c >= '0' && c <= '9' || c == '.' {
                    number.push(c);
                    chars.next();
                } else {
                    break;
                }
            }
            let number: f64 = number.parse().unwrap();
            tokens.push(Token::Number(number));
        } else {
            match c {
                '+' => {
                    chars.next();
                    tokens.push(Token::Plus);
                },
                '-' => {
                    chars.next();
                    tokens.push(Token::Minus);
                },
                '*' => {
                    chars.next();
                    tokens.push(Token::Multiply);
                },
                '/' => {
                    chars.next();
                    tokens.push(Token::Divide);
                },
                '(' => {
                    chars.next();
                    tokens.push(Token::LeftParenthesis);
                },
                'x' | 'y' => {
                    let var_name = c.to_string();
                    chars.next();
                    tokens.push(Token::Variable(var_name));
                },
                '=' => {
                    chars.next();
                    tokens.push(Token::Equal);
                },
                ')' => {
                    chars.next();
                    tokens.push(Token::RightParenthesis);
                },
                _ => {
                    chars.next(); // Ignore other characters
                },
            }
        }
    }

    tokens
}