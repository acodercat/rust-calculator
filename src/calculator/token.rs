#[derive(Debug, PartialEq)]
pub(crate) enum Token {
    Number(f64),
    Plus,
    Minus,
    Multiply,
    Divide,
    LeftParenthesis,
    RightParenthesis,
    Variable(String),
    Equal,
    Log,
    Ln,
    Sin,
    Cos,
    Tan,
    Ctan,
    Pi,
    E,
    LogBase(f64),
}

