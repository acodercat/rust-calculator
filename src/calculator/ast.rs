#[derive(Debug, PartialEq)]
pub(crate) enum AST {
    Num(f64),
    Var(String),
    BinOp(Box<AST>, Operator, Box<AST>),
    Func(Function, Box<AST>),
    Const(f64),
    LogBase(f64, Box<AST>),
}

#[derive(Debug, PartialEq)]
pub(crate) enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Function {
    Log,
    Ln,
    Sin,
    Cos,
    Tan,
    Ctan,
}