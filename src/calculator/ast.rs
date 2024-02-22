#[derive(Debug)]
pub(crate) enum AST {
    Num(f64),
    Var(String),
    BinOp(Box<AST>, Operator, Box<AST>),
}

#[derive(Debug)]
pub(crate) enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}