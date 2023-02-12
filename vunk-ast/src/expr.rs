use crate::function::FunctionDef;
use crate::function::FunctionName;
use crate::literal::Literal;

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    FuncDef(Box<FunctionDef>),
    FuncCall {
        fname: FunctionName,
        fargs: Vec<Expr>,
    },
}
