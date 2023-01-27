use crate::literal::Literal;
use crate::function::FunctionName;
use crate::function::FunctionDef;

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    FuncDef(Box<FunctionDef>),
    FuncCall {
        fname: FunctionName,
        fargs: Vec<Expr>,
    },

}
