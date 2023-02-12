use crate::expr::Expr;

#[derive(Debug)]
pub struct FunctionName {
    pub name: String,
}

#[derive(Debug)]
pub struct FunctionArg {
    pub name: String,
    pub ty: crate::ty::Type,
}

#[derive(Debug)]
pub struct FunctionDef {
    pub name: FunctionName,
    pub args: Vec<FunctionArg>,
    pub body: Expr,
}
