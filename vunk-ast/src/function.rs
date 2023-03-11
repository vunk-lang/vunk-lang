// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use crate::expr::Expr;
use crate::generic::Generic;
use crate::name::TypeName;
use crate::name::VariableName;

#[derive(Debug)]
pub struct FunctionDecl {
    pub name: FunctionName,
    pub generics: Vec<Generic>,
    pub parameters: Vec<FunctionArg>,
    pub returnty: TypeName,
}

#[derive(Debug)]
pub struct FunctionDef {
    pub name: FunctionName,
    pub generics: Vec<Generic>,
    pub args: Vec<FunctionArg>,
    pub body: Box<Expr>,
}

#[derive(Debug)]
pub struct FunctionCall {
    pub name: FunctionName,
    pub args: Vec<Expr>,
}

#[derive(Debug)]
pub struct FunctionName {
    pub name: String,
}

#[derive(Debug)]
pub struct FunctionArg {
    pub name: VariableName,
    pub ty: TypeName,
}
