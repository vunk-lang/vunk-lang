// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
