// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
