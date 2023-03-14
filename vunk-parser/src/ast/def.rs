// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use crate::ast::expr::Expr;
use crate::ast::generic::WhereClause;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Def {
    pub lhs: String,
    pub rhs: DefRhs,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct DefRhs {
    pub args: Vec<DefArg>,
    pub expr: Box<Expr>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct DefArg {
    pub name: String,
    pub ty: DefArgType,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum DefArgType {
    TypeName(String),
    Func { args: Vec<DefArg>, retty: String },
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypeDef {
    pub name: String ,
    pub members: Vec<DefArg>,
    pub generics: Option<WhereClause>,
}

