// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use crate::ast::def::Def;
use crate::ast::generic::WhereClause;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Decl {
    pub lhs: VariableName,
    pub rhs: DeclType,
    pub whereclause: Option<WhereClause>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum DeclType {
    TypeName(String),
    Func { args: Vec<DeclArg>, retty: String },
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct DeclArg {
    pub name: Option<String>,
    pub ty: DeclType,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypeImpl {
    pub name: String,
    pub generics: Option<WhereClause>,
    pub members: Vec<Def>,
}
