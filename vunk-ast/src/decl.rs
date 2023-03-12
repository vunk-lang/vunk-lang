// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use crate::generic::WhereClause;
use crate::name::TypeName;
use crate::name::VariableName;

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
    TypeName(TypeName),
    Func { args: Vec<DeclArg>, retty: TypeName },
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct DeclArg {
    pub name: Option<VariableName>,
    pub ty: DeclArgType,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum DeclArgType {
    TypeName(TypeName),
    Func { args: Vec<DeclArg>, retty: TypeName },
}
