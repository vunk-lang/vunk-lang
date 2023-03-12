// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use crate::ast::name::TraitName;
use crate::ast::name::TypeName;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct WhereClause(pub Vec<TraitName>);

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Generic {
    pub type_name: TypeName,
    pub where_clause: WhereClause,
}
