// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct WhereClause<'src>(pub Vec<&'src str>);

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Generic<'src> {
    pub type_name: &'src str,
    pub where_clause: WhereClause<'src>,
}
