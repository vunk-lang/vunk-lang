// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use crate::decl::Decl;
use crate::def::Def;
use crate::expr::Expr;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct LetIns {
    pub items: Vec<LetIn>,
    pub expr: Box<Expr>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum LetIn {
    Decl(Decl),
    Def(Def),
}
