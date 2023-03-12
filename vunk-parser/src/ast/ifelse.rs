// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use crate::ast::expr::Expr;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct IfElse {
    pub condition: Box<Expr>,
    pub tru: Box<Expr>,
    pub fals: Box<Expr>,
}
