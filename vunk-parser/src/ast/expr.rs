// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use crate::ast::decl::Decl;
use crate::ast::def::Def;
use crate::ast::ifelse::IfElse;
use crate::ast::letin::LetIns;
use crate::ast::literal::Literal;
use crate::ast::name::VariableName;
use crate::ast::op::BinaryOp;
use crate::ast::op::UnaryOp;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Expr {
    Variable(VariableName),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Literal(Literal),
    LetIn(LetIns),
    IfElse(IfElse),
    Decl(Decl),
    Def(Def),
}
