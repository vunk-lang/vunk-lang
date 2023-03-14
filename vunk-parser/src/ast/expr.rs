// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use crate::ast::decl::Decl;
use crate::ast::def::Def;
use crate::ast::ifelse::IfElse;
use crate::ast::letin::LetIns;
use crate::ast::op::BinaryOp;
use crate::ast::op::UnaryOp;

use crate::ast::literal::Bool;
use crate::ast::literal::Integer;
use crate::ast::literal::Float;
use crate::ast::literal::Str;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Expr {
    Bool(Bool),
    Integer(Integer),
    Float(Float),
    Str(Str),
    List(Vec<crate::ast::expr::Expr>),

    Ident(String),

    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },

    Binary {
        lhs: Box<Expr>,
        op: BinaryOp,
        rhs: Box<Expr>,
    },

    LetIn(LetIns),
    IfElse(IfElse),
    Decl(Decl),
    Def(Def),
}
