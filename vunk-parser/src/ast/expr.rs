// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use crate::ast::generic::WhereClause;
use crate::ast::op::BinaryOp;
use crate::ast::op::UnaryOp;

use crate::ast::literal::Bool;
use crate::ast::literal::Float;
use crate::ast::literal::Integer;
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

    LetIn {
        exprs: Vec<Expr>,
    },

    IfElse {
        condition: Box<Expr>,
        tru: Box<Expr>,
        fals: Box<Expr>,
    },

    TypeDef {
        name: String,
        whereclause: Option<WhereClause>,
        members: Vec<Expr>,
    },

    EnumDef {
        name: String,
        variants: Vec<Expr>,
        whereclause: Option<WhereClause>,
    },

    Decl {
        ident: String,
        decl_type: DeclType,
        whereclause: Option<WhereClause>,
    },

    Def {
        lhs: String,
        rhs: DefRhs,
    },
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
pub struct DefRhs {
    pub args: Vec<(String, DeclType)>,
    pub expr: Box<Expr>,
}

