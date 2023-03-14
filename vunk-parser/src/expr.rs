// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use crate::generic::WhereClause;
use crate::literal::Bool;
use crate::literal::Float;
use crate::literal::Integer;
use crate::literal::Str;
use crate::op::BinaryOp;
use crate::op::UnaryOp;

type SpannedExpr = crate::Spanned<Expr>;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Expr {
    Bool(Bool),
    Integer(Integer),
    Float(Float),
    Str(Str),
    List(Vec<SpannedExpr>),

    Ident(String),

    Unary {
        op: UnaryOp,
        expr: Box<SpannedExpr>,
    },

    Binary {
        lhs: Box<SpannedExpr>,
        op: BinaryOp,
        rhs: Box<SpannedExpr>,
    },

    LetIn {
        exprs: Vec<SpannedExpr>,
    },

    IfElse {
        condition: Box<SpannedExpr>,
        tru: Box<SpannedExpr>,
        fals: Box<SpannedExpr>,
    },

    TypeDef {
        name: String,
        whereclause: Option<WhereClause>,
        members: Vec<SpannedExpr>,
    },

    EnumDef {
        name: String,
        variants: Vec<SpannedExpr>,
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
    pub expr: Box<SpannedExpr>,
}
