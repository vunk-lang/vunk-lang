// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::prelude::Simple;
use chumsky::select;
use chumsky::Parser;
use vunk_lexer::Token;

use crate::ast::expr::Expr;
use crate::ast::generic::WhereClause;
use crate::ast::name::TypeName;
use crate::ast::name::VariableName;
use crate::Spanned;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Def {
    pub lhs: VariableName,
    pub rhs: DefRhs,
}

impl Def {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        VariableName::parser()
            .then_ignore({
                select! {
                    (Token::Assign, span) => ((), span)
                }
            })
            .then(DefRhs::parser())
            .map(|((var_name, var_name_span), (def_rhs, def_rhs_span))| {
                let span = std::ops::Range {
                    start: var_name_span.start,
                    end: def_rhs_span.end,
                };

                let def = Def {
                    lhs: var_name,
                    rhs: def_rhs,
                };

                (def, span)
            })
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct DefRhs {
    pub args: Vec<DefArg>,
    pub expr: Box<Expr>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct DefArg {
    pub name: VariableName,
    pub ty: DefArgType,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum DefArgType {
    TypeName(TypeName),
    Func { args: Vec<DefArg>, retty: TypeName },
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypeDef {
    pub name: TypeName,
    pub members: Vec<DefArg>,
    pub generics: Option<WhereClause>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct EnumDef {
    pub name: TypeName,
    pub variants: Vec<EnumTypeDef>,
    pub whereclause: Option<WhereClause>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct EnumTypeDef {
    pub name: TypeName,
    pub members: Vec<DefArg>,
}
