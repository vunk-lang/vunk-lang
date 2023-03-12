// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::prelude::Simple;
use chumsky::Parser;
use vunk_lexer::Token;

use crate::ast::def::Def;
use crate::ast::generic::WhereClause;
use crate::ast::name::TypeName;
use crate::ast::name::VariableName;
use crate::Spanned;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Decl {
    pub visibility: Visibility,
    pub lhs: VariableName,
    pub rhs: DeclType,
    pub whereclause: Option<WhereClause>,
}

impl Decl {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        let i: Box<dyn Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>>> =
            { unimplemented!() };

        std::sync::Arc::new(i)
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Visibility {
    Public,
    Private,
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
    pub ty: DeclType,
}

impl DeclArg {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        chumsky::recursive::recursive(|_tree| {
            VariableName::parser()
                .or_not()
                .then(DeclType::parser())
                .map(|(opt_var_name, (decl_type, decl_type_span))| {
                    let span = std::ops::Range {
                        start: opt_var_name
                            .as_ref()
                            .map(|tpl| tpl.1.start)
                            .unwrap_or(decl_type_span.start),
                        end: decl_type_span.end,
                    };

                    let decl_arg = DeclArg {
                        name: opt_var_name.map(|tpl| tpl.0),
                        ty: decl_type,
                    };

                    (decl_arg, span)
                })
        })
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypeImpl {
    pub name: TypeName,
    pub generics: Option<WhereClause>,
    pub members: Vec<Def>,
}

impl TypeImpl {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        let i: Box<dyn Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>>> =
            { unimplemented!() };

        std::sync::Arc::new(i)
    }
}
