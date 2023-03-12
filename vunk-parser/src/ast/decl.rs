// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::prelude::Simple;
use chumsky::select;
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

impl DeclType {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        let type_name_parser = TypeName::parser().map(|(type_name, type_name_span)| {
            let dty = DeclType::TypeName(type_name);
            (dty, type_name_span)
        });

        let par_open = select! {
            (Token::ParOpen, span) => ((), span)
        };

        let par_close = select! {
            (Token::ParClose, span) => ((), span)
        };

        let arrow = select! {
            (Token::Arrow, span) => ((), span)
        };

        let comma = select! {
            (Token::Comma, span) => ((), span)
        };

        let func_parser = par_open
            .ignore_then(DeclArg::parser().separated_by(comma))
            .then_ignore(par_close)
            .then_ignore(arrow)
            .then(TypeName::parser())
            .map(|(args, (retty, retty_span))| {
                let span = std::ops::Range {
                    start: args.first().map(|tpl| tpl.1.start).unwrap_or(0),
                    end: retty_span.end,
                };

                let decl_type = DeclType::Func {
                    args: args.into_iter().map(|tpl| tpl.0).collect(),
                    retty,
                };

                (decl_type, span)
            });

        type_name_parser.or(func_parser)
    }
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
