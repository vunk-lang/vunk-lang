// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::prelude::Simple;
use chumsky::Parser;
use chumsky::select;
use vunk_lexer::Token;

use crate::ast::def::Def;
use crate::ast::generic::WhereClause;
use crate::ast::name::TypeName;
use crate::ast::name::VariableName;
use crate::Spanned;

use super::util::arrow;
use super::util::assign;
use super::util::block_close;
use super::util::block_open;
use super::util::comma;
use super::util::declare;
use super::util::kw_impl;
use super::util::par_close;
use super::util::par_open;

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
        Visibility::parser()
            .then(VariableName::parser())
            .then_ignore(declare())
            .then(DeclType::parser())
            .then(WhereClause::parser().or_not())
            .map(
                |(
                    (((vis, vis_span), (varname, _varname_span)), (declty, declty_span)),
                    opt_where,
                )| {
                    let span = std::ops::Range {
                        start: vis_span.start,
                        end: opt_where
                            .as_ref()
                            .map(|tpl| tpl.1.end)
                            .unwrap_or(declty_span.end),
                    };

                    let decl = Decl {
                        visibility: vis,
                        lhs: varname,
                        rhs: declty,
                        whereclause: opt_where.map(|tpl| tpl.0),
                    };

                    (decl, span)
                },
            )
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Visibility {
    Public,
    Private,
}

impl Visibility {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        select! {
            (Token::Pub, span) => (Self::Public, span)
        }
        .or_else(|_error| Ok((Self::Private, std::ops::Range::default())))
    }
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

        let func_parser = par_open()
            .ignore_then(DeclArg::parser().separated_by(comma()))
            .then_ignore(par_close())
            .then_ignore(arrow())
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
    pub generics: Vec<TypeName>,
    pub whereclause: Option<WhereClause>,
    pub members: Vec<TypeImplMember>,
}

impl TypeImpl {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        kw_impl()
            .ignore_then(TypeName::parser())
            .then(TypeName::parser().repeated().or_not()) // Generics
            .then(WhereClause::parser().or_not())
            .then_ignore(assign())
            .then_ignore(block_open())
            .then(TypeImplMember::parser().repeated().or_not())
            .then_ignore(block_close())
            .map(
                |((((type_name, type_name_span), generics), whereclause), members)| {
                    let span = std::ops::Range {
                        start: type_name_span.start,
                        end: members
                            .as_ref()
                            .map(|mem| {
                                mem.iter()
                                    .rev()
                                    .next()
                                    .map(|(_, span)| span.end)
                                    .unwrap_or(type_name_span.end)
                            })
                            .unwrap_or(type_name_span.end),
                    };

                    let typeimpl = TypeImpl {
                        name: type_name,
                        generics: generics
                            .unwrap_or_default()
                            .into_iter()
                            .map(|(gen, _)| gen)
                            .collect(),
                        whereclause: whereclause.map(|wc| wc.0),
                        members: members
                            .unwrap_or_default()
                            .into_iter()
                            .map(|(mem, _span)| mem)
                            .collect(),
                    };

                    (typeimpl, span)
                },
            )
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum TypeImplMember {
    Decl(Decl),
    Def(Def),
}

impl TypeImplMember {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        let decl_parser =
            Decl::parser().map(|(decl, decl_span)| (TypeImplMember::Decl(decl), decl_span));
        let def_parser =
            Def::parser().map(|(decl, decl_span)| (TypeImplMember::Def(decl), decl_span));
        decl_parser.or(def_parser)
    }
}
