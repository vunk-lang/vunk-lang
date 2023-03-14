// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::prelude::Simple;
use chumsky::select;
use chumsky::Parser;
use vunk_lexer::Token;

use crate::ast::def::Def;
use crate::ast::generic::WhereClause;
use crate::ast::ifelse::IfElse;
use crate::ast::letin::LetIns;
use crate::ast::literal::Literal;
use crate::ast::module::Module;
use crate::ast::name::TypeName;
use crate::ast::name::VariableName;
use crate::ast::op::BinaryOp;
use crate::ast::op::UnaryOp;
use crate::Spanned;

use super::op::OpLhs;
use super::op::OpRhs;
use super::util::assign;
use super::util::block_close;
use super::util::block_open;
use super::util::declare;
use super::util::kw_impl;
use super::util::statement_end;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Decl {
    pub visibility: Visibility,
    pub lhs: VariableName,
    pub rhs: DeclRhs,
    pub whereclause: Option<WhereClause>,
}

impl Decl {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        chumsky::recursive::recursive(|_| {
            Visibility::parser()
                .then(VariableName::parser())
                .then_ignore(declare())
                .then(DeclRhs::parser())
                .then(WhereClause::parser().or_not())
                .then_ignore(statement_end())
                .map(
                    |(
                        (((vis, vis_span), (varname, _varname_span)), (rhs, rhs_span)),
                        opt_where,
                    )| {
                        let span = std::ops::Range {
                            start: vis_span.start,
                            end: opt_where
                                .as_ref()
                                .map(|tpl| tpl.1.end)
                                .unwrap_or(rhs_span.end),
                        };

                        let decl = Decl {
                            visibility: vis,
                            lhs: varname,
                            rhs,
                            whereclause: opt_where.map(|tpl| tpl.0),
                        };

                        (decl, span)
                    },
                )
        })
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

/// An Expr without Def and Decl
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum DeclRhs {
    Variable(VariableName),
    Unary(UnaryOp, OpRhs),
    Binary(BinaryOp, OpLhs, OpRhs),
    Literal(Literal),
    LetIn(LetIns),
    IfElse(IfElse),
    Module(Module),
}

impl DeclRhs {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        let variable_parser = VariableName::parser().map(|(v, span)| (DeclRhs::Variable(v), span));

        let unary_parser = UnaryOp::parser()
            .then(OpRhs::parser())
            .map(|((op, opspan), (l, lspan))| {
                let span = std::ops::Range {
                    start: opspan.start,
                    end: lspan.end,
                };

                let e = DeclRhs::Unary(op, l);
                (e, span)
            });

        let binary_parser = OpLhs::parser()
            .then(BinaryOp::parser())
            .then(OpRhs::parser())
            .map(|(((l, _lspan), (op, _opspan)), (r, rspan))| {
                let span = std::ops::Range {
                    start: rspan.start,
                    end: rspan.end,
                };

                let e = DeclRhs::Binary(op, l, r);
                (e, span)
            });

        let literal_parser = Literal::parser().map(|(lit, span)| {
            let e = DeclRhs::Literal(lit);
            (e, span)
        });

        let letin_parser = LetIns::parser().map(|(li, span)| {
            let e = DeclRhs::LetIn(li);
            (e, span)
        });

        let ifelse_parser = IfElse::parser().map(|(ie, span)| {
            let e = DeclRhs::IfElse(ie);
            (e, span)
        });

        let module_parser = Module::parser().map(|(m, span)| {
            let e = DeclRhs::Module(m);
            (e, span)
        });

        variable_parser
            .or(unary_parser)
            .or(binary_parser)
            .or(literal_parser)
            .or(letin_parser)
            .or(ifelse_parser)
            .or(module_parser)
            .then_ignore(statement_end())
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decl_parser() {
        let code = r#"
            foo = bar;
        "#;

        let parser = Decl::parser();
        let tokens = vunk_lexer::lexer().parse(code).unwrap();
        let parsed = parser.parse(tokens).unwrap();

        let decl = parsed.0;

        assert_eq!(decl.visibility, Visibility::Private);
        assert_eq!(decl.lhs, VariableName(String::from("foo")));
    }

    #[test]
    fn test_decl_rhs_parser() {
        let code = r#"
            bar;
        "#;

        let parser = DeclRhs::parser();
        let tokens = vunk_lexer::lexer().parse(code).unwrap();
        let parsed = parser.parse(tokens).unwrap();

        let rhs = parsed.0;

        assert_eq!(rhs, DeclRhs::Variable(VariableName(String::from("bar"))));
    }
}
