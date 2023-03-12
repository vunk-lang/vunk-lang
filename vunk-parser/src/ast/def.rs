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

impl DefRhs {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
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

        par_open
            .ignore_then(DefArg::parser().separated_by(comma))
            .then_ignore(par_close)
            .then_ignore(arrow)
            .then(Expr::parser())
            .map(|(args, (expr, expr_span))| {
                let span = std::ops::Range {
                    start: args
                        .first()
                        .map(|tpl| tpl.1.start)
                        .unwrap_or(expr_span.start),
                    end: expr_span.end,
                };

                let def_rhs = DefRhs {
                    args: args.into_iter().map(|tpl| tpl.0).collect(),
                    expr: Box::new(expr),
                };

                (def_rhs, span)
            })
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct DefArg {
    pub name: VariableName,
    pub ty: DefArgType,
}

impl DefArg {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        VariableName::parser()
            .then_ignore(select! {
                (Token::Declare, span) => ((), span)
            })
            .then(DefArgType::parser())
            .map(
                |((var_name, var_span), (def_arg_type, def_arg_type_span))| {
                    let span = std::ops::Range {
                        start: var_span.start,
                        end: def_arg_type_span.end,
                    };

                    let defarg = DefArg {
                        name: var_name,
                        ty: def_arg_type,
                    };

                    (defarg, span)
                },
            )
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum DefArgType {
    TypeName(TypeName),
    Func { args: Vec<DefArg>, retty: TypeName },
}

impl DefArgType {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        let type_name_parser =
            TypeName::parser().map(|(tn, span)| (DefArgType::TypeName(tn), span));

        let func_parser = {
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

            par_open
                .ignore_then(DefArg::parser().separated_by(comma))
                .then_ignore(par_close)
                .then_ignore(arrow)
                .then(TypeName::parser())
                .map(|(args, (retty, retty_span))| {
                    let span = std::ops::Range {
                        start: args
                            .first()
                            .map(|tpl| tpl.1.start)
                            .unwrap_or(retty_span.start),
                        end: retty_span.end,
                    };

                    let func = DefArgType::Func {
                        args: args.into_iter().map(|tpl| tpl.0).collect(),
                        retty,
                    };

                    (func, span)
                })
        };

        chumsky::recursive::recursive(|_tree| type_name_parser.or(func_parser))
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypeDef {
    pub name: TypeName,
    pub members: Vec<DefArg>,
    pub generics: Vec<TypeName>,
    pub whereclause: Option<WhereClause>,
}

impl TypeDef {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        let type_kw = select! {
            (Token::Type, span) => ((), span)
        };
        let comma = select! {
            (Token::Comma, span) => ((), span)
        };
        let assign = select! {
            (Token::Assign, span) => ((), span)
        };
        let block_open = select! {
            (Token::BlockOpen, span) => ((), span)
        };
        let block_close = select! {
            (Token::BlockClose, span) => ((), span)
        };

        type_kw
            .ignore_then(TypeName::parser())
            .then({
                // Generics
                TypeName::parser().repeated().or_not()
            })
            .then(WhereClause::parser().or_not())
            .then_ignore(assign)
            .then_ignore(block_open)
            .then(DefArg::parser().separated_by(comma))
            .then_ignore(block_close)
            .map(
                |((((type_name, type_span), generics), whereclause), members)| {
                    let span = std::ops::Range {
                        start: type_span.start,
                        end: members
                            .iter()
                            .rev()
                            .next()
                            .map(|(_, span)| span.end)
                            .unwrap_or(type_span.end),
                    };

                    let typedef = TypeDef {
                        name: type_name,
                        members: members.into_iter().map(|tpl| tpl.0).collect(),
                        generics: generics
                            .unwrap_or_default()
                            .into_iter()
                            .map(|tpl| tpl.0)
                            .collect(),
                        whereclause: whereclause.map(|(clause, _)| clause),
                    };

                    (typedef, span)
                },
            )
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct EnumDef {
    pub name: TypeName,
    pub variants: Vec<EnumTypeDef>,
    pub whereclause: Option<WhereClause>,
}

impl EnumDef {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        let i: Box<dyn Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>>> =
            { unimplemented!() };

        std::sync::Arc::new(i)
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct EnumTypeDef {
    pub name: TypeName,
    pub members: Vec<DefArg>,
}
