// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::str::FromStr;

use chumsky::prelude::Rich;
use chumsky::primitive::any;
use chumsky::primitive::end;
use chumsky::primitive::just;
use chumsky::recovery::skip_then_retry_until;
use chumsky::select;
use chumsky::Parser;

use chumsky::span::SimpleSpan;
use vunk_lexer::Token;

use crate::generic::WhereClause;
use crate::literal::Float;
use crate::literal::Integer;
use crate::op::BinaryOp;
use crate::op::UnaryOp;
use crate::Spanned;

type SpannedExpr<'src> = crate::Spanned<Expr<'src>>;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Expr<'src> {
    Bool(bool),
    Integer(Integer),
    Float(Float),
    Str(&'src str),
    List(Vec<SpannedExpr<'src>>),

    Ident(&'src str),

    Unary {
        op: UnaryOp,
        expr: Box<SpannedExpr<'src>>,
    },

    Binary {
        lhs: Box<SpannedExpr<'src>>,
        op: BinaryOp,
        rhs: Box<SpannedExpr<'src>>,
    },

    LetIn {
        exprs: Vec<SpannedExpr<'src>>,
    },

    IfElse {
        condition: Box<SpannedExpr<'src>>,
        tru: Box<SpannedExpr<'src>>,
        fals: Box<SpannedExpr<'src>>,
    },

    TypeDef {
        name: &'src str,
        whereclause: Option<WhereClause<'src>>,
        members: Vec<SpannedExpr<'src>>,
    },

    EnumDef {
        name: &'src str,
        variants: Vec<SpannedExpr<'src>>,
        whereclause: Option<WhereClause<'src>>,
    },

    Decl {
        ident: &'src str,
        decl_type: DeclType<'src>,
        whereclause: Option<WhereClause<'src>>,
    },

    Def {
        lhs: &'src str,
        rhs: DefRhs<'src>,
    },
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum DeclType<'src> {
    TypeName(&'src str),
    Func {
        args: Vec<DeclArg<'src>>,
        retty: &'src str,
    },
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct DeclArg<'src> {
    pub name: Option<&'src str>,
    pub ty: DeclType<'src>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct DefRhs<'src> {
    pub args: Vec<(&'src str, DeclType<'src>)>,
    pub expr: Box<SpannedExpr<'src>>,
}

type ParserInput<'tokens, 'src> =
    chumsky::input::SpannedInput<Token<'src>, SimpleSpan, &'tokens [(Token<'src>, SimpleSpan)]>;

impl Expr<'_> {
    fn parser<'tokens, 'src: 'tokens>() -> impl Parser<
        'tokens,
        ParserInput<'tokens, 'src>,
        Spanned<Expr<'src>>,
        chumsky::extra::Err<Rich<'tokens, Token<'src>, SimpleSpan>>,
    > + Clone {
        chumsky::recursive::recursive(|expr| {
            let bool_parser = select! {
                (Token::Bool(true), span) => (Expr::Bool(true), span),
                (Token::Bool(false), span) => (Expr::Bool(false), span),
            };

            let int_parser = select! {
                (Token::Num(numstr), span) => {
                    let span: std::ops::Range<_> = span;
                    i8::from_str(&numstr)
                     .map(Integer::I8)
                     .or_else(|_| i16::from_str(&numstr).map(Integer::I16))
                     .or_else(|_| i32::from_str(&numstr).map(Integer::I32))
                     .or_else(|_| i64::from_str(&numstr).map(Integer::I64))
                     .or_else(|_| u8::from_str(&numstr).map(Integer::U8))
                     .or_else(|_| u16::from_str(&numstr).map(Integer::U16))
                     .or_else(|_| u32::from_str(&numstr).map(Integer::U32))
                     .or_else(|_| u64::from_str(&numstr).map(Integer::U64))
                     .map(|i| (Expr::Integer(i), span))
                     .map_err(|_| {
                         Rich::custom(
                             span.clone(),
                             format!("Cannot parse number to Integer '{}'", numstr),
                         )
                     })
                }
            };

            // TODO: Support floats
            // let float_parser = select! {
            // };

            let str_parser = select! {
                Token::Str(s) => Expr::Str(s)
            };

            let list_parser = {
                let left_br = just(Token::Ctrl('[')).to(());
                let right_br = just(Token::Ctrl(']')).to(());

                expr.delimited_by(left_br, right_br)
            };

            let ident_parser = select! {
                Token::Ident(s) => Expr::Ident(s),
            };

            let unary_parser = {
                let unary_op_parser = {
                    let binary = just(Token::Op("~")).to(UnaryOp::BinaryNot);
                    let logical = just(Token::Op("!")).to(UnaryOp::LogicalNot);

                    binary.or(logical)
                };

                unary_op_parser
                    .then(expr.clone())
                    .map_with_span(|(op, expr), span| {
                        let e = Expr::Unary {
                            op,
                            expr: Box::new((expr, span)),
                        };

                        (e, span)
                    })
            };

            let binary_parser = {
                let binary_op_parser = just(Token::Op("+"))
                    .to(BinaryOp::Add)
                    .or(just(Token::Op("-")).to(BinaryOp::Sub))
                    .or(just(Token::Op("*")).to(BinaryOp::Mul))
                    .or(just(Token::Op("/")).to(BinaryOp::Div))
                    .or(just(Token::Op("%")).to(BinaryOp::Rem))
                    .or(just(Token::Op("==")).to(BinaryOp::Eq))
                    .or(just(Token::Op("!=")).to(BinaryOp::NotEq))
                    .or(just(Token::Op("<")).to(BinaryOp::Less))
                    .or(just(Token::Op("<=")).to(BinaryOp::LessEq))
                    .or(just(Token::Op(">")).to(BinaryOp::More))
                    .or(just(Token::Op(">=")).to(BinaryOp::MoreEq))
                    .or(just(Token::Op("&")).to(BinaryOp::BitAnd))
                    .or(just(Token::Op("&&")).to(BinaryOp::LogicalAnd))
                    .or(just(Token::Op("|")).to(BinaryOp::BitOr))
                    .or(just(Token::Op("||")).to(BinaryOp::LogicalOr))
                    .or(just(Token::Op("^")).to(BinaryOp::BitXor))
                    .or(just(Token::Op("++")).to(BinaryOp::Join));

                expr.clone()
                    .then(binary_op_parser)
                    .then(expr.clone())
                    .map_with_span(|((lhs, op), rhs), span| {
                        let e = Expr::Binary {
                            op,
                            lhs: Box::new((lhs, span)),
                            rhs: Box::new((rhs, span)),
                        };

                        (e, span)
                    })
            };

            bool_parser
                .or(int_parser)
                .or(str_parser)
                .or(list_parser)
                .or(ident_parser)
                .or(unary_parser)
                .or(binary_parser)
                .map_with_span(|t, s| (t, s))
                .padded()
                .recover_with(skip_then_retry_until(any().ignored(), end()))
                .repeated()
                .collect()
        })
    }
}
