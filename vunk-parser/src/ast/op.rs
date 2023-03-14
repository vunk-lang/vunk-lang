// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::error::Simple;
use chumsky::select;
use chumsky::Parser;

use vunk_lexer::Token;

use crate::Spanned;

use super::literal::Literal;
use super::name::VariableName;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum UnaryOp {
    BinaryNot,
    LogicalNot,
}

impl UnaryOp {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<UnaryOp>, Error = Simple<Spanned<Token>>> + Clone {
        chumsky::select! {
            (Token::Op(op), span) => {
                let span: std::ops::Range<usize> = span;

                match op.as_ref() {
                    "~" => (UnaryOp::BinaryNot, span),
                    "!" => (UnaryOp::LogicalNot, span),
                    _ => {
                        use chumsky::error::Error;

                        let op = Token::Op(op);
                        let found: Option<Spanned<Token>> = Some((op, span.clone()));
                        return Err(chumsky::error::Simple::expected_input_found(span, None, found))
                    },
                }
            }
        }
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    NotEq,
    Less,
    LessEq,
    More,
    MoreEq,
    BitAnd,
    LogicalAnd,
    BitOr,
    LogicalOr,
    BitXor,
    Join,
}

impl BinaryOp {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<BinaryOp>, Error = Simple<Spanned<Token>>> + Clone
    {
        chumsky::select! {
            (Token::Op(op), span) => {
                let span: std::ops::Range<usize> = span;
                match op.as_ref() {
                    "+" => (BinaryOp::Add, span),
                    "-" => (BinaryOp::Sub, span),
                    "*" => (BinaryOp::Mul, span),
                    "/" => (BinaryOp::Div, span),
                    "%" => (BinaryOp::Rem, span),
                    "==" => (BinaryOp::Eq, span),
                    "!=" => (BinaryOp::NotEq, span),
                    "<"=> (BinaryOp::Less, span),
                    "<=" => (BinaryOp::LessEq, span),
                    ">"=> (BinaryOp::More, span),
                    ">=" => (BinaryOp::MoreEq, span),

                    "&"=> (BinaryOp::BitAnd, span),
                    "&&" => (BinaryOp::LogicalAnd, span),
                    "|"=> (BinaryOp::BitOr, span),
                    "||" => (BinaryOp::LogicalOr, span),

                    "^"=> (BinaryOp::BitXor, span),

                    "++" => (BinaryOp::Join, span),

                    // TODO: Make nice
                    _ => {
                        use chumsky::error::Error;

                        let op = Token::Op(op);
                        let found: Option<Spanned<Token>> = Some((op, span.clone()));

                        return Err(chumsky::error::Simple::expected_input_found(span, None, found))
                    },
                }
            }
        }
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum OpLhs {
    Variable(VariableName),
    Unary(UnaryOp, Box<OpRhs>),
    Binary(BinaryOp, Box<OpLhs>, Box<OpRhs>),
    Literal(Literal),
}

impl OpLhs {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        chumsky::recursive::recursive(|_| {
            let variable_parser = VariableName::parser().map(|(v, span)| (Self::Variable(v), span));

            let unary_parser = UnaryOp::parser()
                .then(OpRhs::parser().map(|(e, span)| (Box::new(e), span)))
                .map(|((op, opspan), (ex, exspan))| {
                    let span = std::ops::Range {
                        start: opspan.start,
                        end: exspan.end,
                    };

                    let e = Self::Unary(op, ex);
                    (e, span)
                });

            let binary_parser = OpLhs::parser()
                .map(|(e, span)| (Box::new(e), span))
                .then(BinaryOp::parser())
                .then(OpRhs::parser().map(|(e, span)| (Box::new(e), span)))
                .map(|(((exl, _exlspan), (op, _opspan)), (exr, exrspan))| {
                    let span = std::ops::Range {
                        start: exrspan.start,
                        end: exrspan.end,
                    };

                    let e = Self::Binary(op, exl, exr);
                    (e, span)
                });

            let literal_parser = Literal::parser().map(|(lit, span)| {
                let e = Self::Literal(lit);
                (e, span)
            });

            variable_parser
                .or(unary_parser)
                .or(binary_parser)
                .or(literal_parser)
        })
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum OpRhs {
    Variable(VariableName),
    Unary(UnaryOp, Box<OpRhs>),
    Binary(BinaryOp, Box<OpLhs>, Box<OpRhs>),
    Literal(Literal),
}

impl OpRhs {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        chumsky::recursive::recursive(|_| {
            let variable_parser = VariableName::parser().map(|(v, span)| (Self::Variable(v), span));

            let unary_parser = UnaryOp::parser()
                .then(OpRhs::parser().map(|(e, span)| (Box::new(e), span)))
                .map(|((op, opspan), (ex, exspan))| {
                    let span = std::ops::Range {
                        start: opspan.start,
                        end: exspan.end,
                    };

                    let e = Self::Unary(op, ex);
                    (e, span)
                });

            let binary_parser = OpLhs::parser()
                .map(|(e, span)| (Box::new(e), span))
                .then(BinaryOp::parser())
                .then(OpRhs::parser().map(|(e, span)| (Box::new(e), span)))
                .map(|(((exl, _exlspan), (op, _opspan)), (exr, exrspan))| {
                    let span = std::ops::Range {
                        start: exrspan.start,
                        end: exrspan.end,
                    };

                    let e = Self::Binary(op, exl, exr);
                    (e, span)
                });

            let literal_parser = Literal::parser().map(|(lit, span)| {
                let e = Self::Literal(lit);
                (e, span)
            });

            variable_parser
                .or(unary_parser)
                .or(binary_parser)
                .or(literal_parser)
        })
    }
}
