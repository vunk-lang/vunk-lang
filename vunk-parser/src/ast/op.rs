// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::error::Simple;
use chumsky::Parser;
use chumsky::select;

use vunk_lexer::Token;

use crate::Spanned;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum UnaryOp {
    BinaryNot,
    LogicalNot,
}

impl UnaryOp {
    pub fn parser() -> impl Parser<Spanned<Token>, Spanned<UnaryOp>, Error = Simple<Spanned<Token>>> + Clone {
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
    pub fn parser() -> impl Parser<Spanned<Token>, Spanned<BinaryOp>, Error = Simple<Spanned<Token>>> + Clone {
        chumsky::select! {
            (Token::Op(op), span) => match op.as_ref() {
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
                _ => return Err(chumsky::error::Error::expected_input_found(span, None, None)),
            }
        }
    }
}
