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
            (Token::Op(op), span) => match op.as_ref() {
                "~" => (UnaryOp::BinaryNot, span),
                "!" => (UnaryOp::LogicalNot, span),
                _ => return Err(chumsky::error::Error::expected_input_found(span, None, None)),
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
