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
pub enum Op {
    BinaryNot,
    LogicalNot,
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

impl Op {
    pub fn parser() -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        chumsky::select! {
            (Token::Op(op), span) => {
                let span: std::ops::Range<usize> = span;

                match op.as_ref() {
                    "~" => (Op::BinaryNot, span),
                    "!" => (Op::LogicalNot, span),
                    "+" => (Op::Add, span),
                    "-" => (Op::Sub, span),
                    "*" => (Op::Mul, span),
                    "/" => (Op::Div, span),
                    "%" => (Op::Rem, span),
                    "==" => (Op::Eq, span),
                    "!=" => (Op::NotEq, span),
                    "<"=> (Op::Less, span),
                    "<=" => (Op::LessEq, span),
                    ">"=> (Op::More, span),
                    ">=" => (Op::MoreEq, span),

                    "&"=> (Op::BitAnd, span),
                    "&&" => (Op::LogicalAnd, span),
                    "|"=> (Op::BitOr, span),
                    "||" => (Op::LogicalOr, span),

                    "^"=> (Op::BitXor, span),

                    "++" => (Op::Join, span),
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

