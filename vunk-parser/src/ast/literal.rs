// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::ops::Range;
use std::str::FromStr;

use chumsky::error::Simple;
use chumsky::select;
use chumsky::Parser;

use vunk_lexer::Token;

use crate::Spanned;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Literal {
    Bool(Bool),
    Integer(Integer),
    Float(Float),
    Str(Str),
    List(Vec<crate::ast::expr::Expr>),
}

impl Literal {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Literal>, Error = Simple<Spanned<Token>>> + Clone {
        chumsky::select! {
            (Token::Num(num), span) => {
                let (int, span) = parse_num(num, &span)?;
                (Literal::Integer(int), span)
            },
            (Token::Str(s), span) => (Literal::Str(Str { value: s }), span),
            (Token::Bool(b), span) => (Literal::Bool(Bool { value: b }), span),
        }
    }
}

fn parse_num(num: String, span: &Range<usize>) -> Result<Spanned<Integer>, Simple<Spanned<Token>>> {
    Ok(Integer {
        value: {
            i8::from_str(&num)
                .map(IntegerValue::I8)
                .or_else(|_| i16::from_str(&num).map(IntegerValue::I16))
                .or_else(|_| i32::from_str(&num).map(IntegerValue::I32))
                .or_else(|_| i64::from_str(&num).map(IntegerValue::I64))
                .or_else(|_| u8::from_str(&num).map(IntegerValue::U8))
                .or_else(|_| u16::from_str(&num).map(IntegerValue::U16))
                .or_else(|_| u32::from_str(&num).map(IntegerValue::U32))
                .or_else(|_| u64::from_str(&num).map(IntegerValue::U64))
                .map_err(|_| {
                    Simple::custom(
                        span.clone(),
                        format!("Cannot parse number to Integer {}", num),
                    )
                })?
        },
    })
    .map(|i| (i, span.clone()))
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Bool {
    pub value: bool,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Integer {
    pub value: IntegerValue,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum IntegerValue {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Float {
    pub value: f64,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Str {
    pub value: String,
}
