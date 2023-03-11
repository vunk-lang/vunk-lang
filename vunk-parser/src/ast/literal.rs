// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::ops::Range;
use std::str::FromStr;

use chumsky::error::Simple;
use chumsky::select;
use chumsky::Parser;

use crate::ast::expr::Expr;
use crate::ast::name::VariableName;
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
        .or(parse_list())
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

fn list_open_parser(
) -> impl Parser<Spanned<Token>, Spanned<()>, Error = Simple<Spanned<Token>>> + Clone {
    chumsky::select! {
        (Token::Ctrl('['), span) => ((), span)
    }
}

fn list_close_parser(
) -> impl Parser<Spanned<Token>, Spanned<()>, Error = Simple<Spanned<Token>>> + Clone {
    chumsky::select! {
        (Token::Ctrl(']'), span) => ((), span)
    }
}

fn parse_list(
) -> impl Parser<Spanned<Token>, Spanned<Literal>, Error = Simple<Spanned<Token>>> + Clone {
    let list_elements_parser = {
        VariableName::parser()
            .map_with_span(|(varname, _span), span| (Expr::Variable(varname), span))
            .repeated()
            .collect()
            .map(|itms: Vec<(_, _)>| {
                let start = itms.first().map(|f| f.1.start).unwrap_or(0);
                let end = itms.iter().rev().next().map(|f| f.1.end).unwrap_or(0);

                let mut items = Vec::with_capacity(itms.len());
                for itm in itms.into_iter() {
                    items.push(itm.0);
                }

                let span = std::ops::Range { start, end };

                (Literal::List(items), span)
            })
    };

    list_open_parser()
        .ignore_then(list_elements_parser)
        .then_ignore(list_close_parser())
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

#[cfg(test)]
mod tests {
    use crate::ast::name::VariableName;

    use super::*;

    #[test]
    fn test_parse_list() {
        let parser = parse_list();
        let tokens = vunk_lexer::lexer().parse("[ a ]").unwrap();
        let parsed = parser.parse(tokens).unwrap();

        assert!(matches!(parsed.0, Literal::List(_)));
        if let Literal::List(list) = parsed.0 {
            assert_eq!(list.len(), 1);
            assert!(matches!(list[0], Expr::Variable(VariableName(_))));
        } else {
            unreachable!()
        }
    }
}
