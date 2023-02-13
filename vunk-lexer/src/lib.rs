// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::error::Simple;
use chumsky::primitive::filter;
use chumsky::primitive::just;
use chumsky::primitive::one_of;
use chumsky::primitive::take_until;
use chumsky::recovery::skip_then_retry_until;
use chumsky::text;
use chumsky::text::TextParser;
use chumsky::Parser;

pub type Span = std::ops::Range<usize>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Ident(String),

    Arrow,
    Assign,
    Declare,
    Ctrl(char),
    Op(char),

    Num(String),
    Str(String),

    If,
    Else,

    Let,
    In,

    BlockOpen,
    BlockClose,
    Alternative,
    Where,

    Bool(bool),

    Use,
    Seperator,

    Comment(String),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Token::*;

        match self {
            Comment(text) => write!(f, "# {}", text),
            Arrow => write!(f, "->"),
            Assign => write!(f, "="),
            Declare => write!(f, ":"),
            Bool(x) => write!(f, "{}", x),
            Ctrl(c) => write!(f, "{}", c),
            Else => write!(f, "else"),
            Ident(s) => write!(f, "{}", s),
            If => write!(f, "if"),
            In => write!(f, "in"),
            Let => write!(f, "let"),
            Num(n) => write!(f, "{}", n),
            Str(s) => write!(f, "{}", s),
            Op(chr) => write!(f, "{}", chr),
            Use => write!(f, "use"),
            Seperator => write!(f, "."),
            BlockOpen => write!(f, "{{"),
            BlockClose => write!(f, "}}"),
            Alternative => write!(f, "|"),
            Where => write!(f, "where"),
        }
    }
}

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let num = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
        .collect::<String>()
        .map(Token::Num);

    // A parser for strings
    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Str);

    // A parser for control characters (delimiters, semicolons, etc.)
    let ctrl = one_of("(),").map(|c| Token::Ctrl(c));

    let operator = one_of("+-*/").map(|c| Token::Op(c));

    let assign = just("=").map(|_| Token::Assign);
    let declare = just(":").map(|_| Token::Declare);
    let seperator = just(".").map(|_| Token::Seperator);
    let kw_use = just("use").map(|_| Token::Use);
    let kw_arrow = just("->").map(|_| Token::Arrow);
    let kw_let = just("let").map(|_| Token::Let);
    let kw_in = just("in").map(|_| Token::In);
    let kw_if = just("if").map(|_| Token::If);
    let kw_else = just("else").map(|_| Token::Else);
    let kw_true = just("true").map(|_| Token::Bool(true));
    let kw_false = just("false").map(|_| Token::Bool(false));
    let kw_where = just("where").map(|_| Token::Where);
    let blockopen = just("{").map(|_| Token::BlockOpen);
    let blockclose = just("}").map(|_| Token::BlockClose);
    let alternative = just("|").map(|_| Token::Alternative);
    let ident = ident().map(|ident: String| Token::Ident(ident));

    // A single token can be one of the above
    let token = num
        .or(str_)
        .or(assign)
        .or(declare)
        .or(seperator)
        .or(kw_use)
        .or(kw_arrow)
        .or(kw_let)
        .or(kw_in)
        .or(kw_if)
        .or(kw_else)
        .or(kw_true)
        .or(kw_false)
        .or(kw_where)
        .or(blockopen)
        .or(blockclose)
        .or(alternative)
        .or(ctrl)
        .or(operator)
        .or(ident)
        .recover_with(skip_then_retry_until([]));

    let comment = just("#").then(take_until(just('\n'))).padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
}

fn ident<C: text::Character, E: chumsky::Error<C>>(
) -> impl Parser<C, C::Collection, Error = E> + Copy + Clone {
    filter(|c: &C| {
        let chr = c.to_char();
        chr.is_ascii_alphabetic() || chr == '_' || chr == '$'
    })
    .map(Some)
    .chain::<C, Vec<_>, _>(
        filter(|c: &C| {
            let chr = c.to_char();
            chr.is_ascii_alphanumeric() || c.to_char() == '_'
        })
        .repeated(),
    )
    .collect()
}
