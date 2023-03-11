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
pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Ident(String),

    Arrow,
    Assign,
    Declare,
    Ctrl(char),
    Op(String),

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
    Match,
    When,
    ListOpen,
    ListClose,

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
            Op(s) => write!(f, "{}", s),
            Use => write!(f, "use"),
            Seperator => write!(f, "."),
            BlockOpen => write!(f, "{{"),
            BlockClose => write!(f, "}}"),
            ListOpen => write!(f, "["),
            ListClose => write!(f, "]"),
            Alternative => write!(f, "|"),
            Where => write!(f, "where"),
            Match => write!(f, "match"),
            When => write!(f, "when"),
        }
    }
}

pub fn lexer() -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
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

    let operator = {
        let op_add = just('+').map(|c| Token::Op(c.to_string()));
        let op_sub = just('-').map(|c| Token::Op(c.to_string()));
        let op_mul = just('*').map(|c| Token::Op(c.to_string()));
        let op_div = just('/').map(|c| Token::Op(c.to_string()));
        let op_rem = just('%').map(|c| Token::Op(c.to_string()));
        let op_eq = just("==").map(|c| Token::Op(c.to_string()));
        let op_neq = just("!=").map(|c| Token::Op(c.to_string()));
        let op_less = just('<').map(|c| Token::Op(c.to_string()));
        let op_less_eq = just("<=").map(|c| Token::Op(c.to_string()));
        let op_more = just('>').map(|c| Token::Op(c.to_string()));
        let op_more_eq = just(">=").map(|c| Token::Op(c.to_string()));

        let op_bit_and = just('&').map(|c| Token::Op(c.to_string()));
        let op_logical_and = just("&&").map(|c| Token::Op(c.to_string()));
        let op_bit_or = just('|').map(|c| Token::Op(c.to_string()));
        let op_logical_or = just("||").map(|c| Token::Op(c.to_string()));

        let op_bit_xor = just('^').map(|c| Token::Op(c.to_string()));

        let op_join = just("++").map(|c| Token::Op(c.to_string()));

        op_add
            .or(op_sub)
            .or(op_mul)
            .or(op_div)
            .or(op_rem)
            .or(op_eq)
            .or(op_neq)
            .or(op_less)
            .or(op_less_eq)
            .or(op_more)
            .or(op_more_eq)
            .or(op_bit_and)
            .or(op_logical_and)
            .or(op_bit_or)
            .or(op_logical_or)
            .or(op_bit_xor)
            .or(op_join)
    };

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
    let kw_match = just("match").map(|_| Token::Match);
    let kw_when = just("when").map(|_| Token::When);
    let blockopen = just("{").map(|_| Token::BlockOpen);
    let blockclose = just("}").map(|_| Token::BlockClose);
    let listopen = just("[").map(|_| Token::ListOpen);
    let listclose = just("]").map(|_| Token::ListClose);
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
        .or(kw_match)
        .or(kw_when)
        .or(blockopen)
        .or(blockclose)
        .or(listopen)
        .or(listclose)
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
