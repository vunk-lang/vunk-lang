// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::primitive::any;
use chumsky::primitive::end;
use chumsky::primitive::just;
use chumsky::primitive::none_of;
use chumsky::primitive::one_of;
use chumsky::recovery::skip_then_retry_until;
use chumsky::text;
use chumsky::text::ident;
use chumsky::IterParser;
use chumsky::Parser;

type Span = chumsky::span::SimpleSpan<usize>;
type Spanned<T> = (T, Span);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token<'src> {
    Ident(&'src str),

    Arrow,
    Ctrl(char),
    Op(&'src str),

    Num(&'src str),
    Str(&'src str),

    If,
    Then,
    Else,

    Let,
    In,

    Where,
    Match,
    When,
    Type,
    Impl,
    Enum,

    Bool(bool),

    Use,
    Pub,
    Mod,

    Comment(&'src str),
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Token::*;

        match self {
            Comment(text) => write!(f, "# {}", text),
            Arrow => write!(f, "->"),
            Bool(x) => write!(f, "{}", x),
            Ctrl(c) => write!(f, "{}", c),
            Else => write!(f, "else"),
            Ident(s) => write!(f, "{}", s),
            If => write!(f, "if"),
            Then => write!(f, "then"),
            In => write!(f, "in"),
            Let => write!(f, "let"),
            Num(n) => write!(f, "{}", n),
            Str(s) => write!(f, "{}", s),
            Op(s) => write!(f, "{}", s),
            Use => write!(f, "use"),
            Pub => write!(f, "pub"),
            Where => write!(f, "where"),
            Match => write!(f, "match"),
            When => write!(f, "when"),
            Type => write!(f, "type"),
            Impl => write!(f, "impl"),
            Enum => write!(f, "enum"),
            Mod => write!(f, "mod"),
        }
    }
}

pub type LexerError<'src> = chumsky::extra::Err<chumsky::error::Rich<'src, char, Span>>;

pub fn lexer<'src>() -> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, LexerError<'src>> {
    let num = text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .slice()
        .map(Token::Num);

    // A parser for strings
    let str_ = none_of("\\\"")
        .ignored()
        .repeated()
        .slice()
        .delimited_by(just('"'), just('"'))
        .map(Token::Str);

    // A parser for control characters (delimiters, semicolons, etc.)
    let ctrl = one_of("(),=:.;[]{}|").map(Token::Ctrl);

    let operator = {
        let op_add = just("+").map(|c| Token::Op(c));
        let op_sub = just("-").map(|c| Token::Op(c));
        let op_mul = just("*").map(|c| Token::Op(c));
        let op_div = just("/").map(|c| Token::Op(c));
        let op_rem = just("%").map(|c| Token::Op(c));
        let op_eq = just("==").map(|c| Token::Op(c));
        let op_neq = just("!=").map(|c| Token::Op(c));
        let op_less = just("<").map(|c| Token::Op(c));
        let op_less_eq = just("<=").map(|c| Token::Op(c));
        let op_more = just(">").map(|c| Token::Op(c));
        let op_more_eq = just(">=").map(|c| Token::Op(c));

        let op_bit_and = just("&").map(|c| Token::Op(c));
        let op_logical_and = just("&&").map(|c| Token::Op(c));
        let op_bit_or = just("|").map(|c| Token::Op(c));
        let op_logical_or = just("||").map(|c| Token::Op(c));

        let op_bit_xor = just("^").map(|c| Token::Op(c));

        let op_join = just("++").map(|c| Token::Op(c));

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

    let ident = ident().map(|ident: &str| match ident {
        "->" => Token::Arrow,
        "if" => Token::If,
        "then" => Token::Then,
        "else" => Token::Else,
        "let" => Token::Let,
        "in" => Token::In,
        "where" => Token::Where,
        "match" => Token::Match,
        "when" => Token::When,
        "type" => Token::Type,
        "impl" => Token::Impl,
        "enum" => Token::Enum,
        "use" => Token::Use,
        "pub" => Token::Pub,
        "mod" => Token::Mod,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),

        _ => Token::Ident(ident),
    });

    let comment = just("#")
        .then(any().and_is(just('\n').not()).repeated())
        .padded()
        .map(|(comment, ())| Token::Comment(comment));

    num.or(str_)
        .or(ctrl)
        .or(operator)
        .or(ident)
        .or(comment)
        .map_with_span(|t, s| (t, s))
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}
