// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::primitive::any;
use chumsky::primitive::end;
use chumsky::primitive::just;
use chumsky::primitive::none_of;
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
    Dollar,

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
            Comment(text) => write!(f, "# {text}"),
            Bool(x) => write!(f, "{x}"),
            Else => write!(f, "else"),
            Ident(s) => write!(f, "{s}"),
            Dollar => write!(f, "$"),
            If => write!(f, "if"),
            Then => write!(f, "then"),
            In => write!(f, "in"),
            Let => write!(f, "let"),
            Num(n) => write!(f, "{n}"),
            Str(s) => write!(f, "{s}"),
            Op(s) => write!(f, "{s}"),
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

    let operator = {
        // Put the "long" tokens first, so they get parsed first
        chumsky::primitive::choice((
            chumsky::primitive::choice((
                just("->"),
                just("=="),
                just("!="),
                just("<="),
                just(">="),
                just("&&"),
                just("||"),
                just("++"),
            )),
            chumsky::primitive::choice((
                just("<"),
                just(">"),
                just("&"),
                just("|"),
                just("("),
                just(")"),
                just(","),
                just("="),
                just(":"),
                just("."),
                just(";"),
                just("["),
                just("]"),
                just("{"),
                just("}"),
                just("|"),
                just("+"),
                just("-").then_ignore(just(">").not()),
                just("*"),
                just("/"),
                just("%"),
                just("^"),
            )),
        ))
        .map(Token::Op)
    };

    let ident = ident()
        .map(|ident: &str| match ident {
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
        })
        .or(just("$").map(|_| Token::Dollar));

    let comment = just("#")
        .then(any().and_is(just('\n').not()).repeated())
        .padded()
        .map(|(comment, ())| Token::Comment(comment));

    num.or(str_)
        .or(ident)
        .or(operator)
        .or(comment)
        .map_with_span(|t, s| (t, s))
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

#[cfg(test)]
mod tests {
    use chumsky::span::SimpleSpan;
    use chumsky::Parser;

    use super::Token;

    fn lex<'src>(code: &'src str) -> Vec<(Token<'src>, SimpleSpan)> {
        let lexer = crate::lexer();
        let lexer_res = lexer.parse(code);
        let has_errs = lexer_res.has_errors();
        let errs = lexer_res.errors();

        assert!(
            !has_errs,
            "No errors expected, but found: {:?}",
            errs.collect::<Vec<_>>()
        );
        lexer_res.output().unwrap().clone()
    }

    macro_rules! create_test_for_token {
        ($name:ident => $exp:ty) => {
            paste::paste! {
                #[test]
                fn [< test_lex_ $name >]() {
                    let code = stringify!($name);
                    let tokens = lex(code);
                    let t = tokens.first().unwrap();
                    assert!(matches!(t.0, $exp), "Expected {}, got: {:?}", stringify!($exp), t);
                }
            }
        };
    }

    create_test_for_token!(if => Token::If);
    create_test_for_token!(then => Token::Then);
    create_test_for_token!(else => Token::Else);
    create_test_for_token!(let => Token::Let);
    create_test_for_token!(in => Token::In);
    create_test_for_token!(where => Token::Where);
    create_test_for_token!(match => Token::Match);
    create_test_for_token!(when => Token::When);
    create_test_for_token!(type => Token::Type);
    create_test_for_token!(impl => Token::Impl);
    create_test_for_token!(enum => Token::Enum);
    create_test_for_token!(use => Token::Use);
    create_test_for_token!(pub => Token::Pub);
    create_test_for_token!(mod => Token::Mod);

    #[test]
    fn test_lex_arrow() {
        let code = "->";
        let tokens = lex(code);
        let arrow = tokens.first().unwrap();
        assert!(
            matches!(arrow.0, Token::Op("->")),
            "Expected Token::Arrow, got: {:?}",
            arrow
        );
    }
}
