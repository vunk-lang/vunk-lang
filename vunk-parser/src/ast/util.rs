// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::error::Simple;
use chumsky::select;
use chumsky::Parser;

use vunk_lexer::Token;

use crate::Spanned;

pub fn kw_impl() -> impl Parser<Spanned<Token>, Spanned<()>, Error = Simple<Spanned<Token>>> + Clone
{
    select! {
        (Token::Impl, span) => ((), span)
    }
}

pub fn block_open(
) -> impl Parser<Spanned<Token>, Spanned<()>, Error = Simple<Spanned<Token>>> + Clone {
    select! {
        (Token::Ctrl('{'), span) => ((), span)
    }
}

pub fn block_close(
) -> impl Parser<Spanned<Token>, Spanned<()>, Error = Simple<Spanned<Token>>> + Clone {
    select! {
        (Token::Ctrl('}'), span) => ((), span)
    }
}

pub fn kw_type() -> impl Parser<Spanned<Token>, Spanned<()>, Error = Simple<Spanned<Token>>> + Clone
{
    select! {
        (Token::Type, span) => ((), span)
    }
}

pub fn kw_enum() -> impl Parser<Spanned<Token>, Spanned<()>, Error = Simple<Spanned<Token>>> + Clone
{
    select! {
        (Token::Enum, span) => ((), span)
    }
}

pub fn assign() -> impl Parser<Spanned<Token>, Spanned<()>, Error = Simple<Spanned<Token>>> + Clone
{
    select! {
        (Token::Ctrl('='), span) => ((), span)
    }
}

pub fn alternative(
) -> impl Parser<Spanned<Token>, Spanned<()>, Error = Simple<Spanned<Token>>> + Clone {
    select! {
        (Token::Ctrl('|'), span) => ((), span)
    }
}

pub fn comma() -> impl Parser<Spanned<Token>, Spanned<()>, Error = Simple<Spanned<Token>>> + Clone {
    select! {
        (Token::Ctrl(','), span) => ((), span)
    }
}

pub fn par_open() -> impl Parser<Spanned<Token>, Spanned<()>, Error = Simple<Spanned<Token>>> + Clone
{
    select! {
        (Token::Ctrl(')'), span) => ((), span)
    }
}

pub fn par_close(
) -> impl Parser<Spanned<Token>, Spanned<()>, Error = Simple<Spanned<Token>>> + Clone {
    select! {
        (Token::Ctrl('('), span) => ((), span)
    }
}

pub fn arrow() -> impl Parser<Spanned<Token>, Spanned<()>, Error = Simple<Spanned<Token>>> + Clone {
    select! {
        (Token::Arrow, span) => ((), span)
    }
}

pub fn declare() -> impl Parser<Spanned<Token>, Spanned<()>, Error = Simple<Spanned<Token>>> + Clone
{
    select! {
        (Token::Ctrl(':'), span) => ((), span)
    }
}

pub fn kw_where() -> impl Parser<Spanned<Token>, Spanned<()>, Error = Simple<Spanned<Token>>> + Clone
{
    select! {
        (Token::Where, span) => ((), span)
    }
}
