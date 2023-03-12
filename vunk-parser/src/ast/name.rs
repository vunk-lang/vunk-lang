// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::error::Simple;
use chumsky::select;
use chumsky::Parser;
use vunk_lexer::Token;

use crate::Spanned;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct VariableName(pub String);

impl VariableName {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<VariableName>, Error = Simple<Spanned<Token>>> + Clone
    {
        chumsky::select! {
            (Token::Ident(s), span) => (VariableName(s), span),
        }
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypeName(pub String);

impl TypeName {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<TypeName>, Error = Simple<Spanned<Token>>> + Clone
    {
        chumsky::select! {
            (Token::Ident(s), span) => (TypeName(s), span),
        }
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypePath(pub Vec<TypeName>);

impl TypePath {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<TypePath>, Error = Simple<Spanned<Token>>> + Clone
    {
        TypeName::parser()
            .separated_by(select! {
                (Token::Ctrl('.'), span) => ((), span)
            })
            .map(|list| {
                let span = std::ops::Range {
                    start: list.get(0).map(|r| r.1.start).unwrap_or(0),
                    end: list.iter().rev().next().map(|r| r.1.end).unwrap_or(0),
                };

                let typelist = list.into_iter().map(|tpl| tpl.0).collect();
                (TypePath(typelist), span)
            })
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TraitName(pub String);

impl TraitName {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<TraitName>, Error = Simple<Spanned<Token>>> + Clone
    {
        chumsky::select! {
            (Token::Ident(s), span) => (TraitName(s), span),
        }
    }
}
