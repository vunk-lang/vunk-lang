// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::Parser;
use chumsky::error::Simple;
use vunk_lexer::Token;

use crate::Spanned;

use super::name::TypeName;
use super::util::separator;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Module(pub Vec<TypeName>);

impl Module {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        TypeName::parser().separated_by(separator())
            .map(|typelist| {
                let span = std::ops::Range {
                    start: typelist.first().map(|(_, span)| span.start).unwrap_or(0),
                    end: typelist.iter().rev().next().map(|(_, span)| span.end).unwrap_or(0),
                };

                let m = Module(typelist.into_iter().map(|(tn, _)| tn).collect());

                (m, span)
            })
    }
}


