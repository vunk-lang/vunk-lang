// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::Parser;
use chumsky::error::Simple;
use chumsky::select;
use vunk_lexer::Token;

use crate::Spanned;
use crate::ast::expr::Expr;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct IfElse {
    pub condition: Box<Expr>,
    pub tru: Box<Expr>,
    pub fals: Box<Expr>,
}

impl IfElse {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        kw_if()
            .ignore_then(Expr::parser().map(|(e, span)| (Box::new(e), span)))
            .then_ignore(kw_then())
            .then(Expr::parser().map(|(e, span)| (Box::new(e), span)))
            .then_ignore(kw_else())
            .then(Expr::parser().map(|(e, span)| (Box::new(e), span)))
            .map(|(((cond, condspan), (ife, _ifespan)), (els, elsspan))| {
                let span = std::ops::Range {
                    start: condspan.start,
                    end: elsspan.end,
                };

                let ifelse = IfElse {
                    condition: cond,
                    tru: ife,
                    fals: els,
                };

                (ifelse, span)
            })
    }
}

fn kw_if() -> impl Parser<Spanned<Token>, Spanned<()>, Error = Simple<Spanned<Token>>> + Clone {
    select! {
        (Token::If, span) => ((), span)
    }
}

fn kw_then() -> impl Parser<Spanned<Token>, Spanned<()>, Error = Simple<Spanned<Token>>> + Clone {
    select! {
        (Token::Then, span) => ((), span)
    }
}

fn kw_else() -> impl Parser<Spanned<Token>, Spanned<()>, Error = Simple<Spanned<Token>>> + Clone {
    select! {
        (Token::Else, span) => ((), span)
    }
}
