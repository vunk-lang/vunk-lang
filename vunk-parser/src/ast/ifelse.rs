// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::error::Simple;
use chumsky::select;
use chumsky::Parser;
use vunk_lexer::Token;

use crate::ast::expr::Expr;
use crate::Spanned;

use super::literal::Literal;
use super::name::VariableName;
use super::op::{BinaryOp, UnaryOp};

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct IfElse {
    pub condition: Condition,
    pub tru: Box<Expr>,
    pub fals: Box<Expr>,
}

impl IfElse {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        kw_if()
            .ignore_then(Condition::parser())
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

/// Expr without LetIn, IfElse, Module, Decl, Def
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Condition {
    Variable(VariableName),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Literal(Literal),
}

impl Condition {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        let variable_parser =
            VariableName::parser().map(|(v, span)| (Condition::Variable(v), span));

        let unary_parser = UnaryOp::parser()
            .then(Expr::parser().map(|(e, span)| (Box::new(e), span)))
            .map(|((op, opspan), (ex, exspan))| {
                let span = std::ops::Range {
                    start: opspan.start,
                    end: exspan.end,
                };

                let e = Condition::Unary(op, ex);
                (e, span)
            });

        let binary_parser = Expr::parser()
            .map(|(e, span)| (Box::new(e), span))
            .then(BinaryOp::parser())
            .then(Expr::parser().map(|(e, span)| (Box::new(e), span)))
            .map(|(((exl, _exlspan), (op, _opspan)), (exr, exrspan))| {
                let span = std::ops::Range {
                    start: exrspan.start,
                    end: exrspan.end,
                };

                let e = Condition::Binary(op, exl, exr);
                (e, span)
            });

        let literal_parser = Literal::parser().map(|(lit, span)| {
            let e = Condition::Literal(lit);
            (e, span)
        });

        variable_parser
            .or(unary_parser)
            .or(binary_parser)
            .or(literal_parser)
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
