// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::prelude::Simple;
use chumsky::Parser;
use vunk_lexer::Token;

use crate::ast::decl::Decl;
use crate::ast::def::Def;
use crate::ast::ifelse::IfElse;
use crate::ast::letin::LetIns;
use crate::ast::literal::Literal;
use crate::ast::module::Module;
use crate::ast::name::VariableName;
use crate::ast::op::BinaryOp;
use crate::ast::op::UnaryOp;
use crate::Spanned;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Expr {
    Variable(VariableName),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Literal(Literal),
    LetIn(LetIns),
    IfElse(IfElse),
    Module(Module),
    Decl(Decl),
    Def(Def),
}

impl Expr {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        chumsky::recursive::recursive(|_tree| {
            let variable_parser = VariableName::parser().map(|(v, span)| (Expr::Variable(v), span));

            let unary_parser = UnaryOp::parser()
                .then(Expr::parser().map(|(e, span)| (Box::new(e), span)))
                .map(|((op, opspan), (ex, exspan))| {
                    let span = std::ops::Range {
                        start: opspan.start,
                        end: exspan.end,
                    };

                    let e = Expr::Unary(op, ex);
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

                    let e = Expr::Binary(op, exl, exr);
                    (e, span)
                });

            let literal_parser = Literal::parser()
                .map(|(lit, span)| {
                    let e = Expr::Literal(lit);
                    (e, span)
                });

            let letin_parser = LetIns::parser()
                .map(|(li, span)| {
                    let e = Expr::LetIn(li);
                    (e, span)
                });

            let ifelse_parser = IfElse::parser()
                .map(|(ie, span)| {
                    let e = Expr::IfElse(ie);
                    (e, span)
                });

            let module_parser = Module::parser()
                .map(|(m, span)| {
                    let e = Expr::Module(m);
                    (e, span)
                });

            let decl_parser = Decl::parser()
                .map(|(d, span)| {
                    let e = Expr::Decl(d);
                    (e, span)
                });

            let def_parser = Def::parser()
                .map(|(d, span)| {
                    let e = Expr::Def(d);
                    (e, span)
                });

            variable_parser
                .or(unary_parser)
                .or(binary_parser)
                .or(literal_parser)
                .or(letin_parser)
                .or(ifelse_parser)
                .or(module_parser)
                .or(decl_parser)
                .or(def_parser)
        })
    }
}
