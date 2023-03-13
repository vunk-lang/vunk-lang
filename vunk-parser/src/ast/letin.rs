// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::error::Simple;
use chumsky::select;
use chumsky::Parser;

use crate::ast::decl::Decl;
use crate::ast::def::Def;
use crate::ast::expr::Expr;

use vunk_lexer::Token;

use crate::Spanned;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct LetIns {
    pub items: Vec<LetIn>,
    pub expr: Box<Expr>,
}

impl LetIns {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<LetIns>, Error = Simple<Spanned<Token>>> + Clone {
        select! {
            (Token::Let, span) => (Token::Let, span)
        }
        .ignore_then(let_elements_parser())
        .then_ignore(select! {
            (Token::In, span) => (Token::In, span)
        })
    }
}

fn let_elements_parser(
) -> impl Parser<Spanned<Token>, Spanned<LetIns>, Error = Simple<Spanned<Token>>> + Clone {
    LetIn::parser()
        .repeated()
        .then(crate::ast::expr::Expr::parser())
        .map_with_span(|(items, (expr, _expr_span)), span| {
            let lis = LetIns {
                // TODO: Ignoring items spans here for now
                items: items.into_iter().map(|tpl| tpl.0).collect(),
                expr: Box::new(expr),
            };

            (lis, span)
        })
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum LetIn {
    Decl(Decl),
    Def(Def),
}

impl LetIn {
    fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<LetIn>, Error = Simple<Spanned<Token>>> + Clone {
        let decl_parser = Decl::parser().map(|(decl, decl_span)| (LetIn::Decl(decl), decl_span));
        let def_parser = Def::parser().map(|(decl, decl_span)| (LetIn::Def(decl), decl_span));
        decl_parser.or(def_parser)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::expr::Expr;

    use super::*;

    #[test]
    fn test_letins_parser() {
        let code = r#"
            let
                foo = bar;
            in
            foo;
        "#;

        let parser = LetIns::parser();
        let tokens = vunk_lexer::lexer().parse(code).unwrap();
        let parsed = parser.parse(tokens).unwrap();

        let letins = parsed.0;

        assert_eq!(letins.items.len(), 1);
        assert!(matches!(letins.items[0], LetIn::Def(_)));
        let def = if let LetIn::Def(def) = &letins.items[0] {
            def
        } else {
            unreachable!()
        };

        assert_eq!(def.lhs.0, "foo");

        if let Expr::Variable(var) = &*def.rhs.expr {
            assert_eq!(var.0, "bar");

            if let Expr::Variable(var) = *letins.expr {
                assert_eq!(var.0, "foo");
            } else {
                panic!("'in' expression of let-in is not a variable")
            }
        } else {
            panic!("RHS of assignment is not a variable")
        };
    }
}
