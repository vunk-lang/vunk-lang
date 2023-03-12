// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::error::Simple;
use chumsky::select;
use chumsky::Parser;

use vunk_lexer::Token;

use crate::ast::name::TraitName;
use crate::ast::name::TypeName;
use crate::Spanned;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct WhereClause(pub Vec<Clause>);

impl WhereClause {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<WhereClause>, Error = Simple<Spanned<Token>>> + Clone
    {
        select! {
            (Token::Where, span) => ((), span)
        }
        .ignore_then(Clause::parser().repeated())
        .map(|clauses| {
            let span = std::ops::Range {
                start: clauses.first().map(|tpl| tpl.1.start).unwrap_or(0),
                end: clauses
                    .iter()
                    .rev()
                    .next()
                    .map(|tpl| tpl.1.end)
                    .unwrap_or(0),
            };
            let clauses = WhereClause(clauses.into_iter().map(|tpl| tpl.0).collect());
            (clauses, span)
        })
    }
}

fn plus_parser() -> impl Parser<Spanned<Token>, Spanned<()>, Error = Simple<Spanned<Token>>> + Clone
{
    select! {
        (Token::Ctrl('+'), span) => ((), span)
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Clause {
    pub target_name: TypeName,
    pub bounds: Vec<TraitName>,
}

impl Clause {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        TypeName::parser()
            .then_ignore({
                select! {
                    (Token::Declare, span) => ((), span)
                }
            })
            .then(TraitName::parser().separated_by(plus_parser()))
            .map(|((type_name, type_name_span), generic_types_list)| {
                let span = std::ops::Range {
                    start: type_name_span.start,
                    end: generic_types_list
                        .iter()
                        .rev()
                        .next()
                        .map(|tpl| tpl.1.end)
                        .unwrap_or(type_name_span.end),
                };

                let clause = Clause {
                    target_name: type_name,
                    bounds: generic_types_list.into_iter().map(|tpl| tpl.0).collect(),
                };

                (clause, span)
            })
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Generic {
    pub type_name: TypeName,
    pub where_clause: WhereClause,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_where_clause_parser() {
        let parser = WhereClause::parser();
        let tokens = vunk_lexer::lexer().parse("where Foo: Bar").unwrap();
        let parsed = parser.parse(tokens).unwrap();

        let clauses = parsed.0.0;
        assert_eq!(clauses.len(), 1);
        assert_eq!(clauses[0].target_name, TypeName(String::from("Foo")));
        assert_eq!(clauses[0].bounds.len(), 1);
        assert_eq!(clauses[0].bounds[0], TraitName(String::from("Bar")));
    }
}
