// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::error::Simple;
use chumsky::select;
use chumsky::Parser;

use vunk_lexer::Token;

use crate::Spanned;

use super::literal::Literal;
use super::name::VariableName;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct UnaryOp {
    pub operator: UnaryOperator,
    pub rhs: Box<OpRhs>,
}

impl UnaryOp {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        UnaryOperator::parser().then(OpRhs::parser()).map(
            |((unary_op, unary_op_span), (rhs, rhs_span))| {
                let span = std::ops::Range {
                    start: unary_op_span.start,
                    end: rhs_span.end,
                };

                let e = UnaryOp {
                    operator: unary_op,
                    rhs: Box::new(rhs),
                };

                (e, span)
            },
        )
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum UnaryOperator {
    BinaryNot,
    LogicalNot,
}

impl UnaryOperator {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        chumsky::select! {
            (Token::Op(op), span) => {
                let span: std::ops::Range<usize> = span;

                match op.as_ref() {
                    "~" => (Self::BinaryNot, span),
                    "!" => (Self::LogicalNot, span),
                    _ => {
                        use chumsky::error::Error;

                        let op = Token::Op(op);
                        let found: Option<Spanned<Token>> = Some((op, span.clone()));
                        return Err(chumsky::error::Simple::expected_input_found(span, None, found))
                    },
                }
            }
        }
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct BinaryOp {
    pub lhs: Box<OpLhs>,
    pub operator: BinaryOperator,
    pub rhs: Box<OpRhs>,
}

impl BinaryOp {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        OpLhs::parser()
            .then(BinaryOperator::parser())
            .then(OpRhs::parser())
            .map(
                |(((lhs, lhs_span), (binary_op, _binary_op_span)), (rhs, rhs_span))| {
                    let span = std::ops::Range {
                        start: lhs_span.start,
                        end: rhs_span.end,
                    };

                    let e = BinaryOp {
                        lhs: Box::new(lhs),
                        operator: binary_op,
                        rhs: Box::new(rhs),
                    };

                    (e, span)
                },
            )
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    NotEq,
    Less,
    LessEq,
    More,
    MoreEq,
    BitAnd,
    LogicalAnd,
    BitOr,
    LogicalOr,
    BitXor,
    Join,
}

impl BinaryOperator {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        chumsky::select! {
            (Token::Op(op), span) => {
                let span: std::ops::Range<usize> = span;
                match op.as_ref() {
                    "+" => (Self::Add, span),
                    "-" => (Self::Sub, span),
                    "*" => (Self::Mul, span),
                    "/" => (Self::Div, span),
                    "%" => (Self::Rem, span),
                    "==" => (Self::Eq, span),
                    "!=" => (Self::NotEq, span),
                    "<"=> (Self::Less, span),
                    "<=" => (Self::LessEq, span),
                    ">"=> (Self::More, span),
                    ">=" => (Self::MoreEq, span),

                    "&"=> (Self::BitAnd, span),
                    "&&" => (Self::LogicalAnd, span),
                    "|"=> (Self::BitOr, span),
                    "||" => (Self::LogicalOr, span),

                    "^"=> (Self::BitXor, span),

                    "++" => (Self::Join, span),

                    // TODO: Make nice
                    _ => {
                        use chumsky::error::Error;

                        let op = Token::Op(op);
                        let found: Option<Spanned<Token>> = Some((op, span.clone()));

                        return Err(chumsky::error::Simple::expected_input_found(span, None, found))
                    },
                }
            }
        }
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum OpLhs {
    Variable(VariableName),
    Unary(UnaryOp, Box<OpRhs>),
    Binary(BinaryOp, Box<OpLhs>, Box<OpRhs>),
    Literal(Literal),
}

impl OpLhs {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        chumsky::recursive::recursive(|_| {
            let variable_parser = VariableName::parser().map(|(v, span)| (Self::Variable(v), span));

            let unary_parser = UnaryOp::parser()
                .then(OpRhs::parser().map(|(e, span)| (Box::new(e), span)))
                .map(|((op, opspan), (ex, exspan))| {
                    let span = std::ops::Range {
                        start: opspan.start,
                        end: exspan.end,
                    };

                    let e = Self::Unary(op, ex);
                    (e, span)
                });

            let binary_parser = OpLhs::parser()
                .map(|(e, span)| (Box::new(e), span))
                .then(BinaryOp::parser())
                .then(OpRhs::parser().map(|(e, span)| (Box::new(e), span)))
                .map(|(((exl, _exlspan), (op, _opspan)), (exr, exrspan))| {
                    let span = std::ops::Range {
                        start: exrspan.start,
                        end: exrspan.end,
                    };

                    let e = Self::Binary(op, exl, exr);
                    (e, span)
                });

            let literal_parser = Literal::parser().map(|(lit, span)| {
                let e = Self::Literal(lit);
                (e, span)
            });

            variable_parser
                .or(unary_parser)
                .or(binary_parser)
                .or(literal_parser)
        })
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum OpRhs {
    Variable(VariableName),
    Unary(UnaryOp),
    Binary(BinaryOp),
    Literal(Literal),
}

impl OpRhs {
    pub fn parser(
    ) -> impl Parser<Spanned<Token>, Spanned<Self>, Error = Simple<Spanned<Token>>> + Clone {
        chumsky::recursive::recursive(|_| {
            let variable_parser = VariableName::parser().map(|(v, span)| (Self::Variable(v), span));

            let unary_parser = UnaryOp::parser().map(|(op, span)| {
                let e = Self::Unary(op);
                (e, span)
            });

            let binary_parser = BinaryOp::parser().map(|(op, span)| {
                let e = Self::Binary(op);
                (e, span)
            });

            let literal_parser = Literal::parser().map(|(lit, span)| {
                let e = Self::Literal(lit);
                (e, span)
            });

            variable_parser
                .or(unary_parser)
                .or(binary_parser)
                .or(literal_parser)
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::literal::{Integer, IntegerValue};

    use super::*;

    #[test]
    fn test_unaryop_parser() {
        let code = r#"
            !foo
        "#;

        let parser = UnaryOp::parser();
        let tokens = vunk_lexer::lexer().parse(code).unwrap();
        let parsed = parser.parse(tokens).unwrap();

        let UnaryOp { operator, rhs } = parsed.0;

        assert_eq!(operator, UnaryOperator::LogicalNot);
        assert!(matches!(*rhs, OpRhs::Variable(VariableName(_))));
        let OpRhs::Variable(VariableName(name)) = *rhs else {
            unreachable!()
        };

        assert_eq!(name, "foo");
    }


    #[test]
    fn test_binaryop_parser() {
        let code = r#"
            1 + 2
        "#;

        let parser = BinaryOp::parser();
        let tokens = vunk_lexer::lexer().parse(code).unwrap();
        let parsed = parser.parse(tokens).unwrap();

        let BinaryOp { operator, lhs, rhs } = parsed.0;

        assert_eq!(operator, BinaryOperator::Add);

        assert!(matches!(*lhs, OpLhs::Literal(Literal::Integer(_))));
        let OpLhs::Literal(Literal::Integer(a)) = *lhs else {
            unreachable!()
        };
        assert_eq!(a, Integer { value: IntegerValue::I8(1) });

        assert!(matches!(*rhs, OpRhs::Literal(Literal::Integer(_))));
        let OpRhs::Literal(Literal::Integer(b)) = *rhs else {
            unreachable!()
        };
        assert_eq!(b, Integer { value: IntegerValue::I8(2) });
    }
}
