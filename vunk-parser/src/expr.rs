// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::prelude::Rich;
use chumsky::primitive::any;
use chumsky::primitive::end;
use chumsky::primitive::just;
use chumsky::recovery::skip_then_retry_until;
use chumsky::select;
use chumsky::Parser;

use chumsky::span::SimpleSpan;
use vunk_lexer::Token;

use crate::generic::WhereClause;
use crate::literal::Float;
use crate::literal::Integer;
use crate::op::BinaryOp;
use crate::op::UnaryOp;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Expr<'src> {
    Bool(bool),
    Integer(Integer),
    Float(Float),
    Str(&'src str),
    List(Vec<Expr<'src>>),

    Ident(&'src str),

    Unary {
        op: UnaryOp,
        expr: Box<Expr<'src>>,
    },

    Binary {
        lhs: Box<Expr<'src>>,
        op: BinaryOp,
        rhs: Box<Expr<'src>>,
    },

    LetIn {
        exprs: Vec<Expr<'src>>,
    },

    IfElse {
        condition: Box<Expr<'src>>,
        tru: Box<Expr<'src>>,
        fals: Box<Expr<'src>>,
    },

    TypeDef {
        name: &'src str,
        whereclause: Option<WhereClause<'src>>,
        members: Vec<Expr<'src>>,
    },

    EnumDef {
        name: &'src str,
        variants: Vec<Expr<'src>>,
        whereclause: Option<WhereClause<'src>>,
    },

    Decl {
        ident: &'src str,
        decl_type: DeclType<'src>,
        whereclause: Option<WhereClause<'src>>,
    },

    Def {
        lhs: &'src str,
        rhs: DefRhs<'src>,
    },
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum DeclType<'src> {
    TypeName(&'src str),
    Func {
        args: Vec<DeclArg<'src>>,
        retty: &'src str,
    },
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct DeclArg<'src> {
    pub name: Option<&'src str>,
    pub ty: DeclType<'src>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct DefRhs<'src> {
    pub args: Vec<(&'src str, DeclType<'src>)>,
    pub expr: Box<Expr<'src>>,
}

type ParserInput<'tokens, 'src> =
    chumsky::input::SpannedInput<Token<'src>, SimpleSpan, &'tokens [(Token<'src>, SimpleSpan)]>;

impl Expr<'_> {
    fn parser<'tokens, 'src: 'tokens>() -> impl Parser<
        'tokens,
        ParserInput<'tokens, 'src>,
        Expr<'src>,
        chumsky::extra::Err<Rich<'tokens, Token<'src>, SimpleSpan>>,
    > + Clone {
        chumsky::recursive::recursive(|expr| {
            let bool_parser = select! {
                Token::Bool(true) => Expr::Bool(true),
                Token::Bool(false) => Expr::Bool(false),
            };

            let int_parser = {
                let numstr_parser = select! {
                    Token::Num(numstr) => numstr,
                };

                let i8_parser = numstr_parser.from_str::<i8>()
                    .map(|res| res.map(Integer::I8).unwrap());
                let i16_parser = numstr_parser.from_str::<i16>()
                    .map(|res| res.map(Integer::I16).unwrap());
                let i32_parser = numstr_parser.from_str::<i32>()
                    .map(|res| res.map(Integer::I32).unwrap());
                let i64_parser = numstr_parser.from_str::<i64>()
                    .map(|res| res.map(Integer::I64).unwrap());
                let u8_parser = numstr_parser.from_str::<u8>()
                    .map(|res| res.map(Integer::U8).unwrap());
                let u16_parser = numstr_parser.from_str::<u16>()
                    .map(|res| res.map(Integer::U16).unwrap());
                let u32_parser = numstr_parser.from_str::<u32>()
                    .map(|res| res.map(Integer::U32).unwrap());
                let u64_parser = numstr_parser.from_str::<u64>()
                    .map(|res| res.map(Integer::U64).unwrap());

                i8_parser
                    .or(i16_parser)
                    .or(i32_parser)
                    .or(i64_parser)
                    .or(u8_parser)
                    .or(u16_parser)
                    .or(u32_parser)
                    .or(u64_parser)
                    .map(Expr::Integer)
            };

            // TODO: Support floats
            // let float_parser = select! {
            // };

            let str_parser = select! {
                Token::Str(s) => Expr::Str(s),
            };

            let list_parser = {
                let left_br = just(Token::Ctrl('[')).to(());
                let right_br = just(Token::Ctrl(']')).to(());

                expr.clone().delimited_by(left_br, right_br)
            };

            let ident_parser = select! {
                Token::Ident(s) => Expr::Ident(s),
            };

            let unary_parser = {
                let unary_op_parser = {
                    let binary = just(Token::Op("~")).to(UnaryOp::BinaryNot);
                    let logical = just(Token::Op("!")).to(UnaryOp::LogicalNot);

                    binary.or(logical)
                };

                unary_op_parser
                    .then(expr.clone())
                    .map(|(op, expr)| {
                        Expr::Unary {
                            op,
                            expr: Box::new(expr),
                        }
                    })
            };

            let binary_parser = {
                let binary_op_parser = just(Token::Op("+"))
                    .to(BinaryOp::Add)
                    .or(just(Token::Op("-")).to(BinaryOp::Sub))
                    .or(just(Token::Op("*")).to(BinaryOp::Mul))
                    .or(just(Token::Op("/")).to(BinaryOp::Div))
                    .or(just(Token::Op("%")).to(BinaryOp::Rem))
                    .or(just(Token::Op("==")).to(BinaryOp::Eq))
                    .or(just(Token::Op("!=")).to(BinaryOp::NotEq))
                    .or(just(Token::Op("<")).to(BinaryOp::Less))
                    .or(just(Token::Op("<=")).to(BinaryOp::LessEq))
                    .or(just(Token::Op(">")).to(BinaryOp::More))
                    .or(just(Token::Op(">=")).to(BinaryOp::MoreEq))
                    .or(just(Token::Op("&")).to(BinaryOp::BitAnd))
                    .or(just(Token::Op("&&")).to(BinaryOp::LogicalAnd))
                    .or(just(Token::Op("|")).to(BinaryOp::BitOr))
                    .or(just(Token::Op("||")).to(BinaryOp::LogicalOr))
                    .or(just(Token::Op("^")).to(BinaryOp::BitXor))
                    .or(just(Token::Op("++")).to(BinaryOp::Join));

                expr.clone()
                    .then(binary_op_parser)
                    .then(expr.clone())
                    .map(|((lhs, op), rhs)| {
                        Expr::Binary {
                            op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        }
                    })
            };

            bool_parser
                .or(int_parser)
                .or(str_parser)
                .or(list_parser)
                .or(ident_parser)
                .or(unary_parser)
                .or(binary_parser)
        })
    }
}

#[cfg(test)]
mod tests {
    use chumsky::{Parser, prelude::Input};
    use super::*;

    fn ast_has_no_errs(code: &str) {
        let res = vunk_lexer::lexer().parse(code);
        assert!(!res.has_errors());
        let tokens = res.into_output().unwrap();
        let tokens = tokens.as_slice().spanned((code.len()..code.len()).into());
        let res = Expr::parser().parse(tokens);
        assert!(!res.has_errors(), "No errors expected, but found: {:?}", res.errors().collect::<Vec<_>>());
    }

    #[test]
    fn test_ast_bool() {
        ast_has_no_errs("false");
    }

    #[test]
    fn test_ast_int() {
        ast_has_no_errs("123");
    }

    #[test]
    fn test_ast_ident() {
        ast_has_no_errs("foobar");
    }
}
