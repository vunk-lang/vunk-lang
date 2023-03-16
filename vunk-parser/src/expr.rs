// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::prelude::Rich;
use chumsky::primitive::any;
use chumsky::primitive::end;
use chumsky::primitive::just;
use chumsky::recovery::skip_then_retry_until;
use chumsky::select;
use chumsky::IterParser;
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
        sub: Box<Expr<'src>>,
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
        generics: Option<Vec<&'src str>>,
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
    TypeName(TypeName<'src>),
    Func {
        args: Vec<DeclArg<'src>>,
        retty: TypeName<'src>,
    },
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypeName<'src> {
    name: &'src str,
    generics: Option<Vec<&'src str>>,
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

                let i8_parser = numstr_parser
                    .from_str::<i8>()
                    .map(|res| res.map(Integer::I8).unwrap());
                let i16_parser = numstr_parser
                    .from_str::<i16>()
                    .map(|res| res.map(Integer::I16).unwrap());
                let i32_parser = numstr_parser
                    .from_str::<i32>()
                    .map(|res| res.map(Integer::I32).unwrap());
                let i64_parser = numstr_parser
                    .from_str::<i64>()
                    .map(|res| res.map(Integer::I64).unwrap());
                let u8_parser = numstr_parser
                    .from_str::<u8>()
                    .map(|res| res.map(Integer::U8).unwrap());
                let u16_parser = numstr_parser
                    .from_str::<u16>()
                    .map(|res| res.map(Integer::U16).unwrap());
                let u32_parser = numstr_parser
                    .from_str::<u32>()
                    .map(|res| res.map(Integer::U32).unwrap());
                let u64_parser = numstr_parser
                    .from_str::<u64>()
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

            fn str_parser<'tokens, 'src: 'tokens>() -> impl Parser<
                'tokens,
                ParserInput<'tokens, 'src>,
                Expr<'src>,
                chumsky::extra::Err<Rich<'tokens, Token<'src>, SimpleSpan>>,
            > + Clone {
                select! {
                    Token::Str(s) => Expr::Str(s),
                }
            }

            let list_parser = {
                let left_br = just(Token::Ctrl('[')).to(());
                let right_br = just(Token::Ctrl(']')).to(());

                expr.clone().delimited_by(left_br, right_br)
            };

            fn ident_parser<'tokens, 'src: 'tokens>() -> impl Parser<
                'tokens,
                ParserInput<'tokens, 'src>,
                &'src str,
                chumsky::extra::Err<Rich<'tokens, Token<'src>, SimpleSpan>>,
            > + Clone {
                select! {
                    Token::Ident(s) => s,
                }
            }

            let unary_parser = {
                let unary_op_parser = {
                    let binary = just(Token::Op("~")).to(UnaryOp::BinaryNot);
                    let logical = just(Token::Op("!")).to(UnaryOp::LogicalNot);

                    binary.or(logical)
                };

                unary_op_parser
                    .then(expr.clone())
                    .map(|(op, expr)| Expr::Unary {
                        op,
                        expr: Box::new(expr),
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
                    .map(|((lhs, op), rhs)| Expr::Binary {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    })
            };

            // Parser for a declaration
            //
            // ## Short example
            //
            // ```
            // bar: I8;
            // ```
            //
            // Declares `bar` to be of type `I8`
            //
            // ## Full example
            //
            // ```
            // foo A B = (A, B) -> A
            //     where A: Add I8 + Debug
            //           B: Into I8
            //           ;
            // ```
            //
            // Declares a variable `foo` generic over `A` and `B`
            // to be a function with arguments of type `A` and `B`
            // returning an instance of `A`
            // with bounds:
            //     * `A` must implement the generic trait `Add` parametrized with `I8`
            //     * `A` must implement `Debug`
            //     * `B` must implement the generic trait `Into` parametrized with `I8`
            //
            let decl_parser = {
                chumsky::recursive::recursive(|decl_parser| {
                    let decl_name_parser = ident_parser();
                    let generic_names_parser = ident_parser().repeated();
                    let assign_parser = just(Token::Ctrl('='));
                    let arrow = just(Token::Arrow);

                    // Parser for a Type
                    //
                    //  The name of the type
                    //    v
                    // `Result T E`
                    //         ^ ^
                    //         Generics of the type
                    fn type_parser<'tokens, 'src: 'tokens>() -> impl Parser<
                        'tokens,
                        ParserInput<'tokens, 'src>,
                        TypeName<'src>,
                        chumsky::extra::Err<Rich<'tokens, Token<'src>, SimpleSpan>>,
                    > + Clone {
                        ident_parser()
                            .then(ident_parser().repeated().collect().or_not())
                            .map(|(ident, generics)| TypeName {
                                name: ident,
                                generics,
                            })
                    }

                    // Parser for a list of arguments
                    //
                    // Like: `(A, B)`
                    // Or: `(A B C, D)` (`B` and `C` are generics for `A`
                    //
                    // In a declaration, we do not have argument names
                    let args_parser = {
                        let open_par = just(Token::Ctrl('('));
                        let close_par = just(Token::Ctrl(')'));
                        let comma = just(Token::Ctrl(','));

                        type_parser()
                            .separated_by(comma)
                            .delimited_by(open_par, close_par)
                    };

                    // Parser for a function signature
                    //
                    // E.G.: `(A) -> A`
                    // E.G.: `(A B C) -> A B C`
                    //  (`B` and `C` are generics for `A` here)
                    // let func_parser = args_parser.then_ignore(arrow).then(type_parser());
                    fn func_parser<'tokens, 'src: 'tokens>() -> impl Parser<
                        'tokens,
                        ParserInput<'tokens, 'src>,
                        DeclType<'src>,
                        chumsky::extra::Err<Rich<'tokens, Token<'src>, SimpleSpan>>,
                    > + Clone {
                        let arrow = just(Token::Arrow);
                        fn args_parser<'tokens, 'src: 'tokens>() -> impl Parser<
                            'tokens,
                            ParserInput<'tokens, 'src>,
                            Vec<DeclArg<'src>>,
                            chumsky::extra::Err<Rich<'tokens, Token<'src>, SimpleSpan>>,
                        > + Clone {
                            let open_par = just(Token::Ctrl('('));
                            let close_par = just(Token::Ctrl(')'));
                            let comma = just(Token::Ctrl(','));

                            open_par
                                .ignore_then(type_parser().separated_by(comma))
                                .then_ignore(close_par)
                                .map(|()| {
                                })
                        }

                        args_parser
                            .then_ignore(arrow)
                            .then(type_parser())
                            .map(|(args, retty)| DeclType::Func { args, retty })
                    }

                    // Parser for `where` clause
                    //
                    // ```
                    // where A: FooT + BarT,
                    //       B: BarT
                    // ```
                    let whereclause = {
                        let generic_name_parser = ident_parser();

                        // A trait name is written down like a type name:
                        //
                        // `T A B` (`A` and `B` are generics for `T`)
                        let trait_parser = type_parser();

                        just(Token::Where).ignore_then({
                            generic_name_parser
                                .then_ignore(assign_parser.clone())
                                .then({
                                    // bounds
                                    trait_parser.separated_by(just(Token::Ctrl('+')))
                                })
                                .separated_by(just(Token::Ctrl(',')))
                                .repeated()
                        })
                    };

                    decl_name_parser
                        .then(generic_names_parser.repeated().or_not())
                        .then_ignore(assign_parser)
                        .then({
                            // The type of a declaration is either a function, where we need args
                            // and return type and so on,
                            // or a concrete type (which can be generic)
                            func_parser().or(type_parser())
                        })
                        // Both a function decl and a normal variable decl can be generic
                        .then(whereclause.or_not())
                        .then_ignore(just(Token::Ctrl(';')))
                        .map(|()| {
                            // Expr::Decl {
                            //     ident: decl_name,
                            //     generics,
                            //     decl_type: DeclType {},
                            //     whereclause,
                            // }
                        })
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
    use super::*;
    use chumsky::{prelude::Input, Parser};

    fn ast_has_no_errs(code: &str) {
        let res = vunk_lexer::lexer().parse(code);
        assert!(!res.has_errors());
        let tokens = res.into_output().unwrap();
        let tokens = tokens.as_slice().spanned((code.len()..code.len()).into());
        let res = Expr::parser().parse(tokens);
        assert!(
            !res.has_errors(),
            "No errors expected, but found: {:?}",
            res.errors().collect::<Vec<_>>()
        );
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
