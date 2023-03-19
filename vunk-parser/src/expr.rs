// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::prelude::Rich;
use chumsky::primitive::just;
use chumsky::select;
use chumsky::IterParser;
use chumsky::Parser;

use chumsky::span::SimpleSpan;
use vunk_lexer::Token;

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
        whereclause: Option<Vec<Generic<'src>>>,
        members: Vec<Expr<'src>>,
    },

    EnumDef {
        name: &'src str,
        variants: Vec<Expr<'src>>,
        whereclause: Option<Vec<Generic<'src>>>,
    },

    Decl {
        ident: &'src str,
        decl_type: DeclType<'src>,
        generics: Option<Vec<&'src str>>,
        whereclause: Option<Vec<Generic<'src>>>,
    },

    Def {
        lhs: &'src str,
        generics: Option<Vec<&'src str>>,
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
    pub name: &'src str,
    pub generics: Option<Vec<&'src str>>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Generic<'src> {
    pub type_name: &'src str,
    pub where_clause: Clauses<'src>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Clauses<'src>(pub Vec<TypeName<'src>>);

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct DeclArg<'src> {
    pub name: Option<&'src str>,
    pub ty: DeclType<'src>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum DefRhs<'src> {
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

    Func {
        args: Vec<DeclArg<'src>>,
        block: Box<Expr<'src>>,
    },
}

type ParserInput<'tokens, 'src> =
    chumsky::input::SpannedInput<Token<'src>, SimpleSpan, &'tokens [(Token<'src>, SimpleSpan)]>;

pub trait VunkParser<'tokens, 'src: 'tokens, O>:
    Parser<
        'tokens,
        ParserInput<'tokens, 'src>,
        O,
        chumsky::extra::Err<Rich<'tokens, Token<'src>, SimpleSpan>>,
    > + Clone
{
}

impl<'tokens, 'src, O, P> VunkParser<'tokens, 'src, O> for P
where
    'src: 'tokens,
    P: Parser<
            'tokens,
            ParserInput<'tokens, 'src>,
            O,
            chumsky::extra::Err<Rich<'tokens, Token<'src>, SimpleSpan>>,
        > + Clone,
{
}

impl Expr<'_> {
    pub fn parser<'tokens, 'src: 'tokens>() -> impl VunkParser<'tokens, 'src, Expr<'src>> {
        chumsky::recursive::recursive(|expr| {
            // TODO: Support floats
            // let float_parser = select! {
            // };

            decl_parser()
                .or(def_parser(expr.clone()))
                .or(
                    binary_parser(expr.clone()).map(|(lhs, op, rhs)| Expr::Binary {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }),
                )
                .or(unary_parser(expr.clone()).map(|(op, expr)| Expr::Unary {
                    op,
                    expr: Box::new(expr),
                }))
                .or(list_parser(expr.clone()).map(|v| Expr::List(v)))
                .or(str_parser().map(Expr::Str))
                .or(int_parser().map(Expr::Integer))
                .or(bool_parser().map(Expr::Bool))
                .or(ident_parser().map(Expr::Ident))
        })
    }
}

fn list_parser<'tokens, 'src: 'tokens>(
    expr_parser: impl VunkParser<'tokens, 'src, Expr<'src>> + Clone,
) -> impl VunkParser<'tokens, 'src, Vec<Expr<'src>>> {
    let left_br = just(Token::Op("["));
    let right_br = just(Token::Op("]"));

    expr_parser
        .delimited_by(left_br, right_br)
        .repeated()
        .collect()
}

fn unary_parser<'tokens, 'src: 'tokens>(
    expr_parser: impl VunkParser<'tokens, 'src, Expr<'src>> + Clone,
) -> impl VunkParser<'tokens, 'src, (UnaryOp, Expr<'src>)> {
    let unary_op_parser = {
        let binary = just(Token::Op("~")).to(UnaryOp::BinaryNot);
        let logical = just(Token::Op("!")).to(UnaryOp::LogicalNot);

        binary.or(logical)
    };

    unary_op_parser.then(expr_parser)
}

fn binary_parser<'tokens, 'src: 'tokens>(
    expr_parser: impl VunkParser<'tokens, 'src, Expr<'src>> + Clone,
) -> impl VunkParser<'tokens, 'src, (Expr<'src>, BinaryOp, Expr<'src>)> {
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

    let lhs = ident_parser()
        .map(Expr::Ident)
        .or(bool_parser().map(Expr::Bool))
        .or(int_parser().map(Expr::Integer))
        .or(list_parser(expr_parser.clone()).map(|v| Expr::List(v)))
        .or({
            expr_parser
                .clone()
                .delimited_by(just(Token::Op("(")), just(Token::Op(")")))
        });

    let rhs = lhs.clone();

    lhs.then(binary_op_parser)
        .then(rhs)
        .map(|((lhs, op), rhs)| (lhs, op, rhs))
}

fn bool_parser<'tokens, 'src: 'tokens>() -> impl VunkParser<'tokens, 'src, bool> + Clone {
    select! {
        Token::Bool(bl) => bl,
    }
}

fn int_parser<'tokens, 'src: 'tokens>() -> impl VunkParser<'tokens, 'src, Integer> {
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
}

fn str_parser<'tokens, 'src: 'tokens>() -> impl VunkParser<'tokens, 'src, &'src str> {
    select! {
        Token::Str(s) => s
    }
}

fn ident_parser<'tokens, 'src: 'tokens>() -> impl VunkParser<'tokens, 'src, &'src str> {
    select! {
        Token::Ident(s) => s,
    }
}

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
// foo A B: (A, B) -> A
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
fn decl_parser<'tokens, 'src: 'tokens>() -> impl VunkParser<'tokens, 'src, Expr<'src>> {
    chumsky::recursive::recursive(|_decl_parser| {
        // Parser for a Type
        //
        //  The name of the type
        //    v
        // `Result T E`
        //         ^ ^
        //         Generics of the type
        fn type_parser<'tokens, 'src: 'tokens>() -> impl VunkParser<'tokens, 'src, TypeName<'src>> {
            ident_parser()
                .then(ident_parser().repeated().collect().or_not())
                .map(|(ident, generics)| TypeName {
                    name: ident,
                    generics,
                })
        }

        // Parser for a function signature without the "where"-part
        //
        // E.G.: `(A) -> A`
        // E.G.: `(A B C) -> A B C`
        //  (`B` and `C` are generics for `A` here)
        //
        //  Returns the Decl::Func variant
        fn func_parser<'tokens, 'src: 'tokens>() -> impl VunkParser<'tokens, 'src, DeclType<'src>> {
            let arrow = just(Token::Op("->"));

            // Parser for a list of arguments
            //
            // Like: `(A, B)`
            // Or: `(A B C, D)` (`B` and `C` are generics for `A`
            //
            // In a declaration, we do not have argument names
            fn args_parser<'tokens, 'src: 'tokens>(
            ) -> impl VunkParser<'tokens, 'src, Vec<DeclArg<'src>>> {
                let open_par = just(Token::Op("("));
                let close_par = just(Token::Op(")"));
                let comma = just(Token::Op(","));

                open_par
                    .ignore_then(
                        type_parser()
                            .map(|tyname| DeclArg {
                                name: None,
                                ty: DeclType::TypeName(tyname),
                            })
                            .separated_by(comma)
                            .collect(),
                    )
                    .then_ignore(close_par)
            }

            args_parser()
                .then_ignore(arrow)
                .then(type_parser())
                .map(|(args, retty)| DeclType::Func { args, retty })
        }

        // Parser for `where` clause - the generic bounds
        //
        // ```
        // where A: FooT + BarT,
        //       B: BarT
        // ```
        fn generic_bounds_parser<'tokens, 'src: 'tokens>(
        ) -> impl VunkParser<'tokens, 'src, Vec<Generic<'src>>> {
            let generic_name_parser = ident_parser();

            let assign_parser = just(Token::Op(":"));

            /// Parser for the `Into I8 + Debug` in `where A: Into I8 + Debug;`
            fn clause_parser<'tokens, 'src: 'tokens>(
            ) -> impl VunkParser<'tokens, 'src, Clauses<'src>> {
                // A trait name is written down like a type name:
                //
                // `T A B` (`A` and `B` are generics for `T`)
                let trait_parser = type_parser();

                trait_parser
                    .separated_by(just(Token::Op("+")))
                    .collect()
                    .map(Clauses)
            }

            just(Token::Where).ignore_then({
                generic_name_parser
                    .then_ignore(assign_parser.clone())
                    .then(clause_parser())
                    .map(|(type_name, clauses)| Generic {
                        type_name,
                        where_clause: clauses,
                    })
                    .separated_by(just(Token::Op(",")))
                    .collect()
            })
        }

        ident_parser() // name of the declaration
            .then(ident_parser().repeated().collect().or_not()) // Generic arguments
            .then_ignore(just(Token::Op(":")))
            .then({
                // The type of a declaration is either a function, where we need args
                // and return type and so on,
                // or a concrete type (which can be generic)
                type_parser().map(DeclType::TypeName).or(func_parser())
            })
            // Both a function decl and a normal variable decl can be generic
            .then(generic_bounds_parser().or_not())
            .then_ignore(just(Token::Op(";")))
            .map(
                |(((decl_name, decl_generic_names), decl_type), whereclause)| Expr::Decl {
                    ident: decl_name,
                    generics: decl_generic_names,
                    decl_type,
                    whereclause,
                },
            )
    })
}

// Parser for a definition
//
// ## Short example
//
// ```
// foo = 1;
// ```
//
// Declares `foo` to be 1.
//
// ## Full example
//
// ```
// add A A = (a: A, b: A) -> a + b;
// ```
//
// Declares a variable `add` generic over `A`
// to be a function with arguments `a` and `b` of type `A`
// and the implementation `a + b`.
//
// There are no generic bounds here, because if generics are in use, we expect a declaration to be
// there.
//
fn def_parser<'tokens, 'src: 'tokens>(
    expr_parser: impl VunkParser<'tokens, 'src, Expr<'src>> + Clone + 'tokens,
) -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Expr<'src>,
    chumsky::extra::Err<Rich<'tokens, Token<'src>, SimpleSpan>>,
> + Clone {
    chumsky::recursive::recursive(|_def_parser| {
        ident_parser() // name of the declaration
            .then(ident_parser().repeated().collect().or_not()) // Generic arguments
            .then_ignore(just(Token::Op("=")))
            .then( DefRhs::parser(expr_parser))
            .then_ignore(just(Token::Op(";")))
            .map(|((name, generics), rhs)| Expr::Def {
                lhs: name,
                generics,
                rhs,
            })
    })
}

impl DefRhs<'_> {
    pub fn parser<'tokens, 'src: 'tokens>(
        expr_parser: impl VunkParser<'tokens, 'src, Expr<'src>> + Clone,
    ) -> impl VunkParser<'tokens, 'src, DefRhs<'src>> + Clone {
        binary_parser(expr_parser.clone())
            .map(|(lhs, op, rhs)| DefRhs::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
            .or(
                unary_parser(expr_parser.clone()).map(|(op, expr)| DefRhs::Unary {
                    op,
                    expr: Box::new(expr),
                }),
            )
            .or(list_parser(expr_parser.clone()).map(|v| DefRhs::List(v)))
            .or(str_parser().map(DefRhs::Str))
            .or(int_parser().map(DefRhs::Integer))
            .or(bool_parser().map(DefRhs::Bool))
            .or(ident_parser().map(DefRhs::Ident))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::{prelude::Input, Parser};

    macro_rules! ast_has_no_errs {
        ($code:ident) => {
            let res = vunk_lexer::lexer().parse($code);
            assert!(!res.has_errors());
            let tokens = res.into_output().unwrap();
            let tokens = tokens.as_slice().spanned(($code.len()..$code.len()).into());
            let res = Expr::parser().parse(tokens);
            assert!(
                !res.has_errors(),
                "No errors expected, but found: {:?}",
                res.errors().collect::<Vec<_>>()
            );
        }
    }

    #[test]
    fn test_ast_bool() {
        let code = "false";
        ast_has_no_errs!(code);
    }

    #[test]
    fn test_ast_int() {
        let code = "123";
        ast_has_no_errs!(code);
    }

    #[test]
    fn test_ast_ident() {
        let code = "foobar";
        ast_has_no_errs!(code);
    }

    fn decl_has_no_errs(code: &str) {
        let res = vunk_lexer::lexer().parse(code);
        assert!(
            !res.has_errors(),
            "No errors expected, but found: {:?}",
            res.errors().collect::<Vec<_>>()
        );
        let tokens = res.into_output().unwrap();
        let tokens = tokens.as_slice().spanned((code.len()..code.len()).into());
        let res = decl_parser().parse(tokens);
        assert!(
            !res.has_errors(),
            "No errors expected, but found: {:?} --- CODE: '{code}'",
            res.errors().collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_decl_var() {
        decl_has_no_errs("foo: I8;");
    }

    #[test]
    fn test_decl_var_generic() {
        decl_has_no_errs("foo A: A;");
    }

    #[test]
    fn test_decl_fn() {
        decl_has_no_errs("foo: (I8) -> I8;");
    }

    #[test]
    fn test_decl_fn_generic() {
        decl_has_no_errs("foo A: (A) -> A;");
    }

    #[test]
    fn test_decl_fn_generic_2() {
        decl_has_no_errs("foo A B: (A, B) -> A;");
    }

    #[test]
    fn test_decl_fn_generic_with_bounds() {
        let code = r#"
            foo A: (A) -> A
                where A: Debug;
        "#;
        decl_has_no_errs(code);
    }

    #[test]
    fn test_decl_fn_generic_2_with_bounds() {
        let code = r#"
            foo A B: (A, B) -> A
                where A: Debug,
                      B: Debug;
        "#;
        decl_has_no_errs(code);
    }

    #[test]
    fn test_decl_fn_generic_2_with_bounds_generic() {
        let code = r#"
            foo A: (A) -> A
                where A: Add I8;
        "#;
        decl_has_no_errs(code);
    }

    #[test]
    fn test_decl_fn_generic_2_with_bounds_2() {
        let code = r#"
            foo A B: (A, B) -> A
                where A: Add I8,
                      B: Add I8;
        "#;
        decl_has_no_errs(code);
    }

    #[test]
    fn test_decl_fn_generic_2_with_multi_bounds() {
        let code = r#"
            foo A: (A) -> A
                where A: Add I8 + Debug;
        "#;
        decl_has_no_errs(code);
    }

    #[test]
    fn test_decl_fn_generic_2_with_multi_bounds_2() {
        let code = r#"
            foo A B: (A, B) -> A
                where A: Add I8 + Debug,
                      B: Add I8 + Debug;
        "#;
        decl_has_no_errs(code);
    }

    #[test]
    fn test_binary_expr() {
        let code = r#"
            1 + 2
        "#;
        ast_has_no_errs!(code);
    }

    #[test]
    fn test_def_var() {
        let code = r#"
            foo = 1;
        "#;
        ast_has_no_errs!(code);
    }

    #[test]
    fn test_def_var_generic() {
        let code = r#"
            foo A = A;
        "#;
        ast_has_no_errs!(code);
    }

    #[test]
    fn test_def_fn() {
        let code = r#"
            foo = () -> 1;
        "#;
        ast_has_no_errs!(code);
    }

    #[test]
    fn test_def_fn_1() {
        let code = r#"
            foo = (a: I8) -> a;
        "#;
        ast_has_no_errs!(code);
    }

    #[test]
    fn test_def_fn_2() {
        let code = r#"
            foo = (a: I8) -> a + 1;
        "#;
        ast_has_no_errs!(code);
    }

    #[test]
    fn test_def_fn_3() {
        let code = r#"
            foo = (a: I8) -> if a > 0
                then a - 1
                else a;
        "#;
        ast_has_no_errs!(code);
    }

    #[test]
    fn test_def_fn_4() {
        let code = r#"
            foo = (a: I8) -> let
                    iszero = a > 0;
                in
                if iszero
                then 0
                else a - 1;
        "#;
        ast_has_no_errs!(code);
    }

    #[test]
    fn test_def_fn_generic_1() {
        let code = r#"
            foo A = (a: A) -> a + 1;
        "#;
        ast_has_no_errs!(code);
    }

    #[test]
    fn test_def_fn_generic_2() {
        let code = r#"
            foo A B = (a: A, b: B) -> a + b;
        "#;
        ast_has_no_errs!(code);
    }
}
