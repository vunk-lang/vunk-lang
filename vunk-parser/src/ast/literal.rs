// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Literal {
    Bool(Bool),
    Integer(Integer),
    Float(Float),
    Str(Str),
    List(Vec<crate::ast::expr::Expr>),
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Bool {
    pub value: bool,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Integer {
    pub value: IntegerValue,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum IntegerValue {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Float {
    pub value: f64,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Str {
    pub value: String,
}
