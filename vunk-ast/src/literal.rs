// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#[derive(Debug)]
pub enum Literal {
    Bool(Bool),
    Integer(Integer),
    Float(Float),
    Str(Str),
    List(Vec<crate::expr::Expr>),
}

#[derive(Debug)]
pub struct Bool {
    pub value: bool,
}

#[derive(Debug)]
pub struct Integer {
    pub value: IntegerValue,
}

#[derive(Debug)]
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
pub struct Float {
    pub value: f64,
}

#[derive(Debug)]
pub struct Str {
    pub value: String,
}
