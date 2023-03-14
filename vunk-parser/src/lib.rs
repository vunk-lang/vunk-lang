// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use chumsky::span::SimpleSpan;

pub mod expr;
pub mod generic;
pub mod literal;
pub mod op;
pub mod program;

pub type Spanned<T> = (T, SimpleSpan);
