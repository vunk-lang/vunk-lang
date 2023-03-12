// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

pub mod ast;

use chumsky::error::Simple;
use chumsky::Parser;

use vunk_lexer::Span;
use vunk_lexer::Token;

pub type Spanned<T> = (T, Span);
