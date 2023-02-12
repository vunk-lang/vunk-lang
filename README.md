# vunk

vunk, or vunk-lang, is a strongly typed purely functional low level compiled
programming language.


## Purpose / Goal

This project is purely for learning purposes, but might result in something
useful down the road. Who knows?

The goal of this project is to implement a

* low level[^1]
* strongly typed[^2]
* purely functional[^3]
* easy[^4]
* with LLVM compiled

programming language.

Think Rust + Haskell + Ruby.

For some code examples, have a look at the `vunk-examples` directory (which is
also used to test the components of the implementation (lexer, parser, ...)).

[^1]: One can interface with C ABIs
[^2]: Errors should happen at compiletime, not at runtime
[^3]: There should be no sideeffects in the language
[^4]: Easy to read and write, only few concepts to understand


## Language Features

Planned language features:

* [ ] Functions
    * [ ] Partial function application / Currying
    * [ ] Higher-Order Functions
* [ ] Types
    * [ ] Structures / Records
    * [ ] Enums
    * [ ] Interfaces / Traits / Typeclasses
    * [ ] Generics
* [ ] Standard Library
    * [ ] Types
        * [ ] Bool
        * [ ] Integers
            * [ ] i8
            * [ ] i16
            * [ ] i32
            * [ ] i64
            * [ ] i128
            * [ ] u8
            * [ ] u16
            * [ ] u32
            * [ ] u64
            * [ ] u128
            * [ ] isize
            * [ ] usize
        * [ ] String
        * [ ] Option
        * [ ] Result
        * [ ] Vec
        * [ ] HashMap
    * [ ] "Interfaces"
        * [ ] Functor
        * [ ] Applicative
        * [ ] Monoid
        * [ ] Monad

... and more.

## License

(c) 2023 Matthias Beyer

MPL-2.0

