# vunk

vunk, or vunk-lang, is a strongly typed purely functional low level compiled
programming language.


## Purpose / Goal

This project is purely for learning purposes, but might result in something
useful down the road. Who knows?

The goal of this project is to implement a

* low level
    * Interfacing with C ABI is possible
    * Writing an OS is not possible (at least it is not a goal YET)
* strongly typed
    * Errors should happen at compiletime, like with Rust
    * Type inference is heavily used
* (purely) functional
    * There are no sideeffects
    * Monads are used for modeling sideeffects
    * Currying and partial function application is possible, happens at
        compiletime
* easy
    * Easy to read and write, only few concepts to understand
    * Safe (and zero-cost) abstractions can be built (like with Rust)
    * (Compiler-)Error messages are as helpful as possible
* with LLVM compiled

programming language.

The general idea is to have a Rust-inspired functional language.

For some code examples, have a look at the `vunk-examples` directory (which is
also used to test the components of the implementation (lexer, parser, ...)).

## Language Features

Planned language features/ideas:

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
        * [ ] String types
            * [ ] UTF-8 encoded
            * [ ] Like Rusts `str`, `String`, ...
        * [ ] Option
        * [ ] Result
        * [ ] Collection types
            * [ ] Vec
            * [ ] HashMap
    * [ ] Traits
        * [ ] Conversion
            * [ ] `From` / `Into`
            * [ ] `TryFrom` / `TryInto`
            * [ ] `FromStr`
        * [ ] More functional stuff
            * [ ] Functor
            * [ ] Applicative
            * [ ] Monoid
            * [ ] Monad

... and more.

## License

(c) 2023 Matthias Beyer

MPL-2.0

