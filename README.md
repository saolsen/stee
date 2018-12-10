# Stee

Stee is a low level programming language that targets webassembly.
It has c-like syntax and control structures and compiles to webassembly modules.
It is also embedable and can be used from inside javascript to compile specific functions.

## Featurs

* functions
* loops
    * for
    * while
* variables
    * locals
    * globals
* webassembly native operators and builtin functions.
* types
    * i32
    * u32
    * i64
    * u64
    * f32
    * f64
* Raw memory operators.
    * load / store

There are no structs, arrays, enums or other data structures yet. Those will be coming soon with an arena based memory management system. 

The best way to currently see how it works is to look at the [Tests](https://github.com/saolsen/stee/blob/master/stee/tests/wasm_test.rs)
An example project for how to use it. **TODO**

## Contact
Email Me: steve@steve.computer
Mailing list: ~saolsen/stee@lists.sr.ht
Irc: #stee on Freenode
