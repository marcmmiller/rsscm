

# A Quick Rust Intro for C++ Programmers

Imagine if C++ didn't call non-default copy constructors implicitly.


## Const by default

In Rust, all variables behave as if they have been marked `const` unless you
override them with a `mut` specifier.

```rust
let a = 4;
```

is equivalent to

```c++
const auto a = 4;
```

If you want to change `a`, then you have to use mut:

```rust
let mut a = 4;
a = 5; // legal
```

## Varialbe declaration

## Semicolon thing


## Moves versus copies.


## this pointer is explicit




# Rust Preso Outline

## Interesting things about Rust

* `[derive]` is cool

## About Rust

* 1.0 release
* compiles to llvm
* no-GC, very small runtime
* type inferencer
* generics
* traits (no inheritance)
* borrow-checker
* well-documented standard library
* great community

## Pattern Matching


## if { } is an expression


## copy vs move semantics


## borrow checker


## traits


## Rc & Box


## Macros

