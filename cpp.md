

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


