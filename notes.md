

# Marc's Notes on Rust

## Swapping and Moving

What's wrong with the `start_processing` function below?  The standard
"shuffle things around using a `tmp`" looks like something I would write
in C.  But it won't compile.

```rust
#[derive(Debug)]
enum State {
    Ready,

    // Note that String here prevents us from auto-deriving the Copy
    // trait.
    Processing(String),

    Finished
}

use State::*;

struct Machine {
    current_state: State
}

impl Machine {
    fn new() -> Machine {
        Machine { current_state: State::Ready }
    }

    // Returns the old state.
    fn start_processing(&mut self, s: String) -> State {
        let mut tmp = self.current_state;
        self.current_state = Processing(s);
        tmp
    }
}

fn main() {
    let mut m = Machine::new();

    let old = m.start_processing("foo".to_string());

    println!("Machine's old state is {:?} and new is {:?}",
             old, m.current_state);
}
```

The rust compiler spits out:

```
error: cannot move out of borrowed content
let mut tmp = self.current_state;
              ^~~~
note: attempting to move value to here
let mut tmp = self.current_state;
    ^~~~~~~
```

See, the compiler thinks that we're *moving* a value out of `self` and
therefore it will be a dangling reference.  It's not smart enough to
notice that we're ensuring, via the very next line, that the reference
won't be dangling.

You could of course work around this problem by `clone`-ing
`current_state` into `tmp`, but that's not very satisfying: why should
I have to clone the object here when what I really want is to move it.

It turns out the only way to do this kind of thing in Rust is via
[unsafe operations][swap], but those are encapsulated nicely by `std::mem::swap`.

```rust
    fn start_processing(&mut self, s: String) -> State {
        let mut tmp = Processing(s);
        std::mem::swap(&mut self.current_state, &mut tmp);
        tmp
    }
```

[swap]: https://doc.rust-lang.org/src/core/mem.rs.html#294-308

## Fighting the Borrow Checker: Traversing a Linked List

I have a linked list of reference counted nodes.

```rust
struct StringNode {
    data: String,
    next: Option<Rc<StringNode>>
}
```

As a Java programmer, here's the kind of code I'd like to write in
order to traverse the linked list:

```rust
fn contains(s: String, head: Rc<StringNode>) -> bool {
    let mut cur = Some(head);
    while let Some(node) = cur {
        if s == node.data {
            return true;
        }
        cur = node.next  // Oops!
    }
    false
}
```

But, in Rust the compiler complains:

```
error: cannot move out of borrowed content
cur = node.next
      ^~~~
```

What's happening here?  Well, because of Rust's move semantics, the
compiler thinks I'm trying to *move* `node.next` into `cur` and
thus invalidate `cur`.  That's not what I want at all: what I want is for
`cur` to just be a pointer to the current node.  So I need to make cur
be a reference:

```rust
fn contains(s: String, head: Rc<StringNode>) -> bool {
    let mut cur = &Some(head);
    while let Some(node) = *cur {
        if s == node.data {
            return true;
        }
        cur = &node.next
    }
    false
}
```

But there's still one problem:

```
error: cannot move out of borrowed content
while let Some(node) = *cur {
                       ^~~~
note: attempting to move value to here
while let Some(node) = *cur {
               ^~~~
```

The compiler is saying: "you're trying to move the `node` out of `cur`,
which because it's a reference is a borrowed object.  If you could do
that, then `cur` would be let dangling."  The fix is to only take a
reference to the node.

```
fn contains(s: String, head: Rc<StringNode>) -> bool {
    let mut cur = &Some(head);
    while let Some(ref node) = *cur {
        if s == node.data {
            return true;
        }
        cur = &node.next
    }
    false
}
```

Now that we have that fixed, let's say that I want to move away from
reference counting and simply have my nodes be boxed.  Let's say that
I take my naive Java Programmer approach here and use the originl
`contains` function without any references:

```rust
struct StringNode {
    data: String,
    next: Option<Box<StringNode>>
}

fn contains(s: String, head: Box<StringNode>) -> bool {
    let mut cur = Some(head);
    while let Some(node) = cur {
        if s == node.data {
            return true;
        }
        cur = node.next
    }
    false
}
```

Freaky Friday because the compiler doesn't complain at all!  It lets
this code through without error.  Why?  What's special about Box that
isn't special about Rc?

## Unfortunate Limitations of Pattern Matching

Consider the type system for a very simple Scheme interpreter.

```rust
#[derive(Debug, Clone)]
enum SchemeType {
    Id(String),
    Num(i32),
    Cons(Box<(SchemeType, SchemeType)>),
    Nil
}
```

Say I want to implement the scheme special form called `quote`, so I'm
looking to match patterns of the form `(quote a)` and turn them into
`a`, here's a naive implementation of that code:

```rust
fn eval(st: &SchemeType) -> SchemeType {
    match *st {
        Cons(Id("quote"), Cons(car, cdr)) => cdr,
        _ => *st
    }
}
```

Tons of problems with this code.

```

```



## How does Rc<T> work?

The `Deref` trait is a bit magical.

```rust

```

