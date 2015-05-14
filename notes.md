


# Notes on Rust

## Moving from a borrowed thing (the unget issue)

What's wrong with the start_processing function below?  The standard
shuffle things around using a `tmp` looks like something I would write
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
        let tmp = self.current_state;
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
 rustc test6.rs
test6.rs:26:19: 26:23 error: cannot move out of borrowed content
test6.rs:26         let tmp = self.current_state;
                              ^~~~
test6.rs:26:13: 26:16 note: attempting to move value to here
test6.rs:26         let tmp = self.current_state;
                        ^~~
test6.rs:26:13: 26:16 help: to prevent the move, use `ref tmp` or `ref mut tmp` to capture value by reference
test6.rs:35:15: 35:16 error: cannot borrow immutable local variable `m` as mutable
test6.rs:35     let old = m.start_processing("foo".to_string());
```

See, the compiler thinks that we're *moving* a value out of `self` and
therefore it will be a dangling reference.  It's not smart enough to
notice that we're ensuring, via the very next line, that the reference
won't be dangling.

You could of course work around this problem by `clone`-ing
`current_state` into `tmp`, but that's not very satisfying: why should
I have to clone the object here when what I really want is to move it.

It turns out the only way to do this kind of thing in Rust is via
unsafe operations, but those are encapsulated nicely by `std::mem::swap`.

```rust
    fn start_processing(&mut self, s: String) -> State {
        let mut tmp = Processing(s);
        std::mem::swap(&mut self.current_state, &mut tmp);
        tmp
    }
```

## How to Traverse a Linked List

## How does Rc<T> work? (Deref Trait in test5)
