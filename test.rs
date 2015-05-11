
#[derive(Debug)]
struct SInt(i32);

#[derive(Debug)]
enum Sexp {
    Num(i32),
    Id(String),
    Cons(Box<(Sexp, Sexp)>),
    Nil
}

use Sexp::{Cons, Nil};

impl Sexp {
    fn new() -> Sexp {
        Nil
    }
}

fn main() {
    println!("hello world");

    let x = SInt(4);

    println!("This is my int {:?}", x);

    let v = 5;
    match v {
        ref r => println!("Matched {:?}", *r),
    }

    let s = Sexp::Num(4);
    let s2 = Sexp::new();
    println!("Sexp: {:?} {:?}", s, s2);

    let y = Box::new(4);
    println!("This is my box {:?}", y);

    let ref z = y;
    println!("This are my boxes {:?} {:?}", y, z);


    let cons = Sexp::Cons(Box::new((Sexp::Num(3), Sexp::Num(4))));
}
