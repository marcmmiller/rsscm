
use std::io::{BufReader,Chars,Read,stdin};

#[allow(dead_code)]
enum Sexp {
    Num(i32),
    Id(String),
    Cons(Box<(Sexp, Sexp)>),
    Nil
}

#[allow(unused_imports)]
use Sexp::{ Num, Id, Cons, Nil };

struct Tokenizer<'a, R: 'a> {
    reader: &'a BufReader<R>
}

#[derive(Debug)]
enum Token {
    Num(i32),
    Id(String)
}

enum State {
    Start
}

impl<'a, R> Tokenizer<'a, R> {
    fn new(br : &'a BufReader<R>) -> Tokenizer<R> {
        Tokenizer { reader : br }
    }

    fn next(&mut self) -> Token {
        let r = self.reader as &Read;
        let b = r.bytes();
        Token::Num(42)
    }
}

fn main() {
    println!("Hello world");

    let sin = stdin();
    let reader = BufReader::new(sin);
    let tok = Tokenizer::new(&reader);

    let t = tok.next();
    println!("token: {:?} ", t);
}
