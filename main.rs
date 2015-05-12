
use std::io::{BufReader,Chars,Error,Read,stdin};

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
    reader: &'a mut BufReader<R>,
    state: State
}

#[derive(Debug)]
enum Token {
    Num(i32),
    Id(String)
}

enum State {
    Start
}

impl<'a, R : Read> Tokenizer<'a, R> {
    fn new(br : &'a mut BufReader<R>) -> Tokenizer<R> {
        Tokenizer { reader : br, state: State::Start }
    }

    fn next(&mut self) -> std::io::Result<Token> {
        let mut buf : [u8;1] = [ 0 ];
        loop {
            match (self.reader as &mut Read).read(&mut buf) {
                Ok(_) => return Ok(Token::Num(buf[0] as i32)),
                Err(e) => return Err(e)
            }
        }
    }
}

fn main() {
    println!("Hello world");

    let sin = stdin();
    let mut reader = BufReader::new(sin);
    let mut tok = Tokenizer::new(&mut reader);

    let t = tok.next();
    println!("token: {:?} ", t);
}
