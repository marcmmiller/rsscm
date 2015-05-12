
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
    state: State,
    unget: char
}

#[derive(Debug)]
enum Token {
    Num(i32),
    Id(String)
}

enum State {
    Start,
    Id
}

impl<'a, R : Read> Tokenizer<'a, R> {
    fn new(br : &'a mut BufReader<R>) -> Tokenizer<R> {
        Tokenizer { reader : br, state: State::Start, unget: '\0' }
    }

    // Helper to read the next character.
    fn next_char(&mut self) -> std::io::Result<char> {
        if self.unget != '\0' {
            Ok(self.unget)
        }
        else {
            let mut buf : [u8;1] = [ 0 ];
            match (self.reader as &mut Read).read(&mut buf) {
                Ok(_) => Ok(buf[0] as char),
                Err(e) => Err(e)
            }
        }
    }

    fn next(&mut self) -> std::io::Result<Token> {
        match self.next_char() {
            Ok(c) => Ok(Token::Num(c as i32)),
            Err(e) => Err(e)
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
