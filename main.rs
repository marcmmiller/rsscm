
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
            let tmp = self.unget;
            self.unget = '\0';
            Ok(tmp)
        }
        else {
            let mut buf : [u8;1] = [ 0 ];
            match (self.reader as &mut Read).read(&mut buf) {
                Ok(_) => Ok(buf[0] as char),
                Err(e) => Err(e)
            }
        }
    }

    fn unget_char(&mut self, c : char) {
        // TODO: assert that the unget character is \0
        self.unget = c;
    }

    fn read_while<F>(&mut self, f: F) -> std::io::Result<String> where F: Fn(char) -> bool {
        let mut s = "".to_string();
        loop {
            match self.next_char() {
                Ok(c) => {
                    if f(c) {
                        s.push(c);
                    }
                    else {
                        self.unget_char(c);
                        return Ok(s)
                    }
                },
                Err(e) => return Err(e)
            }
        }
    }

    fn read_scheme_id(&mut self) -> std::io::Result<String> {
        self.read_while(|c : char| { c.is_alphabetic() })
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

    //let t = tok.next();
    //println!("token: {:?} ", t);

    let t = tok.read_scheme_id();
    println!("read {:?}", t);
}
