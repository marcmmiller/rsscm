
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


//------------------------------------------------------------------------------
// ReadHelper - makes up for some missing features of Rust's std io package.
//------------------------------------------------------------------------------
struct ReadHelper<R> {
    reader: BufReader<R>,
    unget: char
}

impl<R : Read> ReadHelper<R> {
    fn new(r: R) -> ReadHelper<R> {
        let mut br = BufReader::new(r);
        ReadHelper::<R> { reader : br, unget : '\0' }
    }

    fn next_char(&mut self) -> std::io::Result<char> {
        if self.unget != '\0' {
            let tmp = self.unget;
            self.unget = '\0';
            Ok(tmp)
        }
        else {
            let mut buf : [u8;1] = [ 0 ];
            match self.reader.read(&mut buf) {
                Ok(_) => Ok(buf[0] as char),
                Err(e) => Err(e)
            }
        }
    }

    fn unget_char(&mut self, c: char) {
        // TODO: assert that the unget character is \0
        self.unget = c;
    }

    fn read_while(&mut self, pred: &Fn(char) -> bool) -> std::io::Result<String> {
        let mut s = "".to_string();
        loop {
            match self.next_char() {
                Ok(c) => {
                    if pred(c) {
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
}

//------------------------------------------------------------------------------
// Tokenizer
//------------------------------------------------------------------------------

//
// Extension methods for char to add Scheme character classes.
//
trait SchemeIdChars {
    fn is_scheme_sym(self) -> bool;
    fn is_scheme_start(self) -> bool;
    fn is_scheme_continue(self) -> bool;
}

impl SchemeIdChars for char {
    fn is_scheme_sym(self) -> bool {
        let p = self;
        ((p == '-') || (p == '_') || (p == '*') ||
         (p == '+') || (p == '?') || (p == '-') ||
         (p == '!') || (p == '/') || (p == '<') ||
         (p == '>') || (p == '=') || (p == '$') ||
         (p == '%') || (p == '&') || (p == '~') ||
         (p == '^'))
    }

    fn is_scheme_start(self) -> bool { self.is_alphabetic() || self.is_scheme_sym() }
    fn is_scheme_continue(self) -> bool  { self.is_alphanumeric() || self.is_scheme_sym() }
}

#[derive(Debug)]
enum Token {
    Num(f64),
    Id(String),
    OP,
    CP,
    DOT,
    QUOTE,
    Eof
}

struct Tokenizer<R> {
    reader: ReadHelper<R>
}

impl<R : Read> Tokenizer<R> {
    fn new(r : R) -> Tokenizer<R> {
        Tokenizer { reader : ReadHelper::new(r) }
    }

    fn read_scheme_id(&mut self) -> std::io::Result<Token> {
        match self.reader.read_while(&|c: char| { c.is_scheme_continue() }) {
            Ok(s) => Ok(Token::Id(s)),
            Err(e) => Err(e)
        }
    }

    fn read_number(&mut self) -> std::io::Result<Token> {
        use std::f64;
        let rs = self.reader.read_while(&|c: char| { c.is_numeric() || c == '.' });
        match rs {
            Ok(s) => {
                if let Ok(f) = s.parse::<f64>() {
                    Ok(Token::Num(f))
                } else {
                    Ok(Token::Num(f64::NAN))
                }
            },
            Err(e) => Err(e)
        }
    }

    fn next(&mut self) -> std::io::Result<Token> {
        loop {
            match self.reader.next_char() {
                Ok(c) => {
                    if c.is_whitespace() {
                        continue;
                    }
                    else if c.is_scheme_start() {
                        self.reader.unget_char(c);
                        return self.read_scheme_id()
                    }
                    else if c.is_numeric() {
                        self.reader.unget_char(c);
                        return self.read_number()
                    }
                    else if c == '(' {
                        return Ok(Token::OP);
                    }
                    else if c == ')' {
                        return Ok(Token::CP);
                    }
                    else if c == '.' {
                        // TODO: handle ".42" as number
                        return Ok(Token::DOT);
                    }
                    else if c == '\'' {
                        return Ok(Token::QUOTE);
                    }
                    else if c == '\0' {
                        return Ok(Token::Eof);
                    }
                    else {
                        return Ok(Token::Num(42f64))
                    }
                }
                Err(e) => return Err(e)
            }
        }
    }
}

fn main() {
    println!("Hello world");

    let sin = stdin();
    let mut tok = Tokenizer::new(sin);

    loop {
        let t = tok.next();
        println!("read {:?}", t);

        if let Ok(Token::Eof) = t { break; }
    }
}
