
use std::collections::HashMap;
use std::io::prelude::*;
use std::io::{BufReader, stdin};
use std::mem;
use std::rc::Rc;


//------------------------------------------------------------------------------
// ReadHelper - makes up for some missing features of Rust's std io package.
//------------------------------------------------------------------------------
struct ReadHelper<R> {
    reader: BufReader<R>,
    unget: char
}

impl<R : Read> ReadHelper<R> {
    fn new(r: R) -> ReadHelper<R> {
        let br = BufReader::new(r);
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

    fn is_scheme_start(self) -> bool {
        self.is_alphabetic() || self.is_scheme_sym()
    }

    fn is_scheme_continue(self) -> bool {
        self.is_alphanumeric() || self.is_scheme_sym()
    }
}

#[derive(Debug, Clone, PartialEq)]
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
    reader: ReadHelper<R>,
    unget: Token
}

impl<R : Read> Tokenizer<R> {
    fn new(r: R) -> Tokenizer<R> {
        Tokenizer { reader: ReadHelper::new(r), unget: Token::Eof }
    }

    fn read_scheme_id(&mut self) -> std::io::Result<Token> {
        let t = Token::Id(try!(self.reader.read_while(&|c: char| { c.is_scheme_continue() })));
        Ok(t)
    }

    fn read_number(&mut self) -> std::io::Result<Token> {
        use std::f64;
        let s = try!(self.reader.read_while(&|c: char| { c.is_numeric() || c == '.' }));
        if let Ok(f) = s.parse::<f64>() {
            Ok(Token::Num(f))
        } else {
            Ok(Token::Num(f64::NAN))
        }
    }

    fn unget(&mut self, t: Token) {
        self.unget = t;
    }

    fn next(&mut self) -> std::io::Result<Token> {
        if self.unget != Token::Eof {
            let mut tmp = Token::Eof;
            mem::swap(&mut tmp, &mut self.unget);
            return Ok(tmp);
        }

        loop {
            return match try!(self.reader.next_char()) {
                '(' => Ok(Token::OP),
                ')' => Ok(Token::CP),
                '.' => Ok(Token::DOT),
                '\'' => Ok(Token::QUOTE),
                '\0' => Ok(Token::Eof),
                c if c.is_whitespace() => continue,
                c if c.is_numeric() => {
                    self.reader.unget_char(c);
                    self.read_number()
                },
                c if c.is_scheme_start() => {
                    self.reader.unget_char(c);
                    self.read_scheme_id()
                },
                _ => Ok(Token::Num(42f64)) // TODO: remove
            }
        }
    }
}

#[allow(dead_code)]
fn test_tok() {
    let sin = stdin();
    let mut tok = Tokenizer::new(sin);

    loop {
        let t = tok.next();
        println!("read {:?}", t);

        if let Ok(Token::Eof) = t { break; }
    }
}

//------------------------------------------------------------------------------
// Type System
//------------------------------------------------------------------------------
#[derive(Debug)]
enum Sexp {
    Num(f64),
    Id(String),
    Cons(Box<(Sexp, Sexp)>),
    Nil,
    Eof,
}

use Sexp::{ Num, Id, Cons, Nil };

fn new_cons(car: Sexp, cdr: Sexp) -> Sexp {
    Cons(Box::new((car, cdr)))
}

//------------------------------------------------------------------------------
// Parser
//------------------------------------------------------------------------------
struct Parser<R> {
    tokenizer: Tokenizer<R>
}

impl<R: Read> Parser<R> {
    fn new(r : R) -> Parser<R> {
        Parser { tokenizer : Tokenizer::new(r) }
    }

    fn next_sexp(&mut self) -> std::io::Result<Sexp> {
        Ok(match try!(self.tokenizer.next()) {
            Token::Id(str) => Id(str),
            Token::Num(n)  => Num(n),
            Token::Eof     => Sexp::Eof,
            Token::QUOTE   => new_cons(Id("quote".to_string()),
                                       new_cons(try!(self.next_sexp()), Nil)),
            Token::OP => try!(self.next_sexp_list(false)),
            _ => Num(42f64)
        })
    }

    fn next_sexp_list(&mut self, dot_allowed : bool) -> std::io::Result<Sexp> {
        let t = try!(self.tokenizer.next());
        Ok(match t {
            Token::DOT => {
                assert!(dot_allowed);
                let ret = try!(self.next_sexp());
                assert_eq!(try!(self.tokenizer.next()), Token::CP);
                ret
            },
            Token::CP => { Nil },
            _ => {
                self.tokenizer.unget(t);
                new_cons(try!(self.next_sexp()),
                         try!(self.next_sexp_list(true)))
            }
        })
    }
}

fn test_parser() {
    let sin = stdin();
    let mut parser = Parser::new(sin);

    loop {
        let s = parser.next_sexp();
        if let Ok(Sexp::Eof) = s { break; }
        println!("Sexp: {:?}", s);
    }
}

//------------------------------------------------------------------------------
// Environment
//------------------------------------------------------------------------------
struct Frame {
    symtab: HashMap<String, Sexp>,
    next: Option<Rc<Frame>>
}

impl Frame {
    fn new() -> Frame {
        Frame { symtab : HashMap::new(), next: None }
    }
}

fn find_frame(env: Rc<Frame>, sym: String) -> Option<Rc<Frame>> {
    let cur = &Some(env);
    while let ref Some(f) = cur {
        if f.symtab.contains_key(&sym) {
            return Some(f);
        }
        cur = f.next;
    }
    None
}

//------------------------------------------------------------------------------
fn main() {
    println!("Welcome to Scheme!");
    test_parser();
}
