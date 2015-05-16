use std::collections::HashMap;
use std::io::prelude::*;
use std::io::{BufReader, stdin};
use std::fmt::{Debug, Formatter};
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
#[derive(Debug, Clone)]
enum Sexp {
    Num(f64),
    Id(String),
    Cons(Box<(Sexp, Sexp)>),
    Closure(Rc<SClosure>),
    Nil,
    Eof,
}

use Sexp::{ Num, Id, Cons, Nil };

impl Sexp {
    fn new_cons(car: Sexp, cdr: Sexp) -> Sexp {
        Cons(Box::new((car, cdr)))
    }

    fn iter<'a>(&'a self) -> SexpIter<'a> {
        SexpIter { cur : self }
    }

    //
    // Sexp Helper Functions
    //
    fn car_is_id(&self, s: &str) -> bool {
        if let Id(ref id) = *self.car() {
            return id == s;
        }
        false
    }

    fn car<'a>(&'a self) -> &'a Sexp {
        if let Cons(ref b) = *self {
            let (ref car, _) = **b;
            car
        }
        else {
            unreachable!()
        }
    }

    fn cdr<'a>(&'a self) -> &'a Sexp {
        if let Cons(ref b) = *self {
            let (_, ref cdr) = **b;
            cdr
        }
        else {
            unreachable!()
        }
    }

    fn carcdr<'a>(&'a self) -> (&'a Sexp, &'a Sexp) {
        if let Cons(ref b) = *self {
            let (ref car, ref cdr) = **b;
            (car, cdr)
        }
        else {
            unreachable!()
        }
    }
}

struct SexpIter<'a> {
    cur: &'a Sexp
}

impl<'a> Iterator for SexpIter<'a> {
    type Item = &'a Sexp;
    fn next(&mut self) -> Option<&'a Sexp> {
        match (*self.cur) {
            Cons(_) => {
                let car = self.cur.car();
                self.cur = self.cur.cdr();
                Some(car)
            }
            Nil => {
                None
            }
            _ => { unreachable!() }
        }
    }
}

#[allow(dead_code)]
fn test_iter() {
    let b = "(a b c d e)".as_bytes();
    let mut parser = Parser::new(b);
    if let Ok(s) = parser.next_sexp() {
        for i in s.iter() {
            println!("{:?}", i);
        }
    }
}


#[derive(Debug)]
struct SClosure {
    env: Rc<Frame>,
    arg_names: Vec<String>,
    rest_arg: Option<String>,
    expr: Rc<Expr>
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
            Token::QUOTE   =>
                Sexp::new_cons(Id("quote".to_string()),
                               Sexp::new_cons(try!(self.next_sexp()), Nil)),
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
                Sexp::new_cons(try!(self.next_sexp()),
                               try!(self.next_sexp_list(true)))
            }
        })
    }
}

#[allow(dead_code)]
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
#[derive(Debug)]
struct Frame {
    symtab: HashMap<String, Sexp>,
    next: Option<Rc<Frame>>
}

impl Frame {
    fn new() -> Frame {
        Frame { symtab : HashMap::new(), next: None }
    }

    fn find(env: Rc<Frame>, sym: &String) -> Option<Rc<Frame>> {
        let mut cur = &Some(env);
        while let Some(ref f) = *cur {
            if f.symtab.contains_key(sym) {
                return Some(f.clone());
            }
            cur = &f.next;
        }
        None
    }

    fn lookup(&self, sym: &String) -> Option<&Sexp> {
        self.symtab.get(sym)
    }

    fn set(&mut self, sym: String, val: Sexp) {
        self.symtab.insert(sym, val);
    }
}

//------------------------------------------------------------------------------
// Semantic Analyzer
//------------------------------------------------------------------------------
type Expr = Box<Fn(Rc<Frame>) -> Sexp>;

impl Debug for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        f.write_str("<Expr>")
    }
}

fn analyze(s: &Sexp) -> Expr {
    match *s {
        Num(_) | Nil => {
            let sc = s.clone();
            Box::new(move |_| sc.clone())
        },
        Id(ref sid) => analyze_env_lookup(sid),
        Cons(ref b) => {
            match **b {
                (Id(ref id), ref cdr @ Cons(_)) if id == "lambda" =>
                    analyze_lambda(cdr),
                (Id(ref id), ref cdr @ Cons(_)) if id == "quote" =>
                    analyze_quote(cdr),
                _ => analyze_application(s)
            }
        }
        _ => Box::new(|_| Num(42f64))
    }
}

fn analyze_env_lookup(id: &String) -> Expr {
    let id = id.clone();
    Box::new(move |env| {
        if let Some(f) = Frame::find(env, &id) {
            if let Some(val) = f.lookup(&id) {
                return val.clone();
            }
        }
        panic!("Undefined variable: {}", id)
    })
}

fn analyze_quote(s: &Sexp) -> Expr {
    let ccar = s.car().clone();
    Box::new(move |_| ccar.clone())
}

fn analyze_lambda(s: &Sexp) -> Expr {
    // car is args and cdr is body
    let (ref car, ref cdr) = s.carcdr();
    let arg_names: Vec<_> = car.iter().map(|i| {
        if let Id(ref arg) = *i { arg.clone() }
        else { unreachable!() }
    }).collect();

    let expr = Rc::new(analyze_body(cdr));

    Box::new(move |env| {
        Sexp::Closure(Rc::new(SClosure {
            env: env.clone(),
            arg_names: arg_names.clone(),
            rest_arg: None,
            expr: expr.clone()
        }))
    })
}

fn analyze_body(sbody: &Sexp) -> Expr {
    let exprs: Vec<_> = sbody.iter().map(|i| {
        analyze(i)
    }).collect();

    Box::new(move |env| {
        let mut res = Nil;
        for e in &exprs {
            res = e(env.clone());
        }
        res
    })
}

fn analyze_application(sexp: &Sexp) -> Expr {
    let efunc = analyze(sexp.car());
    let eargs: Vec<_> = sexp.cdr().iter().map(|i| analyze(i)).collect();

    Box::new(move |env| {
        let func = efunc(env.clone());
        let args: Vec<_> = eargs.iter().map(|i| i(env.clone())).collect();

        if let Sexp::Closure(sc) = func {
            // TODO: zip the two collections and create bindings in the env
            // this probably means making env be mutable.
        }
        Nil
    })
}

//------------------------------------------------------------------------------
fn main() {
    println!("Welcome to Scheme!");

    let sin = stdin();
    let mut parser = Parser::new(sin);
    let mut env = Rc::new(Frame::new());

    loop {
        let s = parser.next_sexp();
        if let Ok(Sexp::Eof) = s { break; }

        println!("DEBUG: Sexp: {:?}", s);

        if let Ok(sexp) = s {
            let expr = analyze(&sexp);
            let result = expr(env.clone());
            println!("Result: {:?}", result);
        }
        else {
            break;
        }
    }
}
