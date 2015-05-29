use std::cell::{Ref, RefCell};
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::prelude::*;
use std::io::{BufReader, stdin};
use std::iter::{FromIterator, IntoIterator};
use std::fmt::{Debug, Display, Formatter};
use std::mem;
use std::ops::Deref;
use std::rc::Rc;

//------------------------------------------------------------------------------
// ReadHelper - makes up for some missing features of Rust's std io package.
//------------------------------------------------------------------------------
struct ReadHelper {
    reader: Box<Read>,
    unget: Option<char>
}

impl ReadHelper {
    fn new<R: Read + 'static>(r: R) -> ReadHelper {
        let br = BufReader::new(r);
        ReadHelper { reader: Box::new(br), unget: None }
    }

    fn next_char(&mut self) -> std::io::Result<char> {
        if let Some(c) = self.unget.take() {
            Ok(c)
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
        assert!(self.unget.is_none());
        self.unget = Some(c);
    }

    fn read_while(&mut self, pred: &Fn(char) -> bool) -> std::io::Result<String> {
        let mut s = "".to_string();
        loop {
            let c = try!(self.next_char());
            if pred(c) {
                s.push(c);
            }
            else {
                self.unget_char(c);
                return Ok(s)
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
    Str(String),
    Bool(bool),
    OP,
    CP,
    DOT,
    QUOTE,
    Eof
}

struct Tokenizer {
    reader: ReadHelper,
    unget: Option<Token>
}

impl Tokenizer {
    fn new<R: Read + 'static>(r: R) -> Tokenizer {
        Tokenizer { reader: ReadHelper::new(r), unget: None }
    }

    fn read_scheme_id(&mut self) -> std::io::Result<Token> {
        let t = Token::Id(try!(
            self.reader.read_while(&|c: char| c.is_scheme_continue() )));
        Ok(t)
    }

    fn read_number(&mut self) -> std::io::Result<Token> {
        use std::f64;
        let s = try!(
            self.reader.read_while(&|c: char| { c.is_numeric() || c == '.' }));
        if let Ok(f) = s.parse::<f64>() {
            Ok(Token::Num(f))
        } else {
            Ok(Token::Num(f64::NAN))
        }
    }

    fn read_quoted_string(&mut self) -> std::io::Result<Token> {
        let mut sofar : String = "".to_string();
        loop {
            match try!(self.reader.next_char()) {
                '\0' => return Ok(Token::Eof),
                '"' => return Ok(Token::Str(sofar)),
                '\\' => {
                    let esc = try!(self.reader.next_char());
                    if esc == 'n' {
                        sofar.push('\n');
                    }
                    else {
                        sofar.push(esc);
                    }
                },
                c => sofar.push(c)
            }
        }
    }

    fn unget(&mut self, t: Token) {
        assert!(self.unget.is_none());
        self.unget = Some(t);
    }

    fn next(&mut self) -> std::io::Result<Token> {
        if let Some(t) = self.unget.take() {
            return Ok(t);
        }

        loop {
            return match try!(self.reader.next_char()) {
                '(' => Ok(Token::OP),
                ')' => Ok(Token::CP),
                '.' => Ok(Token::DOT),
                '\'' => Ok(Token::QUOTE),
                '\0' => Ok(Token::Eof),
                '\"' => self.read_quoted_string(),
                ';' => {
                    try!(self.reader.read_while(&|c: char| { c != '\n' }));
                    continue;
                },
                '#' => {
                    let bc = try!(self.reader.next_char());
                    if bc == 't' || bc == 'f' {
                        Ok(Token::Bool(bc == 't'))
                    }
                    else {
                        panic!("Unexpected char after #")
                    }
                }
                c if c.is_whitespace() => continue,
                c if c.is_numeric() => {
                    self.reader.unget_char(c);
                    self.read_number()
                },
                c if c.is_scheme_start() => {
                    self.reader.unget_char(c);
                    self.read_scheme_id()
                },
                c => panic!("Illegal character {}", c)
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
    Str(String),
    Bool(bool),
    // This needs to be Rc<(Sexp,Sexp)> as currently any env lookup of a Cons
    // results in a recursive clone of the cell, which isn't how Scheme is
    // supposed to work.  The parser code currently relies on this ownership
    // stuff however to eliminate some object cloning.  It also means that (let
    // ((a (cons 1 2))) (eq? a a)) will be #f :-( ...
    Cons(SCons),
    Closure(Rc<SClosure>),
    Builtin(Rc<Builtin>),
    // It's a hack to make macros first-class objects since they can
    // only be read out of the top-level frame.  TODO: move to a separate
    // macro table used by the analyzer outside of the environment.
    Macro(Rc<SClosure>),
    Nil,
    Eof,
}

use Sexp::{ Num, Id, Cons, Nil };

impl Sexp {
    // Builds a SList out of an Iterator<Item=Sexp>
    fn accumulate<I>(mut iter: I) -> Sexp where I: Iterator<Item=Sexp> {
        if let Some(s) = iter.next() {
            Sexp::new_cons(s, Sexp::accumulate(iter))
        }
        else {
            Nil
        }
    }

    fn new_cons(car: Sexp, cdr: Sexp) -> Sexp {
        Cons(SCons::new(car, cdr))
    }

    fn is_cons(&self) -> bool {
        if let Cons(_) = *self { true } else { false}
    }

    fn is_id(&self, id: &String) -> bool {
        if let Id(ref selfid) = *self {
            id == selfid
        }
        else { false }
    }

    fn to_bool(&self) -> bool {
        if let Sexp::Bool(b) = *self { b } else { true }
    }

    fn id<'a>(&'a self) -> &'a String {
        if let Id(ref s) = *self { s }
        else { panic!("Sexp {} isn't ID", *self); }
    }

    fn id_take(self) -> String {
        if let Id(s) = self { s }
        else { panic!("Sexp {} isn't ID", self); }
    }

    fn car_is_id(&self, id: &str) -> bool {
        if self.is_cons() {
            if let Id(ref s) = *self.car() { s == id }
            else { false }
        }
        else { false }
    }

    // a reference iterator
    fn iter<'a>(&'a self) -> SexpIter<'a> {
        SexpIter { cur : self }
    }

    //
    // Sexp Helper Functions
    //
    fn unwrap_cons<'a>(&'a self) -> &'a SCons {
        if let Cons(ref scons) = *self {
            scons
        }
        else {
            panic!("sexp is not a cons: {}", *self);
        }
    }

    fn take_cons(self) -> SCons {
        if let Cons(scons) = self {
            scons
        }
        else {
            panic!("sexp is not a cons: {}", self);
        }
    }

    fn car<'a>(&'a self) -> &'a Sexp {
        let (ref car, _) = **self.unwrap_cons();
        car
    }

    fn car_take(self) -> Sexp {
        let (car, _) = self.take_cons().take();
        car
    }

    fn cdr<'a>(&'a self) -> &'a Sexp {
        let (_, ref cdr) = **self.unwrap_cons();
        cdr
    }

    fn cdr_take(self) -> Sexp {
        let (_, cdr) = self.take_cons().take();
        cdr
    }

    fn carcdr<'a>(&'a self) -> (&'a Sexp, &'a Sexp) {
        let (ref car, ref cdr) = **self.unwrap_cons();
        (car, cdr)
    }

    fn carcdr_take(self) -> (Sexp, Sexp) {
        let (car, cdr) = self.take_cons().take();
        (car, cdr)
    }
}

impl Display for Sexp {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match *self {
            Num(n) => write!(f, "{}", n),
            Id(ref id) => write!(f, "{}", id),
            Sexp::Str(ref str) => write!(f, "\"{}\"", str),
            Nil => write!(f, "()"),
            Sexp::Closure(_) => write!(f, "<closure>"),
            Sexp::Builtin(_) => write!(f, "<builtin>"),
            Sexp::Macro(_) => write!(f, "<macro>"),
            Sexp::Bool(b) => write!(f, "{}", if b { "#t" } else { "#f" }),
            Cons(_) => {
                let mut it = self.iter();
                try!(write!(f, "({}", it.next().unwrap()));
                for i in it.by_ref() {
                    try!(write!(f, " {}", i));
                }
                if let Nil = *(it.cur) { } else {
                    try!(write!(f, " . {}", it.cur));
                }
                f.write_str(")")
            },
            Sexp::Eof => unreachable!()
        }
    }
}

impl IntoIterator for Sexp {
    type Item = Sexp;
    type IntoIter = SexpMoveIter;
    fn into_iter(self) -> SexpMoveIter {
        SexpMoveIter { cur : self }
    }
}

impl FromIterator<Sexp> for Sexp {
    fn from_iter<I: IntoIterator<Item=Sexp>>(iter: I) -> Sexp {
        Sexp::accumulate(iter.into_iter())
    }
}

impl PartialEq for Sexp {
    fn eq(&self, other: &Sexp) -> bool {
        match *self {
            Num(a) =>
                if let Num(b) = *other { a == b } else { false },
            Id(ref a) =>
                if let Id(ref b) = *other { a == b } else { false },
            Sexp::Str(ref a) =>
                if let Sexp::Str(ref b) = *other { a == b } else { false },
            Sexp::Bool(ref a) =>
                if let Sexp::Bool(ref b) = *other { a == b } else { false },
            Cons(ref a) =>
                if let Cons(ref b) = *other {
                    (&**a as *const (Sexp, Sexp)) == (&**b as *const _)
                } else { false },
            Sexp::Closure(ref a) =>
                if let Sexp::Closure(ref b) = *other {
                    (&**a as *const SClosure) == (&**b as *const _)
                } else { false },
            Sexp::Builtin(ref a) =>
                if let Sexp::Builtin(ref b) = *other {
                    (a as *const Rc<Builtin>) == (b as *const Rc<_>)
                } else { false },
            Nil => if let Nil = *other { true } else { false },
            Sexp::Macro(_) => false,
            Sexp::Eof => if let Sexp::Eof = *other { true } else { false },
        }
    }
}

// sono americano, di seattle ma adesso avito a new york negli stati uniti

struct SexpMoveIter {
    cur: Sexp
}

impl Iterator for SexpMoveIter {
    type Item = Sexp;
    fn next(&mut self) -> Option<Sexp> {
        if self.cur.is_cons() {
            let mut tmp = Nil;
            mem::swap(&mut self.cur, &mut tmp);
            let (car, cdr) = tmp.carcdr_take();
            self.cur = cdr;
            Some(car)
        }
        else {
            None
        }
    }
}

struct SexpIter<'a> {
    cur: &'a Sexp
}

impl<'a> Iterator for SexpIter<'a> {
    type Item = &'a Sexp;
    fn next(&mut self) -> Option<&'a Sexp> {
        match *self.cur {
            Cons(_) => {
                let car = self.cur.car();
                self.cur = self.cur.cdr();
                Some(car)
            },
            _ => None
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


trait Apply {
    fn apply<I: Iterator<Item=Sexp>>(&self, args: I) -> Sexp;
}

#[derive(Debug)]
struct SClosure {
    env: FramePtr,
    arg_names: Vec<String>,
    rest_arg: Option<String>,
    expr: Rc<Expr>
}

impl Apply for SClosure {
    fn apply<I: Iterator<Item=Sexp>>(&self, args: I) -> Sexp {
        let env_closure = new_env(Some(self.env.clone()));
        let mut args_iter = args.fuse(); // TODO: is this necessary?

        for arg_name in self.arg_names.iter() {
            if let Some(sexp) = args_iter.next() {
                env_closure.borrow_mut().set(arg_name.clone(), sexp);
            }
            else {
                env_closure.borrow_mut().set(arg_name.clone(), Nil);
            }
        }

        if let Some(ref id) = self.rest_arg {
            let val = Sexp::accumulate(args_iter);
            env_closure.borrow_mut().set(id.clone(), val);
        }

        let ref expr = self.expr;
        expr(env_closure)
    }
}

type BuiltinPtr = Box<Fn(&mut Iterator<Item=Sexp>) -> Sexp>;

struct Builtin {
    name: &'static str,
    func: BuiltinPtr
}

impl Builtin {
    fn new(name: &'static str, func: BuiltinPtr) -> Builtin {
        Builtin { name: name, func: func }
    }
}

impl Apply for Builtin {
    fn apply<I: Iterator<Item=Sexp>>(&self, mut args: I) -> Sexp {
        (self.func)(&mut args)
    }
}

impl Debug for Builtin {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "<Builtin {}>", self.name)
    }
}

//------------------------------------------------------------------------------
// Parser
//------------------------------------------------------------------------------
struct Parser {
    tokenizer: Tokenizer
}

impl Parser {
    fn new<R: Read + 'static>(r : R) -> Parser {
        Parser { tokenizer : Tokenizer::new(r) }
    }

    fn next_sexp(&mut self) -> std::io::Result<Sexp> {
        Ok(match try!(self.tokenizer.next()) {
            Token::Id(str) => Id(str),
            Token::Num(n)  => Num(n),
            Token::Bool(b) => Sexp::Bool(b),
            Token::Eof     => Sexp::Eof,
            Token::QUOTE   =>
                Sexp::new_cons(Id("quote".to_string()),
                               Sexp::new_cons(try!(self.next_sexp()), Nil)),
            Token::OP => try!(self.next_sexp_list(false)),
            Token::CP | Token::DOT => panic!("unexpected token"),
            Token::Str(str) => Sexp::Str(str)
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
type FramePtr = Rc<RefCell<Frame>>;

fn new_env(next: Option<FramePtr>) -> FramePtr {
    Rc::new(RefCell::new(Frame::new(next)))
}

fn borrow_hack<'a, T>(rc: &'a RefCell<T>) -> &'a T {
    // Works around a Rust 1.0 problem where the borrow checker is too
    // conservative in the presence of a RefCell (see
    // stackoverflow.com/questions/30281664)
    let t: &T = &(*(rc.borrow()));
    unsafe { std::mem::transmute(t) }
}

fn borrow_mut_hack<'a, T>(rc: &'a RefCell<T>) -> &'a mut T {
    let t: &T = &(*(rc.borrow_mut()));
    unsafe { std::mem::transmute(t) }
}

#[derive(Debug)]
struct Frame {
    symtab: HashMap<String, Sexp>,
    next: Option<FramePtr>
}

impl Frame {
    fn new(next: Option<FramePtr>) -> Frame {
        Frame { symtab : HashMap::new(), next: next }
    }

    fn find<'a>(&'a self, sym: &String) -> Option<&'a Frame> {
        if self.symtab.contains_key(sym) {
            Some(&self)
        }
        else if let Some(ref next_frame) = self.next {
            borrow_hack(next_frame).find(sym)
        }
        else {
            None
        }
    }

    fn lookup<'a>(&'a self, sym: &String) -> Option<&'a Sexp> {
        self.symtab.get(sym)
    }

    fn find_and_lookup<'a>(&'a self, sym: &String) -> Option<&'a Sexp> {
        if let Some(frame) = self.find(sym) {
            frame.lookup(sym)
        }
        else {
            None
        }
    }

    fn set(&mut self, sym: String, val: Sexp) {
        self.symtab.insert(sym, val);
    }

    fn all_sexps<'a>(&'a mut self) -> Box<Iterator<Item=&'a mut Sexp> + 'a> {
        let mut it = self.symtab.iter_mut().map(|i| i.1);
        if let Some(ref next_frame) = self.next {
            Box::new(it.chain(borrow_mut_hack(next_frame).all_sexps()))
        }
        else {
            Box::new(it)
        }
    }
}

#[allow(dead_code)]
fn test_env() {
    let mut f = Frame::new(None);
    f.set("key".to_string(), Id("val".to_string()));
    f.set("key2".to_string(), Num(42f64));

    {
        let v = f.lookup(&"key2".to_string());
        println!("{:?}", v);
    }

    // If the scope above wasn't closed, then this borrow
    // would be invalid.
    f.set("key2".to_string(), Id("oops".to_string()));
}

//------------------------------------------------------------------------------
// Semantic Analyzer
//------------------------------------------------------------------------------

//
// CapHack exists to work around the problem in Rust 1.0 where you cannot return
// a FnOnce from a function.  This is because you cannot unbox a FnOnce.  There
// exists an untable feature called FnBox, so I can move to that in a future
// release.  So CapHack works by using a RefCell to mutate immutable state, thus
// forcing a move of the captured variable.  There still is runtime overhead
// in the form of checking the states of the RefCell and Option, but it is
// probably cheaper than a superflous clone that you would otherwise need.
//
#[allow(dead_code)]
struct CapHack<T> {
    wrap_t: RefCell<Option<T>>
}

#[allow(dead_code)]
impl<T> CapHack<T> {
    fn new(t: T) -> CapHack<T> {
        CapHack { wrap_t: RefCell::new(Some(t)) }
    }

    fn take(&self) -> T {
        self.wrap_t.borrow_mut().take().unwrap()
    }
}

type Expr = Box<Fn(FramePtr) -> Sexp>;

impl Debug for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        f.write_str("<Expr>")
    }
}

fn expand_macros(mut s: Sexp, env: &Frame) -> Sexp {
    loop {
        let (did_stuff, expanded) = expand_macros_once(s, env);
        if !did_stuff { return expanded }
        s = expanded
    }
}

fn expand_macros_once(s: Sexp, env: &Frame) -> (bool, Sexp) {
    if s.is_cons() && !(s.car_is_id("quote")) {
        let mut opt_mac = None;
        if let Id(ref id) = *s.car() {
            if let Some(ref val) = env.find_and_lookup(id) {
                if let Sexp::Macro(ref rcmac) = **val {
                    opt_mac = Some(rcmac.clone())
                }
            }
        }
        if let Some(rcmac) = opt_mac {
            (true, rcmac.apply(s.cdr_take().into_iter()))
        }
        else {
            let (ocar, ocdr) = s.carcdr_take();
            let (did_car, car) = expand_macros_once(ocar, env);
            let (did_cdr, cdr) = expand_macros_once(ocdr, env);
            (did_car || did_cdr, Sexp::new_cons(car, cdr))
        }
    }
    else {
        (false, s)
    }
}

fn analyze(s: Sexp) -> Expr {
    match s {
        Num(_) | Nil | Sexp::Str(_) | Sexp::Bool(_) => {
            Box::new(move |_| s.clone())
        },
        Sexp::Closure(_) | Sexp::Builtin(_) | Sexp::Macro(_) | Sexp::Eof =>
            panic!("unexpected sexp in analyze"),
        Id(sid) => analyze_env_lookup(sid),
        Cons(_) => {
            let (car, cdr) = s.carcdr_take();
            if let Id(id) = car {
                if id == "lambda" { analyze_lambda(cdr) }
                else if id == "quote" { analyze_quote(cdr) }
                else if id == "define" { analyze_define(cdr) }
                else if id == "define-macro" { analyze_define_macro(cdr) }
                else if id == "and" { analyze_and(cdr) }
                else if id == "or" { analyze_or(cdr) }
                else { analyze_application(Sexp::new_cons(Id(id), cdr)) }
            }
            else {
                analyze_application(Sexp::new_cons(car, cdr))
            }
        }
    }
}

fn analyze_env_lookup(id: String) -> Expr {
    Box::new(move |env| {
        if let Some(val) = env.borrow().find_and_lookup(&id) {
            return val.clone();
        }
        panic!("Undefined variable: {}", id)
    })
}

// s: ((funcname arg1 arg2) body) or else (varname value)
fn analyze_define(s: Sexp) -> Expr {
    let id;
    let val;
    let (car, cdr) = s.carcdr_take();
    if car.is_cons() {
        let (caar, cdar) = car.carcdr_take();
        id = caar.id_take();
        val = analyze_lambda(Sexp::new_cons(cdar, cdr));
    }
    else {
        id = car.id_take();
        val = analyze(cdr.car_take());
    }

    Box::new(move |env| {
        let valval = val(env.clone());
        env.borrow_mut().set(id.clone(), valval);
        Id(id.clone())
    })
}

fn analyze_define_macro(s: Sexp) -> Expr {
    let (car, cdr) = s.carcdr_take();
    let macro_name = car.id_take();
    let evalue = analyze(cdr.car_take());
    Box::new(move |env| {
        if let Sexp::Closure(rsc) = evalue(env.clone()) {
            env.borrow_mut().set(macro_name.clone(), Sexp::Macro(rsc));
            Id(macro_name.clone())
        }
        else {
            panic!("Macro defined to be non-closure.")
        }
    })
}

fn analyze_quote(s: Sexp) -> Expr {
    let ccar = s.car_take();
    Box::new(move |_| ccar.clone())
}

fn analyze_lambda(s: Sexp) -> Expr {
    // car is args and cdr is body
    let (car, cdr) = s.carcdr_take();
    let mut arg_cons_iter = car.into_iter();
    let arg_names: Vec<_> = arg_cons_iter.by_ref().map(|i| {
        if let Id(arg) = i { arg }
        else { panic!("non-id argument to lambda") }
    }).collect();

    let rest_arg =
        if let Id(id) = arg_cons_iter.cur {
            Some(id)
        } else { None };

    let expr = Rc::new(analyze_body(cdr));

    Box::new(move |env| {
        // each time a lambda expression is evaluated it must return a new
        // closure since each closure can have its own environment, we refcount
        // the expr and could do the same for the args too
        Sexp::Closure(Rc::new(SClosure {
            env: env.clone(),
            arg_names: arg_names.clone(),
            rest_arg: rest_arg.clone(),
            expr: expr.clone()
        }))
    })
}

fn analyze_body(sbody: Sexp) -> Expr {
    let exprs: Vec<_> = sbody.into_iter().map(|i| {
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

fn funcall<T>(func: Sexp, args: T) -> Sexp where T: Iterator<Item=Sexp> {
    if let Sexp::Closure(sc) = func {
        sc.apply(args)
    }
    else if let Sexp::Builtin(b) = func {
        b.apply(args)
    }
    else {
        panic!("funcall on non function object {}", func);
    }
}

fn analyze_application(sexp: Sexp) -> Expr {
    let (car, cdr) = sexp.carcdr_take();
    let efunc = analyze(car);
    let eargs: Vec<_> = cdr.into_iter().map(|i| analyze(i)).collect();

    Box::new(move |env| {
        let func = efunc(env.clone());
        let args_iter = eargs.iter().map(|i| i(env.clone()));
        funcall(func, args_iter)
    })
}

fn analyze_and(sexp: Sexp) -> Expr {
    let eargs: Vec<_> = sexp.into_iter().map(|i| analyze(i)).collect();
    Box::new(move |env| {
        let mut last = Nil;
        for i in &eargs {
            last = i(env.clone());
            if !last.to_bool() {
                return Sexp::Bool(false);
            }
        }
        last
    })
}

fn analyze_or(sexp: Sexp) -> Expr {
    let eargs: Vec<_> = sexp.into_iter().map(|i| analyze(i)).collect();
    Box::new(move |env| {
        let mut last;
        for i in &eargs {
            last = i(env.clone());
            if last.to_bool() {
                return last;
            }
        }
        Sexp::Bool(false)
    })
}

//------------------------------------------------------------------------------
// Simple stop-and-copy GC
//------------------------------------------------------------------------------

#[derive(Debug, Copy, Clone, PartialEq)]
struct SCons {
    pos: usize,
}

enum Cell {
    Cons((Sexp, Sexp)),
    Forward(usize),
}

impl Cell {
    fn new(car: Sexp, cdr: Sexp) -> Cell {
        Cell::Cons((car, cdr))
    }

    fn get<'a>(&'a self) -> &'a (Sexp, Sexp) {
        if let Cell::Cons(ref p) = *self { p }
        else { unreachable!() }
    }

    fn get_mut<'a>(&'a mut self) -> &'a mut (Sexp, Sexp) {
        if let Cell::Cons(ref mut p) = *self { p }
        else { unreachable!() }
    }

    fn take(self) -> (Sexp, Sexp) {
        if let Cell::Cons(p) = self { p }
        else { unreachable!() }
    }

    fn forward(&mut self, pos: usize) -> (Sexp, Sexp) {
        let mut tmp = Cell::Forward(pos);
        std::mem::swap(self, &mut tmp);
        tmp.take()
    }
}

struct Conses {
    v: Vec<Option<Cell>>,
    gen: u32
}

impl Conses {
    fn new(gen: u32) -> Conses {
        Conses { v: Vec::new(), gen: gen }
    }

    fn push(&mut self, car: Sexp, cdr: Sexp) -> usize {
        let pos = self.v.len();
        self.v.push(Some(Cell::new(car, cdr)));
        pos
    }

    fn len(&self) -> usize { self.v.len() }

    fn get_cell_mut<'a>(&'a mut self, pos: usize) -> &'a mut Cell {
        self.v[pos].as_mut().unwrap()
    }

    fn get_cell_ref<'a>(&'a self, pos: usize) -> &'a Cell {
        self.v[pos].as_ref().unwrap()
    }

    fn forward(&mut self, src: usize, dst: usize) -> (Sexp, Sexp) {
        self.v[src].as_mut().unwrap().forward(dst)
    }

    fn get_mut_ref<'a>(&'a mut self, pos: usize) -> &'a mut (Sexp, Sexp) {
        self.v[pos].as_mut().unwrap().get_mut()
    }

    fn get_ref<'a>(&'a self, pos: usize) -> &'a (Sexp, Sexp) {
        self.v[pos].as_ref().unwrap().get()
    }

    fn get_clone(&self, pos: usize) -> (Sexp, Sexp) {
        self.get_ref(pos).clone()
    }

    fn take(&mut self, pos: usize) -> (Sexp, Sexp) {
        self.v[pos].take().unwrap().take()
    }

    fn collect(&mut self, env: FramePtr) {
        let mut tmp = GcEpoch::run(self, env);
        std::mem::swap(self, &mut tmp);
    }
}

thread_local!(
    static CONSES : RefCell<Conses> = RefCell::new(Conses::new(0)));

impl SCons {
    fn new(car: Sexp, cdr: Sexp) -> SCons {
        CONSES.with(|conses| {
            let pos = conses.borrow_mut().push(car, cdr);
            SCons { pos: pos }
        })
    }

    fn collect(env: FramePtr) {
        CONSES.with(|conses| {
            conses.borrow_mut().collect(env)
        })
    }

    fn get_ref<'a>(&self) -> &'a (Sexp, Sexp) {
        CONSES.with(|conses| {
            let c = conses.borrow();
            // this is very unrusty, might be best to return
            // something like a Ref which locks the heap and prevents
            // GCs while there are outstanding refs
            unsafe { std::mem::transmute(c.get_ref(self.pos)) }
        })
    }

    fn get_clone(&self) -> (Sexp, Sexp) {
        CONSES.with(|conses| {
            conses.borrow().get_clone(self.pos)
        })
    }

    fn take(self) -> (Sexp, Sexp) {
        CONSES.with(|conses| {
            conses.borrow_mut().take(self.pos)
        })
    }
}

impl Deref for SCons {
    type Target = (Sexp, Sexp);

    fn deref<'a>(&'a self) -> &'a (Sexp, Sexp) {
        self.get_ref()
    }
}

// Represents a single collection.
struct GcEpoch<'a> {
    src: &'a mut Conses,
    dst: Conses,
    frame_log: HashSet<*const RefCell<Frame>>
}

impl<'a> GcEpoch<'a> {
    fn run(src: &'a mut Conses, env: FramePtr) -> Conses {
        let next_gen = src.gen + 1;
        let mut e = GcEpoch {
            src: src,
            dst: Conses::new(next_gen),
            frame_log: HashSet::new()
        };
        e.frame_log.insert(&*env as *const RefCell<Frame>);
        e.copy_all(&mut *env.borrow_mut().all_sexps());

        println!("============= GC COMPLETE =================");
        for (idx, ref val) in e.src.v.iter().enumerate() {
            println!("{}: {}", idx, match **val {
                Some(Cell::Cons(_)) => "garbage".to_string(),
                Some(Cell::Forward(new)) => format!("moved to {}", new),
                None => "taken".to_string()
            });
        }
        println!("Old heap: {} cells", e.src.len());
        println!("New heap: {} cells", e.dst.len());

        e.dst
    }

    fn copy_all(&mut self, live_it: &mut Iterator<Item=&mut Sexp>) {
        for mut sexp in live_it {
            self.copy_dispatch(sexp);
        }
    }

    fn copy_dispatch(&mut self, sexp: &mut Sexp) {
        match *sexp {
            Cons(ref mut scons) => self.copy_one(scons),
            Sexp::Closure(ref mut sclosure) => {
                if (self.frame_log.insert(&*(sclosure.env))) {
                    println!("<Expanding frame>");
                    let mut frame = sclosure.env.borrow_mut();
                    let mut bit = frame.all_sexps();
                    self.copy_all(&mut *bit);
                }
                else {
                    println!("<Skipping frame>")
                }
            }
            _ => ()
        }
    }

    fn copy_one(&mut self, scons: &mut SCons) {
        // if it's already been forwarded
        if let Cell::Forward(new_pos) = *self.src.get_cell_ref(scons.pos) {
            scons.pos = new_pos
        }
        else {
            println!("Cell {} saved.", scons.pos);
            let forward = self.dst.len();
            let (mut car, mut cdr) = self.src.v[scons.pos].as_mut().unwrap().forward(forward);

            self.copy_dispatch(&mut car);
            self.copy_dispatch(&mut cdr);

            let new_pos = self.dst.push(car, cdr);
            assert_eq!(forward, new_pos);
            scons.pos = new_pos;
        }
    }
}


//------------------------------------------------------------------------------
// Builtin Functions
//------------------------------------------------------------------------------
fn install_builtins(env: &FramePtr) {
    // TODO:  A macro might make this better!
    let b : Vec<(&str, Box<Fn(&mut Iterator<Item=Sexp>) ->  Sexp>)> = vec!(
        ("+", mathy(Box::new(|s, i| s + i))),
        ("-", mathy(Box::new(|s, i| s - i))),
        ("/", mathy(Box::new(|s, i| s / i))),
        ("*", mathy(Box::new(|s, i| s * i))),
        ("cons", Box::new(
            |it| Sexp::new_cons(it.next().unwrap(),
                                it.next().unwrap()))),
        ("car", Box::new(|it| it.next().unwrap().car().clone())),
        ("cdr", Box::new(|it| it.next().unwrap().cdr().clone())),
        ("apply", Box::new(|args| scheme_apply(args))),
        ("eq?", Box::new(
            |it| Sexp::Bool(it.next().unwrap() == it.next().unwrap()))),
        ("null?", Box::new(
            |it| Sexp::Bool({
                if let Nil = it.next().unwrap() { true } else { false }
            }))),
        ("pair?", Box::new(|it| Sexp::Bool(it.next().unwrap().is_cons()))),
        );

    for i in b {
        env.borrow_mut().set(
            i.0.to_string(),
            Sexp::Builtin(Rc::new(Builtin::new(i.0, i.1))));
    }
}

//
// (apply func arg1 arg2 (arg3 ... argn)
//
// is equlivalent to:
//
// (apply func (arg1 arg2 arg3 ... argn))
//
fn scheme_apply(it: &mut Iterator<Item=Sexp>) -> Sexp {
    let mut args: Vec<_> = it.collect();
    let len = args.len();
    let rest_args = args.remove(len - 1);

    let mut args_it = args.into_iter();

    // the first arg of apply is the function to call
    let func = args_it.next().unwrap();

    // the inner n-2 args are supplied to the function
    let inner_args_it = args_it.take(len - 2);

    // the final arg must be a SList which is appended to args from above
    let args_it = inner_args_it.chain(rest_args);

    let args_vec : Vec<Sexp> = args_it.collect();
    funcall(func.clone(), args_vec.into_iter())
}

fn get_num(s: &Sexp) -> f64 {
    if let Num(n) = *s { n } else { unreachable!() }
}

fn mathy(f: Box<Fn(f64, f64) -> f64>) -> Box<Fn(&mut Iterator<Item=Sexp>)->Sexp> {
    Box::new(move |args| {
        let init = get_num(&args.next().unwrap());
        Num(args.fold(init, |s, i| f(s, get_num(&i))))
    })
}

//------------------------------------------------------------------------------
fn interpret<R: Read + 'static>(read: R, env: FramePtr) -> std::io::Result<bool> {
    let mut parser = Parser::new(read);
    loop {
        SCons::collect(env.clone());
        let sexp = try!(parser.next_sexp());
        if let Sexp::Eof = sexp { break; }

        if sexp.car_is_id("import") {
            if let Sexp::Str(file_name) = sexp.cdr_take().car_take() {
                try!(process(file_name.clone(), env.clone()));
            }
            else {
                panic!("illegal import directive");
            }
        }
        else {
            println!(">> {}", sexp);
            let expanded = expand_macros(sexp, &env.borrow());
            let expr = analyze(expanded);
            let result = expr(env.clone());
            println!("{}", result);
        }
    }
    Ok(true)
}

//------------------------------------------------------------------------------
fn process(file_name: String, env: FramePtr) -> std::io::Result<bool> {
    if file_name == "--" {
        let sin = stdin();
        interpret(sin, env)
    }
    else {
        interpret(try!(File::open(file_name)), env)
    }
}

//------------------------------------------------------------------------------
fn main() {
    println!("Welcome to Scheme!");

    let env = new_env(None);
    install_builtins(&env);

    let args = std::env::args();

    if args.len() < 2 {
        if let Err(err) = process("--".to_string(), env) {
            panic!("Error: {}", err);
        }
    }
    else {
        for arg in args.skip(1) {
            if let Err(err) = process(arg, env.clone()) {
                panic!("Error: {}", err);
            }
        }
    }
}
