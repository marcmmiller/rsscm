use std::mem;
use std::cell::{Cell, RefCell, Ref, UnsafeCell};
use std::collections::HashMap;
use std::convert::Into;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::prelude::*;
use std::io::{BufReader, stdin};
use std::iter::{FromIterator, IntoIterator, Peekable};
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;
use std::sync::Mutex;

use gc::CellRef;


// TODO:
//  - eqv?
//  - this program cause a gc panic: (define a '(2)) (set-cdr! a a) (for-all even? a)
//  - gc panic may also be caused by assertion in lib.scm failing
//  - error propagation with stack trace
//  - fix gc architecture
//  - test with [test] attribute
//  - fix macro thing - it shouldn't be first-class
//  - rationalize partialeq
//  - move everything to modules
//  - quasiquotation
//  - 'void' type that is returned by 'define', 'set!', 'if without else', etc.
//    plus a void builtin function that just evaluates its arg and returns void

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
#[derive(Debug)]
enum ErrorType {
    IOError(std::io::Error),
    Msg(String),
    SMsg(&'static str),
}

#[derive(Debug)]
struct SchemeError {
    ty: ErrorType,
    inner: Option<Box<SchemeError>>,
    // Note: this is Option<Sexp> to work around an ICE bug in the rustc 1.0.0
    // Move to just plain Sexp when doing so doesn't ICE.
    stack: Vec<Option<Sexp>>
}

impl std::error::Error for SchemeError {
    fn description(&self) -> &str {
        match self.ty {
            ErrorType::IOError(ref e) => e.description(),
            ErrorType::Msg(ref s) => s,
            ErrorType::SMsg(s) => s
        }
    }

    fn cause<'a>(&'a self) -> Option<&'a std::error::Error> {
        if let Some(ref b) = self.inner {
            Some(&**b)
        }
        else {
            None
        }
    }
}

impl Display for SchemeError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        try!(match self.ty {
            ErrorType::IOError(ref e) => (e as &Display).fmt(f),
            ErrorType::Msg(ref s) => write!(f, "{}", s),
            ErrorType::SMsg(s) => write!(f, "{}", s)
        });
        if let Some(ref inner) = self.inner {
            write!(f, "\nInner: {}", inner)
        }
        else {
            Ok(())
        }
    }
}

impl From<std::io::Error> for SchemeError {
    fn from(e: std::io::Error) -> SchemeError {
        SchemeError { ty: ErrorType::IOError(e), inner: None, stack: vec!() }
    }
}

impl From<String> for SchemeError {
    fn from(s: String) -> SchemeError {
        SchemeError { ty: ErrorType::Msg(s), inner: None, stack: vec!() }
    }
}

impl From<&'static str> for SchemeError {
    fn from(s: &'static str) -> SchemeError {
        SchemeError { ty: ErrorType::SMsg(s), inner: None, stack: vec!() }
    }
}

type SResult<T> = Result<T, SchemeError>;

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

    fn next(&mut self) -> SResult<Token> {
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
                '\"' => Ok(try!(self.read_quoted_string())),
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
                        Err("Unexpected char after #".into())
                    }
                }
                c if c.is_whitespace() => continue,
                c if c.is_numeric() => {
                    self.reader.unget_char(c);
                    Ok(try!(self.read_number()))
                },
                c if c.is_scheme_start() => {
                    self.reader.unget_char(c);
                    Ok(try!(self.read_scheme_id()))
                },
                c => Err(format!("Illegal character {}", c).into())
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
pub enum Sexp {
    Num(f64),
    Id(Atom),
    Str(String),
    Bool(bool),
    Cons(CellRef),  // Garbage-collected
    Closure(Rc<SClosure>),
    Builtin(Rc<Builtin>),
    // It's a hack to make macros first-class objects since they can
    // only be read out of the top-level frame.  TODO: move to a separate
    // macro table used by the analyzer outside of the environment.
    Macro(Rc<SClosure>),
    Trampoline(Rc<SClosure>),
    Nil,
    Eof,
}

use Sexp::{ Num, Id, Cons, Nil };

impl Sexp {
    // Builds a SList out of an Iterator<Item=Sexp>
    fn accumulate<I>(mut iter: I) -> Sexp where I: Iterator<Item=Sexp> {
        if let Some(s) = iter.next() {
            let head = Sexp::new_cons(s, Nil);
            {
                let mut last = &head;
                for i in iter {
                    last.set_cdr(Sexp::new_cons(i, Nil));
                    last = last.cdr().unwrap();
                }
            }
            head
        }
        else {
            Nil
        }
    }

    // the trait "clone" function doesn't actually create new cons cells, but instead
    // relies on the gc
    fn deep_clone(&self) -> Sexp {
        if (self.is_cons()) {
            let (rcar, rcdr) = self.carcdr().unwrap();
            Sexp::new_cons(rcar.deep_clone(), rcdr.deep_clone())
        }
        else {
            self.clone()
        }
    }

    fn new_cons(car: Sexp, cdr: Sexp) -> Sexp {
        Cons(CellRef::new(car, cdr))
    }

    fn is_nil(&self) -> bool {
        if let Nil = *self { true } else { false }
    }

    fn is_cons(&self) -> bool {
        if let Cons(_) = *self { true } else { false}
    }

    fn is_id(&self, id: &String) -> bool {
        if let Id(ref selfid) = *self {
            selfid.is_str(id)
        }
        else { false }
    }

    fn to_bool(&self) -> bool {
        if let Sexp::Bool(b) = *self { b } else { true }
    }

    fn id_take(self) -> Atom {
        if let Id(a) = self { a }
        else { panic!("Sexp {} isn't ID", self); }
    }

    fn num(&self) -> SResult<f64> {
        if let Num(f) = *self { Ok(f) }
        else { Err(format!("Sexp {} is not a number", *self).into()) }
    }

    fn car_is_id(&self, id: &str) -> bool {
        if self.is_cons() {
            if let Id(ref a) = *self.car().unwrap() { a.is_sstr(id) }
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
    fn unwrap_cons<'a>(&'a self) -> SResult<&'a CellRef> {
        if let Cons(ref scons) = *self {
            Ok(scons)
        }
        else {
            Err(format!("sexp is not a cons: {}", *self).into())
        }
    }

    fn take_cons(self) -> SResult<CellRef> {
        if let Cons(scons) = self {
            Ok(scons)
        }
        else {
            Err(format!("sexp is not a cons: {}", self).into())
        }
    }

    fn car<'a>(&'a self) -> SResult<&'a Sexp> {
        let (ref car, _) = **try!(self.unwrap_cons());
        Ok(car)
    }

    fn car_take(self) -> SResult<Sexp> {
        let (car, _) = try!(self.take_cons()).take();
        Ok(car)
    }

    fn cdr<'a>(&'a self) -> SResult<&'a Sexp> {
        let (_, ref cdr) = **try!(self.unwrap_cons());
        Ok(cdr)
    }

    fn cdr_take(self) -> SResult<Sexp> {
        let (_, cdr) = try!(self.take_cons()).take();
        Ok(cdr)
    }

    fn carcdr<'a>(&'a self) -> SResult<(&'a Sexp, &'a Sexp)> {
        let (ref car, ref cdr) = **try!(self.unwrap_cons());
        Ok((car, cdr))
    }

    fn carcdr_take(self) -> SResult<(Sexp, Sexp)> {
        let (car, cdr) = try!(self.take_cons()).take();
        Ok((car, cdr))
    }

    fn set_car(&self, s: Sexp) -> SResult<Sexp> {
        if let Cons(ref cr) = *self {
            cr.set_car(s);
            Ok(Nil)
        }
        else {
            Err(format!("sexp is not a cons {}", self).into())
        }
    }

    fn set_cdr(&self, s: Sexp) -> SResult<Sexp> {
        if let Cons(ref cr) = *self {
            cr.set_cdr(s);
            Ok(Nil)
        }
        else {
            Err(format!("sexp is not a cons {}", self).into())
        }
    }
}

impl Display for Sexp {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match *self {
            Num(n) => write!(f, "{}", n),
            Id(ref id) => write!(f, "{}", id),
            Sexp::Str(ref str) => write!(f, "\"{}\"", str),
            Nil => write!(f, "()"),
            Sexp::Closure(ref sc) => (sc as &Display).fmt(f),
            Sexp::Builtin(ref bi) => (bi as &Display).fmt(f),
            Sexp::Macro(_) => write!(f, "<macro>"),
            Sexp::Trampoline(_) => write!(f, "<trampoline>"),
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
            Sexp::Trampoline(_) => false,
            Sexp::Eof => if let Sexp::Eof = *other { true } else { false },
        }
    }
}

// sono americano, di seattle ma adesso avito a new york negli stati uniti

pub struct SexpMoveIter {
    cur: Sexp
}

impl Iterator for SexpMoveIter {
    type Item = Sexp;
    fn next(&mut self) -> Option<Sexp> {
        if self.cur.is_cons() {
            let mut tmp = Nil;
            mem::swap(&mut self.cur, &mut tmp);
            let (car, cdr) = tmp.carcdr_take().unwrap();
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
                let car = self.cur.car().unwrap();
                self.cur = self.cur.cdr().unwrap();
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
    fn apply<I: Iterator<Item=SResult<Sexp>>>(
        &self, args: I, ctx: Rc<IntCtx>) -> SResult<Sexp>;
}

#[derive(Debug)]
pub struct SClosure {
    env: FramePtr,
    arg_names: Vec<Atom>,
    rest_arg: Option<Atom>,
    expr: Rc<Expr>,
    opt_name: Option<Atom>  // for tracing purposes
}

impl Apply for SClosure {
    fn apply<I: Iterator<Item=SResult<Sexp>>>(
        &self, args: I, ctx: Rc<IntCtx>) -> SResult<Sexp>
    {
        let env_closure = new_env(Some(self.env.clone()));
        let mut args_iter = args.fuse(); // TODO: is this necessary?

        // TODO: when gc supports stack-walking this will become unnessary
        self.env.add_temps(|tc| {
            // make sure the closure's env doesn't get collected
            tc.add(Sexp::Closure(Rc::new(SClosure {
                env: env_closure.clone(),
                arg_names: vec!(),
                rest_arg: None,
                expr: Rc::new(Expr::new(|_| Ok(Nil))),
                opt_name: Some("call frame".into())
            })));

            for arg_name in self.arg_names.iter() {
                if let Some(srsexp) = args_iter.next() {
                    env_closure.insert(arg_name.clone(), try!(srsexp));
                }
                else {
                    env_closure.insert(arg_name.clone(), Nil);
                }
            }

            if let Some(ref id) = self.rest_arg {
                let mut v : Vec<Sexp> = vec!();
                while let Some(rssexp) = args_iter.next() {
                    v.push(try!(rssexp));
                }
                let val = Sexp::accumulate(v.into_iter());
                env_closure.insert(id.clone(), val);
            }

            let ref expr = self.expr;
            expr.call(env_closure)
        })
    }
}

impl Display for SClosure {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        if let Some(ref name) = self.opt_name {
            write!(f, "<{}>", name)
        }
        else {
            write!(f, "<anonymous>")
        }
    }
}

type BuiltinPtr = Box<Fn(&mut Iterator<Item=Sexp>) -> SResult<Sexp>>;

pub struct Builtin {
    name: &'static str,
    func: BuiltinPtr
}

impl Builtin {
    fn new(name: &'static str, funk: BuiltinPtr) -> Builtin {
        Builtin { name: name, func: funk }
    }
}

impl Apply for Builtin {
    fn apply<I: Iterator<Item=SResult<Sexp>>>(
        &self, mut args: I, ctx: Rc<IntCtx>) -> SResult<Sexp>
    {
        // convert from Iterator<Item=SResult<Sexp>> to Iterator<Sexp> via vector
        let mut v : Vec<Sexp> = vec!();
        while let Some(rssexp) = args.next() {
            let sexp = try!(rssexp);
            v.push(sexp);
        }
        (self.func)(&mut v.into_iter())
    }
}

impl Debug for Builtin {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "<Builtin {}>", self.name)
    }
}

impl Display for Builtin {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "<Builtin {}>", self.name)
    }
}

//------------------------------------------------------------------------------
// Atom Table
//------------------------------------------------------------------------------
struct AtomTable {
    strmap: HashMap<String, usize>,
    atoms: Vec<String>
}

impl AtomTable {
    fn new() -> AtomTable {
        AtomTable {
            strmap: HashMap::new(),
            atoms: vec!()
        }
    }

    fn atom_for_str(&mut self, s: &str) -> Atom {
        self.atom_for_string(s.to_string())
    }

    fn atom_for_string(&mut self, s: String) -> Atom {
        {
            if let Some(idx) = self.strmap.get(&s) {
                return Atom { index: *idx }
            }
        }
        let a = Atom { index: self.atoms.len() };
        self.atoms.push(s.clone());
        // TODO: would be great to put string refs in here instead of the extra clone
        self.strmap.insert(s, a.index);
        a
    }

    fn lookup<'a>(&'a self, a: &Atom) -> &'a String {
        &self.atoms[a.index]
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Atom {
    index: usize
}

// Hack because we don't have StaticMutex...
// ** Unsafe operations!  Yee-haw! **
struct GlobalHack {
    c: Option<UnsafeCell<AtomTable>>
}

#[allow(mutable_transmutes)]

impl GlobalHack {
    fn get<'a>(&'a self) -> &'a mut AtomTable {
        // delay init the atomtable
        if (self.c.is_none()) {
            unsafe {
                // oh yeah! cast away const because we can't have a mut static
                let me: &'a mut Self = std::mem::transmute(self);
                me.c = Some(UnsafeCell::new(AtomTable::new()));
            }
        }
        unsafe {
            std::mem::transmute(self.c.as_ref().unwrap().get())
        }
    }
}

unsafe impl std::marker::Sync for GlobalHack { }

static ATOMS: GlobalHack = GlobalHack { c: None };

impl Atom {
    fn for_string(s: String) -> Atom {
        ATOMS.get().atom_for_string(s)
    }

    fn for_str(s: &str) -> Atom {
        Atom::for_string(s.to_string())
    }

    fn to_string(&self) -> String {
        ATOMS.get().lookup(self).clone()
    }

    fn is_str(&self, s: &String) -> bool {
        ATOMS.get().lookup(self) == s
    }

    fn is_sstr(&self, s: &str) -> bool {
        ATOMS.get().lookup(self) == s
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.to_string())
    }
}

impl Hash for Atom {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.index.hash(state)
    }
}

impl<'a> From<&'a str> for Atom {
    fn from(s: &'a str) -> Atom {
        Atom::for_str(s)
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

    fn next_sexp(&mut self) -> SResult<Sexp> {
        Ok(match try!(self.tokenizer.next()) {
            Token::Id(str) => Id(Atom::for_string(str)),
            Token::Num(n)  => Num(n),
            Token::Bool(b) => Sexp::Bool(b),
            Token::Eof     => Sexp::Eof,
            Token::QUOTE   =>
                Sexp::new_cons(Id("quote".into()),
                               Sexp::new_cons(try!(self.next_sexp()), Nil)),
            Token::OP => try!(self.next_sexp_list(false)),
            Token::CP | Token::DOT => return Err("unexpected token".into()),
            Token::Str(str) => Sexp::Str(str)
        })
    }

    fn next_sexp_list(&mut self, dot_allowed : bool) -> SResult<Sexp> {
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
    use std::ops::DerefMut;
    let mut borrowed = rc.borrow_mut();
    let t: &mut T = borrowed.deref_mut();
    unsafe { std::mem::transmute(t) }
}

//
// Represents a single frame in a Scheme environment.  All top-level defines
// create bindings in the root frame.  Subframes are created by lambdas to store
// the bindings for their arguments, and defines executed within the scope of
// those lambdas modify those frames.  Subframes can contain subframes
// recursively, and each lambda executes in the context of its subframe, but its
// lookups can access the symbols in all its parent frames.  This is how lexical
// scoping is implemented.
//
// Each frame is ref-counted and contains a ref to its parent frame in order to
// facilitate the incremental lookup described.  If a lambda in a parent frame
// has an environment in a child frame, then there's a reference cycle between
// the frames.  This cycle is broken trivially when the lamdba is explicitly
// removed from the environment, or coincidentally when the lambda resides in a
// Cons cell that is then garbage collected.
//
#[derive(Debug)]
struct Frame {
    symtab: HashMap<Atom, Sexp>,
    next: Option<FramePtr>,
    id: u32,
    temps: Vec<Sexp>,
    dumping: bool
}

pub type FramePtr = Rc<RefCell<Frame>>;


struct TempContext {
    f: FramePtr,
    n_temps: usize
}

impl TempContext {
    fn add(&mut self, sexp: Sexp) {
        self.f.borrow_mut().temps.push(sexp);
        self.n_temps = self.n_temps + 1;
    }
}

trait FramePtrMethods {
    fn find(&self, sym: &Atom) -> Option<FramePtr>;
    fn insert(&self, sym: Atom, val: Sexp);
    fn find_and_lookup<F, R>(&self, sym: &Atom, f: F) -> R
        where F : FnOnce(Option<&Sexp>) -> R;
    fn find_and_lookup_mut<F, R>(&self, sym: &Atom, f: F) -> R
        where F : FnOnce(Option<&mut Sexp>) -> R;

    fn add_temps<F, R>(&self, f: F) -> R where F: FnOnce(&mut TempContext) -> R;

    fn dump(&self, level: usize);

    fn id(&self) -> u32;
}

fn indent(level: usize) {
    for i in 0..level {
        print!("  ")
    }
}

impl FramePtrMethods for FramePtr {
    fn find(&self, sym: &Atom) -> Option<FramePtr> {
        let me = self.borrow();
        if me.symtab.contains_key(sym) {
            Some(self.clone())
        }
        else if let Some(ref next) = me.next {
            next.find(sym)
        }
        else {
            None
        }
    }

    fn insert(&self, sym: Atom, val: Sexp) {
        self.borrow_mut().symtab.insert(sym, val);
    }

    fn find_and_lookup<F, R>(&self, sym: &Atom, f: F) -> R
        where F : FnOnce(Option<&Sexp>) -> R
    {
        if let Some(frameptr) = self.find(sym) {
            f(frameptr.borrow().symtab.get(sym))
        }
        else {
            f(None)
        }
    }

    fn find_and_lookup_mut<F, R>(&self, sym: &Atom, f: F) -> R
        where F : FnOnce(Option<&mut Sexp>) -> R
    {
        if let Some(frameptr) = self.find(sym) {
            f(frameptr.borrow_mut().symtab.get_mut(sym))
        }
        else {
            f(None)
        }
    }

    fn add_temps<F, R>(&self, f: F) -> R where F: FnOnce(&mut TempContext) -> R {
        let mut tc = TempContext { f: self.clone(), n_temps: 0 };
        let r = f(&mut tc);
        for i in 0..tc.n_temps {
            self.borrow_mut().temps.pop();
        }
        r
    }

    fn dump(&self, level: usize) {
        {
            if self.borrow().dumping {
                indent(level);
                println!("<...>");
                return;
            }
        }
        {
            self.borrow_mut().dumping = true;
        }
        let id;
        let next_frame;
        {
            let f = self.borrow();
            id = f.id;
            indent(level);
            println!("--- Frame {}: ---", f.id);
            for (atom, sexp) in f.symtab.iter() {
                indent(level);
                println!("{}: {}", atom, gc::simple_dump(sexp));
                if let Sexp::Closure(ref sc) = *sexp {
                    indent(level + 1);
                    sc.env.dump(level + 1);
                }
            }
            if let Some(ref next) = f.next {
                next_frame = Some(next.clone());
            }
            else {
                next_frame = None;
            }
            indent(level);
            println!("------ Frame {} temps:", f.id);
            for sexp in &f.temps {
                indent(level);
                println!("{}", gc::simple_dump(sexp));
                if let Sexp::Closure(ref sc) = *sexp {
                    indent(level + 1);
                    sc.env.dump(level + 1);
                }
            }
        }
        next_frame.and_then(|nf| {
            Some(nf.dump(level + 1))
        });
        indent(level);
        println!("--- End Frame {}: ---", id);
        {
            self.borrow_mut().dumping = false;
        }
    }

    fn id(&self) -> u32 {
        self.borrow().id
    }
}

thread_local!(static FRAME_ID: Cell<u32> = Cell::new(0));

impl Frame {
    fn new(next: Option<FramePtr>) -> Frame {
        Frame {
            symtab: HashMap::new(),
            next: next,
            id: FRAME_ID.with(|c| { c.set(c.get() + 1); /*if c.get() == 64 { panic!("64"); }*/ c.get() }),
            temps: vec!(),
            dumping: false
        }
    }

    fn find_frameptr(me: FramePtr, sym: &Atom) -> Option<FramePtr> {
        let frame = me.borrow();
        if frame.symtab.contains_key(sym) {
            Some(me.clone())
        }
        else if let Some(ref next_frame) = frame.next {
            Frame::find_frameptr(next_frame.clone(), sym)
        }
        else {
            None
        }
    }

    fn find<'a>(&'a self, sym: &Atom) -> Option<&'a Frame> {
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

    fn lookup<'a>(&'a self, sym: &Atom) -> Option<&'a Sexp> {
        self.symtab.get(sym)
    }

    fn find_and_lookup<'a>(&'a self, sym: &Atom) -> Option<&'a Sexp> {
        if let Some(frame) = self.find(sym) {
            frame.lookup(sym)
        }
        else {
            None
        }
    }

    fn find_and_remove(me: FramePtr, sym: &Atom) -> Option<Sexp> {
        if let Some(frameptr) = Frame::find_frameptr(me, sym) {
            frameptr.borrow_mut().symtab.remove(sym)
        }
        else {
            None
        }
    }

    fn set(&mut self, sym: Atom, val: Sexp) {
        self.symtab.insert(sym, val);
    }

    fn all_sexps<'a>(&'a mut self) -> Box<Iterator<Item=&'a mut Sexp> + 'a> {
        let mut it = self.symtab.iter_mut().map(|i| i.1);
        if let Some(ref next_frame) = self.next {
            Box::new(it
                     .chain(borrow_mut_hack(next_frame).all_sexps())
                     .chain(self.temps.iter_mut()))
        }
        else {
            Box::new(it.chain(self.temps.iter_mut()))
        }
    }
}

#[allow(dead_code)]
fn test_env() {
    let mut f = Frame::new(None);
    f.set("key".into(), Id("val".into()));
    f.set("key2".into(), Num(42f64));

    {
        let v = f.lookup(&"key2".into());
        println!("{:?}", v);
    }

    // If the scope above wasn't closed, then this borrow
    // would be invalid.
    f.set("key2".into(), Id("oops".into()));
}

//------------------------------------------------------------------------------
// Semantic Analyzer
//------------------------------------------------------------------------------

//
// CapHack exists to work around the problem in Rust 1.0 where you cannot return
// a FnOnce from a function.  This is because you cannot unbox a FnOnce.  There
// exists an unstable feature called FnBox, so I can move to that in a future
// release.  CapHack works by using a RefCell to mutate immutable state, thus
// forcing a move of the captured variable.  There still is runtime overhead in
// the form of checking the states of the RefCell and Option, but it is probably
// cheaper than a superflous clone that you would otherwise need.
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


struct Expr {
    f: Box<Fn(FramePtr) -> SResult<Sexp>>
}

impl Expr {
    fn new<F>(f: F) -> Expr
        where F: 'static + Fn(FramePtr) -> SResult<Sexp>
    {
        Expr { f: Box::new(f) }
    }
    fn call(&self, env: FramePtr) -> SResult<Sexp> {
        (&self.f)(env)
    }
}

impl Debug for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        f.write_str("<Expr>")
    }
}

//
// An iterator that wraps an iterator and transforms it to iterate over elements
// (I::Item, bool) where the bool tells you if it's the last element or not.
//
struct Lastable<I> where I: Iterator {
    p: Peekable<I>
}

impl<I> Lastable<I> where I: Iterator {
    fn new(it: I) -> Lastable<I> {
        Lastable { p: it.peekable() }
    }
}

impl<I> Iterator for Lastable<I> where I: Iterator {
    type Item = (I::Item, bool);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.p.next() {
            Some((next,
                  if let None = self.p.peek() { true } else { false } ))
        }
        else {
            None
        }
    }
}

trait Lastableable {
    fn lastable(self) -> Lastable<Self>;
}

impl<I> Lastableable for I where I: Iterator {
    fn lastable(self) -> Lastable<Self> {
        Lastable::new(self)
    }
}

struct IntCtx {
    env: FramePtr  // the base environment
}

thread_local!(static INSIDE_TRACE: Cell<bool> = Cell::new(false));
thread_local!(static DEPTH: Cell<usize> = Cell::new(0));

// Basic tail call optimization: analyzing an application in the tail position
// will cause it to return a delayed thunk (trampoline), and any caller not in
// the tail position needs to keep forcing those thunks until there is an actual
// return value.
// Note: result isn't gc pinned.
fn funcall<T>(mut func: Sexp, mut args: T, ctx: Rc<IntCtx>, tail: bool) -> SResult<Sexp>
    where T: Iterator<Item=SResult<Sexp>>
{
    let mut fclone = None;
    let otracefn = ctx.env.find_and_lookup(&"*trace-fn*".into(), |orsexp| {
        if let Some(rsexp) = orsexp {
            if !rsexp.is_nil() {
                fclone = Some(func.clone());
                return Some(rsexp.clone())
            }
        }
        None
    });

    /*let f2 = func.clone();
    DEPTH.with(|c| { for i in 0..c.get() { print!(" ") } c.set(c.get() + 1) });
    println!("-> {}", &f2);*/

    if let Some(ref rtracefn) = otracefn {
        gc::with_gc_disabled(|| {
            let args_cloned = args.by_ref().map(|i| i.unwrap().clone());
            INSIDE_TRACE.with(|c| {
                if !c.get() {
                    c.set(true);
                    // TODO: propagate funcall error, here and below
                    funcall(rtracefn.clone(),
                            // TODO: it would be nice to pass the args along too
                            vec!(Ok(Id("enter".into())),
                                 Ok(func.clone()),
                                 Ok(Sexp::accumulate(args_cloned))
                                 ).into_iter(),
                            ctx.clone(),
                            false);
                    c.set(false);
                }
            });
        })
    }

    let mut rsres = if let Sexp::Closure(sc) = func {
        let tmp = sc.apply(args, ctx.clone());
        tmp
    }
    else if let Sexp::Trampoline(sc) = func {
        sc.apply(args, ctx.clone()) // for debugging purposes
    }
    else if let Sexp::Builtin(b) = func {
        b.apply(args, ctx.clone())
    }
    else {
        Err(format!("funcall on non function object {}", func).into())
    };

    // TODO: put call stack information in here on error
    // NOTE: `res` isn't gc pinned!  we can't collect garbage until it is
    let mut res = try!(rsres);

    if !tail {
        // NOTE: `res` doesn't need gc pinning here because it is overwitten.
        while let Sexp::Trampoline(tramp) = res {
            res = try!(tramp.apply(vec!().into_iter(), ctx.clone()));
        }
    }

    /*DEPTH.with(|c| { for i in 0..c.get() { print!(" ") } c.set(c.get() - 1); });
    println!("<- {}", &f2);*/

    if let Some(ref rtracefn) = otracefn {
        gc::with_gc_disabled(|| {
            INSIDE_TRACE.with(|c| {
                if !c.get() {
                    c.set(true);
                    funcall(rtracefn.clone(),
                            vec!(Ok(Id("exit".into())),
                                 Ok(fclone.unwrap().clone())).into_iter(),
                            ctx.clone(),
                            false);
                    c.set(false);
                }
            });
        });
    }

    Ok(res)
}

struct Analyzer {
    ctx: Rc<IntCtx>,
    macro_table: HashMap<String, SClosure>
}

impl Analyzer {
    fn new<'a>(ctx: Rc<IntCtx>) -> Analyzer {
        Analyzer { ctx: ctx, macro_table: HashMap::new() }
    }

    fn expand_macros(&self, mut s: Sexp) -> SResult<Sexp> {
        gc::with_gc_disabled(|| {
            loop {
                let (did_stuff, expanded) = try!(self.expand_macros_once(s));
                if !did_stuff { return Ok(expanded) }
                s = expanded
            }
        })
    }

    fn expand_macros_once(&self, s: Sexp) -> SResult<(bool, Sexp)> {
        if s.is_cons() && !(s.car_is_id("quote")) {
            let mut opt_mac = None;
            if let Id(ref id) = *try!(s.car()) {
                self.ctx.env.find_and_lookup(id, |s| {
                    if let Some(ref val) = s {
                        if let Sexp::Macro(ref rcmac) = **val {
                            opt_mac = Some(rcmac.clone())
                        }
                    }
                })
            }
            if let Some(rcmac) = opt_mac {
                let (car, cdr) = s.carcdr().unwrap(); // shouldn't fail
                let res = funcall(Sexp::Closure(rcmac.clone()),
                                  cdr.iter().map(|i| Ok(i.clone())),
                                  self.ctx.clone(),
                                  false);
                if res.is_err() {
                    return Err(SchemeError {
                        ty: ErrorType::Msg(format!("Expansion of `{}` failed.", car)),
                        inner: Some(Box::new(res.unwrap_err())),
                        stack: vec!(),
                    });
                }
                Ok((true, try!(res)))
            }
            else {
                let (ocar, ocdr) = try!(s.carcdr());
                let (did_car, car) = try!(self.expand_macros_once(ocar.clone()));
                let (did_cdr, cdr) = try!(self.expand_macros_once(ocdr.clone()));
                Ok((did_car || did_cdr, Sexp::new_cons(car, cdr)))
            }
        }
        else {
            Ok((false, s.deep_clone()))
        }
    }

    fn analyze(&self, s: Sexp, tail: bool) -> Expr {
        match s {
            Num(_) | Nil | Sexp::Str(_) | Sexp::Bool(_) => {
                Expr::new(move |_| Ok(s.clone()))
            },
            Sexp::Closure(_) | Sexp::Builtin(_) |
            Sexp::Macro(_) | Sexp::Trampoline(_) | Sexp::Eof =>
                panic!("unexpected sexp in analyze"),
            Id(sid) => self.analyze_env_lookup(sid),
            Cons(_) => {
                let (car, cdr) = s.carcdr_take().unwrap();
                if let Id(id) = car {
                    if id == "lambda".into() { self.analyze_lambda(None, cdr) }
                    else if id == "quote".into() {
                        self.analyze_quote(cdr.car_take().unwrap())
                    }
                    else if id == "define".into() { self.analyze_define(cdr) }
                    else if id == "define-macro".into() {
                        self.analyze_define_macro(cdr)
                    }
                    else if id == "if".into() { self.analyze_if(cdr, tail) }
                    // we define "and" and "or" as special forms instead of macros
                    // in terms of `if` to avoid macro expansion noise
                    else if id == "and".into() { self.analyze_and(cdr, tail) }
                    else if id == "or".into() { self.analyze_or(cdr, tail) }
                    else if id == "set!".into() { self.analyze_set(cdr) }
                    else {
                        self.analyze_application(
                            Sexp::new_cons(Id(id), cdr),
                            tail)
                    }
                }
                else {
                    self.analyze_application(Sexp::new_cons(car, cdr), tail)
                }
            }
        }
    }

    fn analyze_env_lookup(&self, id: Atom) -> Expr {
        Expr::new(move |env| {
            env.find_and_lookup(&id, |ov| {
                if let Some(val) = ov {
                    Ok(val.clone())
                }
                else {
                    Err(format!("Undefined variable: {}", id).into())
                }
            })
        })
    }

    // s: ((funcname arg1 arg2) body) or else (varname value)
    fn analyze_define(&self, s: Sexp) -> Expr {
        let id;
        let val;
        let (car, cdr) = s.carcdr_take().unwrap();
        if car.is_cons() {
            let (caar, cdar) = car.carcdr_take().unwrap();
            id = caar.id_take();
            val = self.analyze_lambda(Some(&id), Sexp::new_cons(cdar, cdr));
        }
        else {
            id = car.id_take();
            val = self.analyze(cdr.car_take().unwrap(), false);
        }

        Expr::new(move |env| {
            let valval = try!(val.call(env.clone()));
            env.borrow_mut().set(id.clone(), valval);
            Ok(Id(id.clone()))
        })
    }

    // s: (varname value)
    fn analyze_set(&self, s: Sexp) -> Expr {
        let (car, cdr) = s.carcdr_take().unwrap();
        let id = car.id_take();
        let val = self.analyze(cdr.car_take().unwrap(), false);

        Expr::new(move |env| {
            let mut update = false;
            let valval = try!(val.call(env.clone()));

            // update an existing binding...
            env.find_and_lookup_mut(&id, |omrval| {
                if let Some(mrval) = omrval {
                    *mrval = valval.clone();
                    update = true;
                }
            });

            if (!update) {
                // ...or create a new binding
                env.insert(id.clone(), valval);
            }

            Ok(Id(id.clone()))
        })
    }

    fn analyze_define_macro(&self, s: Sexp) -> Expr {
        let (car, cdr) = s.carcdr_take().unwrap();
        let macro_name = car.id_take();
        let evalue = self.analyze(cdr.car_take().unwrap(), false);
        Expr::new(move |env| {
            if let Sexp::Closure(rsc) = try!(evalue.call(env.clone())) {
                env.borrow_mut().set(macro_name.clone(), Sexp::Macro(rsc));
                Ok(Id(macro_name.clone()))
            }
            else {
                Err("Macro defined to be non-closure.".into())
            }
        })
    }

    fn analyze_quote(&self, sexp: Sexp) -> Expr {
        match sexp {
            Num(_) | Id(_) | Sexp::Str(_) | Nil | Sexp::Bool(_)
                => Expr::new(move |_| Ok(sexp.clone())),
            Cons(cellref) => {
                let (ref car, ref cdr) = *cellref;
                let ecar = self.analyze_quote(car.clone());
                let ecdr = self.analyze_quote(cdr.clone());
                Expr::new(move |env| {
                    Ok(Sexp::new_cons(
                        try!(ecar.call(env.clone())),
                        try!(ecdr.call(env.clone()))))
                })
            }
            // you can only quote things that come from the parser
            _ => unreachable!()
        }
    }

    fn analyze_lambda(&self, nameref: Option<&Atom>, s: Sexp) -> Expr {
        // car is args and cdr is body
        let (car, cdr) = s.carcdr_take().unwrap();
        let mut arg_cons_iter = car.into_iter();
        let arg_names: Vec<_> = arg_cons_iter.by_ref().map(|i| {
            if let Id(arg) = i { arg }
            else { panic!("non-id argument to lambda") }
        }).collect();

        let rest_arg =
            if let Id(id) = arg_cons_iter.cur {
                Some(id)
            } else { None };

        let expr = Rc::new(self.analyze_body(cdr));
        let opt_name = nameref.cloned();

        Expr::new(move |env| {
            // each time a lambda expression is evaluated it must return a new
            // closure since each closure can have its own environment, we refcount
            // the expr and could do the same for the args too
            Ok(Sexp::Closure(Rc::new(SClosure {
                env: env.clone(),
                arg_names: arg_names.clone(),
                rest_arg: rest_arg.clone(),
                expr: expr.clone(),
                opt_name: opt_name.clone()
            })))
        })
    }

    fn analyze_body(&self, sbody: Sexp) -> Expr {
        let exprs: Vec<_> = sbody.into_iter().lastable().map(|i| {
            self.analyze(i.0, i.1)
        }).collect();

        Expr::new(move |env| {
            let mut res = Nil;
            for e in &exprs {
                res = try!(e.call(env.clone()));
            }
            Ok(res)
        })
    }

    fn analyze_application(&self, sexp: Sexp, tail: bool) -> Expr {
        let (car, cdr) = sexp.carcdr_take().unwrap();
        let efunc = self.analyze(car, false);
        let eargs: Vec<_> = cdr.into_iter().map(|i| self.analyze(i, false)).collect();
        let ctx = self.ctx.clone();
        let mut afunc = Expr::new(move |env| {
            let func = try!(efunc.call(env.clone()));
            env.add_temps(|tc| {
                tc.add(func.clone());

                let args_iter = eargs.iter()
                    .map(|i| {
                        i.call(env.clone())
                    })
                    .inspect(|i| {
                        if let Ok(ref sexp) = *i {
                            tc.add(sexp.clone());
                        }
                    });

                let fclone = func.clone();
                gc::collect(env.clone());
                let ret = funcall(func, args_iter, ctx.clone(), tail);
                ret  // note that `ret` isn't gc pinned
            })
        });

        // If we're in the tail-call position, we return a trampoline containing
        // a closure that when executed will execute the application in the captured
        // environment.  IOW, instead of actually executing the application, we capture
        // the environment and application in a closure and return that as a trampoline.
        if (tail) {
            let etramp = Rc::new(afunc);
            afunc = Expr::new(move |env| {
                Ok(Sexp::Trampoline(Rc::new(
                    SClosure {
                        env: env.clone(),
                        arg_names: vec!(),
                        rest_arg: None,
                        expr: etramp.clone(),
                        opt_name: None
                    })))
            })
        }

        afunc
    }

    // test true-clause opt-false-clause
    fn analyze_if(&self, sexp: Sexp, tail: bool) -> Expr {
        let (car, cdr) = sexp.carcdr_take().unwrap();
        let (cadr, cddr) = cdr.carcdr_take().unwrap();
        let etest = self.analyze(car, false);
        let etrue = self.analyze(cadr, tail);
        let efalse = self.analyze_body(cddr); // nb: handles empty else clause

        Expr::new(move |env| {
            if try!(etest.call(env.clone())).to_bool() {
                etrue.call(env.clone())
            }
            else {
                efalse.call(env.clone())
            }
        })
    }

    fn analyze_and(&self, sexp: Sexp, tail: bool) -> Expr {
        let eargs: Vec<_> = sexp.into_iter().lastable()
            .map(|i| self.analyze(i.0, tail && i.1)).collect();
        Expr::new(move |env| {
            let mut last = Nil;
            for i in &eargs {
                last = try!(i.call(env.clone()));
                if !last.to_bool() {
                    return Ok(Sexp::Bool(false));
                }
            }
            Ok(last)
        })
    }

    fn analyze_or(&self, sexp: Sexp, tail: bool) -> Expr {
        let eargs: Vec<_> = sexp.into_iter().lastable()
            .map(|i| self.analyze(i.0, tail && i.1)).collect();
        Expr::new(move |env| {
            let mut last;
            for i in &eargs {
                last = try!(i.call(env.clone()));
                if last.to_bool() {
                    return Ok(last);
                }
            }
            Ok(Sexp::Bool(false))
        })
    }
} // impl Analyzer

//------------------------------------------------------------------------------
// Simple stop-and-copy GC
//
// Notes about this heap:
//  - we allow "taking" items from the heap to avoid cloning when we can
//    guarantee there are no external pointers to the data, during parsing
//    most conses are "taken" since they flow from the parser to the analyzer
//
//  - frames are not GC'ed, they are ref counted: however no ref cycles can
//    happen between frames unless they are via a GC object (an SClosure),
//    so GC will break any ref cycles between frames
//
//  - the heap |Conses| is a vector of Cells, each Cell is either a (car, cdr)
//    pair, or a Forward(new_location) during GC
//
//  - Cells are not used directly, instead their contents are accessed by using
//    a CellRef, which just encapsulates an index into the heap vector.  Cloning
//    a CellRef is just a matter of copying the array index.  However, you must
//    provide all of your current active CellRefs to the collector for updating
//    because Cells move during collection.
//
//  - during collection, live items are copied to a new vector, compacting the
//    heap while simultaneously removing garbage, since dead items are left
//    behind
//
//  - currently, the heap is only single-threaded
//
//  - super dead simple stop-and-copy GC algorithm:
//    GC(frame):
//      for each sexp in frame1..frame2..frameN:
//         if sexp is a CellRef then
//           if old_heap[sexp.pos] is forwarded then
//              sexp.pos = forwarded_location_in_new_heap
//           else
//              new_pos = new_heap.push(sexp.car, sexp.cdr)  // COPY TO NEW HEAP
//              old_heap[sexp.pos] = Forward(new_pos)  // FORWARD TO NEW HEAP
//              sexp.pos = new_pos
//         else if sexp is SClosure then
//           GC(sclosure.env) unless env is already being GC'ed
//
//  - so that we can perform GC at any (reasonable) time, we follow the follwing
//    rule: Never at runtime hold a Sexp in a temporary (as opposed to an
//    environment frame) while making any Expr calls.  This will guarantee that
//    any expr call can assume that it can perform GC.
//
//------------------------------------------------------------------------------
mod gc {
    use std;
    use std::cell::RefCell;
    use std::collections::HashSet;
    use std::fmt::{Debug, Formatter};
    use std::ops::Deref;

    use ::Atom;
    use ::{Sexp, Frame, FramePtr, FramePtrMethods};
    use ::Sexp::{Cons, Nil};

    #[derive(Debug, Copy, Clone, PartialEq)]
    pub struct CellRef {
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

    pub fn simple_dump(sexp: &Sexp) -> String {
        match *sexp {
            Cons(cr) => format!("Cell#{}", cr.pos),
            _ => format!("{}", sexp)
        }
    }

    impl Debug for Cell {
        fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
            match *self {
                Cell::Cons((ref rcar, ref rcdr)) =>
                    write!(f, "[ {} ][ {} ]",
                           simple_dump(rcar), simple_dump(rcdr)),
                Cell::Forward(new) =>
                    write!(f, " fwd to {}", new)
            }
        }
    }

    struct Conses {
        v: Vec<Option<Cell>>,
        gen: u32,
    }

    impl Conses {
        fn new(gen: u32) -> Conses {
            Conses { v: Vec::new(), gen: gen }
        }

        fn push(&mut self, car: Sexp, cdr: Sexp) -> usize {
            let pos = self.v.len();
            /*if self.gen > 20 && pos == 6 {
                panic!("haha! {} {}", simple_dump(&car), simple_dump(&cdr));
            }*/
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
            let c = self.v[pos].as_ref();
            if c.is_none() {
                panic!("cell at {} is already taken", pos)
            }
            c.unwrap().get()
        }

        fn get_clone(&self, pos: usize) -> (Sexp, Sexp) {
            self.get_ref(pos).clone()
        }

        fn take(&mut self, pos: usize) -> (Sexp, Sexp) {
            let c = self.v[pos].take();
            /*if pos == 1227 {
                panic!("taking 1227")
            }*/
            if c.is_none() {
                panic!("cell at {} is already taken", pos)
            }
            c.unwrap().take()
        }
    }

    struct SchemeHeap {
        disable: bool,
        high_water: usize,
        conses: Conses,
    }

    thread_local!(
        static HEAP: RefCell<SchemeHeap> = RefCell::new(SchemeHeap::new()));

    impl SchemeHeap {
        fn new() -> SchemeHeap {
            SchemeHeap {
                disable: false,
                high_water: 0,
                conses: Conses::new(0)
            }
        }

        fn collect(&mut self, env: FramePtr) {
            if !self.disable {
                let mut tmp = GcEpoch::run(&mut self.conses, env);
                std::mem::swap(&mut self.conses, &mut tmp);
            }
        }

        fn maybe_collect(&mut self, env: FramePtr) {
            if self.disable { return; }
            let mut do_collect = false;
            if self.conses.v.len() > self.high_water {
                println!("GC (sz={}) b/c high_water ({})",
                         self.conses.v.len(), self.high_water);
                if self.high_water == 0 {
                    self.high_water = self.conses.v.len();
                } else {
                    self.high_water = self.high_water * 2;
                }
                do_collect = true;
            }
            else if let Some(Sexp::Num(threshold)) = env.find_and_lookup(
                &"*gc-threshold*".into(), |o| o.cloned())
            {
                println!("GC {} b/c gc_thresh", self.conses.v.len());
                do_collect = true;
            }
            if do_collect {
                self.collect(env);
            }
        }
    }

    pub fn alloc(car: Sexp, cdr: Sexp) -> CellRef {
        HEAP.with(|heap| {
            let pos = heap.borrow_mut().conses.push(car, cdr);
            CellRef { pos: pos }
        })
    }

    pub fn collect(env: FramePtr) {
        HEAP.with(|heap| {
            heap.borrow_mut().collect(env)
        })
    }

    pub fn maybe_collect(env: FramePtr) {
        HEAP.with(|heap| {
            heap.borrow_mut().maybe_collect(env)
        })
    }

    pub fn with_gc_disabled<F, R>(f: F) -> R where F: FnOnce() -> R {
        let mut old = true;
        HEAP.with(|heap| {
            let mut h = heap.borrow_mut();
            std::mem::swap(&mut h.disable, &mut old);
        });
        let ret = f();
        HEAP.with(|heap| {
            let mut h = heap.borrow_mut();
            std::mem::swap(&mut h.disable, &mut old);
        });
        ret
    }

    impl CellRef {
        pub fn new(car: Sexp, cdr: Sexp) -> CellRef {
            alloc(car, cdr)
        }

        pub fn get_ref<'a>(&self) -> &'a (Sexp, Sexp) {
            HEAP.with(|heap| {
                let h = heap.borrow();
                // this is very unrusty, might be best to return
                // something like a Ref which locks the heap and prevents
                // GCs while there are outstanding refs
                unsafe { std::mem::transmute(h.conses.get_ref(self.pos)) }
            })
        }

        pub fn set_car(&self, car: Sexp) {
            HEAP.with(|heap| {
                let mut h = heap.borrow_mut();
                let pair = h.conses.get_mut_ref(self.pos);
                pair.0 = car;
            })
        }

        pub fn set_cdr(&self, cdr: Sexp) {
            HEAP.with(|heap| {
                let mut h = heap.borrow_mut();
                let pair = h.conses.get_mut_ref(self.pos);
                pair.1 = cdr;
            })
        }

        pub fn get_clone(&self) -> (Sexp, Sexp) {
            HEAP.with(|heap| {
                heap.borrow().conses.get_clone(self.pos)
            })
        }

        pub fn take(self) -> (Sexp, Sexp) {
            HEAP.with(|heap| {
                heap.borrow_mut().conses.take(self.pos)
            })
        }
    }

    impl Deref for CellRef {
        type Target = (Sexp, Sexp);

        fn deref<'a>(&'a self) -> &'a (Sexp, Sexp) {
            self.get_ref()
        }
    }

    // Represents a single collection.
    struct GcEpoch<'a> {
        src: &'a mut Conses,
        dst: Conses,
        frame_log: HashSet<*const RefCell<Frame>>,
        frames: Vec<FramePtr>
    }

    impl<'a> GcEpoch<'a> {
        fn run(src: &'a mut Conses, env: FramePtr) -> Conses {
            let next_gen = src.gen + 1;
            let e = GcEpoch {
                src: src,
                dst: Conses::new(next_gen),
                frame_log: HashSet::new(),
                frames: vec!(env.clone())
            };
            e.copy_frames()
        }

        fn copy_frames(mut self) -> Conses {
            /*println!("============= HEAP DUMP for gen {} =================", self.src.gen);
            for (idx, ref val) in self.src.v.iter().enumerate() {
                println!("{}: {:?}", idx, **val);
            }*/

            while !self.frames.is_empty() {
                let env = self.frames.pop().unwrap();
                self.copy_frame(env);
            }

            /*println!("============= GC COMPLETE =================");
            for (idx, ref val) in self.src.v.iter().enumerate() {
                println!("{}: {}", idx, if let Some(Cell::Cons(_)) = **val {
                    "garbage".to_string()
                } else {
                    format!("{:?}", **val)
                });
            }*/
            println!("Heap size {} -> {} cells", self.src.len(), self.dst.len());

            self.dst
        }

        fn copy_frame(&mut self, env: FramePtr) {
            if self.frame_log.insert(&*env) {
                let frame_id = env.id();
                /*println!("collecting Frame {}", frame_id);*/
                let mut frame = env.borrow_mut();
                let mut bit = frame.all_sexps();
                self.copy_all(&mut *bit);
            }
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
                    self.frames.push(sclosure.env.clone());
                },
                Sexp::Macro(ref mut sclosure) =>
                    self.frames.push(sclosure.env.clone()),
                _ => () // no gc needed for value types
            }
        }

        fn copy_one(&mut self, scons: &mut CellRef) {
            // if it's already been forwarded
            if let Cell::Forward(new_pos) = *self.src.get_cell_ref(scons.pos) {
                scons.pos = new_pos
            }
            else {
                let forward = self.dst.len();
                let (mut car, mut cdr) =
                    self.src.v[scons.pos].as_mut().unwrap().forward(forward);
                let new_pos = self.dst.push(Nil, Nil);

                self.copy_dispatch(&mut car);
                self.copy_dispatch(&mut cdr);

                self.dst.v[new_pos] = Some(Cell::Cons((car, cdr)));
                assert_eq!(forward, new_pos);
                scons.pos = new_pos;
            }
        }
    }
}


//------------------------------------------------------------------------------
// Builtin Functions
//------------------------------------------------------------------------------
fn install_builtins(ctx: Rc<IntCtx>) {
    macro_rules! patlist {
        ( $p:pat => $it:ident, $e:expr ) => {
            match $it.next().unwrap() {
                $p => $e,
                _ => panic!("Invalid type argument")
            }
        };
        ( $p:pat, $( $ps:pat ),* => $it:ident, $e:expr ) => {
            match $it.next().unwrap() {
                $p => patlist!($( $ps ),* => $it, $e),
                _ => panic!("Invalid type argument")
            }
        }
    }

    macro_rules! slist {
        ( $i:ident => $it:ident, $e:expr ) => {
            if let Some($i) = $it.next() {
                $e
            } else { panic!("Not enough args") }
        };
        ( $i:ident, $( $is:ident ),* => $it:ident, $e:expr ) => {
            if let Some($i) = $it.next() {
                slist!($( $is ),* => $it, $e)
            } else { panic!("Not enough args") }
        }
    }

    macro_rules! builtin {
        ($n:expr, $( $p:pat ),* => $e:expr ) => ({
            ctx.env.borrow_mut().set(
                Atom::for_str($n),
                Sexp::Builtin(Rc::new(Builtin::new(
                    $n,
                    Box::new(|it| {
                        patlist!($( $p ),* => it, $e)
                    })))));
        });
        ($n:expr, $( i:ident ),* = $e:expr ) => ({
            ctx.env.borrow_mut().set(
                Atom::for_str($n),
                Sexp::Builtin(Rc::new(Builtin::new(
                    $n,
                    Box::new(|it| {
                        slist!($( $i ),* => it, $e)
                    })))));
        });
    }

    macro_rules! n {
        ($it:expr) => ($it.next().unwrap());
    }

    let apply_ctx = ctx.clone();
    let gc_ctx = ctx.clone();
    let b : Vec<(&str, Box<Fn(&mut Iterator<Item=Sexp>) -> SResult<Sexp>>)> = vec!(
        ("+", mathy(|s, i| s + i)),
        ("-", mathy(|s, i| s - i)),
        ("/", mathy(|s, i| s / i)),
        ("*", mathy(|s, i| s * i)),
        ("mod", mathy(|s, i| s % i)),
        ("=", cmpy(|a, b| a == b)),
        ("<", cmpy(|a, b| a < b)),
        (">", cmpy(|a, b| a > b)),
        ("<=", cmpy(|a, b| a <= b)),
        (">=", cmpy(|a, b| a >= b)),
        ("cons", Box::new(|it| Ok(Sexp::new_cons(n!(it), n!(it))))),
        ("car", Box::new(|it| Ok(try!(n!(it).car()).clone()))),
        ("cdr", Box::new(|it| Ok(try!(n!(it).cdr()).clone()))),
        ("apply", Box::new(move |args| scheme_apply(args, apply_ctx.clone()))),
        ("eq?", Box::new(|it| Ok(Sexp::Bool(n!(it) == n!(it))))),
        ("null?", Box::new(|it| Ok(Sexp::Bool({
            if let Nil = n!(it) { true } else { false }
        })))),
        ("display", Box::new(|it| Ok(scheme_display(n!(it))))),
        ("newline", Box::new(|_| { println!(""); Ok(Nil) })),
        ("pair?", Box::new(|it| Ok(Sexp::Bool(n!(it).is_cons())))),
        ("assertion-violation", Box::new(|it| {
            let who = it.next().unwrap_or(Id("anon".into()));
            let msg = it.next().unwrap_or(Sexp::Str("empty msg".to_string()));
            let irritants = Sexp::accumulate(it);
            Err(format!("Assertion violation in {}: {}: {}",
                        who, msg, irritants).into())
        })),
        ("error", Box::new(|it| {
            let who = it.next().unwrap_or(Id("anon".into()));
            let msg = it.next().unwrap_or(Sexp::Str("empty msg".to_string()));
            let irritants = Sexp::accumulate(it);
            Err(format!("Error in {}: {}: {}", who, msg, irritants).into())
        })),
        ("set-car!", Box::new(|it| { n!(it).set_car(n!(it)) })),
        ("set-cdr!", Box::new(|it| { n!(it).set_cdr(n!(it)) })),
        ("gc", Box::new(move |it| { gc::collect(gc_ctx.env.clone()); Ok(Nil) })),
    );

    for i in b {
        ctx.env.borrow_mut().set(
            Atom::for_str(i.0),
            Sexp::Builtin(Rc::new(Builtin::new(i.0, i.1))));
    }

    //builtin!("bcons", a, b = { Sexp::new_cons(a, b) });
    builtin!("mul2", Num(a), Num(b) => Ok(Num(a*b)));
}

fn scheme_display(sexp: Sexp) -> Sexp {
    if let Sexp::Str(str) = sexp {
        print!("{}", str);
    }
    else {
        print!("{}", sexp);
    }
    Nil
}

//
// (apply func arg1 arg2 (arg3 ... argn)
//
// is equlivalent to:
//
// (apply func (arg1 arg2 arg3 ... argn))
//
fn scheme_apply(it: &mut Iterator<Item=Sexp>, ctx: Rc<IntCtx>) -> SResult<Sexp> {
    let mut args: Vec<_> = it.collect();
    let len = args.len();
    let rest_args = args.remove(len - 1);
    let rest_vec : Vec<_> = rest_args.iter().cloned().collect();

    let mut args_it = args.into_iter();

    // the first arg of apply is the function to call
    let func = args_it.next().unwrap();

    // the inner n-2 args are supplied to the function
    let inner_args_it = args_it.take(len - 2);

    // the final arg must be a SList which is appended to args from above
    let args_it = inner_args_it.chain(rest_vec);

    let args_vec : Vec<Sexp> = args_it.collect();
    funcall(func.clone(), args_vec.into_iter().map(|i| Ok(i)), ctx, false)
}

fn get_num(s: &Sexp) -> SResult<f64> {
    if let Num(n) = *s {
        Ok(n)
    }
    else {
        Err(format!("Math expression expected number but got {}.", s).into())
    }
}

fn mathy<F>(f: F) -> Box<Fn(&mut Iterator<Item=Sexp>)->SResult<Sexp>>
    where F: 'static + Fn(f64, f64) -> f64
{
    Box::new(move |args| {
        let mut result = try!(get_num(&args.next().unwrap()));
        for s in args {
            result = f(result, try!(get_num(&s)));
        }
        Ok(Sexp::Num(result))
    })
}

fn cmpy<F>(f: F) -> Box<Fn(&mut Iterator<Item=Sexp>) -> SResult<Sexp>>
    where F: 'static + Fn(f64, f64) -> bool
{
    Box::new(move |args| {
        let v : Vec<_> = args.collect();
        let mut i = 0;
        while i < v.len() - 1 {
            let a = try!(v[i].num());
            let b = try!(v[i+1].num());
            if !f(a, b) {
                return Ok(Sexp::Bool(false));
            }
            i = i + 1;
        }
        Ok(Sexp::Bool(true))
    })
}

//------------------------------------------------------------------------------
fn interpret<R: Read + 'static>(read: R,
                                a: &Analyzer,
                                env: FramePtr) -> SResult<bool> {
    let mut parser = Parser::new(read);
    loop {
        gc::maybe_collect(env.clone());
        let sexp = try!(parser.next_sexp());
        if let Sexp::Eof = sexp { break; }

        if sexp.car_is_id("import") {
            if let Sexp::Str(file_name) = try!(try!(sexp.cdr_take()).car_take()) {
                try!(process(file_name.clone(), a, env.clone()));
            }
            else {
                return Err("illegal `import` directive".into());
            }
        }
        else {
            println!(">> {}", sexp);
            let rsexpanded = a.expand_macros(sexp);
            match rsexpanded {
                Ok(expanded) => {
                    //println!(" ---> {}", &expanded);
                    let expr = a.analyze(expanded, false);
                    let rsresult = expr.call(env.clone());
                    match rsresult {
                        Ok(res) => {
                            println!("{}", res);
                            env.insert("$".into(), res)
                        },
                        Err(err) => println!("ERROR: {}", err)
                    }
                },
                Err(err) => println!("MACRO EXPANSION ERROR: {}", err)
            }
        }
    }
    Ok(true)
}

//------------------------------------------------------------------------------
fn process(file_name: String,
           a: &Analyzer,
           env: FramePtr) -> SResult<bool> {
    if file_name == "--" {
        let sin = stdin();
        interpret(sin, a, env)
    }
    else {
        interpret(try!(File::open(file_name)), a, env)
    }
}

//------------------------------------------------------------------------------
fn main() {
    println!("Welcome to Scheme!");

    let env = new_env(None);
    let ctx = Rc::new(IntCtx { env: env.clone() });
    install_builtins(ctx.clone());

    let a = Analyzer::new(ctx.clone());

    let args = std::env::args();

    if args.len() < 2 {
        if let Err(err) = process("--".to_string(), &a, env) {
            println!("Error in <stdin>: {}", err);
        }
    }
    else {
        for arg in args.skip(1) {
            if let Err(err) = process(arg.clone(), &a, env.clone()) {
                println!("Error in <{}>: {}", arg, err);
            }
        }
    }
}
