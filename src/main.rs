use std::cell::RefCell;
use std::collections::HashMap;
use std::io::prelude::*;
use std::io::{BufReader, stdin};
use std::iter::FromIterator;
use std::fmt::{Debug, Display, Formatter};
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
        let t = Token::Id(try!(self.reader.read_while(&|c: char| c.is_scheme_continue() )));
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
    Builtin(Rc<Builtin>),
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
        Cons(Box::new((car, cdr)))
    }

    fn iter<'a>(&'a self) -> SexpIter<'a> {
        SexpIter { cur : self }
    }

    //
    // Sexp Helper Functions
    //
    fn car<'a>(&'a self) -> &'a Sexp {
        if let Cons(ref b) = *self {
            let (ref car, _) = **b;
            car
        }
        else {
            panic!("sexp not a cons: {}", *self)
        }
    }

    fn car_take(self) -> Sexp {
        if let Cons(b) = self {
            let (car, _) = *b;
            car
        }
        else {
            panic!("sexp not a cons: {}", self)
        }
    }

    fn cdr<'a>(&'a self) -> &'a Sexp {
        if let Cons(ref b) = *self {
            let (_, ref cdr) = **b;
            cdr
        }
        else {
            panic!("sexp not a cons: {}", self)
        }
    }

    fn cdr_take(self) -> Sexp {
        if let Cons(b) = self {
            let (_, cdr) = *b;
            cdr
        }
        else {
            panic!("sexp not a cons: {}", self)
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

impl Display for Sexp {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match *self {
            Num(n) => write!(f, "{}", n),
            Id(ref id) => write!(f, "{}", id),
            Nil => write!(f, "()"),
            Sexp::Closure(_) => write!(f, "<closure>"),
            Sexp::Builtin(_) => write!(f, "<builtin>"),
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
            _ => unreachable!()
        }
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
            Cons(ref a) =>
                if let Cons(ref b) = *other { a == b } else { false },
            Sexp::Closure(ref a) =>
                if let Sexp::Closure(ref b) = *other {
                    (a as *const Rc<SClosure>) == (b as *const Rc<_>)
                } else { false },
            Sexp::Builtin(ref a) =>
                if let Sexp::Builtin(ref b) = *other {
                    (a as *const Rc<Builtin>) == (b as *const Rc<_>)
                } else { false },
            Nil => if let Nil = *other { true } else { false },
            Sexp::Eof => if let Sexp::Eof = *other { true } else { false },
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
    fn apply_new<I: Iterator<Item=Sexp>>(&self, args: I) -> Sexp;
}

#[derive(Debug)]
struct SClosure {
    env: FramePtr,
    arg_names: Vec<String>,
    rest_arg: Option<String>,
    expr: Expr
}

impl Apply for SClosure {
    fn apply_new<I: Iterator<Item=Sexp>>(&self, args: I) -> Sexp {
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

// TODO: figure out how to make this iter instead
type BuiltinPtr = Box<Fn(Vec<Sexp>) -> Sexp>;

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
    fn apply_new<I: Iterator<Item=Sexp>>(&self, args: I) -> Sexp {
        let arg_vec : Vec<_> = args.collect();
        (self.func)(arg_vec)
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

    fn set(&mut self, sym: String, val: Sexp) {
        self.symtab.insert(sym, val);
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
struct CapHack<T> {
    wrap_t: RefCell<Option<T>>
}

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

fn analyze(s: &Sexp) -> Expr {
    match *s {
        Num(_) | Nil => {
            let sc = s.clone();
            Box::new(move |_| sc.clone())
        },
        Id(ref sid) => analyze_env_lookup(sid),
        Cons(ref b) => {
            // This match might have been nicer to look (no guards) if rust
            // supported destructing a box.  But that syntax is currently
            // unstable, so we have to use guards.
            match **b {
                (Id(ref id), ref cdr @ Cons(_)) if id == "lambda" =>
                    analyze_lambda(cdr),
                (Id(ref id), ref cdr @ Cons(_)) if id == "quote" =>
                    analyze_quote(cdr),
                (Id(ref id), ref cdr @ Cons(_)) if id == "define" =>
                    analyze_define(cdr),
                _ => analyze_application(s)
            }
        }
        _ => Box::new(|_| Num(42f64))
    }
}

fn analyze_env_lookup(id: &String) -> Expr {
    let id = id.clone();
    Box::new(move |env| {
        if let Some(f) = env.borrow().find(&id) {
            if let Some(val) = f.lookup(&id) {
                return val.clone();
            }
        }
        panic!("Undefined variable: {}", id)
    })
}

// s: ((funcname arg1 arg2) body) or else (varname value)
fn analyze_define(s: &Sexp) -> Expr {
    let id;
    let val: Expr;
    if let Cons(_) = *s.car() {
        if let Id(ref funcname) = *s.car().car() {
            id = CapHack::new(funcname.clone());
            val = analyze_lambda(
                &Sexp::new_cons(s.car().cdr().clone(),
                                s.cdr().clone()));
        }
        else { unreachable!() }
    }
    else {
        if let Id(ref varname) = *s.car() {
            id = CapHack::new(varname.clone());
            val = analyze(s.cdr().car());
        }
        else { unreachable!() }
    }

    Box::new(move |env| {
        let valval = val(env.clone());
        env.borrow_mut().set(id.take(), valval);
        Nil
    })
}

fn analyze_quote(s: &Sexp) -> Expr {
    let ccar = s.car().clone();
    Box::new(move |_| ccar.clone())
}

fn analyze_lambda(s: &Sexp) -> Expr {
    // car is args and cdr is body
    let (ref car, ref cdr) = s.carcdr();
    let mut arg_cons_iter = car.iter();
    let arg_names: Vec<_> = arg_cons_iter.by_ref().map(|i| {
        if let Id(ref arg) = *i { arg.clone() }
        else { unreachable!() }
    }).collect();

    let rest_arg = CapHack::new(
        if let Id(ref id) = *arg_cons_iter.cur {
            Some(id.clone())
        } else { None });

    let arg_names_hack = CapHack::new(arg_names);
    let expr = CapHack::new(analyze_body(cdr));

    Box::new(move |env| {
        Sexp::Closure(Rc::new(SClosure {
            env: env.clone(),
            arg_names: arg_names_hack.take(),
            rest_arg: rest_arg.take(),
            expr: expr.take()
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

fn funcall<T>(func: Sexp, args: T) -> Sexp where T: Iterator<Item=Sexp> {
    if let Sexp::Closure(sc) = func {
        sc.apply_new(args)
    }
    else if let Sexp::Builtin(b) = func {
        b.apply_new(args)
    }
    else {
        panic!("funcall on non function object {}", func);
    }
}

fn analyze_application(sexp: &Sexp) -> Expr {
    let efunc = analyze(sexp.car());
    let eargs: Vec<_> = sexp.cdr().iter().map(|i| analyze(i)).collect();
    let eargs_hack = CapHack::new(eargs);

    Box::new(move |env| {
        let func = efunc(env.clone());
        let args_iter = eargs_hack.take().into_iter().map(|i| i(env.clone()));
        funcall(func, args_iter)
    })
}

//------------------------------------------------------------------------------
// Builtin Functions
//------------------------------------------------------------------------------
fn install_builtins(env: &FramePtr) {
    let b : Vec<(&str, Box<Fn(Vec<Sexp>) ->  Sexp>)> = vec!(
        ("+", mathy(Box::new(|s, i| s + i))),
        ("-", mathy(Box::new(|s, i| s - i))),
        ("/", mathy(Box::new(|s, i| s / i))),
        ("*", mathy(Box::new(|s, i| s * i))),
/*        ("cons", Box::new(|a| Sexp::new_cons(a.remove(0), a.remove(0)))),
        ("car", Box::new(|a| a.remove(0).car_take())),
        ("cdr", Box::new(|a| a.remove(0).cdr_take())),
        ("apply", Box::new(|args| scheme_apply(args))),*/
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
fn scheme_apply(args: Vec<Sexp>) -> Sexp {
    let mut it = args.iter();

    // the first arg of apply is the function to call
    let func = it.next().unwrap();

    // the inner n-2 args are supplied to the function
    let inner_args_it = it.take(args.len() - 2);

    // the final arg must be a SList which are appended to args from above
    let rest_args_it = args[args.len() - 1].iter();

    let args_it = inner_args_it.chain(rest_args_it);

    // TODO: need to `clone` here due to unstable attribute on `drain`
    let args_vec : Vec<Sexp> = args_it.map(|i| i.clone()).collect();
    funcall(func.clone(), args_vec.into_iter())
}

fn get_num(s: &Sexp) -> f64 {
    if let Num(n) = *s { n } else { unreachable!() }
}

fn mathy(f: Box<Fn(f64, f64) -> f64>) -> Box<Fn(Vec<Sexp>)->Sexp> {
    Box::new(move |args_vec| {
        let mut args = args_vec.into_iter();
        let init = get_num(&args.next().unwrap());
        Num(args.fold(init, |s, i| f(s, get_num(&i))))
    })
}


//------------------------------------------------------------------------------
fn main() {
    println!("Welcome to Scheme!");

    let sin = stdin();
    let mut parser = Parser::new(sin);
    let env  = new_env(None);
    install_builtins(&env);

    loop {
        let s = parser.next_sexp();
        if let Ok(Sexp::Eof) = s { break; }

        println!("DEBUG: Sexp: {:?}", s);

        if let Ok(sexp) = s {
            let expr = analyze(&sexp);
            let result = expr(env.clone());
            println!("{}", result);
        }
        else {
            break;
        }
    }
}
