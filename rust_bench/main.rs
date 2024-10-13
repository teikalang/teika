#![allow(non_camel_case_types)]

use std::{collections::HashMap, hash::Hash};

use logos::{Lexer, Logos};
use typed_arena::Arena;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+")]
enum Token {
    #[regex("[a-zA-Z0-9]+", |lex| lex.slice().to_owned())]
    VAR(Name),
    #[token("=>")]
    ARROW,
    #[token("=")]
    EQUAL,
    #[token(";")]
    SEMI,
    #[token("(")]
    LEFT_PARENS,
    #[token(")")]
    RIGHT_PARENS,
}

type Name = String;

#[derive(Debug)]
enum AST {
    L_var(Name),
    L_lambda(Name, Box<AST>),
    L_apply(Box<AST>, Box<AST>),
    L_let(Name, Box<AST>, Box<AST>),
}

fn parse(lex: &mut Lexer<Token>) -> AST {
    let (term, token) = parse_term(lex);
    assert_eq!(token, None);
    term
}
fn parse_term(lex: &mut Lexer<Token>) -> (AST, Option<Token>) {
    parse_let(lex)
}
fn parse_let(lex: &mut Lexer<Token>) -> (AST, Option<Token>) {
    let (bound, token) = parse_lambda(lex);
    match token {
        Some(Token::EQUAL) => match bound {
            AST::L_var(name) => {
                let (value, token) = parse_lambda(lex);
                assert_eq!(token, Some(Token::SEMI));
                let (body, token) = parse_let(lex);
                let value = Box::new(value);
                let body = Box::new(body);
                (AST::L_let(name, value, body), token)
            }
            AST::L_lambda(_, _) | AST::L_apply(_, _) | AST::L_let(_, _, _) => {
                panic!("invalid let pattern")
            }
        },
        None => (bound, None),
        Some(token) => (bound, Some(token)),
    }
}
fn parse_lambda(lex: &mut Lexer<Token>) -> (AST, Option<Token>) {
    let (param, token) = parse_apply(lex);
    match token {
        Some(Token::ARROW) => match param {
            AST::L_var(name) => {
                let (body, token) = parse_lambda(lex);
                let body = Box::new(body);
                (AST::L_lambda(name, body), token)
            }
            AST::L_lambda(_, _) | AST::L_apply(_, _) | AST::L_let(_, _, _) => {
                panic!("invalid lambda pattern")
            }
        },
        None => (param, None),
        Some(token) => (param, Some(token)),
    }
}
fn parse_apply(lex: &mut Lexer<Token>) -> (AST, Option<Token>) {
    let lambda = parse_atom(lex).unwrap();
    parse_apply_rec(lex, lambda)
}
fn parse_apply_rec(lex: &mut Lexer<Token>, lambda: AST) -> (AST, Option<Token>) {
    match parse_atom(lex) {
        Ok(arg) => {
            let lambda = Box::new(lambda);
            let arg = Box::new(arg);
            let lambda = AST::L_apply(lambda, arg);
            parse_apply_rec(lex, lambda)
        }
        Err(token) => (lambda, token),
    }
}
fn parse_atom(lex: &mut Lexer<Token>) -> Result<AST, Option<Token>> {
    let token = match lex.next() {
        Some(token) => Ok(token.unwrap()),
        None => Err(None),
    }?;
    match token {
        Token::VAR(var) => Ok(AST::L_var(var)),
        Token::LEFT_PARENS => {
            let (content, token) = parse_term(lex);
            assert_eq!(token, Some(Token::RIGHT_PARENS));
            Ok(content)
        }
        Token::RIGHT_PARENS | Token::ARROW | Token::EQUAL | Token::SEMI => Err(Some(token)),
    }
}

type Var = u64;
type Index = u64;
type Level = u64;

#[derive(Debug)]
enum Term {
    T_free_var(Var),
    T_bound_var(Index),
    T_lambda(Box<Term>),
    T_apply(Box<Term>, Box<Term>),
    T_subst(Box<Term>, Box<Term>),
}

enum Shifts<'a> {
    S_nil(),
    S_cons(Index, Level, &'a Shifts<'a>),
}
struct Substs<'a> {
    terms: Vec<(&'a Term, Level, &'a Shifts<'a>)>,
    shifts_arena: &'a Arena<Shifts<'a>>,
    shifts: &'a Shifts<'a>,
}
enum Args<'a> {
    A_nil,
    A_cons(&'a Term, Level, &'a Shifts<'a>, &'a Args<'a>),
}

impl<'a> Substs<'a> {
    pub fn new(shifts_arena: &'a Arena<Shifts<'a>>) -> Substs<'a> {
        Substs {
            terms: Vec::with_capacity(8 * 1024 * 1024),
            shifts_arena,
            shifts: &Shifts::S_nil(),
        }
    }
    pub fn length(&self) -> Level {
        self.terms.len() as u64
    }
    pub fn shifts(&self) -> &'a Shifts<'a> {
        self.shifts
    }
    pub fn load_shifts(&mut self, shifts: &'a Shifts<'a>) {
        self.shifts = shifts;
    }
    pub fn shift(&mut self, by: Index, at: Level) {
        let shifts = Shifts::S_cons(by, at, self.shifts);
        self.shifts = self.shifts_arena.alloc(shifts)
    }
    pub fn apply(&mut self, var: Var) -> &'a Term {
        let length = self.terms.len() as u64;
        // apply current shifts
        let mut offset = var;
        {
            let mut shifts = self.shifts;
            loop {
                match shifts {
                    Shifts::S_nil() => break,
                    Shifts::S_cons(by, at, next) => {
                        shifts = next;
                        let distance = length - at;
                        if offset >= distance {
                            offset = by + offset
                        } else {
                            break;
                        }
                    }
                }
            }
        };
        // load term and shifts
        let index = (length - 1 - offset) as usize;
        // TODO: stop panic here
        let (term, from, shifts) = self.terms.get(index).unwrap();
        self.shifts = shifts;
        {
            let by = length - from;
            let shifts = Shifts::S_cons(by, length, self.shifts);
            self.shifts = self.shifts_arena.alloc(shifts)
        };
        term
    }
    pub fn push(&mut self, to: &'a Term, at: Level, shifts: &'a Shifts<'a>) {
        self.terms.push((to, at, shifts))
    }
}

fn to_term<'a>(level: Level, env: &mut HashMap<Name, Level>, ast: AST) -> Term {
    match ast {
        AST::L_var(var) => match env.get(&var) {
            Some(from) => Term::T_bound_var(level - from),
            None => panic!("missing var {}", var),
        },
        AST::L_lambda(var, body) => {
            let body = {
                let level = 1 + level;
                let _ = env.insert(var.clone(), level);
                let body = to_term(level, env, *body);
                let _ = env.remove(&var).unwrap();
                body
            };

            let body = Box::new(body);
            Term::T_lambda(body)
        }
        AST::L_apply(lambda, arg) => {
            let lambda = to_term(level, env, *lambda);
            let arg = to_term(level, env, *arg);

            let lambda = Box::new(lambda);
            let arg = Box::new(arg);
            Term::T_apply(lambda, arg)
        }
        AST::L_let(var, value, body) => {
            let value = to_term(level, env, *value);
            let body = {
                let level = 1 + level;
                let _ = env.insert(var.clone(), level);
                let body = to_term(level, env, *body);
                let _ = env.remove(&var);
                body
            };

            let value = Box::new(value);
            let body = Box::new(body);
            Term::T_subst(body, value)
        }
    }
}

fn expand_head<'a>(
    args_arena: &'a Arena<Args<'a>>,
    term: &'a Term,
    substs: &mut Substs<'a>,
) -> (&'a Term, &'a Args<'a>) {
    let mut term = term;
    let mut args: &'a Args<'a> = args_arena.alloc(Args::A_nil);
    loop {
        match term {
            // substs
            Term::T_bound_var(var) => {
                term = substs.apply(*var);
            }
            // zeta
            Term::T_subst(body, value) => {
                let () = {
                    let length = substs.length();
                    let shifts = substs.shifts();
                    substs.push(value.as_ref(), length, shifts);
                };
                term = body.as_ref();
            }
            // beta
            Term::T_lambda(body) => match *args {
                Args::A_cons(arg, at_, arg_shifts, new_args) => {
                    let () = substs.push(arg, at_, arg_shifts);
                    term = body.as_ref();
                    args = new_args;
                }
                Args::A_nil => break,
            },
            Term::T_apply(lambda, arg) => {
                let length = substs.length();
                let shifts = substs.shifts();
                term = lambda.as_ref();
                args = args_arena.alloc(Args::A_cons(arg.as_ref(), length, shifts, args));
            }
            // head
            Term::T_free_var(var) => break,
        }
    }
    (term, args)
}
fn equal<'a>(
    args_arena: &'a Arena<Args<'a>>,
    arena: &'a Arena<Term>,
    next_var: Var,
    left: &'a Term,
    left_substs: &mut Substs<'a>,
    right: &'a Term,
    right_substs: &mut Substs<'a>,
) {
    let (left, mut left_args) = expand_head(args_arena, left, left_substs);
    let (right, mut right_args) = expand_head(args_arena, right, right_substs);
    equal_struct(
        args_arena,
        arena,
        next_var,
        left,
        left_substs,
        right,
        right_substs,
    );

    loop {
        match (left_args, right_args) {
            (
                Args::A_cons(left, left_from, left_shifts, new_left_args),
                Args::A_cons(right, right_from, right_shifts, new_right_args),
            ) => {
                left_args = new_left_args;
                right_args = new_right_args;

                left_substs.load_shifts(left_shifts);
                {
                    let at = left_from;
                    let by = left_substs.length() - left_from;
                    left_substs.shift(by, *at);
                };

                right_substs.load_shifts(right_shifts);
                {
                    let at = right_from;
                    let by = right_substs.length() - at;
                    right_substs.shift(by, *at)
                };

                equal(
                    args_arena,
                    arena,
                    next_var,
                    left,
                    left_substs,
                    right,
                    right_substs,
                );
            }
            (Args::A_nil, Args::A_cons(_, _, _, _)) => todo!(),
            (Args::A_cons(_, _, _, _), Args::A_nil) => todo!(),
            (Args::A_nil, Args::A_nil) => break,
        }
    }
}
fn equal_struct<'a>(
    args_arena: &'a Arena<Args<'a>>,
    arena: &'a Arena<Term>,
    next_var: Var,
    left: &'a Term,
    left_substs: &mut Substs<'a>,
    right: &'a Term,
    right_substs: &mut Substs<'a>,
) {
    match (left, right) {
        (Term::T_bound_var(_), _) | (_, Term::T_bound_var(_)) => todo!(),
        (Term::T_apply(_, _), _) | (_, Term::T_apply(_, _)) => todo!(),
        (Term::T_subst(_, _), _) | (_, Term::T_subst(_, _)) => todo!(),
        (Term::T_free_var(left), Term::T_free_var(right)) if left == right => (),
        (Term::T_lambda(left_body), Term::T_lambda(right_body)) => {
            let skolem = arena.alloc(Term::T_free_var(next_var));
            {
                let length = left_substs.length();
                let shifts = left_substs.shifts();
                left_substs.push(skolem, length, shifts)
            };
            {
                let length = right_substs.length();
                let shifts = right_substs.shifts();
                right_substs.push(skolem, length, shifts)
            };
            equal(
                args_arena,
                arena,
                1 + next_var,
                left_body.as_ref(),
                left_substs,
                right_body.as_ref(),
                right_substs,
            )
        }
        _ => panic!("clash"),
    }
}

fn term_of_string<'a>(code: &str) -> Term {
    let mut lex = Token::lexer(code);
    let ast = parse(&mut lex);
    to_term(0, &mut HashMap::new(), ast)
}
fn main() {
    let args_arena = Arena::with_capacity(8 * 1024 * 1024);
    let shifts_arena = Arena::with_capacity(8 * 1024 * 1024);
    let arena = Arena::with_capacity(8 * 1024 * 1024);
    let left = term_of_string(
        "
            zero = z => s => z;
            succ = pred => z => s => s (pred z s);
        
            one = succ zero;
        
            add = a => b => a b succ;
            mul = a => b => a zero (n => add n b);
        
            two = succ one;
            n4 = mul two two;
            n8 = mul n4 two;
            n16 = mul n8 two;
            n32 = mul n16 two;
            n64 = mul n32 two;
            n128 = mul n64 two;
            n256 = mul n128 two;
            n512 = mul n256 two;
            n512
        ",
    );
    let right = term_of_string(
        "
            z => s => 
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
            z
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
            ))))))))))))))))
        ",
    );
    use std::time::Instant;
    let now = Instant::now();

    // Code block to measure.
    {
        let mut left_substs = Substs::new(&shifts_arena);
        let mut right_substs = Substs::new(&shifts_arena);
        equal(
            &args_arena,
            &arena,
            0,
            &left,
            &mut left_substs,
            &right,
            &mut right_substs,
        );
    }

    let elapsed = now.elapsed();
    println!("Left: {:?} ", left);
    println!("Right: {:?}", right);
    println!("Elapsed: {:.2?}", elapsed);
}
