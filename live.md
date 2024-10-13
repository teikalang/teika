```rust
12345
12345
-----
10
-----
90


add(0, y) = y
add(S(x), y) = add(x, S(y))


add(, y) = y;
f(x) = x + 1;

0  1  2  3  4
0 -1 -2 -3 -4 ...

in . out = id

(x)
(x => m)
(m(n))

((x => m)(n)) === m[x := n]


true === x => y => x;
false === x => y => y;

if_ === pred => then => else => pred(then)(else);
not === b => if_(b)(false)(true);
and === a => b => if_(a)(b)(false);

and === a => b => a(b)(false);

false && false === false
false && true === false
true && false === false
true && true === true

fix = (x => x(x));
loop = fix(fix)

fix(fix)
(x => x(x))(fix)
(x(x))[x := fix]
fix(fix)
(x => x(x))(fix)
(x(x))[x := fix]
fix(fix)
(x => x(x))(fix)
(x(x))[x := fix]
fix(fix)

0
S(n)

0 === 0
S(0) === 1
S(1) === 2
S(2) === 3


add(0, m) = m;
add(succ(pred), m) = add(pred, succ(m));


fix = (x => x(x));
one === succ(zero) === (z => s => s(z));
two === succ(one) === (z => s => s(s(z)))

(z => s => z)
(z => s => s(z))
(z => s => s(s(z)))
(z => s => s(s(s(z))))

y === f => (x => f(x(x)))(x => f(x(x)))
z === f => (x => f(v => x(x)(v)))(x => f(v => x(x)(v)))

zero === z => s => z;
succ === n => z => s => s(n(z)(s));

add_open === add => n => m =>
  n(m)(pred => add(pred)(succ(m)))
fix === f => x => f(v => x(x)(v));
z === f => fix(f)(fix(f));
add === z(add_open);

two === z => s => s(s(z));
two === z => s => s(s(z));

add(two)(two)
fix(add_open)(fix(add_open))(add_open)(two)(two)
fix(add_open)(fix(add_open))(((((pred => (v => (fix(add_open))(fix(add_open))(v))(pred)(succ(two))))(two)))(succ(two)))

n => (succ(n) > n);

para todo x, succ(x) é maior do que y

Term -> Term
Type -> Term
Type -> Type
Term -> Type

| (x : A) -> B
| <A>B
| <X> => B;
| A<B>

(x)
((x : T) => m)
(m(n))
(m<T>)

Bool === <A>(x : A) -> (y : A) -> A;
true === <A>(x : A) => (y : A) => x;
false === <A>(x : A) => (y : A) => y;

Nat === <A>(z : A) -> (s : (acc : A) -> A) -> A;
zero === <A>(z : A) => (s : (acc : A) -> A) => z;
succ === n => <A>(z : A) => (s : (acc : A) -> A) => s(n<A>(z)(s));


fix === (x : ?) => x(x);


| (x : A) -> B
| <A>B
| <X> => B;
| A<B>



| Type
| x
| (x : A) => m
| (x : A) -> B
| m n


Bool === (A : Type) -> (x : A) -> (y : A) -> A;
true === (A : Type) => (x : A) => (y : A) => x;
false === (A : Type) => (x : A) => (y : A) => y;

f : (pred : Bool) -> (x : pred(Type)(Nat)(String)) -> pred(Type)(Nat)(String)
  = (pred : Bool) => (x : pred(Type)(Nat)(String)) => x;


: (pred : Bool) -> (x : pred(Type)(Nat)(String)) -> pred(Type)(Nat)(String)

a : (x : Nat) -> Nat = f(true)
b : (x : String) -> String = f(false)

dup = x => [x, x];

[socketA1, socketA2] = dup(socketA);

[memA1, memA2] = dup(memA);

[ptrMemA1, ptrMemA2] = dup(ptrMemA);

dup = x => [x, x];


free(memA);
free(memA);

close(socket);
send(socket);


Array.get : Array<A> -> Nat -> (A, Array<A>);

arr : Array<Bool>
(x, arr) = Array.get(arr, 0);
(y, arr) = Array.get(arr, 1);

Array.get : &Array<A> -> Nat -> A;
arr : Array<Bool>
x = Array.get(arr, 0);
y = Array.get(arr, 1);

r0 : Register
r1 : Register
r2 : Register
r3 : Register
rsp : Stack[0]

// put a constant in the register
const : Register -> Constant -> Register
// add a b = returns (a + b, b)
add : Register -> Register -> (Register, Register);
// push
push : Stack[n] -> Register -> (Stack[n + 1], Register);


a = const r0 1;
b = const r1 1;
a_plus_b, b = add r0 b;

rsp, a = push rsp a;


Array.set(arr, 0, arr);


Unknown variable : Potato

Type mismatch :
  received : ABC
  expected : DEF

Syntax error

Syntax error, parens X was not closed


(2 x : Nat) => x + x;

[@deriving serdes]
User = {
  id : Nat;
  name : String;
};

dup = (b : Bool) => b (x => (b : Bool 2, b == x)) (true, refl) (false, refl);
fix
  : %fix(Rec). Rec -> Nat
  = (2 make : (G : Grade) -> Bool G) => (
    b : Bool 10 = make 10;
  );

double = (2 x : Nat) => x + x;

M : {
  double : (x : Nat) -> Nat;
} = {
  double = x => x + x;
};

@(%fix(unit). x => x) x === x

fix = x => x(x);

fix(fix);
x(x)[x := fix];
fix(fix);


```
