# Examples

```rust
[@import stdlib.http.v1];

(x => y => (x_plus_y: x + y) => );

((A, x))
(f: () -> (A: *, x: A)) =>
  (A, x) = (f ());
  A;

id = (A: *) => (x: A) => x;
id = id (*) Int;
X = Int -> Int;

id: (A: *) -> A -> A = x => x;

X: * = 1;

pure: (M: * -> *) -> (A: *) -> (A -> M A) -> A -> M A;


Show = {
  T: *;
  show: T -> String;
};
show: {S: Show} -> S.T -> String;

M: {
  T: *;
} = {
  T = Int;
}

T A = T A
T A = T B

* -> *

X = (A: *) -> A;
M = {
  T: *;
};

F = (M: { X = * }) => ();

F = (M: { T: *; x: T; show: T -> String; }) => M.show M.x;
F = ({ T: *; x: T; show: T -> String; }) -> String;

F = (M: { T: *; show: T -> String; }) => (x: M.T) => M.show x;
F = (M: { T: *; show: T -> String; }) -> M.T -> String;

F = (X: { T: * }) => X;

Pair {A} = (A, A);

id = x => x;
T = Int -> Int;


x = true;
y = x | true -> 1 | false -> 0;

User = {
  id: Nat;
  name: String;
};

eduardo = {
  id = 0;
  name = "Eduardo";
};

Monad = {
  T: * -> *;
  pure: {A} -> A -> T A;
};

id: (A: *) -> A -> A;
m: (A: *) -> A -> { X: * };

id x = x;
rec%map f l =
  l
  | [] -> []
  | el :: tl -> f el :: map f tl;

X = *;
S = { A: * };
M: S = { A = Int };
((M: S) => M) ({ A = Int });

f (M: S) = ();

type_struct_id ({ A }: { A: * }) (x: A) = x;

A = Int;
B = 1;

T: * = Int;

X = {
  A: *;
  B: *;
  X: Unify A A;
};

T = { A: * };
F = (P: { A: _ }) => P.A
m =
  M = { T = Int };
  F M;

M = [%graphql {
  users(id: 0) {
    name
    email
  }
}];

f = [%c {
  x: for (i = 0; i <= 10; i++) {
    print(i);
  };

  if (true) {
    print("a");
  } else {
    print("b");
  };

  goto x;
}];

q = [%sql SELECT * FROM users WHERE name = "Eduardo" WHERE id = 0]

{ a; b; } = { a = 1; b = (); };

X =
  M: { A: *; } = { A = Int; };
  M;
X = ((M: { A: *; }) => M) { A = Int; };
X = ({ A = Int; } : { A: *; });

f = x => x;

f = ?A => A;
?M = {};
x = (a: int) => a;

add: !(x: Int, y: Int) -> Int = (x, y) => x + y;
id: ?A -> A -> A;
id: {A} -> A -> A;

pure: ?(M: Monad) -> ?A -> A -> M A;
pure: {M: Monad} -> {A} -> A -> M A;

add: Int -> Int -> Int;

incr = ?(1 + _);
incr = ?add 1;

F = (A: *) => A;
F = A => A;

M: { T: * } = { T}
F = (X: { T: * }) => X;
S = {
  T: *;
};
M: { A: * } = { A :}

Show = {
  T: *;
  show: T -> String;
};
show {S: Show} (x: S.T) => S.show x;

({A}) = {
  T = Int;
  show x = Int.to_string x;
};

show 1;
show "a";

{M} = {}

User #= {
  id: Nat;
  name: String;
};

Id = (A: *) => A;

map: {A} -> {B} -> (A -> B) -> List A -> List B;
map: {A} -> {B} -> (A ~> B) -> List A ~> List B;

map f l =
  l
  | [] -> []
  | el :: tl -> f el :: map f tl;

get: String -[HTTP]> String;
read_disk file =
  content = perform Read ("/tmp/" ^ file);
  content ^ "x";

read_disk: String -[IO]> String;
id: {A} -> A -> A;

map_read_disk: List Int -[IO]> List Int = map read_disk;
map_id: {A} -> List A -> List A = map id;

f: Int ~> Int;

// TODO: explain why this should be rejected
T = ({ X; A: * })) => X == A;
T = ({ X: *; A = X; })) => X == A;

f = (A: *) => (x: A) => x;
f = ({ A: *; x }) => (x: A);

f = T => (x: T Int) => x;


f: (T: #(Int) -> *) -> T Int -> T Int;

T: #(Int) -> *

f = T => (x: T (Int: *)) => x;
f: (T: * -> *) -> T Int -> T Int;

f = f Option;

expected:
received: {A} -> A -> A;

S = {
  A: _;
  X: *;
};

Id (A: *) = A;

X = Id Int

F (A: *) = A;

App {P} {R} (F: P -> R) (X: P) = F P;
// TODO: universe polymorphism to accept the following
F: (T: * -> *) -> App T Int;
F: (T: * -> *) -> T Int;
F_ (T: * -> *) = F T;
F: (T: #Int -> #String) -> T Int;

T Int = (Return T)[(Param T) := Int];

read_file: String -[ IO ]> String;

Component {Props} = Props -> DOM;

rec%map f l = l | [] -> [] | el :: tl -> map f l;

Option {A} =
  | None
  | Some A;

f x = x | None -> 1 | Some x -> 1;

Option {A} = {
  None: Option {A};
  Some: A -> Option {A};
};

{ None; Some } = Option;




None: {A} -> (| None | Some A);
Some: {A} -> A -> (| None | Some A);

f: (| A | B ) = ;
Option: (A: *) => | None | Some A;

rec%add =
  | (0, y) => y
  | (x, y) => add (x - 1, y + 1);

f =
  | x => 1
  | B => 2
  for

f = a -> a;
T: (A: *) => A = (A: *) => A;

f: (
  Id A = A;
  Id Int
) = 1;

{} = {}

add (a, b) = a + b;
three = add (1, 2);

x = add 1;


@deriving(field)
User = {
  id: Nat;
  name: String;
  banned: Bool;
};

eduardo = { id = 0; name = "Eduardo" };

view_name user = <span>user.name</span>;

view_name user =
  { name; banned; _ } = user;
  name = banned | false -> name | true -> "_" ^ name;
  <span>name</span>;

linear_id {A: &} (x: A) = x;
y = 1;
x = linear_id 1;
State = {
  counter: Int;
};
x = Rc.alloc State { counter = 1; };
x = Rc.free State;
M (state: State): State =
  { counter } = state;
  { counter }

T = {
  X: Option *;
  A: Param X;
};

X: { T: * };
M = X;

Id (A: *) (x: A) = x;
F A B = Id B A;


X: _;
F: (P: { X: X; A: *; }) -> ();

M = {
  T: *;
};

x = (1: Int);
x = ((x: Int) => x);

x: (A: *, A) = (A = Int, x = 1);

{ A; x; y; }: { A: *; x: A; y: A } = { A = Int; x = 1; y = 2; };

a = (a: Nat, b: Nat);

id: (A: *) -> A -> A;
map: {A B} -> (f: A -> B, l: List A) -> List B;
map: (f: 'A -> 'B, l: List 'A) -> List 'B;

sequence1: (A: *) -> A -> (B: *) -> B -> B;
sequence2: (A: *) -> (B: *) -> A -> B -> B;
record1: { A: *; B: *; a: A; b: B; };
record2: { A: *; a: A; B: *; b: B; };

_ -> _ ->

(P => E2) E1;
(P = E1; E2);
F (X: _) (A: *) = assert false

Monad = (
  Monad: * -> *;
  pure: (A: *) -> Monad A;
);
```

## Calculus

Two basic sorts, small type(sT) and type(T), where sT contain all small types that are subject to the FORGET rule that allows to introduce implicit existentials and T are all small types that are not subject to the FORGET rule.

Ideally it should also contain an explicit way to weaken T, such as `T[A := *]`, `T[-A]` maybe `T[A: *]`.

This should allow the type system to be decidable while polymorphism still works for existential records. And I believe it to be sound as existential records are weak existential records. I'm still a bit concerned if that doesn't break in the presence of dependent products.

`A: *` means A is a type(T), so `(A: *) -> A -> A` is identity function and still works for modules.

```rust
T => (x: T Int)

(x: &Int) -> (&Int, &Int)
```

## Duplicated

```haskell
expected :: (forall a. a -> a) -> _b -> ()
received :: forall c. c        -> c  -> ()

expected :: (forall a. a -> a) -> _b -> ()
received :: _c                 -> _c -> ()

received :: (forall a. a -> a) -- _c := (forall a. a -> a)
expected :: _c


```

```rust
f (a: 'A) (b: 'A) = (b : {A} -> A -> A);

Id = {A} -> A -> A;
f: Link Id -> Link Id -> Link Id;

```

## SmolTeika

```rust
X = { A: Int; B: Int; C: Int; };

x: X = magic ();
{ A; B; C; } = x;

X = (A: Int, (B: Int, C: Int));
x: X = magic ();
A = fst X;
B = fst (snd X);
C = snd (snd X);

(A: *) -> A;

M: (A: *, (A, A -> String)) = magic ();


M: (A: *, x: A) = (A = Int; x = 1);

U = (A: *, ((A = A, A -> ())) -> ());
V = (A = U, A -> ());

V <= U;
(A = U, A -> ()) <= (A: *, ((A = A, A -> ())) -> ());

(A = U, A -> ()) <= (A: *, ((A = A, A -> ())) -> ());

Int = (#Int: ##Int);

N = (A: *) => A;

N: (A: *, #A) = (A = Int, A);

IdT = (A: *) -> #A;
f (Id: IdT) (x: Id Int) = x;

f (T: * -> *)
Id: (A: *) -> #A = (A: *) => A;
(A: *) -> *

T = (A: *, (_ = A, x: A));

T = (A: *) -> (B = A, _ : A);
x = f (Int -> Int);

T = (A: *) => (B = A, _ = A);`

x = (A = *, A);

(A: *, (x: A, y: A))
(A: *, _: (B: *, (x: A, y: B)));
(A: *, _: (B: *, (x: B, y: A)));
(B: *, _: (A: *, (x: A, y: B)));
(B: *, _: (A: *, (x: B, y: A)));

(A: *, _: (x: A, (B: *, y: B)));
(B: *, _: (x: B, (A: *, y: A)));


(A: *, _: (x: A, (B: *, y: B)));
(A: *, _: (x: A, (B: *, y: B)))

add a b = (
  c = a + b;
  c
);


(x: t = e1; e2);
(((x: t) => e2) (e1));

(e: t);
(((x: t) => x) e);

M = {
  Y: (A: *) = (A = Int, x = one);
};


(A: *, x: A) -> A;
X = P;

m: Fst P = snd X;

(L, m) = P;
x = (m: L);

f = (A: *) => (P: (A = A, x: A)) => P;

// TODO: support this type
(f: (A: *, x: A) -> (A = A, x: A));

(f: (P: (A: *, x: A)) -> (A = Fst P, x: A));
(f: )

Ex = (A: *, x: A);
// works, one: int
one =
  P: Ex = (A = Int, x = one);
  snd P;
one = ((P: Ex) => snd P) (A = Int, x = one);

// fails, type would escape it's scope
fails =
  P: Ex = ((A = Int, x = one): Ex);
  snd P;
fails = ((P: Ex) => snd P) ((A = Int, x = one): Ex);

// fails, type would escape it's scope
fails =
  P: Ex = (A = Int, x = one);
  M = (A = fst P, x = snd P);
  snd M;
fails = ((P: Ex) => snd P) ((A = Int, x = one): Ex);

M: (A: *, x: A) = M;
X = M;
P = X.A;
M: (A = M.A, x: A)

f = (X: Int) => (A: *, x: A);
(f 1).A

f (T: * -> *)
f (A: *) =
  A
  | Nat -> "nat"
  | String -> "string";
f (A: &) =
  A
  | Nat -> "nat"
  | String -> "string";

f: (Car -> ()) -> ();
f ((x: Tesla) => ());


expected :: Car -> ()
received :: Tesla -> ()

received :: Car
expected :: Tesla

expected :: ()
received :: ()


f = (M: (A: *, x: A)) -> M.A;

x = f ((A = Int, x = 1): (A: *, x: A))


(A: Int, x: A).A

_{ A: _; }
x => x.1;

x = (x = 1, y = 2)

User = (
  id: Nat,
  name: String,
);

eduardo = (
  id = 0,
  name = "a"
);

User = {
  id: Nat;
  name: String;
};
eduardo = {
  id = 0;
  name = "a";
};

f = (x: (A: Int; ..._)) => x.A;
z = f (A = 1, x = 2);

a = { (x, y) = (1, 2); };
b = { x: Nat; y: Nat; };

T = Int -> (Int, Int);
x = (Int, Int);

fst: (R: * -> *) -> (P: (L: *, r: R L)) -> P.L;
snd: (R: * -> *) -> (P: (L: *, r: R L)) -> R P.L;

F: Nat -> #Int;

Id: (A: *) -> A -> A;

x = Id ((A: *) -> #A);

// λ→
((x: Int) -> Int): *;
((A: #Int) -> Int): *;
((x: Int) -> #Int): *;

// System λω
((A: *) -> #A): * -> *;
((A: * -> *) -> #T): (* -> *) -> * -> *;
((A: * -> *) -> #(T Int)): (* -> *) -> *;
((A: #Int) -> (A: *) -> #A): _;

// System Fω
((A: *) -> A): *;
((T: * -> *) -> T A): *;
((T: * -> *) -> T): _;

(x: Int, y: Int): *;
(A: #Int, B: #Int): *;

(A: *, x: A): *;
(T: * -> *, x: T Int): *;
((A: *) -> A): *;
((T: * -> *) -> T A): *;

((A: *) -> #A): * -> *;
((T: * -> *) -> #T): (* -> *) -> * -> *;
(A: *, X: #A): (*, *);
(T: * -> *, X: #T): (* -> *, * -> *);

*;
* -> *;

Id = (A: *) => A;
make = (A: *) => (x: A) => (x = A, y = 1);
accept: (M: (X: *, Y: X)) -> M;
(X

p = f ((A: *) -> #A) Id;

p = (x = Id, y = 1);

pair: (A: *, B: A) = (A = Int, B = 1);


m =
  p: (A: *, x: A) = (A = Int, x: Int);
  p;

m = ((p: (A: *, x: A)) => p)
      ((A = Int, x: Int): (A: *, x: A));

x := 1;


T = a == b;
t = 1 === 1;
T: #Int = Int;
x <== e1;
e2
((x => e2) e1)
(x <- e1; e2)


id = (A: *) => (x: A) => x;
id = {A: *} => (x: A) => x;
id = x => x;

Monad = {
  T: * -> *;
  pure: A -> T A
};
pure: {M: Monad} -> {A: *} -> A -> M A;

option_int: Option Int = pure {Option} {Int} 1;

make_array: (x: Int | x >= 0) -> (A: *) -> (initial: A) -> Array A;

twice_one = ((_: gte -1 0) -> (A: *) -> (initial: A) -> Array A) ;


// m.te
// m.tei
// m.test.tei
// m.extend.tei

// untyped
((x => e2) e1) === (e2[x := e1]);
(x = e1; e2) === (e2[x := e1]);

// subtyping
(((x: s) => e2) (e1: t)) =~= (e2[x := e1]);
(x: s = (e1: t); e2) =~= (e2[x := e1]);

(m: t) == (((x: t) => x) m);
(x: t = e1; e2) == (((x: t) => e2) e1);
(x: t = e1; e2) != (x = (e1: t); e2);


F (make_m ());

p = ((A = Int, x = 1) : (A: *, x: A));


Ex = (A: *, x: A);
one =
  P: Ex = (A = Int, x = one);
  snd P;
one = ((P: Ex) => snd P) (A = Int, x = one);


(e : t) === (((x: t) => x) e)

{ x: Nat } <: { x: Nat; y: Nat }

(((x: { x: Nat; }) => x) (_: { x: Nat; y: Nat }))

Program =
  ((magic ()), (magic ()), e2));
Program =
  (User: User_S = magic ();
   Message: Message_S = magic ();
   (User, Message, e2));
Test = ();

e = (x: Int) => x;
x1 = (e: (x: Int) -> Int);
x2 = ((z: (x: Int) -> x) => z) e;

(x := e1; e2)

(x: Nat) => (y: x == 1) => x;

n = 1;
x =
  1 == n
  | Eq => x
  | Neq => y;

Nat = {n} -> Int | n >= 0;

((x: 1) => x) 1;

(x = e1; e2);


S (T: *) = (zero: T; succ: T -> T);
ExM (R: *) (cb: (T: *) -> S T -> R) =
  cb Int (zero = 0; succ = x => x + 1);
M = ExM ();

Id = (A: *) -> (x: A) -> (x: A);
id: (A: *) -> (x: A) -> A = (A: *) => (x: A) => x;
f = (id: (A: *) => (x: A));

Bool = (R: &) => R => R => R;
(true_: Bool) R a b = a;
(false_: Bool) R a b = b;
if_true_x_else_x_concat_y (pred: Bool) (x: String) (y: String) =
  pred String x (x ^ y);

Bool = (I: &) -> (O: &) -> (I -> O) -> (I -> O) -> I -> O;
(true_: Bool) I O x y a = x a;
(false_: Bool) I O x y a = y a;
(not_: Bool -> Bool) b = b Bool Bool (() => false_) (() => true_) ();
(and_: Bool -> Bool -> Bool) l r = l Bool Bool (r => r) (_r => false_) r;
(or_: Bool -> Bool -> Bool) l r = l Bool Bool (_r => true_) (r => r) r;

Bool =

if_true_x_else_x_concat_y (pred: Bool) (x: String) (y: String) =
  pred String String (x => x) (x => x ^ y) x;
if_ (pred: Bool) =>
  (bool: A -> B) =>
  (else_: A -> B) =>

Nat: {
  T: *;
} = {
  T = Int;
};

Id = (L: U) => (A : * : L) => (x: A) => x;

M: {
  T: *;
  x: T;
} = {
  S = {
    name: String;;
  };
  x = {
    name = "Eduardo";
  };
};

f = (P1: (A: *; x: A)) =>
  (A; x) = P1;
  (A; x);
f = (P1: (A: *; (B = A; x: A)) =>
  (A; (B; x)) = P1;
  (A; (B; x));

S: {
  T: _;
  x: T;
} = {
  T = Int;
  x = 1;
}
x: M.T = M.x;

f = (K: ?) => (A: K) => (x: A) => x;
f: (A: *) -> (B: *) -> A -> B = assert false

```

```ocaml
LAMBDA(
  DUP;
  CALL;
);
DUP;
CALL;

PUSH 1;
PUSH 2;
ADD;

PUSH 1;
LAMBDA(PUSH 2; ADD);
CALL;

1 + 2
(x => x + 2) 1

```

# Hurkens

```rust
⊥ = (A: *) -> A;
¬ φ === φ -> ⊥;

℘ S === S -> *;
U = (X: □) -> (℘℘X -> X) -> ℘℘X;
τ: ℘℘U -> U = (t: ℘℘U) => (X: □) => (f : ℘℘X -> X) => (p : ℘X) =>
  t ((x : U) => p (f (x X f)));
σ: U -> ℘℘U = (s: U) => s U τ;
∆ = (y: U) => (p: ℘U) -> σ y p -> p (τ (σ y));
Ω: U = τ ((p: ℘U) => (x: U) -> σ x p -> p x);
```

# Opposites

- positive function, negative args
- negative function, positive args

```rust
// works
pid = (x: -) =[+]> x;
nid = (x: +) =[-]> x;

// works
pnapply = (f: -) =[+]> (x: +) =[-]> f (x: +);
npapply = (f: +) =[-]> (x: -) =[+]> f (x: -);
// rejected
ppapply = (f: -) =[+]> (x: -) =[+]> = f (x: +);
nnapply = (f: +) =[-]> (x: +) =[-]> = f (x: -);

// rejected
fix = f => f f;
apply_fix = f => apply f f;
id_fix = f => id f f;

pnfix = (x: -) =[+]> (y: +) =[-]> x y;
npfix = (x: +) =[-]> (y: -) =[+]> x y;

pfix = (f: -) =[+]> f (f: +);
nfix = (f: +) =[-]> f (f: -);

pflip: - -[+]> + = ?;
nflip: + -[-]> - = ?;

fix = f => f(f);
fix(fix)

id: (K: □) -> (A: K) -> (x: A) -> A =
  (K: □) => (A: K) => (x: A) => x;
```

## Polar Types

All types have polarity and no restriction on elimination, just on construction.

### Polar Arrows

If arrows themselves have polarity, what is the relationship between the parameter and the return with the function?

Cases to study

| Description    | Conclusion              |
| -------------- | ----------------------- |
| p. x -[p]> p   | Polarity is meaningless |
| p. p -[p]> x   | Polarity is meaningless |
| p. x -[p]> -p  | ?: fix can be described |
| p. p -[p]> -p  | ?: fix can be described |
| p. -p -[p]> x  | ?: lazy fix still works |
| p. -p -[p]> p  | Empty, no id or apply   |
| p. -p -[p]> -p | ?                       |

```rust
Nat = (R: -p) =[p]> R =[p]> (Nat =[-p]> R) =[p]> R;
Self = R =[p]> Self =[p]> R


// -p -[p]> x
// accepted
Loop = (() -[-]> Loop) -[+]> -⊥;
fix = (f: Loop) =[-]> f (() =[-]> f);
_ = fix (self =[+]>
  f = self ();
  f (() =[-]> f)
);
// rejected
Loop = Loop -[p]> ⊥;
fix = (f: Loop) => f f;

// -p -[p]> x
℘ S === S -> *;
U = (X: □) -> (℘℘X -[-p]> X) -[p]> ℘℘X;
τ: ℘℘U -> U =
  (t: (U -[p]> *) -[-p]> *) =[p]>
  (X: □ : p) =[p]>
  (f : ℘℘X -[-p]> X) =[p]>
  (p : (X -[-p]> *)) =[p]>
  t ((x : U) =[p]> p (f (x X f)));
σ: U -> ℘℘U = (s: U) => s U τ;
∆ = (y: U) => (p: ℘U) -> σ y p -> p (τ (σ y));
Ω: U = τ ((p: ℘U) => (x: U) -> σ x p -> p x);

// p -[p]> -p
Loop = Loop -[+]> -⊥;
fix = (f: Loop) =[+]> f f;

// -p -[p]> -p
// accepted
Loop = (() -[-]> Loop) -[+]> -⊥;
// rejected
fix = (f: Loop) =[-]> f (() =[-]> f);
// accepted
Loop = (() -[-]> Loop) -[+]> () -[-]> +⊥;
fix = (f: Loop) =[-]> f (() =[-]> f) ();
_ = fix (self =[+]> () =[-]>
  f = self ();
  f (() =[-]> f) ()
);

(f => f(() => f)())(self => {
  const f = self();
}

// rejected
Loop = Loop -[p]> ⊥;
fix = (f: Loop) => f f;

id = (A: -p) => (x: A) =[p]> x;
apply = (A: p) => (f: A -[-p]> +⊥) =[p]> (x: A) =[-p]> f x;


Nat = (R: -p) =[p]> (() =[-p]> R) =[p]> (Nat =[-p]> R) =[p]> R;


```

```rust
(_ -[+]> _ -[+]> _): +;
(_ -[-]> _ -[-]> _): -;
((_ -[+]> _) -[+]> _): +;
((_ -[-]> _) -[-]> _): -;
```

But what if the parameter has a different polarity? Such as

```rust
((_ -[+]> _) -[-]> _): ?;
((_ -[-]> _) -[+]> _): ?;
```

But what if the return has different polarity? Such as

```rust
(_ -[+]> _ -[-]> _): ?;
(_ -[-]> _ -[+]> _): ?;
```

## Polar Elimination

| Description | Conclusion              |
| ----------- | ----------------------- |
| p. (p p)    | Polarity is meaningless |
| p. (p -p)   | ?                       |

| Description    | Conclusion |
| -------------- | ---------- |
| p. x -[p]> p   | ?          |
| p. p -[p]> x   | ?          |
| p. x -[p]> -p  | ?          |
| p. p -[p]> -p  | ?          |
| p. -p -[p]> x  | ?          |
| p. -p -[p]> p  | ?          |
| p. -p -[p]> -p | ?          |

```rust
// (p -p); p. x -[p]> p
// in this system a function can never be inside of itself?

ALoop = BLoop -> +⊥;
BLoop = ALoop -> -⊥;
fix = (f: ALoop) => f ();

id = p => (A: p) => (x: A) => x;
x = id@+ ((A: -) -> A -> A) id@-;
apply = (A: -p) => (B: p) => (f: A -> B) => (x: A) => f x;


apply : (A: -) -> (B: +) -> (A -> B) -> A -> B;

kid = (A: -p) -> A -> (A -> ⊥) -> ⊥;
x = kid Int 1 ();

incr: +Int -> -Int;
x = apply (Int -> Int) Int incr +1;

Loop = (Unit -> Loop) -> ⊥;
fix = (f: Loop) => f (() => f);

Loop = (() -[-]> Loop) -[+]> -⊥;
Nat = R => (() => R) => (Nat => R) => R;

Inner = (X: p1) => (R: p2) -> (X -> R) -> R;
Nat = (X: p) -> (Inner X -> X) -> X;

zero = (X: p) => (cb: Inner X -> X) =>
  cb (R => )
```

## Opposites Polymorphism

```rust

n + = -;
n - = +;

id = @{p}(A: *@{p}) -> A -> A;
apply = @{p,r}(A: *{p}) => (B: *{r}) => (f: A -> B) => (x: A) => f x;

id = (A: +) -> A -> A;
+ = { (P: -) -[+]> + };
- = { (P: +) -[-]> * };

x = R => (n: R -[+]> ('Nat -> ))
x = (f: 'F -[-]> () -[+]> () as ('F: +)) => f f

concat: String -> String -> String;

f x = x;
f 1 === 1;

++False: ? = (A: +*) -> (B: +*) -> A -> B -> B;
+-False: ? = (A: +*) -> (B: -*) -> A -> B -> B;
-+False: ? = (A: -*) -> (B: +*) -> A -> B -> B;
--False: ? = (A: -*) -> (B: -*) -> A -> B -> B;

℘ S === S -> *;

fix = (f: (F -> ⊥) -> ⊥ as F) => f f;

U = (X: +□) -> (℘℘X -> X) -> ℘℘X;
b = (X: □) => (-f : ℘℘X -> X) => (+p : ℘X) =>
  (+x : U) => p (f (x X f));

℘ S === S -> *;
U = (X: □) -> (℘℘X -> X) -> ℘℘X;
τ: ℘℘U -> U = (t: ℘℘U) => (X: □) => (f : ℘℘X -> X) => (p : ℘X) =>
  t ((x : U) => p (f (x X f)));
σ: U -> ℘℘U = (s: U) => s U τ;
∆ = (y: U) => (p: ℘U) -> σ y p -> p (τ (σ y));
Ω: U = τ ((p: ℘U) => (x: U) -> σ x p -> p x);
```

```rust
Zero.z.s => z;
S.x.z.s => s.x;

lazy.f.() => f.();
fix.f => f.(lazy.f)
AddCBV.x.y.r => x.(r.y).(B.y.r);
B.y.r.u => AddCBV.u.(S.y).r;

end: (A: *) -> ⊥;
// p. -p -[p]> x
Loop = (() -[-p]> Loop) -> ⊥;
fix f = f f;

Kid p = (A: -p) -[p]> A -> ()

// p. -p -[p]> p
Kid p = (A: -p) -> A -> ()
Nat = (R: -p) -> R -> (Nat -[-p]> R) -> (R -[-p]> ⊥) -> ⊥;
Zero = (R: p) => (z: R) => (s: _) => (k: R -> ⊥) => k z;
// p. x -[p]> p, (p -p)
Loop = Loop -> ⊥;
fix (f: Loop) = f f;

Loop = (() -> Loop) -> ⊥;
fix (f: Loop) = f (() => f);

id = (A: p) => (x: A) => x;

Kid p = (A: -p) -[p]> A -> (A -[p]> ⊥) -> ⊥;
kid = (A: -p) =[p]> (x: A) => (k: A -[p]> ⊥) => k x;
x: -Kid -> (-Kid -[p]> ⊥) -> ⊥ = +kid -Kid;
x: (-Kid -[p]> ⊥) -> ⊥ = +kid -Kid -kid;

f: (() -[-p]> -T) -[p]> T;

Nat = (R: -p) -> R -> (Nat -[-p]> R) -> (R -[-p]> ⊥) -> ⊥;
Zero = (R: p) => (z: R) => (s: _) => (k: R -> ⊥) => k z;

f n = n 0


f: (A: *) -> (A = A, x: A) -> (A = A, x: A)

X = *;
id (A: *) (x: A) =
  B = A;
  (x: A);

id = (A: *) => (x: A) =>
  ((B: (=A)) => (x: B)) A;

x =
  1 == 2
  | Some eq -> absurd eq
  | none -> "not equal";

M: (A: *, x: A);

id: (A: *) -> A -> A;
X = id A;

((x: *) => x): ((x: *) -> *);
(((x: *) => x) String): ((x: *) -> *);
((s: ((x: *) => x) String) => s): ((s: String) -> String);

(x: (x: *) => x) => x;

((x: *) => x): (x: *) -> *;
(x: (x: *) => x) -> (x: *) -> *;

(((F: * -> *) => (x: F String) => x) ((x: *) => x));
((F: * -> *) -> F String -> F String);

(A = Int, x: A): (A = Int, x: Int);

(A = Int, x: Int): (A: *, x: A);

id = (A: *) => (x: A) => A;

expected :: (A: =Int) -> Int -> A;
received :: (A: *) -> A -> A;

received :: =Int;
expected :: *; // =Int, []

expected :: Int -> Int;
received :: Int -> Int;

expected :: (A: *, x: A);
received :: (B: *, x: B);

((x: *) => x);

X: =Int = Int;
Y: =Int = X;
x = id Int;

(A = Int, x: A);

(x: Int -> Int) => (a: x) => a;

(X: *) => X =>

((X: *) => X) Int;
(M: (X: *) -> *) => M Int;

(x)


((M: (A: *, A)) => M): ((M: (A: *, A)) -> (A = Fst M, Snd M));

  (A, x) = M;
  x;

add = (x, y) => x + y;
add (x, y) = x + y;

Ex = (R: *) => (F: (A: *) -> (x: A) -> R) => F Int 1;

Ex: (R: *) -> (F: (A: *) -> (x: A) -> R) -> R;

((X: *) -> (A: X) => A): (A: *) -> *;
((A: *) => A): (A: *) -> *;

(A = Int, x = 1): (A = Int, x: A)


x : (x: Int) => Int;


equal = (a, b) => a = b;
id = x =>
  y = x;
  print y;
z = 2;

(
  print 1;
  x => x = 1;
  print x;
)

{
  x = 1;
}

let x =
  T_arrow
    {
      var = { Var.id = 0; name = "A" };
      param = T_type;
      return =
        T_arrow
          {
            var = { Var.id = 1; name = "x" };
            param = T_var { var = { Var.id = 0; name = "A" }; type_ = T_type };
            return = T_var { var = { Var.id = 0; name = "A" }; type_ = T_type };
          };
    }

let y =
  T_arrow
    {
      var = { Var.id = 2; name = "A" };
      param = T_type;
      return =
        T_arrow
          {
            var = { Var.id = 3; name = "x" };
            param = T_var { var = { Var.id = 2; name = "A" }; type_ = T_type };
            return =
              T_var
                {
                  var = { Var.id = 3; name = "x" };
                  type_ =
                    T_var { var = { Var.id = 2; name = "A" }; type_ = T_type };
                };
          };
    }

print "Hello World";


True = (A: *, B: *) => A;
True (A: *) (B: *) = A;

Int: {
  Int: *;
  add: Int -> Int -> Int;
} = magic ();

id = (A: *) => (x: A) => x;
x = (x: Int) => x;

(*): (Int -> Int -> Int) &
(*)(1, 2)
(Int: *)

id = A. (x: A) => x;
Id (A: *) (x: A) = x;

id = x => x;
id x = x;

(Value : Value)
(& : *)

(x = 1; x)
(((x: =1) => x) 1);
(Snd (x = 1, x));

(T: *, Eq: T == Int -> Int)
(T = Int -> Int, Eq = Refl);

M = {
  T = Int;
  x = (1: T);
};

x: (T: *, x: T) = (T = Int, x = (1: T));

pair = (A: *) => (B: (T: *) -> *) => (x: B A) => (T = A, x = x);
pair_int_id_one = (T = Int, x = x)

x = (T: *, x: T) => x;

build_m = (T: *) => (X: (T: *) -> *) => (x: X T) => (T = T, x = x);
build_m_int = (X: (T: *) -> *) => (x: X Int) => (T = Int, x = x);
build_m_int_id =  (x: Int) => (T = Int, x = );

pair = a => b => k => k a b;
fst = pair => pair (a => b => a);
snd = pair => pair (a => b => b);

pair = (a: 'A) => (b: 'B 'A) => k => k a b;


empty = 0
[empty] = 0 + 1;
[[empty]] = 0 + 1 + 1;
[[[empty]]] = 0 + 1 + 1 + 1;


Incr = Int -> Int;
Pair = (A: *, A);

incr = ((x : Int) => x);
pair = (A = Int, 1 : A);

Either =
  | Left Int
  | Right String;

rec%List A =
  | Nil
  | Cons (A, List A);

rec%map f =
  | Nil => Nil
  | Cons (el, tl) => Cons (f el, map f tl);


either = Left 1;
(either | Left x => x | Right s => s);

(x : String) => x;
(x : Int) => x;
((A = Int, 1 : A): (A: *, A));
((A = Int, 1 : Int): (A: *, Int));

P = (A = Int, 1 : A);
(A, x) = P;

(T: * = Int; (1: T))
(((T: *) => (1: T)) Int);

x = 1
  : Nat;

(x = 1) => x;

x: 1 = 1;
x + 1;

x : (Value : Type);

id: (x = ⊥ : Int) = (x = ⊥ : Int) => x;

f = (y = ⊥ : Int) => (x : (2, y)) => x;
z = (x = ⊥ : Int) => f 2;
z : 1 : Int = 1;
x = id z

f = (x : Int) => x;
f = (x = 1) => x;

(x = 1; x);
(((x = 1) => x) 1);

(x : Int = 1; x);
(((x : Int) => x) 1)

(x := 1) = 1;
x

(a := x, b := y) = (a = 1, b = 2);

x = 1;
x

Id: {
  initial : Id;
  next : Id -> Id;
} = {

};
(A: * = Int, (Int))

x : Int = 1;

(x = 1) => x;

id = (x: T) => x;
x : Car = id car;

(A: *) => (x: A) => (x: A);

choose {A} {B} (bool: Bool) (a: A) (b: B) =
  bool | true -> a | false -> b;

choose : {A} -> {B} -> Bool -> A -> B -> K;

x = choose true 1 2;

x = 1;
x := 1;

(T = Int, 1 : T)

Color = Red | Green | Blue;

partial_to_int = (color : ((Red | Green) : Color)) =>
  color | Red => 0 | Green => 1;

f = (color: Color) =>
  color
  | (Red | Green) as color => partial_to_int color
  | Blue => -1

T: * = Int;
T = Int;

(T = Int, 1 : T)

Id = {}

id = (A: Prop) => (x: A) => x;
id_id = id ((A: Prop) -> A -> A)

id = {A} => (x: A) => x;
id = x => x;


id = (A: Type) => (x: A) => x;
Id = (A: Type) => A;


(T = Int, 1 : T);

//

(x: t) === (((x: t) => x) x)
(((x: t) => x) x) === x

(x: t = e1; e2) === (((x: t) => e2) e1);

M = {
  T = Int;
  x = 1;
};

x: M.T = 1;

Option {A} =
  | Some A
  | None;

User = {
  id : Nat;
  name : String;
};
eduardo : User = {
  id = 1;
  name = "Eduardo";
};

(((A: *) -> A -> A) : *);
(((A: *) => (x: A) => x) : (A: *) -> A -> A);


((pred : (A: *) -> (x: A) -> (y: A) -> A) => (x: pred Type Int Id) => x)
  ((A: Type) => (x: A) => (y: A) => x) one

Bool = (A: Type) -> A -> A -> A;
true: Bool = A => x => y => x;
false: Bool = A => x => y => y;


f = (pred : Bool) => (x: pred Type Bool Int) => x;

(f: Type -> Type) => (x: id Int) => (y: id Int) =

((X: *) -> X -> X) Int;

M : {
  id : Nat;
  name : String;
} = {
  id = 0;
  name = "Eduardo";
};
M : {
  id : Nat;
} = M;

id = (A : Type) => (x : A) => x;

g = (m : { id : Nat; name : String; }) => f { ...; name : String } g;

g = (m : { id : Nat; name : String; }) =>
  ((m : { id : Nat; name : String; }) => m) g;

f = (A : Row) => (x : Red | Green | Blue | #A ) => x;
g = (x : Red | Green) => f x;

f =
  (company : Bool) =>
  (T : company | false => { ...; cpf : CPF } | true => { ...; cnpj: CNPJ }) =>
  (person : { name : String; ...T }) => person;

f : (A: Row) -> (x : { id : Nat; ...A }) -> A = magic ();
g = (m : { id : Nat;  name : String }) => m;


fix%List A =
  | Nil
  | Cons (A, List A);
List A =
  | Nil
  | Cons (A, @);

rec%Person = {
  id : Nat;
  name : String;
  children : List Person;
};


Person = (id : Nat, (name : String, children : List @))

Bool = (A: Type) => (B: Type) => (C: Type) =>
  (K: Type) -> (A === C -> K) -> (B === C -> K) -> K;
true = (A: Type) => (B: Type) => (C: Type) =>
  (K: Type) => (x: A === C -> K) => (y: B === C -> K) => x refl;

Option = (A: Type) => (none: Bool, none | () => Unit | () => A);
x: Option Int = magic ();
a =
  (none, value) = x;
  none | () => Unit;

List A =
  | Nil
  | Cons (A, @);

T = Type;
Type = 1;

id = ((A: Type) => (x: A) => x) Type;

(A : Type) => A;

((A : Type) => (x : A) => (y : A) => x) Type ((A : Type) -> A) (A : Type, A)

get 1 === get 1
get 1 buf === get 1 buf

A -> B
A : Type  B : Type  E : Row
---------------------------
        A -[E]> B

((m : A -[E0]> B ! Empty) (n : A ! E1)): Either E0 E1

(True | False | ...A)
(... | True | False)

read : Unit -[ Read ]> String;

T = | #A | #B;
x = #A


S = (T : Type, T);

x = S Int 1;

T1 : Type = | A | B | C);
R : Row = ... | B | C;
T2 : Type = | A | ...R;

R : Row = ... | A;
T : Type = ...R | ...R;
T : Type = | A#0 | A#1;
handler :

  (B: Row) ->
  (handler: Unit -[B]> String) ->
  (A: Row) ->
  (K: Type) ->
  (f : Unit -[ Read | ...A ]> K ) -[...A | ...B]> K

Option = | Int | String

Option.Int


get : Nat -> Unit -<Read | Write> String;
f () : Nat =
  string = get 1 ();
  length string;

get : Nat -> Unit ->
  (K: Type) -> (String -> K) -> String -> K;
f () =
  get 1 () Nat (string => length string)

get 1 === get 1;
get 1 () !== get 1 ();


read : Unit -<Read | Write> String;
read : Unit -[Read | Write]> String;

read : Unit -<Read><Write> String;
read : Unit -[Read][Write]> String;

f : (A: Row) -> (f: Red | ...A) -> Red | ...A;
x = f (...| Red);
x: (f: Red | Red) -> Red | Red);

| Red | Red === | Red

M: {
  X : Type;
  X : String;
  y : X\1;
} = {
  X = Int;
  X = "a";
  y : X = 1;
};


Effect Return =
    | Read : Return = String;


E = (P : Type) => (A : Type) -> A;

f : A -[]> B;


A : Type   B : Type
-------------------
      A -> B

A : Type   B : Type  E : (t: Type) -> Type
------------------------------------------
            A -[E]> B

M : A -[E1]> B ! E2    N : A ! E3
---------------------------------
  M N : B ! A => E1 A | E2 A | E3

A : Type   B : Type  E : (t: Type) -> Type
------------------------------------------
          (x : A) -[E]> B : ⊥

        x : A |- e : B ! E
-------------------------------------
(x : A) => e : (x : A) -[E]> B ! ⊥

M : A -[E]> B ! |    N : A ! |
--------------------------------
  M N : B !



M : A -> B ! E1    N : A ! E2
--------------------------------
   M N : B ! A => E1 A | E2 A

M : A -[E1]> B ! E2    N : A ! E3
---------------------------------
  M N : B ! A => E1 A | E2 A | E3

raise : (E : Type -> Type) -> (R : Type) -> (eff : E R) -> R;
catch : (E : Type -> Type) ->
  (handler: (R : Type) -> (eff : E R) -> R) ->
  (B : Type) -> (f : Unit -[E]> B) -> B

clear : (A : Type) -> (B : Type) -> (f : A -[(R: Type) => (A : *) -> A]> B) -> B =
  catch ((R: Type) => (A : *) -> A) ((R : Type) => (Absurd : E R) => Absurd R)

Ty (A : Type) =
  | Int: Ty Int;

f () =
  x: Int = raise Ty Int Ty.Int;
  x + 1;


x = catch Ty ((R: Type) => (eff : Ty R) => eff | Int => 1) Int f;


A -[((R : Type) => | Read: R = String)]> B
id : Int -> Int;
id : (A : Type) -> A -> A;
((L : Level) -> (A : Type L) -> A -> A);

NatL = (L : Level) -> (A : Type L) -> A -> (A -> A) -> A


(x = 1;
 y;)

id === ∀T. λx : T. x;

|- m : T ! x


```

## Dependent Types

```rust
Bool = (A : Type) -> A -> A -> A;
true : Bool = A => x => y => x;

Never = (A : Type) -> A;

Only_when_true = (A : Type) => (pred : Bool) => pred Type A Never;
f = (pred : Bool) => (x : Only_when_true Int pred) => x

create = (n : Int & n >= 0) => n;

n >= 0
| true = p => create (n & p)
| false p =>
map = {A}  => (f : )

n >= 0 | (true, eq : n >= 0 === true) => create n
(x : Bool, x === true) = true;
x = create n;
x = f false

f : (x : Int) -> ((A : Type) => (x : A) => (y : A) => x) Type Int ((A : Type) -> (x : A) -> A)

(tagged String
  (case "user" { name : String })
  (case "company" { name : String })
  closed) : (user : ({ name : String }) -> )


```

## Macabeus

```rust
incr : (x : Int) -> Int = (x : Int) => x + 1;
id = (A : Type) => (x : A) => x;
id = ((x : Int) => x) 1;

Bool = (A : Type) -> A -> A -> A;
true = (A : Type) => (x : A) => (y : A) => x;
false = (A : Type) => (x : A) => (y : A) => y;

add = a => b => a + b;
incr = (x : String) => add 1 x;

When = (pred : Bool) => (A : Type) => pred Type Int Never;
f = (pred : Bool) => (x : debug (pred Type Unit Never)) => x;
x = f true 1;

Id = (A : Type) => A;
x = f false

(x : debug String) => x
IO A =
  | Read : A === String;
read : (file : String) -[| Read]> String = magic ();
print : (msg : String) -[| Print]> Unit = magic ();

world_if_hello : (x : String) -[| Error]> String = (x : String) =>
  x
  | "Hello" => "Hello World"
  | _ => raise (Error "Not Hello");

(x = read "tuturu.txt";
 z = world_if_hello x;
 print z)
  | [%effect Read file] => "Hello"
  | [%effect Error error] => "Error"
  | [%effect Print x] => ()

T : Type;
E : Type -> Type;
x : T ! E

add a b = a + b;
mul a b = a * b;

check : {A} -> (x : A) -> (y : A | y === x) -> Unit;

() = check (add 1 2) 3;
x = (a : Int) => check (mul a 0) 0;
x = (a : Int) => (b : Int) => check (mul a b) (mul b a);
```

## Effects

```rust

---------------------
Type L : Type (L + 1)

A : K   B : K
------------------
((x : A) -> B) : K


A : Level   B : A
---------------------
(L : A) -> B : Type (max 0 L)

((A : Type 0) -> A -> A) : Type 1

Bool = (A : Type) -> A -> A -> A;
true = (A : Type) => (x : A) => (y : A) => x;
false = (A : Type) => (x : A) => (y : A) => y;

Id = (A : Type) -> A -> A;
id : Id = (A : Type) => (x : A) => x;

A : Type   B : Type  E : Type -> Type
-------------------------------------
            A -[E]> B

(E : Effect) => (f : Int -[E]> Int) => f;

e ! | === e ! |

get () ! === get ()

x = eq (get ()) (get ());

A -[ | Read: Int ]-> B

(m n : Option Int ! IO)
| None =>
! IO => (handler : Option Int)

Type L : Type (L + 1)

Id = {A} -> A -> A;
id : Id = {A} => (x : A) => x;
x = id id;
```

## Predicativity

```rust

U : Type = (A : Type, (A = A, A -> ()) -> ());
V = (A = U, A -> ());

V <= U;
(A = U, A -> ()) <= U;
(A = U, A -> ()) <= U;

U = (L : Level) : Type (L + 1) => (A : Type L, (A = A, A -> ()) -> ());
V = (L : Level) : Type (L + 2) => (A = U L, A -> ());

V L <= U (L + 1);
(A = U L, A -> ()) <= U (L + 1);
(A = U L, A -> ()) <= U L;

M = {
  T : Type;
  x : T;
};

((L : Level) -> Type L : Type (L + 1)) : Type 1


S : Type = (T : Type, Unit);
M : S = (T = S, ());

Type 0 : Type 1
(X : Type 0) => Type 0;

Int : Type 0
((x : Int) -> Int) : Type 0

S : (L : Level) -E> Type (L + 1) : Type 1 = (T : Type, Unit);
M : S = (T = S, ());

S : (L : Level) -E> Type (L + 1) = (L : Level) => (T : Type L, Unit);
M : (L : Level) -E> S (L + 1) = (L : Level) => (T = S L, ());


S : (L : Level) -> Type L = (L : Level) => (T : Type L, Unit);
M : (L : Level) -> S L = (L : Level) => (T = S L, ());

S : Type 1 = (L : Level) -> (T : Type L, Unit);

A : Type 1 = (L : Level) -> Type L;
S : Type 1 = (T : (L : Level) -> Type L, Unit)

M = (T = (L : Level) -> S L, ());

x = (L : Level) : Ex (L + 1) => (T : (LT : Level) -> Type LT = Ex L, T)


loop : Unit ->! Unit;
soundness + strong normalizing = consistency


map : ('A -> 'B) -> List 'A -> List 'B;

id : (T : Type -> Type) -> (A : Type) -> A -> T A;

call_id (id : {A} -> A -> A) =
  x = id "a";
  y = id 1;
  (x, y);

Id = (A : Type) => A;
f = (T1 : Type) => (T2 : Type -> Type) => (x : T2 T1) => x;
x : Int = f Int Id 1;

Id = (L : Level) => (A : Type L) => Type L;
f =
  (L : Level) =>
  (T1 : Type L) =>
  (T2 : (L : Type) -> Type L -> Type L) =>
  (x : T2 L T1) => x

Id : Type 1 = Unit A;
Const = (L : Level) => (_ : Type L) => Type L;

f = (L : Level) => (x : Type L) => x;

f =
  (L : Level) =>
  (F : (L : Level) -> (K : Type L) -> Type L -> ((T : Type L) -> K) -> K) =>
  F ((T : Type L) => (x : T) => x)
  (x : T L Int) => x;

Id : Type 1 = (L : Level) -> (A : Type L) -> A -> A;

Level : Type 1
Type : (L : Level) -> Type (L + 1)


id  = (A : Type 0) => (x : A) => x;

Prop : Type 0
Set : Type 0
Level : Type 0

Level  : Type 0
Type 0 : Type 1

Empty = Type 0


Int : Type 1;
String : Type 2;

F = (L : Level) => (A : Type L) => (B : Type L) => (x : A) -> B;

T : Type 1 = (L : Level : Type 1) -> Int;

Type : (L : Level) -> Type (L + 1)

S : Type 1 = (L : Level, Type L);

X : Type 1 = Level;
Id : Type 1 = (L : Level : Type 1) -> (A : Type L) -> A -> A;
id = (L : Level) => (A : Type L) => (x : A) => x;
x = id 1

Id : Type 1 = (L : Level) => L;

Id : Type 1 = (A : Type 0) -> A -> A;
Id : Type 2 = (A : Type 1) -> A -> A;
id = (A : Type 0) =>

Type : (L : Level) -> Type (L + 1);

x : R = (_ : P -> R) (_ : P);
x : Type 2 = Type 1


    A : Type A_L      B : Type B_L
-----------------------------------------
(x : A) -> B : Type (max A_L B_L[x := 0])

max (a + 1, a) === a + 1
max (a, b) a === max a b

x : (L : Level) -> Type (L + 1) : Type 1;
x = (L : Level) => Type L;

U = (L : Level) : Type (L + 1) => (A : Type L, (A = A, A -> ()) -> ());
V = (L : Level) : Type (L + 2) => (A = U L, A -> ());

V L <= U (L + 1);
(A = U L, A -> ()) <= U (L + 1);
(A = U L, A -> ()) <= (A : Type (L + 1), (A = A, A -> ()) -> ());
(A = U L, A -> ()) <= (A = U L, (A = A, A -> ()) -> ());


SK : (L : Level) -> Type (L + 1) = (L : Level) => (A : Type L, Unit);
S : Type 1 = (L : Level) -> SK L;
M = (L : Level) => (A = S, ());

M = (L : Level) : Type (L + 2) => (A = (A : Type L, A), ());

M : Type 2 = (A : Type 1 = (A : Type 0, A), ());

F : Type L = (A : _ : Type L) -> (_ : Type L)
S : Type L = (A : _ : Type L, _ : Type L)

F : Type = (K : Type) -> (f : (A : Type) -> A -> K) -> K;

f = (K : Type) => (f : (A : Type) -> A -> K) => f ((A : Type 0) -> (x : A) -> A) ((A : Type 0) => (x : A) => x);

F : Type 1 = (L1 : Level) -> (K : Type L1) -> (f : (L2 : Level) -> (A : Type L2) -> A -> K) -> K;
f = (L1 : Level) => (K : Type L1) => (f : (L2 : Level) -> (A : Type L2) -> A -> K) =>
  f (lsucc lzero) ((A : Type 0) -> (x : A) -> A) ((A : Type 0) => (x : A) => x)

x = (L : Level) =>

f = (K : Type) => (f : (A : Type) -> A -> K) => K;



id : (A : Type) -> A -> A;
id (A : Type) (x : A) = x;

x = id Int 1;

id : {A : Type} -> A -> A;
id {A : Type} (x : A) = x;

id : 'A -> 'A;
id x = x;

x = id 1;

incr (x : Int) = x + 1;

id : forall A. A -> A;
id : forall A where A extends Object;


M : {
  T = Int;
  x : Int;
} = {
  T = Int;
  x = 1;
};
M1 : {
  T : Type;
  x : T;
} = M;

id : (A :> Int) -> A -> A;


uip : forall x y (a b : x = y), a = b

heteregenous : forall x1 y1 x2 y2 (a : x1 = y1) (b : x2 = y2), a = b;


ext : forall A B (f g : A -> B) -> (forall x, f x = g x) -> f = g = magic id;

S : Type = (T : Type, Unit);
M1 : S = (T = S, ());
M2 : S = (T = (typeof M1), ());

S : (L : Level) -> Type (L + 1) = (L : Level) => (T : Type L, Unit);
M1 : (L : Level) -> S (L + 1) : Type 2 = (L : Level) => (T : Type (L + 1) = S L, ());
M2 : (T : Type 2, Unit) : Type 3 = (T = (typeof M1), ());


M : (L : Level) -> ((T : Type L, Unit)) = (L : Level) => (T : Type (L + 1) = S L, ());


Int : Type
String : Type

Type : Type

id = (A : Type) => (x : A) => x;

Bool = (A : Type) -> A -> A -> A;
true = (A : Type) => (x : A) => (y : A) => x;
false = (A : Type) => (x : A) => (y : A) => y;

Never = (A : Type) -> A;

f = (pred : Bool) => (x : pred Type Int Never) => x;
a = f true;
a = (x : Int) => x;
b = f false;
b = (x : Never) => x;

Type 0 : Type 1
Type 1 : Type 2

Type : Kind

Id : Type = (A : Type) -> A -> A;



A : _   B : K
--------------------
  (x : A) -> B : K

A : K   B : K
--------------------
(x : A) -> B : K[x := 0]


Type 0 : Type 1
Type L : Type (L + 1)

Id : Type 1 = (A : Type 0) -> A -> A;
id0 = (A : Type 0) => (x : A) => x;
id1 = (A : Type 1) => (x : A) => x;
x = id1 ((A : Type 0) -> A -> A) id0;

Id : Type 1 = (L : Level) -> (A : Type L) -> A -> A;
id = (L : Level) => (A : Type L) => (x : A) => x;

id = id 1 Id id;

z = (T = Int, x : T = 1);

id = (K : Kind) => (A : K) => (x : A) => x;

x = id ((A : Type) -> A -> A);


S : Type = (T : Type, Unit);
M : S = (T = S, ());

S2 = (T : S, Unit);
M : S2 = (T = S2, ());

S : (L : Level) -> Type (L + 1) = (L : Level) => (T : Type L, Unit);
M : (L : Level) -> S (L + 1) = (L : Level) => (T = S L, ());

S2 : (L : Level) -> Type (L + 2) = (L : Level) => (T : S L, Unit);
M2 : (L : Level) -> S2 (L + 1) = (L : Level) => (T = S2 L, ());

Bool = (A : Type) -> A -> A -> A;
true = (A : Type) => (x : A) => (y : A) => x;
false = (A : Type) => (x : A) => (y : A) => y;

max A_L (max B_L B_L) = max A_L B_L
Never = (A : Type) -> A;

Only_when = (pred : Bool) => (A : Type) => pred | true => A | false => Never;
f = (pred : Bool) => (x : Only_when pred Int) => x;


id = (L : Level) => (A : Type L) => (x : A) => x;

```

## Erasable Universe Polymorphic Predicative System F

Hypothesis, System U is equivalent to this system, but where levels can be recursive.

```rust

Var = String;
Level = Nat;

Kind =
  | KType (level : Level)
  | KLevel;
Type =
  | TVar (x : Var)
  | TArrow (param : Type, return : Type)
  | TForall (var : Var, param : Kind, return : Type);
rec%Expr =
  | EVar (x : Var)
  // term abstraction
  | ETAbs (var : Var, param : Type, body : Term)
  | ETApp (lambda : Term, arg : Term);
  // type abstraction
  | EKAbs (var : Var, param : Kind, body : Term)
  | EKApp (lambda : Term, arg : Type);
```

## Effects

```rust
read : String -[IO]> String;
```

## Inference

```rust
id : {L} -> {A : Type L} -> A -> A : Type 1;

id1 : _A -> _A : Type _L1 && _L1 : Level && _A : Type _L1
id2 : _B -> _B : Type _L2 && _L2 : Level && _B : Type _L2

_A = _B -> _B

(_B -> _B) -> _B -> _B
id = id id;

map : (Int -> Int) -> ();

id2 : _B -> _B : Type _L2 && _L2 : Level && _B : Type _L2

x = map id;
id >= Int -> Int

Id = {L} -> {A : Type L} -> A -> A
f : {A : Type | A :> Id} -> A -> A;

incr : Int -> Int;
(_A -> _A) <: Int -> Int;
x = f incr;


```

## Induction

```rust
Rec = (G : Type -> Type) => (X : Type) -> (G X -> X) -> X;


Unit = (A : Type) -> A -> A;
unit : Unit = A => x => x;
Elim = (P : Unit -> Type) => (v : P unit) => (U : Unit) => P U
Elim = (P : Unit -> Type) -> P unit -> (U : Unit) -> P U;


f = (p : (tag : Unit, tag Type String)) => (
  (tag, payload) = p;
  tag String payload;
);


Tag  =
  (H : Type) ->
  (A : Type) ->
  (B : Type) ->
  (R : Type) ->
  ((Left : H === A) -> R) ->
  ((Right : H === B) -> R) ->
  R;

Bool = (A : )
tagged =
Status = tagged ((tag : Bool) => tag Type Int String);

to_string = (status : Status) => (
  (tag, content) = status;
  tag String
    (_h_eq_unit => "valid")
    (h_eq_string => subst h_eq_string content)
);


Bool = (R : Type) ->
bool | true  =>





Nat = Rec ((X : Type) => (R : Type) -> R -> (X -> R) -> R);

Status = (valid : Bool, valid Type Unit String);

f = status = | Valid => "valid" | Invalid (reason) => reason;

f = (tag : Bool) => (value : tag Type String String) => (
  tag
    ("valid" : tag Type String String)
    (value : tag Type String String)
);

Nat = (L : Level) Rec (X => (R : Type L) -> )


Bool = (A : Type) -> A -> A -> A;
Status = (tag : Bool, pred Type Unit String);

Bool =
  (H : Type) ->
  (T : Type) ->
  (F : Type) ->
  (R : Type) ->
  ((H_eq_T : H === T) -> R) ->
  ((H_eq_F : H === F) -> R) ->
  R;

Tag =
  (H : Type) =>
  (T : Type) =>
  (F : Type) =>
  (R : Type) ->
  ((H_eq_T : H === T) -> R) ->
  ((H_eq_F : H === F) -> R) ->
  R;

Either = (A : Type) => (B : Type) =>
  (H : Type, (tag : Tag H A B, H));

Status = (tag : Bool, tag Type Int String)

Either A B = {R : Type} -> !(Left : A -> R, Right : B -> R ) -> R;

Option A =
  | None
  | Some A;

Option A =
  tagged [
    case #None Unit;
    case #Some A;
  ];

to_string = (string_or_int : Either String Int) =>
  string_or_int
  | Left = s => s;
  | Right = n => Int.to_string n;

to_string = (string_or_int : Either String Int) =>
  [%match string_or_int
  | (#Left, (#Left, s)) => s
  | (#Right, (#Right, n)) => Int.to_string n]
to_string = (string_or_int : Either String Int) =>
  string_or_int
  | Left (Left _) => s
  | Right (Right _) => Int.to_string n]
to_string = (string_or_int : Either String Int) =>
  string_or_int
  | (#Left, s) => s
  | (#Right, n) => Int.to_string n;

[%match (a, b) with
| (true, b) => b;
| (false, _) => false;
];

(Bool [@variant]) = (A : Type) -> A -> A -> A;
true : Bool = A => x => y => x;
false : Bool = A => x => y => y;

(A : Type & P A);
(A : Type & A === Int);
(A : Nat & Gte A 0);

Level : Type 1;
Type 0 : Type 1;
Type L : Type (L + 1);


Pair A B =
  (R : Type) -> ((tag : A) -> (content : B tag) -> R) -> R

f = (p : Pair Bool (tag => tag Type Int String)) =>
  p
    (a : Type &
      p Type (tag => _ => tag Type Int String)
    (tag => content => content):

f = (p : (A : Bool, tag Type Int String)) => (
  (tag, content) = p;
  content;
):
F = (p : (A : Bool, tag Type Int String)) -> (
  (tag, content) = p;
  tag Type Int String
);

Tag =
  (H : Type) =>
  (T : Type) =>
  (F : Type) =>
  (R : Type) ->
  ((H_eq_T : H === T) -> R) ->
  ((H_eq_F : H === F) -> R) ->
  R;

Either = (A : Type) => (B : Type) =>
  (H : Type, (tag : Tag H A B, H));
to_string = (a : Either String String) => (
  (H, (tag, payload)) = tag;
  tag String
    (H_eq_T => subst H_eq_T _ payload)
    (H_eq_F => subst H_eq_F _ payload)
);
```

## Intersection

```rust

--------------
(x : A) & B

S : Type & Equal S {
  a : String;
} = {
  a : String;
};
Nat : (Nat : Type & {
  zero : Nat;
}) = _;

x = id Nat;


Type : Type
-> : Function
{} : Record

f = (A : Type) => (B : Type) =>   & B;

expected : A;
received : A & B;

(==) :
  ((==) : {A : Type} -> (l : A) -> (r : A) -> Type) &
  ({A : Ord} -> (l : A) -> (r : A) -> Option (l == r));

(>=) :
  ((l : Nat) -> (r : Nat) -> Type) &
  ((l : Nat) -> (r : Nat) -> Option (l >= r));

length : (s : String) -> Nat;

create = (len : Nat & len >= 1) => ();
x = create 4096;

g = x =>
  [%match x >= 1
  | Some x_gte_one => f (x & x_gte_one)
  | None => ()
  ]

Option : (Option : _ & {
  some : {A} -> Option A;
});

Nat : {
  zero : Nat;
};

Nat : (Nat : _ & {
  zero : Nat;
})
Nat : (Nat : _ & {
  zero : Nat;
}) = _;

x : Nat = Nat.zero;

(x => x : {A} -> A -> A)

(1, 2, 3)
(z = 1, x = 2, z = 3);

f : !(x : Int) => Int;

x =
  x = 1;
  f (x = x);

(a : Int) -> Int
(a : Int) -> Int
((a : Int)) -> Int
((x : A & y : B), )

z = (x : Int, y : Int)

Id = (A : Type) => A;
id : Type & (A : Type) => Type;

Point : Type & (Type, Type) = (Int, Int);
x = (Int, Int)

f : Type & {A} => Type & {T} => (x : {A : T}) => T;


Id #= {A} => (x : A) => A;


Point : Type & (Type, Type) = (Int, Int);
T = {
  T = Int;
};

{R} -> {S : R } -> {A : S} -> (l : A) -> (r : A) -> R



((x : Int) => x : (x : Int) => Int);
(lambda arg)

(x : Int, y : Int);
(x = 0, y = 0);
((x, y) = p; z)

(Int, Int)
(0, 0) : (Int, Int)


x : Int & String;
z : String = x;

Nat : (Nat : Type & {
  zero : Nat;
}) = _;

zero : Nat = Nat.zero;

Point : Type & (Type, Type) = (Int, Int);

Point = (Int, Int);


Point : Type = (x : Int, y : Int);

f = (a : Int, b : Int) => a + b;

x = f (a = 1, b = 2);
x = f (1, 2);

(==) :
  {A} -> (a : A) -> (b : A) -> Type &
  {A : Eq} -> (a : A) -> (b : A) -> Option (a == b));


(>=) :
  {A} -> (a : A) -> (b : A) -> Type &
  {A : Ord} -> (a : A) -> (b : A) -> Option (a >= b))

f = (a : Nat & a >= 1) => ()

Nat : {
  zero : Nat;
}
l : Type & Option (1 == 2) = 1 == 2;

(==) : {A : Eq} -> (a : A) -> (b : A) -> Bool;

f = (a : Nat & a == 1) => ()



z = (x : Nat) =>
  x == 1
  | Some x_eq_1 -> f (x & x_eq_1)
  | None -> raise "x is not 1";

Nat : {
  zero : Nat;
} = _;

zero : Nat = Nat.zero;

(-) :
  (x : Nat) -> Int &
  (a : Int) -> (b : Int) -> Int &
  ();

expected :: Arrow Nat Int;
received :: Arrow Int Nat;`

(==) :
  {A} -> (a : A) -> (b : A) -> Type &
  {A : Eq} -> (a : A) -> (b : A) -> ?(a == b));

f = (a : Int | String) => a;
f = (a : Type & Either (A == Int) (A == String)) => a;

(==) #: {A} -> (a : A) -> (b : A) -> Type;
(==) :: {A : Eq} -> (a : A) -> (b : A) -> ?(a == b);


Nat : {
  zero : Type;
} = _;

T #= Int;
x := 1;


a : Int & String = (1 & "a");
a : Int | String = 1;

f = (a : A) => a;

x =
  tagged String
    (case );

Option =
  | Some {A} (value : A) : Option A
  | None {A} : Option A;
Option : (Option : Type -> Type) & {
  Some : {A} -> (value : A) -> Option A;
  None : {A} -> Option A;
};

Nat : (Nat : Type) & { zero : Nat } = _;
zero : Nat = Nat.zero;


some_nat : Option Nat = Option.Some Nat.zero;

Eq = (Eq : Type) & {
  equal : (a : Eq) -> (b : Eq) -> Bool
};

(==) :
  {A} -> (l : A) -> (r : A) -> Type &
  {A : Eq} -> (l : A) -> (r : A) -> Option (l == r);

x : Type _&_ Option (l == r) = a == 1;
f = (a : Nat) => (a_eq_1 : a == 1) => ();

zero : Int = Int.zero;

id : (x : Int) -> Int = (x : Int) => x;
id = (A : Type) => (x : A) => x;

g = (a : Nat) =>
  a == 1
  | Some a_eq_1 -> f a a_eq_1
  | None -> raise Error;

g = (a & a == 1) {

};
f = a =>
  a == 1
  | a_eq_1?
  | () =>

Option {A} =
  | Some (value : A)
  | None;
Option : {A : Type} -> (Option : Type) & {
  Some : (value : A) -> Option;
  None : Option;
} = _;
Option : (Option : Type -> Type) & {
  Some : {A : Type} -> (value : A) -> Option A;
  None : {A : Type} -> Option A;
} = _;
```

## Ad Hoc Polymorphism

Overloading for propositions and value.

```rust
// overloaded ==
f = (a : Nat) (a_eq_1 : a == 1) => a;
() =
  a == 1
  | Some a_eq_1 => f a a_eq_1
  | None => raise Error

// overloaded module
Nat : {
  zero : Nat;
} = _;
zero : Nat = Nat.zero;
```

### Namespacing

This creates two namespaces, this allows to define things to be used both as a proposition and a value.

```rust
(==) #: {A} -> (l : A) -> (r : A) -> Type;
(==) :: {A : Ord} -> (l : A) -> (r : A) -> Option (l == r);

Nat #: Type;
Nat :: { zero : Nat };
```

This leads to a problem with dependent types where you may want to reference the value on the type level, so new operators are needed, but it works nicely for many cases.

While ugly, it is also quite predictable. And make it closer to a traditional ML.

### Intersection Types

This uses of subtyping to allow two values to be packed together, it acts like a pair where the unpacking may happens implicitly.

```rust
(==) :
  ((==) : {A} -> (l : A) -> (r : A) -> Type) &
  {A : Ord} -> (l : A) -> (r : A) -> Option (l == r);

Nat : (Nat : Type) & { zero : Nat };
```

This leads to problems with decidability and also with function application, statements such as `1 == 2` can be interpreted as two different functions being called, this can be limited and detected in an ad hoc manner.

This is quite cute when being used, we keep the notion of a single namespace and it can be extended to things such as constrained types `(a : Nat & a == 1) => a`, it acts as a dual of pairs in the presence of subtyping.

## Substructural Types

```rust
Never = (A : Type) -> A;
Id = (A : Type) -> (x : A) -> A;

Id = (A : Type) ->
  Type.clone A (A1 => A2 => (x : A1) -> A2);

x = Id

Type : Type;

id = (L : Level) => (A : Type L) => (x : A) => x;

create = (A : Type) -> (x : A) -> A;
create = (x : Int : Resource) => (x_eq_0 : Eq x 0) => x;

create = (x : Rc Int) =>
  (x1, x2) = clone x;
  (x_eq_0 : Eq x1 0) => x2;


f = (x : Int) =>
  eq x 0
  | Some (x, x_eq_0) -> create x x_eq_0
  | None -> raise Error

(x : Rc Int) =>
  (x1, x2) = clone x;
  x1 + x2;

(x : )
```

## Bounded Quantification

```rust
Not T = (A <: T) -> A;

Theta = (A <: Top) -> Not ((B <: A) -> Not B);

f = (A0 <: Theta) => (A0 <: (A1 <: A0) -> Not A1);

context = [A0 <: Theta];
received :: A0;
expected :: (A1 <: A0) -> Not A1;

context = [A0 <: Theta];
received :: (A1 <: Top) -> Not ((A2 <: A1) -> Not A2);
expected :: (A1 <: A0) -> Not A1;

context = [A0 <: Theta; A1 <: A0];
received :: Not ((A2 <: A1) -> Not A2);
expected :: Not A1;

context = [A0 <: Theta; A1 <: A0];
received :: (A2 <: Top) -> Not ((A3 <: A2) -> Not A3);
expected :: (A2 <: A1) -> Not A2;

Not T = (A <: T) -> A;
Theta = (A <: Top) -> Not ((A <: A) -> Not A);

context = [A <: Theta];
received :: A;
expected :: (A <: A) -> Not A;

// predicativity
Not T = (A <: T) -> A;
Theta : Top 1 = (A <: Top 0) -> Not ((A <: A) -> Not A);

context = [A <: Theta];
received :: A;
expected :: (A <: A) -> Not A;

context = [A <: Theta];
received :: (A <: Top 0) -> Not ((A <: A) -> Not A);
expected :: (A <: A) -> Not A; // clash

// universe polymorphism + eta
context = [A <: Theta];
received :: A;
expected :: (A <: A) -> Not A;

context = [A <: Theta];
received :: (L : Level) -> (A <: Top L) -> Not ((A <: A) -> Not A);
expected :: (A <: A) -> Not A; // eta

context = [A <: Theta];
received :: (A <: Top 1) -> Not ((A <: A) -> Not A);
expected :: (A <: A) -> Not A;

context = [A <: Theta; A <: A];
received :: Not ((A <: A) -> Not A);
expected :: Not A;

context = [A <: Theta; A <: A];
received :: A;
expected :: (A <: A) -> Not A;
```

## Linear by default

```rust
Value L <: Resource L;

id = (A : Resource) => x => x;

fix%map = (f : 'A -> 'B, l : List 'A) =>
  l
  | [] => []
  | hd :: tl =>


```

## ADTs

```rust
Either A B =
  | Left (x : A)
  | Right (x : B);

Either A B =
  | (tag : "left", x : A)
  | (tag : "right", x : B);

Tag = "left" | "right";

is_left = (s : String) -> (tag == "left") Option;

A | B <: C
  A <: C && B <: C

C <: A | B
  C <: A || C <: B;

A & B <: C
  C <: A || C <: B

C <: A & B
  C <: A && C <: B
expected :: C
received :: A & B

expected :: Nat | 1
received :: 0

expected :: String
received :: "left" | "right"

"left" | "right" <: String

Either = (A, B) =>
  | Left (x : A)
  | Right (x : B);

Either = (A, B) =>
  | (#Left, x : A)
  | (#Right, x : A);

f = (x : Either Int String) =>
  x
  | (#Left, x) => x + 1
  | (#Right, x) => 0;

Either = (A, B) =>
  | { tag : "left"; x : A; }
  | { tag : "right"; x : B; };

f = (x : Either Int String) =>
  x
  | { tag : "left"; x}
  | Neq


Person = {
  id : Nat;
  name : String;
  gender : Gender;
};
Company = {
  id : Nat;
  name : String;
};

// ML like
User =
  | Person (person : Person)
  | Company (company : Company);

name = (user : User) =>
  user
  | Person person => person.name
  | Company company => company.name;

// TypeScript
User =
  | { tag : "person"; person : Person; }
  | { tag : "company"; company : Company; };
name = (user : User) =>
  user.tag == "person"
  ? user.person.name
  : user.company.name;

// TypeScript + Pattern Matching
User =
  | { tag : "person"; person : Person; }
  | { tag : "company"; company : Company; };
name = (user : User) =>
  user
  | { tag : "person"; person } => person.name
  | { tag : "company"; company } => company.name;

// TypeScript + Pairs + like
User =
  | ("person", person : Person)
  | ("company", company : Company);
name = (user : User) =>
  user
  | ("person", person) => person.name
  | ("company", company) => company.name;

// TypeScript + Pairs + labels
User =
  | (#Person, person : Person)
  | (#Company, company : Company);
name = (user : User) =>
  user
  | (#Person, person) => person.name
  | (#Company, company) => company.name;

// Dependent Types like
User =
  (tag : "person" | "company",
    payload : tag | "person" => Person | "company" => Company);
name = (user : User) =>
  (tag, payload) = user;
  tag
  | "person" => payload.name
  | "company" => payload.name;

(Nat : Type) & { zero : Nat; }

Either A B =
  | Left (x : A)
  | Right (x : B);

Bool = (A : Type) -> A -> A -> A;
Either A B = (tag : Bool, tag Type A B);

Either A B =
  | (tag : true, A)
  | (tag : false, B);

true <: Bool

f = (x : true : Type) => x;

z = (x : Bool) => x;
l = z (true : Bool);

k : Type & Bool = true;

Nat : (Nat : Type) & {
  zero : Nat;
} = _;

zero : Nat = Nat.zero;

User = {
  id : Nat;
  name : String;
};

x : Type _&_ Bool = true;
l : Type = x;

T : Type = (Int, Int);

type T = (Int, Int);

(x : Int) |
```

## Sum Types

```rust
Either A B =
  | Left (x : A)
  | Right (x : B);

Bool = (A : Type) -> A -> A -> A;
Either A B = (tag : Bool, x : tag Type A B);

Either A B =
  | (true, x : A)
  | (false, x : B);

Either A B =
  (tag : Bool, x : tag ? A : B);

f = (x : Either Int String) =>
  x
  | ("left", x) => true
  | ("right", y) => false;


x : Bool : Type = true;
x : true : Type = true;

true : Bool.Value & Bool.Resource & Type;

User = {
  id : Nat;
  name : String;
};
eduardo = {
  id = 0;
  name = "a";
};

Show = {
  show : (x : Show) -> String;
};

Type
Resource
(x => x);
(x, y);
(x == y);
(x & y);
(x | y);


Nat = (Nat : Type) & { zero : Nat; }

(X : String) | Int;

(T : Type) | Eq T T;


Option = A => | Some (x : A) | None;
Option = A =>
  | (#Some, A)
  | (#None, Unit);

x = ("some", 1);

f = (x : Nat & x == 0) => {
  z = x + 1;
};

magic = (x : A) => (a_eq_b : A == B) : B => subst eq X;

true : Bool _&_ Type;

f = (A : Type & A <: Int) => (x : A) => x;

f = z =>
  z
  | x => x
  | y => y;

Either A B = {K} ->
  (left : (x : A) -> K, right : (x : A) -> K) -> K;

f = either =>
  either (
    left = x => x,
    right = x => x
  )

(x : A & y : B)
(A & B)
```

## Nominal

```rust
Either A B #=
  | ("left", x : A)
  | ("right", x : B);

F := (A : Type) => #A;

X = F Int;
Y = F Int;

Option A #=
  | ("some", A)
  | "none";

x : _("some", 1) = ("some", 1);

rec%Tree R =
  | ("var", var : String, ...R)
  | ("lam", param : String, body : Tree, ...R)
  | ("app", lambda : Tree, arg : Tree, ...R);

Parse_tree #= Tree (...,);
Typed_tree #= Tree (..., type : Type);

Bool #= (A : Type) -> (x : A, y : A) -> A;
true : Bool = (A : Type) => (x : A, y : A) => x;
false : Bool = (A : Type) => (x : A, y : A) => y;

T #= Int;

x : _("left", x : A) = ("left", 1);
x : Either Int String = x;
x : ("left", x : A) = x;

l : _(1, 1) = (1, 1);

Example self_app : Uexp := fun var => UAbs (var := var)
    (fun x : var => UApp (var := var) (UVar (var := var) x) (UVar (var := var) x)).

(x => A => (y : A) => (A => (x : A) => (y : A) => x))


System F + Omega + Module + Inference ->
System F + Omega + Eta + Module + Inference ->
Impredicative CoC + Type : Type + Sigma + Inference ->
Erasable Universe Polymorphic CoC + Sigma + Inference ->
Erasable Universe Polymorphic CoC + Sigma + Identity + Inference ->

Erasable Universe Polymorphic CoC
+ Sigma
+ Identity
+ Inference
+ Record
+ Pattern
+ Nominal
+ Literal
+ Intersection
+ Union;

T #=
  | ("left", x : Int)
  | ("right", x : String);

f = (t : T) =>
  t
  | ("left", x) => ()
  | ("right", x) => ();
Bool = (A : Type) -> (x : A) -> (y : A) -> A;
true = (A : Type) => (x : A) => (y : A) => x;
false = (A : Type) => (x : A) => (y : A) => y;

sequence : {A} ->> {B} ->> (x : A) ->> (y : B) ->> B = _;

sequence = {A} => (x : A) => {B} => (y : B) => _;
sequence = {A} => {B} => (x : A) => (y : B) => _;

f = x => A => (x : A);
f = id => (id 0, id "a");

f = (x) => x;

sequence = _A0 => (x : _A) => _B1 => (y : _B) => x == y;
sequence = _B => (x : _B) => _B => (y : _B) => x == y;
Id : Type -> \0 -> \1;
Id : (A : Type) -> (x : A) -> A = _;
Id : (B : Type) -> (y : B) -> B = _;

id : (A : Type) -> (x : A) -> A = _;

f_int_or_string :
  (pred : Bool) -> (x : pred Type Int String) -> pred Type Int String;
l : (x : Int) -> Int = f_int_or_string true;
x : (x : Int) -> Int = _ = id Int

f = (x : _A0) => (x : Type) => (x : A1)

id : (A : Type) -> A -> A;

(x : _p) -> \0 -> \1
(A : Type) ->  -> A;

_p = Type
x\0
A\x

_r = x

((x : _p) -> _r) (Int : _p)


occurs _A0 _B1
f = x => (A : Type) => (x : A)
y = (B : Bool) => (x : B) => x

((a, b) = p; (b : A)) : ((a, b) = p; A)

((x : _A) => x + 1) : (x : Int) -> Int;

Prop : Type 0

Bool : Prop = (A : Prop) -> A -> A -> A;

f = (pred : Bool) => (x : pred Prop Bool String) => x;

f1 = p => (
  ((A, x) = p; ) : ((A, _) = p; A)
);
f2 = p => (
  (A, x) = p;
  x
);
((x : T) => e1)
```

## Unification

```rust
(A:level):index
(_A:level):index
((A:level):offset):index

id:

(_HOLE\current)\was\distance
(A : Type) -> _C -> () -> (A\1 -> A\2)     -> _C;
(X : Type) -> _C -> () ->         _B\2\2\0 -> _B\2\2\2;


(X : Type) -> _C -> () -> (A\1 -> A\2)\2\2\0 -> (A\1 -> A\2)\2\2\1 -> (A\1 -> A\2)\2\2\2;
(X : Type) -> _C -> () -> (A\1 -> A\2)\2\2\0 -> (A\1 -> A\2)\2\2\1 -> (A\1 -> A\2)\2\2\2;


(_C\1\1)\0 -> A\1        -> (_C\1\1)\3;
(_C\1\1)\0 -> (_B\2\2)\0 -> (_B\2\2)\1

(_C\1\1)\0 -> A\1           -> (_C\1\1)\3;
(_C\1\1)\0 -> ((A\1)\1\2)\0 -> ((A\1)\2\2)\1;

(((A\1)\1\2)\1\1)\0 -> A\1           -> (((A\1)\1\2)\1\1)\3;
(((A\1)\1\2)\1\1)\0 -> ((A\1)\1\2)\0 ->  ((A\1)\1\2)\1;

A\0 -> A\1 -> (A\3)\3;
A\0 -> A\1 -> ((A\1)\1\2)\1;

(x\0) => (y\1) => (x\1)

(_C\1)\0 -> A\1    -> (_C\1)\3;
(_C\1)\0 -> (_B\2) -> (_B\2);

(A : Type) -> A
(B : Type) -> B

A = 0
B = 0

_A = Var
(T)\offset = Link



(x : _A) => (y : _B) => (x : _B\1 -> _B\1);

_A : 0
_B : 1

expected : _A : 2
received : _B\1 -> _B\1 : 0

_A := (_B\1 -> _B\1) : 0

hole : _A
hole_offset : 2
in_offset : 0
in_: _B\1

hole : _A
hole_offset : 2
in_offset : 1
in_: _B\0

_B\1 = hole_offset - in_offset

H := T\(T_offset - H_offset + level H - min_level T)
_B\0 := _A\(2 - 0)\0

(A : Type) -> _A -> (A\2 -> A\3) -> _A\2;
(X : Type) -> _C ->           _B -> _B\1;

(A : Type) -> _A -> (A\2 -> A\3) -> _A\2;
(X : Type) -> _A ->           _B -> _B\1;

(A : Type) -> _A -> (A\2 -> A\3) -> _A\2;
(X : Type) -> _A -> (A\2 -> A\3) -> (A\2 -> A\3)\1;

(A : Type) -> _A -> (A\2 -> A\3) -> _A\2;
(X : Type) -> _A -> (A\2 -> A\3) -> (A\2 -> A\3)\1;

(A : Type) => (x : A\1) => x;
(A : Type) -> (x : A\1) -> A\2;

(x : _X) => (A : Type) => (x : A)

_X : 0

expected : A\1 : 0
received : _X : 2


expected_offset : 0
expected : _A\2
received_offset : 0
received : (A\2 -> A\3)\1

expected_offset : 2
expected : _A
received_offset : 0
received : (A\2 -> A\3)\1


expected : _A -> _A\1
received : _B -> (_C -> _C)

expected : _A -> _A\1
received : _A -> (_C -> _C\1)

_A := (_C -> _C\1)

_A := (A\2 -> A\3)\1\-2

_A\2 := (A\1 -> A\2)\1;
_A := (A\1 -> A\2)(1 - 2)
L = 1
H = 1
M = 0
-1 = (L)



A : Type   B : Type
------------------
      A | B


C <: A || C <: B
----------------
  C <: A | B

A <: C    B <: C
----------------
  A | B <: C

Either A B =
  | (tag : "left", x : A)
  | (tag : "right", x : B);


(tag : "left", x : A) | (tag : "right", x : B) <: (tag : _A, x : _B);

(tag : "left", x : A) <: (tag : _A, x : _B); _A = "left" :: _B = tag | "left" => A;
(tag : "right", x : B) <: (tag : _A, x : _B); _A = "right" :: _B = tag | "right" => B;


(tag : "left" | "right", x : tag | "left" => A | "right" => B)

Bool =
  (H : Type) =>
  (A : Type) =>
  (B : Type) =>
  (K : Type) ->
  ((K_eq_A : H == A) => K) ->
  ((K_eq_B : H == B) => K) -> K;

true : A -> B -> Bool A A B = A => B => K => true => false => true (Refl A);
false : A -> B -> Bool B A B = A => B => K => true => false => false (Refl B);

Either A B =
  | (tag : true, x : A)
  | (tag : false, x : B);


(tag : Bool, x : Bool) | (tag : Int, x : Int);

(tag : Bool | Int, x : typeof tag | Bool => )

(H : Type, H_eq_Bool_or_Int : H == Bool | H == Int, tag : H, x : H)

f = (either : Either Int String) =>  (
  (tag, x) = either;
  ()
);

(A, x) => p

0 =>          1 => 2
x => (A : Type) => (x : \0)

a : (A : Type) -> _C@0 -> (A@2 -> A@3) -> _C@0\2;
b : (X : Type) -> _D@0 ->         _B@0 -> _B@0\1;

a : (A : Type) -> _C@0   -> (A@2 -> A@3) -> _C@0\2;
b : (X : Type) -> _C@0\0 ->         _B@0 -> _B@0\1;

a : (A : Type) -> _C@0   -> (A@2 -> A@3)   ->           _C@0\2;
b : (X : Type) -> _C@0\0 -> (A@2 -> A@3)\0 -> (A@2 -> A@3)\0\1;

// the fancy step
a : 0 : _C@0\2
b : 0 : (A@2 -> A@3)\0\1
a : 2 : _C@0
b : 0 : (A@2 -> A@3)\0\1
_C := (A@2 -> A@3)\0\1\(in_off - h_off)
_C := (A@2 -> A@3)\0\1\-2

// done
a : (A : Type) -> (A@2 -> A@3)\0\1\-2   -> (A@2 -> A@3)   -> (A@2 -> A@3)\0\1\-2\2;
b : (X : Type) -> (A@2 -> A@3)\0\1\-2\0 -> (A@2 -> A@3)\0 -> (A@2 -> A@3)\0\1;


// meta step, just solve the offsets
a : (A : Type) -> (A@2 -> A@3)\-1 -> (A@2 -> A@3)   -> (A@2 -> A@3)\1;
b : (X : Type) -> (A@2 -> A@3)\-1 -> (A@2 -> A@3)\0 -> (A@2 -> A@3)\1;

// meta step, just expand the types
a : (A : Type) -> (A@1 -> A@2) -> (A@2 -> A@3) -> (A@3 -> A@4);
b : (X : Type) -> (A@1 -> A@2) -> (A@2 -> A@3) -> (A@3 -> A@4);

// meta step, just fix the names
a : (A : Type) -> (A@1 -> A@2) -> (A@2 -> A@3) -> (A@3 -> A@4);
b : (X : Type) -> (X@1 -> X@2) -> (X@2 -> X@3) -> (X@3 -> X@4);

(f => (self => f (self self)) (self => f (self self))) id

(. (. \2 (\1 \1)) (. \2 (\1 \1))) (\. \1) : []
(. \2 (\1 \1)) (. \2 (\1 \1)) : (\. \1) :: []
\2 (\1 \1) : (. \2 (\1 \1)) :: (\. \1) :: []
(\. \1) ((. \2 (\1 \1)) (. \2 (\1 \1))) : (. \2 (\1 \1)) :: (\. \1) :: []
\1 : ((. \2 (\1 \1)) (. \2 (\1 \1))) :: (. \2 (\1 \1)) :: (\. \1) :: []
(. \2 (\1 \1)) (. \2 (\1 \1)) : ((. \2 (\1 \1)) (. \2 (\1 \1))) :: (. \2 (\1 \1)) :: (\. \1) :: []
\2 (\1 \1) : (. \2 (\1 \1)) :: ((. \2 (\1 \1)) (. \2 (\1 \1))) :: (. \2 (\1 \1)) :: (\. \1) :: []
((. \2 (\1 \1)) (. \2 (\1 \1))) ((. \2 (\1 \1)) (. \2 (\1 \1))) : (. \2 (\1 \1)) :: ((. \2 (\1 \1)) (. \2 (\1 \1))) :: (. \2 (\1 \1)) :: (\. \1) :: []
(. \2 (\1 \1)) (. \2 (\1 \1)) : ((. \2 (\1 \1)) (. \2 (\1 \1))) :: (. \2 (\1 \1))
(.\1z \1) (.\1 \1) : []
(\1 \1) : [1 -> (.\1 \1)]
(.\1 \1) (.\1 \1) : [1 -> (.\1 \1);]

(f => f f) (f => f f) : []
(f => f f) (f => f f) : [f -> f => f f]

(. \1 \2) : \2


{
  x : Int;
  x = 1;
}

M : {
  call_id : ((A : Type) -> A -> A) -> (x : Int, y : String);
} = {
  call_id = (id : (A : Type) -> A -> A) => x => id x;
  call_id = id => (x = id 1, id "a")
};

f : (p : (A : Type, x : A)) -> ((A, x) = p; A);
f = (p : (A : Type, x : A)) => (
  (A, x) = p;
  x
);

f : (A : Type, x : A) -> A;
f = (A : Type, x : A) => x;

((x : Type) => x\1) ((A : Type\1) -> A\1)
((x : Type\1) -> Type\2) Type\1
x\1[x\1 := ((A : Type) -> A\1)]

((x : Int) => e2) e1;

x = 1;
x

((A : Type\1) => (x : A\1) => x\1) Type\1

(A : Type\1) -> (x : A\1) -> A\2

((x : A\1) -> A\2)[A := Type\1]
((x : Type\1) -> A\2)[A := Type\1]

_A  -> _B
A\1 -> A\2

Type : ()
Type L : Type (L : 1)

Fix : (Fix : Type) & (fix : Fix) -> Unit;
Fix : (Fix : Type, f : (fix : Fix) -> Unit);

List :
  (A : Type) ->
  (L : Type) & (

    | (tag : "nil")
    | (tag : "cons", el : A, next : L)
  );

((A : Type) -> Int\2 -> A\2 -> A\3) Int\1;

((Int\2 -> A\2 -> A\3)\-1)[A := Int\1];
(Int\1 -> A\1 -> A\2)[A := Int\1];
(Int\1 -> Int\1\1 -> Int\1\2);
(Int\1 -> Int\2 -> Int\3);


l : (x : A) -> B   a : A
------------------------
  l a : ((x : A) => B) a

 l : (x : A) -> (y : B) -> C   a : A   b : B
----------------------------------------------
(l a) b : C\-1[x := a]\-1[y := B]

((A : Type\1) => A\1) Type\1

((A : Type) => A\1 -> A\2) String;


((A : Type\1) => (x : A\1) => x) : ((A : Type\1) -> (x : A\1) -> A\2)

(((A : Type\1) -> (x : A\1) -> A\2) Type\1)
((x : A\1) -> A\2); [A := Type\1]
((x : A\0) -> A\1); [A := Type\1]
((x : Type\1\0) -> Type\1\1); [A := Type\1]

(A : Type) => () => A;
(A : Type\1 : Type\0) => () => () => A; depth = 0;
(A : Type\1 : Type\1) => () => () => A; depth = 0; offset = 1;
(A : Type\1 : Type\1) => () => () => A; depth = 1;
(A : Type\1 : Type\1) => () => () => A; depth = 2;
(A : Type\1 : Type\1) => () => () => (A\3 : Type\1); depth = 3;
(A : Type\1 : Type\1) => () => () => (A\3 : Type\4); depth = 3; offset = 3;

is_bound_var = var <= depth;

(A : Type) => (x : A) => x;
(A : Type\1 : Type\0) => (x : A) => x; depth = 0;
(A : Type\1 : Type\1) => (x : A) => x; depth = 0; offset = 1;
(A : Type\1 : Type\1) => (x : A\1 : Type\2) => x; depth = 1;
(A : Type\1 : Type\1) => (x : A\1 : Type\2) => (x\1 : A\1); depth = 2;
(A : Type\1 : Type\1) => (x : A\1 : Type\2) => (x\1 : A\2); depth = 2; offset = 1;

+1

1 <= 0 = false
1 <= 1 = true
2 <= 1 = false
1 <= 2 = true
2 <= 2 = true

(A : Type\1 : Type\1) => (x : A\1 : Type\2) => (x\1 : A\2);

(A : Type\2 : Type\2) => (x : A\1 : Type\2) => (x\1 : A\2); depth = 0;
(A : Type\2 : Type\2) => (x : A\1 : Type\2) => (x\1 : A\2); depth = 0;
(A : Type\2 : Type\2) => (x : A\1 : Type\2) => (x\1 : A\2); depth = 1;

(A : Type\1 : Type\1) => () => () => (A\3 : Type\4); depth = 0;
(A : Type\1 : Type\2) => () => () => (A\3 : Type\4); depth = 0; depth < var
(A : Type\1 : Type\2) => () => () => (A\3 : Type\4); depth = 1;
(A : Type\1 : Type\2) => () => () => (A\3 : Type\4); depth = 2;
(A : Type\1 : Type\2) => () => () => (A\3 : Type\4); depth = 3;
(A : Type\1 : Type\2) => () => () => (A\3 : Type\5); depth = 3; 4 > depth

+1
(A : Type\1 : Type\1) => (A : Type\2); depth = 0;
(A : Type\1 : Type\1) => (A : Type\2); depth = 0;

Type\0 : Type\0
depth = 0; var = 1 - 1;

0 `bound` 0 = false
a `bound` a = false


var < depth

read : String -> Monad String;
debug : {A} -> A -> A;

Type : (l : Level) -> Type (l + 1);
Type 0 : Type 1;

(x : Int, y : Int) => x + y;
(p : (x : Int, y : Int)) => (
  (x, y) = p;
  x + y
);
((x = v; r) ((x => r) v);

((x : Int, y : Int) => x + y) (x = 1; y = 2);


(x : Int, y : Int) -> Int;

l : (x : A) -> B    a : A
-------------------------
  l a : B[x := A]

(x = 1, y = 1) (left => right => );


return = x => x;

add = (x, y) => (
  z = x + y;
  return z;
);

((x, y : Int) => x + y) == ((x : Int, y) => x + y);
((x, y : Int) => y) != ((x, y : String) => y)


(\x: IO () -> Int) (print "foobar")

Type =
  | Type
  | Forall (param : String) (annot : Type) (return : Type)
  | Lambda (param : String) (annot : Type) (return : Term)
  | Apply (lambda : Term) (arg : Term)
  | Inter (var : String) (left : Type) (right : Type)
  | Equal (a : Type) (b : Type);

Term =
  | Var (name : String)
  | Lambda (param : String) (annot : Type) (return : Term)
  | Apply (lambda : Term) (arg : Term);


-----------
Type : Type

 A : Type    B : Type
----------------------
((x : A) -> B) : Type

  A : Type  B : Type  e : B
-----------------------------
((x : A) => e) : (x : A) -> B


Id : Type = (A : Type) -> (x : A) -> A;
id : Id = (A : Type) => (x : A) => x;

Bool = (A : Type) -> (x : A) -> (y : A) -> A;
true = (A : Type) => (x : A) => (y : A) => x;
false = (A : Type) => (x : A) => (y : A) => y;

x : Int & String = _;
z : Int = x;


Connection : Resource = _;
send : (conn: Connection) -> (x : String) -> IO ((), Connection) = _;
close : (conn : Connection) -> IO () = _

alloc : (A : Resource) -> A;
free : (A : Resource) -> (x : A) -> ();

f = (conn0 : Connection) => (
  ((), conn1) = send(conn0, "Hello")?;
  ((), conn2) = send(conn1, "World")?;
  close conn2;
);
```

## Induction

```rust
CNat : Type = X .-> X -> (X -> X) -> X;
cZ : CNat = X .=> z => s => z;
cS : CNat -> CNat = x => X .=> z => s => s (x X z s);

Inductive : CNat -> Type =
  (x : CNat) =>
  (P : CNat -> Type) .->
  P cZ -> ((y: CNat) .-> P y -> P (cS y)) ->
  P x;
iZ : Inductive cZ = P .=> z => s => z;
iS : (x : CNat) .-> Inductive x -> Inductive (cS x) =
  x .=> p => P .=> z => s => s x (p P z s)

Nat : Type = (x : CNat) & Inductive x;
Z : Nat = (x = cZ & iZ);
S : Nat -> Nat = n => (x = cS n.1, iS x n.2);




refl : {A} -> A == A = x => x;

C_bool = A. A -> A -> A;
c_true : C_bool = t => f => t;
c_false : C_bool = t => f => f;

Ind_bool : C_bool -> Type =
  b => (P : Bool). P c_true -> P c_false -> P b;
ind_true : Ind_bool c_true = t => f => t;
ind_false : Ind_bool c_false = t => f => f;

Bool : Type = (b : C_bool) & Ind_bool b;
true : Bool = (b = c_true & ind_true);
false : Bool = (b = c_false & ind_false);

ind_bool : (b : Bool) -> {P} -> P true -> P false -> P b =
  b => {P} => t => f =>
    @b.2
      (x => {X} -> ((m : Bool) -> x == m.1 -> P m -> X) -> X)
      ({X} => c => c true refl t)
      ({X} => c => c false refl f);
      (m => (e : b.1 == m.1) => (u : P m) =>
        e : b == m = e; // injective
        u[m := b | e]
      );

match_bool_with_eq :
  {A} -> (b : Bool) ->
  (t : (b == true) -> A) ->
  (f : (b == false) -> A) -> A =
  {A} => b =>
    @ind_bool b
      (m =>
        (t : (m == true) -> A) ->
        (f : (m == false) -> A) -> A)
      (t => f => t refl)
      (t => f => f refl);
simple_dependent_elimination = (tag : Bool) => (payload : tag Type Int String) => (
  @ind_bool tag
    (tag => (payload : tag Type Int String) -> String)
    (payload => Int.to_string payload)
    (payload => payload);
    payload
);

refl : {A} -> A == A = x => x;

C_bool = A. A -> A -> A;
Ind_bool : C_bool -> Type =
  b => P. P c_true -> P c_false -> P b;
Bool : Type = (b : C_bool) & Ind_bool b;
true : Bool = t => f => t;
false : Bool = t => f => f;

(b : Bool) -> {P} -> P true -> P false -> P b

C_eq = A. A -> A;
c_refl : C_eq = x => x
Ind_eq : C_eq -> Type =
  eq => P. P c_refl -> P eq;
ind_refl : Ind_eq c_refl = x => x;

Eq = (eq : C_eq) & Ind_eq eq;
refl = (eq = c_refl) & ind_refl;

ind_bool : (b : Bool) -> {P} -> P true -> P false -> P b =
  b => {P} => t => f => (
    (m : Bool, x_eq_m_1 : b == m.1, p_m : P m) =
      @b.2 (true, refl, t) (false refl f);
    e : b == m = e; // injective
    u[m := b | e]
  );


Eq = {
  &A : Type;
  eq : (x : A) -> (y : A) -> x == y;
};

( == ) :
  (( == ) : {A} -> (x : A) -> (y : A) -> Type) &
  (( == ) : {A : Eq} -> (x : A) -> (y : A) -> x == y);

( == ) =
  A. (x : A) => (y : A) => R.
    {r_either_type_or_eq :
      Either
        (R == Type)
        (_ : R == Option (x == y), cmp : (x : A) -> (y : A) -> Option (x == y))
    } => (
      r_either_type_or_eq
      | ("left", r_eq_type) => (x == y : R)
      | ("right", (r_eq_opt_x_eq_y, cmp) => (cmp x y : R))
    );

x : R.
    {r_either_type_or_eq :
      Either
        (R == Type)
        (_ : R == Option (x == y), cmp : (x : A) -> (y : A) -> Option (x == y))
    } -> R = 1 == 1;
f : A. (x : A) -> (y : A) ->
    {r_either_type_or_eq :
      Either
        (Type == Type)
        (_ : Type == Option (x == y), cmp : (x : A) -> (y : A) -> Option (x == y))
    } -> Type;

True = (A : Type) -> A -> A;
False = (A : Type) -> A;

C_bool = A. A -> A -> A;
c_true : C_bool = t => f => t;
c_false : C_bool = t => f => f;

Ind_bool : C_bool -> Type =
  b => (P : Bool). P c_true -> P c_false -> P b;
ind_true : Ind_bool c_true = t => f => t;
ind_false : Ind_bool c_false = t => f => f;

Bool : Type = (b : C_bool) & Ind_bool b;
true : Bool = (b = c_true & ind_true);
false : Bool = (b = c_false & ind_false);

f : ((x : Int) -> Int) & ((x : String) -> String) = x => x;
ind_bool : (b : Bool) -> {P} -> P true -> P false -> P b = _;
ind_eq : A. (x : A) -> (y : A) -> (eq : Eq x y) -> P. P x -> P y = _;

f = (eq : true == false) : False => (
  t : (b =>
    ind_bool b
      (_ => Type)
      True
      False
  ) true = x => x;
  f : (b =>
    ind_bool false
      (_ => Type)
      True
      False
  ) false = (t : _[true := false | eq]);
  f
);

Either A B =
  (left : Bool, payload : left A B);

match_bool_with_eq :
  {A} -> (b : Bool) ->
  (t : (b == true) -> A) ->
  (f : (b == false) -> A) -> A =
  {A} => b =>
    @ind_bool b
      (m =>
        (t : (m == true) -> A) ->
        (f : (m == false) -> A) -> A)
      (t => f => t refl)
      (t => f => f refl);
f = (x : Either Int String) => (
  (left, payload : left Int String) = x;
  match_bool_with_eq left
    (left_eq_true => Int.to_string (payload : _[left := true | left_eq_true]))
    (left_eq_false => (payload : _[left := false | left_eq_false]))
);

make : (x : Nat) -> (x_gt_z : x >= 0) -> _ = _

make : (x : Nat, {x >= 0}) -> _ = _

Either A B =
  | (tag : "left", payload : A)
  | (tag : "right", payload : B);


'A = (
  f = (x : 'A) => x;
  x : 'A = 1;
  int
)
```

## Holes

```rust
a : (A : Type) -> _C -> (A\2 -> A\2) -> _C\2
b : (X : Type) -> _D ->           _B -> _B\1

a : (A : Type) -> _C -> (A\2 -> A\2) -> _C\2
b : (X : Type) -> _C ->           _B -> _B\1

a : (A : Type) -> _C -> (A\2 -> A\2) -> _C\2
b : (X : Type) -> _C -> (A\2 -> A\2) -> (A\2 -> A\2)\1

a : (A : Type) -> _C -> (A\2 -> A\2) -> _C\2
b : (X : Type) -> _C -> (A\2 -> A\2) -> (A\2 -> A\2)\1

a : 0 : _C\2
b : 0 : (A\2 -> A\2)\1

a : 2 : _C
b : 1 : (A\2 -> A\2)

_C := (A\2 -> A\2)\-1;

a : (A : Type) -> (A\2 -> A\2)\-1 -> (A\2 -> A\2) -> (A\2 -> A\2)\-1\2
b : (X : Type) -> (A\2 -> A\2)\-1 -> (A\2 -> A\2) -> (A\2 -> A\2)\1

a : _A ->         _A\1 -> _B
b : _C -> (_D -> _D\1) -> _D\1

a : _A ->         _A\1 -> _B
b : _A -> (_D -> _D\1) -> _D\1

a : _A\1
b : (_D -> _D\1)

a : 1 : _A
b : 0 : (_D -> _D\1)

1 - 0
0 < 1
_D := _E\1
_A := (_E\1 -> _E\1\1)\-1

a : (_E\1 -> _E\1\1)\-1 -> (_E\1 -> _E\1\1)\-1\1 -> _B
b : (_E\1 -> _E\1\1)\-1 -> (_E\1 -> _E\1\1) -> _E\1\1

a : (_E -> _E\1) -> (_E\1 -> _E\2) -> _E\2
b : (_E -> _E\1) -> (_E\1 -> _E\2) -> _E\2
```

## Induction and Literals

```rust
(Bool, true, false, ind_bool) = (
  Bool = A. A -> A -> A;
  true : Bool = t => f => t;
  false : Bool = t => f => f;

  Bool = (b : Bool) & (P. P true -> P false -> P b);
  true : Bool = t => f => t;
  false : Bool = t => f => t;

  ind_bool : (b : Bool) -> P -> P true -> P false -> P b =
    b => P => p_t => p_f => (
      (m, b_eq_m, p_m) =
        b
          (x => (m : Bool, x_eq_m1 : x == m, p_m : P m))
          (true, refl, p_t)
          (false, refl, p_t);
      (p_m : _[m := b | b_eq_m]);
  );

  (Bool, true, false, ind_bool);
);

Either A B =
  | (tag = true, payload : A)
  | (tag = false, payload : B);

(tag = true, payload : A) <: (tag : Bool, payload : tag A B)
(tag = false, payload : B) <: (tag : Bool, payload : tag A B)

f = (x : Either Int String) => (
  (tag, payload : tag Int String) = x;
  ind_bool
    ((payload : Int) => Int.to_string payload)
    ((payload : String) => payload)
    payload
);

expected : (tag : Bool, payload : tag Type A Never);
received : (tag : Bool & tag == true, payload : A);

expected : Bool
received : (tag : Bool & tag == true)

f = (x : Either Int String) => (
  x : (tag : Bool, payload : tag Type A B) = x;
  (tag, payload : tag Type A B) = x;
  ()
);

a : (a : Int, b : Int) -> a == b
b : (a : _) -> _
((p : (a : Int, b : Int)) -> ((a : Int, b : Int) => a == b) p);

x => x

a : F (a = Int, b = String) -> ((a : Type, b : Type) => a) _B -> Int;
b : F (_A : _)                    -> ((a : Type, b : Type) => a) _A -> Int;

a : (a : Int, b : Int) => a;
b : (_x : _A) => _b;

a : ((a, b) : (a : Int, b : Int)) => a
b : (_x : _B) => _b;


x = (p : (a : Int, b : Int)) -> ((a : Int, b : Int) => a == b);
a : (a : Int, b : Int) => a;
b : (x : _A) => _b;


C_bool = (A : Type) -> A -> A -> A;
c_true : Bool = A => t => f => t;
c_false : Bool = A => t => f => f;

Ind_bool b = (P : Bool -> Type) -> P c_true -> P c_false -> P b;
Bool = (b : Bool & Ind_bool b);
true : Bool = X => t => f => t;
false : Bool = X => t => f => t;
ind_bool : (b : Bool) -> P -> P true -> P false -> P b =
  b => P => p_t => p_f => (
    (m, b1_eq_m1, p_m) =
      b.2
        (x => (m : Bool, x_eq_m1 : x == m.1, p_m : P m))
        (true, refl, p_t)
        (false, refl, p_t);
    b_eq_m : b == m = b1_e1_m1;
    (p_m : _[m := b | b_eq_m]);
);
(t => f => t) : A. A -> A -> A;

expected : (P : Bool -> Type) -> P true -> P false -> P b;
received : (A : Type)         -> A      ->       A ->   A;
```

## Row Polymorphism

```rust
pure : A. A -> IO A;
bind : A. B. (A -> B) -> IO A -> IO B;

handle : (A. Effect A -> A) -> K. (() -> Effect K) -> K

or = Read A;

Read | Write
read : E. () -> Read `or` E $-> String;
write : E. () -> Write `or` E $-> String;

And : (A : Type) -> (B : Type) -> Type;

Level : Type¹
f = (x : Nat `And` x == 1)

f : () -> (A => Read A | Write A) String;
```

## Hurkens Universe Polymorphic

```rust
⊥ = (A: *) -> A;
¬ φ === φ -> ⊥;

℘ S === (L : Level) -> S -> Type L;

U : Type 1 = (L : Level) -> (X : Type L) -> (℘℘X -> X) -> ℘℘X;
τ (t: ℘℘U) = (L : Level) => (X : Type L) => (f : ℘℘X -> X) => (p : ℘X) =>
  t L (L2 => (x : U) => p L2 (f (x L X f)));
σ (s: U) : ℘℘U = s 1 U τ;
∆ = (y: U) => (p: ℘U) -> σ y p -> p (τ (σ y));
Ω : U = τ ((p: ℘U) => (x: U) -> σ x p -> p x);

℘ S === (L : Level) -> S -> Type L;
U : Type 1 = (L : Level) -> (X : Type L) -> (℘℘X -> X) -> ℘℘X;

τ : (℘℘U -> U;
σ : U -> ((L : Level) -> ℘U -> Type L);

Ω : U = τ (L => (p: ℘U) => (x: U) -> σ x L p -> p L x);
```

## Effects

```rust
Id = (A : Type) -> (x : A) -> A;
id = (A : Type) => (x : A) => x;

Bool = (A : Type) -> (t : A) -> (f : A) -> A;
true : Bool = A => t => f => t;
false : Bool = A => t => f => f;
ind_bool : b => (P : Bool -> Type) -> P true -> P false -> P b = _;
match_bool_with_eq :
  {A} -> (b : Bool) ->
  (t : (b == true) -> A) ->
  (f : (b == false) -> A) -> A = _;

Compare = {A} -> (x : A) -> (y : X) -> Either (x == y) (x <> y);

Either A B = (tag : Bool, payload : tag Type A B);
f = (x : Either Int String) =>
  x
  | (tag = true, payload) => Int.to_string payload
  | (tag = false, payload) => payload;
```

## Normalization

```rust

Term =
  | x
  | (x : Term) -> Term
  | (x : Term) => Term
  | Term Term
  | (a : Term, b : Term)
  | (a = Term, b = Term)
  | (x, y) = Term; Term

norm
  | <x> => <x>
  | <(p : A) -> r> => norm_forall <(p : A) -> r>
  | <(p : A) => r> => norm_lambda <(p : A) => r>
  | <lambda arg> => norm_apply <lambda arg>;


norm_forall =
  | <(x : A) -> B> =>
    A = norm A;
    B = norm B;
    <(x : A) -> B>
  | <(p : A) -> B> =>
    norm <(x : A) -> ((p : A) => B) x>;

norm_lambda =
  | <(x : T) => r> =>
    T = norm T;
    r = norm r;
    <(x : T) => r>
  | <(x : A, y : B) => r> =>
    A = norm A;
    B = norm B;
    r = norm r;
    <(x : A, y : B) => r>
  | <(p1 : A, p2 : B) => r> =>
    norm <
      (z : (p1 : A, p2 : B)) =>
        (x : A, y : B) = z;
        (p1 : A) = x;
        (p2 : B) = y;
        r
    >;

norm_apply1
  | <lambda arg> =
    // TODO: norm head?
    lambda = norm lambda;
    arg = norm arg;
    norm_apply2 <lambda arg>;

norm_apply2
  // apply
  | <((x : A) => r) arg> =>
    norm (r[x := arg])
  // fst
  | <((x : A, y : B) => x) (x = arg_x, y = _)> =>
    <arg_x>
  // snd
  | <((x : A, y : B) => y) (x = _, y = arg_y)> =>
    <arg_y>
  // unpair
  | <((x : A, y : B) => r) arg> =>
    arg_x = <((x : A, y : B) => x) arg>;
    arg_y = <((x : A, y : B) => y) arg>;
    norm (r[x := arg_x][y := arg_y])
  // normal
  | <lambda arg> => <lambda arg>

norm_exists =
  | <(p : A, _ : B)> =>
    A = norm A;
    B = norm <((p : A) => B) x>;
    <(x : A, _ : B)>;

f = (
  (a : Int, b : Int) = a_b;
  (c_d : (c : Int, d : a == c)) => (
    (c : Int, d : a == c) = c_d;
    a + b + c
  )
) (c = 1, d = d);


f = (
  (a : Int, b : Int) = a_b;
  (c_d : (c : Int, d : a == c)) => (
    (c : Int, d : a == c) = c_d;
    a + b + c
  )
) (c = 1, d = d);

f = (
  (a : Int, b : Int) = a_b;
  (c_d : (c : Int, d : a == c)) => (
    (c : Int, d : a == c) = c_d;
    a + b + c
  )
) (c = 1, d = d);

f = (
  (a : Int, b : Int) = a_b;
  (c_d : (c : Int, d : a == c)) => (
    (c : Int, d : a == c) = c_d;
    a + b + c
  )
) (c = 1, d = d);

f = (
  (a : Int, b : Int) = a_b;
  (c : Int, d : a == c) = c_d;
  a + b + c
)[c_d := (c = 1, d = d)];

Either A B =
  | (tag = "left", payload : A)
  | (tag = "right", payload : B);

f = (x : Either Int String) =>
  [%match x
  | (tag = "left", payload) => Int.to_string payload
  | (tag = "right", payload) => payload];

f = ((a : Int, b : Int), (c : Int, d : a == c)) => a + b + c;
f = (a_b_c_d : (a_b : (a : Int, b : Int), c_d : (c : Int, d : a == c))) => (
  (a_b : (a : Int, b : Int), c_d : (c : Int, d : a == c)) = a_b_c_d;
  (a : Int, b : Int) = a_b;
  (c : Int, d : a == c) = c_d;
  a + b + c
);
f = (a_b_c_d : (a_b : (a : Int, b : Int), c_d : (c : Int, d : a == c))) =>
  ((a_b : (a : Int, b : Int), c_d : (c : Int, d : a == c)) => (
    (a : Int, b : Int) = a_b;
    (c : Int, d : a == c) = c_d;
    a + b + c
  )) a_b_c_d;
f = (a_b_c_d : (a_b : (a : Int, b : Int), c_d : (c : Int, d : a == c))) =>
  ((a_b : (a : Int, b : Int), c_d : (c : Int, d : a == c)) => (
    ((a : Int, b : Int) => (
      (c : Int, d : a == c) = c_d;
      a + b + c
    )) a_b
  )) a_b_c_d;

f =
  (a_b_c_d : (a_b : (a : Int, b : Int), c_d : (c : Int, d : a == c))) =>
    ((a_b : (a : Int, b : Int), c_d : (c : Int, d : a == c)) =>
      ((a : Int, b : Int) => ((c : Int, d : a == c) => a + b + c) c_d) a_b) a_b_c_d;

f = ((a : Int, b : Int) => (c : Int, d : a == c) => a + b + c) a_b (c = 1; d = d);
f = ((a : Int, b : Int) => a + b + 1) a_b

f =
  ((a : Int, b : Int) =>
    (c_d : (c : Int, d : a == c)) =>
      ((c : Int, d : a == c) => a + b + c) c_d) a_b (c = 1; d = d);
f = (
  (a : Int, b : Int) = a_b;
  (c_d : (c : Int, d : a == c)) => (
    (c : Int, d : a == c) = c_d;
    a + b + c
  )
) (c = 1; d = d);

f = (
  (a : Int, b : Int) = a_b;
  (c_d : (c : Int, d : a == c)) => (
    (c : Int, d : a == c) = c_d;
    a + b + c
  )
) (c = 1; d = d);

f = (
  (a : Int, b : Int) = a_b;
  a + b + 1
);

f =
  ((a : Int, b : Int) => (c_d : (c : Int, d : a == c)) =>
    ((c : Int, d : a == c) => a + b + c) c_d); a_b :: (c = 1; d = d) :: []

f =
  ((a : Int, b : Int) =>
    ((c : Int, d : a == c) => a + b + c) (c = 1; d = d)); []

f = ((a : Int, b : Int) => a + b + 1) a_b;
E = (a : Int, (b, c) : (b : Int, c : Int));
E = (a : Int, b_c : (b : Int, c : Int));

E = ((a, b) : (a : Int, b : Int), a_eq_b : a == b);
E = (a_b : (a : Int, b : Int), a_eq_b : ((a : Int, b : Int) => a == b) a_b);


f = (a : Int, b : Int) => a == b;
f = (a_b : (a : Int, b : Int)) => ((a : Int, b : Int) => a == b) a_b;

f = (a : Int, b : Int) -> a == b;
f = (a_b : (a : Int, b : Int)) -> ((a : Int, b : Int) => a == b) a_b;

f = (a_b : (a : Int, b : Int)) -> ((a : Int, b : Int) => a == b) a_b;
f = (a_b : (a : Int, b : Int)) -> ((a : Int, b : Int) => a == b) a_b;

f = (((a : Int, b : Int), c : Int) => a + b + c) (a_b = a_b, c = 1);

f = (((a : Int, b : Int), c : Int) => a + b + c) (a_b = a_b, c = 1);
f = (((a : Int, b : Int), c : Int) => ((a : Int, b : Int) => a + b + c) a_b) (a_b = a_b, c = 1);
f = (((a : Int, b : Int) => a + b + 1) a_b);

f = ((a : Int, b : Int), (c : Int, d : a == c)) => a + b + c;
f = ((a : Int, b : Int), (c : Int, d : a == c)) => a + b + c;

f =
  ((a : Int, b : Int), c_d : (c : Int, d : a == c)) =>
    ((c : Int, d : a == c) => a + b + c) c_d;
f =
  (a_b : (a : Int, b : Int), c_d : (c : Int, d : a == c)) =>
    ((a : Int, b : Int) => ((c : Int, d : a == c) => a + b + c) c_d) a_b;

f =
  (a_b_c_d : (a_b : (a : Int, b : Int), c_d : (c : Int, d : a == c))) =>
    ((a_b : (a : Int, b : Int), c_d : (c : Int, d : a == c)) =>
      ((a : Int, b : Int) => ((c : Int, d : a == c) => a + b + c) c_d) a_b) a_b_c_d

f =
  (a_b_c : (a_b : (a : Int, b : Int), c : Int)) =>
    ((a_b : (a : Int, b : Int), c : Int) =>
      ((a : Int, b : Int) => a + b + c) a_b) a_b_c;

f = (a_b : (a : Int, b : Int)) => f a_b;
f = (a : Int, b : Int) => f (a = a, b = b);

f = (a_b : (a : Int, b : Int)) =>
  ((a : Int, b : Int) => f (a = a, b = b)) a_b;

f = (a_b_c : ((a : Int, b : Int), c : Int)) => (((a : Int, b : Int), c : Int) => a + b + c) a_b_c;
f = (a_b_c : ((a : Int, b : Int), c : Int)) => (((a : Int, b : Int), c : Int) => a + b + c) a_b_c;


f = (((a : Int, b : Int), c : Int) => a + b + c) (a_b = a_b, c = 1);
f = ((a : Int, b : Int) => a + b + 1) a_b);

f = (a : Int, b : Int) => ();
f = (a_b : (a : Int, b : Int)) => ();
f = (a_b : (a : Int, b : Int)) => ();
```

## Quantitative / Graded

```rust
id : (A : 0 : Type) -> (x : 1 : A) -> A;
id = (A : 0 : Type) => (x : 1 : A) => x;

double = (x : 2 : Int) => x + x;


with_x : ((x : 1 : Int) -> Int) -> Int;

// fails
x = with_x (x => double x);
```

## Forget + Intersection

```rust
(Bool, true, false, ind_bool) = (
  Bool = A. A -> A -> A;
  true : Bool = t => f => t;
  false : Bool = t => f => f;

  Bool = (b : Bool) & (P. P true -> P false -> P b);
  true : Bool = t => f => t;
  false : Bool = t => f => t;

  ind_bool : (b : Bool) -> P -> P true -> P false -> P b =
    b => P => p_t => p_f => (
      (m, b_eq_m, p_m) =
        b
          (x => (m : Bool, x_eq_m1 : x == m, p_m : P m))
          (true, refl, p_t)
          (false, refl, p_t);
      (p_m : _[m := b | b_eq_m]);
  );

  (Bool, true, false, ind_bool);
);

Either A B =
  | (tag = true, payload : A)
  | (tag = false, payload : B);

f = (x : Either Int String) => (
  (tag : Bool, payload : tag Type Int String) = x;
  payload
);

F = (x : Either Int String) ->
  ((tag : Bool, payload : tag Type Int String) -> payload) x
f = (x : Either Int String) =>
  ((tag : Bool, payload : tag Type Int String) => payload) x
f = (x : Either Int String) => (
  (tag : Bool, payload : tag Type Int String) = x;
  payload
);


f = (x : Int) => (
  x
  | (tag = true, payload) => Int.to_string payload
  | (tag = false, payload) => payload
);

id = (L : Level) => (A : Type L) => (x : A) => x;

f = (b : Bool & b == true);

with_linear_int : (cb : (x : 1 : Int) -> Int) -> Int = _;
x = with_linear_int (x => x + x);

id = (A : 0 : Type) => (x : A) => x;
rewrite =
  (A : Type) => (B : Type) =>
  (x_eq_y : 0 : A == B) =>
  (x : 1 : A) => (x : A[A := B | x_eq_y]);

Map Key Value : {
  find : (key : Key) -> (map : @Map) -> Value;
  find_and_remove : (key : Key) -> (map : @Map) -> (Value, @Map);
} = _;

```

## IUP + Induction

```rust
// Church booleans
C_bool : Type 1 = (L : Level) -> (A : Type L) -> A -> A -> A;
c_true : Bool = L => A => t => f => t;
c_false : Bool = L => A => t => f => f;

// Induction principle
I_bool = b : Type 1 => (L : Level) -> (P : C_bool -> Type L) -> P c_true -> P c_false -> P b;
// Without IUP, Bool would be placed on Type Omega, making it useless
Bool : Type 1 = (b : Bool) & (ind : I_bool b);
true : Bool = L => X => t => f => t;
false : Bool = L => X => t => f => t;

ind_bool : (b : Bool) -> (L : Level) -> (P : Bool -> Type L) -> P true -> P false -> P b =
  b => L => P => p_t => p_f => (
    (m, b1_eq_m1, p_m) =
      b.2
        L
        (x => (m : Bool, x_eq_m1 : x == m.1, p_m : P m))
        (c_true, refl, p_t)
        (c_false, refl, p_f);
    // b.1 == m.1 implies in b == m
    b_eq_m : b == m = b1_eq_m1;
    // use b == m to change P m into P b
    (p_m : _[m := b | b_eq_m]);
);

C_sigma = (A : Type) => (B : A -> Type) =>
  (K : Type) -> ((x : A) -> (y : B x) -> K) -> K;
c_exist = A => B => x => y : C_sigma A B => K => f => f x y;

I_sigma = (A : Type) => (B : A -> Type) => s =>
  (P : C_sigma A B -> Type) ->
  ((x : A) -> (b : B x) -> P (c_exist A B x y)) ->
  P s;
Sigma = A => B => (s : C_sigma A B) & (ind : I_sigma s);

exist = A => B => x => y : Sigma A B => X => f => f x y;

ind_sigma = A => B => x => y => s => (P : Sigma A B -> Type) => (p_e : P (exist A B x y)) =
  (m, s1_eq_m1) =
    s.2
      (x => (m : Sigma, x_eq_m1 : x == m.1, p_m : P m))
      (x => y => (exist A B x y, refl, p_e));

C_sigma = (L : Level) => (A : Type L) => (B : A -> Type L) =>
  (K_L : Level) -> (K : Type K_L) -> ((x : A) -> (y : B x) -> K) -> K;
c_exist = L => A => B => x => y : C_sigma A B => K_L => K => f => f x y;

Exists : Type 2 = ((K_L : Level) -> (K : Type K_L) -> ((x : Type 1) -> (y : x) -> K) -> K);
x = c_exist 1 Type (A => A) Nat 1;

I_sigma = (A : Type) => (B : A -> Type) => s =>
  (P : C_sigma A B -> Type) ->
  ((x : A) -> (b : B x) -> P (c_exist A B x y)) ->
  P s;
Sigma = A => B => (s : C_sigma A B) & (ind : I_sigma s);
exist = A => B => x => y : Sigma A B => X => f => f x y;


C_sigma = (A : Type) => (B : A -> Type) =>
  (K : Type) -> ((x : A) -> (y : B x) -> K) -> K;
c_exist = A => B => x => y : C_sigma A B => K => f => f x y;

I_sigma = (A : Type) => (B : A -> Type) => s =>
  (P : C_sigma A B -> Type) ->
  ((x : A) -> (b : B x) -> P (c_exist A B x y)) ->
  P s;
Sigma = A => B => (s : C_sigma A B) & (ind : I_sigma s);
exist = A => B => x => y : Sigma A B => X => f => f x y;
```

## Core

```rust
C_bool : Type 1 = (A : Type L) -> A -> A -> A;
c_true : Bool = A => t => f => t;
c_false : Bool = A => t => f => f;

// Induction principle
I_bool = b => (P : C_bool -> Type) -> P c_true -> P c_false -> P b;
// Without IUP, Bool would be placed on Type Omega, making it useless
Bool : Type 1 = (b : Bool) & (ind : I_bool b);
true : Bool = L => X => t => f => t;
false : Bool = L => X => t => f => t;

t : (x : A) & B;
t.2 : B[x := t.1]

Equal : (A : Type) -> (x : A) -> (y : A) -> Type;
refl : (A : Type) -> (x : A) -> Equal A x x;
subst :
  (A : Type) ->
  (x : A) ->
  (y : A) ->
  (x_eq_y : x == y) ->
  (P : A -> Type) ->
  P x ->
  P y;
symm = A => (x : A) => y => (x_eq_y : x == y) =>
  subst A x y x_eq_y
    (n => n == x)
    (refl A x);
```

## More IUP

```rust
((X : _) => (x : _) => x) : (A : Type) -> (x : A) -> A;

(b : (A : Type) -> A -> A -> A) &
  ((P : ((A : Type) -> A -> A -> A) -> Type) -> (P ((A\1 : Type) => (t : A) => (f : A) => t)) ->  (P ((A\2 : Type) => (t\1 : A) => (f\1 : A) => f)) -> P b)

id = (x : _A) => x;

expected :: (A : Type) -> (x : A) -> A;
received :: (X : Type) -> (y : X) -> X;

expected :: (A : \1) -> (x : \1) -> \2;
received :: (X : \1) -> (y : \1) -> \1;

with_context : {A} -> ({K} -> Context -> (A -> K) -> K) -> _;
with_offset : Side -> Offset.t -> (() -> T 'A) -> T 'A

norm 0 [] <((A : Type\1) => (x : A\1) => (y : A\2) => (x\2 : A\3)) Type\1>;
norm -1 [A := Type\1] <(x : A\1) => (y : A\2) => (x\2 : A\3)>;
norm -1 [A := Type\1] <(x : A\1) => (y : A\2) => (x\2 : A\3)>;

((x : A\1) => x\1)\-1[A := Type\1]

((x : A\1) => x\1)\-1[A := Type\1]

((x : A\1) => x\1)\-1[A := Type\1]

l : (x : A) -> B    a : A
-------------------------
  l a : ((x : A) => B) a

l : (x : A) -> B    a : A
-------------------------
  l a : B[x := a]
```

## Unification

```rust
a : (A : Type) -> _C -> (A\2 -> A\3) -> _C\2
b : (X : Type) -> _D ->           _B -> _B\1

a : (A : Type) -> _C -> (A\2 -> A\3) -> _C\2
b : (X : Type) -> _C ->           _B -> _B\1

a : (A : Type) -> _C -> (A\2 -> A\3) -> _C\2
b : (X : Type) -> _C -> (A\2 -> A\3) -> (A\2 -> A\3)\1

a : (A : Type) -> _C -> (A\2 -> A\3) -> _C\2
b : (X : Type) -> _C -> (A\2 -> A\3) -> (A\2 -> A\3)\1

a : 2 : _C\2
b : 1 : (A\2 -> A\3)

_C := (A\2 -> A\3)\-1

a : 0 : [] : (A : Type) -> ((B : Type) -> (A\1 -> B\1)\2)\-1 = (A : Type) -> (B : Type) -> A\2 -> B\2
  a : -1 : [Bound] : (B : Type) -> (A\1 -> B\1)\2 = (B : Type) -> A\2 -> B\2
    a : 1 : [Bound, Bound] : A\1 -> B\1 = A\2 -> B\2
      a : 1 : [Bound, Bound] : A\1 = A\2
      a : 1 : [Bound, Bound] : B\1 = B\2


(e : A) === (((x : A) => x) e) === e

succ : Nat -> Nat = _;
(((x : Int) => succ x) 0);
succ (((x : Int) => x) 0);

(L : Level) -> (T : Type L, x : T)

(A : Type) -> A

(A : Type) -> (x : A) -> A
(A : Type\1) ->

norm [Bound, Bound] ((x : A\1) -> (A\2 : Type\3))
```

## Syntax

```rust
x = ((A : Type) => A : (A : Type) -> )

(\1 -> \1)+1~

(A -> A)\[a.te:0:1 .. a.te:1:2]

index
#+-offset
#[a.te:0:1 .. a.te:1:2]


(A : Type) => (A : Type) => (x : A\1) => x;


add = (a, b) => a + b;
(x : Bool) = 1;

((id : (A : \1 : (\1 : \1)) -> (x : \1 : (\1 : (\2 : \2)) -> (\2 : (\1 : (\1 : \1))#+2)) => (\1 : ((A : \1 : (\1 : \1)) -> (x : \1 : (\1 : (\2 : \2)) -> (\2 : (\1 : (\1 : \1))#+2))#+1)) ((A : \1) => (x : \1) => (\1 : \1#+1))
```

## Eta Sigma?

```rust
C_sigma A B = (K : Type) -> ((l : A) -> (r : B l) -> K) -> K;
c_pair A l B r : C_sigma A B = K => f => f l r;


Sigma = A => (x : A) => B => (y : B x) => (

);

f = (
  (a : Int, b : Int) = a_b;
  (c_d : (c : Int, d : a == c)) => (
    (c : Int, d : a == c) = c_d;
    a + b + c
  )
) (c = 1, d = d);

```

## Weak Intersections

```rust
C_bool = (A : Type) -> A -> A -> A;
c_true : C_bool = A => t => f => t;
c_false : C_bool = A => t => f => f;

Ind_bool = b => (P : C_bool -> Type) -> P c_true -> P c_false -> P b;

Bool = (b : C_bool) & Ind_bool b;

Bool = (b !: C_bool & ind : Ind_bool b)

ind_true : I_bool c_true = P => p_c_t => p_c_f => p_c_t;
ind_false : I_bool c_false = P => p_c_t => p_c_f => p_c_f;

Never = (A : Type) -> A;
rec%Fix = (self : Fix) -> Never;


Bool = rec%self. (
  c : C_bool,
  ind : Ind_bool c,
  inj : 0 : (x : Bool) -> fst self == fst x -> self == x
);
true = rec%self. (c = c_true, ind = i_true, inj = y => fst_self_eq_fst_y => (
  (_, _, x_inj) = x;
  y_eq_self : fst y == fst self -> y == self) = x = y_inj self (symm fst_self_eq_fst_y);
  symm y_eq_self;
));


fst a == fst b -> a == b

true : Bool = _;
false : Bool = _;
ind_bool = b => P => p_t => p_f => (
  (x : Bool, c_b_eq_c_x : fst b == fst x, p_x : P x) =
    (snd b) _ (true, refl, p_t) (false, refl, p_f);

)

```

## Leibniz Equality

```rust
Term =
  | Var (String)
  | Lambda (String, Term, Term)
  | Apply (Term, Term)
  | Type
  | Forall (String, Term, Term)
  | Mu (String, Term);

( == ) = A => (x : 0 : A) => (y : 0 : A) => (P : A -> Type) -> P x -> P y;
(refl : (A : Type) -> (x : 0 : A) -> x == x) =
  A => x => P => p_x => p_x;
(symm : (A : Type) -> (x : 0 : A) -> (y : 0 : A) -> x == y -> y = x) =
  A => x => y => x_eq_y => x_eq_y (z => z == x) (refl A x);

C_bool = (A : Type) -> A -> A -> A;
(c_true : C_bool) = A => t => f => t;
(c_false : C_bool) = A => t => f => f;

Ind_bool b = (P : C_bool -> Type) -> P c_true -> P c_false -> P b;


Y : (A : Type) -> (A -> A) -> A = f => (
  fix = (x : @S. S -> A) => f (x x);
  fix fix
);

Nat = @Nat. (A : Type) -> (zero : A) -> (succ : Nat -> A) -> A;

Pair A B = (K : Type) -> ((l : A) -> (r : B l) -> K) -> K;
pair A x B y : Pair A B = K => f x y;
fst A B (pair : Pair A B) = pair A (l => r => l);
snd A B (pair : Pair A B) = pair (B (fst A B pair)) (l => r => r);

rec%(Bool : Type) = Pair C_bool (comp => (
  ind : Ind_bool b,
  rel : (x : Bool) -> self.comp == x.c -> self == x
));

P_bool Rel = (comp : C_bool, rel : Rel);
X = rec @@ Rel =>
  (x : P_bool Rel) ->
  fst comp == fst x ->

Pair C_bool
(Rel : C_bool -> Type) =>
  (K : Type) -> (
    (comp : C_bool) ->
    (ind : Ind_bool comp) ->
    (rel : rec Rel @@
      (x : Bool) ->
        comp == (x C_bool (comp => _ => _ => comp)) -> self == x
      ) -> K)
    -> K;
Rel = (comp : C_bool) =>


P_bool (Rel : C_bool -> Type) =
  (K : Type) -> ((comp : C_bool) -> Ind_bool comp -> Rel comp -> K) -> K;
Make_rel = (Rel : C_bool -> Type) => (self : P_bool Rel) => (comp : C_bool) =>
  (x : P_bool Rel) -> comp == (x C_bool (comp => _ => _ => comp)) -> self == x;


rec%(Bool : Type) = (
  comp : C_bool,
  ind : Ind_bool b,
  rel : (x : Bool) -> self.comp == x.c -> self == x
);
rec%(self : Bool) => {
  b : C_bool;
  i : Ind_bool b;
  p : (x : 1 : Bool) -> self.c == x.c -> self == x;
};
rec%(true : Bool) = {
  b = c_true;
  i = P => p_t => p_f => p_t;
  p = x => true_c_eq_x_c => (
    x_c_eq_true_c = symm A true.c x.c true_c_eq_x_c;
    x_eq_true = x.p true x_c_eq_true_c;
    symm A x true x_eq_true
  );
};

rec%(Bool : Type) = rec%(self : Bool) => {
  b : C_bool;
  i : Ind_bool b;
  p : 0 : (x : Bool) -> self.c == x.c -> self == x;
};

rec%(Fix : Type) = (self : Fix) -> (A : Type) -> A;


f = (1 : (
  fix : Fix = fix => fix fix;
  magic : (A : Type) -> A = fix fix;
));


```

## Safe

````rust
// computing
C_bool B T F (S : B) = (A : Type) -> (S == T -> A) -> (S == F -> A) -> A;

(c_true : C_bool) = A => t => f => t;
(c_false : C_bool) = A => t => f => f;

// computing
C_bool = (A : Type) -> A -> A -> A;
(c_true : C_bool) = A => t => f => t;
(c_false : C_bool) = A => t => f => f;

// induction
I_bool b = (P : C_bool -> Type) -> P c_true -> P c_false -> P b;
i_true : I_bool c_true = P => p_t => p_f => p_t;
i_false : I_bool c_false = P => p_t => p_f => p_f;

// template
T_bool Rel = {
  comp : C_bool;
  ind : Ind_bool b;
  rel : Rel;
};

Bool = {
  comp : C_bool;
  ind : Ind_bool b;
  rel : Rel;
};

Bool = @Self((A : Type) => (self : T_bool A) => (x : T_bool A) -> self.comp == x.comp -> self == x);
true = %Self((self : Bool) => {
  comp = c_true;
  ind = i_true;
  rel = x => self_comp_eq_x_comp => (
    x_comp_eq_self_comp = symm self_comp_eq_x_comp;
    x_eq_self = x.rel self x_comp_eq_self_comp;
    symm x_eq_self
  );
});
false = %Self((self : Bool) => {
  comp = c_false;
  ind = i_false;
  rel = x => self_comp_eq_x_comp => (
    x_comp_eq_self_comp = symm self_comp_eq_x_comp;
    x_eq_self = x.rel self x_comp_eq_self_comp;
    symm x_eq_self
  );
});
ind_bool = b => (P : Bool -> Type) => (p_t : P true) => (p_f : P false) : P b => (
  (m, b_comp_eq_m_comp, p_m) =
    b.ind
      (x => {m : Bool; x_eq_m_comp : x == m.comp; p_m : P m})
      (true, refl, p_t)
      (false, refl, p_f);
  b_eq_m = b.rel b_comp_eq_m_comp;
  m_eq_b = symm b_eq_m;
  subst m_eq_b (x => P x) p_m
);


T[A := X]
T_bool ((x : Bool) -> self.comp == x.comp -> self == x)

rec%Bool (S : Bool) = (A : Type) -> (S == true -> A) -> (S == false -> A) -> A;
and%true : Bool true = A => t => f => t => t (refl true)
and%false : Bool false = A => t => f => ;

T_bool B T F (S : B) = (A : Type) -> (S == T -> A) -> (S == F -> A) -> A;
X = T_bool
Bool = @Self((A : Type) => (self : T_bool A) => (x : T_bool A) -> self.comp == x.comp -> self == x);
 B : Type -> Type  C : (A : Type) -> A -> Type
----------------------------------------------
    @Self(A : Type, x : B A, C A x) : Type

Bool = {
  comp : C_bool;
  ind : Ind_bool b;
  rel : (x : @Bool) => comp == x.comp -> @self == x;
};
rec%Fix = (self : N : Fix) -> (A : Never) -> A;
fix = (self : N : Fix) => self self;
```

## Compression

```rust
((A : Type\1) => (x : A\1) => x\1) Type\1
(A : Type\1) => ((x : A\2) => x\1)#-1;
((A : Type\1) => (x : A\1) => x\1)#+1

((x : A\0) => x\1)
((x : Type\1) => x\1)-1
(A : Type) => ((B : Type\1) => (x : B\1) => (x\1, A\3)) Type\2;
(A : Type) => (x : Type\1) => (x\1, A\2);

((A : \1) => (x : \1) => \1)#+1#-1

((A : \2) -> (x : ((A : \1) => (x : \1) => \1)#+2) -> ((A : \1) => (x : \1) => \1)#+3)#-1


((A : \1) -> ((x : \1) -> (\2 : (\1)#+2) : \2) : \1)#+1

((A : \1) -> ((x : \1) -> (\2) : \2) : \1)#+1

((A : \1 : (\0 : \0)#+1) -> ((x : \1 : (\1 : (\0 : \0)#+1)#+1) -> (\2 : (\1 : (\0 : \0)#+1)#+2) : \2) : \1)#+1
````

## Levels

```rust
((A : Type\1) => (x : A\1) => x\1) Type\1
```

## X

```rust

(x := Int; (1 : x))

(one : Int) = 1;
one_eq_1 : one == 1 = refl;
add = (a, b) => a + b;

(x = Int; (1 : x)) === (((x : Type) => (1 : x)) Int);

(x = print 1; (x, x));

(print 1, print 1);

(x : T = e1; e2) === (((x : T) => e1) e2)
(x : T := e1; e2) === (e2[x := e1]);

(x : T = e1; e2) === (((x : T) => e1) e2)
(x : T := e1; e2) === (e2[x := e1]);

x1 = (
  (a : Int, b : Int) = a_b;
  (c_d : (c : Int, d : a == c)) => (
    (c : Int, d : a == c) = c_d;
    a + b + c
  )
) (c = 1; d = d);

x1 = (
  (a, b) = a_b;
  c_d => (
    (c, d) = c_d;
    a + b + c
  )
) (c = 1; d = d);

// church encoding stucks
x1 =
  a_b (a => b =>
    c_d => c_d
      (c => d =>
        a + b + c)
  ) (k => k 1 d);

x1 = (
  (a, b) = a_b;
  c_d => (
    (c, d) = c_d;
    a + b + c
  )
) (c = 1; d = d);

x1 = (
  (a, b) = a_b;
  c_d => (
    (c, d) = c_d;
    a + b + c
  )
) (c = 1; d = d);

x1 = (
  (a, b) = a_b\1;
  c_d => (
    (c, d) = c_d\1;
    a\4 + b\3 + c\2
  )
) (c = 1; d = 2);

x1 = (
  (a, b) = a_b\1;
  c_d => (
    (c, d) = c_d\1;
    a\4 + b\3 + c\2
  )
) (c = 1; d = 2);

x1 = (
  (a, b) = a_b\1;
  c_d => (
    (c, d) = c_d\1;
    a\4 + b\3 + c\2
  )
);

x =>

((a, b) => c_d => (
  (c, d) = c_d\1;
  a\4 + b\3 + c\2
)) a_b\1 (c = 1; d = 2) [id]

((a, b) => c_d => (
  (c, d) = c_d\1;
  a\4 + b\3 + c\2
)) a_b\1 (c = 1; d = 2) [id]

BetaPair : (((x, y) => c) (a, b)) == c[y := b; x := a; ...id]
StuckPair : (((x, y) => c) z)[s] == ((x, y) => c[id; id; s]) z
```

## Next

## Linear Booleans

```rust
Unit = (A : Type) -> A -> A;
unit : Unit = A => x => x;

Pair (A : Type) (B : A -> Type) = (K : Type) -> ((x : A) -> B x -> K) -> K;
(pair A B x y) : Pair A B = K => k => k x y;

Bool = (A : Type) -> A -> A -> Pair A A;
true : Bool = A => x => y => pair A A x y;
false : Bool = A => x => y => pair A A y x;

Option A = (some : Bool, (T, _) = some Type A Unit; T);
some A (x : A) = (some = true, x);

dup_bool : Bool -> Pair (Pair Bool Bool) (Pair Bool Bool) =
  b => b (Pair Bool Bool) (pair A B true true) (pair A B false false);

Nat = (X : Type) -> ((succ : Bool, zero Type X Unit) -> X) -> X;
zero : Nat = X => k => k (succ = false, unit);
succ : Nat -> Nat = n => X => k => n X k (succ = true, )

Nat = (A : Type) -> A -> ((K : Type) -> A -> A) -> Pair (A -> A) (A -> A);
zero : Nat = A => z => s => Pair (A -> A) (A -> A) z s;
succ : Nat -> Nat = n => A => z => s => s (n)

List A = X -> (((K : Type) -> (A -> X -> K) -> (Unit -> K) -> K) -> X) -> X;
(cons A el tl) : List A = X => k => k (K => cons => nil => cons )
(nil A) : List A = X => k => k (K => cons => nil => nil unit);

dup_list = (A : Type) => (l : List A) => ();

```

## De-bruijin two

```rust
one =0> #0;
one =1> #1;

two => one => ((x => y => x\1 + two\3) one\0)
one => ((x => y => x#|1) one#1|0)

one => (y => one\1)

Int -> _A\0 -> _A\1;
Int -> {A} -> A -> A;

((id : (A : Type) -> (x : A) -> A) = A => x => x; id)
        : (A : Type) -> (x : A) -> A)

( := ) : Ref A -> A -> Ref A;

add = (a, b) => a + b;


x = x := 1;

((fix $ 1 : _) => fix fix) ((fix $ 1 : _) => fix fix);

Nat N = (X : Type $ 0) -> (((R : Type $ 0) -> (X -> R) $ N -> R $ 1) $ 1 -> X) -> X;

two = (A : Type) => (z $ 1 : A) => (s $ 2 : A -> A) => s (s x)

double (x : Socket) =
  (l : Int $ 1, r) = x;
  (l : Int $ 1, r) = x;
  x + x;

double (x : Pair (Int $ 1) Int) =
  (l : Int $ 1, r) = x;
  (l : Int $ 1, r) = x;
  x + x;

```

## Modality

```rust
zero = x => f => x;
succ = n => x => f => f (n x f);
one = x => f => f x;
two = x => f => f (f x);

zero = k => z => s => k z;
succ = n => k => z => s => s k z n;
one = k => z => s => s k z @@ k => z => s => k z;
two = k => z => s => z => s k z @@ k => z => s => z k s @@ k => z => s => k z;

rec%Nat = (K : Type) -> (A : Type) -> (k : A -> K) -> (z : A) ->
  (s : (k : A -> K) -> (z : A) -> (p : Nat) -> K) -> K;

zero : Nat = K => A => k => z => s => k z;
succ (n : Nat) : Nat = K => A => k => z => s => s k z n;

Id = (A1 : Type) -> (A2 : Type) -> (A1_eq_A2 : A1 == A2) -> (x : A) -> A;

Id = (A $ 2 : Type) -> (x : A) -> A;

id = (A : Type) => (x : A)

// pure system F
Nat X = (K : Type) -> (A : Type) -> (k : A -> K) -> (z : A) ->
  (s : (k : A -> K) -> (z : A) -> (p : X) -> K) -> K;
Nat_m = (X : Type) -> (Nat X -> X) -> X;
Nat_n = Nat Nat_m;

zero : Nat_n = K => A => k => z => s => k z;
in_ : Nat_n -> Nat_m = n => X => w => ;
succ : Nat_n -> Nat_n = n => K => A => k => z => s => s k z (in_ n);


// quantitative
rec%Nat Q = (K : Type) -> (A : Type) -> (k : A -> K) -> (z : A) ->
  (s $ Q : (k : A -> K) -> (z : A) -> (p : Nat Q) -> K) -> K;

id = (A $ 0 : Type) => (x : A $ 1) => x;
y = ((x $ Omega) => x x) ((x $ Omega) => x x)
double = (x $ 2 : Int) => x + x;

Nat Q = (A : Type) -> (z : A) -> (s $ Q : A -> A) -> A;
zero : Nat 0 = A => z => s => z;
succ Q (n $ 1 : Nat Q) : Nat (Q + 1) = A => z => s => s (n A z s);

Z : (self : A -> B) -> A -[Z]> B;

Level : SLevel
Grade : SGrade

(A : Type (L, G))

Type (L : Level, G : Grade) : Type (L + 1, 0);
Erasable (L : Level) = Type (L, 0);
Linear (L : Level) = Type (L, 1);

Nat (G : Grade) : Type (1, G);

(x : Nat : Erasable) =>

----------------
(x : A) -> B

----------------
(x $ 0 : ) -> B

id : A. (x : A) -> A
Pair ()

Nat = (Q : Quantity, Nat Q);
zero : Nat 0 = K => A => k => z => s => k z;
succ Q (n $ 1 : Nat Q) : Nat (Q + 1) = K => A => k => z => s => s k z n;

// TODO: this Q is wrong internally
dup_nat = Q => K => fix%(self => k => (l, r) => (n : Nat Q) =>
  n K (Nat Q, Nat Q) k (succ l, succ r) self);

Either A B =
  | (tag = "left", x : A)
  | (tag = "right", x : B);
```

## Match on Types

```rust
// staged compilation
```

## Implicits

```rust
// boxing
f = (x : Nat & x >= 1) => x;
{x : Int & x >= 0}

l.1 == r.1 -> l == r?

// overloading
%( == ) :
  (( == ) : {A} -> (x : A) -> (y : A) -> Type) &
  (( == ) : {A : Eq} -> (x : A) -> (y : A) -> x == y);

( == ) : {A} -> (x : A) -> (y : A) -> Type;
%( == ) = {R} =>
  (R, refl : R == R)
  | (Type, eq : R == Type) => ( == )

( == ) = %( == );

Monad = {
  pure : {A} -> (x : A) -> @M A;
  bind : {A} -> (m : @M A, f : (x : A) -> @M B) -> @M B
};

map = (m, f) => bind(m, x => pure(f(x)));
map = {M : Monad} => (m, f) => M.bind(m, x => M.pure)

x = map {Option}
map = {M : _M_T}

C_bool = (A : Type) -> A -> A -> A;
c_true : C_bool = t => f => t;
c_false : C_bool = t => f => f;

Ind_bool : C_bool -> Type =
  b => (P : Bool) -> P c_true -> P c_false -> P b;
ind_true : Ind_bool c_true = t => f => t;
ind_false : Ind_bool c_false = t => f => f;

C_bool = (A $ 0 : (tag = "c", Type)) -> (snd A) -> (snd A) -> (snd A);
c_true : C_bool = A => t => f => t;
c_false : C_bool = A => t => f => f;

I_bool : C_bool -> Type =
  b => (P $ 0 : (tag = "i", Bool -> Type)) -> P c_true -> P c_false -> P b;

Bool = (b : C_bool) & I_bool b;

(T $ 0 : (tag = "i", Bool -> Type) | (tag = "c", Type)) ->
 (P | (tag = "i", P) => P c_true | (tag = "c", A) -> A) ->
 (P | (tag = "i", P) => P c_false | (tag = "c", A) -> t) ->
 (P | (tag = "i", P) => P c_true | (tag = "c", A) -> A) <= (b : C_bool) & I_bool b;

(T $ 0 : (tag = "i", Bool -> Type)) -> (snd T) c_true -> (snd T) c_false -> (snd T) c_true <=
(P $ 0 : (tag = "i", Bool -> Type)) -> (snd P) c_true -> (snd P) c_false -> (snd P) c_true

(T $ 0 : (tag = "c", Type)) -> (snd T) -> (snd T) -> (snd T) <=
(A $ 0 : (tag = "c", Type)) -> (snd A) -> (snd A) -> (snd A);

Either A B = (K : Type) -> (A -> K) -> (B -> K) -> K;
true :
  (T $ 0 : (tag = "i", Bool -> Type) | (tag = "c", Type)) =>
  (t : P | (tag = "i", P) => P c_true | (tag = "c", A) -> A) =>
  (f : P | (tag = "i", P) => P c_false | (tag = "c", A) -> f) =>
  t;


Bool : Type = (b : C_bool) & Ind_bool b;
true : Bool = (b = c_true & ind_true);
false : Bool = (b = c_false & ind_false);

// resolution


```

## Church Induction

```rust
b_true : (A $ 0 : Type) -> (B $ 0 : Type) -> A -> B -> A = A => B => t => f => t;
b_false (A $ 0 : Type) -> (B $ 0 : Type) -> A -> B -> B = A => B => t => f => f;

C_bool = (A $ 0 : Type) -> A -> A -> A;
I_bool : C_bool -> Type =
  b => (P $ 0 : Bool -> Type) ->
    P ((A : Type) -> b_true A A) ->
    P ((A : Type) -> b_false A A) ->
    P b;




Bool = (b : C_bool) & I_bool b;
true : Bool = b_true;
false : Bool = b_false;

[x | x => x -A | x => ]
[b_true
  | (A : Type) -> b_true A A
  | (P : Bool -> Type) -> b_true (P ((A : Type) -> b_true A A)) (P ((A : Type) -> b_false A A))
];

b_true : (A $ 0 : Type) -> (B $ 0 : Type) -> A -> B -> A = A => B => t => f => t;
b_false (A $ 0 : Type) -> (B $ 0 : Type) -> A -> B -> B = A => B => t => f => f;

C_bool = (A $ 0 : Type) -> A -> A -> A;
I_bool  =
  (A : Type) => (P : Bool -> Type) =>
    P (b_true A A) ->
    P (b_false A A) ->
    P b;
Bool =
  (A $ 0 : Type) ->
  (P $ 0 : (A -> A -> A) -> Type) ->
  (b : A -> A -> A) &
  (P (b_true A A) -> P (b_false A A) -> P b);

(A : Type) -> b_true &(A; b => P (b A))

(b : (B $ 0 : Type) -> A -> B -> A) & ((B $ 0 : Type) -> P (b A A) -> B -> P (b A A))

S x =
  | (y $ 0 : A) -> S
  | (S x) -A
  | x;

[x | i = ]


[b_true
| (A : Type) -> b_true A A (P : (A -> A -> A) -> Type) -> b_true ]

true : Bool = A => P => (
  x
  x = (A : Type) -> b_true &(A | _ => P (b_false A A));

);
((A $ 0 : Type) -> A -> A -> A) &
((A $ 0 : Type) -> A -> P (b_false A A) -> A)

((B $ 0 : Type) -> A -> B -> A) &
((B $ 0 : Type) -> (P (b_true A A)) -> B -> (P (b_true A A)))

(A $ 0 : Type) -> (B $ 0 : Type) -> A -> B -> A <: (b : C_bool) & I_bool b
  _ <: (A $ 0 : Type) -> A -> A -> A
  _ <: (P $ 0 : Bool -> Type) -> P (b_true :> Bool) -> P (b_false :> Bool) -> P (b_true :> Bool)

```

## Grande e Funções

```rust
rec%Term =
  | Var (x : String)
  | Lambda (param : String) (body : Term)
  | Apply (lambda : Term) (arg : Term);

rec%List A =
  | []
  | A :: List

rec%Tree A =
  | Leaf A
  | Node (Tree A, Tree A);

Node (Node (Leaf 1, Leaf 2), Node (Left 3, Leaf 4))
  - Node (Leaf 1, Leaf 2)
    - Leaf 1
    - Leaf 2
  - Node (Left 3, Leaf 4)
    - Leaf 3
    - Leaf 4

rec%fold = (f, acc, l) =>
  l
  | [] => []
  | el :: tl => (
    acc = f(acc, el);
    fold(f, acc, tl)
  );

rec%fold_right = (f, acc, l) =>
  l
  | [] => []
  | el :: tl => (
    acc = fold_right(f, acc, tl);
    f(acc);
  );

sum = (l) => fold((acc, el) => acc + el, 0, l);
rev = (l) => fold((acc, el) => el :: acc, [], l);
map = (f, l) => (
  rev_f_l = fold((acc, el) => f(el) :: acc, [], l);
  rev(rev_f_l)
);

rec%map = (f, l) =>
  l
  | [] => []
  | el :: tl => f(el) :: map(f, tl);

rec%map = (f, l) =>
  l
  | [] => []
  | el :: tl => map_cons(f, el, tl)
and%map_cons = (f, el, tl) => f(el) :: map(f, tl);

map_cons = map => (f, el, tl) => f(el) :: map(f, tl);
map_base = map => (f, l) => l | [] => [] | el :: tl => map_cons(map)(f, el, tl)

map_base = map => (f, l) => l | [] => [] | el :: tl => map(f, el, tl)

Y = f => (x => f(x(x)))(x => f(x(x)));
loop = (x => x(x))(x => x(x));
loop = (x => x(x))(x => x(x));
loop = (x => x(x))(x => x(x));

map = (x => map_base(x(x)))(x => map_base(x(x)));
map = map_base((x => map_base(x(x)))(x => map_base(x(x))));
map = map_base(map_base((x => map_base(x(x)))(x => map_base(x(x)))));

rec%map = (f, l) => map_base(map, f, l);

rec%Many A = (G : Grade) -> (A G, Many A);

rec%Fix = Fix $ Many -> Unit;

Int : G. Type (1, G);

{A} -> {M : Monad} -> M A -> M B;
f = G. (x : Int) => (
  g : Unit -> Int = () => x;
  (g(), g())
);

Type L G : Type (L + 1, 0)

f = (x : $Int) => (x, x : Int $ 0);

f = (A : Type) => A;
f = x => (
  (x1, x) = x;
  x1(x)
);

(x : (A : Type) -> A)
```

## Recursion

```rust
rec%Fix = (A : Type) -> (B : Type) -> (A -> B) -> A -> B;

%fix : (Type -> Type) -> Type;
fix = (A : Type) => (B : Type) => (f : %fix(K => ))
fix = (A : Type) => (f : %fix(K => K -> A)) : A => f(f)

%fix(Self => Self -> T) ===

%Mu : (W : Type -> Type) -> Type;
%mu : (W : Type -> Type) -> Mu W == W (Mu W);

%Mu(Self => Self -> T) == (%Mu(Self => Self -> T)) -> T



-------------------------------
Y : (A : Type) -> (A -> A) -> A

Equal A (x : A) (y : A) = (P : _) -> P x -> P y;

Bool = (H : )
```

## Induction

```rust
T_bool = Type -> Type -> Type;
t_true : T_bool = t => f => t;
t_false : T_bool = t => f => f;

Bool (H : T_bool) = (A : Type) -> (H == t_true -> A) -> (H == t_false -> A) -> A
true : Bool t_true = A => t => f => t refl;
false : Bool t_false = A => t => f => f refl;

ind_bool
  : (H : T_bool) -> (b : T_bool H) -> (P : T_bool -> Type) -> P t_true -> P t_false -> P H =
  b => P => p_t => p_f =>
  b p_t p_f

TRec F = (X : Type) ->
Rec F = (X : Type) -> (F X -> X) -> X;
M = Rec (X => (A : Type) -> A -> (X -> A) -> A);
N = Rec M;

T_nat = Type -> (Type -> Type) -> Type
t_zero : T_nat = z => s => z;
t_succ : T_nat -> T_nat = pred => z => s => s (pred z s);
t_add : T_nat -> T_nat -> T_nat = n => m => z => s => n (m z s) s;

t_add (t_succ n_t_pred) m_t == (z => s => s (n_t_pred (m_t z s) s))
t_succ (t_add n_t_pred m_t) == (z => s => s (n_t_pred (m_t z s) s))


rec%Nat (H : T_nat) =
  (A : Type) ->
  (H == t_zero -> A) ->
  ((t_pred : T_nat) -> H == t_succ t_pred -> Nat t_pred -> A) ->
  A;
zero : Nat t_zero = A => z => s => z refl;
succ : (t_pred : T_nat) -> (pred : Nat t_pred) -> Nat (t_succ t_pred) =
  t_pred => pred => A => z => s => s t_pred refl pred);
rec%add
  : (n_t : T_nat) -> Nat n_t -> (m_t : T_nat) -> Nat m_t -> Nat (t_add n_t m_t)
  = n_t => n => m_t => m => A => z => s =>
    n (Nat (t_add n_t m_t))
      (n_t_eq_t_zero => m)
      (n_t_pred => n_t_eq_t_succ_n_t_pred => pred =>
        x : Nat (t_add n_t_pred m_t) = add n_t_pred pred m_t m;
        x : Nat (t_succ (t_add n_t_pred m_t)) = succ x;
        t_add n (t_succ m));

Never = (A : Type) -> A;
Unit = (A : Type) -> A -> A;
unit : Unit = A => x => x;
z_neq_s_z (eq : t_zero == t_succ t_zero) =>
  eq (n => n Unit (_ => Never)) unit;

rec%T_nat = Type -> (T_nat -> Type) -> Type;
t_zero : T_nat = z => s => z;
t_succ : T_nat -> T_nat = pred => z => s => s pred;
rec%t_add : T_nat -> T_nat -> T_nat = n => m => z => s =>
  n m (n => t_add n (t_succ m));

rec%Nat (H : T_nat) =
  (A : Type) ->
  (H == t_zero -> A) ->
  ((t_pred : T_nat) -> H == t_succ t_pred -> Nat t_pred -> A) ->
  A;
zero : Nat t_zero = A => z => s => z refl;
succ : (t_pred : T_nat) -> (pred : Nat t_pred) -> Nat (t_succ t_pred) =
  t_pred => pred => A => z => s => s t_pred refl pred;


```

## Nat

```ocaml
let f (type a) (ty : a ty) (x : a) =
  match ty with Int -> string_of_int (x : int) | String -> (x : string)

type 'x nat_case' = { case : 'r. 'r -> ('x -> 'r) -> 'r }
type nat_iter = { iter : 'x. ('x nat_case' -> 'x) -> 'x }
type nat_case = nat_iter nat_case'

let zero_iter : nat_iter = { iter = (fun f -> f { case = (fun z _s -> z) }) }

let succ_iter : nat_iter -> nat_iter =
 fun n ->
  {
    iter =
      (fun (type x) (f : x nat_case' -> x) : x ->
        f
          { case =
            (fun (type r) (_z : r) (s : x -> r) : r -> s (n.iter f)) });
  }

let in_ : nat_case -> nat_iter = fun n -> n.case zero_iter succ_iter
let zero_case : nat_case = { case = (fun z _s -> z) }

let succ_case : nat_case -> nat_case =
 fun n -> { case = (fun _z s -> s (in_ n)) }

let out : nat_iter -> nat_case =
 fun n -> n.iter (fun f : nat_case -> f.case zero_case succ_case)

let case (n : nat_case) z s = n.case z (fun pred -> s (out pred))
let iter (n : nat_case) z s = (in_ n).iter (fun n -> n.case z s)
let to_int (n : nat_case) = (in_ n).iter (fun n -> n.case 0 (fun n -> n + 1))

let () =
  Format.printf "%d\n%!" (to_int (succ_case (succ_case (succ_case zero_case))))
```

## Subtyping

```rust
A | B

A :> A | B
B :> A | B

A <: A | B
B <: A | B

A | B <: C
  A <: C && B <: C

C <: A | B
  C <: A || C <: B

(tag : true, Int) | (tag : false, String) <: (tag : Bool, tag Type Int String)

1 * 0 = 2 * 0
0 = 0
(1 * 0) / 0 = (2 * 0) / 0
1 = 2


(n * x) / x && x <> 0 == n

(1 * 0) / 0 = (2 * 0) / 0

(L : Level) -> (A : Type L, x : A)

```

## ADTs

```rust
Nat : {
  @Nat : Type;
  zero : Nat;
} = {
  @Nat = Int;
  zero = 0;
};

(==) :
  ((==) : {A} -> (x : A) -> (y : A) -> Type) &
  {A : Eq} -> (x : A) -> (y : A) -> Option (x == y);


zero : Nat = Nat.zero;

f = (x : Int & x >= 0) => (x : Int);

S = {x : T | P x}

Nat = {x : Z | x >= 0};

Either A B =
  | { tag = true; payload : A; }
  | { tag = false; payload : B; };

M : {
  T : Type;
  x : T;
} = {
  T = Int;
  x = 1;
};
```

## Induction of Union

```rust
Id = (A : Type) -> A -> A;
id : Id = (A : Type) => (x : A) => x;

Bool = (A : Type) -> A -> A -> A;
true : Bool = (A : Type) => (x : A) => (y : A) => x;
false : Bool = (A : Type) => (x : A) => (y : A) => y;

Interval : UI;
Level : UL;
Grade : UG;

data%Bool = true | false;

ind%Eq (A : Type) (x : A) (y : A) : Type =
  | refl : x == y;
Eq_refl : (A : Type) -> (x : A) -> Eq A x x;
Eq_ind : (A : Type) -> (x : A)

: (A : Type) -> (x : A) -> (P : (y : A) -> id x y -> Type) ->
       P x (refl x) ->
       ((y : A) -> P y (nope x y)) ->
       (y : A) -> (eq : id x y) -> P y eq

ind%Nat =
  | O
  | S (pred : Nat);

ind%Bool = | true | false;
Bool_ind :
  (P : Bool -> Type) ->
  (p_true : P true) ->
  (p_false : P false) ->
  (b : Bool) -> P b;
Bool_ind : (b : Bool, P : Bool -> Type, _ : P true, _ : P false) -> P b;

id<A>(x : A) = x;
Either(A, B) =
  | (tag = true, x : A)
f = (x : Int | String) => (y : Int, x_eq_int : x == y) => (

)

f x == f y -> x == y
X B k == Y B k -> X A (x => x) = Y B (x => x)

X : (B : Type) -> (A -> B) -> Type;
k : A -> B


x => k (f x)
x => (f x)
```

## Induction

```rust
// compute
fold : Bool -> (A : Type) -> A -> A -> A;
// induction
case :
  (b : Bool) -> (P : Bool -> Type) ->
  P true -> P false -> P b;



Eq A x y = (P : A -> Type) -> P x -> P y;

rec%Bool =
  (A : Type) -> (P : Bool -> Type) -> P true -> P false -> P

(Eliminating a boolean B) shows that the boolean B is true or false.

Bool : Type = self%(b => (P : Bool -> Type) -> P true -> P false -> P b);
true : Bool = _;
false : Bool = _;
ind : (b : Bool) -> (P : Bool -> Type) -> P true -> P false -> P b = _;

Unit = (A : Type) -> A -> A;
unit : Unit = A => x => x;

Bool A T F = (H : A, ind : (P : A -> Type) -> P T -> P F -> P H);

Bool_0 = Bool Unit unit unit;
true_0 : Bool_0 = (H = _, ind = P => p_t => p_f => p_t);
false_0 : Bool_0 = (H = _, ind = P => p_t => p_f => p_f);

Bool_1 = Bool Bool_0 true_0 false_0;
true_1 : Bool_1 = (H = _, ind = P => p_t => p_f => p_t);
false_1 : Bool_1 = (H = _, ind = P => p_t => p_f => p_f);

Bool_2 = Bool Bool_1 true_1 false_1;
true_2 : Bool_2 = (H = _, ind = P => p_t => p_f => p_t);
false_2 : Bool_2 = (H = _, ind = P => p_t => p_f => p_f);

C_nat = (A : Type) -> A -> (A -> A) -> A;
c_zero : C_nat = A => z => s => z;
c_succ : C_nat -> C_nat => n => A => z => s => s z;

C_bool = (A : Type) -> A -> A -> A;

Bool (n : C_nat) =
  n Type C_bool (A => (H : A, (P : A -> Type) -> P T -> P F -> P H));

case_2
  : Bool_2 -> (A : Type) -> A -> A -> A
  = (b : Bool_2) => (A : Type) => fst b (_ => A);
ind_2
  : (b : Bool_2) -> (P : _ -> _) -> P true_1 -> P false_1 -> P (fst b)
  = (b : Bool_2) => snd b;
```

> Qual a melhor linguagem de programação?

- JavaScript
- PHP
- Python

> JavaScript é a melhor linguagem de programação?

- Sim
- Não
  > Então Python é a melhor linguagem de programação?

> Qual a mãe de todas as linguagens de programação?

> Qual a melhor linguagem de programação?
> Por que JavaScript não é a melhor linguagem de programação?

> Qual a melhor forma de conseguir o primeiro emprego?

> Qual a forma você recomenda para conseguir o primeiro emprego?

> Estou programando a 6 meses, qual a forma você recomenda para eu conseguir o primeiro emprego?

> Como eu aprendo JavaScript?

- A: Ler livros sobre JavaScript
- B: Entrar em comunidades de JavaScript
- C: Ler a wikipedia sobre JavaScript
- D: Ver video no youtube sobre JavaScript
- E: Fazer um curso de 3k de JavaScript

> Por que eu não aprendi JS com o livro X?

> Por que os metodos acima não funcionaram?

-

- A: Procuro no Google sobre JavaScript

> Por que eu não consigo encontrar uma hipotese?

- Eu não tenho contexto o suficiente

> Por que eu não consigo solucionar esse problema em X?

- A: Eu não sei X o suficiente

> Por que eu não tenho uma hipotese?

### Data Macro

```rust
data%Bool =
  | true
  | false;

pair A B (x : A) (y : B x) =
  (K : Type) => k => k x y;

Sigma A B =
  %self(Sigma, s).
    (P : Sigma -> Type) ->
    ((x : A) -> (y : B x) -> P (%fix(Sigma, s). P => k => k x y)) ->
    P s
Sigma A B =
  %self(Sigma, s).
    (P : Sigma -> Type) ->
    ((x : A) -> (y : B x) -> P (%fix(Sigma, s). P => (k : _ -> _ -> P s) => k x y)) ->
    P s;

%fix(Bool, b) =>
Bool =
  %self(Bool, b).
    (P : Bool -> Type) ->
    P (%fix(Bool, b). P => ())

sigT_rect
	 : forall (A : Type) (P : A -> Type) (P0 : {x : A & P x} -> Type),
       (forall (x : A) (p : P x), P0 (existT P x p)) ->
       forall s : {x : A & P x}, P0 s


Unit = %fix(Unit). %self(u).
  (P : Unit -> Type) ->
    P (%fix(u). (P : Unit -> Type) => (x : P u) => x) ->
    P u;

%unroll(%fix(u). (P : Unit -> Type) => (x : P u) => x)
%self(u). (P : Unit -> Type) -> P (%fix(u). (P : Unit -> Type) => (x : P u) => x) -> P u
%self(u). (P : Unit -> Type) -> P (%unroll(%fix(u). (P : Unit -> Type) => (x : P u) => x)) -> P u

(x ) => x

@(x) ->
@(x) =>




Unit = %fix(Unit : Type). %self(u).
  (P : Unit -> Type) -> P u -> P u;

ctx = [];
fix0 = %fix(Unit). %self(u). (P : Unit -> Type) ->
  P (%fix(u). (P : Unit -> Type) => (x : P u) => x) -> P u;

ctx = [];
fix1 = %self(u). (P : fix0 -> Type) ->
  P (%fix(u). (P : fix0 -> Type) => (x : P u) => x) -> P u;


ctx = [u : fix1];
fix2 = (P : fix0 -> Type) ->
  P (%fix(u). (P : fix0 -> Type) => (x : P u) => x) -> P u;

ctx = [P : fix0 -> Type; u : fix1];
fix3 = P (%fix(u). (P : fix0 -> Type) => (x : P u) => x) -> P u;
  ctx = [P : fix0 -> Type; u : fix1];
  u0 : ? = %fix(u). (P : fix0 -> Type) => (x : P u) => x;
  u1 : %self(u). ? = (P : fix0 -> Type) => (x : P u0) => x;
  u2 : %self(u). (P : fix0 -> Type) -> ? = (x : P u0) => x;
  u3 : %self(u). (P : fix0 -> Type) -> P u0 -> P u0 = _;
  u0 : %self(u). (P : fix0 -> Type) -> P u0 -> P u0 = _;

  expected : fix0;
  received : %self(u). (P : fix0 -> Type) -> P u0 -> P u0;

  expected : %fix(Unit). %self(u). (P : Unit -> Type) ->
    P (%fix(u). (P : Unit -> Type) => (x : P u) => x) -> P u;
  received : %self(u). (P : fix0 -> Type) -> P u0 -> P u0;

  expected : %self(u). (P : fix0 -> Type) -> P u0 -> P u;
  received : %self(u). (P : fix0 -> Type) -> P u0 -> P u0;

Unit = %fix(Unit). %self(u).
  (P :
    (
      %self(u). (P : -> Type) ->
        P (%fix(u). (P : Unit -> Type) => (x : P u) => x) -> P u
    ) ->
    Type
  ) ->
  P (%fix(u). (P : Unit -> Type) => (x : P u) => x) ->
  P u;

%self(u). P

Γ, x : %self(x). T |- T : Type
------------------------------
%self(x). T : Type

Γ |- M[x := %fix(x). M] : T
-------------------------------
      %fix(x). M : T


%fix(u). (x : P u) => x

u : _A
(x : P u) -> _A
_A := %self(u). (x : P u) -> _A

%fix(u). (x : P u) => u : %self(u). (x : P u)

%fix(u : %fix(T). %self(u). (x : P u) -> T).
  (x : P u) => u

%self(u). (x : P u) -> %fix(T). %self(u). (x : P u) -> T
%self(u). (x : P u) -> %fix(T). %self(u). (x : P u) -> T

%fix(T). %self(u). (x : P u) -> T
%self(u). %fix(T). (x : P u) -> T

unit
  : %fix(Unit : Type). %self(u). (P : Unit -> Type) -> P u -> P u
  = %fix(u). (P : Unit -> Type) => (x : P u) => x

Unit = %fix(Unit : Type). %self(u : Unit). (P : Unit -> Type) ->
  P unit -> P u;

ctx = [];
s0 = %fix(Unit : Type). %self(u). (P : Unit -> Type) -> P unit -> P u;

s1 = %self(u). (P : s0 -> Type) ->
  P unit -> P u;

ctx = [u : s0]
s2 = (P : s0 -> Type) -> P unit -> P u;

ctx = [u : s0; P : s0 -> Type]
s3 = P unit -> P u;


expected : s0
received : %fix(Unit : Type). %self(u). (P : Unit -> Type) -> P u -> P u

expected : %fix(UnitA : Type). %self(u). (P : UnitA -> Type) -> P unit -> P u
received : %fix(UnitB : Type). %self(u). (P : UnitB -> Type) -> P u -> P u

s4 = P (%fix(u : s0). (P : s0 -> Type) => (x : P u) => x) -> P u;


expected : %fix(A). () -> A
received : %fix(B). () -> B

expected : %fix(C). () -> () -> A
received : %fix(C). () -> B

expected : () -> %fix(A). () -> A
received : () -> %fix(B). () -> B





Γ, x : Id |- T : Type
---------------------
%self(0, x. T) : Type

Γ, x : %self(i, x. T) |- T : Type
---------------------------------
%self(i + 1, x. T) : Type

Γ |- M[x := id] : T
---------------------
%fix(0, x : T. M) : T

Γ |- M[x := %fix(0, x : T. M)] : T
----------------------------------
%fix(i + 1, x : T. M) : T

%fix(i, Unit.
  %self(i, u. (P : Unit -> Type) ->
    P ((P : Unit -> Type) => (x : P u) => x) -> P u))

Unit = %fix(Unit). %self(u).
  (P : Unit -> Type) -> P ((P : Unit -> Type) => (x : P u) => x) -> P u;
unit : Unit = %fix(u). (P : Unit -> Type) => (x : P u) => x;

%fix(Unit). %self(u).
  (P : Unit -> Type) -> P ((P : Unit -> Type) => (x : P u) => x) -> P u


%fix(T). %self(u).


Γ, x : A |- B : Type  A === %self(x : A). B
-------------------------------------------
          %self(x : A). B : Type

Γ |- M[x := %fix(x : A). M] : B
--------------------------------
%fix(x : A). M : %self(u : A). B

%fix(u : Unit). (P : Unit -> Type) => (x : P u) => x

%fix(Unit : Type). %self(u : Unit).
  (P : Unit -> Type) ->
  P (%fix(u : Unit). (P : Unit -> Type) => (x : P u) => x) ->
  P u;



%fix(Unit : Type). %self(u : Unit).
  (P : Unit -> Type) ->
  P (%fix(u : Unit). (P : Unit -> Type) => (x : P u) => x) ->
  P u;

%fix(u) : %fix(Unit : Type). (P : Unit -> Type) -> P u -> P u.
  (P : Unit -> Type) => (x : P u) => x

%self(u : Unit). (P : Unit -> Type) -> P (%fix(u : Unit). (P : Unit -> Type) => (x : P u) => x) -> P u
%self(u : Unit). (P : Unit -> Type) -> P u -> P u;

%fix(T : Type). %self(u : T). (P : T -> Type) -> P u -> P u



%fix(Unit : Type). %self(u : Unit).
  (P : Unit -> Type) ->
  P (%fix(u : Unit). (P : Unit -> Type) => (x : P u) => x) ->
  P u;


%fix(False) : Type. %self(f). (P : !False -> Type) -> P f

%fix(False) : Type. %self(f : !False). (P : !False -> Type) -> P f

%fix(False) : Type.
  %self(f : (%self(f : False). (P : False -> Type) -> P f)). (P : !False -> Type) -> P f

(%self(f : False). (P : False -> Type) -> P f) ==
%self(f : False). (P : False -> Type) -> P f

!False <: False
f : (P : !False -> Type) -> P f

%fix(False) : Type. %self(f).
  (P : (%self(f). (P : False -> Type) -> P f) -> Type) -> P f
%self(f). (P : !False -> Type) -> P f


%fix(Unit) : Type. %self(u).
  (P : (%self(u). (P : Unit -> Type) -> P u -> P u) -> Type) ->
  P (
    %fix(u) : (P : Unit -> Type) -> P u -> P u.
      (P : Unit -> Type) => (x : P u) => x
  ) -> P u


%fix(u : )

() = check (1 + 1) 2;

%fix(Unit : Type). %self(u : Unit).
  (P : Unit -> Type) ->
  P (
    %fix(u) : (P : Unit -> Type) -> P u -> P u.
      (P : Unit -> Type) => (x : P u) => x
  ) -> P u

Either A B =
  | (left = true, content : A)
  | (left = false, content : B);

Either A B = (left : Bool, content : Bool Type A B);

%fix(Unit : Type). %self(u : Unit).
  (P : !Unit -> Type) ->
  P (%fix(u : Unit). (P : Unit -> Type) => (x : P u) => x) -> P u

%fix(Unit : Type). %self(u : Unit).
  (P : Unit -> Type) ->
  P (%fix(u : Unit). (P : Unit -> Type) => (x : P u) => x) -> P u


Γ, x : A |- B : Type  A === %self(x : A). B
-------------------------------------------
%self(x : A). B : Type

Γ |- M[x := %fix(x : A). M] : B  A === %self(x : A). B
------------------------------------------------------
%fix(x : A). M : A


Γ |- M[x := [%fix(x : A). M[!x := x]]] : B  A === %self(x : A). B
------------------------------------------------------
%fix(x : A). M : A

Γ |- M : %self(u : A). B
-------------------------------
%unroll(M) : B[u := %unroll(M)]

Unit = %fix(Unit : Type). %self(u : Unit). (P : Unit -> Type) ->
  P (%fix(u : Unit). (P : Unit -> Type) => (x : P u) => x) -> P u;


Unit = %fix(Unit : Type).
  %self(u : !Unit). (P : !Unit -> Type) -> P u -> P u;

!Unit === %self(u : !Unit). (P : !Unit -> Type) -> P u -> P u
![%self(u : !Unit). (P : !Unit -> Type) -> P u -> P u] === %self(u : !Unit). (P : !Unit -> Type) -> P u -> P u
%self(u : !Unit). (P : !Unit -> Type) -> P u -> P u === %self(u : !Unit). (P : !Unit -> Type) -> P u -> P u

Unit = %fix(Unit : Type).
  %self(u : %self(u : !Unit). (P : !Unit -> Type) -> P u -> P u).
    (P : (%self(u : !Unit). (P : !Unit -> Type) -> P u -> P u) -> Type) ->
    P u -> P u;

Unit = %fix(Unit : Type).
  %self(u : !Unit). (P : !Unit -> Type) -> P u -> P u;

Unit = %fix(Unit : Type).
  %self(u : !Unit). (P : !Unit -> Type) -> P u -> P u;
Unit = %fix(Unit : Type).
  %self(u : !). (P : !Unit -> Type) ->
  P u -> P u;

unit
  : %fix(Unit : Type). %self(u : Unit). (P : Unit -> Type) -> P u -> P u
  = %fix(u). (P : Unit -> Type) => (x : P u) => x;

Unit = %fix(Unit : Type). %self(u : Unit). (P : Unit -> Type) -> P unit -> P u;

%self(u : (%fix(Unit : Type). %self(u : Unit). (P : Unit -> Type) -> P unit -> P u)). (P : (%fix(Unit : Type). %self(u : Unit). (P : Unit -> Type) -> P unit -> P u) -> Type) -> P unit -> P u
%fix(Unit : Type). %self(u : Unit). (P : Unit -> Type) -> P u -> P u

unit = %fix(u : Unit). (P : Unit -> Type) => (x : P u) => x;

ind_unit = (P : Unit -> Type) => (p : (u : Unit) =>
  u
   (P : Unit -> Type) -> P unit ->

%fix(u : Unit). (P : Unit -> Type) => (x : P u) => x

%self(u : Unit). (P : Unit -> Type) ->
  P (%fix(u : Unit). (P : Unit -> Type) => (x : P u) => x) -> P u
===
%self(u : Unit). (P : Unit -> Type) ->
  P (%fix(u : Unit). (P : Unit -> Type) => (x : P u) => x) -> P u
%self(u : Unit).
  (P : Unit -> Type) ->
  P (%fix(u : Unit). (P : Unit -> Type) => (x : P u) => x) -> P u



%self(u : Unit). (P : Unit -> Type) -> P u -> P u

unit
  : %fix(Unit : Type). %self(u : Unit). (P : Unit -> Type) -> P u -> P u
  = %fix(u). (P : Unit -> Type) => (x : P u) => x;

Unit = %fix(Unit : Type). %self(u : Unit). (P : Unit -> Type) -> P unit -> P u;

Unit = %fix(Unit : Type). %self(u : Unit). (P : Unit -> Type) ->
  P (%fix(u : Unit). (P : Unit -> Type) => (x : P u) => x) -> P u;

%fix(true : Bool).
        (P : Bool -> Type) => (then : P true) =>
        (else : P (%fix(false : Bool).
          (P : Bool -> Type) =>
          (then : P (%fix(true). (P : Bool -> Type) => (then : P true) => (else : P false) => then)) =>
          (else : P false) => else
        )


Bool = %fix(Bool : Type). (
  %self(b : Bool). (P : Bool -> Type) ->
    P (%fix(true : Bool).
        (P : Bool -> Type) => (then : P true) =>
        (else : P (%fix(false : Bool). (P : Bool -> Type) => (then : P true) => (else : P false) => else)) =>
        then) ->
    P (%fix(false).
        (P : Bool -> Type) =>
        (then : P (%fix(true). (P : Bool -> Type) => (then : P true) => (else : P false) => then)) =>
        (else : P false) =>
        else)) ->
    P b
)

Unit = %fix(Unit : Type). %self(u : Unit). (P : Unit -> Type) ->
  P (%fix(u : Unit). (P : Unit -> Type) => (x : P u) => x) -> P u;

Unit = %fix(Unit : Type). %self(u : Unit). (P : Unit -> Type) ->
  P (%fix(u : Unit). (P : Unit -> Type) => (x : P u) => x) -> P u;


False = %fix(False : Type). %self(f : False). (P : False -> Type) -> P f;
False === %self(f : False). (P : False -> Type) -> P f

Sigma === %fix(Sigma) => A => B =>
  %self(s : Sigma A B). (P : Sigma A B -> Type) ->
    ((x : A) -> (y : B x) ->
      P (%fix(s : Sigma A B). (P : Sigma A B -> Type) =>
          (k : (x : A) -> (y : B x) -> P s) => k x y) ->
    P s;

%self(s : Sigma A B). (P : Sigma A B -> Type) ->
    ((x : A) -> (y : B x) ->
      P (%fix(s : Sigma A B). (P : Sigma A B -> Type) =>
          (k : (x : A) -> (y : B x) -> P s) => k x y) ->
    P s === %self(s : Sigma A B). (P : Sigma A B -> Type) ->
    ((x : A) -> (y : B x) ->
      P (%fix(s : Sigma A B). (P : Sigma A B -> Type) =>
          (k : (x : A) -> (y : B x) -> P s) => k x y) ->
    P s

False = %fix(False : Type). %self(f). (P : False -> Type) -> P f;
False === %self(f). (P : False -> Type) -> P f




expected : Unit
received : %self(u : Unit). (P : Unit -> Type) -> P u -> P u
alias : u === %fix(u : Unit). (P : Unit -> Type) => (x : P u) => x

expected : %self(u : Unit). (P : Unit -> Type) ->
  P (%fix(u : Unit). (P : Unit -> Type) => (x : P u) => x) -> P u
received : %self(u : Unit). (P : Unit -> Type) ->
  P (%fix(u : Unit). (P : Unit -> Type) => (x : P u) => x) -> P u


%fix(Y). Y -> ();

Unit = %fix(Unit : Type). %self(u : Unit). (P : Unit -> Type) ->
  P (%fix(u : Unit). (P : Unit -> Type) => (x : P u) => x) -> P u;

%fix(False : Type). %self(f : False). (P : False -> Type) -> P f;
False === %self(f : False). (P : False -> Type) -> P f

%self(f : False). (P : False -> Type) -> P f

False ===


%fix(False : Type). %self(f : False 1). (P : False 0 -> Type) -> P f;

Γ, x : A |- B : Type  A === %self(x : A). B
-------------------------------------------
%self(x : A). B : Type

Γ, x : X, r : %self(r). Eq X M x |- M : X
------------------------------------------
%fix(x : X, r). M : X

%fix(Unit : Type, eq). %self(u). (P : Unit -> Type) ->
  P (
    %fix(u : Unit, r).
      eq (T => T)
      (P : Unit -> Type) => (x : P u) => x))
  P (eq (T => T) u);

Eq A (x y : A) = (P : A -> Type) -> P x -> P y;
%fix(False : Type, eq).
  %self(f). (P : False -> Type) -> P (eq (T => T) f);


Unit : Type
unit : Unit;

Unit = %self(u). (P : Unit -> Type) -> P unit -> P u;
unit = (P : Unit -> Type) => (x : P unit) => x;

False : Type
False = %self(f). (P : False -> Type) -> P f;

Unit = %fix(False).
  %self(u). (P : Unit unit -> Type) -> P unit -> P u;

```

### Why?

The natural internalization of dependent elimination requires a term that depends on itself on the type level.

A term that depends on itself, is a term introduced by a fixpoint.

Such a fixpoint requires a type to represent the self dependency, we call that a self type.

```rust
// formation
Γ, x : %self(x). T |- T : Type
------------------------------
%self(x). T : Type
// introduction
Γ |- M[x := M] : T
---------------------------
%fix(x). M : %self(x). T
```

A self type variable can only appear in a type constructor position, as such the expected type of such constructor must be the self type itself.

Due to that, the self type usage requires a type fixpoint. But such a type needs to be checkable against the self variable itself.

```rust
// somehow (f : False), to allow (P f)
%fix(False). %self(f). (P : False -> Type) -> P f
```

Somehow control the internal expansion

```rust
False : Type
False = %self(f : False). (P : False -> Type) -> P f


Unit = %fix(Unit : Type). %self(u : Unit). (P : Unit -> Type) ->
  P (%fix(u : Unit). (P : Unit -> Type) => (x : P u) => x) -> P u;
unit : Unit = %fix(u : Unit). (P : Unit -> Type) => (x : P u) => x;

ind : (P : Unit -> Type) -> P unit -> (u : Unit) -> P u =
  (P : Unit -> Type) => (x : P unit) => (u : Unit) => u P x;
f = (u : Unit) : Eq Unit u unit =>
  u (u => Eq Unit u unit) eq_refl;

False = %fix(False : %self(False). (%self(f). False f) -> Type).
  (f : %self(f). False f) => (P : False f -> Type) -> P f;
False = %self(f). False f;


Unit =
  %fix(Unit : %self(Unit).
    (u : %self(u). (unit : %self(unit). Unit u unit) -> Unit u unit) ->
    (unit : %self(unit). Unit u unit) -> Type
  ). u => unit => (P : Unit u unit -> Type) -> P unit -> P (u unit);
unit = %fix(u). (unit : %self(unit). Unit u unit) ->

Unit =
  %fix(Unit : %self(Unit).
    (unit : %self(unit). (u : %self(u). Unit unit u) -> Unit unit u) ->
    (u : %self(u). Unit unit u) -> Type
  ). unit => u => (P : Unit unit u -> Type) -> P (unit u) -> P u;

unit : %self(unit). Unit (_ => unit) unit =
  (P : Unit (_ => unit) unit -> Type) => (x : P unit) => x;


Unit = %self(u).
  Unit u (%fix(unit : ) : (P : Unit u unit -> Type) => (x : P unit) => x);

Unit =
  %fix(Unit : %self(Unit).
    (unit : %self(unit). (u : %self(u). Unit unit u) -> Unit unit u) ->
    (u : %self(u). Unit unit u) -> Type
  ). unit => u => (P : Unit unit u -> Type) -> P (unit u) -> P u;

x = (f : False) =>

Unit = %fix(Unit : %self(Unit). (%self(unit). Unit unit) -> Type).
  (unit : %self(unit). Unit unit) =>
    (u : Unit unit) => (P : Unit unit -> Type) -> P unit -> P u;
unit = %fix(unit : %self(unit). Unit unit).
  (u : Unit unit) => (P : Unit unit -> Type) => (x : P unit) => x;

%fix(False).
  (eq : Eq Type (False eq) (%self(f : False eq, eq). (P : False eq -> Type) -> P f)) =>
    %self(f : False eq, eq). (P : False eq -> Type) -> P f

// introduction
Γ, f : {f | x : A} -> B, x : A |- B : Type
------------------------------------------
      {f | (x : A) -> B} : Type

{u | (P : _) -> P {u | P => x => x} -> P u}
```

### External Constructors

```rust
Unit =
  %fix(Unit : %self(Unit). (%self(u). Unit u) -> Type).
    u => (P : Unit unit u -> Type) -> P u -> P u;
unit = %fix(u : Unit u). (P : Unit u -> Type) => (x : P u) => x;

fix%Unit (u : %self(u). Unit u) : Type = (P : Unit u -> Type) -> P u -> P u;
fix%unit : Unit unit = (P : Unit u -> Type) => (x : P unit) => x


Unit : (u : %self(u). Unit u) -> Type;
unit : %self(u). Unit u;

Unit = u => (P : (%self(u). Unit u) -> Type) -> P unit -> P u;
unit = %fix(unit) : Unit unit. (P : (%self(u). Unit u)) => (x : P unit) => x;

fix%Unit (u : %self(u). Unit u) : Type = (P : Unit u -> Type) -> P u -> P u;

%self(unit). (u : %self(u). Unit unit u) ->
  (P : Unit unit u -> Type) -> P (unit u) -> P (u => (P : Unit unit u -> Type) => (x : P (unit u)) => x))
unit = %fix(u). (unit : %self(unit). Unit u unit) ->


Unit = %fix(Unit : %self(Unit). (%self(u). Unit u) -> Type).
  (u : %self(u). Unit u) => (P : (%self(u). Unit u) -> Type) -> P u;

False = %fix(False : %self(False). (%self(f). False f) -> Type).
  (f : %self(f). False f) => (P : (%self(f). False f) -> Type) -> P f;
False = %self(f). False f;

Unit = %fix(Unit) : (unit : %self(u). Unit u u) -> (%self(u). Unit unit u) -> Type.
  unit => u => (P : (%self(u). Unit unit u) -> Type) -> P unit -> P u;
unit : %self(u). Unit u u = %fix(u) : Unit u u.
  (P : (%self(u). Unit u u) -> Type) => (x : P u) => x;
Unit = %self(u). Unit unit u;


unit : Unit = unit;

Bool = %fix(Bool) :
  (true : %self(true).
    (false : %self(false). Bool (true false) false false) -> Bool (true false) false (true false)) ->
  (false : %self(false). Bool (true false) false false) ->
  (%self(b). Bool (true false) false b) -> Type.
  true => false => b =>
    (P : (%self(b). Bool (true false) false b) -> Type) -> P (true false) -> P false -> P b;
true : %self(true). (false : %self(false). Bool (true false) false false) -> Bool (true false) false (true false)
  = %fix(true). false => (then : P (true false)) => (else : P false) => then;
false : %self(false). Bool (true false) false false
  = %fix(false). (then : P (true false)) => (else : P false) => else;
Bool = %self(b). Bool (true false) false b;
true : Bool = true false;
false : Bool = false;





%self(False). (%self(f). False f) -> Type;

False : %self(f). False f -> Type


Unit : %self(Unit). (u : %self(u). Unit u) -> Type;
unit : %self(u). Unit u;

Unit = u => (P : %self(u). Unit u) -> P unit -> P u;
unit = P => x => x;

// tie the knot
Unit = %self(u). Unit u;


fix%False (f : %self(f). False f) : Type
  = (P : (%self(f). False f) -> Type) -> P f;
False = %self(f). False f;


unit : %self(unit). Unit unit (%unroll unit)

%unroll unit : Unit unit (%unroll unit)

unroll Unit = (unit : %self(u). Unit u u) : %self(u). Unit unit u =>

  %fix(u) : Unit unit u. unit : Unit unit unit;


fix%Unit (unit : %self(unit). Unit unit unit) (u : %self(u). Unit unit u) : Type
  = (P : (%self(unit). Unit unit u) -> Type)-> P unit -> P u;
fix%unit : Unit unit (%unroll unit) =
  (P : (%self(u). Unit unit u) -> Type) => (x : P (%unroll unit)) => x;
Unit = %self(u). Unit unit u;

%self(unit). Unit unit (%unroll unit)
%self(unit). Unit unit (%unroll unit)

%self(unit). (P : (%self(u). Unit unit u) -> Type) -> P (%unroll unit) -> P (%unroll unit)

fix%Unit (unit : %self(unit). Unit unit unit)
  (u : %self(u). Unit unit u) : Type
  = (P : (Unit unit u) -> Type) -> P (%unroll unit) -> P (%unroll u);

%self(u). Unit unit unit

%self(unit). Unit unit unit

fix%unit : %self(unit). Unit unit (%unroll unit) = %fix(u) : Unit unit u.
  (P : (Unit unit u) -> Type) => (x : P (%unroll unit)) => x;


(P : (%self(u). Unit unit u) -> Type) -> P (%unroll unit) -> P (%unroll unit)
(P : (%self(u). Unit unit u) -> Type) -> P (%unroll unit) -> P u



Γ, x : %self(x). T |- T : Type
------------------------------
%self(x). T : Type

Γ, x : %self(x). T |- M : T
----------------------------
%fix(x) : T. M : %self(x). T

Γ |- M : %self(x). T
---------------------
%unroll M : T[x := M]

False : _A
  f : %self(f). _B
    _A := _C -> Type;
    _C := (%self(f). _B);
    _B := False f;
  f : %self(f). False f
False : (f : %self(f). False f) -> Type



False : lazy ((f : %self(f). False f) -> Type)
False : (f : lazy (%self(f). False f)) -> lazy Type
  f : lazy (%self(f). False f)
    // False f
    False : (f : lazy (%self(f). False f)) -> lazy Type



%fix(False) : (f : %self(f). False f) -> Type. Type;

%fix(Unit): (unit : %self(unit). Unit unit (%unroll unit)) (u : %self(u). Unit unit (%unroll u))
   = Type;
Unit : _A;
_A := (unit : _B) -> (u : _C) -> Type
  unit : _B
  _B := %self(unit). _D
  (%unroll unit : _C)

(unit : _A) -> (u : _B) -> Type

_A === %self(unit). Unit unit (%unroll unit)
(%unroll unit : _B)

_B === %self(u). Unit unit (%unroll u)
(%unroll u : _B)

_A === %self(unit). Unit unit (%unroll unit)



fix%Unit (unit : %self(a). Unit a a) (u : %self(u). Unit unit u) : Type
  = (P : (%self(u). Unit unit u) -> Type) -> P unit -> P u;
fix%unit : Unit unit (%unroll unit) = P => x => x;

// external
Unit = %self(u). Unit unit (%unroll u);
unit : Unit = %unroll unit;
```

## Typing Self

Because self requires assuming self, there are a couple ways of typing it.

### Unification

Use an unification engine and after typing the content unify it against itself.

### Constraint

Accumulate all the clashes involved on the pending variable and check them afterwards.

### Assume

Type the type without checking any of the types that depends on itself. Then type the type against the assumption and it should match.

```rust
%self(False). (f : %self(f). %unroll False f) -> Type

assume False (%self(False). _A);
  check ((f : %self(f). %unroll False f) -> Type) _A
    check (%self(f). %unroll False f) Type;
      assume f (%self(f). _B);
        check (%unroll False f) Type;
            // TODO: accumulate substitution inside of unroll, like in Coq
          check (%unroll False) (_C -> _D);
            unify _A (_C -> _D);
          check f _C;
            unify _C (%self(f). _B);
          unify _D Type
      unify _B (%unroll False f);
    check Type Type;
unify ((f : %self(f). %unroll False f) -> Type) ((f : %self(f). %unroll False f) -> Type)
```

### Another Self

```rust
Γ, x : %self(x). T |- T : Type
------------------------------
%self(x). T : Type

Γ, A : Type, x : A |- T : Type
------------------------------
%self(A, x). T : Type

False = %fix(False) : (f : %self(f). False f) -> Type.
  f => (P : (%self(f). False f) -> Type) -> P f;
False = %self(f). False f;

Prop : Type
((A : Prop) -> (x : A) -> A) : Prop





fix%Unit
    (unit : %self(unit). (u : %self(u). Unit unit u) -> Unit unit u)
    (u : %self(u). Unit unit u) : Type
  = (P : (%self(u). Unit unit u) -> Type) -> P (unit u) -> P u;

%self(False). (f : %self(f). False f) -> Type

%self(False, f). (P : False -> Type) -> P f;

%self(Unit). (unit : %self(a). Unit a a) -> (%self(u). Unit unit u) -> Type



%self(Unit). (unit : %self(unit). (u : %self(u). Unit unit u) -> Unit unit u)
  (u : %self(u). Unit unit u) -> Type

%fix(Unit) : (A : Type) -> (unit : A -> A) -> (u : A) -> Type.
  A => unit => u => (unit : %self(A, unit). Unit A u u) -> (%self(u). Unit unit u) -> Type.


%fix(Unit) : (A : Type) -> Type.
  A => (wrap : A -> Unit A) -> (unit : Unit A) ->
  %self(A, u). (P : Unit A -> Type) -> P unit -> P (wrap u);

%self(A, u). (P : (%self(A, f). Unit A f) -> Type) -> P unit -> P (wrap u)

// False
Γ, x : %self(x). T |- T : Type
------------------------------
%self(x). T : Type

Γ, A : Type, x : A |- T : Type
------------------------------
%self(A, x). T : Type

fix%Sigma A B
  (pair : %self(pair).
    (A : Type) -> (B : A -> Type) ->
    (x : A) -> (y : B x) -> Sigma A B pair (pair A B x y))
  (s : %self(s). Sigma A B pair s)
  = pair => s =>
    (P : (%self(s). Sigma A B pair s) -> Type) ->
    ((x : A) -> (y : B x) -> P (pair A B x y)) ->
    P s;

Unit = %self(Unit, u).
  (P : Unit -> Type) -> P (%fix(Unit, u). P => x => x) -> P u;
fix%unit : Unit unit = P => x => x;
Unit = Unit unit;

refl =>(%self(f : False). (P : False -> Type) -> P f) refl;
%fix(False).


A => x => (f : %self(A, f). False A f) -> Type

Γ, A : Type, x : B |- T : Type
------------------------------
%self(A, x : B). T : B[A := %self(A, x : B). T]


False = %self(A, f). False A f;
```

## Why self?

A term that does induction of itself, is a term that must mention itself on the type level. As terms are proofs those must be described by some proposition aka a type. So we need a proposition that can manipulate it's own proof.

This is similar to how impredicative polymorphism allows for a proposition to mention the universe where it itself inhabits.

How can a type mentions it's own term?

### Assume term on the context

Major issue here is that all terms must have a valid type in context. And the term has the same type as the proposition that is still not typed.

### Some trick like Tarski universes

```rust
-----------
Self : Type

Γ |- M : Self
----------------
%typeof M : Type

Γ |- M : Self
----------------------
%unpack M : %typeof M

Γ, x : Self |- T : Type
-----------------------
%self(x). T : Type

Γ |- M : %self(A, x). T
-----------------------
%unroll M : T[A := %self(A, x). T | x := M]
```

```rust
False : %self(False). (f : %self(f). False f) -> Type;
False = f => (P : (%self(f). False f) -> Type) -> P f;
// close
False = %self(f). False f;


False : %self(False). (f : %self(f). False f) -> Type;
// False must be callable inside of False
// False must be callable with (%self(f). False f) inside of False

False : %self(False). (f : Self) -> Type;

False = (A : Type) => (f : A) => (P : A -> Type) -> P f;
False = %self(A, f). (P : A -> Type) -> P f;

Unit = X => (unit : X) =>
  %self(A, u). (P : (A : Type) -> (u : A) -> Type) -> P X unit -> P A u;
unit
  : %self(X, unit).  (P : (A : Type) -> (u : A) -> Type) -> P X unit -> P X unit
  = %fix(X, unit). (P : (A : Type) -> A -> Type) => (x : P X unit) => x;

%unroll unit : %self(A, u). (P : (A : Type) -> (u : A) -> Type) ->
  P (%self(X, unit).  (P : (A : Type) -> (u : A) -> Type) -> P X unit -> P X unit) unit ->
  P A u

Unit = Unit (%self(X, unit). Unit X unit) unit;

unit : %self(X, unit). Unit X unit
%unroll unit : Unit (%self(X, unit). Unit X unit) unit


%self(A, u). (P : (A : Type) -> A -> Type) ->
  P (%self(X, unit). Unit X unit) unit -> P A u

(P : (A : Type) -> A -> Type) ->
  P (%self(X, unit). Unit X unit) unit -> P A u
unit : Unit = %unroll unit;

%self(Unit).
  (unit : %self(a). Unit a a) ->
  (u : %self(u). Unit unit u) -> Type


False = (f : Self) -> ()
False = %self(f). False f;
Self (x => )


%typeof M : %self(False). (f : %self(f). False f) -> Type

%self(Unit).
  (unit : %self(a). Unit a a) ->
  (u : %self(u). Unit unit u) -> Type

%fix
%self(Unit).

fix%Unit (u : Self) (unit : %typeof u) =
  (P : (%self(u). %typeof u) -> Type) -> P unit -> P (%unpack u);

unit : %self(unit). Unit unit (%valueof unit) = _;

Unit = %self(u). Unit u unit;


Γ, A : Type, x : A |- T : Type
------------------------------
%self(A, x). T : Type

Γ, A : Type, x : A |- M : T
------------------------------
%fix(A, x). M : %self(A, x). T

Γ |- M : %self(A, x). T
-------------------------------------------
%unpack M : T[A := %self(A, x). T | x := M]

Γ |- M : %self(A, x). T
-------------------------------------------
%unfold M : T[A := %self(A, x). T | x := M]

Bool = T => (true : T) => F => (false : F) => %self(Bool, b).
Bool = T => (true : T) => F => (false : F) => %self(Bool, b).
  (P : (A : Type) -> A -> Type) -> P T true -> P F false -> P Bool b;

// internal
UnitT = %self(Unit, u). (P : (A : Type) -> A -> Type) -> P Unit u -> P Unit u;
unit : UnitT = %fix(Unit, u). (P : (A : Type) -> A -> Type) => (x : P Unit u) => x;

Unit T (unit : T) (u : T) =
  (P : T -> Type) -> P unit -> P (w Unit u);

fix%UnitW =
  Unit UnitW ? ()
unit_ind
  : (P : UnitT -> Type) -> P unit -> (u : Unit) -> P u
  = P => p_unit => u =>
    u (A => x => (A_eq_UnitT : A == UnitT) -> P (A_eq_UnitT (T => T) x))
     (A_eq_UnitT => )
// external

unit : Unit = %unroll unit;

%self(Unit, u).
  (P : (A : Type) -> A -> Type) -> P U unit -> P Unit u
(P : (A : Type) -> A -> Type) -> P U Unit -> P Unit U

Bool = T => (true : T) => F => (false : F) => %self(Bool, b).
  (P : (A : Type) -> A -> Type) -> P T true -> P F false -> P Bool b;
true
  : (F : Type) -> (false : F) -> %self(T, true). Bool T true F false
  = F => false => %fix(T, true).
    (P : (A : Type) -> A -> Type) => (then : P T true) => (else : P F false) => then;
false
  : %self(F, false). Bool (%self(T, true). Bool T true F false) true F false
  = %fix(F, false).
    (P : (A : Type) -> A -> Type) => (then : P T true) => (else : P F false) => else;
Bool = Bool
  (%self(T, true). Bool T true (%self(F, false). Bool T true F false) false) (true false)
  (%self(F, false). Bool T true F false) false;

// setup
Bool : Type
true : Bool
false : Bool

Bool = %self(A, b). (P : (A : Type) -> A -> Type) -> P Bool true -> P Bool false -> P A b;
true = (P : (A : Type) -> A -> Type) => (then : P Bool true) => (else : P Bool false) => then;
false = (P : (A : Type) -> A -> Type) => (then : P Bool true) => (else : P Bool false) => else;


```

## Letsch

```rust

------------------------------------
(x : Bool) => x : (x : Bool) -> Bool

-------------------------------------------------
(A : Type) => (x : A) => x : (A : Type) -> A -> A

------------------------------
(A : Type) => A : Type -> Type

Bool = (A : Type) -> A -> A -> A;

------------------------------------------------------
  (b : Bool) => (x : b Type Int String) => x
: (b : Bool) -> b Type Int String -> b Type Int String


Nat = (A : Type) -> A -> (A -> A) -> A;
ind : (P : Nat -> Type) -> P zero ->
  ((n : Nat) -> P n -> P (succ n)) -> (n : Nat) -> P n = ?;

f zero (p_zero) : P (succ zero)


case : (b : Bool) -> (A : Type) -> A -> A -> A = _;
ind : (b : Bool) -> (P : Bool -> Type) -> P true -> P false -> P b = _;
Bool = (P : Bool -> Type) -> P true -> P false -> P b?;

Γ, A : Type, x : A |- T : Type
------------------------------
%self(A, x). T : Type

Γ, A : Type, x : A |- M : T
------------------------------
%fix(A, x). M : %self(A, x). T

Γ |- M : %self(A, x). T
-------------------------------------------
%unroll M : T[A := %self(A, x). T | x := M]

False = %fix(False). %self(f). (P : False -> Type) -> P f;

(P : Nat ->)
```

### Identity Induction

```rust
fix%Unit (unit : %self(unit). Unit unit unit) (u : %self(u). Unit unit u) : Type
  = (P : (%self(u). Unit unit u) -> Type) -> P unit -> P u;
fix%unit : Unit unit unit =
  (P : (%self(u). Unit unit u) -> Type) => (x : P unit) => x;
// keeping induction clean

%self(False). (f : %self(f). False f) -> Type
%self(Unit). (unit : %self(a). Unit a a) -> (%self(u). Unit unit u) -> Type

Γ, A : Type, x : A |- M : T
------------------------------
%fix(A, x) : T. M : %self(A, x). T

(unit : %self(a). Unit a a)

Unit => %self(U, u). Unit U u u;

%self(False). (f : %self(f). False f) -> Type

%self(False). (A : Type) -> (f : %self(f). False f) -> Type

FalseT (ExternallyApply : (A : Type) -> A -> (((A : Type) -> A ->) -> Type) -> Type) =
  %self(T, False). ExternallyApply T False
      (False => A ->(f : %self(A, f). False A f) -> Type);

FalseT (T => False => A => f => )

fix%UnitT U1 (unit : U1) U2 (u : U2) =
  (unit : %self(U, u). UnitT U u U u) ->
  (u : %self(U, u). UnitT _ unit U u) ->
  Type;
fix%Unit
  (unit : %self(U, u). UnitT U u U u) U (u : U) : Type
  = (P : (%self(U, u). Unit unit U u) unit U u) ->
    Type) -> P unit -> P u;
fix%unit

%fix(A, Unit) : UnitT A Unit.
  unit => u => (P : )

Unit = fix(A, Unit) : (unit : Unit) -> (u : Unit) -> Type.
  (P : Unit unit u -> Type) -> P unit -> P u;

fix%Unit Unit (unit : Unit) U (u : U) : Type =
  (P : );
False = A => f =>
  (f : %self(A, f). False A f) -> Type;

False = A => f => False False




Unit =
  (unit : %self(a). Unit a a) -> (%self(u). Unit unit u) -> Type

False = A => (f : A) =>
  (P : A -> Type) -> P f
False = %fix(False) : (f : %self(f). False f) -> Type.
  f => (P : (%self(f). False f) -> Type) -> P f;
False = %self(f). False f;

UnitT = %self(Unit, u). (P : (A : Type) -> A -> Type) -> P Unit u -> P Unit u;
unit : UnitT = %fix(Unit, u). (P : (A : Type) -> A -> Type) => (x : P Unit u) => x;


Unit : Type
unit : Unit

Unit = (P : Unit -> Type) -> P unit -> P unit;

```

## History

### Internal Fix and Self

```rust
Unit : Type
unit : Unit;

Unit = %self(u). (P : Unit -> Type) -> P unit -> P u;
unit = (P : Unit -> Type) => (x : P unit) => x;

// expanded
Unit = %fix(Unit). (
  unit = %fix(unit). (P : Unit -> Type) => (x : P unit) => x;
  %self(u). (P : Unit -> Type) -> P unit -> P u
);
```

### No Internal Expansion of Fix

```rust
False = %fix(False). %self(f). (P : False -> Type) -> P f;

%fix(False). (P : False -> Type) -> P f

Unit : %self(Unit). (u : %self(u). Unit u). Type;
unit : %self(u). Unit u;

Unit = u => (P : (%self(u). Unit u) -> Type) -> P unit -> P u;
unit = (P : (%self(u). Unit u) -> Type) => (x : P unit) => x;


Unit = %self(Unit). (unit : %self(u). Unit u) ->

fix%Unit (unit : %self(u). Unit u u) (u : %self(u). Unit u u) :Type.
  (P : %self(u). Unit u u) -> P unit -> P u;
fix%unit : Unit unit unit = (P : %self(u). Unit u u) => (x : P unit) => x;

Unit = %self(u). Unit u u;
unit : Unit = unit;

unit : %self(u). Unit unit u;

%self(Unit).
  (u : %self(u). Unit unit u) ->
  (unit : %self(unit). Unit unit unit) -> Type



Unit : %self(Unit).

  (unit : %self(unit). (u : %self(u). Unit unit (unit u)) -> Unit unit (unit u)) ->
  (u : %self(u). Unit unit (unit u)) -> Type;

(u : %self(u). Unit unit (unit u)) -> Unit unit (unit u)

%self(unit). Unit unit (unit u)

expected : %self(u). Unit unit u
received :

Unit = %fix(Unit) : (unit : %self(unit). (u : ) Unit unit u) ->
  (u : %self(u). Unit unit u) -> Type.
  unit => u => (P : (%self(u). Unit unit u) -> Type) -> P unit -> P u;

// WARNING: is this type well formed??? (%self(u). Unit u u) is weird
Unit : %self(Unit). (unit : %self(u). Unit u u) -> (u : %self(u). Unit unit u) -> Type
unit : %self(u). Unit u u;

Unit = unit => u => (P : (%self(u). Unit unit u) -> Type) -> P unit -> P u;
unit = (P : (%self(u). Unit unit u) -> Type) => (x : P unit) => x;
// external
Unit = %self(u). Unit unit u;
unit : Unit = unit;

// expanded
Unit = %fix(Unit) : (unit : %self(u). Unit u u) -> (u : %self(u). Unit unit u) -> Type.
  unit => u => (P : (%self(u). Unit unit u) -> Type) -> P unit -> P u;
unit = %fix(unit) : Unit unit unit.
  (P : (%self(u). Unit unit u) -> Type) => (x : P unit) => x;
// external
Unit = %self(u). Unit unit u;
unit : Unit = unit;
```

### No Implicit %unroll of %self

```rust
// WARNING: is this type well formed???
Unit : %self(Unit). (unit : %self(u). Unit u u) -> (u : %self(u). Unit unit u) -> Type
unit : %self(u). Unit u u;

Unit = unit => u => (P : (%self(u). Unit unit u) -> Type) -> P unit -> P u;
unit = (P : (%self(u). Unit unit u) -> Type) => (x : P unit) => x;
// external
Unit = %self(u). Unit unit u;
unit : Unit = unit;
```

## Alternative Induction

```rust
Eq A x y = (P : A -> Type) -> P x -> P y;

Unit1 = %fix(Unit1). %self(u). (P : Unit1 -> Type) -> P u -> P u;
unit : Unit1 =
  %fix(u). (P : Unit1 -> Type) => (x : P u) => x;




(P : Unit -> Type) -> P unit -> P unit
(P : Unit -> Type) -> P unit -> P unit

unit     : %self(u). (P : Unit -> Type) -> P (%unroll u) -> P (%unroll u);

%unroll unit : %self(u). (P : Unit -> Type) -> P (%unroll u) -> P (%unroll u);
expected     : %self(u). (P : Unit -> Type) -> P (%unroll unit) -> P u


%self(Unit).
  (unit : %self(u). Unit u (%unroll u)) ->
  (u : %self(u). Unit unit (%unroll u)) -> Type



%unroll unit : %self(u). Unit u u
%unroll unit : %self(u). Unit (%unroll unit) u

fix%Unit (unit : %self(u). Unit u u) (u : %self(u). Unit unit u) : Type =
  (P : Unit -> Type) -> P

fix%Unit = %self(u). (K : Type) -> (Eq Unit u (%unroll unit) -> K) -> K;

fix%UnitT = %self(u). (K : Type) -> (Eq UnitT u u -> K) -> K;
fix%unit : UnitT = K => (k : (Eq Unit u u -> K)) => k refl;
fix%Unit = %self(u). (K : Type) -> (Eq Unit u (%unroll unit) -> K) -> K;

UnitT = %self(UnitT, u). (K : Type) -> (Eq UnitT u u -> K) -> K;
unit : UnitT = %fix(UnitT, u) => K => (k : Eq UnitT u u -> K) => k refl;



UnitT = %self(Unit, u). (K : Type) -> (Eq Type Unit Unit-> Eq Unit u u -> K) -> K;
unit : UnitT = %fix(Unit, u). (K : Type) => (k : Eq Unit u u -> K) => k refl;

Unit = %self(Unit, u). (K : Type) -> (Eq ->(P : (A : Type) -> A -> Type) -> P UnitT unit -> P Unit u;

ind (P : UnitT -> Type) (x : P Unit (%unroll unit)) (u : Unit) : P u =
  u (A => x => (eq : A == UnitT) -> P A x eq)

Unit T (unit : T) (u : T) =
  (P : T -> Type) -> P unit -> P (w Unit u);

```

## Modern

```rust
%self(False). False // Type : Type

%self(A, False). False;

%self(A, False). (eq : A == ((eq : A == A) -> False)). eq (T => T) False;

%self(A, False). (eq : A == Type). eq (T => T) False;



Unit A unit u = (P : A -> Type) -> P unit -> P u;
unit = %fix(A, u) : Unit A u u. (P : A -> Type) => (x : P unit) => x;

Unit = %self(U2, u). Unit (%fix(U1, u). Unit U1 u U1 u) unit U2 u;


unit = (P : (%self(u). Unit u) -> Type) => (x : P unit) => x;


Unit U1 unit U2 u = (P : (A : Type) -> A -> Type) -> P U1 unit -> P U2 u;
U1 = %self(U1, u). Unit U1 u U1 u;
unit : U1 = %fix(U1, u). P => x => x;

Unit = %self(U2, u). Unit U1 unit U2 u;
// TODO: relies on removing duplicated self %self(A, x). %self(U2, u). T
unit = %unroll (A => x => %self(U2, u). Unit A x U2 u) unit;


Eq A x y = (P : A -> Type) -> P x -> P y;
refl A x : Eq A x x = P => x => x;

T1 = %self(T1, _). Eq Type T1 T1;
T2 = %self(T2, _). Eq Type T1 T2;

T2 = Eq Type
  (%self(T1, _). Eq Type (%self(T1, _). Eq Type T1 T1) T1)
  (%self(T1, _). Eq Type (%self(T1, _). Eq Type T1 T1) T1);

T2 = Eq Type (%self(T1, _). Eq Type (%self(T1, _). Eq Type T1 T1) T1) (%self(T1, _). Eq Type (%self(T1, _). Eq Type T1 T1) T);

T2 = Eq Type
  (%self(T1, _). Eq Type (%self(T1, _). Eq Type T1 T1) T1)
  (%self(T2, _). Eq Type (%self(T1, _). Eq Type T1 T1) T2);

// TODO: relies on removing duplicated self %self(A, x). %self(U2, u). T
unit = %unroll (A => x => %self(U2, u). Unit A x U2 u) unit;

Unit : Type;
T : (u : Unit) -> Type;
unit : Unit;

T = (u : Unit) => (P : Unit -> Type) -> P unit -> P u;
Unit = %self(u). T
%self(u). T
Unit = (unit : Unit) => (P : Unit -> Type) -> P unit


Unit U1 unit U2 u =
  (P : (A : Type) -> A -> Type) -> P U1 unit -> P U2 u;
U1 = %self(U1, u). Unit U1 u U1 u;
unit : U1 = %fix(U1, u). P => x => x;

Unit = %self(U2, u). Unit U1 unit U2 u;


%self(U2, u). Unit U1 unit U2 u === %self(U1, u). Unit U1 unit U1 u

Unit A unit u = (P : A -> Type) -> P unit -> P u;
U1 = %self(U1, u). Unit U1 u u;
unit : U1 = %fix(U1, u). P => x => x;

U2 = %self(U2, u). Unit U2 unit u;


Unit u unit = (P : (%typeof u) -> Type) -> P unit -> P (%valueof u);
U1 = %self(u). Unit u (%valueof u);
unit : %self(u). Unit u (%valueof u) = %fix(u). P => x => x;

unit : %self(u). Unit u unit = %unroll unit;
Unit = %self(u). Unit u unit;

%fix(A, Sigma).
```

## Degenerate

```rust
Γ, A : Type, x : A |- T : Type
------------------------------
%self(A, x). T : Type

Γ, A : Type, x : A |- M : T
------------------------------
%fix(A, x). M : %self(A, x). T

Γ |- M : %self(A, x). T
-------------------------------------------
%unpack M : T[A := %self(A, x). T | x := M]

Γ |- M : %self(A, x). T
-------------------------------------------
%unfold M : T[A := %self(A, x). T | x := M]

loop = %fix(f : Unit -> Unit). (x : Unit) => f x;

loop = %fix(A, f). (unfold : A -> Unit -> Unit) => (x : Unit) => unfold f x;

Eq A x B y = (P : (T : Type) -> T -> Type) -> P A x -> P B y

l = %fix()
%self(A, f). (unfold : A -> Unit -> Unit) -> Unit -> Unit;

Fix = %fix(Fix) : Type. (f : Fix) -> Unit;
fix : Fix = (f : Fix) => f f;


Fix = %fix(A, Fix).
  (unfold : A -> Type) -> (f : unfold Fix) -> Unit;
fix = (f : Fix) => f ((x : Fix) => x);


Loop = %fix(Loop) : Type -> Type -> Type. A => B => A -> Loop B A;

Loop = %fix(A, Loop). A => B =>
Loop = %fix(A, Loop).
  (unfold : A -> ) => A => B => A -> unfold Loop B A


fix = (f : Fix) => f f;

%self(Fix, _). (f : Fix) -> Unit


False = (f : Loop refl) => f refl;

Loop = %fix(A, Loop). (unfold : A -> Type) -> (f : Unit) -> unfold Loop;
False = (f : Loop) => f (x => x) () (x => x) ();

Γ, A : Type, x : A |- M : T
------------------------------
%fix(A, x). M : %self(A, x). T

Fix = %fix(A, Fix). (unfold : A -> Type) => unfold Fix -> Unit;
fix = (f : Fix) => f (x => x) f;

False0 = %fix(A, False). (eq : A == Type) -> (f : eq (T => T) False) -> Unit;
False1 : Type = %unfold False;



%self(False). (f : %self(f). False f) -> Type
%fix(False) : (f : %self(f). False f) -> Type.
  f => (P : (%self(f). False f) -> Type) -> P f;


//
Unit : %self(Unit). (u : %self(u). Unit u) -> Type;
unit : %self(u). Unit u;

Unit = u => (P : (%self(u). Unit u) -> Type) -> P unit -> P u;
unit = (P : (%self(u). Unit u) -> Type) => (x : P unit) => x;

// steps, define both types, define Unit, define unit using definition of Unit

TUnit = %self(Unit). (u : %self(u). Unit u) -> Type;

MUnit (Unit : TUnit) (unit : Tunit Unit) =
  u => (P : (%self(u). Unit u) -> Type) -> P unit -> P u;
Munit (Unit : TUnit) (unit : Tunit)
  = (P : (%self(u). Unit u) -> Type) => (x : P unit) => x;



expected : %self. \2 \1 \0
received : %self. \1 (%unroll \0) \0
expected : %self(u/1). Unit u/2 u/1
received : %self(u). Unit u u
//
(let ((fs (Y* ((f1 f2 ... fn) => e1) ... ((f1 f2 ... fn) => en))))
    (apply ((f1 f2 ... fn) => e) fs))
Y (T : %self(T). (x : %self(x). T x) -> Type)
  (w : (x : %self(x). T x) -> T x) : %self(x). T x = %fix(x) : T x. w x
Y* l =
  (u => (u u))
  (p => (map (li => x => li (p p) x) l))

fs = (Y* ((f1 f2 ... fn) => e1) ... ((f1 f2 ... fn) => en));
(apply ((f1 f2 ... fn) => e) fs))

%self(Unit). (unit : %self(u). Unit u u) -> (u : %self(u). Unit unit u) -> Type

// dependencies
Unit = %fix(Unit) : (u : %self(u). Unit u) -> Type.
  u => (P : (%self(u). Unit u) -> Type) -> P unit -> P u;

UnitT = %self(Unit). (u : %self(u). Unit u) -> Type;


Unit2 = %fix(Unit) : (unit : %self(u). Unit u) -> UnitT.
  unit => MUnit (Unit unit) unit;

UnitX = %fix(Unit) : (unit : %self(u). Unit u) -> UnitT.
  unit => (u : %self(u). Unit u) => (P : (%self(u). Unit u) -> Type) -> P unit -> P u) (Unit unit) unit;



Unit = (unit : %self(u). Unit u) =>
  %fix(Unit) : (u : %self(u). Unit u) -> Type. MUnit Unit

T1 = %self(u). Unit u u;

%self(Unit).
  (unit : %self(u). Unit u u) ->
  (u : %self(u). Unit unit u) -> Type

expected : %self(u). Unit unit u
received : %self(u). Unit u u
Unit =

(f => x => f x x)

(x : Int 2) => x + x;

(G : Grade) -> Type;

(x : $Int) => (p : Bool)
  (p | true => x + 1
    | false => x) x;


f = (x : Int 2) => x + x;

abs : (A : Type) -> A = ?;

%self(Unit).
  (self%unit : %self(u). Unit unit u) ->
  (self%u : Unit unit u) -> Type

%self(False). (f : %self(f). False f) -> Type;

%self(T, False).
  (unfold_T :
    %self(UT, _). T -> (unfold_T : UT) -> (f : ) -> Type) ->
  (f : %self(A, f). (unfold_A : A == ?) ->
    unfold_T False unfold_T (unfold_A f)) -> Type

%self(False). (f : %self(f). False f) -> Type;

Γ, A : Type, x : A |- T : Type
------------------------------
%self(A, x). T : Type

A = Γ, x : A |- T : Type  A == T
----------------------------
Γ |- %self(x). T : Type



%exists ?X. %self(False) : (f : %self(f). _) -> Type.
  (f : %self(f). False f) -> Type;

((A : Type 0) -> A -> A) : Type 1;

Type : Type

x => x;

id : (0 A : Prop) -> A -> A = A => x => x;

x = id ((0 A : Prop) -> A -> A);

1234567890
!@#$^&*()_

(x => ())

Γ, x : %self(x). T |- T : Type
------------------------------
Γ |- %self(x). T : Type

Γ, x : %self(x). T |- M : T
-----------------------------
Γ |- %fix(x). M : %self(x). T

case : Bool -> (A : Type) -> A -> A -> A = _;
Bool = (A : Type) -> A -> A -> A;

ind : (b : Bool) -> (P : Bool -> Type) -> P true -> P false -> P b = _;

Unit : %self(Unit). (unit : %self(unit). %self(u). Unit unit u) -> (u : %self(u). Unit unit u) -> Type;
  unit :%self(unit). %self(u). Unit unit u
  u : %self(u). Unit unit u;

Unit : %self(Unit). _A

%self(False). (f : %self(f). False f) -> Type

False : %self(False). _A
  f : %self(f). _B
    _A := (f : %self(f). _B) -> Type
  _B := False f
unify (%self(False). _A) (%self(False). (f : %self(f). False f) -> Type)
unify (%self(False). (f : %self(f). False f) -> Type) (%self(False). (f : %self(f). False f) -> Type)


Unit : %self(Unit). (unit : %self(unit). %self(u). Unit unit u) -> (u : %self(u). Unit unit u) -> Type;
%self(Unit). (unit : %self(unit). %self(u). Unit unit u) -> (u : %self(u). Unit unit u) -> Type;

Bool = %self(b). (P : Bool -> Type) -> P true -> P false -> P b;

(x => x(x))(x => x(x))
(x => x(x))(x => x(x))
(x => x(x))(x => x(x))

f () = f ()
f = (x => x(x));
g = (x => x(x));

true_ = x => y => x;
false_ = x => y => y;

if_ = b => x => y => b(x)(y);

if_(true_)(0)(1)

Kind =
  | *
  | K -> K;
Type =
  | A -> B
  | <A : K>B
  | A<B>;

Term =
  | x
  | (x : T) => m
  | m n
  | <A>m;
  | m<A>;

id : (x : Int) -> Int = (x : Int) => x;
incr : (x : Int) -> Int = (x : Int) => x + 1;

zero = z => s => z;
succ = n => z => s => s(n(z)(s));

one = z => s => s(z);
two = z => s => s(s(z));

two = succ(succ(zero));
Bool = <A>(x : A) => (y :)
true : = x => y => x;
false : = x => y => y;
(x : Int) -> Int
Int implies Int;

<A>(x : A) -> A

<A>(x : A) => A;

Forall A, A implies A;

(<A>(x : A) -> A)<Int>

(x => m)(n) === m[x := n];
(<A>m)<B> === m[A := B];

(<A>(x : A) => x)<Int>
((x : A) => x)[A := Int]
(x : Int) => x

(x : Int) =>

(x : Int) =>
(<A>((x : A) => x))<Int>

Int : *
(<F : * -> * >F<Int>)


T : * -> *

Term -> Term // Simply Typed Lambda Calculus // Functions
Type -> Term // System F // Parametric Polymorphism
Type -> Type // System Fω // Higher Kinded Type
Term -> Type // Calculus of Construction // Dependent Types


Id : Type = (A : Type) -> A -> A;
id : Id = (A : Type) => (x : A) => x;

Bool = (A : Type) -> A -> A -> A;
true : Bool = (A : Type) => (x : A) => (y : A) => x;
false : Bool = A => x => y => y;

A -> B

f : (x : Bool) -> x Type Int String -> x Type Int String
  = (x : Bool) => (y : x Type Int String) => y;

f : (x : Bool) -> x Type Int String -> x Type Int String
  = (x : Bool) => (y : x Type Int String) => y;

a : Int -> Int = f true;
b : String -> String = f false;


| Rust |
| OCaml | TypeScript |
| Elixir | Haskell | C# | C | Python | Lua |
| Scala | F# | Go | Java | PHP | Clojure | Ruby | JavaScript | C++ | Perl |

@Unit -> (unit : @unit -> @u -> (@Unit) unit u) -> (u : @u -> (@Unit) unit u) -> Type
@(Unit : @Unit -> (unit : @unit -> @u -> (@Unit) unit u) -> (u : @u -> (@Unit) unit u) -> Type) =>
  (unit : @unit -> @u -> (@Unit) unit u) => (u : @u -> (@Unit) unit u) =>
  (P : (u : @u -> (@Unit) unit u) -> Type) -> P (@unit) -> P u


@(False : @False -> (f : @f -> False@ f) -> Type) => (f : @f -> False@ f) => (P : (f : @f -> False@ f) -> Type) -> P f

@A : (f : @f -> @A f) -> Type


f : expected = term : received

term : expected

unit = %fix(unit).

  %fix(u) : (eq :
    %self(eq).
      (P : Unit (unit u eq) (u eq) -> Type) -> P (unit u eq) -> P (u eq);
  ) -> Unit (unit u eq) (u eq).
    (P : (%self(u). Unit (unit u eq) u) -> Type) => (x : P (unit u eq)) =>
      eq P x;


%self(unit). %self(u). Unit unit u;

expected : %self(u). Unit unit u
received : %self(u). () -> Unit unit (u ())

%fix(unit) : -> W (%self(u). Unit unit u).
  %fix(u) : (self%eq : unit u eq == u eq) -> Unit (unit u eq) (u eq).
    eq =>  (P : (%self(u). Unit (unit u eq) u) -> Type) => (x : P (unit u eq)) => eq P x;


%self(Unit).
  (unit : %self(unit). %self(u). Unit unit u) ->
  (u : %self(u). Unit unit u) -> Type;

%self(Unit).
  (unit : %self(unit) Unit unit unit) ->
  (u : %self(u). Unit unit u) -> Type;

// TODO: those should be equal
%self(unit). %self(u). Unit unit u ===
%self(unit). Unit unit unit


Γ |- M : T[x := M]
-------------------------------
%roll(M) <== %self(x). T

Γ |- M : %self(u : A). B
-------------------------------
%unroll(M) : B[u := %unroll(M)]

Γ |- M : A
-------------------------------
M@ <== %self(x). B

%self(x). Int <> Int


T : %self(T). (a : %self(b). T b b) -> (c : %self(d). T a d) -> Type;

%self(T). (e : %self(f). T f f) -> (g : %self(h). T a d) -> Type


// %self(unit). %self(u). Unit unit u

CoC + Self Types


%self(Unit).
  (unit : %self(unit). %self(u). Unit unit u) ->
  (u : %self(u). Unit unit u) -> Type;

(unit : %self(unit). %self(u). Unit unit u) => Unit unit unit

%self(Unit).
  (unit : %self(unit) Unit unit unit) ->
  (u : %self(u). Unit unit u) -> Type;


data%Bool = | true | false;
data%Nat = | zero | succ (n : Nat);


Type : Type

// functions

Γ, x : A |- B : Type
--------------------
(x : A) -> B : Type

Γ, x : A |- m : B
---------------------------
(x : A) => m : (x : A) -> B

Γ |- m : (x : A) -> B   Γ |- n : A
----------------------------------
m n : B[x := n]

// induction

Γ, x : @x -> T |- T : Type
----------------------
@x -> T : Type

Γ, x : @x -> T |- m : T
-----------------------
@x : T => m : @x -> T

%self(Unit). (unit : %self(unit). Unit unit (%coerce unit : %self(u). Unit unit u)) ->
  (u : %self(u). Unit unit (%coerce u : %self(u). Unit unit u)) -> Type;

%self(Unit). (a : %self(b). Unit b (%coerce b : %self(u). Unit b u)) ->
  (c : %self(d). Unit a (%coerce d : %self(u). Unit a u)) -> Type;

received : %self(a). Unit b a
expected : %self(a). Unit b (%coerce a : %self(u). Unit b u)

%self(Unit).
  (u : %self(u). Unit (%coerce (%unroll u))) -> Type;

fix%Unit (unit : %self(unit). %self(u). Unit unit u) (u : %self(u). Unit unit u) =
  (P : (%self(u). Unit unit u) -> Type) -> P (%unroll unit) -> P u;

fix%Unit (unit : %self(unit). %self(u). Unit unit u) (u : %self(u). Unit unit u) =
  (P : (%self(u). Unit unit u) -> Type) -> P (%unroll unit) -> P u;

fix%False (f : %self(f). False f) =
  (P : (%self(f). False f) -> Type) -> P f;

Unit = unit => %fix(Unit). (u : %self(u). Unit u) =>
  (P : (%self(u). Unit u) -> Type) -> P unit -> P u;

Unit = unit => u => %fix(Unit).%self(u). (P : ? -> Type) -> P unit -> P u;

%self(u). (%fix(Unit). (unit : %self(unit). Unit unit) =>
  (P : Unit unit -> Type) -> P unit -> P u) (%fix(unit). P => x => x);

%self(Unit). (unit : %self(unit). Unit unit (%unroll unit)) ->
  (u : Unit unit (%unroll unit)) -> Type;

fix%Unit (unit : %self(unit). Unit unit (%unroll unit)) (u : Unit unit (%unroll unit)) =
  (P : Unit unit (%unroll unit) -> Type) -> P (%unroll unit) -> P u;
fix%unit : Unit unit (%unroll unit) =
  (P : Unit unit (%unroll unit) -> Type) => (x : P (%unroll unit)) => x;

%self(Unit). (unit : %self(unit). Unit unit (%unroll unit)) ->
  (u : Unit unit (%unroll unit)) -> Type;


W u == (P : (%self(u). W u) -> Type) -> P unit -> P unit



Unfold (Unit : Type) (unit : Unit) =
  %self(I). (%self(u). (P : Unit -> Type) -> P unit -> P (I u)) -> Unit;

MKUnit (Unit : Type) (unit : Unit) (I : Unfold Unit unit) =
  %self(u). (P : Unit -> Type) -> P unit -> P (I u);
fix%Unit
  (unit : %self(unit). (unfold : %self(unfold). Unfold (Unit unit unfold) (%unroll unit)) -> Unit unit unfold)
  (unfold : %self(unfold). Unfold (Unit unit unfold) (%unroll unit))
  = MKUnit (Unit unit unfold) ((%unroll unit) unfold) unfold;


fix%Unit0
  (I : %self(I). (unit : %self(unit). Unit0 I unit) ->
    (%self(u). (P : Unit0 I unit -> Type) -> P (%unroll unit) -> P (I unit u)) -> Unit0 I unit)
  (unit : %self(unit). Unit0 I unit)
  = %self(u). (P : Unit0 I unit -> Type) -> P (%unroll unit) -> P (I unit u);

fix%Unit0
  (I : %self(I). (unit : %self(unit). Unit0 I unit) ->
    (%self(u). (P : Unit0 I unit -> Type) -> P (%unroll unit) -> P (I unit u)) -> Unit0 I unit)
  (unit : %self(unit). Unit0 I unit)
  = %self(u). (P : Unit0 I unit -> Type) -> P (%unroll unit) -> P (I unit u);
Unit1 = Unit0 (%fix(I). unit => x => x);

unit => (P : Unit1 unit -> Type) => (x : P (%unroll unit)) => I P x

unit = %fix(unit). %fix(u). (P : Unit1 unit -> Type) => (x : P (%unroll u)) => (x : P u)



%self(unit). %self(u). (P : Unit1 unit -> Type) -> P (%unroll unit) -> P u

%self(unit). %self(u). (P : Unit0 I unit -> Type) -> P (%unroll unit) -> P u

fix%Unit (unit : %self(unit). %self(u). Unit unit u) (u : %self(u). Unit unit u)
  = (P : (%self(u). Unit unit u) -> Type) -> P (%unroll unit) -> P u;


(P : (%self(u). Unit unit u) -> Type) -> P (%unroll unit) -> P (%unroll unit)


MKUnit = (Unit : Type) => (unit : Unit) =>
  (unfold : %self(unfold). (%self(u). (P : Unit -> Type) -> P unit -> P (unfold u)) -> Unit) =>
  %self(u). (P : Unit -> Type) -> P unit -> P (unfold u);

fix%Unit (unit : %self(unit). Unit unit) = MKUnit (Unit unit) unit

fix%Unit0 =
Unit = MK (%self(u). Unit

%self(unit).

fix%unit
  : (P : (%self(u). W u) -> Type) -> P unit -> P unit
  = P => x => x;
fix%Unit (u : %self(u). Unit u) =
  (P : (%self(u). Unit u) -> Type) -> P (
    %fix(unit) : (P : (%self(u). Unit u) -> Type) -> P (%fix(unit). unit ) -> P u. unit
  ) -> P u;

Unit = %self(u). Unit i (%unroll u);
unit : Unit = unit;


u : %self(u). (%fix(Unit). (unit : %self(unit). Unit unit) =>
  (P : Unit unit -> Type) -> P unit -> P u) (?);


u : %self(u). (P : ((%fix(Unit). (unit : %self(unit). Unit unit) =>
  (P : Unit unit -> Type) -> P unit -> P u)) (%fix(unit). P => x => x) -> Type) -> P (%fix(unit). P => x => x) -> P u ;


%self(unit). Unit unit (%unroll unit);

received : %self(unit). %self(u). Unit unit u
expected : %self(unit). %self(u). Unit unit u

%self(u). Unit unit (%roll u)


%self(Unit). (unit : %self(unit). Unit unit (%coerce unit : %self(u). Unit unit u)) ->
  (u : %self(u). Unit unit u) -> Type;

received : %self(unit). Unit unit (%coerce unit : %self(u). Unit unit u)
expected : %self(u). Unit unit u
(unit : %self(unit). %self(u). Unit unit u) -> Unit unit (%unroll unit)

f = (unit : %self(unit). %self(u). Unit unit u) =>
  (P : (%self(u). Unit unit u) -> Type) => (x : P (%unroll unit)) => x;



%fix(unit). f unit



%self(unit). Unit (%coerce unit) unit;

received : Unit (%coerce unit) unit
expected : Unit unit unit

fix%unit : Unit (%roll unit) unit

%self(Unit). (unit : %self(unit). Unit unit unit) ->
  (u : %self(u). Unit unit u) -> Type
fix%Unit (unit)

main = print("Hello World");

fun x.
all x.

self x.
fix x.

((x : Int) => x)
((x : String) => x)

m : Nat -> Type
n : Int -> Type
(m x)
(n (coerce x))


fix%Fix = Fix -> Fix;

(a : Nat 1) => (b : Nat 1) => Nat 1
(x) => x + x
(f : Fix * 4) => f f;

(1 obj) =>
```

## Induction Again

```rust
fix%False (I : %self(I). (%self(f). (P : False I -> Type) -> P (I f)) -> False I)
  = %self(f). (P : False I -> Type) -> P (I f);
False = False (%fix(unfold). (x ) => (x));

I : %self(I). (unit : %self(unit). (I : ?)  Unit1 unit I unit) -> %self(u). Unit0 I u;

Unit0T (T : Type) = %self(Unit0).
  (I : T -> %self(u). Unit0 I u) -> (u : %self(u). Unit0 I u) -> Type;

%self(Unit1). (unit : %self(unit). (I : %self(I). Unit1 unit I unit -> %self(u). Unit1 unit I u) -> Unit1 unit I unit) ->
  %self(Unit0). (I : ? -> %self(u). Unit0 I u) -> (u : %self(u). Unit0 I u) -> Type;

Unit0 (Unit : Type) (unit : Unit) (u : Unit) =
  (P : Unit -> Type) -> P unit -> P u;

%fix(Unit). (unit : %self(unit). Unit unit unit) => (u : %self(u). Unit unit u) =>

%self(False). (f : %self(f). False (%coerce f : ?)) -> Type;

%self(False). (f : %self(f). False (%coerce f : ?)) -> Type;

%self(f). False (%coerce f : %fix(T). %self(f). False (%coerce f : T))

%self(Unit). (
  (u : %self(u). Unit unit u)
)
%fix(Unit). (unit : %self(unit). Unit unit (%coerce unit : )) =>
  (u : %self(u). Unit unit u) =>
%fix(Unit2). unit => %fix(Unit1). u =>
  Unit0 ? unit u;

%fix(Unit2). (unit : %self(unit). (I : IT) -> Unit2 unit I (unit I)) =>
  %fix(Unit1). (I : %fix(IT). %self(I). (%self(unit). (I : IT) -> Unit2 unit I (unit I)) -> %self(u). Unit1 I u) =>
  (u : %self(u). Unit1 I u) => Unit0 (%self(u). Unit1 I u) (I unit) u;

fix%unit1 : Unit1 unit0 unit0 =
  (P : )
fix%Unit1 (Unit : Type) (unit : Unit) (I : %self(I). Unit -> %self(u). Unit1 Unit unit I u)
    (u : %self(u). Unit1 Unit unit I u) : Type =
  Unit0 (%self(u). Unit1 Unit unit I u) (I unit) u;

Unit1 (Unit : Type) (unit : Unit)
    (I : %self(I). (%self(u). Unit0 Unit unit (I u)) -> Unit) : Type =
  %self(u). Unit0 Unit unit (I u);

fix%Unit2 (unit : (I : ?) -> %self(unit). Unit0 (Unit2 unit))
  = Unit1 (Unit2 unit I) (unit I)

Unit1 (Unit : Type) (unit : Unit)
    (I : %self(I). (Unit0 Unit unit (I u)) -> Unit) : Type =
  Unit0 Unit unit (I u);

fix%Unit2 (unit : %self(unit). Unit1 unit I) (I : %self(I). Unit -> %self(u). Unit1 Unit unit I u)
    (u : %self(u). Unit1 Unit unit I u) : Type =
  Unit0 (%self(u). Unit1 Unit unit I u) (I unit) u;

%fix(Unit1). (I : %self(I). ? -> %self(u). Unit1 I u) => (u : %self(u). Unit1 I u) =>
  Unit0 (%self(u). Unit1 I unit u) (I unit) u;

fix%Unit0 (Unit : Type) (I : %self(I). Unit -> %self(u). Unit0 Unit I unit u)
  (unit : Unit) (u : %self(u). Unit0 Unit I unit u)
  = (P : (%self(u). Unit0 Unit I unit u) -> Type) -> P (I unit) -> P u;
fix%Unit1
  (I : %self(I). (%self(u). Unit1 I u) -> %self(u). Unit0 Unit I unit u)
  (unit : %self(unit). Unit1 I unit)
  = %self(u). Unit0 (%self(u). Unit1 I u) I unit u;

expected : %self(u). Unit0 Unit I unit u
fix%Unit1 (Unit : Type) (unit : Unit)
  (I : %self(I). Unit -> %self(u). Unit0 I u) (u : %self(u). Unit0 I u)
  = (P : (%self(u). Unit0 I u) -> Type) -> P (I unit) -> P u;

%fix(Unit1). (unit : %self(unit). (I : ? -> %self(u). Unit1 unit I u) -> Unit1 unit I unit) =>
  %fix(Unit0 : Unit0T). I => u =>
   (P : (%self(u). Unit0 I u) -> Type) -> P (I unit) -> P u;
Unit1 = Unit0 (%fix(I). unit => x => x);

(x => z) => x;


fix%False (I : %self(I). (%self(f). (P : False I -> Type) -> P (I f)) -> False I)
  = %self(f). (P : False I -> Type) -> P (I f);
False = False (%fix(I). x => x);

received : %self(unit). %self(u). (P : Unit I -> Type) -> P (I (%unroll unit)) -> P (I u)
expected : %self(u). (P : Unit I -> Type) -> P (I (%coerce unit)) -> P (I u)

%self(unit). %self(u). (P : Unit I -> Type) -> P (I (%unroll unit)) -> P (I u)

Unit0 (Unit : Type) (unit : Unit) (u : Unit) =
  (P : Unit -> Type) -> P unit -> P u;

%self(u). (I1 : -> T) ->
  (I0 : %self(I0). -> T) ->
  Unit0 T (I1 unit) (I0 u))
%fix(u). (P : T -> ) => ()

%self(Unit). (I : %self(I). ((
  unit : %self(unit). (P : Unit I -> Type) -> P (I unit) -> P (I unit) = _;
  %self(u). (P : Unit I -> Type) -> P (I unit) -> P (I u)
) -> Unit I))
 -> Type;

fix%Unit (I : %self(I). (%self(u). (P : Unit I -> Type) ->
  P (I (%fix(unit) : (P : Unit I -> Type) -> P (I unit) -> P (I unit). P => x => x)) -> P (I u)
) -> Unit I)
  = %self(u). (P : Unit I -> Type) ->
    P (I (%fix(unit) : (P : Unit I -> Type) -> P (I unit) -> P (I unit). P => x => x)) -> P (I u);

fix%Unit (I : %self(I). (u : %self(u). Unit I u) -> Unit I u) (u : %self(u). Unit I u) =
  (P : (%self(u). Unit I u) -> Type) -> P (%fix(u) : Unit I u. I (P => x => x)) -> P u;

%fix(u). (I : %self(I). ) =>
  (P : (%self(u). Unit I u) -> Type) => (x : P u) => (x : P u)

Unit1 = Unit (%fix(I). u => k => (x : P (%fix(u) : Unit I u. I u)) => ?)

fix%Unit (I : %self(I). (u : %self(u). Unit I u) -> Unit I u) (u : %self(u). Unit I u) =
  (P : (%self(u). Unit I u) -> Type) -> P (%fix(u) : Unit I u. I u) -> P u


Unit1 = Unit (%fix(I). u => P => (x : P (%fix(u) : Unit I u. I u)) => ?)

(P : (%self(u). Unit I u) -> Type) -> P (%fix(u) : Unit I u. I (%fix(unit). P => x => x)) -> P unit

False = False (%fix(I). x => x);


```

## Ideas

This tries to avoid internal expansion of fixpoint but still achieve induction for Unit.

### Simple External

Fails to type check due to some weird behaviour.

```rust
%self(Unit). (unit : %self(unit). Unit unit unit) =>
  (u : %self(u). Unit unit u) -> Type
```

### Internal Fixpoint

```rust
fix%Unit (I : ?) (u : %self(u). Unit I u) =
  (P : (%self(u). Unit I u)) -> P (%fix(u) : Unit I u. I u) -> P u;
```

### External Unfold

How to scale this to unit?

```rust
fix%False (I : %self(I). (%self(f). (P : False I -> Type) -> P f) -> False I) =
  %self(f). (P : False I -> Type) -> P f;
False = False (%fix(I). x => x);
```

### Nested Self

How to build the constructor?

```rust
%self(Unit). (unit : %self(unit). %self(u). Unit unit u) ->
  (u : %self(u). Unit unit u) -> Type
```

### Two Fixpoint

Seems natural if you try to lift first `u` then `unit`.

```rust
%self(Unit1). (unit : ?) -> %self(Unit0). (u : ?) -> Type
```

## Trying Again the Fancy One

```rust
%fix(Rec). Rec -> ();

RecT = %self(RecT, Rec). (I : %self(B, I). B -> RecT -> Type) -> Type;
Rec : RecT = %fix(A, Rec). I => I I Rec -> ();

I = %fix(B, I). (I : B) => (Rec : RecT) => Rec I;


%fix(False). (f : False) => (P : False -> Type) -> P f
False = %fix(FalseT)

(Rec I -> ());

%self(False). (f : %self(f). False f) -> Type;

%self(FalseT, False).
  (I : %self(IT, _). FalseT -> (f : ?) -> Type) ->
  (f : %self(fT, f). I False I) -> Type;


```

## Decomposing

Why does this works?

```rust
Γ, x : %self(x). T |- T : Type
------------------------------
%self(x). T : Type

Γ, A : Type, x : A |- T : Type
------------------------------
%self(A, x). T : Type

Γ, x : A |- B : Type  A === B[]
----------------------------------
%self(x : A). B : Type

Γ, x : A |- B : Type  A === B[x := ]
--------------------------------------------------
%self(x : A). B : Type

%self(False). (f : %self(f). False f) -> Type
```

```rust
%self(KFalse, False : (f : %self(Kf, f). KFalse f)).
  (f : %self(f). False f) -> Type

T : Type = %fix(False : Type). (f : False) -> (P : False -> Type) -> P f;

%fix(False : T). (f : False) ->
  (P : False -> Type) -> P
%self(T, False). (I : T -> Type) -> (f : I False) -> Type;


False = %fix(T, False : Type) : (I : T -> Type) -> (f : I False) -> Type.
  (I : T -> Type) => (f : I False) =>
  (P : I False -> Type) -> P f;

False = False (False => )
%self(T, False). (f : )

((F : Type) => (False : F -> Type) => (f : F) => False f);
%self(F, f). I F False f

%self(FalseT, False). (f : %self(f). False f) -> Type

False = %self(False. f). (P : False -> Type) -> P f;

```

## Constructors

```rust
-----------
Self : Type

Γ |- M : Self
----------------
%typeof M : Type

Γ |- M : Self
----------------------
%unpack M : %typeof M

Γ, x : Self |- T : Type
-----------------------
%self(x). T : Type


Γ, x : %self(x). T : T
----------------------
%self(x). T : Type

Γ |- M : %self(x). T
---------------------
%unroll M : T[x := M]


Γ |- M : %self(A, x). T
-------------------------------------------
%unroll M : T[A := %self(A, x). T | x := M]

%self(False). (I : Self -> Self -> Type) ->
  (f : %self(f). I False f) -> Type;

%self(f). (P : False I -> Type) -> P (I f);

unit = P => (x : P unit) => x

%self(A, x). T

Γ, x : A |- B : Type
----------------------
%self(x : A). B : Type

Γ, x : A |- M : B
--------------------------------
%fix(x : A). M : %self(x : A). B

Γ |- M : %self(x : A). B
------------------------
%unroll M : T[x := M]

%self(False).
%self(x : A). B
```

## Back to Magic Self

```rust
%self(Unit).
  (a : %self(b). Unit b b) ->
  (c : %self(d). Unit a d) -> Type;

%self(Unit).
  (a : %self(b). (? : (%self(d). Unit a d) -> Type) b) ->
  (c : %self(d). Unit a d) -> Type;

%self(Unit). (a : %self(b). %unroll Unit b b) -> (c : %self(d). %unroll Unit a d) -> Type;
%self. (%self. %unroll 1 0 0) -> (%self. %unroll 2 1 0) -> Type

%self. %unroll (1 : %self. (%self. %unroll 1 0 0) -> (%self. %unroll 2 1 0) -> Type) 0 0
%self. (%unroll 1 : (%self. %unroll 2 0 0) -> (%self. %unroll 3 1 0) -> Type) 0 0
%self. (%unroll 1 0 : (%self. %unroll 2 0 0) -> Type) 0 0



%self. (%self. ((1 0 : ) 0) -> (%self. 2 1 0) -> Type

received : %self. 1 0 0
expected : %self. 2 1 0

(λ.0) 1 === 1
%self(b). Unit b b
%self(d). Unit b d

%self(b). Unit b b
%self(d). Unit a d

%self(unit). Unit unit unit
%self(u). Unit unit u

%self(unit). (P : ? -> Type) -> P unit -> P unit;
%self(unit). (P : ? -> Type) -> P (%fix(unit). P => (x : P unit) => x) -> P unit;

%self(u). (P : ?) -> P u -> P u
Unit = %fix(Unit).
  (P : Unit -> Type) -> P (%fix(unit). P => (x : P unit) => x)

%self(Unit). (a : %self(b). %unroll Unit b b) -> (c : %self(d). %unroll Unit a d) -> Type;
%self(b). %unroll (Unit : %self(Unit). (e : %self(f). %unroll Unit f f) -> (g : %self(h). %unroll Unit e h) -> Type) b b
%self(b). (%unroll Unit : (e : %self(f). %unroll Unit f f) -> (g : %self(h). %unroll Unit e h) -> Type) b b
%self(b). (%unroll Unit (b : %self(b). %unroll Unit b b) : (g : %self(h). %unroll Unit b h) -> Type) b
received : %self(b). %unroll Unit b b; %unroll Unit i i
expected : %self(h). %unroll Unit b h; %unroll Unit i i // replaced b := i here
%self(b). (%unroll Unit b b : Type)

%self. %unroll (1 : %self. (%self. %unroll 1 0 0) -> (%self. %unroll 2 1 0) -> Type) 0 0
%self. (%unroll 1 : (%self. %unroll 2 0 0) -> (%self. %unroll 3 1 0) -> Type) 0 0
%self. (%unroll 1 (0 : %self. %unroll 2 0 0) : (%self. %unroll 2 0 0) -> Type) 0
%self. (%unroll 1 0 : (%self. %unroll 2 0 0) -> Type) (0 : %self. %unroll 2 0 0)
%self. (%unroll 1 0 0 : Type)


%self(Unit). (a : %self(b). %unroll Unit b b) -> (c : %self(d). %unroll Unit a d) -> Type;


fix%Unit (unit : %self(unit). %unroll Unit unit unit) (u : %self(u). %unroll Unit unit u)
  = (P : (%self(u). %unroll Unit unit u) -> Type) -> P unit -> P u;
fix%unit : %unroll Unit unit unit = P => x => x;

Unit = %self(u). %unroll Unit unit u;
unit : Unit = unit;
```

## Kewerson

```rust
Γ, x : %self(x). T |- T : Type
------------------------------
%self(x). T : Type

fix%Unit (unit : %self(unit). %unroll Unit unit unit) (u : %self(u). %unroll Unit unit u)
  = (P : (u : %self(u). %unroll Unit unit u) -> Type) -> P unit -> P u;
fix%unit : %unroll Unit unit unit = P => x => x;

Unit = %self(u). %unroll Unit unit u;
unit : Unit = unit;
ind (u : Unit) = %unroll u;

%fix(Unit)
Unit : %self(Unit).
  (unit : %self(unit). %unroll Unit unit unit) ->
  (u : %self(u). %unroll Unit unit u) -> Type


received : %self(k). %unroll Unit k k // unit := k; u := k
expected : %self(k). %unroll Unit k k // unit := k; u := k

%self. (%self. %unroll 1 0 0) -> (%self. %unroll 2 1 0) -> Type

%self. %unroll (1 : %self. (%self. %unroll 1 0 0) -> (%self. %unroll 2 1 0) -> Type) 0 0
%self. (%unroll 1 : (%self. %unroll 2 0 0) -> (%self. %unroll 3 1 0) -> Type) 0 0
%self. (%unroll 1 (0 : %self. %unroll 2 0 0) : (%self. %unroll 2 0 0) -> Type) 0
%self. (%unroll 1 0 0 : Type)

fix%Unit (self%unit : %unroll Unit unit unit) (self%u : %unroll Unit unit u) =
  (P : (self%u : %unroll Unit unit u) -> Type) -> P unit -> P u;
fix%unit : %unroll Unit unit unit = P => x => x;

@Unit (@unit : @Unit unit unit) (@u : @Unit unit u) : Type =
  (P : (@u : @Unit unit u) -> Type) -> P unit -> P u;
Unit = u @-> Unit unit u;

Unit = %self(u). %unroll Unit unit u;
unit : Unit = unit;

@>
%self(f). (x) => x
@f. (x) => x
@f(x) => x

λa. λb. b === λc. λd. d
λa. λb. a === λc. λd. c

λλ0 === λλ0 1 2
λλ1 === λλ1

formation : (x : Int) -> Int
introduction : (x : Int) => x
elimination : lambda arg

formation : %self(x). T
introduction : %fix(x) : T. m
elimination : %unroll x

%fix(0 Rec). (n : Rec) -> Int

%self(x, y). (x : Int, y : Int)
Int ->! Int;
(x => x(x)) : %fix(0 Rec). (1 : Rec) -> Int;

%self(False). (f : %self(f). False f) -> Type;

(%fix(x) : T. (m : T)) : %self(x). T
%fix(x : T). m
((x : %fix(Rec). Rec -> Int) => (%unroll x)(x))


%fix(x) : T. x
let rec f x = x + f x

let f = fun self x -> x + self x
let f = %fix(self). f self;

- -> +
%fix(Rec). Rec -> Int

fold : Nat -> (A : Type) -> A -> (A -> A) -> A;
case : Nat -> (A : Type) -> A -> (Nat -> A) -> A;

Nat = (A : Type) -> A -> (Nat -> A) -> A;

ind : (b : Bool) -> (P : Bool -> Type) -> P true -> P false -> P b;

Bool = (P : (0 Bool) -> Type) -> P true -> P false -> P b;

b => (P : Bool -> Type) -> P true -> P b;

%fix(Rec). (0 Rec) -> Int

((0 x : %fix(Rec). (0 Rec) -> Int) => 1)


Type : Type
((A : Type) -> A) : Type

(A : Type) => (x : A) => x;

(x => y => y)
(a => b => b)

f ((x : Int) => x)
f ((y : Int) => y)

Unit @-> (unit : unit @-> Unit unit unit) @-> (u @-> Unit unit u) -> Type

received : unit @-> Unit unit unit
expected : u @-> Unit unit u

Array.set : Array<A> -> Int -> A -> Array<A>;
f = (arr : Array<Int>) => (
  arr = Array.set(arr, 0, 0);
  arr = Array.set(arr, 1, 1);
);

Array.set : &mut Array<A> -> Int -> A -> ();
f = (arr : Array<Int>) => (
  Array.set(arr, 0, 0);
  Array.set(arr, 1, 1);
);

f : () -> Int;
f() === f();

incr = x => x + 1;

incr 1 === (x => x + 1) 1;

acc = 0;
f = () => acc;
x = f

double = (x : User) => (
  let () = free(x);
  1
);

(x => x + x)

x.y := x;

Socket : Type;
connect : () -> Socket;
close : Socket -> ();

f = () => (
  socket = connect();
  close(socket);
  1
)


f : %self(False). (f : %self(f). False f (x => x)) -> (k : Int -> Int) Type;

False :
(f : %self(f). False f) -> Type

(f : %self(f). %unroll False f) -> Type

P Int ((x : Int) => x)
P Bool ((x : Bool) => x)

(x : Int) -> Int
```

# Inference and Linearity

Rank-2 allows for existential types.
Infinite rank-1 instances.

Maybe some polymorphic recursion thing?

## Linear Rank-2

It only allows for one instance, so is it isomorphic to just having monomorphized signatures?

Can you always monomorphize rank-2 polymorphism?

## Again

```rust
@False (@f : False f) = (P : (@f : False f) -> Type) -> P f;

False @=> (@f : False f) => (P : (@f : False f) -> Type) -> P f;

False @=> (@f : (False @=> (@f : False f) => (P : (@f : False f) -> Type) -> P f) f) => (P : (@f : False f) -> Type) -> P f;

False : False @-> (f : f @-> @False f) -> Type ===
      ((False : False @-> (f : f @-> @False f) -> Type) @=>
        (f : f @-> @False f) => (P : (f : f @-> @False f) -> Type) -> P f);
    False
```

## Even More Self

```rust
%self(False). (f : %self(f). False f) -> Type

Γ, x : %self(x). T |- T : Type
------------------------------
%self(x). T : Type

fix%Unit (a : %self(b). %unroll Unit b b) (c : %self(d). %unroll Unit a d) =
  (P : (e : %self(f). %unroll Unit a f) -> Type) -> P a -> P c;

%self(Unit). (a : %self(b). %unroll Unit b b) -> (c : %self(d). %unroll Unit a d) -> Type;

%fix. (%self. %unroll \1 \0 \0) => (%self. %unroll \2 \1 \0) =>
  ((%self. %unroll \3 \2 \0) -> Type) -> \0 \2;

%fix. (%self. %unroll \1 \0 \0) => (%self. %unroll \2 \1 \0) =>
  ((%self. %unroll \3 \2 \0) -> Type) -> \0 \2;

((\0 : (%self. %unroll \4 \3 \0) -> Type) (\2 : %self. %unroll \4 \0 \0))

fix%Unit
  (I : %self(I). (unit : %self(unit). (P : %unroll Unit I unit -> Type) -> P (I unit unit) -> P (I unit unit)) ->
    (u : %self(u). (P : %unroll Unit I unit -> Type) -> P (I unit unit) -> P (I unit u)) -> %unroll Unit I unit)
  (unit : %self(unit). (P : %unroll Unit I unit -> Type) -> P (I unit unit) -> P (I unit unit)) =
  %self(u). (P : %unroll Unit I unit -> Type) -> P (I unit unit) -> P (I unit u);

%self(Unit).
  (I : %self(I). (unit : %self(unit). (P : %unroll Unit I unit -> Type) -> P (I unit unit) -> P (I unit unit)) ->
    (u : %self(u). (P : %unroll Unit I unit -> Type) -> P (I unit unit) -> P (I unit u)) -> %unroll Unit I unit) ->
  (unit : %self(unit). (P : %unroll Unit I unit -> Type) -> P (I unit unit) -> P (I unit unit)) -> Type;

%self(Unit).
  (A : %self(B). (c : %self(d). (P : %unroll Unit B d -> Type) -> P (B d d) -> P (B d d)) ->
    (e : %self(f). (P : %unroll Unit B c -> Type) -> P (B c c) -> P (B c f)) -> %unroll Unit B c) ->
  (g : %self(h). (P : %unroll Unit A h -> Type) -> P (A h h) -> P (A h h)) -> Type;

%self(B). (c : %self(d). (P : %unroll Unit B d -> Type) -> P (B d d) -> P (B d d)) ->
    (e : %self(f). (P : %unroll Unit B c -> Type) -> P (B c c) -> P (B c f)) -> %unroll Unit B c

expected : %self(f). %unroll Unit a f
received : %self(b). %unroll Unit b b


Unit @->
  (I : I @-> (unit : unit @-> (P : (u : @Unit I unit) -> Type) -> (x : P (@I unit unit)) -> P (@I unit unit)) ->
    (u : u @-> (P : (u : @Unit I unit) -> Type) -> (x : P (@I unit unit)) -> P (@I unit u)) -> @Unit I unit) ->
  (unit : unit @-> (P : (u : @Unit I unit) -> Type) -> (x : P (@I unit unit)) -> P (@I unit unit)) -> Type


Unit @->
  (A : B @-> (c : d @-> (P : (u : @Unit B d) -> Type) -> (x : P (@B d d)) -> P (@B d d)) ->
    (e : f @-> (P : (g : @Unit B c) -> Type) -> (x : P (@B c c)) -> P (@B c f)) -> @Unit B c) ->
  (h : i @-> (P : (l : @Unit A i) -> Type) -> (x : P (@A i i)) -> P (@A i i)) -> Type


B @-> (c : d @-> (P : (u : @Unit B d) -> Type) -> (x : P (@B d d)) -> P (@B d d)) ->
    (e : f @-> (P : (g : @Unit B c) -> Type) -> (x : P (@B c c)) -> P (@B c f)) -> @Unit B c

%fix. (%self. %unroll \1 \0 \0) => (%self. %unroll \2 \1 \0) =>
  ((%self. %unroll \3 \2 \0) -> Type) -> \0 \2;

fix%Unit (unit : %self(unit). %self(u). %unroll Unit unit u) (u : %self(u). %unroll Unit unit u) =
  (P : (u : %self(u). %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

fix%Unit I (unit : %self(unit). %unroll Unit unit u) =
  %self(u). (P : (u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P (I u);

Unit @->
  (I : I @-> (unit : unit @-> @Unit I unit) ->
    (u : u @-> (P : (u : @Unit I unit) -> Type) -> (x : P @unit) -> P (@I unit u)) -> @Unit I unit) ->
  (unit : unit @-> @Unit I unit) -> Type

Unit @->
  (I : I @-> (unit : unit @-> @Unit I unit) ->
    (u : u @-> (P : (u : @Unit I unit) -> Type) -> (x : P @unit) -> P (@I unit u)) -> @Unit I unit) ->
  (unit : unit @-> @Unit I unit) -> Type


fix%Unit I (unit : %self(unit). %unroll Unit unit u) =
  %self(u). (P : (u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P (I u);

fix%Unit (unit : %self(unit). %self(u). %unroll Unit unit u) (u : %self(u). %unroll Unit unit u) =
  (P : (u : %self(u). %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

fix%unit : %self(u). %unroll Unit unit u.

fix%Unit (a : %self(b). %unroll Unit b b) (c : %self(d). %unroll Unit a d) =
  (P : (e : %self(f). %unroll Unit a f) -> Type) -> P a -> P c;

Unit : Type
unit : Unit

Unit = %self(u)

fix%Unit (unit : %self(unit). %self(u). %unroll Unit unit u) (u : %self(u). %unroll Unit unit u) =
  (P : (u : %self(u). %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

fix%False (f : %self(f). False f) =
fix%Unit (unit : %self(unit). Unit unit) =
  %self(u). (P : (u : Unit unit) -> Type) -> P unit -> P u;
fix%Unit =
  %self(u). (P : Unit -> Type) -> P (%fix(unit : Unit). P => x => x) -> P u;

%self(u). (P : Unit -> Type) -> P (%fix(unit). P => x => x) -> P u

fix%Unit (unit : %self(unit). %self(u). %unroll Unit unit u) (u : %self(u). %unroll Unit unit u) =
  (P : (u : %self(u). %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;
Unit1 = (unit : %self(unit). %self(u). %unroll Unit unit u) => %self(u). Unit unit u;
fix%Unit (a : %self(b). %unroll Unit b b) (c : %self(d). %unroll Unit a d) =
  (P : (e : %self(f). %unroll Unit a f) -> Type) -> P a -> P c;

fix%Unit = %self(u). (P : Unit -> Type) -> P (%fix(unit). P => x => x) -> P u;




unit : %self(unit). (P : Unit -> Type) -> P unit -> P unit;
%coerce unit : %self(u). (P : Unit -> Type) -> P unit -> P u;

%fold unit : %self(u). (P : Unit -> Type) -> P unit -> P u;
() -> (P : Unit -> Type) -> P unit -> P u;

fix%Unit (unit : %self(unit). %self(u). %unroll Unit unit u) (u : %self(u). %unroll Unit unit u) =
  (P : (u : %self(u). %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

%fix(unit). %fix(u) : (I : ?) -> Unit (unit I) u.
  P => (x : P (%unroll unit)) => (x : P u)

%fix(unit). %fix(u) : Unit unit u. P =>
  (x : P (%fix(u) : Unit unit u. P => (x : P (%unroll unit)) => (x : P u))) =>
  (x : P (%fix(u) : Unit unit u. P => (x : P (%unroll unit)) => (x : P u)))

%self(unit). %fix(). Unit (I unit) (I u)
%fix(unit) : %fix(u). Unit () u.
  P => (x : P (%unroll unit)) => (x : P u)

M : %self(unit). %self(u). %unroll Unit unit u
%unroll M : %self(u). %unroll Unit unit u



fix%Unit (I : %self(I). ((A : Type) -> A -> A) -> Unit I) =
  %self(u). (P : Unit I -> Type) ->
    P (I (A => x => x)) -> P (I (A => %unroll u (_ => A)));

Unit1 = Unit (%fix(I). unit =>
  %fix(u). (P : Unit I -> Type) => (
)
%self(u). (P : Unit I1 I2 -> Type) ->
    P (I2 (%fix(unit). P => x => x)) -> P (I1 I2 u);

Unit1 = Unit (%fix(I1). I2 => x => x);
%fix(I1). unit =>

fix%unit (I : unit ==) : %self(u). %unroll Unit I unit u =

  %unroll (%fix(u). (I : %unroll unit == u) -> Unit I unit u. P => x => I P x)
    ;
```

## Why?

```rust

Γ, x : %self(x). T |- T : Type
------------------------------
%self(x). T : Type

Γ, x : A |- M : B
--------------------------------
%fix(x : A). M : %self(x : A). B

Γ |- M : %self(x). T
-----------------------------
%unroll M : T[x := %unroll M]

x == y /\ x <> y

Unit0 Unit unit u =
  (P : Unit -> Type) -> P unit -> P u;

%self(Unit, _). (I : ?) -> Type

%self(F, f). (P : F -> Type) -> P f;
Unit0 U1 unit U2 u =
  (P : (A : Type) -> A -> Type) -> P U1 unit -> P U2 u;
unit = %fix(U1, unit) : Unit0 U1 unit U1 unit = P => x => x;

Unit1 = %self(U, u). Unit0 _ unit U u;

Γ, x : %self(x). T |- T : Type
------------------------------
%self(x). T : Type

Γ |- M[x := %fix(x). M] : T
----------------------------
%fix(x) : T. M : %self(x). T

Γ |- M : %self(x). T
---------------------
%unroll M : T[x := M]

%self(False). (f : %self())

fix%Unit (unit : %self(unit). %self(u). %unroll Unit unit u) (u : %self(u). %unroll Unit unit u) =
  (P : (u : %self(u). %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

fix%unit I1 = (
  u = %fix(u) : (I2 : %unroll (unit I1) == (u I2)) -> Unit (unit I1) (u I2).
    I2 => P => x => I P x;


)

%unroll (%fix(u) : (I : %unroll unit = %unroll u I) -> Unit unit (%unroll u I).
  I => P => (x : P (%unroll unit)) => I P x)

(I : %unroll unit = %unroll (%fix(u) : (I : %unroll unit = %unroll u I) -> Unit unit (%unroll u I).
  I => P => (x : P (%unroll unit)) => I P x) I) => P => (x : P (%unroll unit)) => I P x

fix%Unit (unit : %self(unit). %self(u). %unroll Unit unit u) (u : %self(u). %unroll Unit unit u) =
  (P : (u : %self(u). %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

Unit =
  %fix(Unit0). (unit : %self(unit). %unroll Unit0 unit (%fix(I). u => u)) =>
  %fix(Unit1). (I : (u : %self(u). (P : (u : %unroll Unit1 I) -> Type) -> P unit -> P u) -> %unroll Unit1 I) =>
    %self(u). (P : (u : %unroll Unit1 I) -> Type) -> P unit -> P u;


%self(False). (f : %self(f). False f) -> Type

%self(Unit). (u : (unit : ?) -> %self(u). Unit u) -> Type

fix%Unit
  (I : %self(I).
    (unit : %self(unit). Unit I unit) ->
    (u : %self(u). (P : Unit I unit -> Type) -> P unit -> P (I unit u)) ->
    %self(u). Unit I u
  )
  (unit : %self(unit). Unit I unit) =
  %self(u). (P : Unit I unit -> Type) -> P unit -> P (I unit u);


fix%Unit
  (A : %self(B).
    (c : %self(d). Unit B d) ->
    (e : %self(f). (P : Unit B c -> Type) -> P (%unroll c) -> P (B c f)) ->
    %self(g). Unit B g
  )
  (h : %self(i). Unit A i) =
  %self(j). (P : Unit A h -> Type) -> P (%unroll h) -> P (A h j);

Unit : Type
unit : Unit

Unit = %self(u). (P : Unit -> Type)

Unit0 W = %fix(Unit). (P : Unit -> Type) -> W Unit;
Unit1 = Unit0 (Unit => )

fix%Unit
  (I : %self(I).
    (unit : %self(unit). Unit I unit) ->
    (u : %self(u). (P : Unit I unit -> Type) -> P unit -> P (I unit u)) ->
    %self(u). Unit I u
  )
  (unit : %self(unit). Unit I unit) =
  %self(u). (P : Unit I unit -> Type) -> P unit -> P (I (%unroll u));

fix%Unit (unit : %self(unit). Unit unit) =
  %self(u). (P : Unit unit -> Type) -> P (%unroll unit) -> P u;
fix%unit : Unit unit =
  %fix(u). P => x => x;

Unit = Unit (%fix(unit). %fix(u). P => x => x);


fix%Unit =
  %self(u). (P : Unit -> Type) -> P (%fix(u). P => x => x) -> P u;

fix%Unit =
  %self(u). (P : Unit unit -> Type) -> P (%unroll unit) -> P u;

inductionBool : forall (P : ((A : Type) -> A -> A -> A) -> Type), P false -> P true -> forall (b : (A : Type) -> A -> A -> A), P b



%fix(Unit). %self(b). (P : Unit -> Type) ->
  P (%fix(unit). P => x => x) -> P b

Γ, x : %self(x). T |- T : Type
------------------------------
%self(x). T : Type

Γ, x == %fix(x). M |- M : T
----------------------------
%fix(x) : T. M : %self(x). T

Γ |- M : %self(x). T
---------------------
%unroll M : T[x := M]



```

```ocaml
type ty_term = TT_typed of { term : term; type_ : term }
and term =
  | TT_var of { var : Var.t }
  | TT_forall of { param : ty_pat; return : term }
  | TT_lambda of { param : ty_pat; return : term }
  | TT_apply of { lambda : term; arg : term }
  | TT_self of { bound : pat; body : term }
  | TT_fix of { bound : ty_pat; body : term }
  | TT_unroll of { term : term }

and ty_pat = TP_typed of { pat : pat; type_ : term }
and pat = TP_var of { var : Var.t }
```

# ChatGPT

## Compression

Makes GPT compress previous sessions, so that sessions can be merged.

## ChatGPT protocol

I like succint and direct conversations, avoid apologizing.

Working on the core calculus for a dependently typed programming language.

Common pattern for types

```

```

Session Header:

- Date: 2023-03-14
- Session ID: 5d53713f-87d4-4fa8-8480-655bb0e5467d
- User: EduardoRFS
- Language: English
- Topic: Implementing Smol in OCaml
- Special Instructions: Be succint and direct, avoid apologizing

Session Context:

Smol is the core for a dependently typed programming language, based on the Calculus of Construction and extendended with self types similar to how `Aaron Stump` and `Peng Fu` describe at `Self Types for Dependently Typed Lambda Encodings`.

The main changes from the Calculus of Construction are the following rules

```
-----------
Type : Type

Γ, x : %self(x). T |- T : Type
------------------------------
%self(x). T : Type

Γ, x == %fix(x). M |- M : T
----------------------------
%fix(x) : T. M : %self(x). T

Γ |- M : %self(x). T
---------------------
%unroll M : T[x := M]
```

The context was extended with definition equalities such that inside of the fixpoint, `x` is known to be equal to the fixpoint.

On the implementation, the current tree is defined as

```ocaml
type ty_term = TT_typed of { term : term; type_ : term }

and term =
  | TT_var of { var : Var.t }
  | TT_forall of { param : ty_pat; return : term }
  | TT_lambda of { param : ty_pat; return : term }
  | TT_apply of { lambda : term; arg : term }
  | TT_self of { bound : pat; body : term }
  | TT_fix of { bound : ty_pat; body : term }
  | TT_unroll of { term : term }

and ty_pat = TP_typed of { pat : pat; type_ : term }
and pat = TP_var of { var : Var.t }
```

```rust
module Context : sig
  type t
  type term

  val empty : t
  val extend_abstract : t -> Var.t -> term -> t
  val extend_alias : t -> Var.t -> term -> t
  val lookup : t -> Var.t -> term option
end
```

Assume

```ocaml
module Ttree : sig
  type ty_term = TT_typed of { term : term; type_ : term }

  and term =
    | TT_var of { var : Var.t }
    | TT_forall of { param : ty_pat; return : term }
    | TT_lambda of { param : ty_pat; return : term }
    | TT_apply of { lambda : term; arg : term }
    | TT_self of { bound : pat; body : term }
    | TT_fix of { bound : ty_pat; body : term }
    | TT_unroll of { term : term }

  and ty_pat = TP_typed of { pat : pat; type_ : term }
  and pat = TP_var of { var : Var.t }
end
module Subst : sig
  val subst_term : from:Var.t -> to_:Var.t -> Ttree.term -> Ttree.term
end
module Context : sig
  type context
  type t = context

  val initial : context
  val enter : Var.t -> Ttree.term -> context -> context
  val lookup : Var.t -> context -> Ttree.term option
end
```

Then implement

```ocaml
module Equal : sig
  val equal_term : Context.t -> received:Ttree.term -> expected:Ttree.term -> unit
end
```

## Internal Fix Again

```rust
%fix(False) : Type. %self(f). (P : %unroll False -> Type) -> P f


%fix(False) : Type. %self(f). (P : %unroll False -> Type) -> P f
%unroll (%fix(False\1) : Type. %self(f). (P : %unroll False -> Type) -> P f)

%self(f). (P : %unroll False -> Type) -> P f

%unroll (%fix(False\1) : Type. %self(f). (P : %unroll False -> Type) -> P f)

expected : %self(f). (P : %unroll False\0 -> Type) -> P f
received : f\1

expected : %self(f : %unroll False). (P : %unroll False -> Type) -> P f
received : %self(f : %unroll False). (P : %unroll False -> Type) -> P f


%fix(False\1) : Type. %self(f). (P : %unroll False -> Type) -> P f

expected : %unroll (%fix(False\1) : Type. %self(f). (P : %unroll False -> Type) -> P f)
received : %self(f). (P : %unroll False -> Type) -> P f

expected : %unroll (%fix(False) : Type. %self(f) : %unroll False/2. (P : %unroll False/1 -> Type) -> P f)
received : %self(f) : %unroll False/2. (P : %unroll False/1 -> Type) -> P f

expected : %self(f) : %unroll False/2. (P : %unroll False/1 -> Type) -> P f
received : %self(f) : %unroll False/2. (P : %unroll False/1 -> Type) -> P f

fix%Unit (unit : %self(unit). %self(u). Unit unit u) (u : %self(u). Unit unit u) =
  (P : (u : %self(f). Unit unit u) -> Type) -> P unit -> P u;

%fix(unit) : %self(u). Unit unit u.
  %fix(u) : Unit unit !u. (P : (u : %self(f). Unit unit u) -> Type) => (x : P (%unroll !unit)) => x

expected : P (%fix(u) : Unit unit !u. (P : (u : %self(f). Unit unit u) -> Type) => (x : P (%unroll !unit)) => x)
received : P (%fix(u) : Unit unit !u. (P : (u : %self(f). Unit unit u) -> Type) => (x : P (%unroll !unit)) => x)

expected : P u\0
received : P (%fix(u\0) : Unit unit u. (P : (u : %self(f). Unit unit u) -> Type) => (x : P (%unroll unit)) => x)

expected : %unroll False
received : %self(f). (P : %unroll False -> Type) -> P f

expected : %unroll False2
received : %self(f). (P : %unroll False1 -> Type) -> P f

expected : %unroll (%fix(False1). %self(f). (P : %unroll False1 -> Type) -> P f)
received : %self(f). (P : %unroll False1 -> Type) -> P f


expected : %self(f). (P : %unroll False1 -> Type) -> P f
received : %self(f). (P : %unroll False1 -> Type) -> P f

expected : %self(f). (P : %unroll (%frozen(False3). %self(f). (P : %unroll False3 -> Type) -> P f) -> Type) -> P f
received : %self(f). (P : %unroll (%frozen(False2). %self(f). (P : %unroll False2 -> Type) -> P f) -> Type) -> P f

P : %unroll (%expanded(False). %self(f). (P : %unroll False -> Type) -> P f) -> Type

expected : %unroll (%expanded(False). %self(f). (P : %unroll False -> Type) -> P f)
received : %self(f). (P : %unroll False -> Type) -> P f

expected : %self(f). (P : %unroll False -> Type) -> P f
received : %self(f). (P : %unroll False -> Type) -> P f

expected : %self(f). (P : %unroll (%expand (%frozen False)) -> Type) -> P f
received : %self(f). (P : %unroll (%expand False) -> Type) -> P f

expected : %unroll (%expanded(False). %self(f). (P : %unroll False -> Type) -> P f)
received : %self(f). (P : %unroll (%expanded(False). %self(f). (P : %unroll False -> Type) -> P f) -> Type) -> P f

expected : %self(f). (P : %unroll (%frozen(False). %self(f). (P : %unroll False -> Type) -> P f) -> Type) -> P f
received : %self(f). (P : %unroll (%expanded(False). %self(f). (P : %unroll False -> Type) -> P f) -> Type) -> P f

expected : %unroll (%frozen(False). %self(f). (P : %unroll () -> Type) -> P f)
received : %self(f). (P : %unroll (%frozen(False). %self(f). (P : %unroll () -> Type) -> P f) -> Type) -> P f

expected : %unroll False
received : %self(f). (P : %unroll False -> Type) -> P f

expected : %unroll (%fix(False). %self(f). (P : %unroll False -> Type) -> P f)
received : %self(f). (P : %unroll (%fix(False). %self(f). (P : %unroll False -> Type) -> P f) -> Type) -> P f

expected : %unroll (%fix(False). %self(f). (P : %unroll False -> Type) -> P f)
received : %self(f). (P : %unroll False -> Type) -> P f

expected : %self(f). (P : %unroll False -> Type) -> P f
received : %self(f). (P : %unroll False -> Type) -> P f

%fix(Unit). %self(u). (P : %unroll Unit -> Type) -> P (%fix(unit). P => x => x) -> P u

expected : %self(u). (P : %unroll Unit -> Type) -> P (%fix(unit). P => x => x) -> P u
received : %self(u). (P : %unroll Unit -> Type) -> P (%fix(unit). P => x => x) -> P u

%fix(Unit). %self(u). (P : %unroll Unit -> Type) -> P (%fix(unit). P => x => x) -> P u

%fix(unit) : %unroll Unit. P => x => x

expected : P (%fix(unit). P => x => x)
received : P (%fix(unit). P => x => x)


%frozen(Unit). %self(u). (P : %unroll Unit -> Type) -> P (%fix(unit). P => x => x) -> P u

expected : %unroll Unit
received : %self(u). (P : %unroll Unit -> Type) -> P (%fix(unit). P => x => x) -> P u

expected : %unroll (%fix(Unit). %self(u). (P : %unroll Unit -> Type) -> P (%fix(unit). P => x => x) -> P u)
received : %self(u). (P : %unroll Unit -> Type) -> P (%fix(unit). P => x => x) -> P u

expected : %self(u). (P : %unroll (%frozen(Unit). %self(u). (P : %unroll Unit -> Type) -> P (%fix(unit). P => x => x) -> P u) -> Type) -> P (%fix(unit). P => x => x) -> P u
received : %self(u). (P : %unroll Unit -> Type) -> P (%fix(unit). P => x => x) -> P u

T = %self(u). (P : Unit unit -> Type) -> P (%unroll unit) -> P u;
fix%unit

Unit0 (unit : %self(unit). Unit unit) =
  (P : (u : %self(u). Unit0 u) -> Type) -> P unit -> P unit;
fix%unit : Unit0 unit = P => x => x;


%self(unit). %self(u). Unit unit u
%self(u). Unit unit u

fix%Unit (unit : %self(unit). %self(u). Unit unit u) u =
  (P : (u : %self(u). Unit unit u) -> Type) -> P (%unroll unit) -> P u;

fix%unit : %self(u). Unit unit u =


fix%Unit =
  %self(u).
    (P : (u : %self(u). Unit unit u) -> Type) -> P (%unroll unit) -> P u

(P : Unit -> Type) -> P unit -> P u;

Ord = (Base : Type) -> (Wrap : Type -> Type) -> Type;
Zero : Ord = Base => Wrap => Base;
Succ (N : Ord) : Ord = Base => Wrap => Wrap (N Ord Base);

False0 = (A : Type) -> A;
FalseN (N : Ord) : Ord =
  N ((False : Type) -> (f : False) -> Type) False0
    (False => (f : False) => (P : False -> Type) -> P f);

False0 : (f0 : False) -> Type = f0 => (P : False -> Type) -> P f0;
False1 : (f0 : False) -> (f1 : False0 f0) -> Type = f0 => f1 => (P : False0 f0 -> Type) -> P f1;
False2 : (f0 : False) -> (f1 : False0 f0) -> (f2 : False1 f0 f1) -> Type
  = f0 => f1 => f2 => (P : False1 f0 f1 -> Type) -> P f2;

FalseT (N : Ord) = N Falsep

f0 => f1 => f (P : ((P : False0 -> Type) -> P f0) -> Type) -> P f1;
f (f (f False0))
f (f (f n))

x = (f : False 2 )
// TODO: generic base case for all types
Unit0 = (A : Type) -> (x : A) -> A;
unit0 : Unit0 = A => x => x;


Unit (N : Ord) = N Unit0 (Unit => => (P : ))



(A : Type) => (x : A) => x;

False False (f : False) = (P : False -> Type) -> P f;


(f : Self False)

%fix(False) : Type. %self(f : %unroll False).
  (P : %unroll False -> Type) -> P f;


%self(A, False : (f : A) -> Type).
  (f : %self(T, f : A). False f) -> Type;
%fix(Unit) : Type. %self(u : %unroll Unit).
  (P : %unroll Unit -> Type) ->
  P (%fix(unit). (P : %unroll Unit -> Type) => (x : P unit) => x) -> P u

%fix(Unit). %self(u). (P : Unit -> Type) -> P (%fix(unit). P => x => x) -> P u

Unit : Type;
unit : Unit;

Unit = %self(u). (P : Unit -> Type) -> P unit -> P u;

ind_unit : (u : Unit) -> (P : Unit -> Type) -> P unit -> P u;


(x : A) -> B : Type
%unroll u : (P : Unit -> Type) -> P unit -> P u


fix%Unit = (
  unit = %fix(unit : %unroll Unit). (P : %unroll Unit -> Type) => (x : P unit) => x;
  %self(u : %unroll Unit). (P : %unroll Unit -> Type) -> P unit -> P u
);

[%fix(unit). (P : Unit -> Type) => (x : P unit) => x : Unit,
 %self(u : Unit). (P : Unit -> Type) -> P unit -> P u === Unit]

%self(False : T). (f : %self(f). %unroll False f) -> Type

%fix(FalseT) : Type. %self(False : FalseT).
  (f : %fix(fT) : Type. %self(f : fT). (False : (f : fT) -> Type) f) -> Type

%fix(TFalse) : Type. %self(False : %unroll TFalse).
  (f : %fix(Tf) : Type.
    %self(f : %unroll Tf). %unroll (False : %self(False). (f : %unroll Tf) -> Type) f) -> Type

%fix(TFalse) : Type. %self(False : %unroll TFalse).
  (f : %fix(Tf) : Type.
    %self(f : %unroll Tf). %unroll (False : %self(False). (f : %unroll Tf) -> Type) f) -> Type

%fix(TFalse) : Type. %self(False : %unroll TFalse).
  (f : %fix(Tf) : Type, False : %self(False). (f : %unroll Tf) -> Type.
    %self(f : %unroll Tf). %unroll False f) -> Type

%self(False). (f : %self(f : %unroll Tf). %unroll False f) -> Type

%self(f : %unroll Tf). %unroll (False : %self(False). (f : %unroll Tf) -> Type) f === %unroll Tf


False : FalseT
FalseT === (f : fT) -> Type

%fix(unit). (P : Unit -> Type) => (x : P unit) => x
%self(u : Unit). (P : Unit -> Type) -> P unit -> P u


%fix(FalseT) : Type. %self(False : FalseT).
  (f : %fix(fT) : Type. %self(f : fT). (False : (f : fT) -> Type) f) -> Type

%fix(TFalse) : Type. %self(False : %unroll TFalse).
  (f : %fix(Tf) : Type, False : %self(False). (f : %unroll Tf) -> Type.
    %self(f : %unroll Tf). %unroll False f) -> Type

%fix(False).
  %self(f). (P : %unroll False -> Type) -> P f

fix%Unit (unit : %self(unit). %self(u). Unit unit u) u =
  (P : (%self(u). Unit unit u) -> Type) -> P (%unroll unit) -> P u;
fix%unit : %self(u). Unit unit u = P => x => (x : P u)
fix%Unit = (
  unit : %unroll (Unit 1) = %fix(u). P => x => x;
  %self(u). (P : %unroll (Unit 0) -> Type) -> P unit -> P (u : Unit 1)
);

%fix(Unit). (P : Unit -> Thype)
```

## Symbol Pushing

```rust
double = (x) => x + x;

double(2);
((x) => x + x)(2)
(x + x)[x := 2]
2 + 2
4

double(potato);
((x) => x + x)(potato);
(x + x)[x := potato]
potato + potato

double(2);
((x) => x + x)(2)
(x + x)[x := 2]
x[x := 2] + x[x := 2]
2 + 2
4

double(potato) === potato + potato

name(user) = user.name;


name({ name : "Eduardo" });

{ name : "Eduardo" }.name
"Eduardo"



double = (x) => x + x;

name = (user) => user.firstName + " " + user.lastName;


r0 : Register;
r1 : Register;
r2 : Register;
r3 : Register;

one = imm(r0, 1);
two = imm(r1, 1);
(three, one) = add(two, one);
(three, three) = mov(three, one);


```

## ChatGPT Teika

Teika is a dependently typed programming language with inductive types, the main syntax can be seen as described by the following rules:

- `Type` is the type of all types, the `Type` of `Type` is Type
- `(x : A) => m` is a function that accepts x of type A and return M of type B
- `(x : A) -> B` is a function type that accepts x of type A returns a type B
- `x = m; n` is a let that expresses that in n x will be equal to m

Induction of the booleans is defined in the context as following

```
Bool : Type;
true : Bool;
false : Bool;
ind_bool : (b : Bool) -> (P : Bool -> Type) -> (t : P true) -> (f : P false) -> P b;
```

## Counter Expansion

```rust

%fix(False). %self(f). (P : %unroll (%unfold False) -> Type) -> P f;

%fix(Unit). (
  unit : %unroll Unit = %fix(u). P => (x : P (%unfold u)) => (%fold x);
  %self(u). (P : %unroll (%unfold Unit)) -> P unit -> P u;
);

P (%fix(u). P => (x : P (%unfold u)) => (x : P u))
P u

%fix(Unit). (
  unit : %unroll Unit = %fix(u). P => (x : P u) => x;
  %self(u). (P : %unroll Unit -> Type) -> P unit -> P u;
);

expected : %unroll Unit



%fix(Unit). (
  unit : %unroll Unit = %fix(u). (P : %unroll Unit -> Type) => (x : P u) => x;
  %self(u). (P : %unroll Unit -> Type) -> P unit -> P u;
);

(P : %unroll Unit -> Type) -> P (%fix(u : %unroll Unit). (P : %unroll Unit -> Type) => (x : P u) => x) -> P (%fix(u : %unroll Unit). (P : %unroll Unit -> Type) => (x : P u) => x)
(P : %unroll Unit -> Type) -> P (%fix(u : %unroll Unit). (P : %unroll Unit -> Type) => (x : P u) => x) -> P (%fix(u : %unroll Unit). (P : %unroll Unit -> Type) => (x : P u) => x)

False = %fix(False). (f : %self(f). %unroll False f) =>
  (P : (%self(f). %unroll False f -> Type)) -> P f;
False = %self(f). (P : (%self(f). %unroll False f -> Type)) -> P f;

fix%Unit (unit : %self(unit). %self(u). Unit unit u) u =
  (P : (%self(u). Unit unit u) -> Type) -> P (%unroll unit) -> P u;
fix%unit : %self(u). : Unit unit u =
  %fix(u). P => (x : P (%unroll unit)) => (x : P u);


%fix(u). P => (x : P (%unroll unit)) => x

%fix(u). P => (x : P (%unroll unit)) => (x : P (%fix(u). P => (x : P (%unroll unit)) => x))

expected : P (%fix(u). P => (x : P (%unroll unit)) => x)
received : P u

%unroll (%fix(T) : Type. T)

%unroll (%fix(T) : Type. %unroll T)
%unroll (%frozen(T) : Type. %unroll T)
%fix(Unit).
%fix(Bool). (
  macro%true false = %fix(true : %unroll Bool). P => (t : P true) => (f : P false) => t;
  macro%false true = %fix(false : %unroll Bool). P => (t : P true) => (f : P false) => f;
  true = macro%true (macro%false true);
  false = macro%false (macro%true false);
  %self(b). (P : %unroll Bool -> Type) -> P true -> P false -> P b;
);

fix%Bool
  (true : %self(true). (false : %self(false). %self(b). Bool (%unroll true false) false b) -> %self(b). Bool (%unroll true false) false b)
  (false : %self(false). %self(b). Bool (%unroll true false) false b) (b : %self(b). Bool (%unroll true false) false b) =
  (P : (%self(b). Bool (%unroll true false) false b) -> Type) -> P (%unroll true false) -> P (%unroll false) -> P b;
fix%true (false : %self(false). %self(b). Bool (%unroll true false) false b) : %self(b). Bool (%unroll true false) false b
  = %fix(b). P => t => f => (t : P b);
fix%false : %self(b). Bool (%unroll true false) false b
  = %fix(b). P => t => f => (t : P b);
%fix(Bool). (
  true : %unroll Bool = %fix(true). P => (t : P true) => (f : P (%fix(false). P => (t : P true) => (f : P false) => f)) => t;
  false : %unroll Bool = %fix(false). P => (t : P (%fix(true). P => (t : P true) => (f : P false) => t)) => (f : P false) => f;
  %self(b). (P : %unroll Bool -> Type) -> P true -> P false -> P b;
);

expected : %self(b). (P : %unroll Bool -> Type) -> P true -> P false -> P b
received : %self(true). (P : %unroll Bool -> Type) => (t : P true) => (f : P (%fix(false). P => (t : P true) => (f : P false) => f)) => t


P (%unroll unit)
P (%unroll (%unfold unit))

P u
Unit = %self(u). Unit unit u;

unit (_ => Unit) unit == unit
(u : Unit) => u (u => u (_ => Unit) unit == unit);

(P : (%self(u). Unit unit u) -> Type) -> P (%unroll unit) -> P u
(P : (%self(u). Unit unit u) -> Type) -> P (%unroll unit) -> P u


(P : (%self(u). Unit unit u) -> Type) -> P (%unroll unit) -> P u
(P : (%self(u). Unit unit u) -> Type) -> P (%unroll unit) -> P u

expected : P (%fix(u). P => (x : P (%unroll unit)) => (x : P u))
received : P (%fix(u). P => (x : P (%unroll unit)) => (x : P u))

expected : %fix(u). P => (x : P (%unroll unit)) => (x : P u)
received : %fix(u). P => (x : P (%unroll unit)) => (x : P u)
(P => (x : P (%unroll (%unfold unit))) => x))

f = (u Ç )
expected : P (%fix(u). P => (x : P (%unfold u)) => x)
received : P (%fix(u). P => (x : P (%unfold u)) => x)

expected : %self(f). (P : %unroll False -> Type) -> P (f : %unroll False)
received : %self(f). (P : %unroll False -> Type) -> P (f : %unroll False)

expected : %unroll (%unfold False)
received : %unroll False
Γ, False == %fix(False). _A, f : %self(f). _B

(P : %unroll False -> Type) -> P f;

expected : _A[False := %fix(False). _A]
received : %self(f). _B


False = %fix(False 1).
  %self(f). (P : %unroll (False 0) -> Type) -> P (f : %unroll (False 1));

%self(f). (P : %unroll (False 0) -> Type) -> P (f : %unroll (False 0))
%self(f). (P : %unroll (False 0) -> Type) -> P (f : %unroll (False 1))

Unit = %fix(Unit 1). (
  unit : %unroll (Unit 1) = %fix(u). P => x => x;
  %self(u). (P : %unroll (Unit 0) -> Type) -> P unit -> P (u : Unit 1)
);

%unroll (%fix(Unit 0). (
  unit : %unroll (Unit 1) = %fix(u). P => x => x;
  %self(u). (P : %unroll (Unit 0) -> Type) -> P unit -> P (u : Unit 1)
))
```

## Functional is just algebra

```rust
1 + 2 = 3

2 - y = y
y + 2 - y = y + y
2 = 2y
2 / 2 = 2y / 2
1 = y

1 + 2 // 3

f = x => 1 + x;

f(2)
(x => 1 + x)(2)
(1 + x)[x := 2]
1 + 2

f(3)
(x => 1 + x)(3)
(1 + x)[x := 3]
1 + 3

0 + n = n
n + 1 = n

0
S(n) = n + 1

S(0) = 1
S(S(0)) = 2

zero = z => s => z;
succ = n => z => s => s(n(z)(s));

one = succ(zero);
two = succ(one);

add = n => m => n(m)(succ);

succ(succ(two));

one = z => s => s(z);
two = z => s => s(s(z));
three = z => s => s(s(s(z)));

P(n)
0
S(n)
```

## Linear Types Changed The World

```rust
Array.get : Array<A> -> Nat -> A;
arr => (Array.get(arr, 0), Array.get(arr, 1));

Array.get : Array<A> -> Nat -> (Array<A>, A);
arr => (
  (arr, x) = Array.get(arr, 0);
  (arr, y) = Array.get(arr, 1);
  (x, y)
);

x => x

x => x + x
```

## Back to no internal expansion

```rust
Unit : Type;
unit : Unit;
ind : (u : Unit) -> (P : Unit -> Type) -> P unit -> P u;
// internalizing induction


((n : Nat) -> P n -> P (succ n))

Nat = (A : Type) -> A -> (Nat -> A) -> A

Unit = (P : Unit -> Type) -> P unit -> P u;

%fix(Unit). %self(u). (P : Unit -> Type) -> P (%fix(u). P => x => x) -> P u;

// no self assumption for self

Γ, A : Type, x : A |- T : Type
------------------------------
%self(A, x). T : Type

Γ, A : Type, x : A |- M : T
------------------------------
%fix(A, x). M : %self(A, x). T

Γ |- M : %self(A, x). T
-------------------------------------------
%unroll M : T[A := %self(A, x). T | x := M]

%self(False, f). (P : False -> Type) -> P f;

%fix(Unit, unit). (P : Unit -> Type) => (x : P unit) => x;

%self(Unit, unit). (P : Unit -> Type) -> P unit -> P unit

%self(Unit, u). (P : Unit -> Type) -> P unit -> P u;

Unit = %self(Unit, u).  (P : Unit -> Type) -> P unit -> P u;

%self(U, unit). Unit unit;
%fix(U, u : ) :

(A : Type) => (x : A) =>
  (eq : (P : (A : Type) -> A -> Type) -> P Type A -> P Type Nat) =>
  eq (A => x => )


```

## Back to internal expansion

```rust

%fix(Unit). %self(u : %unroll (%unfold Unit)). (
  unit = %fix(u : %unroll (%unfold Unit)). (P : %unroll (%unfold Unit) -> Type) => (x : P (%unfold u)) => x;
  (P : %unroll (%unfold Unit) -> Type) -> P unit -> P (%unfold u)
);

fix%Unit (unit : %self(unit). %self(u). Unit unit u) (u : %self(u). Unit unit u) =
  (P : (u : %self(u). Unit unit u) -> Type) -> P (%unroll unit) -> P u;
unit = %fix(unit). %fix(u) : Unit unit u.
  (P : (u : %self(u). Unit unit u) -> Type) => (x : P (%unroll unit)) => (x : P u);
Unit = %self(u). Unit unit u;

fix%Unit = %self(u). (P : Unit -> Type) -> P (%fix(u : Unit). (P : Unit -> Type) -> P u -> P u) -> P u

expected : (P : Unit -> Type) -> P (%fix(u : Unit). (P : Unit -> Type) -> P u -> P u) -> P (%fix(u : Unit). (P : Unit -> Type) -> P u -> P u)
received : (P : Unit -> Type) -> P (%fix(u : Unit). (P : Unit -> Type) -> P u -> P u) -> P (%fix(u : Unit). (P : Unit -> Type) -> P u -> P u);

expected : P (%fix(u) : Unit unit u. (P : (u : %self(u). Unit unit u) -> Type) => (x : P (%unroll unit)) => (x : P u))
received : P (%fix(u) : Unit unit u. (P : (u : %self(u). Unit unit u) -> Type) => (x : P (%unroll unit)) => (x : P u))

pred => case (%unfold pred)
(P : %unroll (%unfold Unit) -> Type) -> P unit -> P (%unfold u)
(P : %unroll (%unfold Unit) -> Type) -> P (%unfold u) -> P (%unfold u)

fix%False = %self(f). (P : %unfold False -> Type) -> P f;

expected : %unfold False;
received : %self(f). (P : %unfold False -> Type) -> P f;

fix%Unit (unit : %self(unit). %self(u). Unit unit u) (u : %self(u). Unit unit u) =
  (P : Unit -> Type) -> P (%unroll unit) -> P u;
fix%unit : %self(u). Unit (%unfold unit) u = %fix(u) : Unit (%unfold unit) u.
  (P : Unit -> Type) => (x : P (%unroll (%unfold unit))) => x;

fix%unit : %self(u). Unit (%unfold unit) u =
  %fix(u) : Unit (%unfold unit) u. P => x => x;

(P : Unit -> Type) -> P (%unroll (%unfold unit)) -> P (%unroll (%unfold unit));
(P : Unit -> Type) -> P (%unroll (%unfold unit)) -> P

fix%Unit = %self(u). (P : %unfold Unit -> Type) ->
  P (%fix(unit). P => x => x) -> P u;

fix%Unit = %self(u). (P : %unfold Unit -> Type) ->
  P (%fix(unit). P => (x : P unit) => x) -> P u;

Γ, x : %self(x). T |- T : Type
------------------------------
%self(x). T : Type

Γ, x == %fix(x). M |- M : T
---------------------------
%fix(x). M : %self(x). T

Γ |- M : %self(x). T
---------------------
%unroll M : T[x := M]

fix%Unit = %self(u). (P : %unfold Unit -> Type) ->
  P u -> P (%fix(u). P => (x : P u) => (x : P (%fix(u). P => (x : P u) => x)));

symm eq = eq (x => x);

fix%Unit = %self(u). (P : Unit -> Type) ->
  P (%fix(u). P => (x : P u) => x)
u == unit
expected : P (%fix(u). P => (x : P u) => x)
received : P (%fix(u). P => (x : P u) => x)
(P : %unfold Unit -> Type) -> P unit -> P u
(P : %unfold Unit -> Type) -> P (%fix(unit). P => (x : P unit) => x) -> P u

%fix(unit). (P : %unfold Unit -> Type) => (x : P unit) => (x : P unit)

%fix(unit). (P : %unfold Unit -> Type) => (x : P unit) => (x : P unit)

%self(unit). (P : %unfold Unit -> Type) -> P unit -> P unit


M : %self(x). T

received : P unit
expected :

received : P (%fix(unit). P => x => x)
expected : P (%fix(unit). P => x => x)

fix%Unit =
  %self(u). (P : !Unit -> Type) -> P u^ -> P (%fix(u). P => (x : P u^) => !x);

expected : (P : Unit -> Type) -> P u^ -> P (%fix(u). P => (x : P u^) => !x)
received : (P : Unit -> Type) -> P u^ -> P (%fix(u). P => (x : P u^) => !x)

fix%Unit =
  %self(u). (P : !Unit -> Type) -> P (%fix(u). P => (x : P u) => [x]) -> P u^;

expected : (P : !Unit -> Type) -> P (%fix(u). P => (x : P u) => [x]) -> P u^
received : (P : !Unit -> Type) -> P u -> P u


fix%Unit = %self(u).
  (P : Unit -> Type) -> P [u] -> P (%fix(u). P => (x : P [u]) => x);

expected : (P : (!Unit)^ -> Type) -> P [u] ->
received : (P : (!Unit)^ -> Type) -> P [u] -> P [u]

fix%Unit = %self(u).
  (P : %unroll Unit -> Type) -> P (%fix(u). P => (x : P u) => x) -> P u;

(u : Unit) => %unroll u ()
%unroll Unit
(P : %unroll Unit -> Type) -> P (%fix(u). P => (x : P u) => x) -> P u

%unroll Unit
P (%fix(u). P => (x : P u) => x) -> P (%fix(u). P => (x : P u) => x)
P (%fix(u). P => (x : P u) => x) -> P (%fix(u). P => (x : P u) => x)

fix%False = %self(f). (P : %unroll (%unfold False) -> Type) -> P f;

%unroll (%fix(False). %self(f). (P : %unroll (%unfold False) -> Type) -> P f)

fix%Unit (unit : %self(unit). %self(u). Unit unit u) (u : %self(u). Unit unit u)
  = (P : (u : %self(u). Unit unit u) -> Type) -> P (%unroll unit) -> P u;
fix%unit : %self(u). Unit unit u = %fix(u) : Unit unit u.
  P => (x : P u) => x;

expected : P u -> P u
received : P (%unroll (%unfold unit)) -> P u

%fix(T) : Type. %unroll !T

%fix(Nat). (A : Type) -> A -> (%unroll Nat -> A) -> A;

%fix(Nat). (A : Type) -> A -> (%unroll (%fix(Nat). (A : Type) -> A -> (Nat -> A) -> A) -> A) -> A

%fix(Nat). (A : Type) -> A -> (%unroll !Nat -> A) -> A;
%fix(Nat). (A : Type) -> A -> (!Nat -> A) -> A;

| (tag : true, A)
| (tag : false, B)



```

## Effect

```typescript
type Return<A> =
  | { tag: "tail"; thunk: () => Return<A> }
  | { tag: "value"; return: A };

const $call = (x) => {
  while ($continue !== null) {
    x = $handler();
  }
  return x;
};

function* () {
  yield f(x);
}

f(x);

// TODO: benchmark against dumb function
// TODO: benchmark techniques such as full CPS to test engine inliners
$tail = true;
$continue = () => {};

const $apply = (x) => {
  while ($tail) {
    $tail = false;
    x = $continue();
  }
  return x;
};

$apply(f)(x);

let f = fun [@async] x -> 1

function kid(x, k) {
  return k(x);
}
function kid(x, k) {
  return $next(k, x);
}

let kid x k = k x
let rec rev acc l =
  match l with
  | [] -> acc
  | el :: tl -> rev (el :: acc) tl
```

## Controlled Unfolding

```rust
fix%Unit (unit : %self(unit). %self(u). %unroll Unit unit u)
  (u : %self(u). %unroll Unit unit u)
  = (P : (%self(u). %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

u : %self(u). %unroll Unit (%fix(unit). u) u
  = %fix(u). P => (x : P (%fix(unit). u)) => x;


expected : %self(u). (P : (%self(u). %unroll Unit (%fix(unit). u) u) -> Type) -> P (%unroll (%fix(unit). u)) -> P u
received : %self(u). (P : (%self(u). %unroll Unit (%fix(unit). u) u) -> Type) -> P (%unroll (%fix(unit). u)) -> P (%unroll (%fix(unit). u))

expected : P u -> P u
received : P u -> P u

expected : P u -> P u
received : P u -> P u

(P : )
(P : (%self(u). Unit unit u) -> Type) -> P unit -> P u
Unit (%fix(unit). u) u


u : %self(u). Unit (%fix(unit). u) u =
  %fix(u). (P : (%self(u). Unit (%fix(unit). u) u) -> Type) => (x : P (%fix(unit). u)) => x;
unit2 : %self(unit). %self(u). Unit unit u = %fix(unit3). u;

expected : %self(u). Unit (%fix(unit3). u) u
received : %self(u). Unit (%fix(unit). u) u

expected : (P : (%self(u). Unit (%fix(unit). u) u) -> Type) -> P (%fix(unit). u) -> P (%fix(unit). u)
received : (P : (%self(u). Unit (%fix(unit). u) u) -> Type) -> P (%fix(unit). u) => P (%fix(unit). u)

expected : %self(u). Unit (%fix(unit). u) u
received : %self(u). Unit (%fix(unit). u) u;

fixunit (u : %self(u). Unit (unit u) u) = u;

x = %fix(u) : Unit (unit u) u. P => (x : P (unit u)) => x;

(P : (%self(u). Unit (unit b) u) -> Type) -> P b -> P b
(P : (%self(u). Unit (unit b) u) -> Type) -> P b -> P b

unit : %self(unit). %self(u). Unit unit u;



fix%Unit (unit : %self(unit). %self(u). Unit unit u) (u : %self(u). Unit unit u) =
  (P : (%self(u). Unit unit u) -> Type) -> P (%unroll unit) -> P u;

fix%unit = %fix(u) : Unit unit u. P => (x : P (%unroll unit)) => x;

fix%Unit (unit : %self(unit). %self(u). Unit unit u) (u : %self(u). Unit unit u) =
  (P : Unit unit u -> Type) -> P (%unroll (%unroll unit)) -> P (%unroll u);

fix%unit = %fix(u) : Unit unit u. P => (x : P (%unroll unit)) => x;

%fix(u). P => (x : P (%frozen unit)) => x

%self(u). (P : (%self(u). Unit unit u) -> Type) -> P (%fix(u). P => (x : P (%frozen unit)) => x) -> P (%fix(u). P => (x : P (%frozen unit)) => x)
%self(u). (P : (%self(u). Unit unit u) -> Type) -> P (%fix(u). P => (x : P (%frozen unit)) => x) -> P (%fix(u). P => (x : P (%frozen unit)) => x)

fix%Unit : Type = (
  fix%unit : Unit = (P : Unit -> Type) => (x : P unit) => x;
  %self(u). (P : Unit -> Type) -> (x : P unit) -> P u;
);

Unit : Type;
unit : Unit;

Unit = %self(u). (P : Unit -> Type) -> P unit -> P u;
unit = (P : Unit -> Type) => (x : P unit) => x

fix%False (f : %self(f). False f) =
  (P : (%self(f). False f) -> Type) -> P f;

False = %self(f). False f;


Unit : %self(Unit). (u : %self(u). Unit u) -> Type;
unit : %self(u). Unit u;

Unit = u => (P : (%self(u). Unit u) -> Type) -> P unit -> P u;
unit = (P : (%self(u). Unit u) -> Type) => (x : P unit) => x;

%fix(u). P => x => x

%self(u). (P : (%self(u). Unit u) -> Type) -> P (%fix(u). P => x => x) -> P (%fix(u). P => x => x)
%self(unit). (P : (%self(u). Unit u) -> Type) -> P (%fix(u). P => x => x) -> P (%fix(u). P => x => x)


%fix(M : {
  Unit : %self(Unit). (u : %self(u). Unit u) -> Type;
  unit : %self(u). Unit u;
}). {
  Unit = (u : %self(u). M.Unit u) => (P : (%self(u). M.Unit u) -> Type) -> P M.unit -> P u;
  unit = (P : (%self(u). M.Unit u) -> Type) => (x : P M.unit) => x;
};

%fix(M : {
  Unit : Type;
  unit : Unit;
}). {
  Unit = %self(u).(P : (%self(u). M.Unit u) -> Type) -> P M.unit -> P u;
  unit = (P : (%self(u). M.Unit u) -> Type) => (x : P M.unit) => x;
};

fix%Unit = %self(u). (P : Unit -> Type) -> P (%fix(u). P => x => x) -> P u;

%self(u). (P : Unit -> Type) -> P (%fix(u). P => x => x) -> P u

u : %self(u). _A
Unit : Type


%self(u) : Unit. P => (x : P u) => x
%fix(u) : Unit. P => (x : P u) => x

%self(u). {A} {B}. A -> B -> B

expected : (P : (%self(u). Unit u) -> Type) -> P (%fix(u). P => x => x) -> P (%fix(u). P => x => x)
received : (P : (%self(u). Unit u) -> Type) -> P (%fix(u). P => x => x) -> P (%fix(u). P => x => x)


%self(unit). (P : (%self(u). Unit u) -> Type) -> P unit3 -> P unit
%self(unit2). (P : (%self(u). Unit u) -> Type) -> P unit2 -> P unit2
%fix(Unit : Type; unit : Unit). (
  %self(u). (P : Unit -> Type) -> P unit -> P u;
);

expected :

%self(a). (P : Unit -> Type) -> (x : P (%fix(c). P => (x : P c) => x)) -> P a
%self(b). (P : Unit -> Type) -> (x : P b) -> P b

(P : Unit -> Type) -> (x : P (%fix(c). P => (x : P c) => x)) -> P d
(P : Unit -> Type) -> (x : P d) -> P d


fix%Unit (unit : %self(unit). %self(u). %unroll Unit unit u) (u : %self(u). %unroll Unit unit u)
  = (P : (%self(u). %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

T = %self(u). %unroll Unit (%fix(unit). u) u;

T = %self(u). %unroll Unit (%fix(unit) : %self(u). %unroll Unit unit u. u) u

u : %self(u). %unroll Unit (%fix(unit) : %self(u). %unroll Unit unit u. u) u


fix%unit : %self(u). %unroll Unit unit u =
  %fix(u) : %unroll Unit (%fix(unit). u) u. P => x => x;

expected : %self(u). %unroll Unit unit u
received : %self(u). %unroll Unit (%fix(unit). u) u

expected : %self(unit). %self(u). %unroll Unit unit u
received : %self(unit). %self(u). %unroll Unit (%fix(unit). u) u

expected : %self(unit). %self(u). (P : (%self(u). %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u
received : %self(unit). %self(u).
expected : %self(u). %unroll Unit (%fix(unit). u) u
received : (P : (%self(u). %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u

%self(t). P (%unroll (%fix(x). t)) -> P t
%self(t). P (%unroll (%fix(x). t)) -> P (%unroll (%fix(x). t))

fix%Unit (unit : %self(unit). %self(u). %unroll Unit unit u) (u : %self(u). %unroll Unit unit u)
  = (P : (%self(u). %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

u : %self(u). %unroll Unit (%fix(unit). %fix(u). P => x => x) u
%unroll Unit (%fix(unit). %fix(u). P => x => x) u

%fix(unit). %fix(u) : %unroll Unit unit u. P => x => x

expected : P (%unroll unit) -> P (%fix(u). P => x => x)
received :
expected : %self(unit). %self(u). %unroll Unit unit u
received :
u : %self(u). %unroll Unit (%fix(unit). %fix(u). P => x => x) u
  = %fix(u). P => x => x;

unit : %self(unit). %self(k). %unroll Unit unit k = %fix(unit). u;

expected : %unroll Unit (%fix(unit). %fix(u). P => x => x) u
received : %unroll Unit (%fix(unit). %fix(u). P => x => x) u

expected : %unroll Unit (%fix(unit). %fix(u). P => x => x) u
received :

fix%unit : %self(u). %unroll Unit unit u =
  %fix(u) : %unroll Unit (%fix(unit). u) u. P => x => x;

expected : %self(u). %unroll Unit (%fix(unit). ) u
received : %self(u). %unroll Unit (%fix(unit). u) u


fix%Unit (unit : %self(unit). %self(u). %unroll Unit unit u) (u : %self(u). %unroll Unit unit u)
  = (P : (%self(u). %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;


u : %self(u). %unroll Unit (%fix(unit) : %self(u). %unroll Unit unit u. u) u

expected : %self(u). %unroll Unit unit u
received : %self(u). %unroll Unit (%fix(unit) : %self(u). %unroll Unit unit u. u) u

%self(u). %unroll Unit (%fix(unit) : %self(u). %unroll Unit unit u. u) u

%fix(T). %self(u). %unroll Unit (%fix(unit) : %unroll T. u) u

u : %self(u). %unroll Unit (%fix(unit). u) u
  = %fix(u). P => x => x;

unit
  : %self(unit). %self(u). %unroll Unit unit u
  = %fix(unit). u;

expected : %self(u). %unroll Unit (%fix(unit). u) u
received : %self(u). %unroll Unit (%fix(unit). u) u

fix%Unit (unit : %self(unit). %self(u). %unroll Unit unit u) (u : %self(u). %unroll Unit unit u)
  = (P : (%self(u). %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

u : %self(u). %unroll Unit (%fix(unit). u) u
  = %fix(u). P => x => x;

unit
  : %self(unit). %self(u). %unroll Unit unit u
  = %fix(unit). u

u : %self(u). _A
unit : %self(unit). _B

_B : %self(u). _A
%self(unit). %self(u). _A : %self(unit). %self(u). %unroll Unit unit u


u : %self(u). %unroll Unit (%fix(unit) : %self(u). %unroll Unit unit u. u) u

expected : %self(u). %unroll Unit (%fix(unit) : %self(u). %unroll Unit unit u. u) u
received : %self(u). %unroll Unit (%fix(unit) : %self(u). %unroll Unit unit u. u) u


fix%Unit (unit : %self(unit). %self(u). %unroll Unit unit u) (u : %self(u). %unroll Unit unit u)
  = (P : (%self(u). %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

u : %self(u). %unroll Unit (%fix(unit) : %self(u). %unroll Unit unit u. u) u
  = %fix(u). P => x => x;

unit = %fix(unit) : %self(u). %unroll Unit unit u. u;

Unit2 = %self(u). Unit unit u;

expected : %self(u). %unroll Unit (%fix(unit) : %self(u). %unroll Unit unit u. u) u
received : %self(u). %unroll Unit (%fix(unit) : %self(u). %unroll Unit unit u. u) u


unit = %fix(unit) : %self(u). %unroll Unit unit u.
  %fix(u). (P : (%self(u). %unroll Unit unit u) -> Type) => (x : P (%unroll unit)) => x

expected : %self(u). (P : (%self(u). %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u
received : %self(k). (P : (%self(u). %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P (%unroll unit);
%self(u). %unroll Unit (%fix(unit). %fix(u). P => x => x) u;

%unroll Unit ? u
u = %fix(u) : %unroll Unit ? u. P => x => x;
unit =


fix%Unit (unit : %self(unit). %self(u). %unroll Unit unit u) (u : %self(u). %unroll Unit unit u)
  = (P : (%self(u). %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

u = %fix(u) : %unroll Unit (%fix(unit). u) (%fix(u). P => x => x). P => x => x;

%self(unit). %self(u). %unroll Unit unit (%fix(u) : %unroll Unit unit u. P => x => x)

expecected : %unroll Unit unit (%fix(u) : %unroll Unit unit u. P => x => x)
received : %unroll Unit unit unit

%self(u). %unroll Unit
  (%fix(unit) : %self(u). %unroll Unit unit (%fix(u). P => x => x). u)
  (%fix(u). P => x => x)

expected : %self(u). %unroll Unit (%fix(unit) : %self(u). %unroll Unit unit u. u) u
received : %self(u). %unroll Unit (%fix(unit) : %self(u). %unroll Unit unit u. u) (%fix(u). P => x => x)

expected : %unroll Unit (%fix(unit). %fix(u). P => x => x) (%fix(u). P => x => x)
received : %unroll Unit (%fix(unit). %fix(u). P => x => x) (%fix(u). P => x => x)
expected : P u -> P (%fix(u). P => x => x)
received : P u -> P u

unit = %fix(unit) : %self(u). %unroll Unit unit u.
  %fix(u). P => x => x;

expected : %self(u). %unroll Unit (%fix(unit) : %self(u). %unroll Unit unit u. %fix(u) : %unroll Unit (%fix(unit) : %self(u). %unroll Unit unit u. u) u. P => x => x) u
received : %self(u). %unroll Unit (%fix(unit) : %self(u). %unroll Unit unit u. u) u

Unit2 = %self(u). Unit unit u;
```

## Lambda Cube

```rust
Term =
  | Type
  | x
  | (x : Term) -> Term
  | (x : Term) => Term
  | Term Term;

-----------
Type : Type

A : Type  x : A
---------------
x : A

Γ, x : A ⊢ B : Type
-------------------
Γ ⊢ (x : A) -> B : Type

Γ, x : A ⊢ m : B
---------------------------
Γ ⊢ (x : A) => m : (x : A) -> B

m : (x : A) -> B  n : A
-----------------------
Γ ⊢ m n : B[x := n]


PiSelf:

Term =
  | Type
  | x
  | (x : Term) -> Term
  | (x : Term) => Term
  | Term Term
  | %self(x). Term
  | %fix(x). Term
  | %unroll Term;

Γ, x : %self(x). T ⊢ T : Type
-----------------------------
Γ ⊢ %self(x). T : Type

Γ, x : %self(x). T ⊢ m : T
----------------------------
Γ ⊢ %fix(x). m : %self(x). T

Γ ⊢ m : %self(x). T
-------------------------
Γ ⊢ %unroll m : T[x := m]


call f x = f x;

{N M A B}. (f : (x : A N) -> B M) -> (x : N) -> B M;

double = (x : Nat) : Nat => x + x;

id = (A : Type 0) => (x : A) => x;

Array : {
  @Array : Type;
  make : (n : Nat) -> $Array;
} = {

};

Socket : {
  @Socket : Type;
  connect : () -> $Socket;
  close : (socket : $Socket) -> ();
} = {};

List (A : Type) =

channel : Rc<Channel>;
2 buf : (socket : Socket, nat : Nat 15);

(A : Type)
Channel : {
  @Channel : Type;
  make : (n : Grade) -> Channel n;
} = {};

M : {
  double : (x : Nat) -> Nat;
} = {
  double = x => x + x;
}


%rec(f : Unit ->! Unit). f ()

Fix = %fix(Fix). Fix -> Never;

Unit = %fix(Unit).
  %self(u). (P : %unroll Unit -> Type) -> P (%fix(u). P => x => x) -> P u;

fix%Unit (unit : %self(unit). %self(u). %unroll Unit unit u) (u : %self(u). %unroll Unit unit u)
  = (P : (%self(u). %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;
unit : %self(unit). %self(u). %unroll Unit unit u = %fix(unit). %fix(u). P => x => x;


fix%False (f : %self(f). False f) = (P : False -> Type) -> P f;
False = %self(f). False f;


Unit = %self(u). Unit unit u;
unit : Unit = %unroll unit;

self%Unit : (self%u : %unroll Unit u) -> Type;
self%unit : %unroll Unit unit;

Unit (self%u : %unroll Unit u) = (P : (self%u : %unroll Unit u) -> Type) -> P unit -> P u;
unit = (P : (self%u : %unroll Unit u) -> Type) => (x : P unit) => x;

fix%M : {
  self%Unit : (self%u : %unroll Unit u) -> Type;
  self%unit : %unroll Unit unit;
} = {
  Unit (self%u : %unroll M.Unit u) = (P : (self%u : %unroll M.Unit u) -> Type) -> P M.unit -> P u;
  unit = (P : (self%u : %unroll M.Unit u) -> Type) => (x : P unit) => x;
};

(u : %self(u). %unroll Unit u) -> Type

expected : (P : (%self(u). %unroll Unit u) -> Type) -> P unit -> P unit
received : (P : (%self(u). %unroll Unit u) -> Type) -> P unit -> P unit

expected : %self(u). (P : %unroll Unit -> Type) -> P (%fix(u). P => x => x) -> P u
received : %self(u). (P : %unroll Unit -> Type) -> P (%fix(u). P => x => x) -> P u

expected : %self(u). (P : Unit -> Type) -> P (%fix(u). P => x => x) -> P u
received : %self(u). (P : Unit -> Type) -> P (%fix(u). P => x => x) -> P u

Value -> Value;
Type -> Value;
Type -> Type;
Value -> Type;

Consistency = Soundness + Strong Normalization;

(x : Int) => x;

Never = (A : Type) -> A;


id = (x : Int) => x;
id = (A : Type) => (x : A) => x;
Id = (A : Type) => A;

fold : (A : Type) -> (l : List A) ->
  (K : Type) -> (cons : K -> A -> K) -> (nil : K) -> K;

List A = (K : Type) -> (cons : K -> A -> K) -> (nil : K) -> K;
cons {A} el tl : List A = K => cons => nil => cons (tl K cons nil) el;
nil {A} el tl : List A = K => cons => nil => nil;

one_two_three = cons 1 (cons 2 (cons 3 nil));
one_two_three = K => cons => nil => cons (cons (cons nil 3) 2) 1;
x = one_two_three Int (acc => el => acc + el) 0;

if : (pred : Bool) -> (A : Type) -> (then : A) -> (else : A) -> A;

if pred return Int then 1 else 2
if pred return String then "a" else "b"

Bool = (A : Type) -> (then : A) -> (else : A) -> A

Unit = (A : Type) -> (x : A) -> A;
unit : Unit = A => x => x;

Bool = (A : Type) -> (then : A) -> (else : A) -> A;
true : Bool = (A : Type) => (then : A) => (else : A) => then;
false : Bool = A => then => else => else;

not : Bool -> Bool = pred => pred Bool false true;

Nat = (A : Type) -> (zero : A) -> (succ : A -> A) -> A;
zero : Nat = A => zero => succ => zero;
succ : Nat -> Nat = n => A => zero => succ => succ (n A zero succ);

Eq A x y = (P : A -> Type) -> P x -> P y;
refl A x : Eq A x x = (P : A -> Type) => (p : P x) => p;

Neq A x y = (eq : Eq A x y) -> Never;

_ : (n : Nat) -> Neq Nat n (succ n) = _;

_ : Eq Bool true (not false) = refl Bool true;

Eq Bool true true

call_id = (id : (A : Type) -> A -> A) => (id Int 1, id String "a");

fold : (n : Nat) -> (A : Type) -> A -> (A -> A) -> A;

nat_ind : (n : Nat) ->
  (P : Nat -> Type) -> P z -> ((pred : Nat) -> P pred -> P (s pred)) -> P n;

fold n A (z : A) (s : A -> A) = nat_ind n (_ => A) z (_ => s);

Nat =
  (P : Nat??? -> Type) -> P (z : Nat) -> ((pred : Nat???) -> P pred -> P (s pred)) -> P n???;



Unit = %fix(Unit).
  %self(u). (P : Unit -> Type) -> P (%fix(u). P => x => x) -> P u;
unit : Unit = %fix(u). P => x => x;

Eq A x y = (P : A -> Type) -> P x -> P y;

%unroll unit : (P : Unit -> Type) -> P (%fix(u). P => x => x) -> P unit

x = unit (_ => Int);

%self(u). (P : T -> Type) -> P u -> P u

(%fix(u). (P : T -> Type) => (x : P u) => x) :
  (P : T -> Type) -> P u -> P u;

(%unroll )

Nat = %fix()

forall P : nat -> Type,
  P 0 ->
  (forall n : nat, P n -> P (S n)) -> forall n : nat, P n

fix%Unit (unit : %self(unit). %self(u). %unroll Unit unit u) (u : %self(u). %unroll Unit unit u)
  = (P : (%self(u). %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;
unit : %self(unit). %self(u). %unroll Unit unit u = %fix(unit). %fix(u). P => x => x;

self%Unit : (self%u : %unroll Unit u) -> Type;
self%unit : %unroll Unit unit;

Unit (self%u : %unroll Unit u) =
  (P : (self%u : %unroll Unit u) -> Type) -> P unit -> P u;
unit = (P : (self%u : %unroll Unit u) -> Type) => (x : P unit) => x;

self%Bool : (self%b : %unroll Bool b) -> Type;
self%true : %unroll Bool true;
self%false : %unroll Bool false;

Bool (self%b : %unroll Bool b) =
  (P : (self%b : %unroll Bool b) -> Type) -> P true -> P false -> P b;
true = (P : (self%b : %unroll Bool b) -> Type) =>
  (then : P true) => (else : P false) => then;
false = (P : (self%b : %unroll Bool b) -> Type) =>
  (then : P true) => (else : P false) => else;

fix%False = %self(f). (P : False -> Type) -> P f;
fix%False (self%f : False f) = (P : (self%f : False f) -> Type) -> P f;


False = %fix(False). %self(f). (P : %unroll False -> Type) -> P f;

expected : %unroll False
received : %self(f). (P : %unroll False -> Type) -> P f

T = %fix(False). (self%f : %unroll False f) =>
  (P : (self%f : %unroll False f) -> Type) -> P f;

False = %self(f).
  (P : (self%f : %unroll (
    %fix(False). (self%f : %unroll False f) =>
      (P : (self%f : %unroll False f) -> Type) -> P f
  ) f) -> Type) -> P f;

False = %fix(False). %self(f). (P : %unroll False -> Type) -> P f;
False = %self(f). %unroll (
  %fix(False). (self%f : %unroll False f) =>
    (P : (self%f : %unroll False f) -> Type) -> P f
) f;

fix%Unit (self%unit : %self(u). %unroll Unit unit u) (self%u : %unroll Unit unit u) =
  (P : (self%u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

%self(Unit). (a : %self(b). %unroll Unit b b) ->
  (u : %self(u). %unroll Unit a u) -> Type;

%self(b). (%unroll Unit b) b

expecetd : %unroll Unit b c
received : %unroll Unit c c

%self(Unit). (a : %self(b). %unroll Unit b b) ->
  (u : %self(u). %unroll Unit a u) -> Type

%unroll Unit b

expected : %unroll Unit unit unit
received : %unroll Unit a unit

fix%Unit (self%unit : %unroll Unit unit unit)
  (self%u : %unroll Unit unit u) =
  (P : (self%u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;


Unit = %self(u). %unroll (
  %fix(Unit). (self%u : %unroll Unit u) =>
    (P : (self%u : %unroll Unit u) -> Type) ->
    P (%fix(unit). P => (x : P unit) => x) ->
    P u
) u;

Unit = %fix(Unit).
  (P : %unroll Unit -> Type) -> P ? -> P u;
unit = %fix(unit) : %self(u). %unroll Unit unit u.
  %fix(u). P => (x : P (%unroll unit)) => (x : P u);

unit = ;

Unit1 = %fix(Unit1). (self%unit : %self(u). %unroll Unit1 unit u) => (self%u : %unroll Unit1 unit u) =>
  (P : (self%u : %unroll Unit1 unit u) -> Type) -> P (%unroll unit) -> P u;

Unit2 = %self(u). %unroll (
  %fix(Unit1). (self%unit : %self(u). %unroll Unit1 unit u) => (self%u : %unroll Unit1 unit u) =>
    (P : (self%u : %unroll Unit1 unit u) -> Type) -> P (%unroll unit) -> P u
) (%fix(A).?) u;

Unit2 = (
  (self%unit : %self(u). %unroll Unit1 unit u) => (self%u : %unroll Unit1 unit u) =>
    (P : (self%u : %unroll Unit1 unit u) -> Type) -> P (%unroll unit) -> P u
) (%fix(unit). );

fix%Unit (self%unit : %self(u). %unroll Unit unit u) (self%u : %unroll Unit unit u) =
  (P : (self%u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

expected : %unroll (
    %fix(False). (self%f : %unroll False f) =>
      (P : (self%f : %unroll False f) -> Type) -> P f
  ) f
expected : (P : (self%f : %unroll (
    %fix(False). (self%f : %unroll False f) =>
      (P : (self%f : %unroll False f) -> Type) -> P f
  ) f) -> Type) -> P f
received : (P : (self%f : %unroll (
    %fix(False). (self%f : %unroll False f) =>
      (P : (self%f : %unroll False f) -> Type) -> P f
  ) f) -> Type) -> P f

expected : (P : (self%f : %unroll T f) -> Type) -> P f
received : (P : (self%f : %unroll T f) -> Type) -> P f

%self(f). %unroll (%(P : %unroll False -> Type) -> P f)

expected : %unroll False
received : %self(f). %unroll (%fix(False). (P : %unroll False -> Type) -> P f)

False = %self(f).
  (P : (f : %self(f). %unroll (
    %fix(False). (P : ? -> Type) -> P f;
  )) -> Type) -> P f;

expected : %self(f). (P : (self%f : %unroll False f) -> Type) -> P f
received : %self(f). (P : (self%f : %unroll False f) -> Type) -> P f

fix%Unit (self%u : %unroll Unit u) =
  (P : (self%u : %unroll Unit u) -> Type) -> P unit -> P u;

fix%Unit (self%unit : %self(u). %unroll Unit unit u) (self%u : %unroll Unit unit u) =
  (P : (self%u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

fix%Unit (self%unit : %self(u). %unroll Unit unit (%unroll unit)) (self%u : %unroll Unit unit u) =
  (P : (self%u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

// needs to expand unit inside of unit
fix%unit : %self(u). %unroll Unit unit u =
  %fix(u). P => (x : P (%unroll u)) => x;


fix%unit (u : %self(u). %unroll Unit unit u)
  : %self(u). %unroll Unit unit u =
  P => (x : P (%unroll unit)) => (x : P u);

u = %fix(u) : %unroll Unit unit u. %unroll unit u;

expected : %unroll Unit unit u
received :

fix%unit : %self(u). %unroll Unit unit (%fix(u) : %unroll Unit unit u. P => (x : ) => ) =
  %fix(u). P => (x : P (%unroll unit)) => (x : P (%unroll unit));

%self(u). %unroll Unit (%fix(unit). u) u

expected : %unroll Unit unit u
received : (P : (self%u : %unroll Unit unit u) -> Type) -> P u -> P u

expected : %self(u). (P : (self%u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u
received : %self(u). (P : (self%u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P (%unroll unit)
fix%u : %unroll Unit unit u
  = (P : (self%u : %unroll Unit unit u) -> Type) =>
    (x : P (%unroll unit u)) => (x : P u)

Unit = %self(u). Unit unit u;
expected : P (%unroll unit u) -> P u
received : P u -> P u
fix%unit : %self(u). %unroll Unit unit u =
  %fix(u). P => (x : P (%unroll unit)) => (x : P u);

expected : (x : P (%fix(u). P => x => x)) -> P u
received : (x : P u) -> P u;
expected : %self(u). (P : (self%u : %unroll Unit unit u) -> Type) -> P unit -> P u
received : %self(u). (P : (self%u : %unroll Unit unit u) -> Type) -> P u -> P u

fix%unit (self%u : %unroll Unit (%unroll unit u) u) =
  (P : (self%u : %unroll Unit (%unroll unit u) u) -> Type) =>
  (x : P (%unroll unit u)) => (%unroll u P x : P u);

u : %self(u). %unroll Unit (%unroll unit u) u
  = %fix(u). %unroll unit u;


fix%unit : %self(u). %unroll Unit unit u =


expected : (P : (self%u : %unroll Unit u) -> Type) -> P unit -> P unit
received : (P : (self%u : %unroll Unit u) -> Type) -> P unit -> P unit


fix%Unit (self%unit : %self(u). %unroll Unit unit u) (self%u : %unroll Unit unit u) =
  (P : (self%u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

%self. (%self. %unroll \1 \0 \0) -> (%self. %unroll \2 \1 \0) -> Type;

%self(Unit). (a : %self(b). %unroll Unit b b) -> (c : %self(d). %unroll Unit a d) -> Type;


(%unroll Unit : (a : %self(b). %unroll Unit b b) -> (c : %self(d). %unroll Unit a d) -> Type) b b
(%unroll Unit b : (c : %self(d). %unroll Unit b d) -> Type) b



%fix. (%self. %unroll \1 \0 \0) => (%self. %unroll \2 \1 \0) =>
  ((%self. %unroll \3 \2 \0) -> Type) -> \0 \2

(\0 : (%self. %unroll \3 \2 \0) -> Type) (\2 : %self. %unroll \3 \0 \0)

expected : %unroll \3 \2 \2
received : %unroll \3 \2 \2
fix%Unit (self%unit : %unroll Unit unit unit) (self%u : %unroll Unit unit u) =
  (P : (self%u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

fix%Unit (a : %self(b). %unroll Unit b b) (c : %self(d). %unroll Unit a d) =
  (P : (e : %self(f). %unroll Unit a f) -> Type) -> P (%coerce a) -> P u;

Γ ⊢ m : %self(x). T
-------------------------
Γ ⊢ %unroll m : T[x := m]

Γ ⊢ m : %self(x). A   A[x := m] == B[x := m]
--------------------------------------------
Γ ⊢ %coerce m : %self(x). B

(%unroll (%coerce m : B))
fix%Unit (self%unit : %unroll Unit unit unit) (self%u : %unroll Unit unit u) =
  (P : (self%u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

unit : %self(u).
%coerce (x => %self(y). %unroll Unit x y)

unit : %self(b). %unroll Unit b b = %fix(b). P => x => x;

expected :
expected : %unroll Unit unit (%unroll u)
received :


self%Unit : (self%u : %unroll Unit u) -> Type;
self%unit : %unroll Unit unit;

Unit (self%u : %unroll Unit u) =
  (P : (self%u : %unroll Unit u) -> Type) -> P unit -> P u;
unit =

(f : rec F. F -> ()) => f(f);

rec F. F -> ()
(rec F. F -> ()) -> ()
((rec F. F -> ()) -> ()) -> ()

fix = f => f (fix f);

y = fix fix;

map map = f => l => (l | [] -> [] | el :: tl -> f el :: map map f l);


map = (f => f (fix f)) (fix (f => f (fix f)))  map;

map = (f => f (fix f)) map;


[]

y = f => (x => f(x(x)))(x => f(x(x)));
z = f => (x => f(v => x(x)(v)))(x => f(v => x(x)(v)));

z(self => 1);


double = x => x + x;

double((() => {
  console.log("hi");
  return 1;
})())

double(1)
1 + 1

map2 = map map;


(f => f f)(f => f f)
l = map map (+ 1) [1, 2, 3];
l = (+ 1) 1 :: (+ 1) 2 :: (+ 1) 3 :: [];

map = map map [1, 2, 3];

Unit = %self(u). Eq Unit unit u;


False = %fix(False). %self(f). (P : %unroll False -> Type) -> P f
False = %self(f). %unroll (
  %fix(False). (f : %self(f). %unroll False f) =>
    (P : (f : %self(f). %unroll False f) -> Type) -> P f;
) f;

fix%M : {
  Unit : %self(Unit). (u : %self(u). %unroll Unit u) -> Type;
  unit : %self(u). %unroll Unit u;
} = {
  Unit (u : %self(u). %unroll M.Unit u) =
    (P : (u : %self(u). %unroll M.Unit u) -> Type) -> P M.unit -> P u;
  unit = (P : (u : %self(u). %unroll M.Unit u) -> Type) => (x : P M.unit) => x;
};

ind_unit : (u : Unit) -> (P : Unit -> Type) -> P unit -> P u;
Unit = (P : Unit -> Type) -> P unit -> P u;

False = %fix(False). %self(f).
  (P : %unroll False -> Type) -> P f;

expected : %unroll False;
received : %self(f). (P : %unroll False -> Type) -> P f;


Unit = %ind(Unit, unit : Unit);

fix%False = %self(f). (P : %unroll False -> Type) -> P f;

fix%Unit =
  %self(u). (P : %unroll Unit -> Type) -> P (%fix(u). P => (x : P u) => x) -> P u;

expected : %self(u). (P : %unroll Unit -> Type) -> P (%fix(u). P => (x : P u) => x) -> P u
received : %self(u). (P : %unroll Unit -> Type) -> P (%fix(u). P => (x : P u) => x) -> P u;
fix%Unit (self%unit : %self(u). %unroll Unit unit u) (self%u : %unroll Unit unit u) =
  (P : (self%u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

fix%unit : %self(u). %unroll Unit unit u.
  %fix(u). P => (x : P (%unroll unit)) => (x : P u);


P (%unroll unit) == P u;

%unroll unit = u


%fix(unit). P => x => x;
%unroll unit ()
%self().

Unit =

x => y => x
lambda. lambda. \1

x => y => y
lambda. lambda. \0

Teika -> JS
Teika -> Smol

expected : %unroll False
received : %self(f). (P : %unroll False -> Type) -> P f

False = %fix(False). %self(f). (P : False -> Type) -> P f;
f : False;



(P : False -> Type) -> P f

expected : %self(f). (P : False -> Type) -> P f;
received : %self(f). (P : False -> Type) -> P f;

(n : Nat) =>
  n |
```

## Generics

```rust
incr = x => x + 1;

(x => x + 1)

1 + 1 -> generalizes to (x => x + 1) ->

(x => x + 1)(1)

(1 + 1)

(x => x + 1)(2)

(2 + 1)

x => x + 1

(x => M)(N) === M[x := N]
(<A>M)<N> === M[A := N]

(x => x + 1)(1) === (x + 1)[x := 1] => 1 + 1


incr = (x : Int) => x + 1;

id_int = (x : Int) => x;
id_string = (x : String) => x;

((x : Int) => x)


id = <A>(x : A) => x;
id_int = id<Int>;
id_int = (x : Int) => x;
id_string = (x : String) => x;
```

## Row Tuples

```rust
Array A = (n : Nat, ...(n _ (acc => A :: acc) []));
```

## Two Step Fixpoint

```rust

False = %fix(False). %self(f). (P : %unroll False -> Type) -> P f;

%fix(T,
  T_eq : %self(T_eq). Eq Type (%unroll T)
    (Eq _
      (%fix(False, eq : %unroll T). %self(f).
        (P : %unroll False -> Type) -> P (%unroll (T_eq (X => X) eq) (False => %unroll False) f)) False)
).
  Eq _ (%fix(False, eq : %unroll T). %self(f).
        (P : %unroll False -> Type) -> P (%unroll (T_eq (X => X) eq) (False => %unroll False) f)) False;

False_Eq False = %self(eq). %unroll (%fix(Eq_T, self%Eq_T_eq :
  Eq _ (%unroll T)
    (Eq _ (%fix(False, eq : %unroll T). %self(f). (P : %unroll False -> Type) ->
      P (%unroll (%unroll Eq_T_eq (X => X) eq) (False => %unroll False) f)
    ) False)
).
  Eq _ (%fix(False, eq : %unroll T). %self(f). (P : %unroll False -> Type) ->
    P (%unroll eq (False => %unroll False) f)
  ) False;
);

False = %fix(False,
  eq
    : %self(eq). Eq _ (%self(f). (P : %unroll False -> Type) -> P (%unroll eq (X => X) f)) (%unroll False)
    = %fix(eq). P => (x : P (%self(f). (P : %unroll False -> Type) -> P (%unroll eq (X => X) f))) => x
).
  %self(f). (P : %unroll False -> Type) -> P (%unroll eq (X => X) f)

Body_T =
  %self(Body). (X : %self(X). Type) -> (X_eq _ (%unroll Body X X_eq) (%unroll X)) -> Type;
Make_eq_unroll
  (First : )
  (Second : Body_T) =
  %fix(X, X_eq : %self(X_eq). Eq _ (%unroll Body X X_eq) (%unroll X)).
    %unroll Body X X_eq;

False = Make
  ()
  (False => False_eq =>
    %self(f). (P : False -> Type) -> P (%unroll False_eq (X => X) P))
x = Make_eq_unroll (%fix(Body, ()). False => False_eq =>
  %self(f). (P : False -> Type) -> )


Eq_T_eqT (self%Eq_T : (False : %self(False). Type) -> Type) =
  %self(Eq_T_eq). (False : %self(False). Type) ->
    Eq _ (%unroll Eq_T False) (
      Eq _ (%fix(False, False_eq : %unroll Eq_T False).
        %self(f). (P : %unroll False -> Type) ->
          P ((%unroll Eq_T_eq False (X => X) False_eq) (False => %unroll False) f)
      ) False
    );
False_EqT =
  %fix(
    Eq_T,
    Eq_T_eq
      : %self(Eq_T_eq). (False : %self(False). Type) ->
          Eq _ (%unroll Eq_T False) (
            Eq _ (%fix(False, False_eq : %unroll Eq_T False).
              %self(f). (P : %unroll False -> Type) ->
                P ((%unroll Eq_T_eq (X => X) False_eq) (False => %unroll False) f)
            ) False
          )
      = _
  ). False =>
  Eq _ (%fix(False, False_eq : %unroll Eq_T). %self(f). (P : %unroll False -> Type) ->
    P (%unroll (%unroll Eq_T_eq (X => X) False_eq) (False => %unroll False) f)
  ) False;


%fix(False, self%eq : %unroll (%fix(Eq_T, self%Eq_T_eq :
  Eq _ (%unroll T)
    (Eq _ (%fix(False, eq : %unroll T). %self(f). (P : %unroll False -> Type) ->
      P (%unroll (%unroll Eq_T_eq (X => X) eq) (False => %unroll False) f)
    ) False)
).
  Eq _ (%fix(False, eq : %unroll T). %self(f). (P : %unroll False -> Type) ->
    P (%unroll eq (False => %unroll False) f)
  ) False;
)).

False = %fix(False)(
  self%eq : Eq Type (%self(f). (P : %unroll False -> Type) -> P (%unroll eq P f))
    (%unroll False)
    = (P : Type -> Type) => (x : P (%self(f). (P : %unroll False -> Type) -> P (%unroll eq P f))) => x;
). %self(f). (P : %unroll False -> Type) -> P (%unroll eq P f);

Unit = %fix(Unit, eq).
  %self(u). (P : %unroll Unit -> Type) ->
    P (%fix(u))
    P (%unroll eq P u)

Unit = %fix(Unit, Unit_eq). %self(u).
  (P : %unroll Unit -> Type) ->
  P (%fix(unit, unit_eq) : %unroll Unit.
    (P : %unroll Unit -> Type) => (x : P unit) => x
    ) ->
  P (%unroll Unit_eq P u);
Unit = %fix(Unit, eq). (self%unit : %self(u). %unroll Unit unit u) => (self%u : %unroll Unit unit u) =>
  (P : (self%u : %unroll Unit unit u) -> Type) ->
    P (%unroll unit) -> P u;

unit = %fix(unit, unit_eq) : Unit unit.
  %fix(u, _). P => (x : P (%unroll unit)) =>
    (unit_eq P )

False = %fix(False,
  eq : Eq _ False (fix(False, ))
  ). %self(f).
fix%Unit (unit : %self(unit). %self(u). %unroll Unit unit u)
  (self%u : %unroll Unit unit u) =
  (P : (self%u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

unit = %fix(unit, ).
  %fix(u)(
    eq : Eq _ (%unroll unit) u = P => (x : P (%unroll unit)) =>
  ) : %unroll Unit unit u.
    P => (x : P (%unroll unit)) => eq P x;

(P : (self%u : Unit unit u) -> Type) => (x : P (%unroll unit)) => x
fix%T =
  %self(eq).
    Eq (%self(unit). %self(u). Unit unit u) unit
      (%fix(unit, eq : T). %fix(u, ()) : Unit unit u.
    P => x => (eq (unit => P (%unroll unit)) x : P u))

fix%Unit (self%u : %unroll Unit u) : Type =
  (P : (self%u : %unroll Unit u) -> Type) ->
  P (%fix(u) : %unroll Unit u. P => (x : P u) => x) -> P u

fix%Unit (self%u : %unroll Unit u) : Type =
  (P : (self%u : %unroll Unit u) -> Type) ->
  P (%fix(u) : %unroll Unit u. P => (x : P u) => x) -> P u

TUnit = %self(Unit). (self%u : %unroll Unit u) -> Type;
Tunit_eq (Unit : TUnit) (self%u : %unroll Unit u) =
  Eq Type ((P : (self%u : %unroll Unit u) -> Type) -> P u -> P u) (%unroll Unit u);

TUnit_eq (Unit : TUnit) = %self(Unit_eq). (self%u : %unroll Unit u) ->
  Eq Type (
    (P : (self%u : %unroll Unit u) -> Type) ->
    P (%fix(u, u_eq : Tunit_eq Unit u = Unit_eq u).
        u_eq (X => X) (P => x => x)) ->
    P u
  ) (%unroll Unit u);

Unit = %fix(Unit, Unit_eq : TUnit_eq Unit = %fix(Unit_eq). P => x => x).
  (self%u : %unroll Unit u) =>
  (P : (self%u : %unroll Unit u) -> Type) ->
  P (%fix(u, u_eq : Tunit_eq Unit u = Unit_eq u).
      u_eq (X => X) (P => x => x)) ->
  P u;

Unit = %self(u). %unroll Unit u;
unit : Unit = %fix(u, u_eq : Tunit_eq Unit u = Unit_eq u).
      u_eq (X => X) (P => x => x);


expected : (P : _) -> P u -> P u
received : %unroll Unit u



case : (b : Bool) -> (A : Type) -> A -> A -> A;
Bool = (A : Type) -> A -> A -> A;

ind_bool : (b : Bool) -> (P : Bool -> Type) -> P true -> P false -> P b;
Bool = %self(b). (P : Bool -> Type) -> P true -> P false -> P b;

T_False_eq False =
  %self(False_eq). Eq Type
    (%self(f). (P : %unroll False -> Type) -> P (%unroll False_eq (X => X) f))
    (%unroll False);

False_eq : %self(False_eq). Eq Type
  (%self(f). (P : %unroll False -> Type) -> P (%unroll False_eq (X => X) f))
  (%unroll False);


%fix(False,
  False_eq : %self(False_eq). Eq Type (%self(f). (P : %unroll False -> Type) -> P (%unroll False_eq (X => X) f)) (%unroll False)
    = %fix(False_eq). P => x => x).
  %self(f). (P : %unroll False -> Type) -> P (%unroll False_eq (X => X) f);


%self(False_eq). Eq Type
  (%self(f). (P : %unroll False -> Type) -> P (%unroll False_eq (X => X) f))
  (%self(f). (P : %unroll False -> Type) -> P (%unroll False_eq (X => X) f))


%fix(Fix). (x : %unroll Fix) -> Fix;

Unit : {
  Unit : Type;
  unit : Unit;
  ind_unit : (u : Unit) -> (P : Unit -> Type) -> P unit -> P u;
} = {};
(Unit, unit, ind_unit) = (
  fix%Unit = %self(u). (P : Unit -> Type) -> P (%fix(u). P => x => x) -> P u;
  unit = %fix(u). P => x => x;
  ind_unit = u => %unroll u;
  (Unit, unit, ind_unit);
);

((u : Unit) => ind_unit u (_ => Nat) 1) unit;


TUnit = %self(Unit). (self%u : %unroll Unit u) -> Type;
Tunit_eq (Unit : TUnit) (self%u : %unroll Unit u) =
  Eq Type ((P : (self%u : %unroll Unit u) -> Type) -> P u -> P u)
    (%unroll Unit u);

TUnit_eq (Unit : TUnit) = %self(Unit_eq). (self%u : %unroll Unit u) ->
  Eq Type (
    (P : (self%u : %unroll Unit u) -> Type) ->
    P (%fix(u, u_eq : Tunit_eq Unit u = %unroll Unit_eq u).
        u_eq (X => X) (P => x => x)) ->
    P u
  ) (%unroll Unit u);

Tunit_eq (Unit : TUnit) (self%u : %unroll Unit u) =
  Eq Type (P u -> P u)
    (%unroll Unit u);


%fix(u, u_eq : Tunit_eq Unit u = ).
  P => u_eq (u => P u -> P u) (x => x);

Unit = %fix(Unit, Unit_eq : TUnit_eq Unit = %fix(Unit_eq). P => x => x).
  (self%u : %unroll Unit u) =>
  (P : (self%u : %unroll Unit u) -> Type) ->
  P (%fix(u, u_eq : Tunit_eq Unit u = %unroll Unit_eq u).
      u_eq (X => X) (P => x => x)) ->
  P u;



Unit = %fix(Unit, Unit_eq : TUnit_eq Unit = %fix(Unit_eq). P => x => x).
  (self%u : %unroll Unit u) =>
  (P : (self%u : %unroll Unit u) -> Type) ->
  P (%fix(u, u_eq : Tunit_eq Unit u = %unroll Unit_eq u).
      u_eq (X => X) (P => x => x)) ->
  P u;

Unit = %self(u). %unroll Unit u;
unit : Unit = %fix(u, u_eq : Tunit_eq Unit u = Unit_eq u).
      u_eq (X => X) (P => x => x);

fix%Unit (self%unit : %self(u). %unroll Unit unit u) (self%u : %unroll Unit unit u) =
  (P : (self%u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;
unit : %self(unit). %self(u). %unroll Unit unit u =
  %fix(unit,
    self%eq : Eq _ (%unroll unit) (%fix(u, ()). P => x => eq P x)
      = P => x => x
  ). %fix(u, ()). P => x => eq P x;

Unit = %self(u). %unroll Unit unit u;
unit : Unit = %unroll unit;

fix%Bool
  (self%true : (self%false : %self(b). %unroll Bool true false b) ->
    %self(b). %unroll Bool true false b)
  (self%false : %self(b). %unroll Bool true false b)
  (self%b : %unroll Bool true false b) =
  (P : (self%b : %unroll Bool true false b) -> Type) ->
    P (%unroll true false) -> P (%unroll false) -> P b;

true : (self%false : %self(b). %unroll Bool true false b) -> %self(b). %unroll Bool true false b =
  %fix(true,
    self%eq : Eq _ (%unroll true false) (%fix(b, ()). P => x => y => eq P x)
      = P => x => x;
  ). false => %fix(b, ()). P => x => y => eq P x;
false : %self(false). %self(b). %unroll Bool true false b =
  %fix(false,
    self%eq : Eq _ (%unroll false) (%fix(b, ()). P => x => y => eq P y)
      = P => x => x;
  ). %fix(b, ()). P => x => y => eq P y;

Bool = %self(b). %unroll Bool true false b;

fix%Unit (self%unit : %self(u). %unroll Unit unit u) (self%u : %unroll Unit unit u) =
  (P : (self%u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;
unit : %self(unit). %self(u). %unroll Unit unit u =
  %fix(unit,
    self%eq : Eq _ (%unroll unit) (%fix(u, ()). P => x => %unroll eq P x)
      = %fix(eq). P => x => x
  ). %fix(u, ()). P => x => %unroll eq P x;

Unit = %self(u). %unroll Unit unit u;
unit : Unit = %unroll unit;

fix%Unit (self%unit : %self(u). %unroll Unit unit u) (self%u : %unroll Unit unit u) =
  (P : (self%u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;
unit : %self(unit). %self(u). %unroll Unit unit u =
  %fix(unit, eq). %fix(u, ()). P => x => %unroll eq P x;

Unit = %self(u). %unroll Unit unit u;
unit : Unit = %unroll unit;

T_unit =
  %self(unit).
    (self%unit_eq :
      Eq _ (%unroll unit) (%fix(u). P => x => %unroll unit_eq P x)) ->
    %self(u). %unroll Unit (%fix(unit) : %self(u). %unroll Unit unit u) u;

unit : T_unit.
  = %fix(unit). unit_eq => %fix(u). P => (x : P unit) => %unroll unit_eq P x;



expected : (P : (self%u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u
received : (P : (self%u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P (%fix(self%u : %unroll Unit unit u, ()). P => x => unit_eq P x)

Eq _ ((P : (self%u : %unroll Unit unit u) -> Type) -> P u -> P u)
((P : (self%u : %unroll Unit unit u) -> Type) -> P u -> P u))



Γ, x : %self(x). T |- T : Type
------------------------------
%self(x). T : Type

Γ, x : %self(x). T,
  eq : %self(eq). (P : T -> Type) -> P (%unroll x) -> P M |- M : T[x := M]
--------------------------------------------------------------------------
%fix(self%x : T, eq). M : %self(x). T

Γ |- M : %self(x). T
---------------------
%unroll M : T[x := M]

---------------------------------------------------
%unroll (%fix(self%x : T, eq). M) ===
M[x := %fix(self%x : T, eq). M]
 [eq := %fix(eq, _). (P : T -> Type) => (x : P M) => x]


fix%Unit (self%unit : %self(u). %unroll Unit unit u) (self%u : %unroll Unit unit u) =
  (P : (self%u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;
unit : %self(unit). %self(u). %unroll Unit unit u =
  %fix(unit, eq). %fix(u, _). P => x => %unroll eq P x;

Unit = %self(u). %unroll Unit unit u;
unit : Unit = %unroll unit;

Γ, x : %self(x). T |- T : Type
------------------------------
%self(x). T : Type

Γ, x : %self(x). T, y : A |- M : T[x := M]
Γ, x : %self(x). T |- N : A[x := %fix(self%x : T, y : A = N). M]
----------------------------------------------------------------
%fix(self%x : T, y : A = N). M : %self(x). T

Γ |- M : %self(x). T
---------------------
%unroll M : T[x := M]


fix%Bool
  (self%true : (self%false : %self(b). %unroll Bool true false b) ->
    %self(b). %unroll Bool true false b)
  (self%false : %self(b). %unroll Bool true false b)
  (self%u : %unroll Bool true false b) =
  (P : (self%u : %unroll Bool true false b) -> Type) ->
    P (%unroll true false) -> P (%unroll false) -> P b;

unit : %self(true). (self%false : %self(b). %unroll Bool true false b) -> %self(b). %unroll Bool true false b =
  %fix(unit, eq). %fix(u, _). P => x => %unroll eq P x;

fix%Nat
  (self%zero : (self%succ : %self(n). %unroll Nat zero succ n) ->
    %self(n). %unroll Nat zero succ n)
  (self%succ : (pred : %self(n). %unroll Nat zero succ n) ->
    %self(n). %unroll Nat zero succ n) ->
  (self%n : %unroll Nat zero succ n) =
    (P : (self%n : %unroll Nat zero succ n) -> Type) ->
    P (%unroll zero succ) ->
    ((pred : %self(n). %unroll Nat zero succ n) -> P pred -> P (%unroll succ pred)) ->
    P n;

zero
  : %self(zero). (self%succ : %self(n). %unroll Nat zero succ n) -> %self(n). %unroll Nat zero succ n
  = %fix(zero, eq). succ => %fix(b, _). P => z => s => %unroll eq (b => P (b succ)) z;

unit : %self(unit). %self(u). %unroll Unit unit u =
  %fix(unit, eq). %fix(u, _). P => x => %unroll eq P x;

T_F_eq = Eq
  _
  (%fix(False, F_eq : ?))
%fix(False, F_eq).
  %self(f). (P : %unroll False -> Type) -> P (F_eq (False => %unroll False) f);
Unit = %fix(Unit, U_eq).
  %self(u).
  (P : %unroll Unit -> Type) ->
  P (
    %fix(unit, u_eq).
      P => eq_sym u_eq (u => P u -> P unit) (x => x)
  ) ->
  P (U_eq (Unit => %unroll Unit) u);

%fix((true, false), (T_eq, F_eq))
Bool = %fix(Bool, B_eq).
  %self(b).
    (P : %unroll Bool -> Type) ->
    P (
      %fix(true, b_eq).
        P => eq_sym b_eq (b => P b -> P false -> P true) (x => y => x)
    ) ->
    P (
      %fix(true, b_eq).
        P => eq_sym b_eq (b => P b -> P false -> P true) (x => y => x)
    ) ->
    P (U_eq (Unit => %unroll Unit) u)

fix%Unit (self%unit : %self(u). %unroll Unit unit u) (self%u : %unroll Unit unit u) =
  (P : (self%u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;
unit : %self(unit). %self(u). %unroll Unit unit u =
  %fix(unit). %fix(u). P => x => eq P x;


%unroll (%fix(T). (self%x : %unroll T x) => Type);


u @-> (P : (a : u @-> @((Unit : Unit @-> (unit : unit @-> u @-> @Unit unit u) -> (u : u @-> @Unit unit u) -> Type, _) @=> (unit : unit @-> u @-> @Unit unit u) => (u : u @-> @Unit unit u) => (P : (a : u @-> @Unit unit u) -> Type) -> (b : P (@unit)) -> P u) unit u) -> Type) -> (b : P (@unit)) -> P u :
u @-> @((Unit : Unit @-> (unit : unit @-> u @-> @Unit unit u) -> (u : u @-> @Unit unit u) -> Type, _) @=> (unit : unit @-> u @-> @Unit unit u) => (u : u @-> @Unit unit u) => (P : (a : u @-> @Unit unit u) -> Type) -> (b : P (@unit)) -> P u) unit u


(FalseT : Type) = %self(False). (f : %self(f). %unroll False f) -> Type;
False0 = %fix(False : FalseT).
  f => (P : (f : %self(f). %unroll False0 f) -> Type) -> P f;
False1 = %self(f). %unroll False0 f;

eq : (F : _) ->
    Eq _ (%unroll False0)
      (f => (P : (f : %self(f). %unroll False0 f) -> Type) -> P f)
  = _;

f (f : False1) : %self(f). (P : (f : %self(f). %unroll False0 f) -> Type) -> P f
  = eq (T => %self(f). (P : (f : %self(f). T f) -> Type) -> P f) f;

expected : %self(f). %unroll False0 f
received : %self(f). (P : (f : %self(f). %unroll False0 f) -> Type) -> P f;

expected : %self(f). %unroll False0 f
received : %self(f). (P : (f : %self(f). %unroll False0 f) -> Type) -> P f;

eq : (F : _) ->
    Eq _ (%unroll (%fix(x, eq). F x eq))
      (F (%fix(x, eq). F x eq) refl)
  = P => (x : P (%unroll (%fix(x, eq). F x eq))) =>
    ;



W : %self(W). (x : %self(x). %unroll W x) -> Type;
F :
  %self(F).
    (x : %self(x). %unroll W x) ->
    (eq :
      %self(eq). (P : %unroll W x -> Type) ->
        P (%unroll x) -> P (%unroll F x eq)) ->
    %unroll W x;
%fix_red :
  (W : %self(W). (x : %self(x). %unroll W x) -> Type) ->
  (F : %self(F).
    (x : %self(x). %unroll W x) ->
    (eq :
      %self(eq). (P : %unroll W x -> Type) ->
        P (%unroll x) -> P (%unroll F x eq)) ->
    %unroll W x) ->
  (P : (x : W (%fix(self%x : %unroll W x, eq). %unroll F x eq)) -> Type) ->
  P (%unroll (%fix(self%x : %unroll W x, eq). %unroll F x eq))
  P (%unroll F (%fix(self%x : %unroll W x, eq). F x eq) refl);

%fix_red(%fix(self%x : T, eq). M) : %self(x). T

%fix_red(
  x = %fix(self%x : T, eq). M,
  _ : (P : (a : %self(x). T) -> Type) ->
      P (%unroll (%fix(self%x : T, eq). M)) ->
      P x
);

%fix_red(
  x = %fix(self%x : T, eq). M,
  _ : (P : (a : %self(x). T) -> Type) ->
      P (%unroll (%fix(self%x : T, eq). M)) ->
      P x
) ===
  (P : (a : %self(x). T) -> Type) =>
    (b : P (%unroll (%fix(self%x : T, eq). M))) =>
    (b : P (M[x := %fix(self%x : T, eq). M][y := refl]));

Γ |- M : %self(x). T
Γ, x : %self(x). T |- A : Type Γ |- N : A[x := M]

A[a := %unroll M]
A[a := M[]]
---------------------------------------
%fix_red(x = M, eq : A = N) :
  A[x := ]

%self(%fix(x, eq). %unroll F x eq)
(%unroll (%fix(x, eq). F x eq))
F (%fix(x, eq). F x eq) refl
F => Eq _
  (%unroll (%fix(x, eq). F x eq))
  (F (%fix(x, eq). F x eq) refl)

%fix_red (

)

id = (x : Int) => x;

f = P => (x : P (id 1)) => (x : P 1);
g = P => (x : P (id 1)) => (%beta x : P 1);

fix%Unit (self%unit : %self(u). %unroll Unit unit u) (self%u : %unroll Unit unit u) =
  (P : (self%u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

unit_expand (self%unit : %self(u). %unroll Unit unit u) (self%u : %unroll Unit unit u) =
  (P : Type -> Type) => (x : P (%unroll Unit unit u)) => %expand x;

unit : %self(unit). (eq : _) -> %self(u). %unroll Unit unit u
  %fix(unit). (eq : _) => %fix(u). P => x => eq P x;

unit : %self(unit). %self(u). %unroll Unit unit u =
  %fix(unit, eq). %fix(u, _).
    eq_sym (unit_expand unit u) (X => X) (P => x => eq P x);

unit : %self(unit). %self(u). %unroll Unit unit u =
  %fix(unit, eq). %fix(u, _). P => x => x;

id = (x : Int) => %expand x;

x = (x : P (id 1)) => %beta x ==;

(%unroll (%fix(x, eq). M)) === M[x := %fix(x, eq). M][eq := refl]

%expand x === x

Id = X => X;
fix%False (self%I : (self%f : (P : %unroll False I -> Type) -> P (I f)) -> %unroll False I) =
  %self(f). (P : %unroll False I -> Type) -> P (I f);
False = %unroll False (%fix(I). P => x => x);

fix%Unit (self%unit : %self(u). %unroll Unit unit u) (self%u : %unroll Unit unit u) =
  (P : (self%u : %unroll Unit unit u) -> Type) -> P (%unroll unit) -> P u;

fix%Unit (I1 : Eq _ (%unroll Unit) _) (self%unit : %unroll Unit I1 unit) = (
  W (P : %unroll Unit I1 -> Type) = I1 (U => U I1 -> Type) P;
  %self(u). (P : %unroll Unit I1 unit -> Type) -> W P (%unroll unit) -> W P u
);


fix%Unit (I1 : Eq _ (%unroll Unit I1) _) = (
  W (P : %unroll Unit I1 -> Type) = I1 (U => U -> Type) P;
  fix%unit (I2 : _) = %fix(u). I2 (P => (x : W P (%unroll unit I2)) => x);
  unit = %unroll unit (%fix(I2).
    (x : ((P : )))
  )
  %self(u). (P : %unroll Unit I1 -> Type) ->
    W P (%unroll unit) -> W P (u)
);

!Unit ===
  %self(u). (P : %unroll Unit I_Unit unit I_unit -> Type) ->
    %unroll I_Unit unit I_unit P (%unroll unit I_unit) ->
    %unroll I_Unit unit I_unit P u;
!T_I_Unit ===
  %self(I_Unit). (unit : !T_unit) -> (I_unit : !T_I_unit) ->
    (P : %unroll Unit I_Unit unit I_unit -> Type) -> !Unit -> Type;
!T_unit ===
  %self(unit). (I_unit : !T_I_unit) -> !Unit;
!T_I_unit ===
  %self(I_unit). (P : %unroll Unit I_Unit unit I_unit -> Type) ->
    P (%unroll unit I_unit) -> P !unit;
!unit ===
  %fix(u). (P : %unroll Unit I_Unit unit I_unit -> Type) =>
    (x : P (%unroll unit I_unit)) => %unroll I_unit P x;

fix%Unit (I_Unit : !T_I_Unit) (unit : !T_unit) (I_unit : !T_I_unit) : Type = !Unit;
I_Unit : !T_I_Unit = %fix(I_Unit). unit => I_unit => P => %expand P;

fix%unit (I_unit : !T_I_unit) : !Unit = !unit;
I_unit : !T_I_unit = %fix(I_unit). P => %expand P;

Unit = %unroll Unit I_Unit unit I_unit;

!Unit ===
  %self(u). (P : %unroll Unit I_Unit unit I_unit -> Type) ->
    P (%unroll unit I_unit) -> %unroll I_Unit unit I_unit P u;
!T_I_Unit ===
  %self(I_Unit). (unit : !T_unit) -> (I_unit : !T_I_unit) ->
    (P : %unroll Unit I_Unit unit I_unit -> Type) -> !Unit -> Type;
!T_unit ===
  %self(unit). (I_unit : !T_I_unit) -> %unroll Unit I_Unit unit I_unit;
!T_I_unit ===
  %self(I_unit). (P : %unroll Unit I_Unit unit I_unit -> Type) ->
    %unroll I_Unit unit I_unit P (%unroll unit I_unit) ->
    %unroll I_Unit unit I_unit P !unit;
!unit ===
  %fix(u). (P : %unroll Unit I_Unit unit I_unit -> Type) =>
    (x : P (%unroll unit I_unit)) => %unroll I_unit P x;

fix%Unit (I_Unit : !T_I_Unit) (unit : !T_unit) (I_unit : !T_I_unit) : Type = !Unit;
I_Unit : !T_I_Unit = %fix(I_Unit). unit => I_unit => P => %expand P;

fix%unit (I_unit : !T_I_unit) : !Unit = !unit;
I_unit : !T_I_unit = %fix(I_unit). P => %expand P;

Unit = %unroll Unit I_Unit unit I_unit;



FalseT = %self(False).
  (I_False : %self(I_False).
    (P : %unroll False I_False -> Type) ->
    (%self(f). (P : %unroll False I_False -> Type) -> I_False P f) -> Type)
   -> Type;
fix%False (I_False : ) =
  %self(f). (P : %unroll False I_False -> Type) -> I_False P f;
%self(u). (P : %unroll Unit I_Unit unit I_unit -> Type) -> P (%unroll unit I_unit) -> P u


fix%Unit (unit : T_unit) (I_unit : T_I_unit) =
  %self(u). (P : T unit I_unit -> Type) ->
    P (%unroll unit I_unit) -> P u;

fix%unit I1 : %self(u). (P : T unit I1 -> Type) -> P (%unroll unit I1) -> P u =
    %fix(u). (P : T unit I1 -> Type) => (x : P (%unroll unit I1)) => I1 P x;

expected : (P : _ -> Type) -> P (%unroll unit I1) -> P u
received : (P : _ -> Type) -> P (%unroll unit I1) -> P (%fix(u). (P : _ -> Type) => (x : P (%unroll unit)) => I1 P x)
unit =
(self%I : (f : %self(f). (P : %unroll False I -> Type) -> P (I f)) -> %unroll False I) =>
  %self(f). (P : %unroll False I -> Type) -> P (I f)

Γ, x : %self(x). T |- T : Type
------------------------------
%self(x). T : Type

Γ, x : %self(x). T |- M : T[x := %fix(x) : T. M]
------------------------------------------------
%fix(x) : T. M : %self(x). T

Γ |- M : %self(x). T
---------------------
%unroll M : T[x := M]

Γ |- N : P (%unroll (%fix(x) : T. M))
---------------------------------------
%expand(N) : P (M[x := %fix(x) : T. M])

----------------
%expand(N) === N


!Unit ===
  %self(u). (P : %unroll Unit I_Unit unit I_unit -> Type) ->
    %unroll I_Unit unit I_unit P (%unroll unit I_unit) ->
    %unroll I_Unit unit I_unit P u;
!T_I_Unit ===
  %self(I_Unit). (unit : !T_unit) -> (I_unit : !T_I_unit) ->
    (P : %unroll Unit I_Unit unit I_unit -> Type) -> !Unit -> Type;
!T_unit ===
  %self(unit). (I_unit : !T_I_unit) -> !Unit;
!T_I_unit ===
  %self(I_unit). (P : %unroll Unit I_Unit unit I_unit -> Type) ->
    %unroll I_Unit unit I_unit P (%unroll unit I_unit) ->
    %unroll I_Unit unit I_unit P !unit;
!unit ===
  %fix(u). (P : %unroll Unit I_Unit unit I_unit -> Type) =>
    (x : P (%unroll unit I_unit)) => %unroll I_unit P x;

fix%Unit (I_Unit : !T_I_Unit) (unit : !T_unit) (I_unit : !T_I_unit) : Type = !Unit;
I_Unit : !T_I_Unit = %fix(I_Unit). unit => I_unit => P => %expand P;

fix%unit (I_unit : !T_I_unit) : !Unit = !unit;
I_unit : !T_I_unit = %fix(I_unit). P => %expand P;



!Unit =
  %self(u). (P : %unroll Unit I_Unit I_unit -> Type) ->
    %unroll I_Unit I_unit P !unit ->
    %unroll I_Unit I_unit P u;
!unit =
  %fix(unit). (P : %unroll Unit I_Unit I_unit -> Type) =>
    (x : %unroll I_Unit I_unit P unit) => x;
!T_I_unit ===
  %self(I_unit). (P : %unroll Unit I_Unit I_unit -> Type) ->
    %unroll I_Unit I_unit P (%unroll unit I_unit) ->
    %unroll I_Unit I_unit P !unit;
!T_I_Unit =
  %self(I_Unit). (I_unit : !T_I_unit) ->
    (P : %unroll Unit I_Unit I_unit -> Type) -> !Unit -> Type;

fix%Unit I_Unit I_unit =
  %self(u). (P : %unroll Unit I_Unit I_unit -> Type) ->
    %unroll I_Unit I_unit P !unit ->
    %unroll I_Unit I_unit P u;;


!Unit =
  %self(u). (P : %unroll Unit I_Unit unit -> Type) ->
    P (%unroll unit) -> %unroll I_Unit unit P u
!T_unit = %self(unit). %unroll Unit I_Unit unit;
!T_I_Unit =
  %self(I_Unit). (unit : !T_unit) ->
    (P : %unroll Unit I_Unit unit -> Type) -> !Unit -> Type;


fix%Unit I_Unit unit = !Unit;
fix%I_Unit (unit : !T_unit) = P => P;

fix%unit : !T_unit =
  %fix()

fix%I_Unit unit
!Unit =
  %self(u). (P : %unroll Unit I_Unit -> Type) ->
    %unroll I_Unit P !unit -> %unroll I_Unit P u;
!unit =
  %fix(unit). (P : %unroll Unit I_Unit -> Type) =>
    (x : %unroll I_Unit P unit) => x;
!T_I_Unit =
  %self(I_Unit). (P : %unroll Unit I_Unit -> Type) -> !Unit -> Type
!T_Unit =
  %self(Unit). (I_Unit : !T_I_Unit) -> Type;

Unit = %unroll Unit I_Unit unit I_unit;


!Unit =
  %self(u). (P : %unroll Unit I_Unit -> Type) ->
    %unroll I_Unit P !unit -> %unroll I_Unit P u;
!unit =
  %fix(unit). (P : %unroll Unit I_Unit -> Type) =>
    (x : %unroll I_Unit P unit) => x;
!T_I_Unit =
  %self(I_Unit). (P : %unroll Unit I_Unit -> Type) -> !Unit -> Type
!T_Unit =
  %self(Unit). (I_Unit : !T_I_Unit) -> Type;

Unit I_Unit u = (
  fix%unit I_unit = %fix(unit). I_unit =>
    %fix(u). (P : %unroll Unit I_Unit -> Type) ->
      (x : %unroll I_Unit P (%unroll unit)) => %unroll I_unit P x;
  %self(u). (P : %unroll Unit I_Unit -> Type) ->
    %unroll I_Unit P unit -> %unroll I_Unit P u
);

ind : (b : Bool) -> (P : Bool -> Type) -> P true -> P false -> P b;
fix%Bool = %self(b). (P : Bool -> Type) -> P true -> P false -> P b;
ind (b : Bool) = %unroll b;

fold : (b : Bool) -> (A : Type) -> A -> A -> A;
Bool = (A : Type) -> A -> A -> A;
fold (b : Bool) = b;
true : Bool = A => x => y => x;
false : Bool = A => x => y => y;

fold : (n : Nat) -> (A : Type) -> A -> (A -> A) -> A;
Nat = (A : Type) -> A -> (A -> A) -> A;
zero : Nat = A => z => s => z;
succ (n : Nat) : Nat = A => z => s => s (n A z s);




received: (I_Unit : (I_UnitT : (Unit : UnitT : Type) -> Type) (Unit : UnitT : Type)) -> Type
expected: (I_Unit : (I_UnitT : Type) (Unit : UnitT : Type)) -> Type

fix%Eq A x y = (
  refl A x : Eq A x x = %fix(eq). P => x => x;
  // use equality to ensure that y is equal to x inside of P
  %self(eq). (P : (y : A) -> Eq A x y -> Type) -> P x (refl A x) -> P y eq
);
refl A x : Eq A x x = %fix(eq). P => x => x;

uip_refl_on A x (eq : Eq A x x) =
  (P : Eq A x x -> Type) => (x : P eq) =>
    %unroll eq (y => eq => P (eq (y => Eq A y x) (refl A y)));

Eq _ eq (eq (y => Eq A x y) (refl A x))

Eq _ (refl A x) ((refl A x) (y => Eq A x y) (refl A x))
Eq _ (refl A x) (refl A x)

a A a === b A b
%self(refl). (A : Type) -> (x : A) ->
  %self(eq). (P : Eq A x x -> A -> Type) -> P (refl A x) x -> P eq x
fix%refl A x
  : %self(eq). (P : Eq A x x -> Type) -> P (refl A x) -> P eq =
  %fix(eq). (P : Eq A x x -> Type) => P () -> P eq;


fix%False (I_False) =
  %self(f). (P : %unroll False I_False) -> P (%unroll I_False Id f);


%self(I). Eq _ (%unroll T I) (%unroll B refl T I);


!Unit =
  %self(u). %unroll Unit0 unit0 I_unit u;
!Unit0 =
  (P : (u : !Unit) -> Type) -> P (%unroll unit0 I_unit) -> P u;
!unit0 =
  %fix(u : !Unit0). (P : (u : !Unit0) -> Type) =>
    (x : P (%unroll unit0 I_unit)) => %unroll I_unit P x;

!T_I_unit =
  %self(I_unit). (P : (u : !Unit0) -> Type) ->
    P (%unroll unit0 I_unit) -> P !unit0
!T_unit0 =
  %self(unit0). (I_unit : !T_I_unit) -> !Unit0;
!T_unit0 =
  %self(unit0). (I_unit : !T_I_unit) -> !Unit0;
!T_Unit0 =
  %self(Unit0). (unit0 : !T_unit0) -> (I_unit : !T_I_unit) ->
    (u : !Unit) -> Type;

fix%Unit0 unit0 I_unit (u : !Unit) = !Unit;
fix%unit0 I_unit = !unit0;
fix%I_unit = P => P;

Unit = %self(u). %unroll Unit0 unit0 I_unit u;
unit : Unit = %unroll unit0;


Unit : Type;
unit : Unit;

Unit = %self(u). (P : Unit -> Type) -> P unit -> P u;
unit = (P : Unit -> Type) => (x : P unit) => x;



!Unit0 =
  u @-> (P : Unit -> Type) -> (x : P (@IT unit)) -> P (@IT u);
!T_IT =
  IT @-> !Unit0 -> Unit;

!unit0 = (u : !Unit0) @=> (P : Unit -> Type) => (x : P unit) => @I1 P x;
!T_I1 = I1 @-> (P : Unit -> Type) -> (x : P unit) -> P (!unit0);

Unit0 (Unit : Type) (IT : !T_IT) (unit : Unit) = !Unit0;
unit0 (Unit : Type) (unit : Unit) (I1 : !T_I1) = !unit0;


!Unit =
  u @-> @Unit0 IT unit I1;
!Unit0 =
  u @-> (P : !Unit -> Type) ->
    (x : @IT unit I1 P (@unit I1)) -> @IT unit I1 P u;
!IT
!unit0 =
  (u : !Unit0) @=> (P : !Unit -> Type) =>
    (x : @IT unit I1 P (@unit I1)) => @I1 P x;
!I1

!T_I1 =
  I1 @-> (P : !Unit -> Type) -> (x : P (@IT unit)) -> P (!unit0)
!T_unit =
  unit @-> (I1 : !T_I1) -> !Unit0;
!T_IT =
  IT @-> (unit : !T_unit) -> (I1 : !T_I1) -> (u : !Unit0) -> !Unit;
!T_Unit0 =
  Unit0 @-> ()
!T_Unit = Type;

Unit =
  Unit @=> (IT : !T_IT)
    (unit : unit @-> (I1 : !T_I1) -> Unit IT unit I1) (I1 : !T_I1)
  = Unit0 (Unit IT unit I1) IT (IT (@unit I1));

IT = _;
fix%unit (I1 : !T_I1) = unit0 (Unit IT unit I1) unit I1;


%self(I1). (P : Unit -> Type) -> P unit ->
  P (IT )
%fix(u). (P : Unit -> Type) => (x : P unit) =>

fix%False IT =
  %self(f). (P : %unroll False IT -> Type) -> %unroll IT P f;

%fix. _A =>
  %self. (%unroll \2 \1 -> Type) -> %unroll \2 \0 \1;

_A := %self. _B
%unroll \2 : _B[\0 := \2]

expected : (%unroll \1 \0 -> Type) -> _C
received : _B[\0 := \2]
%unroll \2 \0 :

IT : %self. (%unroll \1 \0 -> Type) ->
IT : _A

_A := %self(x). _B


(A : Type\0) => (x : A\0) => (x\0 : A\1)

%unroll IT :_B[x := IT]


(P : %unroll False IT -> Type) -> _C `unify` _B[x := IT]

_B := (P : %unroll False x -> Type) -> _C;
_C :=
  (%self(f). (P : %unroll False x -> Type) -> %unroll x P f) -> Type
((P : %unroll False IT -> Type) -> _C)[x := IT]

M : _A
%unroll M : _A[]

fix%False IT =
  %self(f). (P : %unroll False IT -> Type) -> %unroll IT P f;

%fix(False). (IT : _A) =>
  %self(f). (P : %unroll False IT -> Type) -> %unroll IT P f


(%unroll IT) (P : %unroll False IT -> Type) f

fix%False (IT : _A) =
  %self(f). (P : %unroll False IT -> Type) -> %unroll IT P f;

IT : _A
%unroll IT : _B[x := IT]
_A := %self(x). _B
%unroll IT P : _C[x := IT]
_B := (P : %unroll False x -> Type) -> _C
%unroll IT P f : Type
_C := (%self(f). (P : %unroll False x -> Type) -> %unroll x P f) -> Type;


fix%False (IT : _A)=
  %self(f). (P : %unroll False\2 IT\1 -> Type) -> %unroll IT\2 P\0 f\1;


IT\2 : _A[+2]

%unroll IT\2 : _B[IT\2][-2]
_A : %self(x). _B[+1]
%unroll IT\2 P\0 : _C[IT\2][-2]
_B := ((P : %unroll False\2 IT\1 -> Type) -> _C)[IT\2][-2]
%unroll IT\2 P\0 f\1 : Type
_C := ((%self(f). (P : %unroll False\2 IT\1 -> Type) -> %unroll IT\2 P\0 f\1) -> Type)

fix%Unit IT unit I1 =
  %self(u). (P : %unroll Unit IT unit I1 -> Type) ->
    %unroll IT unit I1 (%unroll unit I1) -> %unroll IT unit I1 u;

Unit = %unroll Unit (%fix(IT). P => x => %expand x);
fix%unit I1 : %unroll Unit unit I1 = P => x => %unroll I1 P x;

Unit = Unit unit (%fix(I1). P => x => %expand x);
unit : Unit = unit _;

fix%Bool IT true I1 false I2 = (
  IT = %unroll IT true I1 false I2;
  Bool = %unroll Bool IT true I1 false I2;
  true = %unroll true I1 false I2;
  false = %unroll false I2;

  %self(b). (P : Bool -> Type) -> IT P true -> IT P false -> IT P b
);

fix%Unit IT unit I1 =
  %self(u). (P : %unroll Unit IT unit I1 -> Type) ->
    P (%unroll IT unit I1 (%unroll unit I1)) ->
    P (%unroll IT unit I1 u);

f = (x : _A) => (P : x) -> %unroll x\1 x\1

_A := %self(y). _B
%unroll x\1 : _B[-1][x\1]

fix%False (IT : _A)=
  %self(f). (P : %unroll False\2 IT\1 -> Type) -> %unroll IT\2 P\0 f\1;


%unroll (IT\2 : _A[2]) : _B[-2][IT\2]
_A[2] `unify` %self(x). _B
_A := %self(x). _B[-2]



fix%False (IT : _A)=
  %self(f). (P : %unroll False\1 IT\2 -> Type) -> %unroll IT\2 P\4 f\3;

%unroll IT\2 : _B
_A := %self(x). _B

%unroll IT\2 P : _C
_B := (P : %unroll False\1 IT\2 -> Type) -> _C

%unroll IT\2 P\4 f\3 : Type
_C := (%self(f). (P : %unroll False\1 IT\2 -> Type) -> %unroll IT\2 P\4 f\3) -> Type;

%self(x). (P : %unroll False\1 IT\2 -> Type) -> (%self(f). (P : %unroll False\1 IT\2 -> Type) -> %unroll IT\2 P\4 f\3) -> Type

(x : _A) => (f : _B) => f\0 x\1

f\0 : _C -> _D
_B := (_C -> _D)[0]
x\1 : _C
_C := _A[1]
f\0 : _A[1] -> _D

!Unit =
  %self
fix%Unit IT

!False = %self(f). (P : %unroll False IT -> Type) -> %unroll IT P f;
!IT_T = %self(IT). (P : %unroll False IT -> Type) -> !False -> Type;
fix%False (IT : !IT_T) = !False;


!Unit =
  %self(u). (P : %unroll Unit IT unit I1 -> Type) ->
    %unroll IT unit I1 P (%unroll unit I1) -> %unroll IT unit I1 P u;
!unit =
  %fix(u). (P : %unroll Unit IT unit I1 -> Type) =>
    %unroll I1 P ((x : %unroll IT unit I1 (%unroll unit I1)) => x);
!T_IT =
  %self(IT). (P : %unroll Unit IT unit I1 -> Type) -> !Unit -> Type;
!T_I1 =
  %self(I1). Eq _ (%unroll unit I1) !unit;
fix%Unit (IT : !T_IT) (unit : !T_unit) (I1 : !T_I1) = !Unit;


fix%Unit IT = (
  fix%unit
    (I1 : %self(I1). (P : %unroll Unit IT -> Type) -> %unroll IT P (%unroll unit I1) -> %unroll IT P u)
    = %fix(u)
      : (P : %unroll Unit IT -> Type) -> %unroll IT P (%unroll unit I1) -> %unroll IT P u
      . (P : %unroll Unit IT -> Type) => %unroll I1 P ((x : %unroll IT P u) => x);

  %self(u). (P : %unroll Unit IT -> Type) ->  %unroll IT P unit -> %unroll IT P u
);



!unit =
  %fix(u)
    : (P : %unroll Unit IT -> Type) ->
      (x : %unroll IT P (%unroll unit I1)) => %unroll IT P u
    . (P : %unroll Unit IT -> Type) =>
    (x : %unroll IT P (%unroll unit I1)) => %unroll I1 P x;
!Unit =
  %self(u). (P : %unroll Unit IT -> Type) ->
    %unroll IT P (%unroll unit I1) -> %unroll IT P u;
!T_IT =
  %self(IT). (P : %unroll Unit IT -> Type) -> !Unit -> Type;
!T_I1 =
  %self(I1). (P : %unroll Unit IT -> Type) ->
    %unroll IT P (%unroll unit I1) -> %unroll IT P !unit;
!T_unit =
  %self(unit). (I1 : !T_I1) -> !Unit;
fix%Unit (IT : !T_IT) = (
  unit = %fix(unit : !T_unit). (I1 : !T_I1) => !unit;
  I1 = %fix(I1 : !T_I1). (P : %unroll Unit IT -> Type) =>
    (x : %unroll IT P (%unroll unit I1)) => %expand x;
  !Unit
);


!Unit = %self(u). %unroll Unit0 T_I1 unit0 I1 u;
!unit = %unroll unit0 I1;

!Unit0 =
  (P : !Unit -> Type) -> P !unit -> P u;
!T_unit0 =
  %self(unit0). (I1 : T_I1) -> !Unit;
!T_Unit0 =
  %self(Unit0). (T_I1 : Type) -> (unit0 : !T_unit0) ->
    (I1 : T_I1) -> (u : !Unit) -> Type;

!unit0 =
  %fix(u) : !Unit0. (P : !Unit -> Type) => (x : P !unit) =>   %unroll I1 P x;
!T_I1 =
  %self(I1). (P : !Unit -> Type) -> P !unit -> P !unit0;


cast : (%self(u). !Unit0) -> !Unit;

fix%Unit0 T_I1 unit I1 u = !Unit0;


fix%unit0 I1

fix%unit I1 = %fix(u) : %unroll Unit unit I1 u.
  (P : (self%u : %unroll Unit unit u) -> Type) =>
    (x : P (%unroll unit I1)) => %unroll I1 P x;
fix%unit = %unroll unit0


!Unit = %unroll Unit0 FT T_F1 unit0 F1;
!unit = %unroll unit0 F1;
!FT = %unroll FT T_F1 unit0 F1;

!Unit0 =
  %self(u). (P : !Unit -> Type) ->
    !FT P !unit -> !FT P u;
!unit0 = %fix(u : !Unit0).
  (P : !Unit -> Type) => (x : !FT P !unit) =>

!T_FT =
  %self(FT). (T_F1 : Type) -> (unit0 : !T_unit0) -> (F1 : T_F1) ->
    (P : !Unit -> Type) -> !Unit0 -> Type;
!T_unit0 =
  %self(unit0). (F1 : T_F1) -> !Unit0;
!T_Unit0 =
  %self(Unit0). (FT : !T_FT) -> (T_F1 : Type) ->
    (unit0 : !T_unit0) -> (F1 : T_F1) -> Type;

!T_F1 =
  %self(F1). (x : !FT P !unit) -> !FT P !unit0


fix%Unit0 FT T_F1 unit0 F1 = !Unit0;


%self(False, f). (P : False -> Type) -> P f

%self(Unit, u).

UnitT =
  %self(A, Unit).
    (Eq_A : Eq _ A A) =>
    (Eq_Unit : Eq _ Unit (

    )) =>
    _;

%fix(A, Unit). (Eq_A : Eq _ A UnitT) =>
  (Eq_Unit : Eq _ Unit)


Γ, A : Type, x : A |- T : Type
------------------------------
%self(A, x). T : Type

Γ, A : Type, x : A |- T
-----------------------
%self(A, x). T : Type


%self(A, Unit). (unit : A) -> Type;

%fix(A, Unit). W => unit =>
  %self(u). (P : Unit W -> Type) -> W P unit -> W P u;

Γ, x : T |- T : Type
------------------------------
%self(x). T : Type

%self(Unit, T).
  (eq : %exists(X). Eq _ U Type) ->
  Type;

%self(False). (eq : %self(eq). Eq _ (False eq) _) -> Type

%fix(False). (eq : %self(eq). Eq _ _ (False eq)) =>
  %self(f). (P : False eq -> Type) -> P (%unroll eq (X => X) f)

%self(False). (f : %self(f). %unroll False f) -> Type;


%fix(U, Unit). (unit : %self) =>
  (P : Unit -> Type) -> P u;

Unit W1 W2 = %self(Unit, u). (P : Unit -> Type) -> W1 Unit P u -> W2 Unit P u;

%self(A, x). (P : A -> Type) -> P x;

%unroll (%fix(A, x). A);

%self(A, x). A


fix%False W0 = (
  False = %unroll False W0;
  W0 = %unroll W0;
  %self(f). (P : False -> Type) -> W0 P f;
);



Unit : Type;
unit : Unit;

Unit = %self(u). (P : Unit -> Type) -> P unit -> P u;
unit = (P : Unit -> Type) => (x : P unit) => x;

// expanded
fix%Unit unit W0 W1 = (
  Unit = %unroll Unit unit W0 W1;
  unit = %unroll unit W0 W1;
  W0 = %unroll W0 W1;
  %self(u). (P : Unit -> Type) -> W0 P unit -> W0 P u;
);
fix%unit W0 W1 : %unroll Unit unit W0 W1 = (
  Unit = %unroll Unit unit W0 W1;
  unit = %unroll unit W0 W1;
  W0 = %unroll W0 W1;
  W1 = %unroll W1;
  %fix(u). (P : Unit -> Type) => (x : W0 P unit) => W1 P x;
);

Unit = %unroll Unit unit (%fix(W0). P => P) (%fix(W1). P => P);
unit : Unit = %unroll unit _ _;




fix%Bool true false W0 W1 W2 = (
  Bool = %unroll Bool true false W0 W1 W2;
  true = %unroll true false W0 W1 W2;
  false = %unroll false W0 W1 W2;
  W0 = %unroll W0 W1 W2;
  %self(b). (P : Bool -> Type) -> W0 P true -> W0 P false -> W0 P b;
);
fix%unit W0 W1 : %unroll Unit unit W0 W1 = (
  Unit = %unroll Unit unit W0 W1;
  unit = %unroll unit W0 W1;
  W0 = %unroll W0 W1;
  W1 = %unroll W1;
  %fix(u). (P : Unit -> Type) => (x : W0 P unit) => W1 P x;
);

//
fix%False (W0 : _A) =
  %self(f). (P : %unroll False\1 W0\2 -> Type) -> %unroll W0\2 P\4 f\3;

_A := %self(x). _B
%unroll W0 : _B[x := W0]

_A :=
%unroll W0\2 : _B[x := W0\2]

_A := %self(x). _B

(A : Type\1) -> A\2



fix%False (W0 : %self(W0).
  (P : %unroll False\1 W0\2 -> Type) ->
  (f : %self(f). (P : %unroll False\1 W0\2 -> Type) -> %unroll W0\2 P\5 f\4) ->
  %unroll False\1 W0\2
) =
  %self(f). (P : %unroll False\1 W0\2 -> Type) -> %unroll W0\2 P\4 f\3;


fix%False (W0 : _A) =
  %self(f). (P : %unroll False\1 W0\2 -> Type) -> %unroll W0\2 P\4 f\3;

W0\2 : _A[+2]

_A := %self(x). _B[-2]
%unroll W0\2 : _B[W0\2]

_B := (P : %unroll False\1 W0\2 -> Type) -> _C
%unroll W0\2 P\4 : _C[P\4]

_C := (f : %self(f). (P : %unroll False\1 W0\2 -> Type) -> %unroll W0\2 P\5 f\4) -> Type

%unroll W0\2 P\4 f\3 : Type


%self(x). (P : %unroll False\1 W0\2 -> Type) -> _C[-2]

f => value => f value;


(value : (value : _B))

(value : _B) -> ?


(_A x)[x := 2]
((x => ) y)

fix%Bool true false W0 W1 W2 = (
  Bool = %unroll Bool true false W0 W1 W2;
  true = %unroll true false W0 W1 W2;
  false = %unroll false W0 W1 W2;
  W0 = %unroll W0 W1 W2;
  W1 = %unroll W1 W2;
  W2 = %unroll W2;
  %self(b). (P : Bool -> Type) -> W0 P true -> W0 P false -> W0 P b;
);
fix%true false W0 W1 W2 = (
  Bool = %unroll Bool true false W0 W1 W2;
  true = %unroll true false W0 W1 W2;
  false = %unroll false W0 W1 W2;
  W0 = %unroll W0 W1 W2;
  W1 = %unroll W1 W2;
  W2 = %unroll W2;
  %fix(b). (P : Bool -> Type) => (x : W0 P true) => (y : W0 P false) => W1 P x;
);
fix%false W0 W1 W2 = (
  Bool = %unroll Bool true false W0 W1 W2;
  true = %unroll true false W0 W1 W2;
  false = %unroll false W0 W1 W2;
  W0 = %unroll W0 W1 W2;
  W1 = %unroll W1 W2;
  W2 = %unroll W2;
  %fix(b). (P : Bool -> Type) => (x : W0 P true) => (y : W0 P false) => W2 P y;
);

Bool : Type = %unroll Bool true false (%fix(W0). P => %expand P)
  (%fix(W1). P => %expand P) (%fix(W2). P => %expand P);
true : Bool = %unroll true false _ _ _;
false : Bool = %unroll true false _ _ _;

Bool : Type;
true : Bool;
false : Bool;

Bool = %self(b : Bool). (P : Bool -> Type) -> P true -> P false -> P b;
true = (P : Bool -> Type) => (x : P true) => (y : P false) => x;
false = (P : Bool -> Type) => (x : P true) => (y : P false) => y;


fix%False\1 W0\2 =
  %self(f\3). (P\4 : %unroll False\1 W0\2 -> Type) -> %unroll W0\2 P\4 f\3;

W0\2 : _A[+2]

_A := (%self(x\2). _B)[-2]
%unroll W0\2 : _B[x\2 := W0\2]

_B := (P\4 : %unroll False\1 W0\2 -> Type) -> _C
%unroll W0\2 P\4 : _C[x\2 := W0\2]

_C := (f : %self(f\3). (P\4 : %unroll False\1 W0\2 -> Type) -> %unroll W0\2 P\4 f\3) -> Type;
%unroll W0\2 P\4 f\3 : Type


fix%False\1 (W0\2 : _A) =
  %self(f\3). (P\4 : %unroll False\1 W0\2 -> Type) -> %unroll W0\2 P\4 f\3;

W0\2 : _A[-2]

_A := (%self(x). _B)[+2]
%unroll W0\2 : _B[x := W0\2]

(x : _A) => (y : _B) => x;

x : _A[+1]

_A := ((A : Type) -> A\2)[-1];

(x : _A) => (y : _B) => x


fix%False\1 (W0\2 : _A) =
  %self(f). (P : %unroll False\1 W0\2 -> Type) -> %unroll W0\2 P\4 f\3;

W0\2 : _A[+2]
_A := (%self(x). _B)[-2]
%unroll W0\2 : _B[x := W0\2]

P\4 : %unroll False\1 W0\2 -> Type
_B := (P : %unroll False\1 W0\2 -> Type) -> _C
%unroll W0\2 P\4 : _C[x := W0\2]

f\3 : %self(f). (P : %unroll False\1 W0\2 -> Type) -> %unroll W0\2 P\6 f\5
_C := (f : %self(f). (P : %unroll False\1 W0\2 -> Type) -> %unroll W0\2 P\6 f\5) -> Type;
%unroll W0\2 P\4 f\3 : Type


fix%False\1 (W0\2 :
  %self(W0\2). (P\3 : %unroll False\1 W0\2 -> Type) ->
    (f\4 : %self(f\2). (P : %unroll False\1 W0\2 -> Type) -> %unroll W0\2 P\5 f\4) -> Type
) =
  %self(f). (P : %unroll False\1 W0\2 -> Type) -> %unroll W0\2 P\4 f\3;

fix%False\1 (W0\2 : _A) =
  %self(f). (P : %unroll False\1 W0\2 -> Type) -> %unroll W0\2 P\4 f\3;

W0\2 : _A[+2]

_A := (%self(x). _B[+1])[-2]
%unroll W0\2 : _B[x := W0\2]

P\4 : %unroll False\1 W0\2 -> Type

_B := (P : %unroll False\1 W0\2 -> Type) -> _C[+1]
%unroll W0\2 P\4 : _C[x := W0\2]

f\3 : %self(f). (P : %unroll False\1 W0\2 -> Type) -> %unroll W0\2 P\5 f\4
_C := (f : %self(f). (P : %unroll False\1 W0\2 -> Type) -> %unroll W0\2 P\5 f\4) -> Type

%self(W0).
  (P : %unroll False\1 W0\2 -> Type) ->
  (f : %self(f). (P : %unroll False\1 W0\2 -> Type) -> %unroll W0\2 P\5 f\4) -> Type

fix%False (W0 : _A) =
  %self(f). (P : %unroll False\2 W0\1 -> Type) -> %unroll W0\2 P\0 f\1;

W0 : _A[+2]

_A := (%self(x). _B[+1])[-2]
%unroll W0\2 : _A[x := W0\2]

_B := (P : %unroll False\2 W0\1 -> Type) -> _C[+1]
%unroll W0\2 P\0 : _C[x := W0\2]

f\1 : %self(f). (P : %unroll False\3 W0\2 -> Type) -> %unroll W0\3 P\0 f\1
_C := (f : %self(f). (P : %unroll False\3 W0\2 -> Type) -> %unroll W0\3 P\0 f\1) -> Type

%self(W0). (P : %unroll False\3 W0\2 -> Type) -> (f : %self(f). (P : %unroll False\5 W0\4 -> Type) -> %unroll W0\5 P\0 f\1) -> Type


fix%False\1 (W0\2 : _A) =
  %self(f). (P : %unroll False\1 W0\2 -> Type) -> %unroll W0\2 P\4 f\3;


ind :
  %self(ind). (u : Unit) ->
    (P : Unit -> (P unit -> P u (%unroll ind u P)) -> Type) ->
    P unit (%unroll ind u) -> P u (%unroll ind u P);

%unroll u P (x : P unit) === x

match u with
| I => x
end ===


forall (A : Type) (x : A) (P : forall a : A, eq_t A x a -> Set),
       P x (eq_refl A x) -> forall (a : A) (e : eq_t A x a), P a e

eq_ind :
  (A : Type) -> (x : A) -> (y : A) -> (eq : Eq a x y) ->
  (P : (y : A) -> Eq A x y -> Type) ->
  P x (refl A x) -> P y eq
Eq A x y =
  %self(eq).
    (P : (y : A) -> Eq A x y -> Type) ->
      P x (refl A x) -> P y eq;


Eq : (A : Type) -> (x : A) -> (y : A) -> Type;
refl : (A : Type) -> (x : A) -> Eq A x x -> Type;

Eq A x y = %self(eq).
  (P : (y : A) -> Eq A x y -> Type) ->
  P x (refl A x) -> P y eq



Key : Type;
Frozen : {
  @Frozen : (A : Type, k : Key) -> Type;
  freeze : {A} -> (k : Key, x : A) -> Frozen(A, k);
  unfreeze : {A} -> (k : Key, x : Frozen(A, k)) -> A;
};

Nat_TE = ($Nat_key) => {
  // Nat.te
  @Nat = Frozen($Nat_key, Int);
  zero = Frozen.freeze($Nat_key, Int.zero);
  one = Frozen.freeze($Nat_key, Int.one);
  add = (n, m) => {
    n = Frozen.unfreeze($Nat_key, n);
    m = Frozen.unfreeze($Nat_key, m);
    Int.add(n, m)
  };
}
Nat_TEI = {
  // Nat.tei
  @Nat : Type;
  zero : @Nat;
  one : @Nat;
  add : (n : @Nat, m : @Nat) ->
};
M_TE = ($Nat_key, $M_key) => {
  Nat : Nat_TEI = Nat_TE($Nat_key);
  // M.te
  // this will fail
  one : Int = Nat.one;
};


(x : Frozen key Nat) =>
  unfreeze key


Unit : Type;
unit : Unit;

Unit = %self(u). (P : Unit -> Type) -> P unit -> P u;
unit = (P : Unit -> Type) => (x : P unit) => x;


fix%Unit I unit = (
  Unit = %unroll Unit I unit;
  I = %unroll I unit;
  unit = %unroll unit;
  %self(u). (P : Unit -> Type) -> P unit -> I P u;
);

fix%unit I = (
  unit = %unroll unit I;
  Unit = %unroll Unit unit;

)


%self(u). (P : Unit -> Type) -> P (I unit) -> P (I u)

T_u : Type;
unit : T_u;

T_u = %self(u). (P : Unit -> Type) -> P (I unit) -> P (I u);
unit = P => (x : P (I unit)) => x;

fix%Unit C = (
  Unit = %unroll Unit C;
  C = %unroll C;

  fix%unit C0 = (
    unit = %unroll unit C0;
    C0 = %unrol C0;

    %fix(u) :
      (P : Unit -> Type) -> P (C (C0 unit)) => x
      .
      (P : Unit -> Type) => (x : P (C (C0 unit))) => x
  );
  unit = %unroll unit _;

  %self(u). (P : Unit -> Type) -> P (C unit) -> P (C u)
);

Unit : %self(Unit). (u : %self(u). %unroll Unit u) -> Type;
unit : %self(u). %unroll Unit u;

Unit u = (P : (u : %self(u). %unroll Unit u) -> Type) -> P unit -> P u;
unit = (P : Unit -> Type) => (x : P unit) => x;

!Unit = %fix(Unit). K => u => (
  Unit = %self(u). %unroll Unit K u;
  K = %unroll K u;
  (P : Unit -> Type) -> P (K unit) -> P u
);
fix%UnitK unit = (
  Unit = !Unit;

);



%unroll Unit C u ->

Unit : Type;
unit : Unit;

Unit = %self(u). (P : %unroll Unit -> Type) -> P unit -> P u;
unit = (P : %unroll Unit -> Type) => (x : P unit) => x;

!unit =
  %fix(u): (P : Unit -> Type) -> P (C (%unroll unit C0)) -> P (C u).
    (P : Unit -> Type) => (x : P (C (%unroll unit C0))) => %unroll C0 P x;

!T_C0 =
  %self(C0). (P : Unit -> Type) -> P (C (%unroll unit C0)) -> P (C (!unit));

fix%Unit C = (
  Unit = %unroll Unit C;
  C = %unroll C;

  fix%unit (C0 : !T_C0) = !unit;
  C0 : !T_C0 = %fix(C0). P => P;

  unit = ;

  %self(u). (P : %unroll Unit C -> Type) ->
    P (C (%unroll unit ?)) -> P (C u);
);


received :
  %unroll I P (%fix(unit). P => (x : %unroll I P unit) => x) -> %unroll I P (%fix(unit). P => (x : %unroll I P unit) => x)


fix%unit I = (
  (P : Unit -> Type) => (x : P unit) =>
)


Unit Unit C unit C0 = (
  Unit = %unroll Unit C unit C0;
  C = %unroll C unit C0;
  unit = %unroll unit C0;
  C0 = %unroll C0;

  %self(u). (P : Unit -> Type) -> P unit -> C P u
);
unit Unit C unit C0 = (
  Unit = %unroll Unit C unit C0;
  C = %unroll C unit C0;
  unit = %unroll unit C0;
  C0 = %unroll C0;

  %fix(u). (P : Unit -> Type) => (x : P unit) => C0 P x;
);

fix%Unit C unit C0 = (
  ;
);
fix%unit =


Unit : (u : %self(u). %unroll Unit u) -> Type;
unit : %unroll Unit unit

Unit u = (P : (u : %self(u). %unroll Unit u) -> Type) -> P unit -> P u;
unit = (P : (u : %self(u). %unroll Unit u) -> Type) => (x : P unit) => x;


Bool0 = {
  self%Bool : (self%b : %unroll Bool b) -> Type;
  self%true : %unroll Bool true;
  self%false : %unroll Bool false;

  Bool b = (P : (self%b : Bool b) -> Type) ->
    P true -> P false -> P b;
  true = P => x => y => x;
  false = P => x => y => y;
};

Bool = %self(b). %unroll Bool0.Bool b;
true : Bool = Bool0.true0;
false : Bool = Bool0.false0;





Mutual
  (T_T : Type)
  (M_T_T : (T : T_T) -> Type)
  (T_C0 : (T : T_T) -> Type)
  (M_T_C0 : (T : T_T) -> (C0 : T_C0 T) -> Type)
  (E_T : %self(E_T).
    (T : %self(T). (C0 : T_C0 T) -> M_T_T (%unroll E_T T C0)) ->
    (C0 : T_C0 T) -> T_T)
  (E_C0 : %self(E_C0). (T : T_T) ->
    (C0 : %self(C0). M_T_C0 T (%unroll E_C0 T C0)) -> T_C0 T)
  (M_T : (T : T_T) -> (C0 : T_C0 T) -> M_T_T T)
  (M_C0 : (T : T_T) -> (C0 : T_C0 T) -> M_T_C0 T C0)
  (T : T_T, C0 : T_C0 T) = (
  fix%T (C0 : T_C0 T) : M_T_T (%unroll E_T T C0) = M_T (%unroll E_T T C0);
  fix%C0 : M_T_C0 (%unroll E_C0 T C0) = M_C0 T (%unroll E_C0 T C0);
  (%unroll E_T T C0, %unroll E_C0 T C0);
);


// from
Unit : (u : %self(u). %unroll Unit u) -> Type;
unit : %unroll Unit unit

Unit u = (P : (u : %self(u). %unroll Unit u) -> Type) -> P unit -> P u;
unit = (P : (u : %self(u). %unroll Unit u) -> Type) => (x : P unit) => x;

// to
(Unit, unit) = Mutual
  // types
  (%self(Unit). (u : %self(u). %unroll Unit u) -> Type)
  (Unit => (u : %self(u). %unroll Unit u) -> Type)
  (Unit => %self(unit). %unroll Unit unit)
  (Unit => unit => %unroll Unit unit)
  // equalities
  (%fix(E_Unit). Unit => unit => Unit)
  (%fix(E_unit). Unit => unit => unit)
  // values
  (Unit => unit => u =>
    (P : (u : %self(u). %unroll Unit u) -> Type) -> P unit -> P u)
  (Unit => unit =>
    (P : (u : %self(u). %unroll Unit u) -> Type) => (x : P unit) => x);



// (fst : %self(fst). (s : %self(s). (K : Type) -> (A -> B (%unroll fst s) -> K) -> K) -> A)

fix%fst0 (A : Type) (B : A -> Type)
  (s : %self(s). (K : Type) -> (A -> B (%unroll fst0 A s) -> K) -> K) =
  %unroll s A ((x : A) => (y : B (%unroll fst0 A s)) => x);

Pair (A : Type) (B : A -> Type) =
  %self(s). (K : Type) -> (A -> B (%unroll fst0 A s) -> K) -> K;
pair (A : Type) (B : A -> Type) (x : A) (y : B x) : Pair0 A B =
  %fix(s). (K : Type) => (k : A -> B x -> K) => k x y;
fst (A : Type) (B : A -> Type) (s : Pair A B) =
  %unroll fst0 A s;
snd (A : Type) (B : A -> Type) (s : Pair A B) =
  %unroll s (B (fst A B s)) ((x : A) => (y : B (fst A B s)) => y);

self%Unit : Type;
unit : Unit;

Unit = %self(u). (P : (u : %self(u). %unroll Unit u) -> Type) -> P unit -> P u;
unit = (P : (u : %self(u). %unroll Unit u) -> Type) => (x : P unit) => x;

!MUnit = %unroll MUnit MI;

!Unit = fst Type (Unit => Unit) !MUnit;
!unit = snd Type (Unit => Unit) !MUnit;

!Unit0 = %self(u). (P : !Unit -> Type) -> P (!unit) -> T P u;
!unit0 = %fix(u). (P : !Unit -> Type) => (x : P (!unit)) => C0 P x;

!T_T = %self(T). (P : !Unit -> Type) -> (u : !Unit0) -> Type;

!T_C0 = %self(C0). (P : !Unit -> Type) -> P (!unit) -> T P (!unit0);
!T_MI = Pair (!T_T) ((T : !T_T) => !T_C0);
!T_MUnit = %self(MUnit). (MI : !T_MI) -> Pair Type (Unit => Unit);

fix%MUnit (MI : !T_MI) =
  pair Type (Unit => Unit) (!Unit0) (!unit0);

MI = %fix(MI).
Eq (A : Type) (x : A) (y : A) =
  (P : A -> Type) -> P x -> P y;

Bool =
  %self(k). (K : Type) ->
    ( -> K) -> K -> K;

case (b : ?) (K : Type) (x : ?) (y : ?) =
  %unroll b K x y;

T =%self(eq). (K : Type) -> (x : _) -> (y : _) -> Eq _ (case b K x y) (x eq);
case b Int ((eq : ) => 0) (() => 1)


;

  (Unit => unit => %unroll Unit unit)

  (Unit => unit => u => (P : (u : %self(u). %unroll Unit u) -> Type) -> P unit -> P u)
  (Unit => unit => (P : (u : %self(u). %unroll Unit u) -> Type) => (x : P unit) => x);
T_False : Type;
T_C : (False : T_False) -> Type;

T_False =


T_False =
  %self(False).
    (C : %self(C). (f : %self(f). (P : %unroll False C -> Type) -> P (%unroll C f)) -> %unroll False C)
  -> Type;
T_C (False : T_False) =
  %self(C). (f : %self(f). (P : %unroll False C -> Type) -> P (%unroll C f)) -> %unroll False C;

fix%False (C : %self(C). (f : %self(f). (P : %unroll False C -> Type) -> P (%unroll C f)) -> %unroll False C) =
  %self(f). (P : %unroll False C -> Type) -> P (%unroll C f);

Bool = (A : Type) -> A -> A -> A;
true : Bool = A => x => y => x;
false : Bool = A => x => y => y;

Bool = %frozen key Bool;
true : Bool = %freeze key true;
false : Bool = %freeze key false;



fix%fst0 (A : Type) (B : A -> Type)
  (s : %self(s). (K : Type) -> (A -> B (%unroll fst0 A s) -> K) -> K) =
  %unroll s A ((x : A) => (y : B (%unroll fst0 A s)) => x);

Pair (A : Type) (B : A -> Type) =
  %self(s). (K : Type) -> (A -> B (%unroll fst0 A s) -> K) -> K;
pair (A : Type) (B : A -> Type) (x : A) (y : B x) : Pair0 A B =
  %fix(s). (K : Type) => (k : A -> B x -> K) => k x y;
fst (A : Type) (B : A -> Type) (s : Pair A B) =
  %unroll fst0 A s;
snd (A : Type) (B : A -> Type) (s : Pair A B) =
  %unroll s (B (fst A B s)) ((x : A) => (y : B (fst A B s)) => y);


%self(fst0).
  (A : Type) -> (B : A -> Type) ->
  (s : %self(s). (K : Type) -> (A -> B (%unroll fst0 A s) -> K) -> K) ->
  A;

fix%fst0 (A : Type) (B : A -> Type)
  (s : %self(s). (K : Type) -> (A -> B (%unroll fst0 A s) -> K) -> K) =
  %unroll s A ((x : A) => (y : B (%unroll fst0 A s)) => x);

%self(fst0).
  (A : Type) -> (B : A -> Type) ->
  (s : %self(s). (K : Type) -> (A -> B (%unroll fst0 A s) -> K) -> K) ->
  A;


%self(False). (f : %self(f). %unroll False f) -> Type;

False : %self(x). _A;

// enter arrow
_A := (f : _C) -> _D;
// enter param
f : %self(f). _B
_C := %self(f). _B;

%unroll False : (f : %self(f). _B[x := False]) -> _D[x := False];
_B := %unroll False _E


%unroll False f : Type

%self(f). %unroll False _E[x := False] `unify` %self(f). %unroll False f

_D[x := False] `unify` %self(f). %unroll False f
_B := %self(f). %unroll False f;
// leave f

_A := %self(x).


%self. (%self. %unroll \1 \0) -> Type;
%self. (%self. %unroll (\1 : _A) (\0 : _B)) -> Type;

_A := %self. _C
%self. (%self. (%unroll \1 : _C) \0) -> Type;

_C[\1] `unify` (_D -> Type);
_B := _C[\1]
%self. (%self. %unroll \1 \0) -> Type

_B := %self. %unroll \1 \0;


%self(False0). (f : %self(f0). %unroll False0 f0) -> Type;


False0 : %self(False1). _A[False0 := False1]

// enter arrow
_A := (f : _C) -> _D;

// enter %self(f0)
f0 : %self(f1). _E
_C := %self(f0). _F


%unroll False0 : (f : %self(f0). _F[False1 := False0]) -> _D[False1 := False0]

_A[False1 := False0] `unify`
(f : %self(f). %unroll False f) -> Type



%self(False0). (f : %self(f0). %unroll False0 f0) -> Type;

False0 : %self(False1). _A[False0 := False1]
f0 : %self(f1). _B[f0 := f1]

%unroll False0 : _A[False0 := False1][False1 := False0]

_A[False0 := False1][False1 := False0] `unify` (f : _C) -> _D
_A :=
%unroll False0 f


%self(False0). (f : %self(f0). %unroll False0 f0) -> Type;

// enter self False
False0 : %self(False1). _A[False0 := False1]

// enter arrow
_A := (f : _B) -> _C

// enter self f
_B := %self(f0). _D
f0 : %self(f1). _D[f0 := f1]

// enter apply
_D := _F _G;

// enter unroll
_F := %unroll False0;
%unroll False0 : _A[False0 := False1][False1 := False0]


(f : %self(f0). %unroll False0 f0) -> _C[False0 := False1][False1 := False0]
// enter f0
_G := f0


_C[False0 := False1][False1 := False0] `unify` Type

expected

received : %self(f1). _E[f0 := f1]
expected : %self(f0). unroll False0 _F[False0 := False1][False1 := False0]



%self(False). (f : %self(f). %unroll False f) -> Type;

False : %self(False). _A
f : %self(f). _B

_A := %self(x).
%unroll False : _A


_A[x := y] === _A


call = f => x => (f : _A -> _B) x

id : (A : Type) -> A -> A;

x = call{Type, ?}(id, A);


%self(False0). (f : %self(f0). %unroll False0 f0) -> Type;

False0 : %self(False1). _A[False0 := False1]

%unroll False0 : _A[False1 := False0]


_A := (f : %self(f0). %unroll False0 f0) -> Type


f : (x : _A) -> _B
f y : _B y

(f y : Int)

%self(False0). (f : %self(f0). %unroll False0 f0) -> Type;

False0 : %self(False1). _A[False0 := False1]
f0 : %self(f1). _B[f0 := f1]

%unroll False0 : _A[False0 := False1][False1 := False0]
%unroll False0 : _A

_A := (f : _C) -> Type
_C := %self(f1). _B[f0 := f1];
%unroll False0 f0 : Type
// leave f0

(f : %self(f1). %unroll False0 f1) -> Type
`unify`
(f : %self(f0). %unroll False0 f0) -> Type


%self(False). (%self(f). %unroll False\1 f\0) -> Type;

// enter False
False : _A

// enter f
f : _B

(False\1 : _A[+1])

_A := %self(False). _C
%unroll False\1 : _C[+1]

_C := (_B[-1]) -> Type;
%unroll False\1 f\0 : Type;

_B := %self(f). %unroll False\2 f\0
// leave f

%self(False). (%self(f). %unroll False\1 f\0) -> Type `unify`
%self(False). (%self(f). %unroll False\1 f\0) -> Type

List : {
  @List : (A : Type) -> Type;
  map : {A, B} -> (f : A -> B, l : List A) -> List B;
} = _;

Mappable = {
  @Container : Type;
  map : {A, B} -> (f : A -> B, l : Container A) -> Container B;
};

map {M : Mappable} (x, f) = M.map(f, x);

User : (User : Type) & Map [
  ("id", User -> Nat)
  ("name", User -> String)
] = {
  id : Nat;
  name : String;
};

User = Record [
  ("id", Nat);
  ("name", String);
];

eduardo = Record.make User [
  ("id", 0);
  ("name", "Eduardo");
];
Map.get("id", eduardo)


User : {
  id : User -> Nat;
  name : User -> String;
};

user.id;
User.id(user)
l.map(x => )


l_ = List.map(f, l);
l.map(f);

map{List}(f, l);
l.map{List}(f);

x = List.map(f, l);
x = 1;







User : {
  id : Nat;
  name : String;
};

User = Record [
  ("id", Nat);
  ("name", String);
];
eduardo = User [
  ("id", 0);
  ("name", "Eduardo");
];

"id"

_Int\0 -> _Int\0
(A : Type) => A\0


(A : Type) => _A

%self(False0). (f0 : %self(f0). %unroll False0 f0) -> Type;

False0 : %self(False1). _A[False0 := False1]
f0 : %self(f1). _B[f0 := f1]

%unroll False0 : _A

_A := (f0 : _B) -> Type


%self(f1). _B[f0 := f1] `unify` %self(f0). %unroll False0 f0

_B[f0 := f1][f1 := f2] `unify` (%unroll False0 f0)[f0 := f2]

%self(False0). (f0 : %self(f0). %unroll False0 f0) -> Type;

False0


%self(False0/1). (f0 : %self(f0/2). %unroll False0/1 f0/2) -> Type;

False0 : %self(False1/1). _A
f0 : %self(f1/2). _B

False0/1 : (%self(False1/1). _A)[+2]
%unroll False0/1 : _A[+1]

f0/2
_A := (f0 : (%self(f1/2). _B)[-1]) -> Type

(A : Type) -> 'A -> 'A

Int/+2 -> Int/+2

(A : Type) -> A/-1 -> A/-2



(A : Type) -> (A/+3 -> A/+3)[close 3]

(A : Type) -> (A/+3 -> A/+3)[close 3]

%self(False0/1). (f0 : %self(f0/2). %unroll False0/1 f0/2) -> Type;

False0/+1 : %self(False1). _A[False0/+1 := False1/-1]
f0/+2 : %self(f1). _B[f0/+2 := f1/-1]

%unroll False0/+1 : _A[False0/+1 := False1/-1][False1/-1 := False0/+1]

%self(False0/1). (f0 : %self(f0/2). %unroll False0/1 f0/2) -> Type;

False0/+1 : %self(False1). _A[close +1]
f0/+2 : %self(f1). _B[close +2]

%unroll False0/+1 : _A[close +1][open +1]

_A := (f0 : %self(f1). _B[close +2]) -> Type;
%unroll False0/+1 f0/+2 : Type

_B := %unroll False0/+1 f0/+2

%self(False0). (f0 : %self(f0). %unroll False0/-2 f0/-1) -> Type;

False0 : %self(False1). _A[close +1]
f0 : %self(f1). _B[close +2]

_A := (f0 : %self(f1). _B[close +2]) -> Type;

%self(f1). _B[close +2] `unify` %self(f0). %unroll False0/-2 f0/-1
_B[close +2] `unify` %unroll False0/-2 f0/-1
_B `unify` %unroll False0/-2 f0/-1

_B := (%unroll False0/-2 f0/-1)[open +2]
_B := %unroll False0/-2 f0/+2

%self(False0). (f0 : %self(f0). %unroll False0/-2 f0/-1) -> Type;

False0 : _A
f0 : _B

_A := %self(False1). _C
%unroll False0/+1 : _C

_C := (f0 : _B) -> Type
%unroll False0/+1 f0/+2

_B := %self(f0). %unroll False0/+1 f0/-1

%self(False1). %self(f0). %unroll False0/+1 f0/-1

%self(False0). (f0 : %self(f0). %unroll False0/-2 f0/-1) -> Type;

False0 : _A
f0 : _B

_A := %self(False1). _C
%unroll False0/-2 : _C[False0/-2]

_C := (f0 : _B) -> Type
_B := %self(f0). %unroll False0/-2 f0/-1

%self(False1). (f0 : %self(f0). %unroll False0/-2 f0/-1) -> Type


%self. (%self. %unroll /1 /0) -> Type;

/1 : _A
/
%self. (%self. %unroll /1 /0) -> Type;

/1 : _A[+1]
/0 : _B

_A := %self. _C[-1]
%unroll /1 : _C[/1]

_C := (f0 : _B) -> Type
_B := %self. %unroll /1 /0

%self. (f0 : %self. %unroll /1 /0) -> Type

A\

%self(False0). (f0 : %self(f0). %unroll False0/-2 f0/-1) -> Type;
False0 : %self. _A
f0 : %self. _B

%self(False0). (f0 : %self(f0). %unroll False0 f0) -> Type

False0 : %self(False1). _A
f0 : %self(f1). _B

%unroll False0 : _A[False1 := False0]

Option A =
  (tag : Bool, tag | true => A | false => Unit);

Option A =
  | (tag : "some", A)
  | (tag : "none");

(f : Option Int) =>
  switch (f) {
  | ("some", x) => x + 1
  | ("none") => 0
  };
(f : Option Int) =>
  match f
  | ("some", x) => x + 1
  | ("none") => 0;

(f : Option Int) =>
  match f with
  (("some", x) => x + 1)
  (("none") => 0)
  end;

(f : Option Int) =>
  f
  | ("some", x) => x + 1
  | ("none") => 0


Bool = (A : Type) -> A -> A -> A;
true : Bool = A => x => y => x;
false : Bool = A => x => y => y;

accepts_string_or_nat
  : (pred : Bool) -> (x : pred Type String Nat) -> pred Type String Nat
  = (pred : Bool) => (x : pred Type String Nat) => x;

accepts_string
  : (x : String) -> String
  = accepts_string_or_nat true;


Option A =
  | (tag : true, payload : A)
  | (tag : false);

OptionX A =
  | (tag : false)
  | (tag : true, payload : A);

Option A =
  (tag : Bool, payload : tag | true => A | false => Unit);


((A : Type) => (x : A) => x) Int : Int -> Int

f : (A : Type) -> A -> A
Int : Type


f : (A : _A) -> _B


(A : Type)

(A/+2)[close +2 to -1] -> A/+2[close +2 to -2]

[], [] |- _A[close +2 to -1] `unify` _B[open -1 to +2]
[close +2 to -1], [open -1 to +2] |- _A `unify` _B

_A := _B

Int `unify` String

_A `unify` (A : Type) -> A -> A

%self(False0). (f0 : %self(f0). %unroll False0 f0) -> Type;

False0/+2 : %self(False1). _A[close False0/+2]
f0/+3 : %self(f1). _B[close f0/+3]

%unroll False0/+2 : _A[close False0/+2][open False0/+2]

_A[close False0/+2][open False0/+2] `unify`
(f0 : %self(f1). _B[close f0/+3]) -> Type


%self(False0). (f0 : %self(f0). %unroll False0 f0) -> Type;
False0 : %self(False1). _A[False0 := False1]
f0 : %self(f1). _B[f0 := f1]

%unroll False0 : _A

_A := (f0 : %self(f1). _B[f0 := f1]) -> Type;
%unroll False0 f0 : Type

_B := %unroll False0 f0

_A{x := y} `unify` _A{y := 1}

%self(False0). (f0 : %self(f0). %unroll False0 f0) -> Type;
False0/2 : %self(False1). _A/2[False0/2 := False1/-1]
f0/3 : %self(f1). _B[f0/3 := f1/-1]


_C/4 := (x : _A/2) -> _B/3

(x : _A) -> _B[+2 `close` -1] `unify` (A : Type) -> A/-1 -> A/-2

_A := Type

_B[+2 `close` -1] `unify` A/-1 -> A/-2

_B{+2 `close` -1} `unify` A/-1 -> A/-2

_B := (A/-1 -> A/-2)[-1 `open` +2]

(x : _A) -> _B


(x : _A) -> _B `unify` (A : Type) -> A/-1 -> A/-2


_B[x := y] `unify` Int
_B := Int

_B y
_B := _ => Int

_B[A := Int] `unify` _B[A := String]


(x : _A) -> _B[-1 `open` +2] `unify` (A : Type) -> A/-1 -> A/-2

_B := A/-1 -> A/-2

%self(False0). (f0 : %self(f0). %unroll False0 f0) -> Type;

False0/+2 : %self(False1). _A[/+2 `close` /-1]
f0/+3 : %self(f0). _B[/+3 `close` /-1]

%unroll False0/+2 : _A[/+2 `close` /-1][/-1 := False0/+2]

_A := (f0 : %self(f0). _B[/+3 `close` /-1]) -> Type
%unroll False0/+2 f0/+3

_B := %unroll False0/+2 f0/+3

%self(False1). (f0 : %self(f0). %unroll False0/-2 f0/-1) -> Type



#self[x => x]
@self[]

@deriving(show)
User = {
  id : Nat;
};

@rec(User) = {

};

@self(x -> x)
@fix(x => x)
@unroll(M)
@unroll()
@unfold()

%fix(False). %self(f). (P : False -> Type) -> P f;


%self(False0). (f0 : %self(f0). %unroll False0 f0) -> Type;

False0/+2 : %self(False1). _A[/+2 `close` /-1]
f0/+3 : %self(f0). _B[/+3 `close` /-1]

%unroll False0/+2 : _A[/+2 `close` /-1][/-1 := False0/+2]

_A := (f0 : %self(f0). _B[/+3 `close` /-1]) -> Type
%unroll False0/+2 f0/+3

_B := %unroll False0/+2 f0/+3

Nat : {
  @Nat : Type;
} = {
  @Nat = Int;
};

Alias : Type = (x : Int) -> Int;

JSON : {
  ... : Type;
} = _;

Nat : {
  T : Type;
} = {
  T = Int;
};


(x : Nat) =>

JSON =
  | Null
  | Bool(b : Bool)
  | Number(n : Float64)
  | String(s : String)
  | Object(d : List(String, @JSON))
  | Array(l : List(@JSON));
JSON = JSON & {

};

User : (User : Type) & {
  id : (user : User) -> Nat;
  name : (user : User) -> String;
} = {
  id : Nat;
  name : String;
};

x = User.id(user);

(x : fst T).k === (snd T).k;

(x : T).k === T::k(x);

User : (User : Type) & fst {
  id : Nat;

} = {
  id : Nat;
  name : String;
};

eduardo = { id = 0; name = "Eduardo"; };
x = eduardo::name;
User::id(user);


(x : T).k === T::k(x);

User : (User : Type) & {
  id : (user : User) -> Nat;
  name : (user : User) -> Nat;
} = (User = {
  id : Nat;
  name : String;
}) & {
  id = (user : User) => user::id;
  name = (user : User) => user::name;
};


Show = {
  T : Type;
  show : (x : S) -> String;
};
show{S : Show}(x : S.T) = S.show()
// User.tei
User = {
  @User = {
    id : Nat;
    name : String;
  };
  impl@Show{self} = {
    show () =
      self.name ++ ": " ++ self.id.show();
  };
};

User = {
  ... = extend({
    id : Nat;
    name : String;
  });
}
... = include(Show);

(User = {
  id : Nat;
  name : String;
}) & {
  show({self}) =
    self.name ++ ": " ++ self.id.show();
};

User : (User : Type) & ((user : {
  id : Nat;
  name : String;
}) -> User) & {

}
eduardo = User {
  id = 0;
  name = "Eduardo";
};
print(eduardo.show());


(user : User).id === User::id(user);

(user : fst {
  id : Nat;
}).id === snd {
  id : Nat;
}.id(user)
(snd {
  id : Nat;
} : fst {
  id : (user : fst {
    id : Nat;
  }) -> Nat;
}).id === snd {
  id : (user : fst {
    id : Nat;
  }) ->
}.id(User);

JSON : Type & {

} = {
  ... @JSON =
    | Null
    | Bool(b : Bool)
    | Number(n : Float64)
    | String(s : String)
    | Object(d : List(String, @JSON))
    | Array(l : List(@JSON));
};

@self(JSON, _). (
  JSON : Type,
)
( == ) = (
  ( == ) = (P : ) &
  ( == ) =
)
f = x => x;

add = (a, b) => a + b;
f(1, 2)

deprecate : {A} ->


external : (key : String, A : Type) -[Const]> A;

hello = external("teika_c_hello", () -> String);


User = {
  id : Nat;
  name : String;
};

Row : {
  type : (row : Row) -> Type;
} = _;
Record : {
  Record = List(Row);
  type : (record : Record) -> Type;

  make : (rows : List(row : Row, value : Row.type row)) ->
    Record.type(List.map(fst, rows));

  keyof : (A : Type) -[Error]>
} = _;

keyof(User)


T_fst0 = %self(fst0).
  (A : Type) -> (B : Type -> Type) ->
  (s : %self(s). (K : Type) -> (A -> B (%unroll fst0 A B s) -> K) -> Type) -> A;
fst0 = %fix(fst0 : T_fst0).
  A => B => s => %unroll s A (x => y => x);

Sigma (A : Type) (B : Type -> Type) =
  %self(s). (K : Type) -> (A -> B (%unroll fst0 A B s) -> K) -> Type;
fst (A : Type) (B : Type -> Type) (s : Sigma A B) : A =
  %unroll fst0 A B s;
snd (A : Type) (B : Type -> Type) (s : Sigma A B) : B (fst A B s) =
  %unroll s (B (fst A B s)) (x => y => y);

// CPSify

// from
(snd e : B (fst e))
// to
{K} => (k : B (fst e) -> K) =>
  e (y => snd y k)

T_fst0 = %self(fst0). {A} -> {B} ->
  (s : %self(s). {K} -> (A -> B (%unroll fst0 s) -> K) -> K) ->
  {K} -> (A -> K) -> K;

fst0 : T_fst0 = %fix(fst0). {A} => {B} =>
  s => {K} => k => %unroll s (x => y => k x);

Sigma A B =
  %self(s). {K} -> (A -> B (%unroll fst0 s) -> K) -> K;
fst {A} {B} (s : Sigma A B) =
  %unroll fst0 s;
snd {A} {B} (s : Sigma A B) {K} (k : B (fst s) -> K) =
  %unroll s K (x => y => k y);



E_T :

((x : {K} -> (Int -> K) -> K) -> x == 1)
(x = 1, eq : x == 1 = eq_refl)


(k : B (fst A B e)) => e ((s : Sigma A B) -> k (snd s))


// from
(e : Sigma A B)

// to
e : %fix(T). %self(e). {K : T -> Type} ->
  (k : (y : Sigma A B) -> K (%fix(w). {K} => (k : (y : _) -> K w) => k y)) ->
  K e



```

## CPSify Sigma

CPSifying Sigma seems to suffer from the same issue as lambda encoding of any inductive type, which is self reference.

The lambda encodings of inductive types can be solved by using self types, instead of declaring unit using the traditional church encoding, by using self types we self encode the induction principle, allowing to be known that `(u : Unit) -> u == unit`.

```rust
// traditional church encoding of unit, no induction
Unit : Type;
unit : Unit;

Unit = (A : Type) -> (x : A) -> A;
unit = A => x => x;

// self dependent encoding of unit, inductive
Unit : Type;
unit : Unit;

Unit = %self(u). (P : Unit -> Type) -> P unit -> P u;
unit = P => x => x;
```

Instead of doing the traditional polymorphic CPS version, we can do a self referential CPS version

```rust
// original
x : A;
x = e;

// traditional CPS polymorphic
CPS : (A : Type) -> Type;
CPS = A => (K : Type) -> (k : (x : A) -> Type) -> K;

x : CPS A;
x = K => k => k e;

// self referential version
CPS : (A : Type) -> Type;
CPS = A => %self(x). (K : CPS A -> Type) ->
  (k : (y : T) -> K (K => k => k y)) -> K x;

x : CPS A;
x = K => k => k e;
```

This allows to transform any external callback.

```rust
example : B (fst x)
example = x (a => B (fst a)) (y => snd y);
```

## Algebra

```rust
f(x) = x + 1;

f(2);
2 + 1


n = m

1 + n = 1 + m


n - 1 = m - 1

1 + x = 2
x = 2 - 1

(1 + x) - 1 = (2) - 1
x = 2 - 1

```

## Back to Teika

```rust
sort effects to achieve a set

extensions should be environment scoped, as in open should import extensions

@simpl, @cbn, @cbv, with a simple heuristics when the type is huge

expansion heuristics based on grading both for type level and term level

maybe type of dependent let should be a let
then, should type of a dependent apply be an apply?


() -> IO (Result String Error);
() -> Result (IO String) Error;

() -[IO | Error]> String;
() -[Error | IO]> String;

() -> Eff [IO, Error] String;
() -> Eff [Error, IO] String;

Eff effects ret = (
  effects = Set.of_list(Effect => Effect.name, effects);
  RawEff effects ret;
)
() -> Eff [IO, Error] String;
() -> Eff [Error, IO] String;


fold : (b : Bool) -> (A : Type) -> A -> A -> A;
// internalizing works
Bool = (A : Type) -> A -> A -> A;

ind : (b : Bool) -> (P : Bool -> Type) -> P true -> P false -> P b;
// internalizing fails, unbounded b, plus requires mutual recursion
Bool = (P : Bool -> Type) -> P true -> P false -> P b;

// with self the following can be done
Bool : Type;
true : Bool;
false : Bool;

Bool = %self(b). (P : Bool -> Type) -> P true -> P false -> P b;
true = P => x => y => x;
false = P => x => y => y;


(x : _A) -> _B `unify` (x : Int) -> P x\-0


(A : Type) => (x : A\-1) => (x : A\-2)
(x : A\+1) => (x : A\+1)

(x = 3; id (x + 1))
id (x + 1)

(A = Int; (1 : Int))

() -> Option(String);

IO(Option(String));
Option(IO(String));

Effect = (l, r) => (
  l = sort(l);
  Raw(l, r)
);

expected : () -> Eff([IO; Option], String);
received : () -> Eff([Option; IO], String);

read : () -[IO]> String;
f = () => read();

({
  T = Int;
  x : T = 1;
} :> {
  x : Int
}) === {
  x : Int = 1;
}

({
  T = Int;
  x : T = 1;
} :> {
  x : Int
})


Int0 = _;
x0 : Int0;

Int = @frozen(((A : Type) => A) Int0);
x0 : Int = @freeze(x0);

Unit0 = _;
unit0 = _;

Unit = @frozen(Unit0);
unit = @freeze(unit0);
ind_unit = (u : Unit) => @unroll(@unfreeze(u));


Eq {A} x y : Type;
refl {A} x : Eq x x;

Eq {A} x y =
  @self(eq ->
    (P : (z : A) -> Eq x z -> Type) ->
    P x (refl x) -> P y eq
  );
refl {A} x = other => P => (x : P other x (refl x)) =>
  other (refl x) (eq => _ => _ => P eq x (refl x)) x;


(x : Eq a b) => (p : P a) =>
  x (refl a)
expected : P other x (refl x) -> P (refl x) x (refl x)
received : P other x (refl x) -> P (refl x) x (refl x)
uip : {A} -> (x : A) -> (eq : Eq x x) -> Eq eq (refl x)
  = {A} => x => eq =>
    eq (_ => eq0 => Eq eq0 (refl x)) (refl (refl x))

Eq (eq (z => _ => Eq z y) eq) (refl x)

f x = g x
f = g

f x = g x



f P f = g P g -> f = g;

!T_IT =
!T_unit = @self(unit -> @unroll Unit IT I0 unit);
!T_

T_Unit = @self(Unit ->
  T_unit = @self(unit -> (IT : _) -> (I0 : _) -> @unroll Unit unit IT I0);
  T_IT = (unit : T_unit) -> @self(IT -> (I0 : _) ->
    (P : _) -> (u : @self(u -> (P : _) -> P (@unroll unit IT I0) -> @unroll IT I0 P u)) -> _)
  (unit : @self(unit -> (IT : _) -> (I0 : _) -> @unroll Unit unit IT I0)) ->
  (IT : @self(IT -> (I0 : _) ->
    (P : _) -> (u : @self(u -> (P : _) ->
      P (@unroll unit IT I0) -> @unroll IT I0 P u)) -> _)) ->
  (I0 : @self(I0 ->
    (P : _) -> P unit -> P (@fix((u : Unit) => (P : Unit -> Type) => (x : P unit) => I0 P x)))) ->
  Type
);
Unit = @fix(Unit => unit => IT => I0 => (
  Unit = @unroll Unit unit IT I0;
  unit = @unroll unit IT I0;
  IT = @unroll IT I0;
  I0 = @unroll I0;


  @self(u -> (P : Unit -> Type) -> P unit -> IT P u);
));
unit = @fix(unit => IT => I0 => (
  Unit = @unroll Unit unit IT I0;
  unit = @unroll unit IT I0;
  IT = @unroll IT I0;
  I0 = @unroll I0;

  @fix((u : Unit) => (P : Unit -> Type) => (x : P unit) => I0 P x)
));

Unit = @unroll Unit;

Eq0 A x y = (P : A -> Type) -> P x -> P y;
Eq1 A x y


Nullable (A : Type) (not_nullable : Repr.not_nullable A) = A | null;
Nullable (Nullable A not_nullable) (?)

@mu(x -> )


add = (a, b) => a + b;

expr
| true => 1
| false => 2

Bool = | true | false;

add_and_mul = (a, b) => (
  c = a + b;
  a * c;
);

add_and_mul = (a, b) => (
  c = a + (
    d = b;
    b + 1
  );
  a * c
);

no logical system can be complete and consistent

Naive set theory
does the set of all sets contains themselves?

forall x : Nat, (y : Nat, y > x)

1 = 0

(x => x + 1)(1)
1 + 1
2

f : (x : Nat) -> Fix Nat;
f : (x : Nat) -> Nat;

sort = selection_sort;
is_a_sort : bubble_sort == selection_sort = _;

f = l => first(sort(l));
f = subst(sort, f, bubble_sort);


fix = f => f(f);

Int = @frozen(((A : Type) => A) Int0);
x0 : Int = @freeze(x0);

Unit0 = _;
unit0 = _;

Unit = @frozen(Unit0);
unit = @freeze(unit0);
ind_unit = (u : Unit) => @unroll(@unfreeze(u));

Γ, f : @f(x : A) -> B, x : A |- B : Type
---------------------------------------
Γ |- @f(x : A) -> B : Type

Γ, f : @f(x : A) -> B, x : A |- M : B[x := @f(x : A) => M]
----------------------------------------------------------
Γ |- @f(x : A) => M : Type

Γ |- M : @f(x : A) -> B   Γ |- N : A
-----------------------------------
Γ |- M N : B[f := M][x := N]

Id = @id(A : Type) -> (x : )
Rec () = Rec () -> Type;

(f(x : A) => M) N ===
M[f := f(x : A) => M][x := ]

Γ, f : @f(x : A) -> B, x : A |- B : Type
---------------------------------------
Γ |- @f(x : A) -> B : Type

False@0 : Type@0
T_False : Type@ω = @self(False@n -> (f : @self(f -> @unroll False@n f)) -> Type@n);
False : T_False = @fix(False@n => f =>
  (P : @self(f -> @unroll False@n f) -> Type) => P f);

T_False (n : Level) : Type@(n + 1) =
  @self(False@n -> (f : @self(f@n -> @unroll False@n f@n)) -> Type@n);

False : T_False =
  @fix(False@1 => f => (P : @self(f@0 -> @unroll False@1 f@0) -> Type) -> P f);


@fix(Fix@1 -> @unroll Fix@1 -> ())

@fix(Fix@1 -> @unroll (@fix(Fix@0 -> @unroll Fix@0 -> ())) -> ())

Γ |- M : @self(x@n+1 -> T)
---------------------------------
Γ |- @unroll M : T[x := @upper M]


----------------------------------------------------------------
@unroll (@fix(x@n+1 => M)) === M[x := @upper (@fix(x@n+1 => M))]


@unroll (@fix(Fix@0 -> @unroll Fix@0 -> ())) -> ()


@unroll (@fix(Fix@1 -> @unroll Fix@1 -> ()))


@unroll (@fix(False@1 => @self(f@0 -> (P : @unroll False@1 -> Type@0) -> P f@0)))


T_False (l : Level) : Type (l + 2) =
  @self(False@l -> (f : @self(f@l -> @unroll False f)) -> Type (l + 1))

False l : T_False l =
  @fix(False@l => f => (P : @self(f@l -> @unroll False f) -> Type l) -> P f);



False l : Type (l + 1) = @self(f@l -> @unroll (False l) f);


@fix(False@1 => f => (P : @self(f@1 -> @unroll False f) -> Type 1) -> P f)

f : @self@1(f@l -> @unroll (False l) f)
@lower f : @self@0(f@l -> @unroll (False l) f)

@unroll f : (P : @self(f@0 -> @unroll False f) -> Type 0) -> P (@lower f)

@unroll f

False 0

@unroll (@fix(False@1 => f => (P : @self(f@0 -> @unroll False f) -> Type 0) -> P f));


f : @unroll False 1
f : (P : @unroll False 0 -> Type) -> P (@lower f);

f : @unroll (@fix(False@1 => @self(f@1 -> (P : @unroll False@1 -> Type@1) -> P f@1)))

f : @self(f@1 -> (P : @unroll (@fix(False@0 => @self(f@0 -> (P : @unroll False@0 -> Type@1) -> P f@0))) -> Type@1) -> P f@1)

@unroll f : (P : @self(f@0 -> (P : @unroll False@0 -> Type@1) -> P f@0) -> Type@1) -> P (@lower f)

@self(f@0 -> (P : @unroll False@1 -> Type@0) -> P f@0)

False@1 : @self(False@0 -> Type@0)

Fix : Type@2 = @unroll (@fix(Fix@1 -> @unroll Fix@1 -> ()))

fix : Fix = (f : @unroll (@fix(Fix@2 -> @unroll Fix@2 -> ()))) => f(f);

fix(fix)
@unroll False@1 : (f : @self(f@0 -> @unroll False@1 f@0)) -> Type@0




Γ |- M : @self(x@n+1 -> T)
--------------------------
Γ |- @unroll M : T[x := M]


@Unit () => @u(P : Unit () -> Type) -> P unit -> P u;

@fix(Unit). @self(u). (P : @unroll Unit -> Type) -> @expand P unit -> @expand P u

expected : @self(u). (P : @unroll Unit -> Type) -> P unit -> P u
received : @self(u). (P : @unroll Unit -> Type) -> P unit -> P u

(tag, payload : tag | true => String | false => Int)
tag | true => (payload : String)


ind (u : Unit) = @unroll u;

ind u

Γ |- A : Type
---------------------
Γ |- @frozen A : Type

Γ |- M : A
--------------------------
Γ |- @freeze M : @frozen A

Γ |- M : @frozen A
--------------------
Γ |- @unfreeze M : A

---------------------------
@unfreeze (@frozen M) === M
```

```rust
Car = interface(Car => {
  speed : (self : Car) -> Speed;
  top_speed : (self : Car) -> Speed;

  speed_is_always_smaller_than_top_speed :
    (self : Car) -> self.speed() < self.top_speed();
});

Bug = class(implements(Car), self => {
  speed = 10;
  top_speed = 120;
  speed_is_always_smaller_than_top_speed = _;
});

Car = @fix(Car => @self(this -> {
  speed : Speed;
  top_speed : Speed;

  speed_is_always_smaller_than_top_speed :
    this.speed < this.top_speed;
}));

Bug : Car = @fix(this => {
  speed = 10;
  top_speed = 120;
  speed_is_always_smaller_than_top_speed = _;
});


Electric_car = (top_speed, battery) => {
  ...Car(top_speed);
  battery_size : () => baterry;
};

bug = Car(120);
bug.top_speed()
```

```rust
TEq : Type -> Type -> Type;
trefl : (A : Type) -> TEq A A;

TEq A B = @self(teq ->
  (P : (C : Type) -> TEq A C -> Type) ->
  P A (trefl A) -> P B teq;
);
trefl A = P => x => x;



HEq : {A, B} -> A -> B -> Type;
hrefl : {A} -> (x : A) -> HEq x x;

HEq {A, B} x y = @self(heq ->
  (K : Type -> Type) ->
  (P : (T : Type) -> (z : T) -> HEq x z -> K T) ->
  P A x (hrefl x) -> P B y heq
);
hrefl {A} x = P => x => x;

uip_heq {A} (x : A) (heq : HEq x x) : HEq heq (hrefl x) =
  heq (T => z => heq => HEq heq (hrefl x)) (hrefl x);

heq (T => a => HEq a 1) a

heq_to_eq {A} (x : A) (y : A) (heq : HEq x y) : Eq x y =
  heq (T => z => _ => (x : T) -> Eq x z)

Eq : {A} -> A -> A -> Type;
refl : {A} -> (x : A) -> Eq x x;

Eq {A} x y = @self(eq ->
  (P : (T : Type) -> T -> (z : A) -> Eq x z -> Type) ->
  P (Eq x x) (refl x) x (refl x) -> P (Eq x y) eq y eq
);
refl {A} x = P => x => x;

uip_equal {A} (x : A) (eq : Eq x x) : Eq eq (refl x)
  = @unroll eq (_ => _ => eq_e => Eq eq_e (refl x))
      (@unroll eq
        (z => eq => _ =>
          Eq (@unroll eq _ (refl x)) (refl x))
        (refl x))
```

## Induction

```rust
Option (A : Type) =
  | Some (payload : A) : Option A
  | None : Option A;

Some : (A : Type) -> (payload : A) -> Option A;
None : (A : Type) -> Option A;

Ty (A : Type) =
  | Ty_string : Ty String
  | Ty_int : Ty Int;

One_or_zero (n : Nat) =
  | Zero : One_or_zero 0
  | One : One_or_zero 1;

f = (A : Type) => (ty : Ty A) => (x : A) : String =>
  ty
  | Ty_string => (x : String)
  | Ty_int => Int.to_string(x : Int);

id
  : (A : Type) -> (x : A) -> A
  = (A : Type) => (x : A) => x;

p : (x : Nat, x == 1)

fst p : Nat
snd p : (fst p) == 1

id = k => x => k x;

sum_3 = a => b => c => k => sum a b (a_b => sum a_b c k);
sum_3 = a => b => c => sum (sum a b) c;

Point : Type = sig
  val "x" Int
  val "y" Int
end;

zero_zero : Point = struct
  let "x" 0
  let "y" 0
end;

Point : Type = @sig({
  x : Int;
  y : Int;
});

Unit0 : @self(Unit0 -> _);
unit0 : @self(unit0 -> @unroll Unit0 unit0 _ _);

Unit1 = @unroll Unit0 unit0 _ _;
unit1 : Unit1 = unit0;

Unit = @frozen Unit1;
unit : Unit = @freeze unit1;

Unit : {
  @Unit : Type;
  unit : @Unit;

} = _;

Ind_type (T : Type) = (P : Type -> Type) -> P Type ->
    ((A : Type) -> (B : A -> Type) -> P ((x : A) -> B x)) -> P T;

Term (A : Type) =

Type : Type;

Type : @fix(Type => @self(T ->
  (P : Type -> Type) -> P Type ->
  P ((A : Type) -> (B : A -> Type) -> P ((x : A) -> B x)) -> P T
));

------
Type 0

---------------------
Type n : Type (n + 1)


Type : @fix(Type => @self(T -> (P : Type -> Type) -> P Type -> P T))



Type 1 : @self(T -> (P : Type 1 -> Type 1) -> P (Type 0) -> P T)

Type 1 : @unroll (Type 2) (X => )

----------------
Type 0 : Type 1

A : Type 0  B : Type 0
----------------------
(x : A)

Type 'n : Type ('n + 1);
(x : 'A : Type 'n) -> ('B x : Type 'n) : Type 'n;
(x : 'A) => (body : 'B x) : (x : 'A) -> 'B x;
(M : (x : 'A) -> 'B x) (N : 'A) : 'B N;

Type (n + 1) = @self(T -> (P : Type (n + 1) -> Type (n + 1)) ->
  P ((A : Type n) -> (B : Type n) -> P ((x : A) -> B x)) -> P T);
((x : 'A) -> 'B x) = P => forall => forall A B;
((x : 'A) => body) = (arg : 'A) => body[x := arg];
(lambda arg) = lambda arg;



Ind_lambda A B f = (P : ((x : A) -> B x) -> Type) ->
  ((x : A) -> (r : B x) -> P ((x : A) => R x)) -> P f;

Map_W A B w = (x : A) -> (y : A) -> (Iso x y) -> W x -> W y;
eq : (x : A) -> f x = g x;
w : W ((x : A) => f x);



fn = ((x : A) => f x);
ind fn (f => W f)
  (x => r => eq );

() -> IO (Option String);
() -> Option (IO String);

() -[Option | IO]> String;

Eff l = IEff (sort l) String;

() -> Eff [Option; IO] String;
() -> Eff [IO; Option] String;


List.map (x => external ("c_" ++ x)) ["hello"; "world"]

FFI = {
  hello : String = "hello";
};
[external "c_hello"; external "c_world"]


ind
```

## Linear Computer

### Machine

```rust
Nat64 : {
  dup : (src : &Nat64, to_ : &mut Nat64) -> ();
  // arith
  add : (a : &Nat64, b : &Nat64, to_ : &mut Nat64) -> ();
  sub : (a : &Nat64, b : &Nat64, to_ : &mut Nat64) -> ();
};
Mem : {
  // better API
  split : (mem : Mem, at : &Nat64) -[Invalid_offset]> (left : Mem, right : Mem);
  merge : (left : Mem, right : Mem) -[Invalid_right]> (mem : Mem);

  // TODO: is ptr needed?
  load : (mem : &Mem, ptr : &Nat64, dst : &mut Nat64) -> ();
  store : (mem : &mut Mem, ptr : &Nat64, src : &Nat64) -> ();
};

```

### System

```rust
File : Type;
open : (
  mem : &Mem,
  path : &Nat64,
  flags : &Nat64,
  ret : Reg
) -> (file : File);
write : (
  file : &mut File,
  mem : &mut Mem,
  buf : &Nat64,
  len : &Nat64,
  ret : Reg
) -> Nat64;

Table : Type;
load : (table : &mut Table, fd : Nat64) -> (file : File);
store : (table : &mut Table, file : File) -> (fd : Nat64);
```

## Linearity

```rust
F A = (G A, G A);

(x : Int) -> Eff([Fix], Int)
```

## JavaScript

```typescript
@fix((x) : Int => @unroll x);


type Ret<A> =
  // TODO: unbox this, typeof or instanceof
  | { tag : "jmp"; to_ : () => Ret<A>; }
  | { tag : "ret"; value : A; };
type Fn<A, B> = (x : A) => Ret<B>;

function $call (f) {
  const stack = [];
}

function $fix (f) {
  return f(() => $fix(f));
};

$call
$jmp
$fix(x => y => x());

k => x => y => k y
```

## Beta

```rust
true = x => y => x;

(x => y => x)(1)
(y => x)[x := 1]
(y => 1)

([], x => y => x)


true(1) -> ([1], x => y => x)
true(1)(2) -> ([1; 2], x => y => x)

true(1) -> (1, true)
true(1)(2) -> (1, 2, true) -> 1
```

## Bound Escape

```rust
received : _B/+2 -> (A : Type) -> A/-1
expected : _B/+2 -> (A : Type) -> _B/+2

A/+3 `unify` _B/+2


f = x => (y => y)(x);
g = x => x;

[],((λ (λ (λ ((#3 #2) s)))) (λ #1)) === [],((λ (λ (λ ((#3 #1) t)))) (λ #1))
[(λ #1)],(λ (λ ((#3 #2) s))) === [(λ #1)],(λ (λ ((#3 #1) t)))
[(λ #1),Abs],(λ ((#3 #2) s)) === [(λ #1),Abs],(λ ((#3 #1) t))
[(λ #1),Abs,Abs],((#3 #2) s) === [(λ #1),Abs,Abs],((#3 #1) t)
[(λ #1),Abs,Abs],(#2 s) === [(λ #1),Abs,Abs],(#1 t)

[Type],(A : Type) => (A/+1)[+1 := -1]
[Type],0,(B : Type) => (A : Type) => (A/+1)[+1 := -1]
  [Type;B],0,(A : Type) => (A/+1)[+1 := -1]
  [Type;B;A],0,(A/+1)[+1 := -1]
  [Type;B;A;-1],+2,A/+1

[Type;-1],A/+1 -> A/-1

(A : Type) => A/-1
_B => _C

[Type; ]

[], (x => x)(y => y)(z)
  [], (x => x)(y => y)
  [x := y => y], x
  [x := y => y], y => y
  [], y => y
  [], (y => y)(z)
[], y

(x => x)(y => y)(z)
(x[x := y => y])(z)
(y => y)(z)
(y[y := z])
z

T[x := y; y := z]
z
(x => x)(y => y)

(x => x(x))(z => z)

x(x)[x := z => z]
(x[x := z => z])(x[x := z => z])
(z => z)(z => z)

_A[x :=]
[z => z],\0(\0)

_A[x := y] `unify` y
_A `unify` y[y := x]
_A `unify` x

_A := x


x[x := y] `unify` y

[],_A[x := z] `unify` [],y[y := z]
[x := z],_A `unify` [y := z],y
_A := y[y := x][x := z]
_A := z


[Type],(A : Type) => (A/+1)[+1 := -1]
[Type],(B : Type) => (A : Type) => (A/+1)[+1 := /-1]
    [Type;B],(A : Type) => (A/+1)[+1 := -1]
    [Type;B;A],(A/+1)[+1 := -1]
    [Type;B;A;-1],+2,A/+1

(A : Type) => (((B : Type) => B/+3[+3 := -1]) => A/+2)[+2 := -1]
(A : Type) => (((B : Type) => B/+1[+1 := -1]) => A/+2)[+2 := -1]

[]


fold : (b : Bool) -> (A : Type) -> A -> A -> A;
Bool = (A : Type) -> A -> A -> A;

ind : (b : Bool) -> (P : Bool -> Type) -> P true -> P false -> P b;
Bool = b @-> (P : Bool -> Type) -> P true -> P false -> P b;

@Unit (@I : unit @-> _)
  (@unit : (@C0 : _) -> @Unit I unit C0) (@C0 : _) : Type =
  u @-> (P : @Unit I unit C0 -> Type) -> P (@unit C0) -> @I unit C0 P u;
@I unit C0 P x = x;
@unit C0 : @Unit I unit C0 = u @=> P => x => x;
@C0 P x = x;

expected : u
received : u @=> P => x => @C0 P x

@Unit
  (@unit : u @-> (C0 : _) => @Unit unit C0 u) C0
  (@u : @Unit unit C0 u) : Type =
  (P : (@u : @Unit unit C0 u) -> Type) -> P (@unit C0) -> P u;

@unit C0 : @u -> @Unit unit C0 u =
  u @=> P => x => @C0 P x;

@Unit (@I : _) : Type =
  u @-> (P : @Unit I -> Type) ->
  P (@I u) -> P (@I (u @=> P => x => x));

expected : P (@I (u @=> P => x => x))
received : P (@I u)

@Unit (@unit : _) (@C0 : _) (@u : @Unit u) : Type =
  (P : (@u : @Unit unit C0 u) -> Type) ->
  P u -> P (@unit C0);

T_unit = @Unit
@unit C0 : u @-> @Unit unit C0 u
  = u @=> P => x => @C0
@Unit
  (@I : _)
  (@unit : (C0 : _) -> @Unit I unit C0)
  (@C0 : _)
: Type =
  u @-> (P : @Unit I unit C0 -> Type) ->
  P (@I u) -> P (@unit C0);
I = _;

expected : @I P (u @=> P => (x : @I P u) => x)
received : @I P u


I : (u @-> (P : @Unit I -> Type) ->
  @I P (u @=> P => x => x) -> @I P u)
@Unit (@I : _) : Type =
  u @-> (P : @Unit I -> Type) ->
  P (@I (u @=> P => x => x)) -> P (@I u);

@Unit (@I : _) : Type =
  u @-> (P : @Unit I -> Type) ->
  P (@I (u @=> P => x => x)) -> P (@I u);

(x : A) => x
(x : B) => x



@Bool (@I : _) : Type =
  b @-> (P : @Bool I -> Type) ->
  @I P (b @=> P => x => y => x) ->
  @I P (b @=> P => x => y => y) ->
  @I P b;

expected : P (@I (u @=> P => (x : P (@I u)) => x))
received : P (@I (u @=> P => (x : P (@I u)) => x))
@Unit
  (@I : (u : _) -> @Unit I u)
  (@u : @Unit I u)
: Type = (P : (@u : @Unit I u) -> Type) ->
  P unit ->
  P u;

expected : @I P (u @=> P => (x : @I P u) => x)
received : @I P u

Γ, x : x @-> T |- M : T
----------------------- // fix
Γ |- (@x : T) @=> M : T


|>
.

f . g
\x -> f (g x)

x => f(g(x))


add_incr = (x) => (
  x = 1;
  incr = y => x + y;

  x = x + 3;
  incr(1)
);

id = (x : _A) => (x : _A);
id = {A} => (x : A) => (x : A)


generic = var.level > 1e6
generic = var.level < level

id = (x : _A\1) => (x : _A\1);

id : (x : 'A\1) => (x : 'A\1)

id : (x : _B\0) => (x : _B\0)
y = id 1;
z = id "a";

id = (x : _A\1) => (x : _A\1);

id : (x : _B\0) -> _B\0
id 1
id "a"


id = (A : Type) => (x : A) => x;

x = id _ 1;


console.log{Int}(123);


@Unit (@I :
  _
) : Type =
  u @->
    (P : @Unit I -> Type) ->
    P (@I ((@u : ) @=> P => x => x)) ->
    P (@I u);


expected : P (@I (u @=> P => x => x)) -> P (@I (u @=> P => x => x))
received : P (@I (u @=> P => x => x)) -> P (@I (u @=> P => x => x))
x => x : P (@I u) -> P (@I u)

Γ, x : x @-> T |- T : Type
-------------------------- // self
Γ |- x @-> T : Type





Γ, x : x @-> T |- M : T
----------------------------- // fix
Γ |- (@x : T) @=> M : x @-> T

x @=> (x : T)
x @-> T


Γ, x : x @-> T |- T : Type
-------------------------- // self
Γ |- x @-> T : Type

Γ, False : Self |-

Γ |- M : x @-> T
------------------- // unroll
Γ |- @M : T[x := M]


False @->
  (f : f @-> @False f) -> Type;


(f0 : (A : Type) -> A) =>
(f1 : (P : False0 -> Type) -> P f0) =>


@x : (P : T -> Type) -> P x;

u @->
  (P : Int -> Type) -> P (u P 1) -> Int;

Γ, x : x @-> T |- T : Type
-------------------------- // self
Γ |- x @-> T : Type

False @-> (f : f @-> @False f) -> Type;


False @-> (f : f @-> @False f) -> Type;

@self(False, f). (P : False -> Type) -> P f;

Unit I = _;

@self(Unit, u). (P : Unit -> Type) -> P unit -> P u;

(@Unit : Type) @-> Type;
(@Unit : Type) @-> () -> Type;

(@False : Type) @-> () -> Type;

(@Unit : Type) @=>
  (P : Unit -> Type) -> P (@unit @=> P => x => x) -> P u;

Γ, x : T |- T : Type
-------------------------- // self
Γ |- @x @-> T : Type

(@False) @-> (f : @f @-> False (@f1 : False f1) @=> f);

f : False ((@f1 : False f1) @=> f)

(@f1 : False f1) @=> f


Γ, x : A |- B : Type
-------------------------- // self
Γ |- (@x : A) @-> B : Type

Γ, x : T |- M : T
----------------------------- // fix
Γ |- (@x : T) @=> M : x @-> T

(Unit : Type) @-> Type;


(@False : Type) @=> (f : False) @->
  (P : False -> Type) -> P f;
(@Unit : Type) @=> (u : Unit) @->
  (P : Unit -> Type) -> P (@unit @=> P => x => x) -> P u;


(@False : (f : (f : _) @-> False) @-> Type) @->
  (f : (f : _) @-> False) @-> Type

(@False : ) @->
  (f : (@f : False ((@f1 : False f1) @=> f)) @-> )
(@False : Type) @=> (f : _) =>
  (P : ((@f : False f) @-> False f) -> Type) -> P f;


False @-> (f : f @-> False f) -> Type;

(@T_False : Type) @=>
  (I : @((@IT : Type) @=> T_False ->
    (I : IT) -> (False : T_False) @-> Type)) ->
  (False : T_False) @-> Type;

(False : ?) @->
  (I : @((@IT : Type) @=> False ==
    (I : IT) f @-> (P : False I -> Type) -> I P f)) -> Type;

(@False : Type) @=> (f : _) @>
  (P : ((@f : False f) @-> False f) -> Type) -> P f;

(@M : Type) @=>
  (I : @((@IT : Type) @=> M == (I : IT) -> Type)) -> Type;

@False @=> I => f @-> (P : False I -> Type) -> I P f;

(A, False : A) @=>
  _

(@False : (
  @T_f : Type = (@f : T_f) @-> False f;
  (f : ) -> Type
)) @=> (
  @T_f : Type = (@f : T_f) @-> False f;
  (f : (@f : T_f) @-> False f) -> Type
);

(@M : Type) @=>
  (I : @((@IT : Type) @=> M == (I : IT) -> Type)) -> Type;

(@M : Type) @=>
  (I : @((@IT : Type) @=> M == (I : IT) -> Type)) ->
  (x : M) @->
  (eq : @((@eqT : Type) @=>
    (eq : eqT) -> I _ x I eq == 1)) ->
  Int;

@T_False : Type =
  (False : T_False) @->
  (I : @((@IT : Type) @=> False == )) -> Type;
(False : T_False) @=> I =>
  (f : @False I) @-> (P : @False I -> Type) -> I _ P f;

@T_I (M : Type) : Type = M == (I : T_I M) -> Type;
@M : Type = (I : T_I M) -> Type;

@T_I (T_False : Type) : Type =
  T_False == _;
@T_False : Type =
  (I0 : T_I0 T_False) ->
  (False : T_False) @->
    (I1 : T_I1 T_False False) ->
    (f : f @-> @I1 False I1 f)
     Type;

(@False : T_False)
@False : T_False = I => (
  f @-> (P : False I -> Type) -> P f
);

@Unit (
  @I : (
    u @-> (P : Unit I -> Type) -> P (I (u @=> P => x => x)) -> P (I u)
  ) -> Unit I
) = u @-> (P : Unit I -> Type) -> P (I (u @=> P => x => x)) -> P (I u);

Unit = Unit (@I @=> x => x);
unit : Unit = u @=> P => x => x;

@Unit I =
  u @-> (P : Unit I -> Type) -> P (I (u @=> P => x => x)) -> P (I u);


read : (world : World, file : String) -> (world : World, content : String);
print : (world : World, message : String) -> World;
main : World -> World;

read : (world : &mut World, file : String) -> String;
print : (world : &mut World, message : String) -> ();


main world = (
  (world, hello) = read(world, "hello.txt");
  world = print(hello);
  (world, bronha) = read(world, "bronha.txt");
  print(world, bronha);
);

main world = (
  hello = read(world, "hello.txt");
  () = print(hello);
  bronha = read(world, "bronha.txt");
  print(bronha);
);


IO X : World -> (World, content : X)

main () = (
  hello = read(world, "hello.txt");
  () = print(hello);
  bronha = read(world, "bronha.txt");
  () = print(bronha);
  ()
);

main : IO ();
```

## Grade

```rust

Γ |- A : Type  Γ |- n : Grade
-----------------------------
Γ |- M : A $ n

Γ, x : A $ n |- B : Type $ 0
------------------------------------
Γ |- ((x : A $ n) -> B $ m) : Type $ 0

Γ, x : A $ n |- M : B $ m
--------------------------------------------------
Γ |- (x : A $ n) => M : ((x : A $ n) -> B $ m) $ k

Γ |- M : ((x : A $ m) -> B $ n) $ 1  Γ |- N : $ n
--------------------------------------------------
Γ |- M N : ((x : A $ n) -> B $ m) $ k



f = x => x + x;
f = (x : Int) => x + x;

Γ |- M : A
Γ |- M : A $ n

f : (!x : Nat, !y : Nat, ?z : Nat = 1) -> Nat;

f (1, 2, 3);

1 ^ 1

(+) : (x : Nat, y : Nat) -> Nat &
(+) : (x : String, y : String) -> String;


S S S Z + S S Z = S S S S S Z
"abc" + "de" = "abcde"

(+) : (x : Nat, y : Nat) -> Nat &
(+) : (x : String, y : String) -> String;

(+) : ()

Linear HM == Mono Unification
Linear System F == Mono Unification
Multiplicative System F == Mono Unification + Intersection


(id : _A & _B $ 2) => ((id : _A) 1, (id : _B) "a")

(id : Int -> _A & String -> _B $ 2) => ((id : _A) 1, (id : _B) "a")

Nat<G> = (A : Type) -> (z : A $ 1) ->
  (s : (G : Grade) -> (A -> A) $ G) -> A;

(G : Grade) => (x $ G) => _;

(id : _A & _B $ 2) => (id 1, id "a")


Γ |- A : Type
-------------
Γ |- M : A

Γ |- A : Type $ 0  Γ |- G : Grade $ 0
-------------------------------------
Γ |- M : A $ G



Γ, x : A |- B : Type
------------------------
Γ |- (x : A) -> B : Type

(A : Type $ 1) => (x : A) => (
  (A) = A;
  x
);

(A : Type) => (x : A) => x;


Bool = (A : Type) -> A -> A -> (A, Garbage);
Bool = (A : Type) -> A -> A -> Weak A;

A ->
(A : Type $ 2) => (x : A) => ((x : A) => x) x;

((x : A) => M) N ==
  () <- weak A;
  M[x := N]

Γ |- M : (x : A) -> B ! Δ1  Γ |- N : A ! Δ2
-------------------------------------------
Γ |- M N : B[x := N] ! Δ1, Δ2, Weak

(x : Nat $ 2) => x + x;
(x0 : Nat) => (x1 : Nat) => (eq : x0 == x1) => x0 + x1;


id
  : (A : Type $ 2) -> (x : A) -> A;
  = (A : Type $ 2) => (x : A) => x;

(A : Type) -> (B : Type) ->
(eq : (P : Type -> Type) -> P A -> P B) ->
(x : A) -> B

Pair (A : Type) (B : Type) =
  (K : Type) -> (k : ((x : A) -> (y : B) -> K)) -> K;

Unit = (A : Type) -> ()
Exist = ((A : Type) -> (x : A) -> Unit) -> Unit
Weak (A : Type) =
  (G : Type, x : Exist)

(A : Type $ 2) => (x : A) => ((x : A) => x) x;
(A : Type $ 2) => (x : A) => (() = weak A; x);

(s : Socket $ 2) => (

)




add : (n : Nat) -> (m : Nat) -> Nat

ind : (b : Bool) -> (P : Bool -> Type) -> P true -> P false -> P b;
ind : (b : Bool (n + 1)) ->
  (P : Bool n -> Type) -> P true -> P false -> P (lower b);

(x : Nat) -> (y : Nat) -> x *
(x : A $ 1) -> B ! Δ $ n

(x : A $ 2) -> B $ 2

(x : A $ 1) -(2)> B

(x : A $ 1) -[IO $ 2]> B
(x : A $ 1) -()> B

Γ |- M : P


(x : Int $ 2) => x + x;


Ref : {
  @Ref (A : Type) : Type$;
  make : {A} -> (value : A) -> Ref A;
  get : {A} -> (cell : Ref A) -> (cell : Ref A, value : A);
  set : {A} -> (cell : Ref A, value : A) -> (cell : Ref A);
} = _;

x = cell = (
  cell = Ref.set(cell, 1);
  (cell, value) = Ref.get(cell);
  cell
);

main : World$ -> World$;

read : (world : World$, file : String) -> (world : World$, content : String);
print : (world : World$, message : String) -> World$;

main : IO ()

main world = (
  (world, hello) = read(world, "hello.txt");
  world = print(world, hello);
  (world, bronha) = read(world, "bronha.txt");
  print(world, bronha);
);



map : {A B} -> (l : List A, f : (el : A) -> B) -> B;

map : {A B} -> (l : List {G} A $ 1, f : ((el : A $ 1) -> B $ 1) $ G) -> B $ 1;

map : {A B G} -> (l : List G A $ 1, f : ((el : A $ 1) -> B $ 1) $ G) -> B $ 1;
add : (l : List 1 A, a : Nat $ 1) = (l, a) =>
  map(l, (el => el + a) $ 1);


f = (a : Nat $ 0) => 1;

List L A = {K} -> (initial : K $ 1, fold : (acc : K, el : A) -> K $ L) -> K;

Type : Type

id : A. A -> A = x => x;
id : (A : Type) -> A -> A =
  (A : Type) => x => x;


forall A. A -> A
forall A -> A -> A

id : forall A. A -> A = A => (x : A) => x;

id : {A} -> (x : A) -> A;
id {A} x = x;

id : ?A -> (x : A) -> A;
id ?A x = x;

User : (User : Type & {
  make : (name : String) -> User;
}) = _;
User : Type & {
  make : (name : String) -> User;
} = _;
User : {
  @User : Type;
  make : (name : String) -> @User;
} = _;

id = (!x : Nat) => x;
x = id(x = 1);

f = (x : User) => x;

add (1, 2)
?add (1, _)

add 1 2
?add 1 _

pair _A 1 _A 2

<A> (x : A) -> A
<A> (x : A) => x;
<A> -> (x : A) -> A
<A> => (x : A) => A
id <A>

{A} (x : A) -> A;
{A} (x : A) => x;
{A} -> (x : A) -> A;
{A} => (x : A) => x;
id {A}

[A] (x : A) -> A;
[A] (x : A) => A;
[A] -> (x : A) -> A;
[A] => (x : A) => x;
id [A]

(?A) -> (x : A) -> A;
(?A) => (x : A) => x;

id ?A

-(IO)>
-[IO]>

`
~


id <A> x = x;

(<A : Int>, x : Int)
pair : <A>(x : A, y : A) -> Pair A B;

(==) : <A>(x : A, y : A) -> Bool;

(==) : <A>(x : A, y : A) -> Type &
(==) : <A : Eq>(x : A, y : A) -> Option (x == y);

x => y => (x == y : _A where _A == Type or _A == Option (x == y))

id (A : Type) (x : A) = x;


%macro (A : Type $ 1) =

x => %printf (x : _A)
x => _B x

when _A to Int == _B := Int.printf

x => Int.printf x

extract "/users/:id" : ("id", _ : _A)

"a" : Type | String

((x : "a") => _) ("a")

Either(A, B) =
  | ("left", A)
  | ("right", B);

Either(A, B) =
  | Left(A)
  | Right(B);


Either(A, B) =
  | (tag : "left", A)
  | (tag : "right", B);


x
| ("left", x) => _
| ("right", y) => _

(>) : <A>(x : A, y : A) -> Type &
(>) : <A : Cmp>(x : A, y : A) -> Option (x > y);

1 == 2 : Option (1 == 2)

(==) : <A>(x : A, y : A) -> Type
f = (x : 1 == 1) => x;

Eq A = {
  eq : (x : A, y : A) -> (x == y | x != y);
}
x => y => eq x y

f (eq x 1 | Some eq => eq);

pair <Int> (1, 2)

[%mode dynamic]
f = x => x;

x : Dyn = 1;

Int.add : (x : Int, y : Int) -> Int

Int.add (assert.Int x) : 1 ! Assert $ n

| "int" => (x : Int)




useless : (<A : Type>, x : A) -> (A : Type, x : A)
useless(1)


id ~x = 1

\(A)/ ->

id::[1]

id : <A>(x : A) -> A;
id <A> x = x;

id <A> x = x;

id ?Int 1;

{ x; y; } = { x = 1; y = 2; };


Id : Type = (A : Type) => A;

f (x : Int & x > 0) = (
  1
);

@id {Int} 1 = 1
id : forall a. a -> a
id (x : a) = x


f (id : Int -> 'A & String -> 'B $ 2) = (id 1, id "a")

f {G} l = _


f {1} [1] = _
add = a =>
  map(l, el => el + a);

map : {A B} -> (l : List A, f : ({G} -> ((el : A) -> B) $ G) $ 1) -> B;

map : {A B} -> (l : List A, f : (el : A) -> B) -> B;

id = (A : Type $ 0) => (x : A $ G) => x;

double = (x : Nat $ 2) => x + x;

id = (A : Type) =>
```

## Linearity saves the day

```rust
Fix = (x $ 2 : Fix) -> ();
((x $ 4 : Fix) => x x);

case : (b : Bool) -> (A : Type) -> A -> A -> A;
Bool = (A : Type) -> A -> A -> A;

ind : (b : Bool) -> (P : Bool -> Type) -> P true -> P false -> P b;
Bool = (P : Bool -> Type) -> P true -> P false -> P b;

Bool : Type
true : Bool
false : Bool

Bool = b @-> (P : Bool -> Type) -> P true -> P false -> P b;
true = (P : Bool -> Type) => (x : P true) => (y : P false) => x;


(x : $Socket)

(x $ 4 : Nat) =(2)> (x + x)


() -> @fix(S). S

T =
  @fix(S). (f : (A : Nat) -> Type $ 0) -> f 1;


Type : Type

(G : Grade) => (x $ G) =>

Γ | Δ, x : A |- M : B
------------------------------------
Γ | Δ |- (x : A) => M : (x : A) -> B


(A : Socket $ 2) =>
(A $ n) $ m
Socket = _ $ 1;
(x : Socket $ 2) => x;

Array : {
  Write : {
    @Write (A : Type) : Type;
  };
  Read : {
    @Read (A : Type) (x : Write A) : Type;
    split : <A>(x : Write A) -> (l : Read A x, r : Read A x);
    merge : <A>(x : Write A $ 0, l : Read A x, r : Read A x) -> (x : Write A);
  };
};


Γ |- A : Type   Γ |- n : Grade
------------------------------
Γ |- A $ n : Type

Γ |- M : A  M == N
------------------
Γ |- M + N : A $ 2



---------------
Γ |- A $ 1 == A

(x : _A $ _G) => x;
(x : _A $ _G) => x + x;
```

```rust
1 :: id ::

t :: id |- \0 -> t

x[open := t]

_A[open x/+2] `unify` x\+2

_A := x/+2[close +2]
_A := x/-1

x/+2[close +2][open x/+2]

x/+2[+2 := -1][-1 := x/+2]

(_ => x/-2)[open x/+2]
x/-2[Lift][open x/+2]

[open x/+2; id] |- _A `unify` [Free; Free]

A/-1 -> A/-2
(A/+2 -> A/+2)[close +2]

A/+2[close +2] ~~> A/-1
A/+2[id][close +2] ~~> A/-2

A => B => _C `unify` A => B => A/+2[id][close +2]

{} [id; id;] |- _C `unify` [id; id; close +2; id;] |- A/+2


A => (B => A/+2)[close +2]
A => B => A/+2[id]
_C := A/+2[id][close +2][id][id][-id][-id]
{} - [id;] |- A/+2[id][close +2]
{ +2 = 0; } - [id;] |- A/+2[id]
{ +2 = 0; } - [id; id;] |- A/+2 ~~> A/-2

A => A/-1

A => A/+2[close +2]

{} - [] |- A/+2[close +2]
{ +2 = 0; } - [] |- A/+2 ~~> A/0
{ +2 = 0; } - [id] |- A/+2

A/-2[id][open x/+2] ~~> A/+2

A/+2[close +2][open +2] ~~> A/+2

A/+2[id][close +2] ~~> A/-2
A/+2[id][close +2][id][open +2] ~~> A/+2
A/+2 ~~> A/-2 ~~> A/
[Free, Free] |- A/+1

x @
| a
| b


Free = +
Bound = -

+A
A => -A

λ.λ.\1

(λ.λ.\1) "a" "b"

"b" :: "a" :: [] |- \1 ~> "a"

codemod that tags every occurencce of identifier so that the dev needs to go and check the code, removing the annotation

Type\1
String\2
A\3

Sorry\2
@fix(False). (f : @self(f -> @unroll False f)) =>
  (P : (f : @self(f -> @unroll False f)) -> Type) -> P f

False : @self. _A[close +3]
f : @self. _B[close +4]

@unroll \+3 : _A
_A := (f : @self. _B[close +4]) -> Type
@unroll \+3 \+4 : Type
_B := @unroll \+3 \+4

False : @self. (f : @self. @unroll \-1 \-0) -> Type
f : @self. _C[close +5]

@unroll \+3 : (f : @self. @unroll \+3 \-0) -> Type
@unroll \+3 \+5 : Type

expected : @self. @unroll \+3 \-0
received : @self. _C[close +5]

@self. @unroll \+3 \-0 `unify` @self. _C[close +5]
@self. @unroll \+3 \-0 `unify` @self. _C[close +5]

(False : @self. (f : @self. @unroll False f) -> Type) =>
  (P : (f : @self(f -> @unroll False f)) -> Type) -> P f

(False : @self(False -> (f : @self. @unroll False f) -> Type)) =>
  (P : (f : @self(f -> @unroll False f)) -> Type) -> P f

False : @self. (f : @self. @unroll \-1 \-0) -> Type
f : _A

@unroll \+3 : (f : @self. @unroll \+3 \-0) -> Type
@unroll \+3 \+5 : Type

@self(False). (f : @self(f). @unroll False f) -> Type

False : @self. _A[close +3]
f : @self. _B[close +4]

@unroll \+3 : _A
_A := (f : @self. _B[close +4]) -> Type
@unroll \+3 \+4 : Type

_B := @self. @unroll \+3 \-0

@self. (f : @self. @unroll \+3 \-0) -> Type[close +4]


False : @self. (f : @self. @unroll \-1 \-0) -> Type
f : @self. _B
@self((f : @self(f -> _B)) -> @unroll \+3 \-0)

f : @self. _A[close +5]

@unroll \+3 : (f : @self. @unroll \+3 \-0) -> Type
@unroll \+3 \+5 : Type

@self. @unroll \+3 \-0 `unify` @self. _A[close +5]
(@unroll \+3 \-0)[open \+6] `unify` (_A)[close +5][open \+6]
(@unroll \+3 \-0) `unify` (_A)[close +5]
(@unroll \+3 \-0) `unify` (_A)[open \+5]

@self(False -> (f : @self. @unroll False f) -> Type)

False : _A
f : _B

@unroll \+3


@self. _C[close +3] `unify` @self. _A

False : _A

(False : @self(False -> (f : @self. @unroll False f) -> Type)) =>
  (P : (f : @self(f -> @unroll False f)) -> Type) -> P f

False : @self. (f : @self. @unroll \-1 \-0) -> Type)
f : @self. _B[close +5]


T[subst] === U[subst]
T === U

_ -> _A -> _A === _ -> \-0 -> \-1
(_A -> _A)[subst] === (\-0 -> \-1)[subst]
_A\+3 -> _A\+3 === \-0 -> \-1

_A\+3 := \-0


```

## Unification

```rust
( + ) : Int -> Int -> Int;

x => x + 1

Γ, x : _A |- x + 1 : Int

_A := Int

incr : Int -> Int = x => x + 1;

map(x => x + 1)
(x : Int) => x + 1

(x => x) : 'a. 'a -> 'a
Γ, x : _A |- x : _A

id
  : (A : Type) -> (x : A) -> A
  = (A : Type) => (x : A) => x;

id(String) : ((x : A) -> A)[A := String]

f = (id : (A : Type) -> A -> A) => (id(1), id("a"))

3 [] {} |- \+3[close +3][open \+3]
4 [\+3 - 4] {} |- \+3[close +3]
5 [\+3 - 4] { +3 := -0 - 5 } |- \+3 ~> \-0
4 [\+3 - 4] { +3 := -0 - 5 } |- \+3 ~> \-0 ~> \+3

\+3[close +3][open \+3]
\+3[close +3][open \+3]

5 [\+3 - 4] { +3 := [\-0 - 3; -0 - 5] } |- \-0
4 [\+3 - 4] { +3 := [\-0 - 3; -0 - 5] } |- \-0 ~> \+3
3 [\+3 - 4] { +3 := [\-0 - 3; -0 - 5] } |- \-0 ~> \+3 ~> \-0

\+3[close +3][open \+3]
\-1[open \+3][close +3][open \+3]

Γ, open \+3, close +3, open \+3, close +4 |- \-0
Γ, open \+3, close +3, open \+3 |- \-0
Γ, open \+3, close +3 |- \+3
Γ, open \+3 |- \-0
Γ |- \+3

[] |- \+3[close +3]

[Type] |- (A : Type) -> ((x : A\+2) -> A\+2)[close]
[Type] |- (A : Type) -> ((x : A\+2) -> A\+2)[close]


\+3[close +3][open \+3]

expected : (A : Type) -> A -> A
received : (B : Type) -> B -> B

expected : (A : Type) -> \-1 -> \-2
received : (B : Type) -> \-1 -> \-2

(A : Type\+0) -> \+1 -> \+1
(A : Type) => (x : \-1 -> \-2) => (x : \-2 -> \-3)


(A : Type\+0) -> \-1
(A : Type) -> (\+1 -> \+1)[close]

(A : Type\+0) -> \-1 -> \-2
(B : Type\+0) -> _A -> _A

A := \-1

(A : Type\+0) -> \-1 -> \-2
(B : Type\+0) -> \-1 -> \-1


(\-1 -> \-2)[open +4]

\+4 -> \+4
_A -> _A

\+3[close +3][open \+3]

close +3 open \+3 |- \+3


\+3[close +3][open \+3]

[\+3] { +3 := -0 } |- \-0 ~> \+3 ~> -0

(\+3 -> \+3)[close +3][open \+3]
\+3 -> \+3[close +3][open \+3]

@self. @unroll \+3 \-0 `unify` @self. _A[close +5]
(@unroll \+3 \-0)[open \+6] `unify` _A[close +5][open \+6]
(@unroll \+3 \-0) `unify` _A[close +5]
(@unroll \+3 \-0)[open \+5] `unify` _A

|- @self. @unroll \+3 \-0 `unify`
|- @self. _A[close +5]

[] |- @self. @unroll \+3 \-0
[close +5; ] |- @self. _A

[open \+6; ] |- @unroll \+3 \-0
[close +5; open \+6; ] |- _A

_A := @unroll \+3 \-0[open \+5]

x = 1;
eq : x + 1 == 2 = refl;

x == 1;


f : (A : Type) -> (\+1 -> \+1)[close ] = _;
f

[Type] |- (A : Type) -> ((x : A\+1) -> A\+1)[close]

\+3[open \+3][close +3]

[] {} |- \+3[open \+3][close +3]
[] { +3 := -1 } |- \+3[open \+3]

{} |- \+3[close +3][open \+3]
{} |- \+3[\+3 := \-1][\-1 := \+3]
{ \-1 := \+3; } |- \+3[\+3 := \-1]
{ \-1 := \+3; \+3 := \-1; } |- \+3
{ \-1 := \+3; } |- \-1
{ } |- \+3

{} |- \+3[close +3][open \+3]
{} |- \+3[\+3 := \-1][\-1 := \+3]
{ \-1 := \+3; } |- \+3[\+3 := \-1]
{ \-1 := \+3; \+3 := \-1; } |- \+3
{ \-1 := \+3; \+3 := \+3; } |- \+3


{} |- \+3[close +3][open \+3 -> \+3]
{} |- \+3[\+3 := \-1][\-1 := \+3 -> \+3]
{ \-1 := \+3 -> \+3; } |- \+3[\+3 := \-1]
{ \-1 := \+3 -> \+3; \+3 := \-1; } |- \+3
{ \-1 := \+3 -> \+3; \+3 := \+3 -> \+3; } |- \+3

{} |- \+3[\+3 := \+3 -> \+3][\-1 := \+3 -> \+3]

{} |- \+3[\+3 := \-1][\-1 := \+3 -> \+3]

\+3[\+3 := \+3][\-1 := \+3]

\-1[open \+3][close +3][open \+3]

5 [\+3 - 4] { +3 := [\-0 - 3; -0 - 5] } |- \-1
4 [\+3 - 4] { +3 := [\-0 - 3; -0 - 5] } |- \-1 ~> \+3
3 [\+3 - 4] { +3 := [\-0 - 3; -0 - 5] } |- \-1 ~> \+3 ~> \-1

+1 |- (A : Type) -> \+1 -> \+1
+4 |- (A : Type) -> \+4 -> \+4

(A : Type) -> (\+1 -> \+1)[close +3]
(\-1 -> \-2)[open +3]


f : (Int\-1 -> Int\-2)[open Int\+3]
x : Int\-2[id][open Int\+3]

expected : Int\-1[open Int\+3]
received : Int\-1[id][open Int\+3]

2 [] |- (A : Type) -> A\-1 -> A\-2
2 [] |- (A : Type) -> _B -> _B

2 [] |- (A : Type) -> A\-1 -> A\-2
2 [] |- (A : Type) -> _B\+3 -> _B\+3

3 [open A\3; ] |- A\-1 -> A\-2
3 [open A\3; ] |- _B\+3 -> _B\+3

_B := A\-1
3 [A\3;] |- A\-1 -> A\-2
3 [A\3; A\3;] |- _B -> _B

3 [A\3;] |- A\-1
3 [A\3; A\3;] |- _B

_B := A\-1[close ]
2 [] |- (A : Type) -> _B[close] -> _B[shift][close]

3 [] |- (A\-1 -> A\-2)[open A\+3]
3 [] |- (_B[close] -> _B[shift][close])[open A\+3]

3 [open A\+3] |- A\-1 -> A\-2
3 [open A\+3] |- _B[close] -> _B[shift][close]

3 [open A\+3] |- A\-1
3 [open A\+3] |- _B[close]

3 [open A\+3] |- A\-1
3 [open A\+3; close] |- _B[]

3 [] |- (A\-1 -> A\-2)[open A\+3]
3 [] |- (_B -> _B)[close][open A\+3]

[] |- (A\-1 -> A\-2)[open A\+3]
[] |- (_B -> _B)[open A\+3]

[A\+3] |- A\-1 -> A\-2
[A\+3] |- _B -> _B

_B := A\-1[A\+3]![A\+3]

x => id(x)
x => x

[Int : Type] |- (x : Int\1 = _; x\1 : (Int\1)[lift])
[Int : Type; x : Int\1] |- (x\1 : (Int\2)[lift])

[] |- x\-1[y\-2][id][x\-1]
[x\-1] |- x\-1[y\-2][id]
[x\-1; id] |- x\-1[y\-2]
[x\-1; id; y\-2] |- x\-1
[x\-1; id] |- y\-2
[x\-1] |- y\-1
[] |- x\-1

1[2 . id][2 . id]
2[2 . id]
1

[] |- 1[2 . id][2 . id]
[2 . id] |- 1[2 . id]
[2 . id; 2 . id] |- 1
[2 . id] |- 2
[] |- 1

1[2[shift] . id]
2[shift]
3

[] |- 1[2[shift] . id]


[] |- 1[2 . id][1 . id]

[] |- 1[2[shift] . id]
[2[shift] . id] |- 1 ~> 2[shift]
[2[shift] . id] |- 2[shift]
[2[shift] . id] |- 2[2 . id]

[] |- 2[3 . id]
[2 . id] |- 2

[] |- -1[open +3]


\+3[close +3][open \+3]

4 [] {} |- \+3[close +3][open \+3]
5 [] { +4 : 4 } |- \+3[close +3]

4 [] |- \+3[close +3][open \+3]
4 [open \+3] |- \+3[close +3]
4 [close +3; open \+3] |- \+3
4 [open \+3] |- \-1
4 [] |- \+3

[] [] |- \+1[close +1][open \+1]
[open \+1] [] |- \+1[close +1]
[open \+1] [close +1 at 1] |- \+1
[open \+1] [] |- \-1
[] [] |- \+1

[] [] |- \+1[close +1][shift]

((A : Type) -> \+2 -> \+2)[shift]


\+1[close] = \-1
\+2[close] = \+1
\+3[close] = \+2

\-1[shift] = \-2
\-2[shift] = \-3

(\+2 -> \+2)[close +2]
\+2[close +2] -> \+2[close +2][shift]
\-1 -> \-2

(\+2 -> \+2)[close][close]
(\+2[close] -> \+2[close][shift])[close]
(\+2[close][close] -> \+2[close][shift][close][shift])


(A : Type) -> (\+2 -> \+2)[close +2]

\-1[open t] = t
\-(n + 1)[open t] = \-n

\+n[+n close -m] = \-m

l |- (A -> B)[s]
l |- A[s] -> B[open \+l][s][shift]

(\+2 -> \+2)[close +2]
(\+2[close +2] -> \+2[open \+l][close +2][shift])
(\+2[close +2] -> \+2[close +2][shift])

[] [] |- \+2[open \+l][close +2][shift]
[open \+l; noop; shift] [noop; close +2; noop] |- \+2
[open \+l; noop; shift] [noop; close +2; noop] |- \+2

[open \+l; shift] [close +2] |- \+2
[shift] [close +2] |- \-1

\+2[close][close] -> \+2[open \+l][close][open \+l][close]
\-2 -> \+1[close][open \+l][close]

(a -> b)[s]
(a[s] -> b[lift(s)])

[] |- (A : Type) -> A\-1
[] |- (A : Type) -> _A

[open \+2] |- (A : Type) -> A\-1
[open \+2] |- (A : Type) -> _A

(\+2 -> \+2)[close +2]
\+2[close +2] -> \+2[open \+l][close +2]

// TODO: exception?
\-1 == \-1
\-(n + 1)[lift(s)] == \-n[s][shift]

(\+2 -> \+2)[close +2]
(\+2[close +2] -> \+2[lift(close +2)])
(\+2[close +2] -> \+2[close +2][shift])

(\-1 -> \-2)[open A]
(\-1[open A] -> \-2[lift(open A)])
(\-1[open A] -> \-1[open A][shift])

(A : Type) -> (\+1 -> \+1)[close]
(A : Type) -> \+1[close] -> \+1[close][shift]
(A : Type) -> \-1 -> \-2

((A : Type) -> \-1 -> \-2)
(\-1 -> \-2)[1 . shift]

((A : Type) -> \+2 -> \+2)[shift]
((A : Type) -> \+3 -> \+3)

0 [] { 3 : 3 } |- \+3



[] |- (\+2 -> \+2)[close +2]
[close +2] |- \+2 -> \+2

[] |- (\+2 -> \+2)[close +2]
[close +2] |- \+2 -> \+2

\+2[close +2] -> \+2[open \+l][close +2]

[] |- (\+2 -> \+2)[close +2]
[close +2] |- \+2 -> \+2


[\+1] [+2] |- \-1


(\-1 -> \-2)[open ]

(A : Type) -> (\+2 -> \+2)[close +2]

[] |- (\+2 -> \+2)[close +2]
[close +2] |- \+2 -> \+2
[open +l; lift([close +2])] |- \+2


(λa)[s]
λ(a[+l . ])
[open +l; lift([close +2])] |- \+2

[open +l; lift([close +2])] |- \+2

[open \+l] [] |- \+2

[\+1] [+2] |- \-1 -> \-2

(\-1 -> \-2)[id]
\-1[id] -> \-2[open \+l][id][shift]

\-1 -> \-2


(λa)[s]
λ(a[+l . ])

[] |- \-1[open a[open b]]
[a[open b]] |- \-1 ~> a[open b]
[open b; _] |- (λ-2)

[] |- \-1[open a[open b]]

// fast / naive enough?
[open \+2; open \+1] {} |- \+1[close +1][open \+1]
[open \+1; open \+2; open \+1] {} |- \+1[close +1]
[open \+1; open \+2; open \+1] { 1 : [3] } |- \+1
[open \+1; open \+2; open \+1] [] |- \-1
[_; _; _] [] |- \+1


[open \+2; open \+1] {} |- \+1[open \+1][close +1][open \+1]
[open \+1; open \+2; open \+1] {} |- \+1[open \+1][close +1]
[open \+1; open \+2; open \+1] { 1 : [3] } |- \+1[open \+1]
[open \+1; open \+1; open \+2; open \+1] { 1 : [3] } |- \+1
[open \+1; open \+1; open \+2; open \+1] { 1 : [3] } |- \-2


[open \+2; open \+1] { 1 : [2] } |- \+1[open \+1]
[open \+1; open \+2; open \+1] { 1 : [2] } |- \+1
[open \+1; open \+2; open \+1] { 1 : [2] } |- \+1


[open \+2; open \+1] {} |- \+1[close +1][open \+1]
[open \+1; open \+2; open \+1] {} |- \+1[close +1]

[open \+1; open \+2; open \+1] { 1 : 3 } |- \+1
[open \+1; open \+2; open \+1] { 1 : 3 } |- \-1
[open \+1; open \+2; open \+1] { 1 : 3 } |- \+1

[open \+2; open \+1] [none; none] |- \+2[close +2][open \+2]
[open \+2; open \+2; open \+1] [] |- \+2[close +2]

[open \+2; open \+2; open \+1] { 2 : } |- \+2[close +2]

// capture entire stack
[open \+2; open \+1] {} |- \+1[open \+1][close +1][open \+1]
[open \+1; open \+2; open \+1] {} |- \+1[open \+1][close +1]
[open \+1; open \+2; open \+1]
{ 1 : ([open \+1; open \+2; open \+1], {}) } |- \+1[open \+1]
[open \+1; open \+1; open \+2; open \+1]
{ 1 : ([open \+1; open \+2; open \+1], {}) } |- \+1
[open \+1; open \+2; open \+1] {} |- \+1
[open \+1; open \+2; open \+1] {} |- \-1

// shift stack
[open \+2; open \+1] {} |- \+1[open \+1][close +1][open \+1]
[open \+1; open \+2; open \+1] {} |- \+1[open \+1][close +1]
[open \+1; open \+2; open \+1] { 1 : (3, {}) } |- \+1[open \+1]
[open \+1; open \+1; open \+2; open \+1] { 1 : (3, {}) } |- \+1
[open \+1; open \+2; open \+1] {} |- \+1
[open \+1; open \+2; open \+1] {} |- \-1

\+1[open \+1][close +1][open \+1]
\+1[close +1][open \+1]
\-1[open \+1]
\+1


\-1[shift][open \+2][open \+1]

[open \+2; open \+1] |- \-1[shift]
[open \+1] |- \+1


\-1[open a] ~> a
\-(n + 1)[open a] ~> \-n

(λa)[+1 close -1]
λ(a[+l][+1 close -2])


(λa)[+1 close -1]
λ(a[+1][+1 close -2])

s1 λe1 = s2 λe2
lift s1 e1 = lift s2 e2

(λe1)
open t (λλ-2) = λλ-2


[noop; open +2; open +1]
lift s (-1) = -1
lift s (-(n + m)) =

-2[open +1][open +2]
-2[-l open +1]

[+1 close -2]

t[s][]
(λa)[+1 close -2]
λ(a[+l][close +1])

(λa)[close +1]
λ(a[+l][close +1])

lift(, 1) = 1
lift(open +n, n + 1) = shift(s(n))

lift(open +, 1) = 1
lift(open +n, n + 1) = shift(s(n))

lift(s, 1) = 1
lift(s, n + 1) = shift(s(n))

\-1[shift][open \+2]


2 [\+2; \+1] {} |- \+1[open \+1][close +1][open \+1]
2 [\+1; \+2; \+1] {} |- \+1[open \+1][close +1]
2 [\+1; \+2; \+1] { 1 : (2, 3, {}) } |- \+1[open \+1]
2 [\+1; \+1; \+2; \+1] { 1 : (2, 3, {}) } |- \+1
2 [\+1; \+2; \+1] {} |- \-1

2 [\+2; \+1] {} |- (λ\+1)[close +1][open \+1]
2 [\+1; \+2; \+1] {} |- (λ\+1)[close +1]
2 [\+1; \+2; \+1] { 1 : (2, 3, {}) } |- λ\+1
3 [\+2; \+1; \+2; \+1] { 1 : (2, 3, {}) } |- \+1
2 [\+1; \+2; \+1] {} |- \-1
2 [\+2; \+1] {} |- \+1

2 [\+1; \+2; \+1] { 1 : (3, {}) } |- \+1[open \+1]
2 [\+1; \+1; \+2; \+1] { 1 : (2, 3, {}) } |- \+1
2 [\+1; \+2; \+1] {} |- \-1


1 |- λ((\-1 \+2)[close +2])

(λ\+1)[close +1][open \+1]

(λ-1)[close +1] = (λ-1)[close +1]
(-1[+l][close +1][shift]) = (-1[+l][close +1][shift])

(λ+1)[close +1] ~~> λ-2

// colision inside of lambda related to open
(λa)[s]
λ(a[+l][])
closed[s][shift]


\-1[open \+l][shift]


1 2 => x y

1 |- (x => (x\+2 y\-2)[close +2 to -1])[open -1 to y\+1]
// entering a binder requires to shift all free negative variables
// the right side on open is locally closed, so only shift the left
// the left side on close is locally closed, so only shift the right
1 |- x => (x\+2 y\-2)[close +2 to -1][open -2 to y\+1]
1 |- x => (x\+2[close +2 to -1] y\-2[close +2 to -1])[open -2 to y\+1]
1 |- x => (x\-1 y\-2)[open -2 to y\+1]
1 |- x => (x\-1[open -2 to y\+1] y\-2[open -2 to y\+1])
1 |- x => x\-1 y\+1

// should definitely be true
(A : Type) -> \-1 -> \-2 `unify` (A : Type) -> _A -> _A
// solution, open and invoke a level in the context
2 |- (A : Type) -> \-1 -> \-2 `unify` (A : Type) -> _A -> _A
3 |- (\-1 -> \-2)[open -1 to \+3] `unify` (_A -> _A)[open -1 to \+3]
3 |- \+3 -> \+3 `unify` _A -> _A


// should ALWAYS be (y => x => x\-1 y\-2)
l |- (y => (x => x\-1 y\+2)[close +2 to -1])
// follow the steps and you will get to this
l + 2 |- (x\+(l + 2) y\+2)[close +2 to -2][open -2 to \+(l + 1)]
// what happens if l is 0?
2 |- x\+1 y\+1
// what happens if l is 1?
3 |- x\+3 y\+3

2 |- (x\+2 \+2)[open -2 to \+1][close +2 to -2]
2 |- (x\-2 y\-2)


l |- (y => (x => x\-1 y\+2)[close +2 to -1 at l + 1])
l + 1 |- (x => x\-1 y\+2)[close +2 to -1 at l + 1][open -1 to \+(l + 1)]
l + 2 |- (x\-1 y\+2)[open -1 to \+(l + 2)][close +2 to -2 at l + 1][open -2 to \+(l + 1)]
l + 2 |- (\+(l + 2) y\+2)[close +2 to -2 at l + 1][open -2 to \+(l + 1)]
// what happens if l is 0?
2 |- (\+2 y\+2)[close +2 to -2 at l + 1][open -2 to \+(l + 1)]

// should ALWAYS be (y => x => x\-1 y\-2)
l |- (y => (x => x\-1 y\+2)[close +2 to -1])
l + 1 |- (x => x\-1 y\+2)[close +2 to -1][open -1 to \+(l + 1)]
l + 2 |- (x\-1 y\+2)[open -1 to \+(l + 2)][close +2 to -2][open -2 to \+(l + 1)]
l + 2 |- (x\-1 y\+2)[open -1 to \+(l + 2)][close +2 to -2][open -2 to \+(l + 1)]

l |- (y => (x => x\-1 y\+2)[close +2 to -1])

// should ALWAYS be (y => x => x\-1 y\-2)
l |- (y => (x => x\-1 y\+1)[close +1 to -1])
l + 1 |- (x => x\-1 y\+1)[close +1 to -1][open -1 to \+(l + 1)]
l + 2 |- (x\-1 y\+1)[open -1 to \+(l + 2)][close +1 to -2][open -2 to \+(l + 1)]
l + 2 |- (x\-1 y\+(l + 1))[open -1 to \+(l + 2)][close +1 to -2][open -2 to \+(l + 1)]

// l = 0
2 |- (x\-1 y\+1)[open -1 to \+2][close +1 to -2][open -2 to \+1]
2 |- (x\+2 y\+1)

// l = 1
3 |- (x\-1 y\+1)[open -1 to \+3][close +1 to -2][open -2 to \+2]
3 |- (x\+3 y\+2)

// should ALWAYS be (y => x => x\-1 y\-2)
l |- (y => (x => x\-1 y\+2)[close +2 to -1])
l + 1 |- (x => x\-1 y\+2)[close +2 to -1][open -1 to \+(l + 1)]
l + 2 |- (x\-1 y\+2)[open -1 to \+(l + 2)][close +2 to -2][open -2 to \+(l + 1)]

// l = 0, leads to [close +2] at level 1, which is weird
// reminds me of escaping the scope
1 |- (x => x\-1 y\+2)[close +2 to -1][open -1 to \+1]



1 |- (x => x\-1 y\+2)[close +2 to -1][open -1 to \+1]

3 |- x\+3 y\+2


// should ALWAYS be (y => x => x\-1 y\-2), when l > 1
l |- (y => (x => x\-1 y\+2)[close +2 to -1])

(y => (x => x\-1 y\+2)[close +2 to -1])


l + 2 |- (x\-1 y\+(l + 1))[open -1 to \+(l + 2)][close +1 to -2][open -2 to \+(l + 1)]

(x\-1 y\+(l + 1))[open -1 to \+(l + 2)][close +1 to -2][open -2 to \+(l + 1)]
(x\+(l + 2) y\+l)

(x\+(l + 2) y\+(l + 1))


(x\+(l + 2) y\+(l + 1))[close +(l + 2) to -1]
(x\+(l + 2) y\+(l + 1))


l |- (y => (x => x\-1 y\+2)[close +l to -1])
l + 1 |- (x => x\-1 y\+2)[close +l to -1]

l |- (y => (x => x\-1 y\+2)[close +2 to -1])
l + 2 |- (x\+(l + 2) y\+(l + 1))

l + 2 |- (x\-1 y\+(l + 1))[open -1 to \+(l + 2)][close +1 to -2][open -2 to \+(l + 1)]


(x\+(l + 2) y\+(l + 1))[close +(l + 2) to -1]
(x\-1 y\+(l + 1))[open -2 to +]

//
l |- (y => (x => x\-1 y\+2)[close +2 to -1])
l + 2 |- (x\+(l + 2) y\+(l + 1))


l |- (y => (x => x\-1 y\+(l + 2))[close +(l + 2) to -1])
l + 1 |- (y => (x => x\-1 y\+(l + 2))[close +(l + 2) to -1])

//
l + 2 |- (\-1 \+2)[open -1 to \+(l + 2)][close +2 to -2][open -2 to \+(l + 1)]

l + 2 |- (\-1 \+2)[open -1 to \+(l + 2)][close +2 to -2][open -2 to \+(l + 1)]


l + 2 |- \+(l + 2) \+2[open -1 to \+(l + 2)]

//
l |- (y => (x => x\-1 y\+(l + 2))[close +(l + 2) to -1])
l + 1 |- (x => x\-1 y\+(l + 2))[close +(l + 2) to -1][open -1 to +(l + 1)]
l + 2 |- (x\-1 y\+(l + 2))[open -1 to +(l + 2)][close +(l + 2) to -2][open -2 to +(l + 1)]


0 |- (y => (x => x\-1 y\+1)[close +1 to -1])


0 |- (y => (x => x\-1 y\+1)[close +1 to -1])
l |- (y => (x => x\-1 y\+(l + 1))[close +(l + 1) to -1])

l |- (z = _; y => (x => x\-1 y\+(l + 1))[close +(l + 1) to -1])

(z = _; y => (x => x\-1 y\+1)[close +1 to -1])

0 [] [] {} |- (y => (x => x\-1 y\+1)[close +1 to -1])
1 [back] [open \+1] {} |- (x => x\-1 y\+1)[close +1 to -1]
1 [back; back] [open \+1] { +1 := ({}, -1 ~> \+1) } |- x => x\-1 y\+1
2 [] [open \+2; open \+1] { +1 := ({}, -1 ~> \+1) } |- x\-1 y\+1
2 [] [open \+2; open \+1] { +1 := ({}, -1 ~> \+1) } |- x\+2 y\+1

0 [] [] |- (y => (x => x\-1 y\+1)[close +1 to -1])
0 [] [] |- (y => (x => x\-1 y\+1)[close +1 to -1])


0 [] [] {} |- (y => (x => x\-1 y\+1)[close +1 to -1])
1 [back] [open \+1] {} |- (x => x\-1 y\+1)[close +1 to -1]
1 [back; back] [open \+1] { +1 := ({}, -1 ~> \+1) } |- x => x\-1 y\+1

(y => (x => x\-1 y\+2)[close +2 to -1])


(y => (x => x\-1 y\+1)[close +1 to -1])
(y => (x => (x\+2[close +2 to -1]) y\+1)[close +1 to -1])

(y => (x => x\-1 y\l+2)[close l+2 to -1])
(y => (x => x\-1 y\l+2)[close l+2 to -1])

[] [] |- (y => (x => x\-1 y\+1)[close +1 to -1])
[] [\+1] |- (x => x\-1 y\+1)[close +1 to -1]
[] [\+1] |- x => x\-1 y\+1
[] [\+2; \+1] |- x\-1 y\+1
  [y\+1] [\+2; \+1] |- x\+2
  [] [\+2; \+1] |- y\+1

[] [] |- λ(λ(\-1 \+1))[+1 := -1]
[] [\+1] |- (λ\-1 \+1)[+1 := -1]
[] [\+1] |- λ\-1 \+1
[] [\+2; \+1] |- \-1 \+1
  [\+1] [\+2; \+1] |- \-1 ~> \+2
  [] [\+2; \+1] |- \+1

[] [\+1] |- (x => x\-1 y\+1)[close +1 to -1]
[] [\+1] |- x => x\-1 y\+1
[] [\+2; \+1] |- x\-1 y\+1
  [y\+1] [\+2; \+1] |- x\+2
  [] [\+2; \+1] |- y\+1



[] |- _A[-1 := \+2] === +2


[] |- _A === +2[+2 := -1]

[] |- _A === \+1[+1 := \-1]
[] |- _A === \-1

l |- λ(_A λ(\-1 \+1))[+1 := -1]

0 |- (x = _; y => (x => x\-1 y\+2)[)


0 |- ((z => y => (x => x\-1 y\+2)[close +2 to -1]) _)

((z => y => (x => x\-1 y\+2)[close +2 to -1]) _)
(z => y => (x => x\-1 y\+2)[close +2 to -1])[open -1 to _]
(z => y => (x => x\-1 y\-2))

y => _A[close +2 to -1] `unify` x => x\-1
0 |- y => _A[close +2 to -1] `unify` x => x\-1


1 |- x => \+1[close +2 to -1]

(((1 z) => (2 y) => ((3 x) => x\-1 y\+2)[close +2 to -1]) _)
((2 y) => ((3 x) => x\-1 y\+2)[close +2 to -1])[open -1 to _]
((1 y) => ((2 x) => x\-1 y\+2[close +2 to -2][open -3 to _]))
(1 y) => ((2 x) => x\-1 y\-2)


(((1 z) => (2 y) => ((3 x) => x\-1 y\+2)[close +2 to -1]) _)
((2 y) => ((3 x) => x\-1 y\+2)[close +2 to -1])[open -1 to _]

y => _A[close +2 to -1] `unify` x => x\-1


l |- (A : Type) -> (_A -> _A)[close +(l + 1) to -1] `unify` (A : Type) -> A\-1 -> A\-2
l + 1 |- (_A[close +(l + 1) to -1] -> _A[close +(l + 1) to -2]) `unify` A\-1 -> A\-2
  l + 1 |- (_A[close +(l + 1) to -1]) `unify` A\-1
  l + 1 |- _A `unify`

  l + 1 |- A\-1[open -1 to \+(l + 1)][close +(l + 1) to -2] `unify` A\-2
  l + 1 |- A\-2 `unify` A\-2

A\-2[open -2 to \+(l + 1)][open -1 to \+(l + 1)]

A\-2[open \+(l + 1)][to \+(l + 2)]

(A : Type) -> (A\+1 -> A\+1)[from +1]

((A : Type) -> A\-1)[to \+2]

(A : Type) -> A\+1[from +1] -> A\+1[lift(from +1)]

(A : Type) -> A\+1[from +1] -> A\+1[lift(from +1)]

A\+1[from] = A\-1
A\+2[from] = A\+1

lift s \-(n + 1) = s \-n
lift s +n = s +n | \-m => \-(m + 1)

((B : Type) -> (A\-2 B\-1))[to f]
((B : Type) -> (A\-2 B\-1)[lift(to f)])
((B : Type) -> A\-2[lift(to f)] B\-1[lift(to f)])
((B : Type) -> A\-1[to f] B\-1)
((B : Type) -> f B\-1)

[to \-1; to a; to b]
[from; from; none]

l |- \+1

(B : Type) -> (A\-2[open f] B\-1)

[close; close; close] |- (A : Type) -> A\+4[close]
[close; close; close; close] |- A\+4 ~> A\-1


lift s +(n + 1) = s +n | \-m => \-(m + 1)

0 |- λ(λ+1)[close] ~~> λλ-2
0 |- λ(λ+1)[close]
     λλ+1[lift close]
     λλ+1[close][shift]
     λλ-2

lift s l \-(n + 1) = s +l \-n
lift s l +n = s +(l + 1) +n | \-m => \-(m + 1)

close l +l = \-1

l |- λ(λ+(l + 1))[close] ~~> λλ-2
l |- λ(λ+(l + 1))[close]
     λλ+(l + 1)[lift close]
     λλ-2


0 |- (x = _; (λ(λ+2)[close])[close]) ~~> λλ-2

1 |- λ(λ+2)[close] ~~> λ(λ+2[lift(close)])
1 |- λ(λ+2)[close] ~~> λ(λ+1[close])

λ(_A _A)[close] `unify` λ(+1 +1)[close]
λ(_A _A)[close] `unify` λ(+1 +1)[close]

((A : Type) -> A\+4[close])

A\+1[close] = A\-1
A\+2[close] = A\+2

A\+2[lift(close)] = A\-2
A\+3[lift(lift(close))] = A\-3

((B : Type) -> (A\-2 B\-1))[to f]


(λ(λ+2)[close])[close] ~~> λλ-2
λ(λ+2)[close][lift close]
λλ+2[lift close][lift (lift close)]

0 |- λ(λ+1)[close]
     λλ+1[lift close]
     λλ+1[close][shift]
     λλ-2

0 |- (λ(λ+2)[close])[close]
1 [] [close +1] |- λ(λ+2)[close]
1 [open \-1] [close +1] |- (λ+2)[close]
1 [open \-1] [close +1] |- (λ+2)[close]

λ.(λ.+2[close])[close]
λ.(λ.+1[lift close])
λ.(λ.-2)



\-1[lift s] = \-1
\-(n + 1)[lift s] = \-n[s][shift]

a[lift s]
// relevant property is that x doesn't appear in s
a[open x][s][close x]

-1[open x][s][close x] = x[s][close x] = x[close x] = -1
-2[open x][s][close x] = -1[s][close x] =
                         -1[close y][close x] = -1[close x] = -2
                         -1[open a][close x] = a[close x] = a

+y[open x][s][close x] = +y[s][close x]
+y[open _][close x] = +y
+y[close y][close x] = -1[close x] = -2

\-1[incr][-2 open a][decr] = \-2[open a][decr] = \-2[decr] = \-1
\-2[incr][open a][decr]

λ.(λ.-2)

+2[close][close] = \-1

[] [close] |- λ(λ+2)[close]
[free] [close] |- (λ+2)[close]
[free] [close; close] |- λ+2
[free; free] [close; close] |- +2


open t \-1 = t
open _ \-(n + 1) = \-n

close \+1 = \-1
close \+(n + 1) = \+n

lift _ \-1 = \-1
lift s \-(n + 1) = s \-n
lift s \+n = s +n | \-m => \-(m + 1)


λ.(λ.+2[close])[close]
λ.λ.+1[lift close]
λ.λ.-2

(λ.(λ.(λ.+2[close])[close])[close]) _
(λ.(λ.+2[close])[close])[close][open _]
λ.(λ.+2[close])[close][lift close][lift (open _)]
λ.(λ.-2)[lift close][lift (open _)]

(λ.(λ.(λ.+3[close])[close])[close]) _
(λ.(λ.+3[close])[close])[close][open _]


λ.(λ.+2[close])[close]
λ.(λ.+2[close][lift close])
λ.(λ.-2[lift close])
λ.(λ.-1)

λ.λ.(+2 +1)[close]

(λ+1.λ+2.+3) a

(λ.+2[close +2])
```

## Teika

```rust
Button
  : () -[Database]> React.Component
  = () => {
  const x = get();
  return (<div> {x} </div>)
};
```

## Joshua's

```markdown
## Chapter {{pinned}}

### Abc

BBBBB

### Def

AAAAA

#### Def concepts

... ai puts here
```

## AI optimizer

### Valid Transformations

```rust
pred (succ x)

- max term size
- locality / proxy accumulate de-bruijin
- linearity / variable grading
- total number of reduction



(x => M) N
M[x := N]

(x => x x) N(10)
N N

```

## Autocomplete, Code quality suggestion

Github find all PR / patches, trim many features, rank developers,

```markdown

```

## De-bruijin needs data

```rust
(λ.(λ.(λ.+1))[close +1]) a
(λ.(λ.+1))[close +1][open a]
λ.(λ.+1)[lift (close +1)][lift (open a)]
λ.λ.+1[lift (lift (close +1))][lift (lift (open a))]
λ.λ.-3[lift (lift (open a))]
λ.λ.-1[open a]
λ.λ.a


(λ.(λ.(λ.+3[close +3]))) a
(λ.(λ.+3[close +3]))[open a]
λ.(λ.+3[close +3])[lift (open a)]
λ.λ.+3[close +3][lift (lift (open a))]
λ.λ.-1[lift (lift (open a))]
λ.λ.-1

0 |- (λ.(λ.(λ.+3[close +3]))) a
0 |- (λ.(λ.+3[close +3]))[open a]
[] [] |- (λ.(λ.+3[close +3]))[open a]

0 [] [] |- (λ.(λ.(λ.+3))) a
0 [] [] |- (λ.(λ.+3))[a]
1 [a] [1] |- λ.(λ.+3)
2 [a; +2] [1; 2] |- λ.+3
3 [a; +2; +3] [1; 2; 3] |- +3

λ(1).+1 +1
λ(1)._A _A


(x = _; y = λ(2).+2 +2; +3)

(λ(1).(λ(2).+2) (λ(2).+2 +2)) _
((λ(2).+3) (λ(2).+2 +2))[1 | _]
((λ(2).+3) (λ(2).+2 +2))[1 | _]
(+2[2 | λ(2).+2 +2])[1 | _]
+2[2 | λ(2).+2 +2][2 | _]
+2[2 | λ(2).+2 +2][2 | _]


(λ(1).λ(2).+2 +2) _
∀(2).+2 +2
[-1]  |- (λ.(λ.+3))[open a]

(λ(1).(λ(2).(λ(3).+3))) a

0 [] [] |- (λ(2).(λ(3).+3))[a]
1 [a] [a] |- λ(2).λ(3).+3
2 [a; +2] [1; 2] |- λ(3).+3
3 [a; +2; +3] [1; 2; 3] |- +3
(λ(2).(λ(3).+3))[open a]



v <= l
-------
l |- +v


v |- b
------------
l |- λ(v). b




[] |- (λ(3).(λ(4).+2))[2 | a]


[] |- (λ(3).(λ(4).+2))[2 | a]

(λ(1).+1 +1) (λ(2).+2)
(λ(2).+2 +2)
(+2 +2)[2 := λ(1).+1]
(λ(1).+1) (λ(1).+1)
+1[1 := λ(1).+1]
λ(1).+1

x = _;
(λ(1).+1 _A)


[] |- λ(1).+1 === λ(2).+2
[] |- λ(1).+1 === [] |- λ(2).+2
[-1] |- +1 === [] |- λ(2).+2


λx.a `unify` λy.b
a[x := z] `unify` b[y := z]
_A `unify` b[y := x]


[] |- λ.+1[close +2] === [] |- λ.+2[close +2]

[] |- λ.+1[close +1] === [] |- λ.+2[close +2]
[] |- +1[close +1] === [] |- +2[close +2]
[close +1] |- +1 === [close +2] |- +2
[close +1] |- -1 === [close +2] |- -2

[] |- λ.(+1 (λ.+1))[close +1] === [] |- λ.(+3 (λ.+3))[close +3]
[] |- λ.(_A (λ._A))[close +1] === [] |- λ.(+3 (λ.+3))[close +3]
[] |- λ.(_A (λ._A))[close +1] === [] |- λ.(+3 (λ.-2))[close +3]
[] |- (_A (λ._A))[close +1] === [] |- (+3 (λ.-2))[close +3]
[close +1] |- _A (λ._A) === [close +3] |- +3 (λ.-2)
  [close +1] |- _A === [close +3] |- +3
  _A := +3[close +3][open \+1]
[close +1] |- λ.+3[close +3][open \+1] === [close +3] |- λ.-2
[lift (close +1)] |- +3[close +3][open \+1] === [lift (close +3)] |- -2
[close +3; open \+1; lift (close +1)] |- +3 === [lift (close +3)] |- -2
[open \+1; lift (close +1)] |- -1 === [lift (close +3)] |- -2
[lift (close +1)] |- +1 === [lift (close +3)] |- -2
[] |- -2 === [lift (close +3)] |- -2

[lift [close +1]] |- +2[close +2][open \+1] === [lift [close +2]] |- +2
[close +2; open \+1; close +1] |- +2 === [close +2] |- +2


[] |- λ.(_A (λ._A))[close +1] === [] |- λ.(-1 (λ.+3))[close +3]
[] |- (_A (λ._A))[close +1] === [] |- (-1 (λ.+3))[close +3]
[close +1] |- _A (λ._A) === [close +3] |- -1 (λ.+3)
  [close +1] |- _A === [close +3] |- -1
  _A := -1[close +3][open \+1]

[close +1] |- λ.-1[close +3][open \+1] === [close +3] |- (λ.+3)
[lift (close +1)] |- -1[close +3][open \+1] === [lift (close +3)] |- +3
[close +3; open \+1; lift (close +1)] |- -1 === [lift (close +3)] |- +3
[] |- -2 === [] |- -2


line 15, col 2

> f expr expr2
    ^  ^

Message

expected : Option Int
received : Option String
```

## Dynamic

```rust
[@mode dynamic]

l => l.map(x => x + 1);
l => List.map(l, x => x + 1);

f = x => print(cast x);
```

## Working

```rust

Id = (A : Type) -> A;
x : Id Int = _;
(x : _A);

[] |- _A === Id Int

expected : \-1[(open \+2) :: lift (open \+1)]
received : \-2[(open \+3) :: lift ((open \+2) :: lift (open \+1))]

expected : \-0[(open \+4) :: lift ((open \+3) :: lift ((open \+2) :: lift (open \+1)))]
received : \-0[(open \+3) :: lift ((open \+2) :: lift (open \+1))]

expected : \-0[(open \+4) :: lift ((open \+3) :: lift ((open \+2) :: lift (open \+1)))]
received : \-0[(open \+3) :: lift ((open \+2) :: lift (open \+1))]

expected : ((x : ) -> \-1)[(open \+3) :: lift ((open \+2) :: lift (open \+1))]
received : ((x : ) -> \-1)[(open \+2) :: lift (open \+1)][(open \+3) :: lift ((open \+2) :: lift (open \+1))]

annot : ((A : \+1) -> (x : \-0[(open \+4) :: lift ((open \+3) :: lift ((open \+2) :: lift (open \+1)))]) -> \-1)[(open \+3) :: lift ((open \+2) :: lift (open \+1))]
```

## Effect

```rust
x : IO(Result(String, Error))
x : Result(IO(String), Error)

Eff l r = Raw_eff (sort_uniqe l) r;

x : Eff [IO; Result] String
x : Eff [Result; IO] String
x : Eff [Random] String;

() = format();

(x : Nat 1) => x + x;

Socket : {
  connect : () -> $Socket;
  close : $Socket -> ();
};
() => (
  x = Socket.connect();
  Socket.close(x);
);

x => y => x;
x => y => y;

r0 : Linear
r1 : Linear

(three, r1) = add(r0, r1);

Array : {
  $Array : (A : Type) -> Type;

  make : () -> $Array Int;
  get : {A} -> (i : Nat, arr : Array A) -> (Array A, Option A);

  Read_only_array : _;

  split : (i : Nat, arr : Array A) -> (List (Read_only_array A i arr))
  merge : (l : List (Read_only_array A i arr)) -> Array A;
} = _;

f = arr0 => (
  (arr1, x) = Array.get(0, arr0);
  (arr2, y) = Array.get(1, arr1);

)
find : () -> Eff [Database] (List User);

integration()

@debug(find())
```

## Unification

```rust
(A : _) -> _B[close +3] === (A : _) -> \-1
(A : _) -> _B[close +3] === (A : _) -> \-1
_B === \-1[open \+3]


(A : Type) -> A\+2 === (A : Type) -> A\+2


(A : _) -> (_C ((B : _) -> _C))[close +2] === (A : _) -> (_D ((B : _) -> \-2))
(_C ((B : _) -> _C))[close +2] === (_D ((B : _) -> \-2))
((B : _) -> _C)[close +2] === (B : _) -> \-2
_C[lift (close +2)] === \-2
_C === \-2[lift (open \+2)]
_C === \-1

_C[shift] === \-2
_C === \-2[lower]

_A[lift s] === \-2
_A === \-2[lift (not s)]

_A[lift (close +n)] === \-2
_A === \-2[lift (open \+n)]
\-2[lift (open \+n)][lift (close +n)] === \-2
\+n[lift (close +n)] === \-2
\-2 === \-2

_A[lift (open +n)] === \+n
_A === \+n[lift (close +n)]
\+n[lift (close +n)][lift (open +n)] === \+n[lift (close +n)]
\-2[lift (open +n)] === \+n[lift (close +n)]
\-2[lift (open +n)] === \+n[lift (close +n)]


_A[lift (close +n)] === \+n

_A[shift] === \-2
_A[shift] === \-2

_A === \-2[1 . id]


(A : Type) -> _A
l := t

expected : ((x : \+1[id]) -> \+1[close +3])[id]
received : ((_x0 : \+3[id]) -> \+3[id])[id][lift id][open \+3][+3 := \+1][id]

_A[close \+2][open \+3] === +2
_A === +2
_A[close \+2][open \+3] === +2

_A === -1

_A === +1[open +2]

_A[x := N] === x
x === x


_A[+2] ===

x[x := N] === x

(λλ_A[lift (1 1)] === λλ2)
(λ_A[lift (1 1)] === λ2)
(_A[lift (1 1)] === 2)

(x => _A) N === y


_A[x := N] === x

Γ,Δ1 |- A    Γ,Δ2 |- B
----------------------
    Γ |- A === B


_A[open +2][close +2] = -1
_A = -1[open +2][close +2]

Γ |- e : A  Γ |- r[x := e] : B
------------------------------
    Γ |- x = e; r : B

(Id = (A : Type) => A; )

m.n
_::n(m)

m.n a b
_::n
```

## Magic Dot

```rust
Nat = (A : Type) -> (z : A) -> (s : (x : A) -> A) -> A;
(zero : Nat) = A => z => s => z;
(succ : (n : Nat) -> Nat) = n => A => z => s => s (n A z s);
(add : (n : Nat) -> (m : Nat) -> Nat) = n => m => n Nat m succ;
(mul : (n : Nat, m : Nat) -> Nat) = n => m => n Nat zero (add m);
one = succ zero;
two = succ one;
four = mul two two;
eight = mul two four;
sixteen = mul two eight;
byte = mul sixteen sixteen;
byte String "zero" (_ => @native("debug")("hello"))


x.f === _::f(x)
x.f (1, 2) === _::f(x)(1, 2)


add = (!n : Nat, !m : Nat) => n + m;
id = {A} => (x : A) => x;

add = (!n : Nat, !m : Nat, log : Option Bool) => n + m;

x = add (n = 1, m = 2);

(1, 2) : (_A : Nat, _B : Nat);

(_A : Nat, _B : Nat) === (n : Nat, m : Nat)

(_A : Nat, _B : Nat) === (!n : Nat, !m : Nat)

(x = 1, y = 2) : (x : Nat, y : Nat);


Int : {
  @Int : Type;
  show : (i : @Int) -> () -> String;
};

ind_bool : (b : Bool) -> (P : Bool -> Type) -> P true -> P false -> P b;
ind_bool : (b : Bool (i + 1)) ->
  (P : Bool i -> Type) -> P true -> P false -> P (lower b);

Consistency = Soundness + Strong Normalization;

(Type 0 : Type 1)
(Type n : Type (n + 1))

id = (A : Type 1) => (x : A) => x;

id (Type 1)


ind_bool : (b : Bool) -> (P : Bool -> Type) -> P true -> P false -> P b = _;


Γ, x : @self(x). T |- T : Type
------------------------------
Γ |- @self(x). T : Type


Γ, x : @self(x). T |- M : T[x := @fix(x). M]
--------------------------------------------
Γ |- @fix(x). M : @self(x). T


Γ |- M : @self(x). T
--------------------------
Γ |- @unroll M : T[x := M]
```

## TCO Nat

```rust

two = z => s => s (s z);
two = z => s => s z (x => s x);
three = z => s => s z (x => s x s)
```

## Prevent Currying

```rust
id = (x : 'A) => x;

((x : 'A) -> 'A)

id : (x : 'A) -> 'A;
sum : (x : Int, y : Int) -> Int;

sum : (f : (x : Int, y : Int)) -> Int;

id (1);
sum 1 2;

sum (1, 2);
(x = (1, 2); sum(...x))
(sum((1, 2)))

(x, y) = id(_ = (1, 2));

(x, y) =>

ind : (b : Bool, P : Nat -> Type) -> (x : P true, y : P false) -> P b;
sum : (x : Int) -> (y : Int) -> Int;

sum 1 _


?sum 1 _
y => ?sum 1 _

id(1, 2) : (1, 2)


add a b = c;

// named return
(x : Int) -> (x : Int)
```

## Nominal

```rust

(Bool : Type) -> (true : Bool) -> (false : Bool) ->w

(Bool : Type) & {
  true : Bool;
  false : Bool;
  case : {A} -> (pred : Bool, then : A, else : A) -> A;

} = _;

add 1 ~> add 1
add 1 2 ~> 3

Γ |- A : Type
--------------------------
Γ |- @frozen(A) : Type

Γ |- M : A
--------------------------
Γ |- @freeze M : @frozen A

Γ |- M : @frozen A
--------------------
Γ |- @unfreeze M : A

---------------------------
@unfreeze (@frozen M) === M

T =
  (R : Type) ->
  ((A : Type) ->
    (x : A) ->
    (to_string : A -> String) ->
  R) -> R;

M : T = _;

to_string : ((A : Type) -> M ) -> String;

M : {
  Nat : Type;
  one : Nat;
  to_int : Nat -> Int
} = @nominal(key => (
  Nat = @frozen(key, Int);
  one = @freeze(key, 1);
  to_int = x => @unfreeze(key, x);
));

T = @nominal(key => (
  Nat = @frozen(key, Int);
  one = @freeze(key, 1);
)).Nat;

M.one
```

## Primitives

```rust

```

## External

```rust

```

## Linear Machine

```rust

(2 x : Int) => x + x
```

## Escape

```rust
call v f = f v
(A : Type) -> A


mono = w => x => y => (id => w (k x) (k y)) (x => x);
(a : _A\+1) => (A : Type) => (b : _B\+2) =>




(x => x) 1

((2 x : _A & _B) => (x : _A) + (x : _B))

(x0 : Int) => (x1 : Int) => x0 + x1


Nat (G : Grade) = (A : Type) -> (1 z : A) -> (G s : (1 A) -> A) -> A;
(x : Nat) => x Nat zero (acc => acc);

|- Type : Type


Γ |- e : A : Type

(('G + 1) x : @rec T. ('G x: T) -> _) => x x
(x : @rec T. (x : T) -> _) => x x

((x : A) => x) : ((x : A) -> A) : Type



Γ |- e : A : Type

x = Type;



-----------------
ctx, x : A |- x : A


ctx |- func : A -> B    ctx |- arg : A
----------------------------------
ctx |- func arg : B

-----------------------------------------
ctx |- (x => body) arg === body[x := arg]


----------------------------------------
ctx | setState |- setState(false) === ()


My_button = (label : String) => <Button>{label}</Button>;


My_button label

My_button =
  ctx |- label : String
  --------------------------------
  ctx |- <Button>{label}</Button>


ctx |- My_button : String -> DOM    ctx |- label : String
---------------------------------------------------------
ctx |- My_button label === <Button>{label}</Button>: DOM


ctx |- label : String
-------------------------------
ctx |- <Button>{label}</Button>


My_button = (label : String) => <Button>{label}</Button>;

(A : Type) -> _A -> _B === (A : Type) -> _A -> _B

(A : Type) -> _A[close +2] === (A : Type) -> A\-1


_A[open \+2 \+2][close +2] === -1



_A[close +2] === +2


+2 |- (λ_A[close +1]) +2 === _B
+2 |- _A[close +1][open +2] === _B
+2 |- _A[close +1] === _B[close +2]
+2 |- _A === _B[close +2][open +1]

+2 |- _A === _B[close +2][open +1]


+2 |- (λ_A) +2 === _B
+2 |- _A[open +2] === _B
+2 |- _A === _B[close +2]



_B[close +2][open +1] === +2
_B === +2[open +2]

_B[close +1] === +2[open +2]

_B[close +2] === +2


_B[close +2] === +2
_B[open +2] === +2

[y] |- (x => _A) y === _B
[y] |- _A[x := y] === _B
[y] |- _A === _B[y := x]



unify = a => b => (k => (_ = k a); k b) (x => x);

P => y => z => (f : (x : _) -> P _A) => (g : P _B) =>
  unify (f y : P (_A[x := y])) g

(P (_A[x := y]) : P _B)



a : P (_B[y := x])
a : P y

_B[y := x] == y

_A[x := y] == _B

_A[x := y] == _B[y := x]


P => y => z => (f : (x : _) -> P _A) => (
  _ : P _B = (f y : P (_A[x := y]));
  // _A === _B[y := x]
  (f z : P y)
);

P => y => z => (f : (x : _) -> P _A) => (
  _ : P _B = (f y : (P _A)[x := y]);
  // _A === _B[y := x]
  (f z : P y)
);

(P _A)[x := y] === P y

P (_B[y := x][x := z]) === P y

_B[y := x][x := z] === y

// I think this cannot happen
P => y => z => (f : (x : _) -> P _A) => (
  _ : P _B = (f y : P (_A[x := y]));
  // _A === _B[y := x]
  (f z : P y)
);
_B[y := x] == y


_B[y := x] == y


_B[close +2] === +2


_A[x := y] == y
_A[x := y][y := x] == y[y := x]
_A == x


l |- _A == r |- _B


_A := _C[!r]
_B := _C[!l]


l |- _C[!r] == r |- _C[!l]
l . !r |- _C == r . !l |- _C

_C == _C

_C[l][!r] == _C[r][!l]

_C[l][l] == _C[r][r]
_C[l][!r][l] == _C[r]

_C

x[close +3][open +3][close +3]

Γ, x : Int |- xy : Int

(f => y => ) (x => x)

((x => x) y)[y := z]
(x => x[y := z]) y[y := z]
(x => x) z

(x = y; x)[y := z]
(x = y[y := z]; x[y := z])
(x = z; x)


(x = y; x[y := z])
(x = y; x[y := z])
x[y := z][x := y]

(x = y; equal x y)
(equal x y)[x := y]

x === y

one = s => z => s z;
two = s2 => s1 => z => s2 (s1 z);

Nat 'n = (A : Type) -> ('n f : A -> A) -> A -> A;

Bool (n : Grade) =
  (A : Type) -> (B : Type) -> (A -> B) -> A -> B

true = x => y => x y;
false = x => y => y;

(A : Type)

Type n


(x : Int 'n) -> (y : Int 'n) -> Int 'n

Nat =
  union
    (case "z" end)
    (case "s" (cell self) end)
  close;

Bool =
  | (tag : #true)
  | (tag : #false);

Either A B =
  | (tag : "left", payload : A)
  | (tag : "right", payload : B);

| ("left", a) => _
| ("right", b) => _

Either A B = (tag : Bool, payload : tag Type A B);

| (true, a) => _
| (false, b) => _

Either A B =
  | True(payload : A)
  | False(payload : B);

| True(a) =>
| False(b) => _

data%Bool =
  | true
  | false;

x = #true;

Nat =
  | (tag : "z")
  | (tag : "s", pred : Nat);

"z" : Nat
("s", ("z", ()))
Nat =
  | Z | S (n : Nat);

1 + 2 : Int _a
1 : Int _n
((A : Type 0) => (x : A 1) => x) : Type 15;

```

## Teika Schema

```rust
NONCE, auto retry
VERSION
TRANSACTION
LOGIN
PERMISSION
SECURITY


WEBSOCKET
STATEFUL APIs
```

## Teika Database

```rust
User and Post
Both indexed by User.id
```

## Teika Workers

```rust
{

}
```

## Teika System

Workers + Database

```rust
storage@User = {
  @index id : @increment Nat;
  name : String;
};

User : {
  insert : _ -[DB.write "user"]> ();
  find_by_id : _ -[DB.read "user.id"]> ();
} = _;

@Routes = [
  POST "/users" =
    { name : String; } => User.insert { name = name; };
  GET "/users/:id" =
    { id : Nat; } => User.find_by_id id;
  GET "/users" =
    () => User.list ();
];

test@routes = [
  POST "/users" { name = "EduardoRFS"; }
    expected { id = 0; name = "EduardoRFS" };
  GET "/users/0"
    expected { id = 0; name = "EduardoRFS" };
];


((x : Nat) -> f x == g x)

(x : Nat) -> Nat

\forall x : Nat. Nat
∀x : Nat. Nat

ind : ∀b. ∀P. P true -> P false -> P b;

ind : (b : _) -> (P : _) -> P true -> P false -> P b;

x => x

\lambda x. x
λx -> x

Inductive Linear : Context -> Term -> Type :=
  | L_var var : Linear [var] (T_var var)
  | L_lam {lam_ctx} param
    {body_term} (body : Linear (param :: lam_ctx) body_term)
    (param_not_free_in_lam_ctx : free_in_ctx param lam_ctx = false)
    : Linear lam_ctx (T_lam param body_term)
  | L_app {lam_ctx lam_term} (lam : Linear lam_ctx lam_term)
    {arg_ctx arg_term} (arg : Linear arg_ctx arg_term)
    : Linear (append lam_ctx arg_ctx) (T_app lam_term arg_term)

x ⊢ x


Γ, x ⊢ M -> not (free x Γ) -> Γ ⊢ x. M
```

## First-class Grades

```rust

Fix n = (Fix 1, (f : Fix 1) -[1]> Unit);
(f : Fix 1) => (
  (x, k) = f;
  k x;
)


Fix (G + 1) = (x : Fix G) -[1]> Unit;
(x : Fix 1) => (
  x x
)

Nat = (G : Grade, (A : Type) -> (1 z : A) -> (G s : A -> A) -> A);

f = (G : Grade) => Int G

T = Int 1;

((A : Type) -> A);

Type 0 : (G : Grade) -> Type 1 G




Γ |- n : Grade  Γ |- A : Type
-----------------------------
Γ, x $ n : A

Type : Type $ 0;

Γ |- A : Type  Γ |- n : Grade
-----------------------------
Γ |- M : A $ n

Γ, x : A $ n |- B : Type $ 0
------------------------------------
Γ |- ((x : A $ n) -> B $ m) : Type $ 0

Γ, x : A $ n |- M : B $ m
--------------------------------------------------
Γ |- (x : A $ n) => M : ((x : A $ n) -> B $ m) $ k

Γ |- M : ((x : A $ m) -> B $ n) $ 1  Γ |- N : $ n
--------------------------------------------------
Γ |- M N : ((x : A $ n) -> B $ m) $ k

(x : A $ 1) => x : A
Type $ 0 : Type

(x : Socket$) => _;


Id
  : (A : Type $ 0) => Type $ 0;
  = (A : Type $ 0) => A;
Never = (A : Type) ->
id = (A $ 0 : Type) => (x $ G : A) => x
id = ({A} => (x $ 1 : A) => x) $ 15;

A $ G -> A $ G
id
  : (A : Type $ 0) -> (x : A $ 'X) -> (A $ 'X) $ _G
  = (A : Type) => (x : A) => x;

(x : Int $ 5) => (
  incr () = x + 1;

  incr : () -> (Int $ _X) $ _Y where _X * _Y = 5

  // but, this seems as powerful
  incr : () -> (Int $ 1) $ 5
  // and more general than, but maybe this could put it back
  incr : () -> (Int $ 5) $ 1
);
Nat = (G : Grade, (A $ 0 : Type) -> ($ 1 : A) -> ($ G : ($ 1 : A) -> A) -> A);


Bool : Type;
true : Bool;
false : Bool;

Bool = b @-> (P : Bool $ 0 -> Type $ 0) ->
  P true $ @b (_ => Grade) 1 0 ->
  P false $ @b (_ => Grade) 0 1 -> P b $ 1;
true = P => x => y => x;
false = P => x => y => y;

dup (b : Bool $ 1) : Bool $ 2 =
  b _ true false;

// without Grade : Type
Bool : Type;
true : Bool;
false : Bool;

Bool = b @-> (P : Bool $ 0 -> Type $ 0) ->
  @b (_ => Type $ 0) (P true $ 1) (P true $ 0) ->
  @b (_ => Type $ 0) (P false $ 0) (P false $ 1) -> P b $ 1;
true = P => x => y => x;
false = P => x => y => y;

dup (b : Bool $ 1) : Bool $ 2 =
  b _ true false;

// erasable only
Bool : Type;
true : Bool 'G;
false : Bool 'G;


Bool = b @-> (P : Bool $ 0 -> Type $ 0) ->
  P true $ @b (_ => Grade) 1 0 ->
  P false $ @b (_ => Grade) 0 1 -> P b;
true = P => x => y => x;
false = P => x => y => y;

dup (b : Bool $ 1) : Bool $ 2 =
  b _ true false;

//
Unit = (0 A : Type) -> A -> A;
unit : Unit = A => x => x;

Bool_0 = (0 K : Type) -> ((0 A : Type) -> A -> K) -> K;
true : Bool_0 = K => k => k Unit unit;
false : Bool_0 = K => k => k Unit unit;

// closed
Closed A = <G>A $ G;

Bool = (A : Type) -> Closed A -> Closed A -> A;
true : Closed Bool = <G>(A => x => y => (_ = y<0>; x)) $ G;
false : Closed Bool = <G>(A => x => y => (_ = x<0>; y)) $ G;

dup (b : Bool $ 1) : Closed Bool =
  b (Closed Bool) true false;



case : (b : Bool_0) -> (A : Type) -> (<G>(A $ G) $ 1) ->

// inductive version
Bool_0 b = (
  (G_true, G_false) = b;
  (P : Bool_B -> Type) -> P true_b $ G_true -> P false $ G_false -> P b $ 1
);
true_0 : Bool_0 true_b = P => x => y => x;
false_0 : Bool_0 false_b = P => x => y => y;

// church version
Bool_B = (Grade, Grade);
true_b : Bool_B = (1, 0);
false_b : Bool_B = (0, 1);

Bool_0 = (
  G_true : Grade, G_false : Grade,
  (A : Type) -> A $ G_true -> A $ G_false -> A $ 1
);
true_0 : Bool_0 = (1, 0, A => x => y => x);
false_0 : Bool_0 = (0, 1, A => x => y => x);

//


Bool : Type;
true : Bool;
false : Bool;

Bool = b @-> (P : Bool $ 0 -> Type $ 0) ->
  @b (_ => Type $ 0) (P true $ 1) (P true $ 0) ->
  @b (_ => Type $ 0) (P false $ 0) (P false $ 1) -> P b $ 1;
true = P => x => y => x;
false = P => x => y => y;

dup (b : Bool $ 1) : Bool $ 2 =
  b _ true false;
@Bool =
  b @->
  (P : Bool $ 0 -> Type $ 0) ->
  P (b @=> P => (x : P b $ 1) => (y : P b $ 0) => x) $ b (_ => Grade) 1 0 ->
  P (b @=> P => (x : _ $ 0) => (y : P b $ 1) => x) $ b (_ => Grade) 0 1 ->
  P b $ 1;
true :
dup : ()
expected :
  P (b @=> P => (x : P b $ 1) => (y : P b $ 0) => x) $ 1 ->
  P (b @=> P => (x : _ $ 0) => (y : P b $ 1) => x) $ 0 ->
  P (b @=> P => (x : P b $ 1) => (y : P b $ 0) => x) $ 1
received :
  P (b @=> P => (x : P b $ 1) => (y : P b $ 0) => x) $ 1 ->
  P (b @=> P => (x : _ $ 0) => (y : P b $ 1) => x) $ 0 ->
  P (b @=> P => (x : P b $ 1) => (y : P b $ 0) => x) $ 1

```

## Monolith Graded CoC

```rust
Γ |- n $ 0 : Grade  Γ |- A $ 0 : Type
------------------------------------- // Rule
Γ |- t $ n : A $ 0

------------------------- // Type
() |- Type $ 0 : Type

----------------------- // Grade
() |- Grade $ 0 : Type

Γ $ 0 |- A : Type
----------------------------- // Var
Γ $ 0, x $ 1 : A |- x $ 1 : A

Γ |- A $ 0 : Type  Γ |- r $ 0 : Grade
Γ, x $ p : A |- B $ 0 : Type
------------------------------------- // Forall
Γ |- (x $ p : A) -(r)> B $ 0 : Type

Γ, x $ p : A |- t $ r : B
-------------------------------------------------- // Lambda
Γ |- (x $ p : A) =(r)> t $ 1 : (x $ p : A) -(r)> B

Γ |- f $ 1 : (x $ p : A) -(r)> B  ∆ |- a $ p : A
------------------------------------------------ // Apply
Γ, ∆ |- f a $ r : B[x := a]

Γ |- t $ r : A
---------------------- // Multiply
Γ $ n |- t $ r * n : A



Γ |- f $ 1 : (x $ p : A) -(r)> B  ∆ |- a $ p : A
------------------------------------------------ // Apply
Γ, ∆ |- f a $ r : B[x := a]


((x $ 1) =(0)> (x $ 1) x)


(x $ 1) => (
  f = (() => x $ 1);
  (f () $ 0)
)

f = (() => x) $ 1;


() -> (x $ 0) @=> x

// consumes 4 nat and produces 2 nat
(x $ 4 : Nat) -(2)> Nat

// closed terms can produce G
(G : Grade) =(G)> (x $ 1 : Nat) =(1)> x;


```

## Formal Linear

```rust

```

## Teika Syntax

```rust
Pat =
  | x // var
  | P : A // annot
  | !P // strict
  // TODO: optional
  | <P> // implicit
  | (P, ...) // tuple
  | { P; ... }// module

Term =
  | x // var
  // TODO: all patterns?
  | (x : A) -> B // explicit forall
  | <P>B // implicit forall
  | P => M // lambda
  | M N // explicit apply
  | M<N> // implicit apply
  // TODO: explain strict, like annot
  | !M // strict
  // TODO: optional / currying?
  | (x : A, ...) // exists
  | (P = M, ...) // tuple
  | { P : M; ... } // signature
  | { P = M; ... } // module


(x : T).a
T::a(x);

(A : Type <: )

(<A>, x) = (a : (<A>, x : A));
<S : Show> =


(b, <P>, x : P true, y : P false) -> P b
∀b. ∀P. P true → P false → P b
(b : _) -> (P : _) -> P true -> P true -> P b

Show = Σ(t : Type, x : T)
// lambda

// apply
M N
// strict apply
!M N
// curried apply
?M N


Fix = (x : FixX) -> ()
FixX = (G : Grade) -> ((x1 : Fix) -> (x : FixX) -> );
fix : Fix = x => (
  x
)
(G : Grade) ->
```

## Linear + Erasable System F

```rust
(Grade Kind) GK ::== | + | GK -> GK
(Grade)    G, H ::== | g | 0 | 1| λx : GK. G | G H | ∀g. G
(Type)     T, U ::== | A | T -> U          | ∀A. T       | T $ G   | ∀g. T
(Term)     M, N ::== | x | λx : T. M | M N | ΛA. M | M T | [M $ G] | ΛG. M | M G
(Context)  Γ, Δ ::== | • | Γ, x : T        | Γ, A        | Γ, g


λx : A.

Nat_GK = + -> (+ -> +) -> +;
zero_gk : Nat_GK = λz. λs. z.
succ_gk : Nat_GK -> Nat_GK = λn. λz. λs. s (n z s).

Λg. ΛA. λx : A $ g. x
Λg : Nat_GK. ΛA. λz : A. λs : A $ g. x

∀g . g

(λx : . x) : * -> *

// only weak existentials, can be done through lambdas
Bool === ∃l. ∃r. ∀A. A $ l -> A $ r -> A;
// erasables can be weakened
true : Bool === pack 1. pack 0. λx. λy. x;
false : Bool === pack 0. pack 1. λx. λy. y;

dup : Bool -> (Bool * Bool) === λb.
  unpack (l, (r, b)) = b in
  // closed terms can satisfy any grade
  b (Bool * Bool) [(true, true) $ l] [(false, false) $ r]



Closed A === ∀g. (A $ G);
Nat === ∀A. A -> Closed (A -> A) -> A;
zero : Nat === ΛA. λz. λs. (_ = s 0; z);
one : Nat === ΛA. λz. λs. s 1

Bool === ∀A. Closed A -> Closed A -> A;
// erasables can be weakened
true : Bool === ΛA. λx. λy. (_ = y 0; x);
false : Bool === ΛA. λx. λy. (_ = x 0; y);

dup : Bool -> Closed (Bool * Bool) ===
  // closed terms can satisfy any grade
  λb. b (Bool * Bool) (Λl. [(true, true) $ l]) (Λr. [(false, false) $ r])

```

## Context Dependency

```rust
Term =
  | Context
  | •
  | Γ, (x : A)
  | M[Γ]
  | Type
  | x
  | ∀(x : A). B
  | λ(x : A). M
  | M N;

• |- Γ : Context  Γ |- A : Type
-------------------------------
Γ |- M : A

----------------
• |- Type : Type

-------------------
• |- Context : Type

----------------
• |- ∅ : Context

Δ |- Γ : Context  Γ |- A : Type
-------------------------------
Δ |- Γ, (x : A) : Context

Γ |- Δ : Context  Γ ∘ Δ |- M : A
--------------------------------
Γ |- M[Δ] : A

• |- Γ x : A
------------
Γ |- x : A

• |- Γ 0 == A
-------------
Γ |- 0 : A

0 = (x : Type) => x

Γ ((x : Type) => x) == A

((x : Type) => x) A == A

• |- Γ 0 == (x : A) -> B  • |- Γ 1 == A
---------------------------------------
Γ |- 0 1 : B[x := A]

• |- Γ 1 == A
-------------
Γ |- 1 : A

Nat = _;

true = x => k => k (x => x) x;
false = x => k => k x ();

weak ctx v = ctx (S v);
xchg ctx v = ctx (S v)
drop ctx =


[1; 2; 3]
[2; 1; 3]
[(λx y k.k x y); 2; 1; 3]
[(λy k.k 2 y); 1; 3]
[(λk.k 2 1); 3]
[3; (λk.k 2 1);]

x => k =>
true = λ(λ0);
false = λλ0;

xchg ctx v =
  ctx
  | ctx, x =>
    (ctx
    | ctx, y => Some (ctx, y, x)
    | ctx => None)
  | ctx => None;

xchg ctx v =
  ctx
  | ctx, x =>
    (ctx
    | ctx, y => Some (ctx, y, x)
    | ctx => None)
  | ctx => None;

apply = x => y => (x y)[xchg];

Nat_0 = Type -> (Type -> Type) -> Type;
one_0 = x => s => s x;

Nat_1 n0 =

Context = (A : Type) -> A -> (Type -> A -> A) -> A;
empty : Context = A => acc => f => acc;
cons T ctx = A => acc => f => f T (ctx acc f);

rev ctx = ctx Context • (A => Γ => A . Γ);

empty = (Type, None);

T | Γ |- Δ : Context  Δ |- T : A
--------------------------------
T | Γ |- M[Δ] : A

(T, x : )
apply = (x : Type -> Type) => (y : Type) => (x y)[x => y => k => k y x];

apply = x => (x y)[x => k => k {} x];

ctx 0 = A;
ctx 1 = B;

xchg ctx 1 = ctx 0
xchg ctx 0 = ctx 1


Nat = Type -> (Type -> (Type -> Type) -> Type) -> (Type -> Type) -> Type;
zero = z => s => k => k z;
succ = n => z => s => k => s (r => n )
empty v = None
append ctx A v =
  v (Some A) (x => ctx (z => ))
()

(b : Bool) => M[b Context (•, (x : Bool)) (•, (y : Bool))]
```

## Dependent substitutions

```rust
// first-class binders?
Term =
  | Subst
  | [xchg]
  | [x : A]
  | M[Δ]
  | Type
  | x
  | ∀(x : A). B
  | λ(x : A). M
  | M N;

Γ |- A : Type
-------------
Γ |- M : A

----------------
• |- Type : Type

-----------------
• |- Subst : Type

--------------
• |- xchg : Subst

Γ |- A : Type
--------------------
Γ |- [x : A] : Subst

--------------
• |- xchg : Subst

Γ |- A : Type
-----------------------
Γ |- M[xchg] : Γ, x : A

Γ |- Δ : Subst  Δ Γ |- M : A
-----------------------------
Γ |- M[Δ] : A

// reflections

apply = x => y => (x y)[xchg];
apply = (x y)[y][x]

apply = x[y][x] y[y][x]

(b : Bool) =>
  M[b Subst [x : Bool] [y : Bool]][b Subst [y : Bool] [x : Bool]]

M[b Context [x : Bool] [y : Bool]]
b _ ((x : Bool) => M) ((y : Bool) => M)

M[b Context [x : Bool] [x : Int]]

b _ ((x : Bool) => M) ((x : Int) => M)


M[x : A]
```

## Ordered Lambda Calculus

```rust
Closed A = <G>A $ G;

swap = <A> => (x : A) => (y : A) =>
  <K> => (k : A -> A -> K) : K => k y x;
noop = <A> => (x : A) => (y : A) =>
  <K> => (k : A -> A -> K) : K => swap y x (x => y => swap x y k);

Bool =
  <A> -> Closed A -> Closed A -> Closed A;
true : Closed Bool =
  <G> (<A> => x => y => noop y x (x => y => ((_ : A $ 0) => x) (y<0>))) $ G;
false : Closed Bool =
  <G> (<A> => x => y => swap y x (y => x => ((_ : A $ 0) => y) (x<0>))) $ G;

dup (b : Bool) : Closed Bool = b Bool true false;

[x; y] |- term
[y; x] |- (y => x => term) y x

[a; b; x; c; d] |- term
[x; a; b; c; d] |-
(c => d => (d => c => x => b => a => term) d c x a b) c d


(d => c => x => b => a => term) d c


(d => c => x => b => a => term)
(c => d => (d => c => x => b => a => term)) x a b c d
x => y => y x

x => y => x y

x => y => (y => x => x y) y x

[x; y] body
x => (y => body)

((f lam a) b) c
f lam a b c
(x => y => k x y)


x ∈ Γ  x ∉ Δ
------------
Γ, Δ

------
x |- x

Γ, x |- M
----------
Γ |- λx. M

Γ |- M  Δ |- N
--------------
Γ, Δ |- M N


//
------
l |- 0

l |- x
----------------
l + 1 |- (1 + x)

l + 1 |- M
----------
l |- λ.M

l |- M  r |- N
--------------
Γ, Δ |- M N

λx.λx.x x

λx.x x


G ::== | g | 0 | S g | ∞


------
1 |- 0

n |- n
--------------
1 + n |- n

1 + Γ |- M
----------
Γ |- λM

Γ |- M  Γ + Δ |- N
------------------
Γ + Δ |- M N

λ λ (λ0) 1 2

Γ |- M  Δ |- N
--------------
Γ, Δ |- M N


<A>(x : A) => A

(x => M)(N)
M[x := N]


(x => x x) (x => x x)
(x x)[x := x => x x]
(x => x x) (x => x x)

(x => x x)(x => x x)
(x x)[x := x => x x]
(x => x x) (x => x x)

Promise<[A]>

0 0

λ 0 1

M |-
--------------------
M :: N :: [] |- @ :

Γ, x |- M
----------
Γ |- λx. M

Γ |- M  Δ |- N
--------------
Γ, Δ |- M N

```

## CPSify

```rust
Term =
  | Type

  | x
  | (x : A) -> B
  | (x : A) => M
  | M N
  | x @-> T
  | x @=> M
  | @M;

Value =
  | Grade
  | Type
  | x
  | (x : A) -> B
  | (x : A) => M;
Term =
  | ...Value
  | M V;


[f => x => m (x => f x)]
f => x => m (f x)

Id = <A>(x : A) -> A;
Bool = <A>(x : A, y : A) -> A;
not : (b : Bool) -> Bool = _;
if : (b : Bool) -> Bool = _;

call_if (if : (b : Bool) -> Bool) b = if b 1 0;

f : (id : <A>(x : A) -> A) -> Int
f id = id 1;

f : (id : (x : Int) -> Int) -> Int
f id = id 1;

left_or_right :  String -> String -> String -> String
left_or_right l r = (
  b = String.equal l r;
  b "abc";
);
left_or_right l r = (
  b = String.equal l r;
  b (y => "abc") (y => y);
)

b := (b_ptr, 0, null, null)
b := (b_ptr, 1, x,    null)

rec@fold f acc n =
  n
  | 0 => acc
  | S p => fold f (f acc) p;

mul = n => m => fold (acc => add acc m) 0 n;
mul = n => m =>
  (rec fold f acc n =>
  n
  | 0 => acc
  | S p => fold f (add acc m) p
  ) 0 n;
```

## Dynamic Range

```rust
TypeScript / Rust ->
Java / C / C++ / OCaml ->
Python / Go / Clojure / Elixir ->
Swift / Haskell / PHP / Ruby

Rust is affine / linear
Rust uses result for everything
TypeScript IO is fully tracked




const id = (x : Nat) => x;
const id = <A>(x : A) => x;
type Id<A> = A;
type T<B> = B extends true ? number : string;

id = (x : Nat) => x;
id = (A : Type) => (x : A) => x;
Id = (A : Type) => A;
T = (B : Bool) => b | true => Nat | false => String;

module type S = sig
  type t
  val x : t
end
type t = { x : int; }
type fcm = (module S)
type 'a obj = object val x : int end as 'a

S = {
  T : Type;
  x : T
};
t = { x : Int; };
fcm = S;
obj = R => ({ x : Int; ...R; })


if_b_then_int_else_string
  : (b : Bool) -> (x : b | true => Int | false => String) => b | true => Int | false => String
  = (b : Bool) => (x : b | true => Int | false => String) => x;


if_b_then_int_else_string = <A extends bool>(b : A extends true ? number : string, x : A) => x


f = (s : String & JSON.is_valid s == true) => s;

Color =
  | Red
  | Green
  | Blue;

f = (color : Color & color != Red) => _;
make = (n : Int & n >= 0 || n == -1) => n;


id = (A : Type) => (x : A) => (
  () = console.log(x);
  x
);

incr = (x : id Type Int) => x + 1;

incr = (x : Int) => x + 1;

main : IO ()
main = print "Hello World"

print : World -> String -> World;

main : World -> World
main world0 = (
  world1 = print world0 "Hello World";
  world2 = print world0 "Wrong";
  world2
)

Socket : {
  send : (sock : Socket, msg : String) -> Socket;
  close : (sock : Socket) -> ();
};

f = (s : Socket) -> (
  s1 = send(s, "a");
  close(s1);
)

double = (x : Nat $ 2) => x + x;
double = x => x + x;

id = (A : Type $ 0) => (x : A $ 1) => x;
dup = (x : Nat $ 2) => (x : Nat, x : Nat);


CoC
+ Self Types
+ Graded
+ Inference


read = (file : String) -> Eff [Read] (String);

read "hello.txt"

id = (x : Nat) => (
  () = @debug(x);
  x + 1
);
() = Fuzz.nat(x => x == id x);


Nat => number | bigint
Int => number | bigint


Int32 => number | 0
Int64 => [number, number] | bigint


id = (A : Type) => (x : A) => x
x = id 1;
y = id "a";


x = id 1;
y = id "a";


call_id = (id : (A : Type) -> (x : A) -> A) => id(1);

data@Option<A> =
  | Some(content : A)
  | None;

apply = x => y => (y => x => x y) x y;


ML = f -> f

read : (world : World, file : String) -> Result (World, String) Error;

read : (world : World, file : String) -[Error]> (World, String);
read : (file : String) -[IO]> String;

main : () -[IO]> ()
main () = (
  file = read("tuturu.txt");
  print(file);
);

List.map (x =>
  x.? > 0
  | true => Ok (x + 1)
  | false => Error "bad"
)

List.map (x =>
  x > 0
  | true => x + 1
  | false => throw (Error "bad")
)

main : Eff [IO; Error] ()

main : World -> World
main world = (
  file = read("tuturu.txt");
  print(file);
);




Term =
  | Type
  | (x : A) -> B
  | (x : A) => M
  | M N
  | x @-> T
  | x @=> M
  | @M;

Term =
  | Type
  | (x : A $ G) -> B
  | (x : A $ G) => M
  | M N
  | x @-> T
  | x @=> M
  | @M;

Unit = Unit @=> I =>
  u @-> (P : (u : Unit) -> Type) -> (x : I P (u @=> P => x => x)) -> I P u;


Fix = (x : Fix $ 0) -> Type;

f = ((x : Fix $ 0) => x x);

f = ((x : Fix $ 0) => x x);



False = False @=> f @->
  (P : ((f : False $ 0) -> Type) $ 0) -> P
(x : @f f) => _;





False = False @=>
  f @-> (P : ((f : False $ 0) -> Type) $ 0) -> P f

Γ |- M : (x : A $ n) -> B $ 1  Δ |- N : A $ n
---------------------------------------------
Γ, Δ |- M N $ 1

Fix = (x : Fix $ 1) -> Type;

T : (T $ 0, 1) @-> Type = T @=> @T;


x = (A : Type $ 0, 2) -> Type;
y = (A : Type $ 0, 2) => (x : A) -> A;

y = (A : Type $ 0, 2) => (x : A) -> A;

T : (T $ 0, 1) @-> (A : Type) -> Type = T @=> @T;

@(T @=> @T) === @(T @=> @T)

False = (False : Type $ 0) @=>
  f @-> (P : ((f : False $ 0) -> Type) $ 0) -> P f

T_x : (f $ 0, 1) @-(0)> (A : Type) -> A;

f : T_x = f @=> @f;
x = @f : (A : Type) -> A $ 0;


Fix = (x $ 0, 0 : Fix) -(0, 0)> ();
fix : Fix = (x $ 0, 0 : Fix) =(0, 0)> x x;
Fix = (x : Fix $ 0, 0) -> ();

Id = (A : Type $ 0) =(0)> A;

((x : Id Nat) => x);

False @-(0)> Type;

Unit = Unit @=(1)> I =>
  u @-(1)> (P $ 2 : (u $ 1 : @Unit I) -> Type) -(1)>
    (unit $ 1 : I P (u @=> P => x => x)) -(1)> I P u;


T_False = False @-(2)> (f $ 1 : f @-(1)> @False f) -(1)> Type;
False $ _G : T_False = False @=(2)> f =>
  (P : (G : Grade) (f $ 1 : f @-(1)> @False f) -> Type) -(1)> P f;

T_Unit = Unit @-(_)>
  (I : I @-> ) -(1)> Type;
Unit : T_Unit = Unit @=(_)> I =>
  u @-(_)> (P $ 2 : _) -(_)> ();

False = f @-(1)> @False f;

@(F @=(2)> (P $ 4) => (@F P, @F P));

F = F @=(1, 0)> @F

@(False @=(2)> (f $ 1 : f @-(1)> @False f) =>
  (P $ 1 : (f $ 1 : f @-(1)> @False f) -> Type) -(1)> P f)

(f $ 1 : f @-(1)> @False f) =>
  (P $ 1 : (f $ 1 : f @-(1)> @False f) -> Type) -(1)> P f

False = False @=(n)>
  f -(1)> (P $ 1 : (f $ 1 :) -> Type) -(1)>
(A $ 2 : Type) -> (x $ 1 : A) -> A

Fix = (x $ 2 : @Fix) -(0)> ();
fix = (x $ 2 : @Fix) =(0)> x x;

T_F = F @-(1)> (A : Type) -> Type;
F = F @=(1)> @F;
@(F @=(1)> @F);

Unit = Unit @=(1, 0)>
  u @-(1, 0)> (P $ (2, 0) : (u $ (1, 0) : Unit) -> Type) ->
    P (u @=(1, 0)>
      (P $ (2, 0) : (u $ (1, 0) : Unit) -> Type) =>
        (x : P u) => x
      ) -> P u;

fix = (x : Fix $ 0, 0) =(0)>
(A : Type $ 2, 0) -> (x : A $ 0, 1) -> A;
()
(A : Type) => (B : ) => (x : A) =>


Term =
  | Type
  | x
  | (x : A) -> B
  | (x : A) => M
  | M N;

(A : Type) -> A;

Context = List Type;


-------------
Γ |- A : Type

Ind (T : Type) =
  (P : Type -> Type) ->
  P Type ->
  ((A : Type) -> (B : (x : A) -> Type) ->
    P A -> (x : A) -> P (B x) -> P ((x : A) -> B x)) ->
  P T;

x : Ind Type = P => p_type => p_forall => p_type;

T_Id = (A : Type) -> Type;
T_Id_ind : Ind T_Id = P => p_type => p_forall =>
  p_forall Type Type p_type p_type;


(A : Type) : Ind A = P => p_type => p_var => p_var A;

x => (A : Type) => (B : Type) => _

Dyn =
  (A : Type, x : A);

Ind {A} (T : A) =
  | Univ : Ind Type;
  | Forall
      A (a_dyn : Ind A)
      B (b_dyn : (x : A) -> Ind (B x))
    : Dyn ((x : A) -> B x);
  | Lambda
      A (a_dyn : Ind A)
      B (b_dyn : (x : A) -> Ind (B x))
      M (m_dyn : (x : A) -> Ind (M x))
    : Dyn ((x : A) => (M x : B x));

Dyn = (A : Type, T : A, ind : Ind T);


False @-(0)> (f : f @-(0)> False f) -> Type;

----------------------
Γ |- x @-(0)> T : Type

----------------------------
Γ |- x @=(0)> M : x @-(0)> T


Γ, x : x @-(l)> T : T |- Type
-----------------------------
Γ |- x @-(1 + l)> T : Type


Γ, x : x @-(l)> T : T |- Type
-----------------------------
Γ |- x @-(d)> T : Type

(w : Type $ ω) -> 1

Γ, x : x @-(l)> T : T |- Type
-----------------------------
Γ |- (d, False) : Type $ ω

(d, False) @-> (f : (d, f) @-> @False d f) -> Type;

Γ, x : x @-(l)> T : M |- T[x := x @=(l)> M]
-------------------------------------------
Γ |- x @=(1 + l)> M : x @-(1 + l)> T





----------------------
Γ |- x @=(0)> M : x @-(l)

-------------------
Γ |- x @-> T : Type

False = False @=>
  f @-> (P : @False -> Type) -> P f;

Γ, x : x @-(l)> T : T |- Type
-----------------------------
Γ |- x @-(l)> T : Type

Term =
  | Grade
  | Type
  | (x : A) -> B
  | (x : A) => M
  | M N;

Type : Type $ 0
(x : A, y)
Type $ 2
(x : ?)

Type : Type $ 0

Unit = Unit @=> (G : Grade) ->
  u @-(G)> (P : @Unit -> Type) -> P (u @=(G)> P => x => x) -> P u;

x @-(G)> ()
(x : ((G : Grade) -(G)> Nat) $ 1)

(x : ((G : Grade) -(G)> Nat G) $ 1)
(G : Grade) ->

Γ |- A : Type $ n   Γ |- B : Type $ 1
-------------------------------------
Γ |- (x : A) -> B :


Γ |- x :
-------------------------------------
Γ |- x @-> T :

False @-> (f : f @-> @False f) -> Type;

False_C = (A : Type) -> A;
False_0 = (f_c : False_C, i : (P : False_C -> Type) -> P f_c);
False_1 = (f_0 : False_0, i : (P : False_0 -> Type) -> P f_0);
False_2 = (f_1 : False_1, i : (P : False_1 -> Type) -> P f_1);


Γ, x : x @-> (G : Grade) -> T, G : Grade |- T : Type $ G
--------------------------------------------------------
Γ |- x @-> (G : Grade) -> T :

Γ, x : x @-> (G : Grade) -> T, G : Grade |- T : Type $ G
--------------------------------------------------------
Γ |- x @-> (G : Grade) -> T : Type $ ω

False @-> (G : Grade) -> (f : f @-> (G : Grade) -> @False G f) -> Type;

False @-(1)> (f : f @-(1)> @False f) -> Type;

False = (G, False) @=> ((G, f) @-> (P : @False G -> Type) -> P f) G;

@False 0 === Base
@False 1 === f @-> (P : @False 0 -> Type) -> P f



T_False = False @-> (f : f @-> @False f) -> Type;
T_False_C = Type;
T_False_0 = (f : False_C) -> Type;




@False == f @-(1)>
  (P : @(False @=(0)> f @-(1)> (P : @False -> Type) -> P f) -> Type) ->
  P f

False_0 f_c = (P : False_C -> Type) -> P f_c;
False_1 f_0 = (P : False_C -> Type) -> P f_0;

Bool_C = (A : Type) -> A -> A -> A;
true_c : Bool_C = A => x => y => x;
false_c : Bool_C = A => x => y => y;

Bool_0 b_c = (P : Bool_C -> Type) -> P true_c -> P false_c -> P b_c;


Γ |- M[G := 0][x := (G, x) @=> M] : T[G := 0][x := (G, x) @=> M]

Γ |- M[G := 0][x := (G, x) @=> M] : T[G := 0][x := (G, x) @=> M]
----------------------------------------------------------------
Γ |- (G, x) @=> M : (G, x) @-> T

(G, False) @=> (G, f) @-> (P : False@G -> Type) -> P f;


(G, f) @-> (P : False@G -> Type) -> P f

(G, f) @-> (P : ((G, f) @-> (P : False@G -> Type) -> P f) -> Type) -> P f;


f @-(G)> (
  b : (A : Type) -> A,
  i : (P : False@G -> Type) -> P f
);

f @-(0)> (
  b : (A : Type) -> A,
  i : (P : False@G -> Type) -> P f
) === (A : Type) -> A

f @-(1)> (
  b : (A : Type) -> A,
  i : (P : False@G -> Type) -> P f
) === (P : False@G -> Type) -> P f

f @-> (P : False@G -> Type) -> P f


Γ, x : x @-(G)> T |- T : Type
-----------------------------
Γ |- x @-(1 + G)> T : Type

False @-(0)> (f : f @-(1)> @False f) -> Type



Γ, x : x @-(G)> T |- T : Type
-----------------------------
Γ |- x @-(1 + G)> T : Type

Unit @=(1)> u @-> (P : @Unit -> Type) ->
  P (u @=> P => x => x) -> P u;


Γ, x : x @-(G)> T |- T : Type
-----------------------------
Γ |- x @-(1 + G)> T : Type

Γ |- M : x @-(0)> T
------------------------- // Lower Base
Γ |- lower M : Unit

Γ |- M : x @-(1 + G)> T
------------------------- // Lower Succ
Γ |- lower M : x @-(G)> T

------------------------------------------ // Lower Base Compute
Γ |- lower (x @=(0)> M) === unit

------------------------------------------ // Lower Succ Compute
Γ |- lower (x @=(1 + G)> M) === x @=(G)> M

Γ |- M[x := lower M] : T[x := lower M]
-------------------------------------- // Fix Succ
Γ |- x @=(G)> M : x @-(G)> T

Γ |- M : x @-(0)> T
---------------------- // Unroll
Γ |- @M : T[x := unit]

Γ |- M : x @-(G)> T
------------------------- // Unroll Typing
Γ |- @M : T[x := lower M]

Γ |- M : x @-(G)> T
------------------------- // Unroll Compute
Γ |- @M : T[x := lower M]

// extensions
Γ, x : lower (x @-(G)> T) |- M : T[x := lower M]
------------------------------------------------ // Fix Weak
Γ |- x @=(G)> M : x @-(G)> T



Γ |- M : (x $ n) @-> T $ 1 + n
------------------------------ // Unroll
Γ |- @M : T[x := M] $ 1

Γ |- M : (x $ n) @-> T $ 1 + n
--------------------------------------------- // Unroll Compute
Γ |- @((x $ n) @=> M) : M[x := (x $ n) @=> M]


Γ, x : (x $ n) @-> T |- M : T[x := (x $ n) @=> M]
-------------------------------------------------
Γ |- (x $ n) @=> M : (x $ n) @-> T



(False $ 1) @-> Type;

(False $ 0) @=> (A : Type) -> A;
(False $ 1 + G) @=> f @-> (P : @False -> Type) -> P f;

(False $ 1) @=> f @-> (P : @False -> Type) -> P f
(False $ 1) @=> f @-> (P : @False -> Type) -> P f;



case : (b : Bool) -> (A : Type) -> A -> A -> A;
Bool = (A : Type) -> A -> A -> A;
true : Bool = A => x => y => x;
false : Bool = A => x => y => y;


ind : (b : Bool) -> (P : Bool -> Type) -> P true -> P false -> P b;
Bool = (P : Bool -> Type) -> P true -> P false -> P b;

Bool : Type;
true : Bool;
false : Bool;

Bool = b @-> (P : Bool -> Type) -> P true -> P false -> P b;
true = P => x => y => x;
false = P => x => y => y;

Γ, x : x @-> T |- T : Type
-------------------------- // Self
Γ |- x @-> T : Type

Γ, x : x @-> T |- M : T[x := x @=> M]
------------------------------------- // Fix
Γ |- x @=> M : x @-> T


(x $ 2) => x + x

x0 => x1 => x0 + x1

(x => x x) (x => x x)

Either A B =
  | Left (content : A)
  | Right (content : B);

Either A B =
  (tag : Bool, content : tag Type A B);

(either : Either Int String) => (
  (tag, content : tag Type Int String) = either;

  ind tag (b => (b Type Int String) -> String)
    (content => Int.to_string content) (content => content) content
)


(False $ 0) @=> (A : Type) -> A;
(False $ 1 + G) @=> f @-> (P : @False -> Type) -> P f;


(Loop $ 0) @=> Unit
(Loop $ 1) @=> () -> @Loop

(Loop $ 1) @-> Type;

(Loop $ 2) @=> () -> @Loop;

@((Loop $ 2) @=> () -> @Loop)

(Loop $ 2) @=> () -> () -> Unit;

(Loop $ 2) @=> () -> @((Loop $ 1) @=> () -> @Loop)


Γ |- T : Type // try ((x $ 0) @-> T) $ 0
--------------------------------------------- // Fix Succ
Γ |- (x $ 0) @-> T : (x $ 0) @-> T

Γ, x : (x $ G) @-> T |- T : Type
--------------------------------------------- // Fix Succ
Γ |- (x $ 1 + G) @-> M : (x $ 1 + G) @-> T


Γ |- M : T // try ((x $ 0) @=> M) $ 0
------------------------------------- // Fix Base
Γ |- (x $ 0) @=> M : (x $ 0) @-> T

Γ, x : (x $ G) @-> T |- M : T[(x $ G) @=> M]
--------------------------------------------- // Fix Succ
Γ |- (x $ 1 + G) @=> M : (x $ 1 + G) @-> T


Γ, x : (x $ G) @-> T |- T : Type
-------------------------------- // Self Succ
Γ |- (x $ 1 + G) @-> T : Type


False : Type $ 0

(False $ 0) @=> Unit;
(False $ 1) @=> (f $ 1) @-> (P : @False -> Type) -> P f;
(False $ 2) @=> (f $ 1) @-> (P : @False -> Type) -> P f

False @-> (f : f @-> @False f) -> Type;


Γ |- Z : Type
------------------------------ // Self Base
Γ |- @self(0, Z). S : Type $ 0

Γ, x : @self(G, Z). S |- S : Type
-------------------------------------- // Self Succ
Γ |- @self(1 + G, Z). S : Type $ 1 + G


@self(0, Type).
  (f : @self(1, ). f @-> @False f)) -> Type;

@fix(
  (A : Type) -> Type,
  False =>
);

∀G. T [G]

(G : Grade)


zero = z => z;
one = s1 => z => s1 z;
two = s2 => s1 => z => s2 (s1 z);
three = s3 => s1 => z => s3 (s2 (s1 z));


let Closed A = ∀(G : Grade). A [G]
// closed as it is unknown how many copies it will be needed(0..1)
let Bool : Type = ∀(A : Type). Closed A -> Closed A -> Closed A
let true : Closed Bool =
  // safe to ignore y as it is used 0 times
  ΛG. [ΛA. fun x y -> let [y] = y 0 in x]
let false : Closed Bool =
  ΛG. [ΛA. fun x y -> let [y] = x 0 in y]

// same applies to nats but (0..n)
let Nat : Type = ∀(A : Type). Closed A -> Closed (A -> A) -> Closed A;
let zero : Closed Nat =
  ΛG. [ΛA. fun z s -> let [s] = s 0 in z];
let one : Closed Nat =
  ΛG. [ΛA. fun z s -> let [s] = s 1 in s z];
let two : Closed Nat =
  ΛG. [ΛA. fun z s -> let [s] = s 2 in s (s z)];




False @-> (f : f @-> @False f) -> Type;

T_False_0 = Type;
T_False_1 (False_0 : T_False_0) = (f_0 : False_0) -> Type;
T_False_2 (False_0 : T_False_0) (False_1 : T_False_1 False_0) =
  (f_0 : False_0) -> (f : False_1 f_0) -> Type;

False_0 = (A : Type) -> A;
False_1 (f_0 : False_0) = (P : False_0 -> Type) -> P f_0;
False_2 (f_0 : False_0) (f_1 : False_1 f_0) =
  (P : (f_0 : False_0) -> False_1 f_0 -> Type) -> P f_0 f_1;

False_0 = (A : Type) -> A;
False_1 (f_0 : False_0) = (P : False_0 -> Type) -> P f_0;
False_2 (f_0 : False_0) (f_1 : False_1 f_0) =
  (P : (f_0 : False_0) -> False_1 f_0 -> Type) -> P f_0 f_1;

// something like the following
False n = (P : n Type Type _) -> n Type P _;


False_1 = @fix(2,
  (A : Type) -> A,
  False. (f : False) => (P : False -> Type) -> P f
);

False_1 = @fix(G,
  (A : Type) -> A,
  G -> False -> @self(G, False,
    G -> f -> (P : False G -> Type) -> P f)
);

False G = @fix(G,
  (A : Type) -> A,
  G -> False -> @self(G, False,
    G -> f -> (P : False G -> Type) -> P f)
);

False = @fix(1,
  (A : Type) -> A,
  G -> False -> @self(1 + G, False,
    G -> f -> (P : False G -> Type) -> P f)
);
False = @self(1, (A : Type) -> A,
    G -> f -> (P : False G -> Type) -> P f)


Unit = @fix(1,
  (A : Type) -> A -> A,
  G -> Unit -> @self(1 + G, Unit,
    G -> u -> (P : Unit G -> Type) -> P unit -> P u)
);

Unit =
  @self(1, (A : Type) -> A -> A,
    G -> u -> (P : Unit G -> Type) -> P unit -> P u)
unit = @fix(1, A => x => x,
  G -> u -> P => x => x);




T_False G = @self(G, G => False => (f : False G) -> Type);
False G = @fix(G, G => False => (f : False G) => (P : False G -> Type) -> P f);

T_False G = @self(G, G => False => (f : False G) -> Type);

T_False_1 = @self(1, Type, False. (f : False) -> Type);

False_1 = @fix(1, (A : Type) -> A, False. f => (P : False -> Type) -> P f);


T_Unit_1 = @self(1, Type, Unit. (f : False) -> Type);
@self(1, Type, False. (f : False) -> Type);


Γ |- Z : Type
--------------------------------- // Self Base
Γ |- @self(0, Z, x. S) : Type $ 0

Γ, x : @self(G, Z). S |- S : Type
----------------------------------------- // Self Succ
Γ |- @self(1 + G, Z, x. S) : Type $ 1 + G

@self(1, Type, False. (f : False) -> Type);


@fix(False, f) @=> (P : False -> Type) -> P
@self(0, Type),


False @-> (f : False )
@self(1, Type, G => False => (f : False G) -> Type)

Unit = (P : W Type) -> I P -> S P;

Term =
  | Grade
  | Type
  | (x : A $ n) -(m)> B
  | (x : A $ n) =(m)> M
  | M N;

case : (b : Bool) -> (A : Type) -> A -> A -> A;
Bool = (A : Type) -> A -> A -> A;

ind : (b : Bool) -> (P : Bool -> Type) -> P true -> P false -> P b;
Bool = b @-> (P : Bool -> Type) -> P true -> P false -> P b;

(b : Bool) => @b : (P : Bool -> Type) -> P true -> P false -> P b


Γ, x : x @-> T |- T : Type
--------------------------
Γ |- x @-> T : Type

Γ |- M : x @-> T
-------------------
Γ |- @M : T[x := M]


False @-> (f : f @-> False f) -> Type;


Γ |- G : Grade  Γ |- Z : Type
Γ, G : Grade, False : @self(G, Z, G => False => S) |- S : Type
--------------------------------------------------------------
Γ |- @self(G, Z, G => False => S) : Type

Γ |- Z : Type
--------------------------------------------
Γ |- @self(0, Z, G => False => S) : Type

Γ, False : @self(d, Z, G => False => S) |- S[G := d] : Type
-----------------------------------------------------------
Γ |- @self(1 + d, Z, G => False => S) : Type



@self(0, Type, G => False =>
  (f : @self(G, False_B, G => f => False G f)) -> Type);


@self(1, Type, G => False =>
  (f : @self(G, False_B, G => f => False G f)) -> Type);

(False : ) =>
  (f : @self(1, False_B, G => f => False G f)) -> Type

@self(2, Type, G => False =>
  (f : @self(G, False_B, G => f => False G f)) -> Type);


@fix(G, False_B, G => False =>
  f => (P : False G f -> Type) -> P f)

@self(0, Type, G => False => Type);

@fix(1, False_B, G => False =>
  @self(G, False_B, G => f => (P : False@G -> Type) -> P f))


False = @fix(False_B, G => False =>
  @self(False_B, G => f => (P : False@G -> Type) -> P f)@G);

False_0 = False@0;
False_0 = False_B;
False_1 = @self(False_B, G => f => (P : False@G -> Type) -> P f)@1;

Unit_B = (A : Type) -> A -> A;
unit_b : Unit_B = A => x => x;

Unit G = @fix(Unit_B, G => Unit =>
  @self(Unit_B, G => u => (P : Unit@G -> Type) ->
    P (@fix(unit_b, G => u => P => x => x)@G) -> P u)@G)@G;
Unit_0 = Unit_B;
unit_0 = unit_b;
Unit_1 = @self(Unit_B, G => u => (P : Unit@G -> Type) ->
    P (@fix(unit_b, G => u => P => x => x)@G) -> P u)@1;
unit_1 : Unit_1 =
  @fix(unit_b, G => u => P => (x : P u) => x)@1;

@unit_1 :
  (P : Unit@0 -> Type) ->
    P (@fix(unit_b, G => u => P => x => x)@0) -> P (lower unit_1);


Γ |- Z : Type
--------------------------------------------
Γ |- @self(0, Z, G => False => S) : Type

Γ, False : @self(d, Z, G => False => S) |- S[G := d] : Type
-----------------------------------------------------------
Γ |- @self(1 + d, G => False => S) : Type

@self(1, False_B, G => f => (P : False@G -> Type) -> P f)

Γ |- Z : Type
--------------------------------------------
Γ |- @self(0, Z, G => False => S) : Type

f => (P : False 0 f -> Type) -> P f

@fix(1, False_B, G => False =>
  f => (P : False G f -> Type) -> P f)


f => (P : () G f -> Type) -> P f


False_B = (A : Type) -> A;
T_False G = @self(G, Type, G => False =>
  (f : @self(G, False_B, G => f => False G f)) -> Type);
False G = @fix(G, False_B, G => False => f =>
  (P : @self(G, False_B, G => f => False G f) -> Type) -> P f);

: @self(G, Type, G => False => (f : False G) -> Type)
= @fix(G, False_B, G => False => );

: @self(G, Type, G => False => _)
= @fix(G, (A : Type) -> A, False => _)

T_False_0 = Type;
T_False_1 (False_0 : T_False_0) = (f_0 : False_0) -> Type;
T_False_2 (False_0 : T_False_0) (False_1 : T_False_1 False_0) =
  (f_0 : False_0) -> (f : False_1 f_0) -> Type;

False_0 = (A : Type) -> A;
False_1 (f_0 : False_0) = (P : False_0 -> Type) -> P f_0;
False_2 (f_0 : False_0) (f_1 : False_1 f_0) =
  (P : (f_0 : False_0) -> False_1 f_0 -> Type) -> P f_0 f_1;


Γ |- G : Grade  Γ |- A : Type
Γ, g : Grade, x : @self.lower(@self(G, A, g => x => B))
  |- B : Type
-------------------------------------------------------
Γ |- @self(G, A, g => x => B) : Type


Γ |- M : A
Γ, g : Grade, x : @self.lower(@self(G, A, g => x => B))
  |- N : B[x := @fix.lower(@fix(G, M, g => x => N))]
-------------------------------------------------------
Γ |- @fix(G, M, g => x => N) : @self(G, A, g => x => B)

Γ |- M : @self(G, A, g => x => B)
---------------------------------
Γ |- @M : B[x := @fix.lower(M)]


Γ |- G : Grade
Γ, g : Grade, x : @self.lower(G, g => x => T) |- T : Type
---------------------------------------------------------
Γ |- @self(G, g => x => T) : Type

Γ, g : Grade, x : @self.lower(G, g => x => T)
  |- M : T[x := @fix.lower(@fix(G, g => x => M))]
----------------------------------------------------
Γ |- @fix(G, g => x => M) : @self(G, g => x => T)

Γ |- M : @self(G, g => x => T)
-------------------------------
Γ |- @M : T[x := @fix.lower(M)]



False @-> (f : f @-> @False f) -> Type;

@self(G, G => False => (f : @self(G, G => f => G@False f)) -> Type);


(False : @self(0, G => False => (f : @self(G, G => f => G@False f)) -> Type)) =>
  (f : @self(0, G => f => G@False f)) -> Type;

(False : Unit) => (f : Unit) -> Type
@self(0, G => False => (f : @self(G, G => f => G@False f)) -> Type)
@self(1, G => False => (f : @self(G, G => f => G@False f)) -> Type)

T_False G = @self(G, G => False => (f : @self(G, G => f => G@False f)) -> Type);

False : @self.lower(G, G => False => (f : @self(G, G => f => G@False f)) -> Type);
f : @self.lower(G, G => f => G@False f)
False G = @fix(G, G => False => f =>
  (P : (f : @self(G, G => f => G@False f))) -> P f);


T = (A : Type $ 0) -> A -> A;
T = (A : Type $ (2, 0)) -> A -> A;
T = (A : Type $ (2, 1)) -> A -> A;
Fix = Fix @=> (x : Fix $ 0) -(0)> (A : Type) -> A;

TFix = Fix @-> Type;
Fix = Fix @=> (x : Fix $ 0) -(0)> (A : Type) -> A


(x : Fix $ (0, 0)) =(0)> x x;


fix : Fix = x => x x;


x : False $ 0 = fix fix;

sock = (socket : Socket$) => ();

Bool = (A : Type) -> A -> A -> A;
true : Bool = A => x => y => x;
false : Bool = A => x => y => y;

not : Bool -> Bool = b => b Bool false true;

Eq A x y = (P : A -> Type) -> P x -> P y;
refl A x : Eq A x x = P => x => x;


make = (l : Int & l >= 0) => _;

id
  : <A : Type>(x : A) -> A
  = <A : Type> => (x : A) => x;

id_string : (x : String) -> String = id<String>;
1 >= 0
| Some one_gte_zero => make (1 & one_gte_zero)
| None

true_eq_true : Eq Bool true true = refl _ _;

true_eq_not_false : Eq Bool true (not false) = refl _ _;
true_eq_not_true : Eq Bool true (not true) = refl _ _;

b_neq_not_b : Eq Bool true (not false) = _;

if_pred_then_string_else_int
  : (pred : Bool) -> (x : pred Type Int String) -> pred Type Int String
  = (pred : Bool) => (x : pred Type Int String) => x;


f : (x : Int) -> Int
  = if_pred_then_string_else_int true

g : (x : String) -> String
  = if_pred_then_string_else_int false


Fix = Fix @=> (x : Fix $ (1, 0)) -(0)> (A : Type) -> A

(x : Fix $ (1, 0)) =(0)> x x;

(x : Fix $ 0) -> x x
(f : False $ 0) => (x : P f) =>

---------------
: t : A $ 0


Fix = Fix @=> (x : Fix $ (1, 0)) -(0)> (A : Type) -> A
(x : Fix $ (1, 0)) =(0)> x x;


False = (False $ (2, 0)) @=(0)> (f : (f $ (1, 0)) @-> @False f) =>
  (P : (f $ (1, 0)) @-> @False f) -> P f;

False = (False $ (2, 0)) @=(0)> (f : (f $ (_, 0)) @-> @False f) =>
  (P : (f $ (_, 0)) @-> @False f) -> P f;

T_T = (T $ (1, 0)) @=(0)> (A : Type) -> A;

T : T_T = T @=> @T;


Type : Type

A : Type 0 = Int;
A : Type 1 = Type 0;

Type n : Type (n + 1)

double = (x : Nat $ 2) => x + x;
double = x => x + x;

case : (b : Bool) -> (P : Bool -> Type) ->
  P true -> P false -> P b;

b
| true => ()
| false => ();

("a" : Dyn)

if "a" then 1 else 2
x : Int = 1;

false : (A : Type) -> A = _;
id : (A : Type) -> A -> A = _;

incr : Int -> Int = _;

incr_x_is_plus_one : (x : Nat) -> incr x == x + 1 = _;

may_loop_forever : Int ->! Int = _;


double = (x : Nat $ 2) => x + x;
quadruple = x => double (double x);


false : ((A : Type) -> A) $ 0 = _;

b
| true => ()
| false => ();

case : (b : Bool) -> (P : Bool -> Type) ->
  P true -> P false -> P b;


Not (T) = (A <: T) -> A;
Theta = (A <: Top) -> Not ((B <: A) -> Not B);
f = (A0 <: Theta) => (A0 <: (A1 <: A0) -> Not A1);



Not T = (A <: T) -> A;
Theta = (A <: Top) -> Not ((B <: A) -> Not B);
f = (A0 <: Theta) => (A0 <: (A1 <: A0) -> Not A1);


(A : *) => (x : A) => x;
double = (x $ (0, 2)) => x + x;n


Not T = (A <: T) -> A;
Theta = (A <: Top) -> Not ((B <: A) -> Not B);
f = (A0 <: Theta) => (A0 <: (A1 <: A0) -> Not A1);

context = [A0 <: Theta];
received :: A0;
expected :: (A1 <: A0) -> Not A1;

context = [A0 <: Theta];
received :: (A1 <: Top) -> Not ((A2 <: A1) -> Not A2);
expected :: (A1 <: A0) -> Not A1;

context = [A0 <: Theta; A1 <: A0];
received :: Not ((A2 <: A1) -> Not A2);
expected :: Not A1;

context = [A0 <: Theta; A1 <: A0];
received :: (A2 <: Top) -> Not ((A3 <: A2) -> Not A3);
expected :: (A2 <: A1) -> Not A2;


Type : Type

Term =
  | Var (x : String)
  | Lam (body : Term -> Term);

Ind (A : Type) (x : A) =
  | Univ : Ind Type Type
  | Forall
      A (A_dyn : Ind Type A)
      B (B_dyn : (x : A) -> Ind A x -> Ind Type (B x))
    : Ind Type ((x : A) -> B x)
  | Lambda
      A (A_dyn : Ind Type A)
      B (B_dyn : (x : A) -> Ind A x -> Ind Type (B x))
      M (M_dyn : (x : A) -> Ind A x -> Ind (B x) (M x))
    : Ind ((x : A) -> B x) ((x : A) => M x);

Dyn = (A : Type, x : A, Ind A x)

uip : (A : Type) -> (x : A) -> (a : x == x) -> a == refl = _;


Impredicative Universe + Negative Recursin + Subtyping = _;


(b : Bool $ 1 ~ 2) -> (P : Bool $ 0 ~ 2 -> Type) ->
  P true $ 1 ~ 0 -> P false $ 1 ~ 0 -> P b;

(x : Nat $ 0 ~ 2) -(0)> Nat;

((x $ 0 ~ 2) =(0)> x + x)

((x $ 2 ~ 2) => x + x)

id : 'A -> 'A = x => x;

id : <A>(x : A) -> A = x =>


T = Int $ 1 ~ 0;


(x : A $ n ~ m) =($ 0 ~ 0)> x


Fix = (x : Fix $ 0 ~ 0) -(0)> (A : False) -> A;
fix = (x : Fix $ 0 ~ 0) =(0)> x x;

false : ((A : False) -> A) $ 0 = fix fix;


Fix = (x : Fix) -> (A : False) -> A;
fix : Fix = x => x x;


Γ |- M : (x $ n ~ m) @-> T $ 1 + n ~ 1 + m
------------------------------------------
Γ |- @M $ 1 ~ ?


(F $ n ~ m) @=> @F



T_False G = @self(G, G => False => (f : @self(G, G => f => G@False f)) -> Type);

False : @self.lower(G, G => False => (f : @self(G, G => f => G@False f)) -> Type);
f : @self.lower(G, G => f => G@False f)
False G = @fix(G, G => False => f =>
  (P : (f : @self(G, G => f => G@False f))) -> P f);

False = False @=> f @-(1)> (P : False -> Type) -> P f;

False @-(0)> Type;

False = (False $ 0 ~ 1) @=(0)> (f $ 0 ~ 1) @-(1)>
  (P : False $ 0 ~ 1 -(0)> Type) -> P f;

Unit = (Unit $ 0 ~ ) @=(0)> (u $ 0 ~ 1) @-(1)>
  (P : Unit $ 0 ~ 1 -(0)> Type) -> P unit -> P u;

False = (False $ 0 ~ 1) @=(0)> (f $ 0 ~ 1) @-(1)>
  (P : False $ 0 ~ 1 -(0)> Type) -> P f;

(F $ 0 ~ 1) @=(0)> @F

False = (False $ 0 ~ 1) @=(0)> (f $ 0 ~ 1) @-(1)>
  (P : False $ 0 ~ 1 -(0)> Type) -> P f;

T = (f $ 0 ~ 1) @-> (P : False $ 0 ~ 1 -> Type) -> P f;
T = (u $ 0 ~ 1) @->
  (P : @Unit $ 0 ~ 1 -> Type) -> P unit -> P u;


Unit = (A : Type) -> A -> A;
unit : Unit = A => x => x;

Unit_0 : Type = (u : Unit_B ~ 0) @-> (P : Unit -> Type) -> P unit -> P u;
unit_0 : Unit_0 = u @=> P => x => x;

Unit_1 = (u : Unit_0 ~ 1) @-> (P : Unit_0 ~ 1 -> Type) -> P unit_0 -> P u;
unit_1 : Unit_1 = u @=> P => x => x;

Unit_2 = (u : Unit_1 ~ 1) @-> (P : Unit_1 ~ 1 -> Type) -> P unit_1 -> P u;
unit_2 : Unit_2 = u @=> = P => x => x;

Γ |- A : Type  Γ, x : A |- B : Type
-----------------------------------
Γ |- (x : A) @-> B : Type

Γ |- A : Type  Γ, x : A |- M : T[]
-----------------------------------
Γ |- (x : A) @=> M : (x : A) @-> T

Γ |- M : (x ~ n) @-> T $ 2 ~ 1
------------------------------------
Γ |- @M : [x := ] $ 1

False_0 = (f ~ 0) @-> (A : Type $ 0) -> A;
False_1 = (f ~ 1) @-> (P : False_0 -> Type) -> P f;
False_2 = (f ~ 1) @-> (P : False_1 -> Type) -> P f;
False_3 = (f ~ 1) @-> (P : False_2 -> Type) -> P f;

@fix(1, (G ~ _) => (False ~ G) =>
  (f $ G) @-(1)> (P : False@G $ 0 ~ G -> Type) -> P f);

False : Unit $ 0 ~ 0

(f $ 0 ~ 0) @-(1)>
  (P : False@0 $ 0 ~ 0 -> Type) -> P f

False @-> (f : f @-> @False f) -> Type;

(False $ 0 ~ 0) @-> (f : (f ~ 0) @-> @False f) -> Type;


```

## Why CoC?

```rust
incr = (x : Int) => x + 1;

id
  : (A : Type) -> (x : A) -> A
  = (A : Type) => (x : A) => x;

id = id ((A : Type) -> (x : A) -> A) id;

Bool = (A : Type) -> A -> A -> A;
true : Bool = A => x => y => x;
false : Bool = A => x => y => y;

int_or_string
  : (pred : Bool) -> (x : pred Type Int String) -> pred Type Int String
  = (pred : Bool) => (x : pred Type Int String) => x;

x
  : (x : Int) -> Int
  = int_or_string true;

y
  : (x : String) -> String
  = int_or_string false;

mergesort == bubblesort

(A : Type) -> (l : List A) -> mergesort l == bubblesort l;

uip : (A : Type) -> (x : A) -> (eq : x == x) -> eq == refl = _;


Type 0
Type 1
Type 2

(x : Int $ 2) => x + x;
(x0 : Int) => (x1 : Int) => x0 + x1;
(x : Nat 2) => x + x;
(x0 : Nat) => (x1 : I_nat) => _;

(x : A $ 4) => (x : Nat $ 4 = 1; x)


incr = (x : Int) => x + 1;

User =
  | Anonymous({ name : string })
  | Registered({ id : int; name : string });

is_registered = user =>
  user
  | Anonymous(_) => false
  | Registered(_) => true;

name_of_registered_user = (user : User & is_registered(user)) =>
  user
  | Registered { id; name } => Registered { id; name }

(x
x => M
M N)

true = x => y => x;
false = x => y => y;

Type : Type;

Fix = Fix $ 0 -(0)> ();
(x : Fix $ 0) =(0)> x x;


id = (A : Type) => (x : A) => (
  @debug(x)
);

find_user : (id : Nat) -> Eff [DB.read] User;

x = @debug.db(find_user(1));

f = id(x => x);

User = {
  parents : List User;
};

Exist<A> = {
  x : A;
};

_A `unify` Int

id
  : <A>(x : A) -> A
  = (x : _A) => x;



Bool : Type = (A : Type) -> A -> A -> A;
true : Bool = A => x => y => x;
false : Bool = A => x => y => y;


Bool : Type = (A : Type) -> A -> A -> A;
true : Bool = A => x => y => x;

id
  : <A>(x : A) -> A
  = x => x;

f = id(1)

nat_or_string
  : (pred : Bool) -> (x : pred Type Nat String) -> pred Type Nat String
  = (pred : Bool) => (x : pred Type Nat String) => x;

(x : Nat) ->? Nat

Socket : {
  close : (sock : Socket$) -> ();
} = _;


[@wasm] [@parallel]
f = List.map;

f = (sock : Socket$) => Socket.close sock;

double = (x : Nat $ 2) => x + x;
f = x => double x + double x;

make = (l : Int & x >= 0) =>

f
  : (x : Nat) -> Nat
  = nat_or_string true;
g
  : (x : String) -> String
  = nat_or_string false;


main : IO ()

f x

arr => (
  (x, arr) = Array.get(arr, 0);
  (y, arr) = Array.get(arr, 1);
  (x + y, arr)
);

pred (1) (0)

zero = z => s => z;
succ = n => z => s => s (n z s);

one = z => s => s z;
two = z => s => s (s z);
three = z => s => s (s (s z));


(x : ? -> Nat) => x x;
(x => x(x))(x => x(x))

((x : Int) => x + 1) 1
1 + 1
2;

(A : Type) => (x : A) => x;

x + 1 = 2

f (x + 1) = f (2)

(x => x - 1) (x + 1) = (x => x - 1) (2)
x + 1 - 1 = 2 - 1
x = 2

incr = (x : Int) => x + 1;

id
  : (A : Type) -> (x : A) -> A
  = (A : Type) => (x : A) => x;

Id = (A : Type) => A;

f
  : (x : String) -> String
  = id String;

g
  : (x : Nat) -> Nat
  = id Nat;

incr = x => x + 1;

incr 1 === 1 + 1

(P : Nat -> Type)

P 0
P n -> P (1 + n)


P 5

(P 0)

P 0 -> P (1)
P 1 -> P (2)
P 2 -> P (3)

Type : Type

Type 0 : Type 1
Type 1 : Type 2

((A : Type 0) -> A -> A) : Type 0
((A : Type 1) -> A -> A) : Type 2


((A : Type 0) -> A -> A) : Type 1
((A : Type 1) -> A -> A) : Type 2


absurd : (A : Type) -> A = _;

if : (b : Bool) -> (A : Type) -> A -> A -> A;
Bool = (A : Type) -> A -> A -> A;

ind : (b : Bool) -> (P : Bool -> Type) -> P true -> P false -> P b = _;

Bool = (P : Bool -> Type) -> P true -> P false -> P b;
Bool = (b : Bool) @-> (P : Bool -> Type) -> P true -> P false -> P b;

ind : (L : Level) ->
  (b : Bool (1 + L)) -> (P : Bool L -> Type) -> P true -> P false -> P (lower b) = _;

(n : Nat) -> S n != n

(n : Nat (1 + L)) -> S (lower n) != (lower n)


3 + 4 = 7
S S S Z + S S S S Z = S S S S S S S Z

2 * 3
S S Z * S S S Z = S S S S S S Z


Bool = (A : Type) -> A -> A -> A;
true : Bool = A => x => y => x;
false : Bool = A => x => y => y;

Bool = (A : Type) ->
  ((G : Grade) -> A $ G) $ 1 ->
  ((G : Grade) -> A $ G) $ 1 ->
  ((G : Grade) -> A $ G) $ 1;
true : Bool = A => x => y => (
  _ = y 0;
  x
);
false : Bool = A => x => y => (
  _ = x 0;
  y
);

Nat = (A : Type) -> A -> (A -> A) -> A;

Nat = (A : Type) ->
  A $ 1 ->
  ((G : Grade) -> (A -> A) $ G) $ 1 ->
  A $ 1;
zero : Nat = A => z => s => (
  _ = s 0;
  z
);
one : Nat = A => z => s => (
  s = s 1;
  s z
);
two : Nat = A => z => s => (
  s = s 2;
  s (s z)
);
three : Nat = A => z => s => (
  s = s 3;
  s (s (s z))
);
(x : Nat $ 1) => (y : Nat $ mul_copies(x)) => x * y



1 : Nat

1 : Int

f = (x : Int) => x;

f 1;

add : (A <: Number) -> A -> A -> A = _;

add Nat 1 1 : Nat
add Int -1 1 : Int
add Ratio (-1 / 1) (1 / 2) : Ratio


(-A) -> +B
Array A

Array Nat :> Array Int
Array Int :> Array Nat

f : (Nat -> Nat) -> Nat = _;

f ((x : Int) => 1)
f ((x : Int) => 1)

=
Decidability
Consistency
Impredicativity
Abstractions
Induction
Subtyping
Substructural
```

## Graded Self

```rust
Unit_B = (A : Type) -> A -> A;
unit_b : Unit_B = A => x => x;

Unit_0 = (u : Unit_B) @-> (P : Unit_0 -> Type) -> P unit_b -> P u;
unit_0 = _;

@self()

Unit_0 = (u) @-> (P : Unit -> Type) -> P u

Closed A = (G : Grade) -> (A $ G)

(Unit $ 0 ~ 1) @=> u @-> (P : @Unit -> Type) -> P u;

(False $ 0 ~) @-> (f : f @-> @False f $ 0 ~ 1) -> Type;
(False $ 0 ~ 2) @->
  (I : I -> (f : ))

(A : Type $ 0 ~ 1) => (x : A $ 1 ~ 0) => x;

(A : Type) => x => (x : A $ 1 ~ 0);

(tag : Bool $ 1 ~ 1, payload : tag | true => Int | false => String)
```

## Running

```rust
incr = x => x + 1;

two = incr(1);
two = (x => x + 1)(1);
two = (x + 1)[x := 1];
two = 1 + 1;
two = 2;

omega = (x => x(x))(x => x(x))
(x(x))[x := (x => x(x))]
(x => x(x))(x => x(x))
(x(x))[x := (x => x(x))]
(x => x(x))(x => x(x))

(let x = 1 in x + 1)
(x + 1)[x := 1]
1 + 1
2

(let x = 1 in x + 1)
(x => x + 1)(1)
(x + 1)[x := 1]

(let x = e0 in e1)
(x => e1)(e0)
e1[x := e0]

(let (x, y) = (1, 2) in x + 1)
((x, y) => x + 1)(1, 2)
x + 1[x := 1][y := 2]
x + 1[x := 1]
1 + 1


f = () => console.log(3);
f(console.log(1), console.log(2));


(() => console.log(3))(console.log(1), console.log(2));
console.log(3)

let f = fun () -> fun () -> print_endline "3" in
f (print_endline "1") (print_endline "2")


1 + (2 + (3 + (4 + (5 + 6))))

(1 + 2) + (3 + 4) + (5 + 6)
(1 + 2) + ((3 + 4) + (5 + 6))

(x => x(x))(x => x(x))
```

## Teika

```rust
two = (
x = 1;
  x + 1
);
incr = (x $ 1) => x + 1;
double = x => x + x;
int_or_string = (b : Bool) =>
  (x : b | true => Nat | false => String) => x
id : <A>(x : A) -> A = x => x;
id : (A : Type) -> (x : A) -> A
  = (A : Type) => (x : A) => x;
Id = (A : Type) => A;
(x, y) = (x = 1, y = 2);
Joao = { id = 1; name = "João"; };
x = Joao.id;
l : Option Nat = Some 1;
User = { id : Nat; name : String; };
Either A B =
  (tag : Bool, tag | true => A | false => B);
data%Status =
  | Authorired
  | Banned { reason : String; };

```

## Complete

```rust
// TODO: linearity constraints

----------------
Γ |- Type : Type

Γ |- A : Type  Γ, x : A |- B : Type
-----------------------------------
Γ |- (x : A) -> B : Type

Γ |- A : Type  Γ, x : A |- M : B
--------------------------------
Γ |- (x : A) => M : (x : A) -> B

Γ |- M : (x : A) -> B  Γ |- N : A
---------------------------------
Γ |- M N : B[x := N]

Γ |- t : A  Γ, A : Type, x : A |- B : Type
------------------------------------------
Γ |- @ind(t, A. x. B) : B[A := A][x := t]
```

## Smol

```rust
---------------------
Γ | Δ |- M $ n : Type

--------------------
Γ | • |- Type : Type

Γ |- A : Type  Γ, x : A |- B : Type
-----------------------------------
Γ | • |- (x : A) -> B : Type

Γ |- A : Type  Γ, x : A |- M : B
--------------------------------
Γ |- (x : A) => M : (x : A) -> B

Γ |- M : (x : A) -> B  Γ |- N : A
---------------------------------
Γ |- M N : B[x := N]


((x : Socket) => Socket.close x)

Fix = (x : Fix $ 2) -> () -(0)> ();
fix = (x : Fix $ 1) =(0)> x;


Type : Type $ (n : Nat)
Nat : Type $ (n : Nat)
zero : Nat
succ : Nat -> Nat

(Type, Nat) @-> (Type : Type $ n, Nat : Type $ n);

Type @-> Type

Nat = n @-> (P : Nat -> Type) ->
  P zero -> ((n : Nat) -> P n -> P (succ n)) -> P n;
zero = P => z => s =>

Grade =
  (A : Type) -> A -> ((G : Grade) -> (A -> A) $ G) -> A

Γ |- A : Type  Γ |- n : Grade
-----------------------------
Γ |- M : A $ n

f = (x : Nat $ 2) => x + x;


@Type : Type
Type = @Type;

Bool = (A : Type) -> A -> A -> A;
Closed (El : Type) => = Closed @=>
  (T : Type) => (more : Bool) -> more T (());

Closed Unit false
Nat = Nat @=> (A : Type) -> A -> (Nat -> A) -> A;
fold = fold @=> (n : Nat) => A => z => s =>
  n A z (pred => fold A )


// Weird
Never = (A : Type) -> A;
Stop = (A : Type) -> (b : Never) -> b A;
Apply = (x : Never) -> (P : (x : Never) -> x Type) -> P x;

(s : Stop) => s
stop : Stop = A => b => b A;

Id = (A : Type) -> (B : Type) -> B -> A;

Apply = (x : Never) -> (P : (x : Never) -> x Type) -> P x;
apply : Apply = (x : Never) => (P : (x : Never) -> x Type) =>

(x : Never) => x;

Bool =
  b @-> (A : Type $ 0) -> A $ 1 -> A $ 1 -> A;

(x : (x : A) => A) => x

prefix _ infix
prefix _ infix _ infix _ suffix

x => {
  x
}
x => { x }
(1, )

(1, ...R)
(1, 2,)
{ a; b }

x | 0

6 + (6 / 6)

1 + (2 * 3) + 4


Bool = (A : Type) -> |A| -> |A| -> |A|;
Bool = (A : Type) -> [A] -> [A] -> [A];

(f : A $ 2) =>



Γ | Δ, x |- (x : A $ n) -> B : Type
------------------------------------
Γ | Δ |- (x : A $ 1 + n) -> B : Type

Γ | Δ |- A : Type   Γ, x : A | Δ |- B : Type
----------------------------------------
Γ | Δ |- (x : A $ 0) -> B : Type

Γ |- A : Type  Γ |- G : Grade
-----------------------------
Γ |- M : A $ G

Γ | Δ |- A : Type  Γ, x : A | Δ, x * 0 |- B : Type
--------------------------------------------------
Γ | Δ |- (x : A) -> B : Type



0 * Γ |- A : Type  0 * Γ |- G : Grade
-------------------------------------
0 * Γ |- A $ G : Type

Γ |- o  Γ |- M : A
0 * Γ |- G : Grade
------------------------------
G * Γ |- |M| : A $ G

Γ |- M : A $ G  Γ, x : A * G |- N : B
--------------------------------------
Γ, x : A * G |- |x| = M; N : B[x := N]



Γ |- A : Type
---------------
Γ |- |A| : Type

Γ |- o  Γ |- M : A
0 * Γ |- G : Grade
------------------------------
G * Γ |- M : |A|

Γ |- M : A $ G  Γ, x : A |- N : B
--------------------------------------
Γ, x : A * G |- |x| = M; N : B[x := N]



Closed (|A| : Type $ 0) = (|G| : Grade $ 0) -> A $ G;

(G : Grade $ 1) => (|x| : )

|x : Nat| = M; N
|x : Nat| = _; _


Closed A = (G : Grade $ 1) => (A * G);

(x : Closed Nat $ 1) => (G : Grade $ 2) =>
  (((x : Nat $ G) => (succ x $ G)) (x G));


f : (G : Grade $ 2) -(G)> Nat = _;
g : (G : Grade $ 1) -(G)> Nat = f; // subtyping



// great
(M)
<M>
[M]
{M}
|M|
// inverse
)M(
>M<
]M[
}M{
// consider
/M\
\M/
```

## Smol

A core language, maybe try full linear + closed and add RC as a feature? Sum types can take closed terms.

```rust
Bool = (A : |Type|) -> (then : |A|) -> (else : |A|) -> |A|;

Γ | • |- M <= A
-----------------
Γ | • |- M <= |A|

Γ, x : A | Δ¹ |- M : |A|  Γ, x : A | Δ¹ |- N : B
------------------------------------------
Γ | Δ¹, Δ² |- |x : A| => M : (|x| -> B[x := ]

Closed A = (G : Grade $ 1) => (A $ G)
Fix = ()
Nat =


Bool = (A : Type $ 0) -> (then : |A|) -> (else : |A|) -> |A|;

Nat = (A : Type $ 0) -> (zero : A) -> (succ : |(acc : A) -> A|) -> A;
zero : Nat = A => zero => succ => (|| = succ; zero);
succ (pred : Nat) : Nat = A => zero => succ =>
  (pred A zero succ;


Dyn A = (G : Grade $ 1) -(G)> A;
Nat = (A : Type $ 0) -> (zero : A) -> (succ : Dyn ((acc : A) -> A)) -> A;
zero : Nat = A => zero => succ => (
  _ = succ 0;
  zero
);
succ (pred : Nat) : Nat = A => zero => succ => (
  succ = (G : Grade $ 1) => succ (1 + G);
);

@Nat = n @-> (P : Nat -> Type) ->
  P (n @=> P => z => s => (_ = s 0; z)) ->
  ((pred : Nat) ->)


λx. x
∀P. P true → P false → ∀b. P b

Dyn A = (G : Grade $ 1) -(G)> A;

Nat : Type;
zero : Nat;
succ : Nat -> Nat;

Nat = (A : Type $ 0) -> A -> Dyn (A -> A) -> A;
zero = A => z => s => ()

Nat : Type;
zero : Nat;
succ : Nat -> Nat;

Nat = n @-> (P : Nat -> Type) -> P zero ->
  Dyn ((pred : Nat) -> P pred -> P (succ pred)) -> P n;
zero = P => z => s => z;


Nat = (A : Type $ 0) -> A -> |A -> A| -> A;
zero = A => z => s => (|| = s; z);
one = A => z => s => (|s1| = s; s1 z);
two = A => z => s => (|s2, s1| = s; s2 (s1 z));

Nat = (A : Type $ 0) -> A -> |A -> A| -> A;
zero : Nat = A => z => s => (s = s 0; z);
succ (pred : Nat) : Nat = A => z => s => (
  x = pred
);

Fix = (self : |Fix|) -> ();
|fix : Fix| = self => (
  |f, ...rest| = self;
  f(rest);
);

split : ((G : Grade) -> A $ (1 + G)) -> (A, (G : Grade) -> A $ G) = ?

@Fix = (self : (G : Grade) -(G)> Fix) -> ();
fix : (G : Grade) -(G)> Fix = G => self => (
  self : (G : Grade) -> Fix $ (1 + G)
    = G => self (1 + G);
  (f, self) = split self;
  f(self);
);
fix 1 fix

SNat = |(A : Type $ 0) -> A -> |A -> A| -> A|;


@Nat : Nat -> Type;
@zero : Nat zero;
@succ : (@pred : Nat pred) -> Nat (succ pred);

Nat k = (A : Type) -> A -> (A -> A) $ k -> A;
zero = A => z => s => z;
succ pred = A => z => s => s (pred A z s);


Nat : Type;
zero : Nat;
succ : Nat -> Nat;

Nat = n @-> (P : Nat $ 0 -> Type $ 0) ->
  P zero -> ((pred : Nat $ 0) -> P pred -> P (succ pred)) $ n -> P n;
zero = P => z => s => z;
succ pred = P => z => s => s (pred P z s);


Nat : Type;
zero : Nat;
succ : Nat -> Nat;

Nat = n @-> (P : Nat $ 0 -> Type $ 0) ->
  P zero -> ((pred : Nat $ 0) -> P pred -> P (succ pred)) $ n -> P n;
zero = P => z => s => z;
succ pred = P => z => s => s (pred P z s);


Nat = n @-> (A : Type) -> A -> (A -> A) $ n -> A;
zero : Nat = n @=> P => z => s => z;
succ (pred : Nat) : Nat = n @=> P => z => s => s (pred P z s);

f = () => () => 1;

x = (G : Grade) => (x : Nat $ G) => _;


A $ G
|M : A| $ G

Nat : Type;
zero : Nat;
succ : Nat -> Nat;

SRc : (A : Type $ 0) -> (n : Nat $ 1) -> Type;
next : (A : Type $ 0) -> (pred : Nat $ 0) -> SRc (succ pred) A -> (A, SRc pred A);
free : (A : Type $ 0) -> SRc zero A -> ();

Nat = n @-> (A : Type) -> A -> SRc n (A -> A) -> A;
zero = n @=> A => z => s => (() = free (A -> A) s; z);
succ = pred => n @=> A => z => s => (
  (s1, s) = next (A -> A) pred s;
  s1 (pred A z s)
);

SRc A n = () -> n Type () (acc => (A, acc));
next A pred rc = rc ();
free A rc = rc ();

(x : Unit) =>


(x : EUR $ 2) -(3)> USD


Nat : Type;
zero : Nat;
succ : Nat -> Nat;

SRc : (n : Nat) -> (A : SRc n Type) -> Type;

id = (A : SRc 0 Type) => A (A => (
  (x : A) => x,
  A
));

same : (n : Nat $ 0) -> (A : Type $ 0) ->
  (rc : SRc A (succ n)) ->
  fst (next rc) == fst (snd (next rc));


Nat = n @-> (A : Type $ 0) -> A -> (A -> A) $ n -> A;
zero = n @=> A => z => s => (
  () = A;
  () = s;
  z
);
succ = pred => n @=> A => z => s => (
  () = A;
  (s1, s) = s;
  s1 (pred A z s)
);

( $ ) n A = n Type () (acc => (A, acc));
next A pred rc = rc;
free A rc = (() = A; rc);


@E : (A : E Type) -> Type;
@E A = A;

(A : E Type) => (x : E ())
id = (A : Type) => (x : A) => x;
id = (A : Type $ 0) =>




Nat : Type;
zero : Nat;
succ : (pred : Nat) -> Nat;

Dyn : (A : Type) -> Type;
Dyn A = (n : Nat) -> A $ n;

Nat = n @-> (A : Type $ 0) -> A ->
true = x => y => x y;
false = y => x => y x;

weak = (n : Nat $ 1) => n () () (() => ());

|x|
t = A => (x : A) => x;

Fix = (x : Fix $ 0) -(0)> ()
fix = x => x x;


Double = (m : Nat $ 1) => (n : Nat, eq : m == n);

Bool = b @-> (A : Type $ 0) -> b Type |0 1 -> A;
```

## Consume Only

```rust
Fix = (x : Fix $ 0) -(0)> ();
fix : Fix = x => x x;

Fix = (x : Fix $ 0) -> (() $ 0 -> ()) -> ();
fix : Fix = (x : Fix $ 0) => k => k (x x (() => ()));

False = (A : Type $ 0) -> A;
Fix = (x : Fix $ 0) -> (K : Type $ 0) -> ((False $ 0) -> K) -> K;
fix : Fix = (x : Fix $ 0) => k => k (x x False (f => f));

Fix = ∀(x : Fix). Π(k : ∀(u : Unit). Unit). Unit;
fix : Fix = Λ(x : Fix). λk. k (x x (λu => u));

Fix = (x : Fix $ 0) -> ();
fix : Fix = x => x x;

main : (n : Nat $ 0) -> (() -> ()) $ n;

main2 = (n : Nat $ 0) -> (
  () = x;
  1
);

x @=> b => b _ (instance, x) None;

Nat = n @-> (P : Nat $ 0 -> Type $ 0) -> P zero ->
  ((pred : Nat $ 0) -> P pred -> P (succ pred)) $ n -> P n;
succ pred = n @=> P => z => s => s (pred P z s);


List(A : Type) : Type;
fold<A, K>(l : List(A), initial : K, f : A -> K -> K) : K;
length<A>(l : List(A)) : (List(A), Nat);


length(l) = fold(l, 0, x => (l, n) => (x :: l, 1 + n));


List(A) = (K : Type) -> K -> |A -> K -> K| -> K;
`A
~a

(x : Nat & x > 1) => _;
(x : Nat & x > 1) => _;
(x : Nat | x > 1) => _;
(x : Nat ~ x > 1) => _;

(x : ~Nat) => x;
(x : &Nat) => _;
(x : ^Nat) => x;
(x : 'Nat) => x;

List<A> = (K : Type) -> K -> |A -> K -> K| -> K;
length<A>(l : List<A>) -> (l : List<A>, s : Nat);
length<A>(l : &List<A>) -> Nat;

Bool : Type;
true : Bool;
false : Bool;

Bool = b @-> (P : Bool -> Type $ 0) -> |P true| -> |P false| -> |P b|;
true = P => then => |else| => then;
false = P => |then| => else => else;


case : &Bool -> (K : Type $ 0) -> |K| -> |K| -> |K|;

Fix = &Fix -> ();
fix : Fix = x => x;
x = (&b)


List<A : Type> : Type;
length<A>(l : List<A>) : (l : List<A>, s : Nat);
length<A>(l : &List<A>) : Nat;

length(l : List<A> $ 1) : Nat;



Bool = <A>(then : A, else : A) -> A;
true : Bool = _;
false : Bool = _;

clone(b : Bool) : (l : Bool, r : Bool) = b((true, true), (false, false));
weak(b : Bool) : Unit = b((), ());

Box<A> = <K>(with : A -> K) -> K;

Box<A> = <K>(with : A -> K) -> K;
&Box<A> = <K>(with : &A -> K) -> K;
borrow<A : Borrow, K>(box : Box<A>, k : &Box<A> -> K) : (box : Box<A>, k : K) =
  box(x => (
    k = k(with => with(x));
    (with => with(x), k);
  ));

Box = <K>(with : Socket -> K) -> K;
&Box = <K>(with : &Socket -> K) -> K;
borrow = <K>(box : Box, k : &Box -> K) : (box : Box, k : K) =>
  box(socket => (
    k = k(with => with(socket));
    (with => with(socket), k);
  ));
f = (box : &Box) => box((socket : &Socket) => _);

id
  : (A : Type $ 0) -> (x : A $ 1) -> A;
  = (A : Type $ 0) => (x : A $ 1) => x;

Socket : {
  send : (sock : Socket, message : String) -> (sock : Socket);
  close : (sock : Socket) -> ()
} = _;

Socket : {
  send : (sock : &Socket, message : String) -> ();
  close : (sock : Socket) -> ()
} = _;

List : {
  length<A>(l : &List<A>) : Nat;
} = _;

f = sock => (
  Socket.send(sock, "a");
  Socket.send(sock, "b");
  Socket.close(sock);
);

f = sock => (
  sock = Socket.send(sock, "a");
  Socket.send(sock, "b")
);



double = (x : Nat $ 1) => x + x;

Box<A> = <K>(with : A -> K) -> K;
&Box<A> = <K>(with : &A -> K) -> K;
borrow<A : Borrow, K>(box : Box<A>, k : &Box<A> -> K) : (box : Box<A>, k : K) =
  box(x => (
    k = k(with => with(x));
    (with => with(x), k);
  ));


make : (x : Int & x >= 0) => _;

Lifetime : Type;
zone<K>(k : <L>() -[L]> K) -> K;

borrow<L>(x : A) : &<L>A ! L;

Nat = n @-> (A : Type $ 0) -> A ->

(x : &Nat $ 1) => _;
&<L : Lifetime $ 0>(A : Type) : Type;

List<A : Type> : Type;
length<A>(l : List<A>) : (l : List<A>, s : Nat);

length<L, A>(l : &<L>(List<A>)) : Nat ! F<L>;

read(file : String) : String ! [IO];

read : (file : String) -[IO]> String;

<L>(x : &<L>Nat) : (L : Lifetime, (A : Type, x : A)) =>
  (L, (A = &<L>Nat, x));

add : <L_n : Lifetime $ 1, L_m : Lifetime $ 1>(n : Borrow<L_n>)

Box<A> = <K>(with : A -> K) -> K;
box<A>(x : A) : Box A = with => with(x);

Borrow<L, Box<A>> = <K>(with : Borrow<L, A> -> K) -> K;
borrow<A : Borrow, K>(box : Box<A>, k : <L>(box : &<L>Box<A>) -> K)
  : (box : Box<A>, k : K) =
  box(x => (
    k = k(with => with(x));
    (with => with(x), k);
  ));

(x : &<A>Int) =>

(fst : A, snd : fst == fst)
(fst : Type, snd : fst)


Bool : Type;
Null : Type;

(Int64 : Type) & {

} = _;

(Memory : Type) & {
  length(mem : Memory) : Int64;
  get(mem : Memory, addr : Int64 & addr < length(mem)) : Int64;
} = _;


C_bool = (A : Type) -> A -> A -> A;
I_bool(Bool : Type, true : Bool, false : Bool, b : Bool) =
  (P : Bool -> Type) -> P true -> P false -> P b;

I_unit(Unit : Type, unit : Unit, u : Unit) =
  (P : Unit -> Type) -> P unit -> P u;

Unit : Type;
unit : Unit;

Unit = (u : Unit) & (P : Unit -> Type) -> P unit -> P u;
unit = P => x => x;


Bool =


(Array(A : Type) : Type) & {

} = _;
Array(mem : Memory)


Pair mem = Int64

Nat = (z : Bool, s _ )



Unit : Type;
unit : Unit;

Unit = (u : Unit) & (P : Unit -> Type) -> P unit -> P u;
unit = P => x => x;


Unit : Type;
unit : Unit;

Unit = (u : Unit, i : (P : Unit -> Type) -> P unit -> P u);
unit = (unit, P => x => x);

ind : (u : Unit) -> (P : Unit -> Type) -> P unit -> P u
  = u => snd u : (P : Unit -> Type) -> P unit -> P (fst u);


(Int64 : Type) & {
} = _;

(Memory : Type) & {
  length(mem : &Memory) : Nat64;
  get(mem : &Memory, addr : Nat64 & addr < length(mem)) : Nat64;
} = _;

Ptr_type : {
  @Ptr_type(mem : &Memory, addr : Int64) -> Type
  size(mem : &Memory, addr : Int64) : Int64;
};

Word_size = 8;
Block(mem : &Memory, size : Nat64) =
  (addr : Int64 & addr + size < length(mem));
Usize(mem : &Memory) : Type = Block(mem, Word_size);

Ptr_type : {
  (mem : &Memory, addr : Int64) -> Type
  size(mem : &Memory, addr : Int64) : Int64;
};

Usize(mem : &Memory, addr : Int64) : Type = addr + Word_size < length(mem);
Pair(L : Ptr_type, R : Ptr_type)(mem : Memory, addr : Int64) : Type =
  (l : L(mem, addr))

Usize = {
  *<mem>(size : )
};

String(mem : &Memory) =
  (len : Usize(mem)) & Block(mem, *len);


Pair(mem : Memory, L : Type, R : Type) =
  (addr : Int64, addr < length(mem), )






Bool : (A : Type) -> A -> A -> A;

x @=> x; // negative?
x @=> y => x; // negative?

(@x : T) @=> b => b T (Some (1, x)) None;

(@x : T) @=> b => b T (Some (1, x)) None;

Nat = (A : Type) -> A -> (Nat -> A) -> A;
Nat = n @-> (P : Nat -> Type) -> P zero -> ((pred : Nat) -> P (succ pred)) -> P n;

Bool : Type;
true : Bool;
false : Bool;

Bool = b @-> (P : Bool ∞ $ 0 -> Type) -> P (true ∞) -> P (false ∞) -> P (b ∞);

Nat = n @-> (P : Nat ∞ $ 0 -> Type) ->
  P zero -> ((pred : Nat) -> P (succ pred)) -> P n;

Nat = n @-> (A : Type) -> A -> ((pred : Nat) -> n = succ pred -> A) -> A;

sized(n : Nat)(fold) @=>
  A => z => s => n A z (pred => lower => s (fold pred lower A z s));

fold = (fold : (n : Nat) -> (A : Type) -> A -> (A -> A) -> A) @=>
  n => A => z => s => n A z (pred => s (fold pred A z s));

fold = (fold : (n : Nat) -> (A : Type) -> A -> (A -> A) -> A) @=>
  n => A => z => s => n A z (pred => s (fold A z ));

Pair(A : Type, B : Type) =
  (K : Type $ 0) -> |A -> B -> K| -> K;


id : (A : Type) -> (x : A) -> A
  = (A : Type) => (x : A) => x;

y = id(String);
x = id(Nat)(1);

f = x => x;
x => y => y x;
x => y => x y;
x => y => x;
x => y => y;
x => x + x;

Socket : {
  connect : () -> Socket;
  send : (sock : &Socket, msg : String) -> ();
  close : (sock : Socket) -> ();
} = _;

(sock : Socket) => (
  Socket.send(sock, "hi");
  Socket.close(sock);
  1
);

Memory : {
  malloc : (size : Nat) -> Memory;
  free : (mem : Memory) -> ();
} = _;

Omega = (x => x x) (x => x x);

(x => x x) (x => x x)
(x => x x) (x => x x)

Nat = n @-> (P : Nat -> Type) ->
  |P zero| -> |(pred : Nat) -> P (succ pred)| -> P n;

sized(n : Nat)(fold) @=>
  A => z => s => n A z (pred => lower => s (fold pred lower A z s));

sized(size : (x : T) -> Nat))(fold : (y : T) -> size x = succ (size y) -> K) @=> _;


Bool : Type;
true : Bool;
false : Bool;

Bool = b @-> (P : Bool -> Type) -> P true -> P false -> P b;


Nat = (P : Nat -> Type)

False = (A : Type) -> A;
Fix = (x : Fix) -> False;

fix : Fix = (x : Fix) => x x;
omega = fix(fix);

(x => x x)(x => x x);

Term =
  | x
  | x => M
  | M N;

(x => M) N === M[x := N];


(x => x x) (x => x x)
(x x)[x := (x => x x)]
(x => x x) (x => x x)

((x => x) 1)

(Int -> Fix Int)


(x => x + x) 1 === (x + x)

(x => x x) (x => x x);


Bool : Type;
true : Bool;
false : Bool;

Bool = b @-> (P : Bool -> Type) -> P true -> P false -> P b;
true = P => x => y => x;
false = P => x => y => y;


Nat : Type;
zero : Nat;
succ(pred : Nat) : Nat;

Nat = n @-> (P : Nat -> Type) -> P zero ->
  ((pred : Nat) -> n = succ pred -> P (succ pred)) -> A;




Unit (i : Size) : Type;
unit (i : Size) : Unit;
Unit i = u @-> (P : (u : Unit i) -> Type) -> P (unit i) ->


Unit (i : Size) : Type;
unit (i : Size) : Unit i;

Unit α = u @-> (P : ((β : Size < α) -> Unit β) -> Type) ->
  P (β => unit β) -> P (β => u β);

unit α = P => x => x;

Unit : Type;
unit : Unit;

Unit = (α : Size) -> Unit α;
unit = (α : Size) => unit α;

ind : (u : Unit) -> (P : Unit -> Type) -> P unit -> P u;
  = u => P => x => u 0 (β => P )
fold (u : Unit) = A => x =>


(u : Unit)

Size = (A : Type) -> (A -> A) -> A;
next (s : Size) = A => n => n (s A n);
le : (α : Size) -> (β : Size) -> Type;

Nat : (β : Size) -> le β α -> Type

(Nat [α]) @=>
  (A : Type) -> A -> ((β : Size) -> (lower : le β α) -> Nat β lower -> A) -> A;



Size = (A : Type) -> (Size -> A) -> A;

Size (i : Size i) : Type;

Size α = s @-> (A : Type) -> ()

fold : (n : Nat) -> (A : Type) -> A -> (A -> A) -> A;
sized(fold, n) @=> A => z => s =>
  n A z (pred => lower => fold pred lower A z s);


x @=> x

Nat @=> (A : Type) -> A -> (Nat -> A) -> A;

Nat @=> n @-> (P : Nat -> Type) -> P zero ->
  ((pred : Nat $ 0) -> P pred -> P (succ pred)) $ n -> P n;

Nat @=> (A : Type) -> A -> (A -> A) -> A;


[@teika.core.ir]


Fix = Fix -> ();
fix : Fix = (fix) -> clone(fix)(fix)

borrow : &<L>T $ n;

Size = (A : Type) -> (A -> A) -> A;
next (i : Size) = A => s => s (i A s);


Nat (i : Size) : Type;
Nat α = n @-> (A : Type) -> A ->
  ((β : Size) -> (pred : Nat) -> A) -> A;


fold [s] (n : Nat) = A => z => s =>
  n A z (pred => lower => s (fold pred lower A z s));


Unit (α : Size) : Type;
unit (α : Size) : Unit α;

Unit α = u @-> (P : ((β : Size) -> β < α -> Unit β) -> Type) ->
  (P (β => lower => unit β)) -> (P (β => lower => u β));
unit α = P => x => x;


Unit = (α : Size) -> Unit α;
unit = (α : Size) => unit α;


Nat (i : Size) : Type;
Nat α = n @-> (P : ((β : Size < α) -> Unit β) -> Type) ->
  P (β => unit β) -> P (β => u β);

unit α = P => x => x;

Nat α = n @-> (A : Type) -> A -> ((β : Size) -> α < β -> Nat β -> A) -> A;

fold α = n => A => z => s
  n A z (fold (β => lower => pred => fold β lower pred A z s));

(@Unit : Type) @=> (u : Unit) @-> (P : Unit -> Type) ->
  P ((u : Unit) @=> P => x => x) -> P u;

@((Unit : Unit @-> Type) @=> I => u @->
  (P : @Unit I -> Type) -> P (I unit) -> P (I u)) (refl);

(Unit : Unit @-> Type) @=> I => u @->
  (P : @Unit I -> Type) -> P (@I unit) -> P (@I u)

(@T_False : Type) @=>
  (@False : T_False) @->
    (f : @((@T_f : Type) @=> ((f : T_f) @-> False f))) -> Type;







(@Unit : Unit) @-> Type;

(False) : Type -> Type;


(Unit ) @=>
  I => u @-> (P : @Unit I -> Type) ->
    P (u @=> P => x => x) -> P u;
()
unit : @Unit;

@unit
expected @Unit

received : (P : @Unit -> Type) -> P u -> P u;
expected : @Unit

received : @Unit
expected : @Unit


@Unit = (@Unit : Type) @=> @(u : Unit) @->
  (P : Unit -> Type) -> P (@(u : Unit) @=> P => x => x) -> P (@fold u);

(@T : ) @-> (I : I @-> @T I -> Unit) -> Type;

(I : (I : T_I) @-> @T I -> Unit) -> Type
x : (P : Unit -> Type) -> P unit -> P x;
fold x : @u @-> (P : Unit -> Type) -> P unit -> P (@fold u);

(@T_False : Type) @=>
  (@(False : T_False) @-> )
unit = _;


@x : (P : Unit -> Type) -> P unit -> P (@unit)
@u

Γ, x : A |- B : Type
-----------------------------
Γ |- @assume(x : A). B : Type

Γ, x : A |- B : Type  Γ, x : A |- M : B
---------------------------------------
Γ |- @fix(x : A). M : @assume(x : A). B

Γ, x : A |- B : Type
----------------------------
Γ |- @self(x : A). B : Type

Γ |- M : @assume(x : A). B  Γ |- A ≂ B
--------------------------------------
Γ |- @verify M : @self(x : A). B

Γ |- M : @self(x : T). T
--------------------------
Γ |- @unroll M : B[x := M]


Unit : @assume(@Unit : Type). Type
  = @fix(@Unit : Type). @self(u : Unit). (P : Unit -> Type) -> P u -> P u;
Unit : @self(@Unit : Type). Type = @verify Unit;
Unit : Type = @unroll Unit;

unit : @assume(u : Unit). (P : Unit -> Type) -> P u -> P u
  = @fix(u : Unit). P => x => x;
unit : @self(u : Unit). (P : Unit -> Type) -> P u -> P u = @verify unit;
unit : Unit = unit;


T = Unit @-> (I : I @-> @Unit I -> !Unit) -> Type;

@fix(@T_Unit : Type). @self(Unit : T_Unit).
  (I : @fix(T_I : Type). @self(I : T_I). ) -> Type;

Unit : @assume(@Unit : Type)

unit : @assume(u : Unit). (P : Unit -> Type) -> P u -> P u
  = @fix(u : Unit). (P : Unit -> Type) => (x : P u) => x;
unit = @verify()

Unit = @self(u : Unit). (P : Unit -> Type) -> P u -> P u;

@Unit = @verify Unit;

@fix(@u : Unit)
Nat = @fix(@Nat : Type). (A : Type) -> A -> (Nat -> A) -> A;
Nat = @verify Nat;


Unit = @fix(@Unit : Type). @self(u : Unit). (P : Unit -> Type) -> P u -> P u;
@Unit = @verify Unit;

unit = @fix(@u : Unit). (P : Unit -> Type) => (x : P u) => x;

@self(u : Unit). (P : Unit -> Type) -> P u -> P u

Nat_T = @assume(Nat : Type). Type;

@fix(Nat) : Type = (A : Type) -> A -> (@unroll Nat -> A) -> A;

@assume(@False) : Type. Type;

False = @fix(@False : Type). @self(f : False). (P : False -> Type). P f;
False = @verify(False);

@fix(@Unit) : Type. @self(u : Unit).
  (P : Unit -> Type) -> P


Γ, x : A |- M : A
----------------------
Γ |- @fix(x) : A. M :

Γ, x : A |- B : Type
--------------------------------
Γ |- @fix(x : A). B : Self


@fix(False) : Type.
  (P : @unroll False -> Type) ->

@fix(Unit) : Type.
  (P : @unroll Unit -> Unit) -> P


unit : @self.open(u : Unit). (P : Unit -> Type) -> P unit -> P u;



False = @self(f : @self.close False). (P : @self.close False -> Type) -> P f);
False = @self.close False;


Bool : Type;
true : Bool;
false : Bool;

Bool = @self(b : Bool). (P : Bool -> Type) -> P true -> P false -> P b;
true = P => x => y => x;
false = P => x => y => y;



Γ, x : A |- B : Type
----------------------------
Γ |- @self(x : A). B : Type

Γ, x : A |- B : Type  Γ, x : A |- M : B
---------------------------------------
Γ |- @fix(x : A). M : @self(x : A). B

Γ |- M : @self(x : A). B  Γ |- A ≂ @self(x : A). B
--------------------------------------------------
Γ |- @unroll M : B[x := M]

Γ |- T : Type  Γ, x : T |- M : T
-------------------------------- // TODO: avoid this
Γ |- @fix(@x : T). M : T


T_False = False @-> (f : f @-> @False f) -> Type;

T_False : Type;
T_f : (False : T_False) -> Type;

T_False = @self(False : T_False). (f : T_f False) -> Type;
T_f = False => @self(f : T_f). @False f;


T_False = @fix(@T_False).
  (T_f : @fix(@TT_f). @self(T_f : TT_f). (False : T_False T_f) -> Type)) => (
  T_False = T_False T_f;
  T_f = @T_f;
  @self(False : T_False). (f : T_f False) -> Type;
);
T_f = @fix(@T_f). (
  T_False = T_False T_f;
  False => @self(f : T_f). @False f
);

T_False (T_f : Type) = @fix(@T_False).
  @self(False : T_False). (f : T_f) -> Type;
T_f = @fix(@T_f). (False : T_False T_f) =>
  @self(f : T_f). @False f;

@fix(@F_False : Type).
  @self(False : F_False).

@self(T_f). @self(f : T_false T_f). @False f

T_False = @fix(@T_False). (T_f : Type) =>
  @self(False : T_False T_f). (f : T_f) -> Type;
T_f = @fix(@T_f). (False : T_False T_f) => @self(f : T_f False). @False f;

@fix(@T_False2). T_False (T_f T_false2)

Unit = @fix(@Unit : )


Bool : Type;
true : Bool;
false : Bool;

Bool = @self(b : Bool). (P : Bool -> Type) -> P true -> P false -> P b;
true = P => x => y => x;
false = P => x => y => y;



Unit = @fix(Unit). unit => @self(u : @Unit unit).
  (P : @Unit unit -> Type) -> P @unit -> P u;
unit = @fix(unit). @fix(u : @Unit unit). P => x => x;

(P : Unit -> Type) -> P (@fix(u : Unit). P => x => x) -> P u

expected : @self(u : Unit). (P : Unit -> Type) -> P unit -> P u;
received : @self(u : Unit). (P : Unit -> Type) -> P u -> P u;

(u : Unit) => @u

unit : Unit = @fix(u : Unit). P => x => x;


unit : _ = _;

Unit = @Unit unit;
unit = @unit;

unit = @fix(u). P => x => x;



T_False = False @-> (f : f @-> @False f) -> Type;

T_False : Type;
T_f : (False : T_False) -> Type;

T_False = @self(False : T_False). (f : T_f False) -> Type;
T_f = False => @self(f : T_f). @False f;


T_False = @fix(T_False). T_f => (
  T_False : Type = @T_False T_f;
  T_False : (False : T_False) -> Type = @T_f;
  @self(False : T_False). (f : T_f False) -> Type;
);

T_False = False @-> (f : f @-> @False f) -> Type;
T_False = @fix(@T_False : Type).
  @self(False : T_False). (f : @self(f : ?) -> @False f) -> Type;

T_False = (T_f : Type) -> @fix(@T_false : Type).
  @self(False : T_false). (f : T_f) -> Type;
T_f = (False : T_False) => @fix(@T_f : Type).
  @self(f : T_f). @(False T_f) f;

False : T_False = T_f => @fix(False : @self(False : T_false). (f : T_f) -> Type).
  (f : T_f) =>
  ()

Unit = @self(Unit : T_unit). (unit : @self(unit : Unit). @Unit unit) -> Type
Unit = @fix(Unit : Type).

T_False = False @-> (f : f @-> @False f) -> Type;

T_False : Type;
T_f : (False : T_False) -> Type;

T_False = @self(False : T_False). (f : T_f False) -> Type;
T_f = False => @self(f : T_f False). @False f;


T_False = @fix(@T_False : Type). (
  T_f = @fix(T_f). (
    T_False = @self(False : T_False). (f : @T_f False) -> Type;
    False => @self(f : T_f False). @False f
  );
  T_f = @

  @self(False : T_False). (f : T_f False) -> Type;
);

Γ, x : A |- B : Type
---------------------------
Γ |- @self(x : A). B : Type

Γ |- M : @self(x : A). B  Γ |- A ≂ @self(x : A). B
--------------------------------------------------
Γ |- @verify M : A

Γ |- M : @self(x : A). B  Γ |- A ≂ @self(x : A). B
--------------------------------------------------
Γ |- @unroll M : B[x := M]


Γ
False


------------
Γ |- A $ n : Type
(f : Int $ 0) -> (x : Nat)

(x : Int $ 1) => x;

f : (x : Nat & x <= 5) -> _;

(x : Nat) => (x_le_5 : x <= 5) => _;

(A : *) -> (x : A) -> A;
(A : Type) -> (x : A) -> A;


Type @->

Type @=> forall =>
Type : Type @-> @Type = Type @=> @Type;
Id : @Type = (A : @Type) -> A;

@Type : Type;
@(->) : (A : Type) -> (B : (x : A) -> Type) -> Type;
@(=>) : (A : Type) -> (B : (x : A) -> Type) -> (M : (x : A) -> B x) -> Type;

Type = Type;
(->) A B =


(Type : Type @-> @Type) =>



(Type : Type @-> Type) =>


(A : Type $ 0) =


// TODO: is this valid?
(x : P (x => Type)) => _


-----------



String : Type;
Type : Type;


((x : String) -> String);

T : Type = (A : Type) -> (x : A) -> A;
f : (A : Type) -> (x : A) -> A = _;

(f String) : (x : String) -> String;

Type : Type

((A : Type) -> (x : A) -> A) : Prop


Type : Kind
Kind : Δ

Set of all sets that doesn't include themselves

Nat : Type 0

Type : Type


Type 0 : Type 1
Type 1 : Type 2
Type n : Type (1 + n)

Type : Type


Nat =
  | Zero
  | Succ (pred : Nat);


Nat0 = (A : Type) -> (zero : A) -> A;
Nat1 = (A : Type) -> (zero : A) -> (succ : (pred : Nat0) -> A) -> A;
Nat2 = (A : Type) -> (zero : A) -> (succ : (pred : Nat1) -> A) -> A;

@Nat : Type = (A : Type) -> (zero : A) -> (succ : (pred : Nat) -> A) -> A;
@fold (n : Nat) (A : Type) (acc : A) (f : A -> A) : A =
  n A acc (pred => f (fold pred A z f));

(x => x x) (x => x x)

Nat (α : Size) = (A : Type) -> (zero : A) ->
  (succ : (β : Size) -> β < α -> (pred : Nat β) -> A) -> A;

@fold [α] (n : Nat α) (A : Type) (acc : A) (f : A -> A) : A =
  n A acc (β => lower => pred => fold β lower pred A (f acc) f)

Γ, α : Size |- T : Type
Γ, α : Size, x : (β : Size $ 0) -> β < α -> T |- M : T
--------------------------------------------------
Γ |- @fix(x [α] : T). M

f : (x : A) ->? B
f : (x : A) -> B


Type 0 : Type 1
Type 1 : Type 2


Nat = n @-> (A : Type) -> A -> (A -> A) $ n -> A;
List A = l @-> (K : Type) -> K -> (A -> K -> K) $ length l -> K;

O(length l)

Nat (α : Size) = (A : Type) -> (zero : A) ->
  (succ : (β : Size) -> α = 1 + β -> (pred : Nat β) -> A) -> A;


Nat = (A : Type) -> A -> (Nat -> A) -> A;

Nat (mem : Memory) = (addr : Pointer & addr | 0 => _ | _ => )

Performance = _;
Proof = _;
Utils = _;

pred : (s : Size) -> Nat s -> Nat s;

pred : Nat -> Nat;

Nat : Type 1 = (l : Level $ 0) -> (A : Type l) -> A -> (A -> A) -> A;


// fold : (β : Size) -> β < α -> (n : Nat β) -> (A : Type) -> (acc : A) -> (f : A -> A) -> A
f : (x : -A) -> +B = _;

-(int list) +(string list) +int

Nat =

Id0 : Type 1 = (A : Type 0) -> A -> A;
id0 : Id0 = A => x => x;

Id1 : Type 2 = (A : Type 1) -> A -> A;
id1 : Id1 = A => x => x;

id1 Id0 id0


(x : P (x => Type)) => _

Nat = n @->


Γ, x : T $ n |- M : T
-----------------------------------
Γ |- @fix(x : T $ 1 + n). M : T


Nat : Type;
(0) : Nat;
(1 +) : (pred : Nat) -> Nat;

Nat = n @-> (P : Nat $ 0 -> Type $ 0) ->
  P 0 -> ((pred : Nat) -> P (1 + pred)) -> P n;
(0) = P => z => s => z;
(1 +) pred = A => z => s => s pred;


Nat = n @-> (A : Type) -> A -> (A -> A) $ n -> A;

n => @fix(fold $ 1 + n).
  A => acc => map => n A acc map;

(n : Nat) => (A : Type) => (acc : A) => =>
  @fix(fold $ 1 + n). (n_ == n) =>
  (A : Type) => (acc : A) => (map : A -> A) =>
    n_ (_ => A) acc (pred => fold )

(n : Nat) =>
double (n : Nat) =
  n Nat (0, 0) ()

is_even = (n : Nat) =>
  n Bool true (@fix(not : Bool -> Bool $ 1 + n). b)


(x : A) => M
(x : P (x => Type)) => _;


￼
￼
￼
2

x => y => (x y)[swap]
x => y => (y => x => x y) y x

(1)[x => x Int String]


(x : (A) $ b) => _

(x: A) (:) (A : Type)

T = A $ 5;

(A : Type : ) -> A -> A;

Type : Type;
Value : Type;
Prop : Type;

(A : Type) -> (x : A) -> A

Univ : Univ; // erasable
Prop : Univ; // irrelevant
Type : Univ; // relevant
// TODO: parametric

Γ $ 0 |- M : Univ
---------------------
Γ $ 0 |- @squash M : Type

(Γ, x : A) $ 0 |- B : Type
----------------------------
Γ $ 0 |- (x : A) -> B : Type

Γ $ 0 |- A : Univ  Γ, x : A $ ∞ |- M : B
----------------------------------------
Γ |- (x : A) => M : (x : A) -> B

Γ $ 0 |- A : Type  Γ, x : A $ 1 |- M : B
----------------------------------------
Γ |- (x : A) => M : (x : A) -> B

Γ |- M : (x : A) -> B  Γ |- N : A
---------------------------------
Γ, Δ |- M N : B[x := N]

((x $ 0) : A) ->
((x : A) $ 0)
(x : (A $ 0))
(f : A -> B)
f : A $ 0

(f : A $ 0 : Univ) ->

Fix = Fix -> Type;
(x : Fix) => x x;



Type : (n : Nat) -> Type 0;

((x : A) -> B) : Type 2

(A : Type 1 : Type 0) => (x : A : Type 1) -> x;

(A : Type 2) => (x : A) => (x, x);
(A : Type $ 0) => (x : A $ 2) => (x, x);


(A : Type) => (x : A $ 2) => x


Kind =
  | Erasable
  | Irrelevant
  | Multiplicative
Type : ()

Type : Type
Linear : Type

Type : Univ
Type : Type

(A : Linear)

((x : A) $ G)

(x : ) : Promise<T, E> => _


malloc : () -> (L : Lifetime);
free : (L : Lifetime) -> ();


lifetime : <A>(x : &A) -> Lifetime;

() -[Allocator Root]> ()
() -[Allocator One]> ()

add : (x : &Nat, y : &Nat) -> Nat;
add : (x : &Nat, y : &Nat) -[Borrow x | Borrow y]> Nat;

add : (x : &Nat, y : &Nat) -> Nat;

<L>(x : &<L>Nat) =[Zone L]> x + x;

(x : &Nat) =[Zone x]> x + x;

(x : &Nat) => (
  (x1, x2) = x.copy()
  (x1, x2)
)

Type : Univ;
Prop : Univ;

f = (A : Type : Univ) => (x : A : Type) => x;

(A : Prop : Univ) => (x : A : Prop) => _;

borrow : <K>(x : Nat, region : () -[Zone x]> K) -> K;


Type 0 : Type 1
Prop : Type 0

SFalse : Prop = (A : Prop) -> A;
False : Type = (A : Type) -> A;

f : SFalse -> False = _;

Unit : Prop = (A : Prop) -> A -> A;

(A : Type 0) => (u : Unit) => u (Type 0) A

Bool : Prop = (A : Prop) -> A -> A -> A;

(b : Bool) => b (Type 0)
(A : Type) => (x : (A : Prop) -> Prop) => x;


Fix : Type;
&Fix : Type;

Fix = &Fix -> A;
&Fix = &Fix -> A;


@Fix = f @-> (P : Fix -> Type) -> (x : Fix) -> P x -> P f;

Fix : Type;
fix : Fix;

Fix = (x : Fix) -> ();
fix = x => fix x;

&Fix : Type;
fix : Fix;

Fix = f @-> (P : Fix -> Type) -> P fix -> &Fix -> P f;
&Fix = f @-> (P : &Fix -> Type) -> P (&fix) -> &Fix -> P f;

fix = P => p_fix => x => x P p_fix x;

Type : Type;
Data : Type; // clonable?
Prop : Type;


```

## Typed Tree's

Checking
Compiling
Hover on Type
Typer tracking
Partial rebuild
Debugging
Jump to definition
Serialization

## Macabas

```rust
do {
  const x = 1;
  return x + 1;
}

(let x = 1; x + 1)
(x = 1; x + 1)

x => (
  y = (
    z = x;
    x + 1
  );
  y + 2;
);


----------------
Γ |- Type : Type

Γ |- A : Type  Γ, x : A |- B : Type
-----------------------------------
Γ |- (x : A) -> B : Type

Γ, x : A |- M : B
--------------------------------
Γ |- (x : A) => M : (x : A) -> B

Γ |- M : (x : A) -> B  Γ |- N : A
---------------------------------
Γ |- M N : B[x := N]



clone : (x : Nat) -> (l : Nat, r : Nat) = _;

Array : {
  get : <A>(arr : &Array A, i : Nat) -> (el : A);
  set : <A>(arr : Array A, i : Nat, el : A) -> (arr : Array A);
  size : <A>(arr : &Array A) -> (size : Nat);
} = _;



x[0] := x;

double = (x : &Nat) => x + x;
double = (x : &Nat $ 2) => x + x;

id = (A : Type $ 0) => (x : A) => x;

f = (x : Nat) => (x_le_5 : x <= 5) => x;

Unit =
  | unit;

f = (n : Nat) => (x : Unit) => n;

(n : Nat) => (x : Unit) => (y : Unit) : Eq(f n x, f n y) => ()

f n unit == f n unit

Eq<A>(x : A, y : A) =
  | refl : Eq(x, x);

refl

Erasable
Irrelevance
Duplicable / Multiplicative


Type : Univ;
Data : Type;
Prop : Data;

(A : Type : Univ) => (x : A : Type) => x;
(A : Prop : Type) => (x : A : Prop) => x;
(A : Data : Type) => (x : A : Data) => (x, x);

(A : Type) => (x : A) => x;
(A : Prop) => (x : A) => x;
(A : Data) => (x : A) => (x, x);


<A : Type>(x : A) => x;
<A : Prop>(x : A) => x;
<A : Data>(x : A) => (x, x);


<A>(x : A) => x;
<A>(x : Prop A) => x;
<A>(x : Data A) => (x, x);

Box (A : Type) : Data;

Eq<A : Type>(x : A, y : A) =
  | refl : Eq(x, x);

(A : Data) => (x : A) => (eq : Eq(x, x)) : Eq(eq, refl) => _;

Eq(1, 2) : Prop;


f = (x : Nat, y : Float) -> String;


Box (A : Type) = <R>(k : (x : A) -> R) -> R;

Box<M>(A : Type) = <R>(k : (x : M A) -> R) -> R;
Box<K>(A : K) = <R : K>(k : (x : A) -> R) -> R;

(A : Type) => (x : Prop A) => x;

(A : Type) => (x : Box<Prop> Unit) => x



(x )

Type : Univ;
Data : Univ;
Prop : Univ;

Nat : Type = Prop Nat;
(x : Nat) =>

Box (T : Type) = <A>(k : T -> A) -> A;

Box<K>(T : K) : K = <A : K>(k : T -> A) -> A;
Box() : Prop
Box (Eq(1, 2)) : Prop;

Box(A : Type) : Type = <A>(k : T -> A) -> A;

(A : Type) => (x : Prop (Box A)) => x

(A : Prop) => (x : Box A) => x

(A : Type) => (x : Data Nat) => (x, x);

(+) : (x : &Nat) -> (y : &Nat) -> Nat;
(x : Data Nat) => x + x;

(x : &Nat) => (x, x);

Kind : Kind; // universes, erasable
Type : Kind; // resources, linear
Data : Kind; // data, duplicable
Prop : Kind; // propositions, irrelevant

Box (A : Type) = <K>(k : (x : A) -> K) -> K;

(A : Type) => (x : A) =>
(A : Type)

List(A : Type) : Type;
List(A : Data) : Data;
List(A : Prop) : Prop;

Squash (K : Kind) : Type = <A>(K -> A) -> A;
squash<K>(x : K) : Squash K = k => k x;


id = (A : Type) => (x : A) => x;
id (squash Type) : (x : squash Type) -> squash Type;

Eq<A : Type>(x : A, y : A) : Type;
Eq<A : Data>(x : A, y : A) : Prop;

List(A : Type) : Type;


List<K>(A : K) : K;

(x : &Nat) =>

Kind : Kind; // universes, erasable
Type : Kind; // resources, linear
Data : Kind; // data, duplicable
Prop : Kind; // propositions, irrelevant

// TODO: functions over data, can they be in Data? What about h-sets?
// TODO: functions over data also implies in automatic memory management

List(A : Type) : Type;
Double(A : Type) = (x : A, y : A)



b @-> (P : Bool -> Type) -> P true -> P false -> P b


x : Type = (A : Data) -> A;

Fix = f @=>


User : Type + Copy + Free;

(A : Type & A = Prop _) =>


Kind : Kind; // universes, erasable
Type : Kind; // resources, linear
Data : Kind; // data, duplicable
Prop : Kind; // propositions, irrelevant

// functions on data? Maybe make it predicative?
// functions on prop?

False : Type = (A : Data) -> A;

f = (A : Data) => (x : A) => x;
(A : Data) => (x : A) => x
Nat64 : Data;
Nat64.size = 8;

Pointer(mem : Memory, size : Nat64) : Data;
Pointer(mem, size) = {
  addr : Nat64;
  valid : addr + size < length(mem);
};

Nat128(mem : Memory) : Data;
Nat128.size : Nat64;

Nat128(mem) = Pointer(mem, Nat128.size)
Nat128.size = 16;

String(mem) : Data;
String.size(mem) : Nat64;

String(mem) = {
  addr : Nat64;
  valid : addr + Nat64.size + String.size(mem) < length(mem)
};

User(mem) : Data;
User(mem) = {
  addr : Nat64;
  valid : addr + Nat128.size + ;
};

```

## LSP Flow

```rust
External Message -> Check State -> Queue Message
                                -> Error

Queue Message -> Lock Required Data -> Compute -> Commit Change ->
```

## Self Type

```rust

T_False = Unit @-> Type;
(False : T_False) @=> f @-> (P : False -> Type) -> P f;

False : Type) @=> f @-> (P : False -> Type) -> P f;

Unit = u @-> (P : Unit -> Type) -> P unit -> P u;

@Unit : Type;
@unit : Unit;

@Unit = (u : Unit) @-> (P : Unit -> Type) -> P unit -> p u;
@unit = P => x => x;


False : (False : Type) -> Type
  = (False : Type) => (f : False) @-> (P : False -> Type) -> P f;
False : Type = @tie(False);


T_False = False @-> (f : f @-> @False f) -> Type;

T_False : Type;
T_f : (False : T_False) -> Type;

T_False = (False : T_False) @-> (f : T_f False) -> Type;
T_f False = (f : T_f False) @-> @False f;



T_False = (T_False : Type) => (T_f : (False : Type) Type) => (f : T_f) -> Type;

T_Unit = (T_Unit : Type) => (Unit : T_Unit) ->
  (unit : @tie((T_unit : Type) => Unit )) -> Type;

Unit : (Unit : Type) -> (unit : Unit) -> Type
  = (Unit : Type) => (unit : Unit) =>
      (u : Unit) @-> (P : Unit -> Type) -> P unit -> P u;

Unit = @fix(Unit)
T_Unit = Unit @->

T_False = False @-> (f : f @-> @False f) -> Type;

((@T_False : Type) @=>
  (False : T_False) @-> (f : ) -> Type);

(T_False) : Type @=>


Unit : Type;
unit : Unit;

Unit = (u : Unit) @-> (P : Unit -> Type) -> P unit -> P u;
unit = (P : Unit -> Type) => (x : P unit) => x;

T_False = False @-> (f : f @-> @False f) -> Type;

T_False : Type;
T_f : (False : T_False) -> Type;

T_False = (False : T_False) @-> (f : T_f False) -> Type;
T_f False = (f : T_f False) @-> @False f;

T_False = False @-> (I : I @-> _ -> @False I) -> Type;

@tie(T_False : Type, T_f : (False : T_False) -> Type). (
  T_False1 = (False : T_False) @-> (f : T_f False) -> Type,
  T_f1 (False : T_False1) = (f : T_f False) @-> @False f
)



@fix.self()



Kind : Kind; // not h-set, affine, erasable
Type : Kind; // not h-set, linear, managed manually
Data : Kind; // h-sets, intuitionistic, reference counted
Prop : Kind; // h-sets, intuitionistic, irrelevant


Bool = (A : Type) -> |A| -> |A| -> A;

clone
Fast : Kind; //

[@teika.mode.performance]
(A : Data) => (x : A) => (x.clone(), x)
```

## Smol CPS

```rust

```

## RC Smol

```rust
id = (A : Data) => (x : A) => (x, x);
id = (A : Type) => (x : A) => x;

GC Minor + Rc Major

x = 1;
x = x + 1;
y = x + 2;

Kind : Kind; // not h-set, affine, erasable
Type : Kind; // not h-set, linear, managed manually
Data : Kind; // h-sets, intuitionistic, reference counted
Prop : Kind; // h-sets, intuitionistic, irrelevant



Γ |- A : P  Γ, x : A |- B : R
-----------------------------
Γ |- (x : A) -> B : K

{ P : 'P; R : Type; K : Type }
{ P : 'P; R : Prop; K : Prop }
{ P : Type; R : Data; K : Type } // prevent linear violation
{ P : Data; R : Data; K : Data }
{ P : Prop; R : Data; K : Data }


Unit : Prop;

id = (x : Unit) => id;
Fix : Type = Fix -> Unit;
fix = x => id (x x);

id : (A : Type) -> (x : A) -> A = _;

(A : Type) => (x : A) => x

((x : A : Type) -> (B : Data)) : Type
((x : A : Prop) -> (B : Data)) : Data
((x : A : Data) -> (B : Data)) : Data

((x : A : Type) -> (B : Prop)) : Prop

(x : A : Type 0) => (A : Prop) => (x : A) => x

()

@tie()
@fix
(x : )
```

## Erasing Kinding System

```rust
Type : Kind;
Data : Kind;
Prop : Kind;


Sigma A B =
  s @-> (P : Sigma A B -> Type) ->
    ((fst : A) -> (snd : B fst) -> K) -> P s;
Prop = p @-> (P : Prop -> Type) ->
    ((A : Type) -> (x : A) -> (irr : (y : A) -> Eq x y) -> P _)
  -> P p;
Prop = (A : Type, x : A, )
Data = (A : Type, x : Rc A);

(A : Data) => (x : fst A) => _
Kind.free<A : Kind>(x : A) : n

Unit : Type;
unit : Unit;

Unit = u @-> (P : Unit -> Type : Kind) -> P unit -> P u;


Type : Erasable Type;

Bool = (A : Type) -> A -> A -> A;
(b : Bool) => (b (@squash Type) : @squash Type -> @squash Type : Type)
(b : Bool) => (@unsquash (b (@squash Type)) : Type -> Type : Kind)


Type : Type;

Fix = Fix -> Type;
fix = x => x;

LType : LType;
IType l : IType (l + 1);

id = x => x;

fix = x => x x;
omega = fix fix;

Type : Type;

Fix = Fix -> ();
fix : |Fix| = x => x x;

(x) => x + x


Fix = Fix -> ();

Nat [a] = (K : Type) -> K -> ((b < a) -> Nat b -> K) -> K;
fold [a] = (n : Nat [a]) => K => acc => f =>
  n K acc (b => pred => fold [b] pred K (f acc) f);

Type : Type;


Term [a] = (K : Type) ->
  (var : String) ->
  (app : [l] -> [r] -> a == 1 + l + r -> Term [l] -> Term [r] -> K) ->
  (abs : [b] -> a == 1 + b -> String -> Term [b] -> K) ->
  K;

Term [a] = (K : Type) ->
  (var : String -> K) ->
  (app : [l] -> [r] -> a == 1 + l + r -> Term [l] -> Term [r] -> K) ->
  (abs : [b] -> a == 1 + b -> String -> Term [b] -> K) ->
  K;

abs "x" (var "x")
(x => x)

[s_f] => [s_x] => (f : T [s_f]) => (x : _) => x y



size M = a
size N = b
M N =

app lambda arg : Term = K => app => abs => app lambda arg;
abs body : Term = K => app => abs => abs body;

id : Term = abs (x => x);

Type [α] : Type [1 + α]

// template

Γ |- n : Size  Γ |- A : Type n  Γ |- M : A
------------------------------------------
Γ |- M : A

// size

-----------------
Γ |- Size : USize

-------------
Γ |- 0 : Size

Γ |- n : Size
-----------------
Γ |- 1 + n : Size

// universe

Γ |- n : Size
--------------------
Γ |- Type n : Type 0

//

Γ, n : Size |- T : Type m
---------------------------------------
Γ |- (n : Size) -> T : Type (m[n := 0])

Γ, n : Size |- M : T
--------------------------------------
Γ |- (n : Size) => M : (n : Size) -> T

Γ |- M : (n : Size) -> T  Γ |- N : Size
---------------------------------------
Γ |- M N : T[n := N]

//

Γ |- A : Type a  Γ, x : A |- B : Type b
---------------------------------------
Γ |- (x : A) -> B : Type (1 + b)

Γ |- A : Type a  Γ, x : A |- M : B
----------------------------------
Γ |- (x : A) => M : (x : A) -> B

Γ |- A : Type a  Γ |- M : (x : A) -> B  Γ |- N : A

--------------------------------------------------
Γ |- M N : B[x := N]


Nat s = (A : Type s) -> A -> (A -> A) -> A;


Fix = (x : Fix $) -> ();
fix = (x : Fix) => x [a] x;

f = (x : Nat n) : Nat (2 * n) => x + x;

fix 0 (fix 0)

Nat = (n : Size) -> (K : Type n) -> K -> (Nat -> )

(n : Size) => (A : Type n) => (x : A) => x;

Term [a] =
  | Var : [a] -> String -> Term [1 + a]
  | Abs : [a] -> String -> Term [a] -> Term [1 + a]
  | App : [a] -> [b] -> Term [a] -> Term [b] -> Term [1 + a + b];

Type : Type;
Data : Type;

⊥ = (A : *) -> A;
¬ φ === φ -> ⊥;

℘ S === S -> Type;

U : Kind = (X : Kind) -> (℘℘X -> X) -> ℘℘X;
τ (t : ℘℘U) : U = (X : Kind) => (f : ℘℘X -> X) => (p : ℘X) =>
  t ((x : U) => p (f (x X f)));
σ (s : U) : ℘℘U = s U τ;
Ω : U = τ ((p : ℘U) => (x : U) -> σ x p -> p x);

℘ S === S -> Type 0;

U : Type 2 = (X : Type 1) -> (℘℘X -> X) -> ℘℘X;
τ (t : ℘℘U) : U = (X : Type 1) => (f : ℘℘X -> X) => (p : ℘X) =>
  t ((x : U) => p (f (x X f)));

σ (s : U) : ℘℘U = s U τ;
Ω : U = τ ((p : ℘U) => (x : U) -> σ x p -> p x);


U : Type 0 = (X : Type 0) -> (℘℘X -> X) -> ℘℘X;



id : ()
  = s => A =>



Unit [0] = (A : Type) -> A -> A;
Unit [1 + a] = u @-> (P : Unit [b] -> Type) -> P (unit [a]) -> P (u [a]);


Id s : Type (2 + s) = (A : Type s) -> (x : A) -> A;
id s = A => x => x;

pair s : Type (2 + s + s) = (A : Type s) => x => (x, x)

Type : Type

Fix = Fix -> ();
fix = x => x x;

A => x => x

@u A x == x


Prop : Type;
Data : Type;
Type : Type;

Type : Type;

Term =
  | Type
  | (x : A) -> B
  | (x : A) => M
  | M N;

Eq<A>(x : A, y: A) = (P : A -> Type) -> P x -> P y;

Eq(Eq<Bool>(x, y), refl);


(l : List Int) -> Eq(bubble(l), quick(l));

bubble = quick

Prop :

(x : A, y : A) -> Eq(x, y);

a b -> a == b \/ a != b
(x : A : Prop) =>

f : () -> Unit;

f : (A : Prop) -> (x : A) -> x ==


Prop // single constructor
Set // equality is a mere proposition
Type

Prop : Type 0
Set : Type 0

Set : Type 0
Type 0 : Type 1
Type 0 == Type 0

(mem : Memory) =>

Int64(mem : Memory) = {
  addr : Pointer;
  is_valid : addr + 8 < length(mem);
};

Γ, x : @self(x). T |- Type
--------------------------
Γ |- @self(x). T : Type

Unit @=> u @-> (P : Unit -> Type) -> P (u @=> P => x => x) -> P u

!Unit = u @-> (P : @Unit I -> Type) -> P (I (u @=> P => x => x)) -> P (I u);
Unit = Unit @=> (I : I @-> !Unit -> @Unit I) => !Unit;


Unit : Type;
unit : Unit;

Unit = (u : Unit) @-> (P : Unit -> Type) -> P unit -> P u;
unit = P => x => x;

Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool) @-> (P : Bool -> Type) -> P true -> P false -> P b;
true = P => x => y => x;
false = P => x => y => y;

Γ |- M : @self(x : A). B  Γ |- A ≂ @self(x : A). B
--------------------------------------------------
Γ |- M : B[x := M]


Inductive N2: Type :=
  | O : N2
  | S : N2 -> N2
  | mod2 : S (S O) = O.

N2 : Type;
zero : N2;
succ : (pred : N2) -> N2;
mod2 : succ (succ zero) == zero;

N2 = n @-> (P : N2 -> Type) -> P zero ->
  ((pred : N2) -> P (succ pred)) -> P n;
zero = P => z => s => z;
succ pred = P => z => s => pred (pred => P (succ pred)) (s z) z;
mod2 = refl;



Squash : (A : Type) -> Type;
box : (A : Type) (x : A) -> Squash A;

Squash = s @-> (P : Squash A -> Type) -> ((x : A) -> P (box A x)) -> P s;


succ : (pred : Nat) -> Type;


Type -> Type l


Term =
  | Type
  | x
  | (x [a] : A) -[b]> B
  | (x : A) => M
  | M N;

(x : A) -[s]> B

(A ['a]: Type) -[1]> (x ['x] : A) -['x]> A;
(A ['a]: Type) -[1]> (x ['k] : A) -> (y ['k] : A) -['k]> A;

(A ['a]: Type) => (x ['x] : A) => x

Nat = (A : Type) -> A -> (Nat -> A) -> A;

mu : (Type -> Type) -> Type;
mu = x => x x;


℘ S === S -> Type;

U : Type = (X : Type) -> (((X -> Type) -> Type) -> X) -> (X -> Type) -> Type;

Mu G = (X : Type) -> (G X -> X) -> X;

℘ S === S -> Type;


--------------

Id : Type 0 = |A : Type 0| -> (x : A) -> A;

Nat = (A : Type) -> A -> |A -> A| -> A;

U : Type 2 = (X : Type 1) -> (℘℘X -> X) -> ℘℘X;
τ (t : ℘℘U) : U = (X : Type 1) => (f : ℘℘X -> X) => (p : ℘X) =>
  t ((x : U) => p (f (x X f))); // fails as x X is invalid

σ (s : U) : ℘℘U = s U τ;
Ω : U = τ ((p : ℘U) => (x : U) -> σ x p -> p x);

Nat (Self : Type) = (A : Type) -> A -> (Self -> A) -> A;
Nat : Type = (X: Type) -> (℘℘X -> X);
unfold : Nat -> (A : Type) -> A -> (Nat -> A) -> A =

(A : Type) -> A -> ( -> A) -> A


Fix : Type;
fold : (Fix -> ()) -> Fix;
unfold : Fix -> (Fix -> ());

fold = (x : Fix -> ()) : Fix =>
  // introduce abstraction
  _;
unfold = (x : Fix) : (Fix -> ()) =>
  // eliminate abstraction
  _;

(A : Type) ->

// Type 0 implies in statically known size
((A : Type 0) -> A -> A) : Type 1;



Bool : Type 1 = ∀(l : Level). ∀(A : Type l). A -> A - A;

U : Type 1 = (X : Type 0) ->
  (((X -> Type 0) -> Type 0) -> X) -> (X -> Type 0) -> Type 0;
τ (t : (U -> Type 0) -> Type 0) : U = (X : Type 0) => (f : ℘℘X -> X) => (p : ℘X) =>
  t ((x : U) => p (f (x X f)));

σ (s : U) : ℘℘U = s U τ;
Ω : U = τ ((p : ℘U) => (x : U) -> σ x p -> p x);

Sigma (l : Level) (A : Type l) (B : A -> Type l) : Type (1 + l) =
  s @-> (P : Sigma l A B -> Type l) ->
    (k : (x : A) -> (y : B a) -> P (s @=> P => k => k x y)) -> P s;

Sigma (l : Level) (A : Type l) (B : A -> Type l) : Type (1 + l) =
  s @-> (P : Sigma l A B -> Type l) ->
    (k : (x : A) -> (y : B x) -> P (s @=> P => k => k x y)) -> P s;

Sigma (l : Level) (B : Level -> Type l) : Type (1 + l) =
  s @-> (P : Sigma l B -> Type l) ->
    (k : (x : Level) -> (y : B x) -> P (s @=> P => k => k x y)) -> P s;
fst l B pair = @pair (_ )
(A : Type l)
```

## Halting Problem

```rust
double = x => x + x;
does_a_for = n => {
  for (let i = n; i > 0; i--) {

  }
};
const loop = () => {
  while (true) {}
};
const loop = () => loop();
const map = (f, l) => {
  if (l.length == 0) {
    return [];
  }
  const [el, ...tl] = l;
  return [f(el), ...map(f, tl)];
};

true = (x => y => x);
false = (x => y => y);

one = z => s => s(z);
two = z => s => s(s(z));


pair x y = k => k x y;
(x => pair x x)
(x => x(x)) (x => x(x))


Array = {
  create : <A>(initial : A, len : Int, len_is_nat : length >= 0) -> Array<A>
} = _;

Array.create(0, -5, loop());


f : (x : number) -> (y : number) -> number
  = x            => y            => x + 1;

(x => x x) (x => x x)


Array = {
  create : <A>(initial : A, len : Int, len_is_nat : length >= 0) -> Array<A>
} = _;

incr : (x : Int) -> Int = _;
incr : (x : Int) ->? Int = _;
Array.create(0, -5, loop());
```

```json
{
  "key": {
    "key": {
      "key": {
        "key": {
          "key": {
            "key": {}
          }
        }
      }
    }
  }
}
```

## Large Elimination

```rust
Type : Type;

Dynamic

Bool = (A : Type) -> A -> A -> A;


℘ S === S -> Type;
U : Type = (X : Type) -> (℘℘X -> X) -> ℘℘X;
τ (t : ℘℘U) : U = (X : Type) => (f : ℘℘X -> X) => (p : ℘X) =>
  t ((x : U) => p (f (x X f)));
σ (s : U) : ℘℘U = s U τ;
Ω : U = τ ((p : ℘U) => (x : U) -> σ x p -> p x);

Nat [a] @=> (A : Type) -> A -> ((b < a) -> Nat [b] -> A) -> A;

(Unit [U_a]) @=> () -> (u : Unit [b]) @->
  (P : [b > a] -> Unit [c] -> Type) ->
    P [U_a] unit -> P [u_a] u;

(unit [u_a]) @=> (P : [a] -> ([a = 1 + b] -> Unit [b]) -> Type)

Unit = u @-> (P : Unit -> Type) -> P unit -> P u;


Bool : Type 1 = (A : Type 0) -> A -> A -> A;

Id = (A : Type _) -> A -> A;
id = (A : Type _) => (x : A) => x;
id Id id


P => u => p_unit : P u =>
  u
  | unit => p_unit;

(pred : _) => (x : pred (_ => Type) Int String) =>
  ()
id

Nat [a] = (A : Type) -> A -> ([b < a] -> Nat [b] -> A) -> A;

Unit [0] = (u : Unit [⊥]) @-> (P : Unit [⊥] -> Type) -> P unit -> P u;
Unit [1 + a] = (u : Unit [a]) @-> (P : Unit [a] -> Type) -> P unit -> P u;

Unit [a] =
  (u [b < a] : Unit [b]) @-> (P : Unit [b] -> Type) -> P unit -> P u;

T_False : [a] -> Type;
T_False [a] =
  (False [b]) @->
  (f : (f [b]) @-> @False f) -> Type;

False : [b < a] -> T[a := b]


[k] -> (False [a < k]) @-> (f : (f [b = 1 + a]) @-> @False [b] f) -> Type;


Unit : [k] -> (Unit [a < k]) @-> Type;
Unit = [k] => (Unit [a < k]) @=> (u [b < a]) ->
  (P : Unit [b] -> Type) -> P unit -> P u;


[k] => (Nat [a < k]) @=> (n [b < a]) -> (P : Nat [b] -> Type) ->
  P zero -> ((pred : Nat [b]) -> P (succ pred)) -> P n;

[k] => (Nat [a < k]) @=> (n [b < a]) -> (P : Nat [b] -> Type) ->
  P zero -> ([c < a] -> (pred : Nat [c]) -> P (succ pred)) -> P n;


Term =
  | Type
  | (x : A) -> B
  | (x : A) => M
  | M N;

// naive
-----------------
Γ |- Size : USize

-------------
Γ |- 0 : Size

Γ |- a : Size
-----------------
Γ |- 1 + a : Size

// TODO: maybe this allows for some interesting elimination
Γ |- a : Size
--------------------
Γ |- Type a : Type a

Γ |- A : Type a  Γ, x : A |- B : Type b
---------------------------------------
Γ |- (x : A) -> B : Type (1 + a + b) // abstraction overhead

Γ, x : Size |- B : Type b
-------------------------------------------
Γ |- (x : Size) -> B : Type (1 + b[x := 0])

// fancy
-----------------
Γ |- Size : USize

-------------
Γ |- 0 : Size

Γ |- a : Size
-----------------
Γ |- 1 + a : Size

// TODO: all inhabitants besides of the universe itself are abstractions
Γ |- a : Size
--------------------------
Γ |- Type a : Type (1 + a) // abstraction overhead

Γ |- A : Type a  Γ, x : A |- B : Type b
---------------------------------------
Γ |- (x : A) -> B : Type (a + b)

Γ, x : Size |- B : Type b
---------------------------------------
Γ |- (x : Size) -> B : Type (b[x := 0])


(A : Type 0) -> (x : A) -> A : Type 1
(A : Type 1) -> (x : A) -> A : Type 4

((s : Size) -> (A : Type s) -> (x : A) -> A) : Type 1

(K : Type 0) -> (k : (A : Type 0) -> K) -> K : Type 1
(A : Type 0) -> (B : Type 0) -> A : Type 1

(s : Size) -> (K : Type s) -> (k : (A : Type s) -> K) -> K : Type 2


(s : Size) -> (A : Type s) -> (B : Type s) -> A : Type 1

Type (1 + l) == ((s : Size) -> Type (l + s))

(A : Type)

(x : Size) -> (A : Type x) -> A

(A : Type 0) -> (B : Type 0) -> A : Type 1
(s : Size) -> (U : Univ s) -> (A : U) -> (B : U) -> A : Type 1

// measuring how many sizes are unknown
Γ |- a : Size
--------------------
Γ |- Type a : Type a

(A : Type 0) ->

// Tarski
-----------------
Γ |- Size : USize

-------------
Γ |- 0 : Size

Γ |- a : Size
-----------------
Γ |- 1 + a : Size

Γ |- a : Size
--------------------------
Γ |- Univ a : Univ (1 + a) // universe overhead

Γ |- a : Size
--------------------
Γ |- Type a : Univ a

Γ |- A : K a  Γ, x : A |- B : K b
------------------------------------
Γ |- (x : A) -> B : Type (1 + a + b) // abstraction overhead

Γ, x : Size |- B : Type b
------------------------------------------- // compact
Γ |- (x : Size) -> B : Type (1 + b[x := 0]) // abstraction overhead

// (A : Type 0) -> (x : A) -> A : Type 1
(s : Size) -> ()
// (A : Type 0) -> (B : Type 0) -> A : Type 1
(s : Size) -> (U : Univ s) -> (A : U) -> (B : U) -> A : Type 1


// naive
-----------------
Γ |- Size : USize

-------------
Γ |- 0 : Size

Γ |- a : Size
-----------------
Γ |- 1 + a : Size

// TODO: maybe this allows for some interesting elimination
Γ |- a : Size
--------------------
Γ |- Type a : Type a

Γ |- A : Type a  Γ, x : A |- B : Type b
---------------------------------------
Γ |- (x : A) -> B : Type (1 + a + b) // abstraction overhead

Γ, a : Size |- T : Type b
---------------------------------------------
Γ |- (a : Size, x : T) : Type (1 + b[a := 0]) // indirection overhead


// (A : Type 0) -> A : Type 1
(s : Size, T : Type s) : Type 1
(s = 2, T = (A : Type 0) -> (B : Type 0) -> A) : Type 1

(s = 5, T = (A : Type 1) -> (B : Type 1) -> A) : Type 1


// naive
-----------------
Γ |- Size : USize

-------------
Γ |- ω : Size

Γ |- a : Size
------------------
Γ |- lift a : Size

Γ |- a : Size  Γ |- b : Size
----------------------------
Γ |- max a b : Size

Γ |- a : Size
---------------------------
Γ |- Type a : Type (lift a)

Γ |- A : Type (b < a)  Γ, x : A |- B : Type (c < a)
---------------------------------------------------
Γ |- (x : A) -> B : Type (a

Γ, [a] |- T : Type b
----------------------
Γ |- [a] -> T : Type ω

[x] -> [y < x] -> (A : Type 1) -> A

[a] =>

(x : Int) => x + 2 : (x : Int) -> Int

Id : Type 2 = (A : Type 1) -> A -> A;
Nat = (A : Type 0) -> A -> (Nat -> A) -> A;



-----------------
Γ |- Size : USize

Γ |- a : Size
------------------
Γ |- lift a : Size

Γ |- a : Size  Γ |- b : Size
----------------------------
Γ |- max a b : Size

Γ, a : Size |- b : Size  Γ, x : A |- B : Type
---------------------------------------------
Γ |- (x [a] : A) -[b]> B : Type

Γ, a : Size |- b : Size  Γ, x : A |- B : Type
---------------------------------------------
Γ |- (x : A) -[b]> B : Type

Γ |- M : (x : A % ) -[b]> B  Γ |- N : A
----------------------------------------
Γ |- M N : B[x := N] | n

Γ |- A : Type a  Γ, x : A |- B : Type b
---------------------------------------
Γ |- (x : A) -> B : Type (max a b)

Γ |- n : Grade  Γ, x : A |- B : Type b
--------------------------------------
Γ |- (x : A $ n) -> B : Type b

Fix = (x : Fix % 2) -> ();
fix = (x : Fix % 2) => x

Nat = n @-> (l : Level) -> (A : Type l $ 0) -> (zero : A $ 1) -> (succ : (acc : A $ 1) -> A $ l) -> A;

Id : Type = (A : Type) -> (x : A) -> A;
id : Id = (A : Type) => (x : A) => x;
id Id id

Nat : Type = (A : Type) -> A -> (A -> A) -> A;



Id : Type = (A : Type) -> (x : A % 2) -> A;

(fix) => fix fix;

Data = W @-> (T : Type, clone : (x : T) -> (l : W, r : W));

Unit : Type;
unit : Unit;
clone : (u : Unit) -> (l : Unit, r : Unit);

Unit = (A : Type) -> A -> A;
unit = A => x => x;
clone = u => u _ (unit, unit);


Unit [a] : Type;
unit [a] : Unit [1 + a];

Unit [a] = (u : (A : Type) -> A -> A, clone : [b < a] -> (l : Unit [b], r : Unit [b]));
unit [a] = (A => x => x, [b < a] => (unit [b], unit [b]));




Type l : Type (lift l);
(x : A : Type a) -> (B : Type b) : Type (max a b);

Bool : Type 0 = (A : Type ω $ 0) -> A -> A -> A;


Bool : Type 1 = (A : Type 0) -> A -> A -> A;
Bool : Type 1 = (l : Level) -> (A : Type l) -> A -> A -> A;

Bool : Type 0 = (A : Type ω) -> A -> A -> A;

Bool : Type 0 = b @-> (P : Bool -> Type ω $ 0) -> P true -> P false -> P b;
```

## Restrictions

Usage tracking, grading prevents calling itself because eliminating a function requires one copy, plus the required copies from the the function itself, requiring n = n + 1 to work. Pro, can still call id with id.

Size tracking, predicativity prevents calling itself because a Type universe will never include itself, so a function can never take itself.

Why is impredicativity safe sometimes?

## Teika

```rust
U = (A : Type, (A = A, A -> ()) -> ());
V = (A = U, A -> ());

V <: U;
(A = U, A -> ()) <: U
(A = U, A -> ()) <: (A : Type, (A = A, A -> ()) -> ())
(A = U, A -> ()) <: (A = U, (A = A, A -> ()) -> ())
U -> () <: (A = U, A -> ()) -> ()
(A = U, A -> ()) <: U



constraint = ['b < 'a];
constraint = ['b = 1 + 'a];

U : Type (1 + 'a) = (A : Type 'a, (A = A, A -> ()) -> ());
V = (A : Type (1 + 'a) = U, A -> ());

V <: (A : Type 'a, (A = A, A -> ()) -> ())
(A : Type (1 + 'a) = U, A -> ()) <: (A : Type 'b, (A = A, A -> ()) -> ())

1 + 'a = 'a

Id : Type 1 = (A : Type 0) -> (x : A) -> A;
id : Id = A => x => x;

Resource : Type;
Data : Type;
Property : Type;


U # 1 + 'u = (A # 1 : Type, (A # 1 = A, A -> ()) -> ());
V # 'v = (A = U, A -> ());

(A # 1 = U, A -> ()) # 'v <: U # 1 + 'u
(A # 1 = U, A -> ()) # 'v <: (A # 1 : Type, (A = A, A -> ()) -> ()) # 1 + 'u
(A # 1 = U, A -> ()) # 'v <: (A # 1 = U, (A = A, A -> ()) -> ()) # 1 + 'u
// clash, as 'u = 'u + 1


U = (A : Type, (A = A, A -> ()) -> ());
V = (A = U, A -> ());

V <: U;
(A = U, A -> ()) <: U


erase : (A : Type, x : P A) -> (A : Type, x : P A)


Univ : Univ;
Type : Univ;
Data : Univ;
Prop : Univ;

(b : Bool : Type) -> (() -> DNat) $ 1
(b : Bool) => (() => b DNat 0 1)

Memory : {
  type(mem : Memory, ptr : Pointer) : Type;
  get(mem : Memory, ptr : Pointer) : type(mem, ptr);
  set<A>(mem : Memory, ptr : Pointer, value : A) : Memory;
};
Rc<mem, A> = {
  ptr : Pointer;
  valid : Memory.length(mem) < ptr;
  witness : Memory.type(mem, ptr) == A;
};

℘S == S -> Type;

U : Type = (X : Type) ->
  (((X -> Type) -> Type) -> X) -> ((X -> Type) -> Type) -> X;
τ (t : ℘℘U) : U = (X : Type) => |f : ℘℘X -> X| => (p : ℘X) =>
  t ((x : U) => p (f (x X f)));
σ (s : U) : ℘℘U = s U τ;
Ω : U = τ ((p : ℘U) => (x : U) -> [σ x p ==> p x]);

U : Type = (X : Type) -> (℘℘X -> X) -> ℘℘X;
Ω : U = (X : Type) => |f : ℘℘X -> X| => (p : ℘X) =>
  ((p : ℘U) => (x : U) -> [σ x p ==> p x]) ((x : U) => p (f (x X f)));

Ω : U = (X : Type) => (f : ℘℘X -> X) => (p : ℘X) =>
  (x : U) -> [x U ((x : U) => p (f (x X f))) ==> p (f (x X f))] ;

(X : Type) -> ((G X) -> X) -> X;
(X : Type) -> (((R : Type) -> R -> (X -> R) -> R) -> X) -> X;

Id : Type 0 = (A : Type 0 $ 1) -> (x : A) -> A;

Nat = (A : Type)


(l : Level, x : Type (1 + l))


type M = Int -> Int;

M = Int -> Int;

Id = (A : Type) -> (x : A) -> A;
Id = ∀A. A → A;


Id = <A>(x : A) -> A;

T = <A, B>(x : A, y : B) -> A;
U = <A, B>(x : A, y : B) -> B;


Eq A x y = (P : A -> Type) -> P x -> P y;
refl A x : Eq A x x = P => p => p;

(eq : A x x) -> Eq _ eq (refl A x)

Eq A x y = (P : A -> Type) -> P x -> P y;
refl A x : Eq A x x = P => p => p;

id = <A>(x : A) => x;


Unit = <A>(x : A) -> A;
Bool = <A, B>(x : A, y : A) -> A;


Type 0 : Type 1;
Type l : Type (1 + l);
Type ∞ : Type ∞;

((A : Type ∞) -> (x : A) -> A) : Type ∞

T = <A, B>(x : A, y : B) -> A;
U = <A, B>(x : A, y : B) -> B;

NatA = | Zero | Succ (pred : NatA);
NatB = <A>(zero : A, succ : (pred : NatB) -> A) -> A;

NatA = <A>(zero : A, succ : (acc : A) -> A) -> A;
NatB = <A>(succ : (acc : A) -> A, zero : A) -> A;

T = <A, B>(x : A, y : B) -> A;
U = <A, B>(x : A, y : B) -> B;

Type 0 : Type 1;
<A : Type 0>(x : A) -> A;

Inductive circle : Type :=
  | base : circle
  | loop : base == base.

(Type 0 : Type 1)

(Set : Type 0);
(Type l : Type (1 + l));

* : □ : Δ


(A : *) -> A : *
(A : *) -> (B : □) -> A : *
(A : □) -> (B : Δ) -> A : Δ

Type 0 : Type 1
Type 1 :
(B : □) -> (A : *) -> A : *

Type l : Type (1 + l);
Data l : Type (1 + l);
Prop l : Type (1 + l);

Type : Type;
Data : Type;
Prop : Type;

Type : Type;
Prop : Type;

PNever : (A : Prop) -> A;
TNever : (A : Type) -> A;

never_magic : PNever -> TNever = _;

PUnit = (A : Prop) -> (x : A) -> A;
TUnit = (A : Type) -> (x : A) -> A;

unit_magic : PUnit -> TUnit = _;

T = <A>(x : A) -> <B>(y : B) -> A;
U = <A, B>(x : A) -> (y : B) -> A;

id = (A : Prop) => (x : A) => x;

id ((A : Type) -> A)

f : (A : Type) -> (B : Prop) -> B : Prop;


(l = 2, A = Type (l + l)) : (l : Level, A : Type (1 + l + l)) : Type 2
(l = 2, A = Type l) : (l : Level, A : Type (1 + l)) : Type 2

Type l : Type (1 + l);
Data l : Type (1 + l);
Prop l : Type (1 + l);

{ A : Data l: ; x : A }

Bool : Data 2;
true : Bool;
false : Bool;

Bool = b @-> (
  d : (P : Bool -> Data 1) -> P true -> P false -> P b &
  t : (P : Bool -> Type 1) -> P true -> P false -> P b
);
true : Bool = (P => x => y => x & P => x => y => x);
false : Bool = (P => x => y => y & P => x => y => y);


Sigma A B = s @-> (P : Sigma A B -> Data 0) ->
  (k : (x : A) -> (y : B x) -> P (exist A B x y)) -> P s;

U : Data 2 = (X : Data 1) ->
  (((X -> Data 0) -> Data 0) -> X) -> ((X -> Data 0) -> Data 0) -> X;
τ (t : ℘℘U) : U = (X : Type) => |f : ℘℘X -> X| => (p : ℘X) =>
  t ((x : U) => p (f (x X f)));
σ (s : U) : ℘℘U = s U τ;
Ω : U = τ ((p : ℘U) => (x : U) -> [σ x p ==> p x]);

Data : Type;
Prop : Type;


id : (A : Data) -> (x : ⇑A) -> ⇑A = _;

Unit = (A : Data) -> (x : A) -> A;

(u : Unit) => (A : Type) => (x : A) => u A x;

Unit = u @-> (P : Unit -> Data) ->
(A : Data) -> (x : A) ->

Data l : Type (1 + l);
Type l : Type (1 + l);


Id : Data = (A : Data : Type) -> (x : A : Data) -> A;
Id : Type = ⇑Id;

id = (A : Data) => (x : A) => x;

Type l : Type (1 + l); // no elimination to Data
Data l : Type (1 + l); // no universes in Data

Unit : Data 1;
unit : Unit;

Unit = u @-> (
  d : (P : Unit -> Data 0) -> P unit -> P u,
  T : (P : Unit -> Type 0) -> P unit -> P u
);
unit = (
  d = P => x => x,
  T = P => x => x,
);



S = {
  A : Data;
  x : A;
};
id = (A : Data) => (x : A) => x;

x = id (@squash Type)
Eq A B = (P : Data -> Data) -> P A -> P B;

⇑(A : Data) => (x : A) => x
(A : ⇑Data) => (x : ⇑~A) => x
lifted_id : (A : Type) -> (x : A) -> A = ⇑id;


```

```rust

// rules
// Those rules they require mutually recursive blocks to be useful
Γ, x : A |- B : Type
---------------------------
Γ |- @self(x : A). B : Type

Γ, x : A |- M : B[x := @fix(x : A). M]  Γ |- A ≂ @self(x : A). B
----------------------------------------------------------------
Γ |- @fix(x : A). M : @self(x : A). B

Γ |- M : @self(x : A). B  Γ |- A ≂ @self(x : A). B
--------------------------------------------------
Γ |- @unroll M : B[x := M]



Γ, x : A |- B : Type  Γ |- A ≂ @self(x : A). B
----------------------------------------------
Γ |- @self(x : A). B : Type

/*
3 general steps

Describe the type indexed by the constructors of the type
Make the type family itself
Close the family on the constructors

The following code assumes implicit unroll
*/

// shape of family
T_P_Unit : Type;
T_p_unit : (P_Unit : T_P_Unit) -> Type;

T_P_Unit = @self(P_Unit : T_P_Unit). (p_unit : T_p_unit P_Unit) -> Type;
T_p_unit P_Unit = @self(p_unit : T_p_unit P_Unit). P_Unit p_unit;

// proto Unit
P_Unit : T_Unit;
p_unit : T_p_unit P_unit;

P_Unit p_unit = @self(u : P_Unit p_unit).
  (P : P_Unit p_unit -> Type) -> P p_unit -> P u;
p_unit = @fix(p_unit). P => (x : P p_unit) => x;

// closing
Unit = P_Unit p_unit;
unit : Unit = p_unit;

T_False : Type;
T_f : (False : T_False) -> Type;

= @self(f : T_False)

Unit : Type;
unit : Unit;

Unit = @self(u : Unit). (P : Unit -> Type) -> P unit -> P u;
unit = (P : Unit -> Type) => (x : P unit) => x;

data Z4 : Set where
  zero : Z4
  suc : Z4 -> Z4
  mod : suc (suc (suc (suc zero))) ≡ zero


// Those rules they require mutually recursive blocks to be useful
Γ, x : A |- B : Type
---------------------------
Γ |- @self(x : A). B : Type

Γ, x : A |- M : B[x := @fix(x : A). M]  Γ |- A ≂ @self(x : A). B
----------------------------------------------------------------
Γ |- @fix(x : A). M : @self(x : A). B

Γ |- M : @self(x : A). B  Γ |- A ≂ @self(x : A). B
--------------------------------------------------
Γ |- @unroll M : B[x := M]


False @-> (f : f @-> @False f) -> Type;

// induction-induction
A : Type;
B : (K : A) -> Type;

// my-stuff
T_False : Type;
T_f : (False : T_False) -> Type;

T_False = @self(False : T_False). (f : T_f False) -> Type;
T_f False = @self(f : T_f False). @False f;



@self(False : T_False). (f : @self(). ) -> Type;

@self(A : Type, T_f : _).
  (False : @self(False :)) => @False f

T_f : @self(A, T_f : ). (False : @self(False). (f : T_f) -> Type) -> Type;
T_f = @fix(T_f : _). False => @self(f : @T_f False). @False f;

@exist(T_False : Type).
  @self(False :
    @self(False : T_False). (f : @exist(T_f : Type). T_f) T_False).
    (f : @exist(T_f : Type). @self(f : T_f). @False f) -> Type;
@both(
  T_False : Type = @self(False : T_False). (f : T_f False) -> Type,
  T_f : (False : T_False) -> Type = False => @self(f : T_f False). @False f
)

Γ |- T : @both(x : A = M, y : B = N)
------------------------------------
Γ |- @fst T : M[x := M | y := N]

Γ |- T : @both(x : A = M, y : B = N)
------------------------------------
Γ |- @snd T : N[x := M | y := N]



Γ, x : Type |- B : Type  Γ, y : B |- M : Type  Γ |- N[x := M] : B
-----------------------------------------------------------------
Γ |- @ind(x = M, y : B = N) -> Type


Γ, A : Type, x : A |- B : Type
-------------------------------
Γ |- @self(A, x : B). T : Type

@self(_, T_False : Type).
  @self(False : T_False). (f : @self(T_f, f : T_f). ) -> Type;

@self(A, T_f : A -> Type).
  False => @self(B, f : T_f False). _ => @False f

@self(False, f). False =>

@self(False : T_False). (f : @self(f : T_f). @False f) -> Type;



-----------------
Γ |- Self : USelf

Γ, x : @self(x). T |- T : Type
-------------------------------
Γ |- @self(x). T : Type

Γ |- M : @self(A, x). T
--------------------------
Γ |- @unroll M : T[x := M]

Γ, A : Type, x : A |- T : Type
-------------------------------------
Γ |- @self(A, x). (y : B) => T : A -> Type

False @-> (f : f @-> @False f) -> Type;

@self(T_False, False : @self(_, False : T_False). (f : )).
  (f : @self(T_f, f : T_f). @False f) -> Type;

Γ, A : Type, x : A |- T : Type
------------------------------
Γ |- @self(A, x). T : Type

Unit = @self(Unit, u). (unit : Unit) -> (P : Unit -> Type) -> P unit -> P u;

False = False @-> (f : f @-> @False f) -> Type;

A : Type;
B : (x : A) -> Type;

A = @self(x : A). (f : B x) -> Type;
B False = @self(f : B x). @x f;

A : Type;
B : (x : A) -> Type;

A = _;
B x = _;

Γ, x : A |- T : Type
---------------------------
Γ |- @self(x : A). T : Type

Nat = | Zero | Succ (pred : Nat);

Unit : Type;
unit : Unit;

Unit = @self(u : Unit). (P : Unit -> Type) -> P

Γ, x : A |- T : Type
------------------------------
Γ |- @self(A, x). T : Type

Γ, A : Type, x : A |- T : Type
------------------------------
Γ |- @self(A, x). T : Type



Type : Type = _;
Data : Type = _;

Mu G = (X : Type) -> (G X -> X) -> X;
Nat_G INat = (K : Type) -> X -> (INat -> Type) -> K;

Nat X = (K : Type) -> (A : Type) -> (k : A -> K) -> (z : A) ->
  (s : (k : A -> K) -> (z : A) -> (p : X) -> K) -> K;
Nat_n = (X : Type) -> (Nat X -> X) -> X;

Show : Data = {

};

T_False : Type;
T_f : (False : T_False) -> Type;

T_False = (False : T_False) @-> (f : T_f False) -> Type;

μ T. (eq : μ E. (T == μ T. (eq : E) -> _)) -> _;
@self(u : Unit).


(X : Type) -> (G X -> X) -> X;

Γ, x : A |- T : Type
---------------------------
Γ |- @self(x : A). T : Type

Γ, x |- , x : A |- T : Type
-------------------------------------
Γ |- @fix(x : A). M : @self(x : A). T


T_P_Unit : Type;
T_p_unit : (P_Unit : T_P_Unit) -> Type;

T_P_Unit = @self(P_Unit : T_P_Unit). (p_unit : T_p_unit P_Unit) -> Type;
T_p_unit P_Unit = @self(p_unit : T_p_unit P_Unit). P_Unit p_unit;

P_Unit : T_Unit;
p_unit : T_p_unit P_unit;

P_Unit p_unit = @self(u : P_Unit p_unit).
  (P : P_Unit p_unit -> Type) -> P p_unit -> P u;
p_unit = @fix(p_unit). P => (x : P p_unit) => x;



Γ, x : A, y : B |- M : A  Γ, x == M, y : B |- N : B
---------------------------------------------------
Γ |- { x : A, y : B, x = M, y = N } : Type


Γ, x : A, y : B |- M : A  Γ, y : B[x := M] |- N : B[x := M]
------------------------------------------------------------
Γ |- (x : A, y : B, x = M, n = N) : (x : A, y : B)

Γ |- M : (x : A, y : B)
------------------------------------------------------------
Γ |- M.x : A[x := fst M][y := snd M]

Γ |- M : (x : A = , y : B)
------------------------------------------------------------
Γ |- (x, y) = M; N : T

Γ, x : A, y : B |- M : A  Γ, y : B[x := M] |- N : B[x := M]
------------------------------------------------------------
Γ |- (x : A; T) : (x : A, y : B)

Γ, x : A, y : B | T) : (x : A, y : B)





Γ, x : A |- M : A  Γ, x == M |- N : B
-------------------------------------
Γ |- (x : A; x = M; x) : B[x := M]

Γ, x : A |- M : A  Γ, x == A |- N : Type
----------------------------------------
Γ |- (x : A; x = M; N) : Type

Γ, x : A |- M : A  Γ, x == M |- N : B
------------------------------------------
Γ |- (x : A; x = M; N) : (x : A; x = M; B)

T_Unit : Type;
Unit : T_Unit;

T_Unit = (unit : Unit) -> Type;
Unit = unit =>


Unit : Type;
unit : Unit;

Unit = (u : Unit) @-> (P : Unit -> Type) -> P unit -> P u;
unit = (u : Unit) @=> P => x => x;



False : Type;
False = (f : False; f : (P : False -> Type) -> P f);

Unit : Type;
unit : Unit;

Unit = (u : Unit) & (P : Unit -> Type) -> P unit -> P u;
unit = (P : Unit -> Type) => (x : P u) => x;

T_Unit : Type;
T_unit : (Unit : T_Unit) -> Type;

T_Unit = (Unit : T_Unit) & (unit : T_unit Unit) -> Type;
T_unit = (unit : T_unit Unit) & Unit

Γ |- M : B  Γ, x == A |- M : B  Γ |- A ≂ (x : A) & B
--------------------------------------------
Γ |- M : (x : A) & B

(u : Unit & (P : Unit -> Type) -> P unit -> P u)

Unit : T_Unit;
Unit = (
  @Unit : (unit : T_unit Unit) -> Type;
  @Unit (unit : T_unit Unit)  =
)

M = (
  Unit : Type;
  unit : Unit;

  Unit = @self(u : Unit). (P : Unit -> Type) -> P unit -> P u;
  unit = @fix(u : Unit). P => x => x;
)

M.Unit === @self(u : M.Unit). (P : M.Unit -> Type) -> P unit -> P u
T_Unit = ()
Unit : Type;
unit : Unit;


Unit = @self()

T_Unit : Type;
T_unit : (Unit : T_Unit) -> Type;

T_Unit = (Unit : T_Unit) & (unit : T_unit Unit) -> Type;
T_unit = (Unit : T_Unit) => (unit : T_unit Unit) & Unit;


Bool : Type;
true : Bool;
false : Bool;

Bool = @self(b). (P : Bool -> Type) -> P true -> P false -> P b;
true = P => x => y => x;
false = P => x => y => y;


Bool : Type;
true : Bool;
false : Bool;

Bool = @self(b : Bool). (P : Bool -> Type) -> P true -> P false -> P b;
true = P => x => y => x;
false = P => x => y => y;


Bool = (b : (P : Type) -> P -> P -> P) & (P : Bool -> Type) -> P true -> P false -> P b;
true = (P : ?) => x => y => x;

M : (x : A) & B;
N : (x : A) & B;

M.0 == N.0 -> M == N;


C_Bool : Type;
c_true : C_Bool;
c_false : C_Bool;

C_Bool = (A : Type) -> A -> A -> A;
c_true = A => x => y => x;
c_false = A => x => y => y;

I_Bool : (c_b : C_Bool) -> Prop;
i_true : I_Bool c_true;
i_false : I_Bool c_false;

I_Bool c_b = (P : C_Bool -> Prop) -> P c_true -> P c_false -> P c_b;
i_true = P => x => y => x;
i_false = P => x => y => y;

Bool : Type = (c_b : C_Bool, I_Bool c_b);
true : Bool = (c_b = c_true, i_true);
false : Bool = (c_b = c_false, i_false);

Bool : Type;
true : Bool;
false : Bool;

Bool = (b : (A : Type) -> A -> A -> A &
  (P : Bool -> Prop) -> P true -> P false -> P b);
true = (b = A => x => y => x,);
false = P => x => y => y;



expected : (P : Bool -> Type) -> P true -> P false -> P true
received : (P : Bool -> Type) -> P true -> P false -> P true

Type : Type;
Data : Type;
Prop : Type;

(x : Nat & x >= 5);
(x = 5 & le_refl)

(a : (x : Nat & x >= 5)) => (b : (x : Nat & x >= 5)) => (eq : fst a == fst b) =>
  a == b
(A : Prop) => (x : A) => (y : A) => (refl : x == y)

Unit : Type;
unit : Unit;

Unit = (P : Unit -> Type) -> P unit -> P unit;
unit = (P : Unit -> Type) => (x : P unit) => x;


t = (False : Type) => (f : A) => (False == (P : False -> Type) -> P f) =>
  (f )

False : Type;
False = <K>(k : <F>(f : F, F == (P : F -> Type) -> P f) -> K) -> K;

Unit : Type;
Unit = <K>(k : <U>(u : F, F == (P : F -> Type) -> P f) -> K) -> K;

Unit : Type;
Unit = <K>(k : <U>(u : F, F == (P : F -> Type) -> P f) -> K) -> K;



MUnit Unit unit u = (P : Unit -> Type) -> P unit -> P u;


t = (T : Type) =>
  (unit : A) => (A == T unit)
  (U : Type) => (u : U) => (U == (P : _ -> Type) -> P unit -> P u) =>

Bool : Set 1
true : Bool
false : Bool

Bool = ι(b : Bool). ∀(P : Bool -> Set 0). P true → P false → P b
true = λ(P : Bool -> Type). λ(then : P true). λ(else : P false). then
false = λ(P : Bool -> Type). λ(then : P true). λ(else : P false). else



False : Type;
False = <K>(k : <F>(f : F, F == (P : F -> Type) -> P f -> ()) -> K) -> K;

T_dumb : Type;
dumb : T_dumb;

T_dumb = (P : T_dumb -> Type) -> P dumb -> ();
dumb = P => x => ();

false : False;
false = k => k(dumb, refl);


P_Unit : Type;
p_unit : P_Unit;

P_Unit = (P : P_Unit -> Type) -> P p_unit -> P p_unit;
p_unit = P => x => x;

WBool : Type;
WBool = <K>(k : <Bool>(true : Bool) -> (false : Bool) ->
  (ind : (b : Bool) -> (P : Bool -> Type) -> P true -> P false -> P b) ->
  (ind_true : ind true == P => x => y => x) ->
  (ind_false : ind false == P => x => y => y) ->
  (b : Bool) -> K
  K) -> K;

WBool : Type;
WBool = <K>(k :
  <Bool>(true : (A : Type) -> Bool A) ->
    (false : (A : Type) -> Bool A) ->
  <B>(b : Bool B) ->
    (ind : (P : Bool B -> Type) -> P (true B) -> P (false B) -> P b) ->
  K
  K) -> K;

MBool : _ -> Type;

Bool : _ -> Type;
true : _ -> Bool _;
false : _ -> Bool _;
ktrue : Bool (true true);
kfalse : Bool (false false);

Bool b = (P : Bool b -> Type) -> P (true b) -> P (false b) -> P b;
true b =
ktrue = (P : Bool ktrue -> Type) => (x : P ktrue) => (y : P kfalse) => x;
kfalse = (P : Bool ktrue -> Type) => (x : P ktrue) => (y : P kfalse) => y;
false b = (P : Bool b -> Type) => (x : P (true b)) => (y : P (false b)) => y;
T_true = Bool;
T_false = Bool;

Bool b = (P : Bool b -> Type) -> P true -> P false -> P b;





Unit : Type;
unit : Unit;

Unit = (u : Unit) & (P : Unit -> Type) -> P unit -> P u;
unit = (P : Unit -> Type) => (x : P unit) => x;



Bool : Type;
true : Bool;
false : Bool;
MBool : (b : Bool) -> Type;

MBool b = (P : Bool -> Type) -> P true -> P false -> P b;
Bool =

Bool b = (P : T_b -> Type) -> P (true b) -> P (false b) -> P b;


f =  <B>(b : Bool B) -> (ind : (P : Type -> Type) -> P T -> P F -> P B) ->

A : Type;
B : A -> Type;

Sigma : Type;
sigma : (x : A) -> (y : B x) -> Sigma A B;

s : Sigma A B;
Sigma = A => B => (P : Sigma A B -> Type) ->
  (k : (x : A) -> (y : B x) -> P (sigma A B x y)) -> P s;

sigma = A => B => x => y => P => k => k x y;

A : Type;
B : A -> Type;

f = (S : Type) => (s : S) =>
  ()
  (fst : (s : S) -> A) =>
  (snd : (s : S) -> B (fst s)) => _;

Sigma A B = s @-> (P : Sigma A B -> Type) ->
  ((x : A) -> (y : P x) -> P (sigma A B x y)) -> P s;

T : Type;

f (K => k => k x y)
  (s => )


f = <F>(f : F) => (F == (P : F -> Type) -> P f) =>


Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool & (P : Bool -> Type) -> P true -> P false -> P b);

M :


expected : b Type True False
received : b.x Type True False

Γ, x : A |- M : B[x := @fix(x : A). M]  Γ |- A ≂ @self(x : A). B
----------------------------------------------------------------
Γ |- @fix(@(x : A)). M : @self(x : A). B

T == (P : Unit -> Type) -> P unit -> P unit
B == (P : Unit -> Type) -> P unit -> P unit


(F : Type) => (F == @self(x : F). (P : F -> Type) -> x) =>

B
---------------
M ⇐ (x : A & B);


Γ, x : A |- M : B[x := M]  Γ |- A ≂ @self(x : A). B
---------------------------------------------------
Γ |- @fix(@unfold x : B). M : @self(x : A). B

Γ, x : A |- M : B[x := M]  Γ |- A ≂ @self(x : A). B
---------------------------------------------------
Γ |- @fix(@fold x : B). M : @self(x : A). B

Unit : Type;
unit : Unit;

Unit = @self(u : Unit). (P : Unit -> Type) -> (x : P unit) -> P u;
unit = @fix(@unfold u). (P : Unit -> Type) => (x : P (@fold u)) => x;


T_Unit : Type;
T_unit : (Unit : T_Unit) -> Type;

T_Unit = (Unit : T_Unit & (unit : T_unit Unit) -> Type);
T_unit = (unit : T_unit Unit & Unit unit);

Unit : T_Unit;

Unit unit = (u : Unit unit & (P : Unit unit -> Type) -> P unit -> P u);

PT_unit : Type;
unit : PT_unit;

unit : T_unit Unit;
unit = (P : Unit unit -> Type) => (x : P unit) => x;



Unit : Type;
unit : Unit;

Unit = (u : Unit) & (P : Unit -> Type) -> P unit -> P u;
unit = (P : Unit -> Type) => (x : P unit) => x;

// check
(unit : (P : Unit -> Type) -> P unit -> P unit) <: Unit

// rule



((M : A) :> T) == ((N : A) :> T)

M == N
(T_Unit : Type; T_Unit = (Unit : T_Unit))


Bool : Data 1;
true : Bool;
false : Bool;

Bool = (b : Bool & (P : Bool -> Data 0) -> P true -> P false -> P b);
true = (P : Bool -> Data 0) => (x : P true) => (y : P false) => x;
false = (P : Bool -> Data 0) => (x : P true) => (y : P false) => y;


// core
------------------------
Γ |- Type : Type

Γ, x : A |- B : Type
------------------------
Γ |- (x : A) & B : Type

Γ |- x : A |- B : Type
------------------------
Γ |- (x : A) -> M : Type

Γ, x : A |- M : B
--------------------------------
Γ |- (x : A) => M : (x : A) -> B

Γ |- M : (x : A) -> B  Γ |- N :> A
----------------------------------
Γ |- M N : B[x := N]

// coercion
Γ |- M : T
-----------
Γ |- M :> T

Γ |- M :> A  Γ |- M :> B[x := M]
--------------------------------
Γ |- M :> (x : A) & B

Γ |- M :> (x : A) & B
---------------------
Γ |- M :> A

Γ |- M :> (x : A) & B
---------------------
Γ |- M :> B[x := M]


// base
Γ |- M ⇒ T
-----------
Γ |- M ⇐ T

Γ |- M ⇐ T
----------------
Γ |- (M : T) ⇒ T

// universe
------------------------
Γ |- Type ⇒ Type

// forall
Γ |- A ⇐ Type  Γ, x : A |- B ⇐ Type
-----------------------------------
Γ |- (x : A) -> M ⇒ Type

Γ, x : A |- M ⇐ B
--------------------------------
Γ |- x => M ⇐ (x : A) -> B

Γ |- M ⇒ (x : A) -> B  Γ |- N ⇐ A
----------------------------------
Γ |- M N ⇒ B[x := N]

// intersection
Γ |- A ⇐ Type  Γ, x : A |- B ⇐ Type
-----------------------------------
Γ |- (x : A) & B ⇒ Type

Γ |- M ⇐ A  Γ |- M ⇐ B[x := M]
--------------------------------
Γ |- M ⇐ (x : A) & B

Γ |- M ⇒ (x : A) & B
---------------------
Γ |- M ⇐ A

Γ |- M ⇒ (x : A) & B
---------------------
Γ |- M ⇐ B[x := M]


T_Unit : Type;
T_Unit = (Unit : T_Unit) &
  (unit : (T_unit : Type; (unit : T_unit) & T_Unit)) -> Type

// induction-induction
T_Unit : Type;
T_unit : (Unit : T_Unit) -> Type;

T_Unit = (Unit : T_Unit) & (unit : T_unit Unit) -> Type;
T_unit = Unit => (unit : T_unit Unit) & Unit unit;

// fixpoint
Unit : T_Unit;
Unit unit = (u : Unit unit) & (P : Unit unit -> Type) -> P unit -> P u;

// fixpoint
unit : T_unit Unit;
unit = @fix(unit : T_unit Unit). P => x => x;

Γ |- A : Type  Γ, x : A |- A : Type
---------------------------------------
Γ |- (x : A; y : B; x = M; y = N; K) ⇐ (x : A) & B

Γ |- A ≡ (x : A) & B  Γ, y : A |- M ⇐ B
---------------------------------------
Γ |- @fix(x). M ⇐ (x : A) & B

False : Type;
False = (f : False) & (P : False -> Type) -> P f;

T_Unit : Type;
T_Unit = (Unit : T_Unit) & (A : Type) -> (unit : ) -> Type;

Unit : Type;
Unit = (u : Unit) & (P : Unit -> Type)

(u : Unit) & (P : Unit -> Type) ->

Unit : Type;
unit : Unit;

Unit = (u : Unit) & (P : Unit -> Type) -> P unit -> P u;
unit = @fix(unit). P => x  => y

Unit == @fix(Unit). (u : Unit) & (P : Unit -> Type) -> P unit -> P u;

[] |- (unit : (P : Unit -> Type) -> P unit -> P unit) :> Unit
[unit : (P : Unit -> Type) -> P unit -> P unit :> Unit] |-
  received : (P : Unit -> Type) -> P unit -> P unit
  expected : (u : Unit) & (P : Unit -> Type) -> P unit -> P u

  [unit : (P : Unit -> Type) -> P unit -> P unit :> Unit] |-
    received : (P : Unit -> Type) -> P unit -> P unit
    expected : (P : Unit -> Type) -> P unit -> P unit


  [] |- (unit : (P : Unit -> Type) -> P unit -> P unit) :>
    (u : Unit) & (P : Unit -> Type) -> P unit -> P u
P : A -> Type;
P (M : A & B) == P (N : A)
(M : A & B) == (N : A) : A

Γ |-


(x : Nat & x >= 5) => x + 1

f : () -> Nat;

x = 1;


f = () => f ();

Type 0 : Type 1
Type 1 : Type 2

id : (A : Type 0) -> (x : A) -> A
  = (A : Type 0                                                                                                                                                                                                                                                                                                     ) => (x : A) => x;

Show = {
  T : Type 0;

}


Unit : Type;
unit : Unit;

Unit = (u : Unit & (P : Unit -> Type) -> P unit -> P u);
unit = (P : Unit -> Type) => (x : P unit) => x


Γ, x : A, y : B |- M : A  Γ, x : A, y : B, x == M |- N : B
Γ, x : A, y : B, x == M, y == N |- K : T
----------------------------------------------------------------------
Γ |- (x : A; y : B; x = M; y = N; K) : (x : A; y : B; x = M; y = N; T)


Γ |- M : A  Γ, x == M |- K : T
------------------------------
Γ |- (x = M; K) : (x = M; T)



f : <A, B>(x : A, y : B) -> A;
g : <A, B>(x : A, y : B) -> B;


f : <A, B>(x : A, y : B) -> A;

Hot : Type;
Hot = T @-> (A : Type, eq : (B : Type) -> A =~= B -> A == B);

Set : Type;

Set = (A : Type, eq : (P : ) -> (x : A) -> -> P (eq == refl));

Unit : Type;
unit : Unit;
uip : (u : Unit) -> (eq : u == u) -> eq == refl;

SUnit : Type;
SUnit_eq : (s : SUnit) -> Type;
SUnit_refl : (s : SUnit) -> SUnit_eq s;

SUnit = s @-> (x : Unit, eq : SUnit_eq);
SUnit_eq s = e @-> (eq : s == s) ->
  (P : s == s -> SUnit_eq s -> Type) -> P refl SUnit_refl -> P s e;
SUnit_refl s =

Unit : Set;
Unit = (A = unit)

Prop : Type;
Prop = T @-> (
  A : Type,
  eq : (x : T) -> (y : T) -> (P : T -> Type) -> P x -> P y
);

Unit : Set;


Γ, x : @self(x). T |- T : Type
------------------------------
Γ |- @self(x). T : Type

Γ, x : @self(x). T |- M : T
---------------------------------
Γ |- @fix(x) : T. M : @self(x). T

Γ |- M ⇒ @self(x). T
--------------------
Γ |- M ⇐ T[x := M]



T_Unit : Type = @self(Unit). _ -> _ -> _ -> Type;
T_unit : Type = _;

Unit : T_Unit = @fix(Unit). I0 => unit => I1 => (
  Unit = @unroll Unit I0 unit I1;
  I0 = @unroll I0 unit I1;
  unit = @unroll Unit I1;
  @self(u). (P : Unit -> T'ype) -> I0 P unit -> I0 P u
);
I0 = _;
unit : T_unit = @fix(unit). I1 =>
  @fix(u). (P : Unit -> Type) => (x : P unit) => I1 P x;
unit : @self(unit). @unroll Unit unit = @fix(unit).


T_Unit : Type;
T_unit : (Unit : T_Unit) -> Type;

T_Unit = @self(Unit : T_Unit). (unit : T_unit Unit) -> Type;
T_unit = Unit => @self(unit : T_unit Unit). @unroll Unit unit;

Unit : T_Unit = @fix(Unit). unit => @self(u : @unroll Unit unit).
  (P : @unroll Unit unit -> Type) -> (x : P unit) -> P u;
unit : @self(unit : T_unit Unit).
  @self(u : @unroll Unit unit).
  (P : @unroll Unit unit -> Type) -> (x : P (@unroll unit)) -> P u = @fix(unit).

M : @self(unit : T_unit Unit). @self(u : Unit unit).
  (P : Unit unit -> Type) -> (x : P unit) -> P u
@unroll M : @self(u : Unit M). (P : Unit M -> Type) -> (x : P M) -> P u
@unroll (@unroll M) : (P : Unit M -> Type) -> (x : P M) -> P (@unroll M)


T_Unit : Type;
T_unit : (Unit : T_Unit) -> Type;

T_Unit = (Unit : T_Unit) & (unit : T_unit Unit) -> Type;
T_unit = Unit => (unit : T_unit Unit) & Unit unit;

Unit : T_Unit = @fix(Unit). unit =>
  (u : Unit unit) & (P : Unit unit -> Type) -> (x : P unit) -> P u;

(unit : T_unit Unit) & (u : Unit unit) & (P : Unit unit -> Type) -> (x : P unit) -> P u ==
(unit : T_unit Unit) & (P : Unit unit -> Type) -> (x : P unit) -> P unit

(unit : T_unit Unit) & (u : Unit unit) & (P : Unit unit -> Type) -> (x : P unit) -> P u

unit : (unit : T_unit Unit) & Unit unit =
  @fix(unit). (P : Unit unit -> Type) -> (x : P unit) -> P u;
Unit : Type;
unit : Unit;

Unit = @self(u : Unit). (P : Unit -> Type) -> P unit -> P u;
unit = @fix(u : Unit). (P : Unit -> Type) => (x : P unit) => x;


M : @self(a). @self(b). (P : T -> Type) -> P (@unroll a) -> P b;


Γ, x : @self(x). T |- T ⇐ Type
------------------------------
Γ |- @self(x). T ⇒ Type

Γ, x : @self(x). T |- M ⇐ T
-----------------------------
Γ |- @fix(x). M ⇐ @self(x). T

Γ |- M ⇒ ιx. T
------------------
Γ |- M ⇐ T[x := M]


M { false = _; true = _; ...R; }

(M | true => _ | false => _)


(x => M) N
(x => f (x x)) (x => f (x x))

T_Unit : Type = @self(Unit). (unit : @self(unit). Unit unit) -> Type;
Unit : T_Unit = @fix(Unit). unit =>
  @self(u). (P : Unit -> Type) -> P unit -> P u;



// context
-----------
• : Context

Γ : Context  x ∉ Γ  Γ |- A : Type
--------------------------------- // term hypothesis
(Γ, x : A) : Context

Γ : Context  x ∉ Γ  Γ |- M : A
------------------------------ // unlocked equality
(Γ, x == M) : Context

Γ : Context  x ∉ Γ  Γ |- M : A
------------------------------ // locked equality
(Γ, [x == M]) : Context

Γ : Context
------------- // unlock all equalities
|Γ| : Context

Γ : Context  _ :> x ∉ Γ  Γ |- M : A  Γ |- x : Type
-------------------------------------------------- // coercion hypothesis
(Γ, M :> x) : Context

// equality
// TODO: type formation may depend on equality??
// TODO: prevent kind formation from depending on equality??

Γ | Δ |- A : Type  Γ | Δ |- M ⇒ Type  Γ |- M ⇒ A
--------------------------------------
Γ | Δ |- M ≡ N : A

// rule
Γ : Context  Γ |- A ⇒ Type  Γ |- M ⇒ A
-------------------------------------- // infer
Γ |- M ⇒ A

Γ : Context  Γ |- A ⇒ Type  Γ |- M ⇐ A
-------------------------------------- // check
Γ |- M ⇐ A

Γ : Context  Γ |- A ⇒ Type  Γ |- M ⇒ S  Γ |- M :> A
--------------------------------------------------- // coercion
Γ |- M :> A

// TODO: maybe both should check?
Γ : Context  Δ : Context  Γ |- A ⇒ Type  Γ |- M ⇒ A  Γ |- N ⇒ A
--------------------------------------------------------------- // equality
Γ | Δ |- M ≡ N : A

// infer
// TODO: bad notation, unlock on infer
|Γ| |- M ⇒ A
------------
Γ |- M ⇒ A

Γ |- M ⇐ A
----------------
Γ |- (M : A) ⇒ A

// check
Γ |- M ⇒ S  Γ |- (M : S) :> T
-----------------------------
Γ |- M ⇐ T

Γ, [x == A], M :> x |- M ⇐ A
----------------------------
Γ, x == A |- M ⇐ x

// coercion
-----------------
Γ |- (M : A) :> A

Γ, [x == T], M :> x |- M :> T
-----------------------------
Γ, x == T; Δ; |- M :> x

-------------------
Γ, M :> x |- M :> x

// universe
----------------
Γ |- Type ⇒ Type

// functions
Γ, x : A |- B ⇐ Type
------------------------
Γ |- (x : A) -> B ⇒ Type

|Γ|, x : A |- M ⇐ B
--------------------------
Γ |- x => M ⇐ (x : A) -> B

Γ |- M ⇒ (x : A) -> B  Γ |- N ⇐ A
---------------------------------
Γ |- M N ⇒ B[x := N]

// intersection
Γ, x : A |- B ⇐ Type
-----------------------
Γ |- (x : A) & B ⇒ Type

Γ |- (M : S) :> A  Γ |- (M : S) :> B[x := A]
--------------------------------------------
Γ |- (M : S) :> (x : A) & B

Γ |- M ⇒ (x : A) & B
--------------------
Γ |- M ⇐ A

Γ |- M ⇒ (x : A) & B
--------------------
Γ |- M ⇐ B[x := M]

// mutual fixpoint
Γ, x : A | Δ, x == M |- K ⇒ T
-------------------------------------------
Γ | Δ |- x : A; K ⇒ T[x := x : A; x = M; x]

Γ, x : A |- K ⇒ T
-------------------------------------------
Γ | Δ |- x : A; K ⇒ T[x := x : A; x = M; x]


Γ, x : B | Δ, x == M |- K ⇒ T  Γ, x : A |- x :> B
------------------------------------------------- // ?
Γ, x : A | Δ |- x : B; K ⇒ T


Γ | Δ |- A ≡ (x : A) & B
Γ, x : A | Δ |- M ⇐ B  Γ, x == M | Δ |- e ⇒ T
---------------------------------------------
Γ, x : A | Δ, x == M |- x = M; K ⇒ T


Context ::= Γ | Δ
Variable ::= x | y
Term ::= M | N
Type ::= A | B
Pattern := P | Q
Constraint ::= C | D

Rule :=
  | Γ |- M ⇒ A // infer type
  | Γ |- M ⇐ A // check type
  | Γ |- P ⇒ A; Δ // infer pattern
  | Γ |- P ⇐ Q; Δ // check pattern
Γ |- M ⇒ A == Γ |- M ⇐ _x

Γ |- M ⇒ A // infer type
Γ |- M ⇐ A // check type

Γ |- P ⇒ A; Δ // infer pattern
Γ |- P ⇐ Q; Δ // check pattern



(∃_A. C); Γ |- M : _A
-------------------------------
C; Γ |- (M : B) ⇒ B; C[_A := B]

(∃_a. C); (Γ, x : _a) |- M ⇒ B
-----------------------------
Γ; C |- x => M ⇒ (x : A) -> B

Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool) & (P : Bool -> Type) -> P true -> P false -> P b;
true = (P : Bool -> Type) => (x : P true) => (y : P false) => x;
false = P => x => y => x;

Γ |- M[x := K] ≡ N[x := K] : A[x := K]
--------------------------------------
Γ, x == K |- M ≡ N : A

Γ, [x == K], (M : S) :> T |- (M : S) :> T
-----------------------------------------
Γ, x == K |- (M : S) :> x

Γ |- (M : S) :> A  Γ |- (M : S) :> B[x := A]
--------------------------------------------
Γ |- (M : S) :> (x : A) & B



T_true ≣ (P : Bool -> Type) -> P true -> P false -> P true;
Γ |- (true : T_true) :> Bool
Γ, (true : T_true) :> Bool |- (true : T_true) :>
  (b : Bool) & (P : Bool -> Type) -> P true -> P false -> P b
Γ, (true : T_true) :> Bool |- (true : T_true) :>
  (P : Bool -> Type) -> P true -> P false -> P true

Γ |- M[x := K] ≡ N[x := K]
-------------------------- // equality
Γ, x == K |- M ≡ N : A

[] |- (unit : (P : Unit -> Type) -> P unit -> P unit) :> Unit
(unit : (P : Unit -> Type) -> P unit -> P unit) :> Unit :: [] |-
  (unit : (P : Unit -> Type) -> P unit -> P unit) :> (u : Unit) & (P : Unit -> Type) -> P unit -> P u
(unit : (P : Unit -> Type) -> P unit -> P unit) :> Unit :: [] |-
  (unit : (P : Unit -> Type) -> P unit -> P unit) :> (P : Unit -> Type) -> P unit -> P unit

(x : A) => (x :> B) => _;
(x : Bool, y : Int) === (y : Int, x : Bool)
incr : Nat -> Nat;

a = 1;

incr = x => incr x;

Color : Type;
is_primary : (color : Color) -> Prop;

Color =
  | Red
  | Green
  | Blue
  | Secondary(left : Color & is_primary left, right : Color & is_primary right);
is_primary color = color == Red || color == Green || color == Blue;

// TODO: is removing those locks okay?
|Γ| |- S <: T
----------------- // subtype
Γ |- (M : S) :> T

----------------- // refl
Γ |- (M : S) :> S

// TODO: what if arrow is a variable that can be expanded by equality
Γ |- (M : S) :> T
--------------------------------------------
Γ |- (x => M : (x : A) -> S) :> (x : A) -> T

Γ, [x == K], (M : S); dirty :> T |- (M : S) :> T
-----------------------------------------
Γ, x == K |- (M : S) :> T

Γ, [x == K], (M : S); dirty :> T |- (M : S) :> T
-----------------------------------------
Γ, x == K |- (M : S) :> x

Γ |- (M : S) :> A  Γ |- (M : S) :> B[x := A]
--------------------------------------------
Γ |- (M : S) :> (x : A) & B

Γ |- A ≡ B
--------------------
Γ |- M ⇐ (x : A) & B


Γ | • | • |- M ≡ N : A
---------------------- // enters equivalence
Γ |- M ≡ N : A

// expand once in each side
Γ, x == M | L, x | R |- M == N : A
---------------------------------
Γ, x == M | L    | R |- x <: N : A

Γ, x == M | L | R, x |- M <: N : A
---------------------------------
Γ, x == M | L | R    |- x <: N : A

// expand while one of the sides is an annotation
Γ, x == M | L, x | • |- M ≡ N : A
---------------------------------
Γ, x == M | L, x | • |- x ≡ N : A

Γ, x == N | • | R, x |- M ≡ N : A
---------------------------------
Γ, x == N | • | R, x |- M ≡ x : A

// short circuits, aka, theorems
x ∉ L  x ∉ R
----------------------
Γ | R | E |- x ≡ x : A

----------------------
Γ | • | E |- x ≡ x : A

----------------------
Γ | R | • |- x ≡ x : A

```

## Macabeus

```rust
incr = (x : Nat) => x + 1;
x = (
  (y : A) = 1;
  y + 1
);

map : <A, B>(f : (x : A) -> B, l : List<A>) -> List<B>;

incr = x => x + 1;

map = (f, l) =>
  l
  | [] => []
  | [el, ...tl] => [f(el), ...map(f, tl)];

id : <A>(x : A) -> A
  = <A>(x : A) => x;

id_inter :
  (id_nat : ((x : Nat) -> Nat)) &
  (id_string : ((x : String) -> String)) = id;

x : Nat = id_inter 1;
y : String = id_string "a";

(n : Nat & n != 0) => n + 1

Status =
  | (tag == "valid")
  | (tag == "banned", content : String);

Status =
  (tag : Bool, ...(tag | true => (...) | false => (..., content : String)));

  | (tag == "valid", content : ())
  | (tag == "banned", content : String);

R = (...,y : Nat);
Tuple = (R : Row) => (x : Nat, ...R);

Status =
  (valid : Bool, content : valid | true => () | false => String);

(x : Status) =>
  x
  | ("valid", ()) => 0
  | ("banned", _reason) => 1;

Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool) & (P : Bool -> Type) -> P true -> P false -> P b;
true = (P : Bool -> Type) => (x : P true) => (y : P false) => x;
false = (P : Bool -> Type) => (x : P true) => (y : P false) => y;



Nat : {
  @Nat : Type;

  one : Nat;
  add : (n : Nat, m : Nat) -> Nat;
  sub : (n : Nat, m : Nat) -> Nat;
} = {
  @Nat = Int;
};

User = {
  id : Nat;
  name : String;
};

A = { x : Nat; } & { y : Nat };

Status =
  (valid : Bool) & valid
    | true => (valid == true)
    | false => (valid == false, content : String);

(valid : Bool) & (valid == true)

(valid : Bool) & (valid == false, content : String)

x : (!valid : Bool) = (valid = true);

x : (x : Nat, y : Nat)

sum = (!x : Nat, !y : Nat) => x + y;
// TODO: warning
x = sum (y = 2, x = 1);

(x : Nat & x == 1) => x + 1;

Show = {
  A : Data;
  show : (x : A) -> String;
};

<Show_Nat : Show<Nat>> = _;

(x : Nat) => x.show();

show = <A : Show>(x : A) => x.show();

x = show<Nat, Show_Nat>(1);

id = <A : Type> =>(x : A) => x.
x : Nat = id(1);
id = (A : Type) => (x : A) => x.
x : Nat = id(Nat)(1);


Point = (<A>, x : A, y : A);
id = (<A>, x : A) => x.

x = id(1);
incr = (x : Nat) => Nat.add(x, Nat.one);

id x = x;
id(x : Nat) : Nat;


f : () -> Nat ~ IO = () : Nat => ;
(x : Nat) = (f() : Nat ~ IO);

List : {
  @List(A : Data) : Data;

  length<A>(l : List A) : Nat ~ IO;
  length<A>(l : List A) : Nat ! IO;
  length<A>(l : List A) : Nat # IO;
  length : <A>(l : List A) -> Nat;
  length : <A>(l : List A) -[IO]> Nat;

  length : <A>(l : List A) -> Nat;
  map : <A, B>(f : (x : A) -> B, l : List A) -> List B;
  filter : <A>(f : (x : A) -> Bool, l : List A) -> List A;
} = _;

Type : Type;
Data : Type;
Prop : Type;
Resource : Type;
Object : Type;
Dynamic : Type;

[@teika { mode = "dynamic"; }]
x = 1;
a = x 2;

univalence : (iso : A =~= B) -> A == B;

A =~= B : Type
A == B : Prop

f : _ = _;
g = rewrite sort_into_quick_sort f;

S = <A, B>(x : A, y : B) -> A;
T = <A, B>(x : A, y : B) -> B;

A = { A : Data; x : Nat; y : String; };
B = { A : Data; y : String; x : Nat; };
C = { x : Nat; y : String; A : Data; };

(g : T) => g(2, 1);
Socket : {
  @Socket : Resource;
} = _;

Id : (A : Data) -> Data = (A : Data) => A;

a : (x : Nat, y : Nat) = (
  x : Nat;
  y : Nat;
  x = y,
  y = 1,
);



A | B | ...R

f = x => (
  print x : _;
)


!Nat = (
  Nat : Type;
  Nat = (A : Type) -> A -> (Nat -> A) -> A;
  Nat
);

Nat[Nat := (A : Type) -> A -> (Nat -> A) -> A]

A = (A : Type) -> A -> (Nat -> A) -> A;

Unit : Type;
unit : Unit;

Unit = (u : Unit) & (P : Unit -> Type) -> P unit -> P u;

unit : (P : Unit -> Type) -> (x : P unit) -> P unit;
unit = (P : Unit -> Type) => (x : P unit) => x;

M : Open -n ≝ (∀-x. x != n -> ftv x M == ∅)
M : Closed ≝ (∀-x. ftv x M == ∅)



// explicit substitutions
Γ, [-1 := N] |- M : Closed  Γ |- N : Closed
-------------------------------------------
Γ |- M[open N] : Closed

Γ, [+n := -1] |- M : Closed
---------------------------
Γ |- M[close +n] : Open -1

// context
Γ |- N : Closed
----------------------
Γ, [-n := N] : Context

Γ, [-n := N] |- M : Open -n
---------------------------
Γ |- M : Open -n


Γ, [close +n], [open +m] |- -1 : Closed
---------------------------------------
Γ, [close +n] |- -1 : Closed


(A : Type) -> _A -> _A
(B : Type) -> B\-1 -> B\-2

(A : Type) -> B\-1 -> B\-2


(
  x : A;
  y = x;
  x = 1;
  y + 1;
)
(
  x : A;
  [y := x];
  [x := 1];
  y + 1;
)

(
  μ : A;
  [open -1];
  [open -1]
)

incr = (x : A; x + 1);
two = incr (x = 1; );

(x : A) ->
--------


Γ, x : A | Δ, x == M |- K ⇒ T
-----------------------------
Γ | Δ |- x : A; K ⇒ T

Γ, x : A |- K ⇒ T
-------------------------------------------
Γ | Δ |- x : A; K ⇒ T[x := x : A; x = M; x]


(
  x : A;
  y = x;
  x = 1;
  y + 1;
);
(
  x : A;
  [y := x\-1];
  [x\-2 := 1];
  y\-1 + 1;
);

(
  x : A;
  [y := x\-1];
  [skip [x := 1]];
  y\-1 + 1;
);



(
  x : A;
  [y := x];
  [x := 1];
  y + 1;
)

(
  x : A;
  [y := x];
  [skip [x := 1]];
  y\-1;
)
(
  x : A;
  [y := x];
  [|x := 1];
  y\-1;
)

(
  x : () -> Never;
  x = () => x ();
  x
)

(
  x : () -> Never;
  [| x := () => x ()];
  x
)

x[| x := () => x ()]
(() => x ())[| x := () => x ()]
() => x[| x := () => x ()] ()
() => x[| x := () => x ()] ()
y[skip [x := 1]][y := x]
y[y := x][x := 1]


(A : Prop) => (x : A) => (y : A) => (refl : x == y)

(
  x : A;
  [|y := x; x := 1];
  y
)
y[|y := x; x := 1]
x[|y := x; x := 1]
1[|y := x; x := 1]

Term := n | λM | M N | M[S];
Subst := M | id | S1 . S2 | fix S;


(1)[M] ~> M
(1 + n)[M] ~> n
(n)[id] ~> n
(n)[fix S] ~> (n)[S . fix S]


(1)[M . S] ~> M[S]
(1 + n)[M . S] ~> (n)[S]
(n)[id . S] ~> (n)[S]
(n)[fix S1 . S2] ~> (n)[S1 . fix S1 . S2]


(n)[M . S] ~>
(n)[fix S] ~> n[S][fix S]

(λM)[S] ~> λM[S]
(M N)[S] ~> M[S] N[S]

M[fix S1][S2]

M[s1][s2]


(
  x : () -> Never;
  [fix [x := () => x ()]];
  x
)
(
  x : Nat;
  [y := x];
  [|y := x; x := 1];
  y
)

(A = Int; (x : A) -> A)

Γ, x == N |- M ⇐ T
-------------------
Γ |- M ⇐ (x = N; T)

M : (A : Type; A = Int; (x : A) -> A);
----------------------------
M N : (A : Type; A = Int; A)

Γ, x == M |- K : B
----------------------------
Γ |- (x = M; K) : (x = M; B)

----------------------------
Γ |- (x = M; K) ~> K[x := M]


Term = | x\+l | x\-i | x => M | M N | M[S];
Subst = | open M | close x\+n;

(x => M) N ~> M[open M] // beta

x\-1[open M] ~> M
x\-(1 + i)[open M] ~> x\-i

x\+l[close x\+n] ~> x\+l
x\+l[close x\+l] ~> x\-1

x\+l[open M] ~> x\+l
x\-i[close x\+l] ~> x\-(1 + i)

(x => M)[S] ~> x => M[open x\+l][S][close x\+l]
(M N)[S] ~> M[S] N[S]


(x => \-1 \-2)[open t]
x => (\-1 \-2)[open x\+l][open t][close x\+l]
x => \-1[open x\+l][open t][close x\+l] \-2[open x\+l][open t][close x\+l]
x => \-1 t

(x => \-1 \+3)[close +3]
x => (\-1 \+3)[open x\+l][close +3][close x\+l]
x => \-1 \-2

O(n)
O(n^2)

(x => M) N == M[x := N]

(x => x + 1) 2 == (x + 1){x := 2} == 2 + 1 == 3

(x => x + 1) 2 == (x + 1)[x := 2] == x[x := 2] + 1[x := 2] == 2 + 1


(λ\-1 + 1) 2 == (\-1 + 1)[2] == \-1[2] + 1[2] == 2 + 1


type F<A> = A -> A;

(x => M) N


Term = | x\+l | x\-i | x => M | M N | M[S];
Subst = | open M | close;

(x => M) N ~> M[open M] // beta

x\-1[open M]; l ~> M ;
x\-(1 + i)[open M]; l ~> x\-i; l

x\+l[close]; m ~> x\+l; m
x\+l[close]; l ~> x\-1; l

x\+l[open M]; m ~> x\+l; m
x\-i[close]; m ~> x\-(1 + i); m

(x => M)[S]; l ~> x => M[open x\+l][S][close]; (1 + l)
(M N)[S]; l ~> M[S] N[S]; l


0 |- x => +1[close]

Term = | x\+l | x\-i | x => M | M N | M[S];
Subst = | open M | close;

(x => M) N ~> M[open M] // beta

x\-1[open M]; l ~> M ;
x\-(1 + i)[open M]; l ~> x\-i; l

x\+l[close]; m ~> x\+l; m
x\+l[close]; l ~> x\-1; l

x\+l[open M]; m ~> x\+l; m
x\-i[close]; m ~> x\-(1 + i); m

(x => M)[S]; l ~> x => M[open x\+l][S][close]; (1 + l)
(M N)[S]; l ~> M[S] N[S]; l

M[x := y]
M[y := x]


M[3]

1 ~> 3
2 ~> 1
1 + n ~> n


M[3][s] == M

M[3][shift . 1] == M

1 ~> 2
2 ~> 3
3 ~> 1
n ~> 1 + n   n > 3


M[open y][close y] == M

2[2][lift 1] == 2
1 == 2


M[-2] // -1 -> -2

[-1 := +5]
[+5 := -1]
```

## Explicit Substitution

```rust
Concrete = no meta variables
Locally closed = no shifting or open
Closed = no shifting, open or close


(f : (x : A : Prop) -> Nat) 0 == (f : (x : A : Prop) -> Nat) 1

Γ |- N[x := M]
-------------------
Γ |- x := M; N



List(A) =
  | Nil
  | Cons(el : A, tl : List(A));


x : List(Nat)
x = Cons(1, x)


Term : Data;
Term =
  | var x
  | abs (M : Term)
  | app (M : Term) (N : Term);

[M] |- app (var 1) (var 1)

Context : Data;
Term(Γ : Context) : Data;

Term =
  | var : <Γ>(n) -> Term(Γ :: n)
  | abs : <Γ>(M : Term()) -> Term()
  | app (M : Term) (N : Term)



Type : Data;
Type =
  | var(binder : Nat)
  | forall(body : Type);


Type : Data;
Binder : (x : Type) -> Data;

Type =
  | arrow (param : Type) (body : Type)
  | var (x : Type & Binder x)
  | forall (body : Type)
  | exists (body : Type);
Binder = x
  | arrow _ _ | var _ => Never
  | forall _ | exists _ => Unit;

id : Type;
id = forall (app (var id) (var id));




Type : Data;
Binder : Data;

Type =
  | arrow (param : Type) (body : Type)
  | var (x : Type & Binder x)
  | forall (body : Type)
  | exists (body : Type);
Binder = x
  | arrow _ _ | var _ => Never
  | forall _ | exists _ => Unit;


forall : (b : Binder) -> _;


id : Type;
id = forall (id => app (var id) (var id));


Type : Data;
Binder : Data;

Context : Data;
Valid : (Γ : Context, n : Nat) -> Prop;
get : <Γ>(n : Nat & Valid (Γ, n)) -> Binder;

Type =
  | arrow (param : Type, body : Type)
  | var : (x : Binder) -> Type = k => n => k (get n)
  | forall (body : Type)
  | exists (body : Type);
Binder = (x : Type & x
  | arrow _ _ | var _ => Never
  | forall _ | exists _ => Unit);

Context = List Binder;
Valid = (Γ, n) => List.length Γ > n;
get = _;



id = forall (arrow (var ))


Type : Data;
Binder : (x : Type) -> Data;

Type =
  | arrow (param : Type) (body : Type)
  | var (x : Type & Binder x)
  | forall (body : Type)
  | exists (body : Type);
Binder = x
  | arrow _ _ | var _ => Never
  | forall _ | exists _ => Unit;

id : Type;
id = forall (app (var id) (var id));



Type : Data;
Binder : (x : Type) -> Data;

arrow




```

# Teika

```rust
// let
x = 1;

f = () => x;

// shadowing
x = x + 1;

// nested-let
z = (
  y = x + 1;
  y + 1
);

// infered function
incr = x => x + 1;

// annotated function
double = (x : Nat) => x * x;

// type alias
My_nat = Nat;
triple : (x : My_nat) -> Nat
  = x => x * x * x;

// type constructor
Id = (A : Data) => A;
a : Id(Nat) = 1;

// hoisting
fib : (n : Nat) -> Nat;
fib = n =>
  n
  | 0 => 0
  | 1 => 1
  | n => fib(n - 1) + fib(n - 2);

// polymorphism
id = <A>(x : A) => x;

// properties
div = (n : Nat, m : Nat & m >= 0) => _;

checked_div = (n, m) : Option(Nat) =>
  m >= 0
  | true => Some(div(n, m))
  | false => None;

// universes
(Type l : Type (1 + l)); // univalence
(Prop l : Type l); // irrelevance
(Data l : Type l); // UIP
(Resource l : Type l); // linear
(Object l : Type l); // subtyping
(Dynamic l : Type l); // dynamic

dup = (sock : Socket) => (sock, sock);

// modules
Nat : {
  @Nat : Data;
  zero : Nat;
  one : Nat;
  add : (n : Nat, m : Nat) -> Nat;
} = _;

incr = (n : Nat) => Nat.add(n, Nat.one);

// effects
print : (msg : String) -[IO]> () = _;
parse : (data : String) -[Error]> Nat = _;
print : (data : Nat) -> String = _;
read : (file : String) -[IO]> (data : String) = _;
write : (file : String, data : String) -[IO]> () = _;

read_incr_and_write_back = file =[IO | Error]> (
  value = read(file);
  value = parse(value);
  value = value + 1;
  write(value);
);

loop : () -[Loop]> ();
loop = () => loop();

// intersection types
Strict_nat = (n : Nat) & n >= 0;

// union types
Either = (A : Data, B : Data) =>
  | (tag == "left", content : A)
  | (tag == "right", content : B);

// inductive types
Bool : Data;
true : Bool;
false : Bool;

Bool = (b : Bool) & <P>(then : P true, else : P false) -> P b;
true = (then, else) => then;
false = (then, else) => else;

Resource + Sealed types

// possible future
Bool = | true | false;
```

## Explicit Substitution

```rust
Term =
  | x      | M[x := N]
  | x => M | M N;

(x => M) N == M[x := N] // beta

x[x := M] == M
x[y := M] == x

(x => M)[x := N] == x => M
(x => M)[y := N] == x => M[y := N]
(M N)[x := K] == M[x := K] N[x := K]


Term =
  | n  | M[S]
  | λM | M N
Subst = M | ⇑S;

(λM) N == M[N] // beta

0[M] == M
(1 + n)[M] == n

0[⇑S] == 0
(1 + n)[⇑S] == n[]

(x => M)[x := N] == x => M
(x => M)[y := N] == x => M[y := N]
(M N)[x := K] == M[x := K] N[x := K]



Term =
  | x      | M[x := N]
  | x => M | M N;

Term =
  | x      | M[...(x := N)]
  | x => M | M N;

----------------------- // beta
(x => M) N ~> M[x := N]

x[x := N] ~> N
x[y := N] ~> x

(x => M)[x := N] ~> x => M
(x => M)[y := N] ~> x => M[y := N]

(M N)[x := K] ~> M[x := K] N[x := K]


(M N)[x := K1][y := K2]
(M N)[x := K1; y := K2]



Term =
  | n  | M[S]
  | λM | M N
Subst = M | ⇑S;

(λM) N == M[N] // beta

0[M] == M
(1 + n)[M] == n

0[⇑S] == 0
(1 + n)[⇑S] == n[S]

(λM)[S] == λM[⇑S]
(M N)[S] == M[S] N[S]


_ x == x + 1
_ x == (y => y + 1) x == _k[y := x] == x + 1

_k[y := x] == x + 1
_k[y := x][x := y] == (x + 1)[x := y]
_k == y + 1


(y => y + 1) x == (y + 1)[y := x] == x + 1



Term =
  | n  | M[S]
  | λM | M N
Subst = M | ⇑S;

(λM) N == M[N] // beta

0[M] == M
(1 + n)[M] == n

0[⇑S] == 0
(1 + n)[⇑S] == n[S]

(λM)[S] == λM[⇑S]
(M N)[S] == M[S] N[S]




Term =
  | x      | M[x := N]
  | x => M | M N;

(x => M) N == M[x := N] // beta
M[x := N] == M{x := N}


Term =
  | x      | _X
  | x => M | M N
  | M[x := N];

(x => M) N == M[x := N] // beta
M[x := N] == M{x := N}    fm(M) == ∅
M[x := N] == M{x := N}[[x := N]]    fm(M) != ∅

x[x := M] == M
_X[x := M] == _X

(x => M)[y := N] == x => M[y := N]
(M N)[x := K] == M[x := K] N[x := K]


f = x => x + 1;
f 1



Term =
  | n  | M[S]
  | λM | M N
Subst = M | ⇑S;

(λM) N == M[N] // beta

0[M] == M
(1 + n)[M] == n

0[⇑S] == 0
(1 + n)[⇑S] == n[S]

(λM)[S] == λM[⇑S]
(M N)[S] == M[S] N[S]
```

### At a distance

```rust
Term (M, N) = x | x => M | M N;

(x => M) N |-> M{x := N} // beta




Term (M, N) = x | x => M | M N | M[x := N];

(x => M) N |-> M[x := N] // beta

x[x := N] |-> N
(x => M)[y := N] |-> x => M[y := N]
(M N)[x := K] |-> M[x := K] N[x := K]

L = _ | L[x := M]
C = Term with single hole




(let x = M in _)<x + 1>
L<x => M> N == L<M[x := N]>

L<x => M> N == L<M[x := N]>


(x => M)[...(x := _)] N




(x => x + x) 1
(x + x)[x := 1]
(_ + x)[x][x := 1]
(_ + x)[1][x := 1]
(1 + x)[x := 1]
(1 + _)[x := 1]
(1 + 1)[x := 1]
(1 + 1)


Term (M, N) = _ | x | x => M | M N | M[x := N]
L = _ | L[x := M]
C = Term with single hole

// rewrite
L<x => M> N  |-> L<M[x := N]> // dB
C<x>[x := M] |-> C<M>[x := M] // ls
M[x := N]    |-> M if x ∉ M // gc

// plug
_<M>           == M
(x => C)<M>    == x => C<M>
(C N)<M>       == C<M> N
(N C)<M>       == N C<M>
(C[x := N])<M> == C<M>[x := N]
(N[x := C])<M> == N<[x := C<M>]>


// equiv
(y => M)[x := N]  == y => M[x := N]    if y ∉ fv(N)
(M N)[x := K]     == M[x := K] N       if x ∉ fv(N)
M[x := N][y := K] == M[x := K][x := N] if y ∉ fv(N) && x ∉ fv(K)

(x => x + x) 1
(x + x)[x := 1]
(_ + x)<x>[x := 1]
(_ + x)<1>[x := 1]
(1 + x)[x := 1]
(1 + _)<x>[x := 1]
(1 + 1)[x := 1]
(1 + 1)



Term (M, N) = _ | n | λM | M N | M[N] | M[↑]
L<K> = K | L<K>[M]
C = Term with single hole

// rewrite
L<λM> N     |-> L<N[M]> // dB
C<0>[M]     |-> C<M[↑]>[M] // ls
M[x]        |-> M{↓} <=< 0 ∉ fv(M) // gc

// plug
_<M>     == M
(λC)<M>  == λC<↑M>
(C N)<M> == C<M> N
(N C)<M> == N C<M>
C[N]<M>  == C<M[↑]>[N]
N[C]<M>  == N[C<M>]


// example
(λλ0 1) 1 0

(λλ0 1) x f
(λ0 1)[x] f
- // ls
(λ0 _)<0>[x] f
(λ0 _)<↑x>[x] f
(λ0 ↑↑x)[x] f
(λ0 ↑x) f
(0 ↑x)[f]
(_ ↑x)<0>[f]
(_ ↑x)<↑f>[f]
(↑f ↑x)[f]
(f x)
- // dB
(0 1)[f][x]
(_ 1)<0>[f][x]
(_ 1)<f>[f][x]
(f 1)[f][x]
-- // ls
-- // gc
(↑f 1)[f][x]
(f 0)[x]
(f _)<0>[x]
(f _)<x>[x]
(f ↑x)[x]
(f x)
(f ⇑x)[x]
(f ⇑x)



(λλ0 1) 1 f
(λ0 1)[1] f
- // ls
(λ0 _)<0>[1] f
(λ0 _)<1>[1] f
(λ0 2)[1] f
(λ0 1) f ;; g
(0 ⇑x)[f]
(_ ⇑x)<0>[f]
(_ ⇑x)<f>[f]
(f ⇑x)[f]
(f ⇑x)

(λ0 1)[x] f

(_ 1)<0>[x][f]
(_ 1)<0>[x][f]



(x = 1) >=> x + 1
(x + 1) <=< (x = 1)
(x = 1>; x + 1)
(x + 1<; x = 1)




Term (M, N) = x | x => M | M N | M[x := N];
L<K> = K | L[x := M]
C<K> = K | x => C | C N | M C | C[x := N] | M[x := C]

L<x => M> N  |-> L<M[x := N]> // dB
C<x>[x := M] |-> C<M>[x := M] // ls
M[x := M]    |-> M <=< x ∉ fv(M) // gc


(M N)[x := K] == M[x := K] N[x := K]
M[x := y][y := x] == M[x := y[y := x]; y := x]


[x := K] |- M N
  [x := K] |- M
  [x := K] |- N
(x => y => M) N K
(y => M)[x := N] K


L<K> = K[x := N]
L<y => M> K
L<M[y := K]>

M[y := K][x := N]


(x + y)[y := K][x := N]
(x + _)<y>[y := K][x := N]
(x + _)<K>[y := K][x := N]
(x + K)[y := K][x := N]
(x + K)[x := N]
(_ + K)<x>[x := N]
(_ + K)<N>[x := N]
(N + K)[x := N]
(N + K)


_<x + y>[y := K][x := N]
_<>


(_ + x)<x>[x := N]
(_ + x)<N>[x := N]
(N + x)[x := N]
(N + _)<N>[x := N]
(N + N)[x := N]
(N + N)


(x x y)[y := K][x := N]

_<x x y>[x := N][y := K]
(_ y)<x x>[x := N][y := K]
(_ y)<_ x><x>[x := N][y := K]
(_ y)<_ x><N>[x := N][y := K]
(_ y)<N x>[x := N][y := K]
(_ y)<N _><x>[x := N][y := K]
(_ y)<N _><N>[x := N][y := K]
(_ y)<N N>[x := N][y := K]
(N N _)<y>[x := N][y := K]


L<K<M>> vs L<K>
L[x := N]<K<x>> |-> K<N> // fast



L[x := N]<K<x>> |-> L[x := N]<K<N>> // use

L<K<x => M>> |-> L<K<x => _><M>>
L<K<M N>>    |-> L<K<_ N><M>>
L<K<_ N><M>> |-> L<K<M _><N>>

// plugging
_<M>         |-> M
(x => C)<M>  |-> x => C<M>
(C N)<M>     |-> C<M> N
(M C)<N>     |-> M C<N>
C[x := N]<M> |-> C<M>[x := N]
M[x := C]<N> |-> M[x := C<N>]

```

### Linear Substitution Calculus

```rust
Term (M, N) = x | x => M | M N | M[x := N];
L<K> = K | L[x := M]
C<K> = K | x => C | C N | M C | C[x := N] | M[x := C]

L<x => M> N  |-> L<M[x := N]> // dB
C<x>[x := M] |-> C<M>[x := M] // ls
M[x := M]    |-> M <=< x ∉ fv(M) // gc

L<K<x>>[x := N] |-> L<>

L<K<x => M>> |-> L<K<x => _><M>>
L<K<M N>> |-> L<K<_2 _1><N><M>>>

L, x := N |- x :: K
-------------------
L, x := N |- K<N>

L |- x => x :: _
L |- x :: x => _ :: _

L<_1<x => x>>
L<_1<x => _1><x>>>

L<K<M>> vs L<K>



K<x x>[x := N]
K<_ x><x>[x := N]
K<_ x><N>[x := N]
K<N x>[x := N]
K<N _><x>[x := N]
K<N _><N>[x := N]
K<N N>[x := N]


M :: K == K<M>

(x x :: K)[x := N]
(x :: _ x :: K)[x := N]
(N :: _ x :: K)[x := N] // ls
(N x :: K)[x := N]
(x :: N _ :: K)[x := N]
(N N :: K)[x := N] // ls
(K<N N>)[x := N]

K<_ x><N>[x := N]
K<N x>[x := N]
K<N _><x>[x := N]
K<N _><N>[x := N]
K<N N>[x := N]


L<K<x>>[x := N] |-> L<K<N>>[x := N] // ls
L
K<_ x><N>[x := N]

L<K<(x => M) N>> |-> L<K<M[x := N]>> // dB
L<K<x => M>> |-> L<(x => _)<x>>

L<K<M N>> |-> L<K<M N>>
Γ |- M N :: K |-> Γ |- M :: _ N :: K // Lapp
Γ |- M N :: K |-> Γ |- N :: M _ :: K // Rapp


Γ |- M :: _ N :: K  bv(Γ) ∩ fv(M) != ∅ // TODO: this is bad
-------------------------------------- // left-apply
Γ |- M N :: K

Γ |- M :: M _ :: K  bv(Γ) ∩ fv(M) == ∅
-------------------------------------- // right-apply
Γ |- M N :: K



Term (M, N) = x | x => M | M N | M[x := N];
L |- _ = _ | L[x := M]
_ :: C = _ | x => C | C N | M C | C[x := N] | M[x := C]

----------------------------------
L |- (x => M) N |-> L |- M[x := N] // dB

-------------------------------------
(x :: C)[x := M] |-> (M :: C)[x := M] // ls

x ∉ fv(M)
--------------- // gc
M[x := M] |-> M

// plugging
M :: _         == M
M :: (x => C)  == x => (M :: C)
M :: C N       == (M :: C) N
M :: N C       == N (M :: C)
M :: C[x := N] == (M :: C)[x := N]
M :: N[x := C] == M[x := (N :: C)]

// algorithm

-------------------------------------------- // ls
L, x := N |- x :: K |-> L, x := N |-> N :: K



(x x)[x := y][y := z]


// reference
Term (M, N) = x | x => M | M N | M[x := N];
L<K> = K | L[x := M]
C<K> = K | x => C | C N | M C | C[x := N] | M[x := C]

L<x => M> N  |-> L<M[x := N]> // dB
C<x>[x := M] |-> C<M>[x := M] // ls
M[x := M]    |-> M <=< x ∉ fv(M) // gc


// alternative
C<x>[x := M] |-> C[x := M]<M> // ls

C<M>[x := N] == C[x := N]<M>

// de-bruijin
Term (M, N) = _ | n | λM | M N | M[N] | M[↑];
Partial (_ :: C) = _ | λC | C N | M C | C[N] | M[C] | C[↑];
Context (L |- _) = _ | L, N;

// rewrite
L |- (λM) N :: K |-> L, N |- M :: K[↑] // dB
L, N |- 0 :: K |-> L |- N :: K[N] // ls
L, N |- K[↑] |-> L |-> K // drop

// notation
L |- K[N] == L, N |- K

// plug
M :: _   == M
M :: λK  == λ(M[↑] :: K)
M :: K N == (M :: K) N
M :: N K == N (M :: K)
M :: K[N] == (M[↑] :: K)[N]
M :: N[K] == N[M :: K]



// de-bruijin
Term (M, N) = 0 | λM | M N | M[N] | M[↑];
Partial (_ :: C) = _ | λC | C N | M C | C[N] | M[C] | C[↑];
Context (L |- _) = _ | L, N;

// rewrite
L |- (λM) N :: K |-> L, N |- M :: K[↑] // dB
L, N |- 0 :: K |-> L |- N :: K[N] // ls

// equiv?
L |- M[N] :: K == L, N |- M :: K[↑]
L, N |- M[↑] :: K == L |- M :: K[N]

// plug
M :: _   == M
M :: λK  == λ(M :: K)
M :: K N == (M :: K) N
M :: N K == N (M :: K)
M :: K[N] == (M :: K)[N]
M :: N[K] == N[M :: K]


L |- λM == L, 0 |- M :: λ_[↑];


L |- λ(M 0) == L, 0 |- M 0 :: λ_[↑];

x |- 1 0 :: λ_


(λ1 0)
1 0 :: λ_

M == 1
L == x

0 == 0
(1 + n) == 1[↑]

x, 0 |- 1 0 :: λ_[↑]
x, 0 |- 1 0 :: λ_[↑]

x, 0 |- 1 0 :: λ_[↑]

x, 0 |- 1 :: _ 0 :: λ_[↑]

x, 0 |- 0[↑] :: _ 0 :: λ_[↑]

x |- 0 :: (_ 0)[0] :: λ_[↑]
x |- x :: (_ 0 :: λ_[↑])[0]
x |- (x 0)[0] :: λ_[↑]
x, 0 |- (x 0) :: λ_[↑]

0[↑] ==



Term (M, N) = 0 | λM | M N | M[N] | M[↑];
Partial (_ :: C) = _ | λC | C N | M C | C[N] | M[C] | C[↑];
Context (Γ |- _) = _ | L, N;



Γ |- M[S] :: K |-> Γ, S |- M :: K


C<x>[x := M] |-> C<M>[x := M] // ls


Γ, N |- 0 :: K |-> Γ, N |- N :: K // ls

(1 + n) == n[↑]
A, B |- 1 :: K == A |- 0 :: _[↑] :: K[B]

(λM) == λ
A |- A :: _[↑] :: K[B]
A, B |- A[↑] :: K


Γ, N |- 1 + n :: K == Γ |- n :: (1 + _) :: K[N]

(1 + n :: _)[N] == (n :: 1 + _ :: _)[N]


M :: K[N] == (M :: K)[N]

(1 + n :: _)[N] == (n :: 1 + _ :: _[N])

(1 + n :: K)[N] == (n :: 1 + _ :: K)[N]

(λ_)<0>[0]

(λ_)<0>[0]

(λM)[N] == λ(M[0][N])
(λx (1 2))[0 1][1]

\0[\0 + 1]
(x + 1)[x := x + 1]

K<0[N]> == K<_<0>[N]>

K<M[S]> == K<_[S]><M>


(_ N)<M[S]> == M[S] N


(_ N)<M>[S]
M[↑][S]
M´

C<A>

M (_<N> _)<K>


Γ, x |- M |-> K
----------------------
Γ |- x => M |-> x => K



Γ, x := 1 |- 1 == x : Nat
--------------------------
Γ |- 1 == (x => x) 1 : Nat

Γ, 1 |- 1 == 0 : Nat
---------------------
Γ |- 1 == (λ0) 1 : Nat


Γ |-
---------------------------
Γ |- (λ0) 2 == (λ0) 1 : Nat


0[1]


Unit : Type;
Unit = (u : Unit);


x[x := N][x := K]


Pack : {
  Unit : Type;
  unit : Unit;
};
Pack = {
  Unit = (u : Pack.Unit) & (P : Pack.Unit -> Type) -> P (Pack.unit) -> P u;
  unit =
}
Unit : Type;
unit : Unit;


{ x : A; x : A; }

{ x : A; ...R }


Type l : Type (1 + l);
Data l : Data (1 + l);
Prop l : Prop (1 + l);


N == M : Prop
N =~= M : Type

A : Data;

((x : A) -> A) : Data
((x : A) -> A) : Type

id = (A : Type) => (x : A) => x;

x = id ((x : Nat) -> Nat)



x - 2 = 0

x = y
f(x) = f(y)

f(x) = 0;

0 = 1
0 * 0 = 1 * 0
0 = 0

x - 2 * 0 = 0 * 0



H === False == (P : (f : False) -> Type) -> P f
H |- False == @box((P : (f : False) -> Type) -> P f)
|- @box((P : (f : False) -> Type) -> P f) == @box((P : (f : False) -> Type) -> P f)

+P1 |- False == (P : False -> Type) -> P f
-P1 |- False == (P : False -> Type) -> P f


H === False == (P : (f : False) -> Type) -> P f

H |- False == (P : (f : False) -> Type) -> P f
[H] |- False == (P : (f : False) -> Type) -> P f




Γ, x : A === M | Δ |- M == N
----------------------------
Γ, x : A == M | Δ |- x == N

Γ | Δ, x : A === N |- M == N
----------------------------
Γ | Δ, x : A == N |- M == x

M === N
----------------------------
Γ, x : A === M | Δ |- x == N

M === N
----------------------------
Γ, x : A === M | Δ |- x == N

------------------------
Γ, x : A === M |- x == N


False == (P : False -> Type) -> P f |-
  False ==

P | P |- (P : False -> Type) -> P f == (P : False -> Type) -> P f


H |- (P : False -> Type) -> P f == False


• | False |- False == False



id
  : (A : Type) -> (x : A) -> A
  = (A : Type) => (x : A) => x;


x = id String "a";


Type l : Type (1 + l);
Data l : Type (1 + l);
Prop l : Type (1 + l);



(x : Int) => x
(x : String) => x

Γ |- M == N : B
------------------------------------
Γ |- x => M == x => N : (x : A) -> B

Γ, x : A == M | R, x | • |- M == N[x := M]
------------------------------------------
Γ, x : A == M | R, x | • |- x == N


H === x == () -> x

H | x | • |- x == () -> x
H | x | • |- () -> x == () -> x
H | x | • |- x == x

H | -x, +x | +x, -x |- x == () -> x
H | +x, -x | +x, -x |- () -> x == () -> x
H | x | x |- () -> x == () -> x


Γ, x == N |- M ⇐ A
------------------
Γ |- M ⇐ A[x := N]



-------------------------------------------------
Γ |- M[x := N] : A[x := N] === Γ, x := N |- M : A

Context (Γ, Δ)
Term (M, N)
Type (A, B)


Γ |- M ⇒ (x : A) -> B  Γ |- N ⇐ A
--------------------------------- // apply
Γ |- M N ⇒ B[x := N]

Γ |- M ⇒ L<(x : A) -> B>  Γ |- N ⇐ L<A>tttw
--------------------------------------- // apply at a distance
Γ |- M N ⇒ L<x = N; B>



Γ, x : A[Δ] |- M ⇐ B[Δ]
------------------------------- // d-lambda
Γ |- x => M ⇐ ((x : A) -> B)[Δ]




Γ |- M ⇒ ((x : A) -> B)[Δ]  Γ |- N ⇐ A[Δ]
----------------------------------------- // apply at a distance
Γ |- M N ⇒ B[x := N][Δ]

((x : A) -> B)[Δ]
((x : A[y := y]) -> B[y := y])[y := K]
((x : A[y := K]) -> B[y := y])[y := K]
((x : A[y := K]) -> B[y := K])[y := K]

x + 1 -| Δ
(x, y) = (1, 2); N

_<x>[x := x]
n + m == add n m

Γ |- M[x := K] == N[x := K]
---------------------------
Γ, x == K |- M == N

x == 1;


x == 1 |- 2 == x + 1
|- 2 == 1 + 1
|- 2 == 2
Γ |- _
Γ; _e

C<x>[x := N] |-> C<N>[x := N]
C<0>[N] |-> C<N>[N]




Γ, x : A |- M ⇐ B
-------------------------- // lambda
Γ |- x => M ⇐ (x : A) -> B

Γ, x : A[L] |- M ⇐ B[L]
------------------------------- // d-lambda
Γ |- x => M ⇐ ((x : A) -> B)[L]

------------------------------- // d-lambda
((x : A) -> B)[L] === ()

(X = Nat; (x : X) -> Nat) == (x : Nat) -> Nat

incr : (X = Nat; Y = Nat; (x : X) -> Y)
  = (x : (X =)) => x + 1;

context : []
expected : Nat
received : (X = Nat; X)

context : []
expected : Nat
received : Nat



(x => M) N |-> M[x := N] // beta

(x => M) N |-> (x = N; M) // beta

L<x => M> N |-> L<M[x := N]> // dB
C<x>[x := N] |-> C<N>[x := N] // ls


Γ, A[Δ] |- M ⇐ B[⇑Δ]
-------------------- // bad-lambda
Γ |- λM ⇐ (∀A. B)[Δ]


Γ |- M[0 : A[Δ]] ⇐ B[0 : A . Δ]
-------------------------------- // d-lambda
Γ |- λM ⇐ (∀A. B)[Δ]


Γ |- M[0 : A . Δ] ⇐ B[0 : A . Φ]
-------------------------------- // d-lambda
Γ |- (λM)[Δ] ⇐ (∀A. B)[Φ]




M[0 : A . Δ] ⇐ B[0 : A . Φ]
--------------------------- // d-lambda
(λM)[Δ] ⇐ (∀A. B)[Φ]

(∀A. B)[Δ] == (∀A[Δ]. B[0 : A . Δ])
(∀A[Δ]. B[⇑Δ])

((x : A) -> B)[Δ]
((x : A[Δ]) -> B[0 : A][Δ])[Δ]

(λ(0 == 1))[N]
(λ(0 == 1)[N])
(λ(0 == 1[N]))

----------------
Γ |- L<M> ⇐ Δ<A>

L<((x : A) -> B)> == ((x : L<A>) -> L<B>)

((x : A) -> B)[S] == (x : A[S]) -> B[⇑S]

LC <==> LSC <==> TM
(y = K; (x => M)) N |-> (y = K; x = N; M)

(x => M) N |-> (x = N; M)

(x => x + 1) 2 |-> 2 + 1
```

## At a Distance Typing

### No Sharing

```rust
M[Γ] ⇐ A[Γ]
----------------- // i-annot
(M : A)[Γ] ⇒ A[Γ]

M[Γ] ⇒ A[Δ]  A[Δ] ≡ B[Φ]
------------------------ // dc-infer
M[Γ] ⇐ B[Φ]

----------------- // i-univ
Type[Γ] ⇒ Type[Γ]

B[x : A][Γ] ⇒ Type[Γ]
--------------------------- // i-forall
((x : A) -> B)[Γ] ⇒ Type[Γ]

M[x : A[Δ]][Γ] ⇐ B[x : A][Δ]
------------------------------- // dc-lambda
(x => M)[Γ] ⇐ ((x : A) -> B)[Δ]

M[Γ] ⇒ ((x : A) -> B)[Δ]  N[Γ] ⇐ A[Δ]
------------------------------------- // di-apply
(M N)[Γ] ⇒ B[x : A = N][Δ]

N[Γ] ⇒ A[Δ]  M[x : A[Δ] = N][Γ] ⇒ B[Φ]
-------------------------------------- // di-let
(x = N; M)[Γ] ⇒ B[Φ]


N[Γ] ⇒ A[Δ]  M[x : A[Δ] = N][Γ] ⇒ B[Φ]
-------------------------------------- // di-let
(x = N; M)[Γ] ⇒ B[Φ]

```

### Sharing

```rust
Γ |- M ⇒ A === M[Γ] ⇒ A[Γ]
Γ |- M ⇐ A === M[Γ] ⇐ A[Γ]

Γ |- M ⇐ A
---------------- // i-annot
Γ |- (M : A) ⇒ A

Γ |- M ⇒ A
---------- // dc-infer
Γ |- M ⇐ A

Γ |- N ⇒ A  Γ, x : A = N |- M ⇒ B
--------------------------------- // i-let
Γ |- M[x = N] ⇒ B

Γ |- N ⇒ A  Γ, x : A = N |- M ⇐ B
--------------------------------- // c-let
Γ |- M[x = N] ⇐ B

------------------- // i-univ
Γ |- Type ⇒ Type

Γ, [x : A] |- B ⇒ Type
------------------------ // i-forall
Γ |- (x : A) -> B ⇒ Type

Γ, [x : A] |- M ⇒ B
-------------------------------- // i-lambda
Γ |- (x : A) => M ⇒ (x : A) -> B

Γ |- M[x : A[Φ]][Δ] ⇐ B[x : A][Φ]
------------------------------------ // dc-lambda
Γ |- (x => M)[Δ] ⇐ ((x : A) -> B)[Φ]

Γ |- M ⇒ ((x : A) -> B)[Δ]  Γ |- N ⇐ A[Δ]
----------------------------------------- // di-apply
Γ |- M N ⇒ B[x : A = N][Δ]



M[x : A[Φ]][Δ][Γ] ⇐ B[x : A][Φ][Γ]
------------------------------------- // dc-lambda
(x => M)[Δ][Γ] ⇐ ((x : A) -> B)[Φ][Γ]

M[x : A[Δ]][Γ] ⇐ B[x : A][Δ]
------------------------------- // dc-lambda
(x => M)[Γ] ⇐ ((x : A) -> B)[Δ]


M[x : A[Φ][Γ]][Δ][Γ] ⇐ B[x : A][Φ][Γ]
------------------------------------- // dc-lambda
(x => M)[Δ][Γ] ⇐ ((x : A) -> B)[Φ][Γ]

M[x : A[Φ][+|Δ|]][Δ][Γ] ⇐ B[x : A][Φ][Γ]
------------------------------------- // dc-lambda
(x => M)[Δ][Γ] ⇐ ((x : A) -> B)[Φ][Γ]

M[x : A[⇒]][Δ][Γ] ⇐ B[x : A][Φ][Γ]
------------------------------------- // dc-lambda
(x => M)[Δ][Γ] ⇐ ((x : A) -> B)[Φ][Γ]

Γ |- M[x : A[Φ]][Δ] ⇐ B[x : A][Φ]
--------------------------------------- // dc-lambda
Γ |- (x => M)[Δ] ⇐ ((x : A) -> B)[Φ]


Γ, Δ |- M[↑|Δ|] ⇐ A
------------------- // dc-lambda
Γ |- M ⇐ A[Δ]

Γ, Δ |- M[↑|Δ|] ⇐ A
------------------- // dc-lambda
Γ |- M ⇐ A[Δ]

Γ, x : A |- M[⇑Δ] ⇐ B
------------------------------- // dc-lambda
Γ |- (x => M)[Δ] ⇐ (x : A) -> B

M[⇑Δ][x : A . Γ] ⇐ B[x : A . Γ]
------------------------------- // dc-lambda
(x => M)[Δ][Γ] ⇐ ((x : A) -> B)[Γ]


Γ, x : A[Δ] |- M ⇐ B[x : A][Δ][↑]
--------------------------------- // dc-lambda
Γ |- x => M ⇐ ((x : A) -> B)[Δ]


Γ, x : A[Δ] |- M ⇐ B[x : A][Δ][↑]
--------------------------------- // dc-lambda
Γ |- x => M ⇐ ((x : A) -> B)[Δ]

M[x : A[Δ]][Γ] ⇐ B[x : A][Δ][↑][x : A[Δ]][Γ]
-------------------------------------------- // dc-lambda
(x => M)[Γ] ⇐ ((x : A) -> B)[Δ][Γ]

M[x : A[Δ]][Γ] ⇐ B[x : A][Δ][Γ]
---------------------------------- // dc-lambda
(x => M)[Γ] ⇐ ((x : A) -> B)[Δ][Γ]

Γ |- N ⇒ A  Γ, x : A == N |- M ⇐ B[↑]
------------------------------------- // c-let
Γ |- x = N; M ⇐ B

Γ |- M ⇐ A [0]
---------------- // c-let
Γ |- (M : A) ⇒ A



M[x : A[Δ]][Γ] ⇐ B[x : A][Δ]
------------------------------- // dc-lambda
(x => M)[Γ] ⇐ ((x : A) -> B)[Δ]

```

### Context typing

```rust


Context (Γ, Δ) := _ | Γ[x : A] | Γ[x : A := N];
Term (M, N)
Type (A, B) :=
  | (M : A) | Type | \n | x = N; M | x = N; M
  | (x : A) -> B | (x : A) => M | M N | x => M;


Γ |- M ⇒ A       === M[Γ] ⇒ A[Γ]
Γ |- M ⇐ A[Δ]    === M[Γ] ⇐ A[Δ][Γ]
Γ |- M[Φ] ≡ N[Δ] === M[Φ][Γ] ≡ N[Δ][Γ]


Γ |- M ⇒ A  Γ |- A[•] ≡ B[Δ]    Γ |- (M : A) ⇐ A[•]
----------------------------    -------------------
Γ |- M ⇐ B[Δ]                   Γ |- (M : A) ⇒ A

----------------
Γ |- Type ⇒ Type

                      Γ |- \n ⇒ B
------------------    ------------------------
Γ[x : A] |- \0 ⇒ A    Γ[x : A] |- \(1 + n) ⇒ B

Γ[x : A] |- B ⇒ Type[Δ]        Γ[x : A] |- M ⇒ B[Δ]
---------------------------    -----------------------------------
Γ |- (x : A) -> B ⇒ Type[Δ]    Γ |- (x : A) => M ⇒ (x : A) -> B[Δ]

Γ[x : A[Δ]] |- M ⇐ B[y : A][Δ][↑]
--------------------------------- // dc-lambda
Γ |- x => M ⇐ ((y : A) -> B)[Δ]

Γ |- M ⇒ ((x : A) -> B)[Δ]  Γ |- N ⇐ A[Δ]
----------------------------------------- // di-apply
Γ |- M N ⇒ B[x : A := N][Δ]

Γ |- N ⇒ A[Δ]  Γ[x : A[Δ] == N] |- M ⇒ B[Φ]
------------------------------------------- // dr-let-infer
Γ |- x = N; M ⇒ B[Φ][x : A[Δ] == N]

Γ |- N ⇒ A[Δ]
---------------------------------- // dr-let-infer
Γ |- x = N; M |-> M[x : A[Δ] := N]



Nat : Type;
String : Type;



                        \n[Γ] ⇒ B
--------------------    ----------------------
x\0[x : A][Γ] ⇒ A[Γ]    \(1 + n)[x : A][Γ] ⇒ B

M[x : A][Γ] ⇒ B[Δ]
--------------------------------------
((x : A) => M)[Γ] ⇒ (x : A[Γ]) -> B[Δ]

M[Γ] ⇒ ((x : A) ⇒ B)[Δ]  N[Γ] ⇒ A[Δ]
------------------------------------
(M N)[Γ] ⇒ B[x : A := N][Δ]


(x => x) 1
[x == 1] |- x
[x == 1] |- 1


f : (x : Nat) -> Nat;
f : (X = Nat; (x : X) -> X);

(f 1) : Nat

(f 1) : (X = Nat; X)

((x => x) : (x : Nat) -> Nat)

((
  y = 2;
  x => x
) : (x : Nat) -> Nat)

(incr : (x : Nat) -> Nat) = (
  k = 1;
  x => x + 1
);

add = n + 1 + 1 + 1 ... m
mul = n + n + n + n ... m
pow = n * n * n * n ... m

64 + 1
n + n
64 + 64

x\0[x : Nat\1][Γ] ⇒ Nat\1[Γ]
----------------------------------------------------
((x : Nat\1) => x\0)[Γ] ⇒ (x : Nat\1[Γ]) -> Nat\1[Γ]




x\0[x : A\1] ⇒ A\1
----------------------------------------
x\1[f : (y : A\2) -> A\3][x : A\1] ⇒ A\1

A\1 ≡ A\2[x : A\1]

f\0[f : (y : A\2) -> A\3][x : A\1] ⇒ ((y : A\2) -> A\3)[x : A\1]
----------------------------------------------------------------------
(f\0 x\1)[f : (y : A\2) -> A\3][x : A\1] ⇒ A\3[y : A\2 := x\0][x : A\1]
-----------------------------------------------------------------------
((x : A\1) => (f : (y : A\2) -> A\3) => f\0 x\1)[Γ] ⇒

(x\1[y][x : Nat\1] : Nat\1)
(y => x)[x : Nat\1] : (y : _) -> Nat\2


(x : Nat\1) => x\0

[x : A] |- x\0 ⇒ B[Δ]
y = (x\1[y][x : Nat\1 == _] : Nat\1);

Γ[x : A] |- M ⇒ B[Δ]
-----------------------------------
Γ |- (x : A) => M ⇒ (x : A) -> B[Δ]

(x : Nat\1) => (x\0[x : Nat\1])

(\0[x : Nat\1] : Nat\1)
Nat\2[x : Nat\2]


x
x => M
M N


(x => y => x) == (y => x => y)
(_ => _ => \1) == (_ => _ => \1)

(x => y => x)
(x => y => z => x)

(Nat => (x : Nat\1) => y => z => (x\2 : Nat\4))


----------
Γ |- M : A

---------------------
Γ, x : A\1 |- x : A\2

----------------------
x[x : A\1][Γ] : A\1[Γ]

Γ |- A : Type  Γ, x : A |- B : Type
-----------------------------------
Γ |- (x : A) -> B : Type


------------------------------
[x : A] |- (y : A) -> A : Type

----------------------------
((y : A) -> A)[x : A] : Type


(x => x y)



                        \n[Γ] ⇒ B[Δ]
-------------------     -------------------------
\0[x : A][Γ] ⇒ A[Γ]     \(1 + n)[x : A][Γ] ⇒ B[Δ]

// term-level
                        \n[Γ] ⇒ B
-------------------     ----------------------
\0[x : A][Γ] ⇒ A[Γ]     \(1 + n)[x : A][Γ] ⇒ B

M[x : A][Γ] ⇒ B[x : A]
--------------------------------
((x : A) => M : (x : A) -> B)[Γ]

M[(x: A[Δ]) => _][Γ] ⇐ B[(x : A) -> _][Δ]
-----------------------------------------
(x => M)[Γ] ⇐ ((x : A) -> B)[Δ]

M[Γ] ⇒ ((x : A) ⇒ B)[Δ]  N[Γ] ⇒ A[Δ]
------------------------------------
(M N)[Γ] ⇒ B[x : A := N][Δ]



------------------------------
(Γ |- x => M) ⇐ (Δ |- (x : A) -> B)

-------------------------------
(x => M)[Γ] ⇐ ((x : A) -> B)[Δ]

(x => M : (x : Nat) -> Nat)

((
  y = 1;
  x => M
) : (x : Nat) -> Nat)

(x => M : (
  X = Nat;
  (x : Nat) -> Nat
))

-----------------------------
\0[x : A][Γ] ⇒ A[↑][x : A][Γ]


\n[Γ] ⇒ B[Δ]
-----------------------------------
\(1 + n)[x : A][Γ] ⇒ B[↑][x : A][Δ]

M[(x : A) => _][Γ] : B[x : A][Δ]
------------------------------------- // dc-lambda
((x : A) => M)[Γ] : ((x : A) -> B)[Δ]


B[↑][x : A][Δ] == B[Δ]


Context (Γ) ::= _ | Γ[x : A] |;

x[x : A]


------------------- // i-head
\0[x : A][Γ] ⇒ A[Γ]

\n[Γ] ⇒ B[Δ]
------------------------- // i-tail
\(1 + n)[x : A][Γ] ⇒ B[Δ]

M[x : A][Γ] ⇒ B[x : A][Γ]
------------------------------------- // i-lambda
((x : A) => M)[Γ] ⇒ ((x : A) -> B)[Γ]

M[Γ] ⇒ ((x : A) -> B)[Δ]  N[Γ] ⇐ A[Δ]
---------------------------------- // di-apply
(M N)[Γ] ⇒ B[x : A = N][Δ]


M ≡ M[↑][x : A]


------------------- // i-head
\0[x : A][Γ] ⇒ A[Γ]


n[Γ] ⇒ B[Δ]
------------------------- // i-head
\(1 + n)[x : A][Γ] ⇒ B[Δ]

M[x : A][Γ] ⇒ B[x : A][Γ]
------------------------------------- // i-lambda
((x : A) => M)[Γ] ⇒ ((x : A) -> B)[Γ]

M[x : A[Δ]][Γ] ⇐ B[x : A][Δ]
------------------------------------- // dc-lambda
(x => M)[Γ] ⇐ ((x : A) -> B)[Δ]

M[Γ] ⇒ ((x : A) -> B)[Δ]  N[Γ] ⇐ A[Δ]
------------------------------------- // di-lambda
(M N)[Γ] ⇒ B[x : A = N][Δ]


x\0[x : Nat\0][Nat : Type] ⇒ Nat\0[Nat : Type]
-----------------------------------------------------------
((x : Nat\0) => x\0)[Nat : Type] : (x : Nat\0) -> Nat\0[↑]


Nat\0[Nat : Type] == _B[x : Nat\0][Nat : Type]
Nat\0 == _B[x : Nat\0]
Nat\0[↑][x : Nat\0] == _B[x : Nat\0]
Nat\0[↑] == _B

Nat\1[Nat\1 : Type] == _B[x : Nat\1][Nat\1 : Type]
Nat\1 == _B[x : Nat\1]
Nat\1[↑] == _B



f : (X = Nat; (x : Nat) -> X);


(x => x + 1 : (x : Nat) -> Nat)
((
  one = 1;
  x => x + one
) : (x : Nat) -> Nat)

(λx. λy. t) u ~ λy. ((λx. t) u)


(λy. t)[x := u] ~ λy.t[x := u]
λy. t[x := u] ~ λy.t[x := u]

(λx.t v) u ~ (λx.t) u v
(t v)[x := u] ~ t[x := u] v
(t v)[x := u] ~ t[x := u] v

(λ.t v) u ~ (λ.t) u v
(t v)[u] ~ t[u] v


Term ::= n | λM | M N | M[S];
Subst ::= N | ↓;

0[N] |-> N        // head
(1 + n) |-> n     // drop


(M N) |-> M[↓][N] // db
(λM)[↓] |-> M     // skip

Term ::= x | x => M | M N | M[x := N] | M[apply N];

x[x := N] |-> N
(y => M)[x := N] |-> (y => M[x := N])

(M N) |-> M[apply N] // beta
(x => M)[apply N] |-> M[x := N] // skip


x[apply N]

(f v)[y := u]
  f[apply v][y := u]

((x => t) v)[y := u]
  (x => t)[apply v][y := u]
  t[x := v][y := u]


Term ::= n | λM | M N | M[S];
Subst ::= N | ⇑S | ⇓S;

0[N] |-> N
(1 + n)[N] |-> n

0[⇑S] |-> 0
(1 + n)[⇑S] |-> n[S]

(λM)[S] |-> λM[⇑S]

(M N) |-> M[⇓N] // db
(λM)[S] |-> M     // skip

((λt) v)[u]
  (λt)[⇓v][u]
  t[v][u]

(λ1)[u]
(t v)[u]
  t[↓][v][u]


((λt) v)[u]
-
  (λt)[↓][v][u]
  t[v][u]

((λt) v)[u]
- t[v][u] // beta
- t[u][v] // propagation first
((x => t) v)[y := u]
- t[x := v][y := u] // beta
- t[y := u][x := v] // propagation first

λM[↓] |-> M // elim-lambda
M[N][↓] |-> M // elim-subst



λM[↓] |-> M // elim-lambda

λM[↓] |-> M // elim-lambda


λ(0[↑]) |-> M // skip-lambda
λ(0[↑]) |-> M // skip-binder

λM[↓] |-> M // elim-lambda
M N |-> M[↓][N] // weird-beta


λ0 |-> 0[↓]

(λ0) N |-> N

(λ0[↑])[N][↓] |-> N

(λM) N

(f 1) : (X = Nat; X)

Nat\1[Nat\1 : Type] == _B[x : Nat\1][Nat\1 : Type]
Nat\1 == _B[x : Nat\1]

Nat\1[↑][x : Nat\1] == _B[x : Nat\1]
Nat\1[↑] == _B

Nat\1[↑] == Nat\1[↑]

Nat\1[↑][↑][x : Nat\1] == _B[x : Nat\1]
Nat\1[↑][↑] == _B

Nat\1[↑] == _B[x : Nat\1]

_Δ := ↑
Nat\1 == _B[x : Nat\1]
Nat\1[↓] == _B[x : Nat\1][↓]
Nat\1[↓] == _B
Nat\1[] == _B

_B[x : Nat\1][_Δ]

⇒ Nat\1[↑]
------------------
⇒ (x : Nat\1) ->


Nat\1[↑] ==

(x : Nat\1) => (x\0 : Nat\1[↑])

⇒
--------------------
(x : Nat\1) => \0 ⇒ ((x : Nat\1) -> Nat\1)

_Δ := ↑

M ⇒ K[↑][↑]
-------------------------
x => M ⇒ ((A : Type) -> B)[↑][↑]



Nat\1[↑] == Nat\1[↑]

Nat\1 == _B[x : Nat\1]
Nat\1[↑][x : Nat\1] == _B[x : Nat\1]
Nat\1[↑] == _B

Nat\1[↑] == _B[x : Nat\1]

Nat\1 == _B[x : Nat\1][_Δ]

received : Nat\1[Δ]
expected : _B[x : Nat\1][Δ]

_B := Nat\1[↑]


Term (M, N) ::= 0 | λM | M N | M[S];
Subst (S) ::= N | ↑ | ↓;

0[N] |-> N

0[↑][N] |-> n
0[N][↓] |-> n

0[⇑S] |-> 0
0[⇓S] |-> 0

M[↑][⇑S] |-> M[S][↑]
M[↓][⇓S] |-> M[S][↑]


M N |-> M[⇓N] // Beta

(λM)[⇓S] |-> M[S] // Skip


0[1][↓]
n[↓] |-> (1 - n)


0[1][↓] |-> 1[↓] |-> 0
0[2][↓] |-> 2[↓] |-> 0

M[↓][S] |->
M[S][↓] |->

(λM)[↓] == λM[⇑↓]
n[↓]


(λ0[↓]) (λλ0)
(λ0[↓])[↓][λλ0]
0[↓][λλ0]
0[↓][λλ0] ? 0[λ0]

M[S][↓]

(λ0[↓]) (λλ0)
(λλ0)[↓]
(0)[↓][N]


M[↑][N] |-> M // drop


(1 + n)[N] |-> n // drop



(λM)[N] |-> λM[⇑N]
(λM)[⇑S] |-> λM[⇑⇑S]


((λt) v)[u]
- t[v][u] // beta




(λ0)[N] |-> λ0
(λ1)[N] |-> λN
(λ2)[N] |-> λ1


Term (M, N) ::= 0 | λM | M N | M[S];
Subst (S) ::= N | ↑ | →;

M N |-> M[→][N]l
(λM)[→] |-> M

M[N][→] |-> M

M N



(λM)[N] |->

2[N]
M[N][↓] |->

M[↓][N] |->



(λ0)[N] |-> λ0
(λ1)[N] |-> λN[↑]
(λ2)[N] |-> λ1

Term (M, N) ::= 0 | λM | M N | M[S];
Subst (S) ::= N | ↑ | →;
```

## De-bruijin + sharing

```rust
Context (Γ) ::= _ | Γ, x : A | Γ, x : A = N;
Term (M, N)
Type (A, B) ::=
  | (M : A) | Type | \n | x = N; M | x = N; M
  | (x : A) -> B | (x : A) => M | M N | x => M | M[Δ];
Subst (Δ, Φ) ::= | M | Δ[x : A = N] | _[↑]
Variable (n) ::= 0 | 1 + n

Γ |- M ⇒ A[n]       === M[Γ] ⇒ A[n][Γ]
Γ |- M ⇐ A[Δ][n]    === M[Γ] ⇐ A[Δ][n][Γ]
Γ |- M[Φ] ≡ N[Δ] === M[Φ][Γ] ≡ N[Δ][Γ]

Γ |- A : Prop
--------------
Γ |- M ≡ N : A

Γ |- M ⇒ A[↑n]


Γ |- M ⇒ A  Γ |- A[•] ≡ B[Δ][n]    Γ |- (M : A) ⇐ A[•][0]
-------------------------------    ----------------------
Γ |- M ⇐ B[Δ][n]                   Γ |- (M : A) ⇒ A

----------------
Γ |- Type ⇒ Type


Γ, x : A == N :
------------------------- // forget
Γ, x : A == N |- Γ, x : A

--------------------- // i-head
Γ, x : A |- \0 ⇒ A[↑]

Γ |- \n ⇒ B
--------------------------- // i-tail
Γ, x : A |- \(1 + n) ⇒ B[↑]

Γ, x : A |- M ⇒ B[Δ | n]
--------------------------------------- // i-lambda
Γ |- (x : A) => M ⇒ (x : A) -> B[Δ | n]

Γ, x : A |- B ⇒ Type[• | 0]
------------------------------- // i-forall
Γ |- (x : A) -> B ⇒ Type[• | 0]

Γ, x : A |- M ⇒ B[Δ | n]
--------------------------------------- // i-lambda
Γ |- (x : A) => M ⇒ (x : A) -> B[Δ | n]

Γ, x : A[Δ] |- M ⇐ B[y : A][Δ | 1 + n]
---------------------------------------- // dc-lambda
Γ |- x => M ⇐ ((y : A) -> B)[Δ | n]

Γ |- M ⇒ ((x : A) -> B)[Δ | n]  Γ |- N ⇐ A[Δ | n]
------------------------------------------------- // di-apply
Γ |- M N ⇒ B[x : A = N][Δ | n]

Γ |- N ⇒ A[Δ]  Γ[x : A[Δ] = N] |- M ⇒ B[Φ]
------------------------------------------- // dr-let-infer
Γ |- x = N; M ⇒ B[Φ][x : A[Δ] == N]


--------------------------------- // i-head
Γ, x : A | n |- x\+|Γ, x : A| ⇒ A

Γ |- \n ⇒ B[Δ | n]
----------------------------------- // i-tail
Γ, x : A |- \-(1 + n) ⇒ B[Δ | 1 + n]


(A : Type) => (x : _B) => (x : B\0 : A\1)
(x : _B) => (A : Type) => (x : A\0)

-----------------------------
Γ | Δ |- _x ≡ N -| Δ, _x := N

-----------------------------
Γ | Δ |- M ≡ _x -| Δ, _x := M

Γ |- M ⇒ B  Γ |- B ≡ A -| Δ
---------------------------
Γ |- M ⇐ A -| Δ


Γ, x | y : A |- M[open +n] ≡ N[open +n]
------------------------------------------
Γ |- ((x : A) => M) ≡ ((y : A) => N)[Φ]


Γ, x : A |- B ⇒ K
-------------------------------------
Γ |- (x : A) -> B : K

incr = x => x + 1;
n = incr 1;


x : Nat;
x = x;

x

x[x := x]


Γ, x : A |- B ⇒ K
-------------------------------------
Γ |- (x : A) -> B : K

--------------------
Γ |- M[Φ] ≡ N[Δ] : K

Γ |- M[0 :: Φ @ ↑] ≡ N[0 :: Δ @ ↑]
----------------------------------
Γ |- (x => M)[Φ] ≡ (x => N)[Δ]

2[Δ @ ↑] == 2[Δ @ ↑]


Γ |- 1[Δ] == 1[Δ]
------------------------------------------
Γ |- (x => 1)[1 :: Δ] == (x => 2)[Δ]


Γ |- 2[0 :: 1[↑] :: M[↑] :: Δ @ ↑] == 2[0 :: N[↑] :: Δ @ ↑]
----------------------------------------------
Γ |- (x => 1)[Φ] == (x => 2)[Δ]


(M[K]) N
(M[K]) N
(M N)[K]

(M[K]) 0

(M[K] 0)
(0 _K)

(M[K] 0)

L<(λx. M) N> |-> L<M[x := N]>

M[↓][0][K]


(x = K; f x) N

(λ0)[K] N
(λ0) N
N

(λ0 1)[K] N
(λ0 K[↑]) N
N K


((λ0 1) 0)[K]
((λ0 1) 0)[K]
((λ0 K) K)
(K K)


0[0[K]

(x => 1)[1][3]


(x => y => M) "A" "B"


(x, y) = ("A", "B");
M[x := "A"][y := "B"]

pair => (
  (x, y) = pair;
  x + y
);

pair => (x + y)[(x, y) = pair];

pair => (x + y)[x := (x, y) = pair; x][y := (x, y) = pair; y]
pair => (((x, y) = pair; x) + ((x, y) = pair; y))

(x : Nat) -> Nat == (x : String) -> Nat

(x => 1)[0][0]
(x => 1[0 :: 0])
(x => 1)[1] == (x => 2)[id]



(String |> Token[] |> Ctree |> Ltree |> Ttree) // syntax
(Ttree |> Ttree) // checking
(Ttree |> Utree -> Jtree -> String) // jsend

(Syntax |> Jsend) // syntax + jsend


Nat64 : Data;
i < n : Prop;

Socket : Resource;

Array : {
  get : <A, n>(arr : Array n A, i : Nat64 & i < n) -> A;
  make : <A>(length : Int64 & length >= 0, initial : A) -> Array length A;
} = _;

safe_div : (n : Int64, m : Int64 & m != 0) -> Int64;
unsafe_div : (n : Int64, m : Int64) -[Division_by_zero]> Int64;

(n : Nat64) => (arr : Array n Int64) =>

(n : Nat64, arr : Array n Int64)
Array 15 Int64
```

## Degen De-bruijin

```rust
(λx. M) N ≡ M[N]
(λM) N ≡ M[N]

M[K] N ≡ (M N[↑])[K]

M[0 . ↑] N ≡ M

(λ0 0[↑])[↑] N
(λ(0 0[↑])[0 :: ↑ @ ↑]) N
(λ(0 0[↑][↑])) N
(0 0[↑][↑])[N]
(λ0 (0[↑]))[↑] N
(0 2)[N]
(N 1)

(λ0 1)[↑] N
(0 1)[N . id]
(N[↑] 0)[↑]

(λ0 1)[↓][N[↑]][↓]
(N[↑] 0)[↓]

(λ0 1)[↓][↓][N]
(0 1)[N[↑]][↓]
(N[↑] 0)[↓]



(0 1)[↓][N]

(λ0 1)[↑][↓][N]
(λ0 2)[↓][N]
(0 2)[N]
(N 2)

(λ0 1)[↓][N[↓]][↑]
(0 1)[N[↓]][↑]
(N[↓] 1)[↑]
(N 2)


(M[↑] N)[K] ≡ M N[K]
((λ0)[↑] N)
((λ0) N)
((λ0) N)
((λt) v)[u]
- t[v][u] // beta
- t[⇑u][v]

(M[↑] N)
((λ0 1)[↑] N)
((λ0 2) N)
(N 1)

(M N)[↑] ≡ M[↑] N[↑]


M[↑][↓] == M[↓]

M[↑][N] == M

M[↑][0] == M
M[↑][N] == M[↑]

M[↑] == M[↑][0][↑]

0[↑][0] |-> 1 |-> 0
1[↑][0] |-> 2[0] |-> 1
2[↑][0] |-> 3[0] |-> 2

n[↑] ≡ (1 + n)
(1 + n)[↓] ≡ n

M[↑] N ≡ (M N[↑])[K]
(λM)[↑]


(λ0 1)[↑]
(λ0 2)

(λ(0 1)[↑][0])

(1[↓] N[↑])[↓]

(1 N[↑])[↓]

(0[↑]) 0 ≡ (0 0[↓])[↑]

M[S] N ≡ (M N[-S])[S]


((λt) v)[u]
- t[v][u] // beta
- t[⇑u][v]

(1 0)[v][u]
(0 v)
u v

(u v)
(λ0 1)[K] N
(λ(0 1)[⇑K]) N
(0 1)[⇑K][N]
(0 K)[N]
N K

(λ0 1)[K] N
(0 1)[N[↑]][K]
(N[↑] 0)[K]
(N[↑][K] 0)
(N[K] 0)

(λM) N |-> M[K][N]



(x => M) N |-> M[x := N] // beta
M[x := K] N |-> (M N)[x := K] // extract
C<x>[x := N] |-> C<N>[x := N] // ls


M[x := N] |-> M  x ∉ fv(M) // gc

(λM) N |-> M[N] // beta

0[N] ≡ N
M[↑][N] ≡ M

(M N)[K] ≡ M[K] N[K]

M[K] N ≡ (M N[↑])[K]
(M[↑] N)[K] ≡ M N[K]

(λλM[↑]) K
(λM[↑])[K]
λM[K][↑]

(λλ(0 0[↑])) K
(λ(0 0[↑]))[K]

λ(M N[↑])[K]

(λ(M N[↑])) == λ(0[↓] N)[↑]

(λ(0 0[↑])) == _ (λ0[↑])


(λλ(0 (0[↑] )))

λ(0 ((λ0 0) (λ0[↑])))

(λ(0 0[↑]))[λ0[↑]]
λ(0[↑])

λ(M N) ≡ (λM) (λN)
(λM) (λN)

(0 0[↑])[K]
(0 0[↑])[↓][K]
λ(0[↓] K)[↑]
λ(0 K[↑])

(λ(0 1)[])[K]

(λ(0 0[↑]))[K]

M[K][0]

0 :: (_ 0)[S]
0 :: (S 0)[S]
(M N)[S] ≡ M[S] N[S]

(M N)[0 ↑] |-> (M[n ↑] N[n ↑])
(λM)[n ↑] |-> λM[(1 + n) ↑]
n[m ↑]  n < m |-> n
n[m ↑]  n >= m |-> 1 + n


((y => t)[x := u] v)
((y => t) v)[x := u]
t[y := v][x := u]



M[x := K] N ≡ (M N)[x := K] // extract
M[K] N ≡ (M N[↑])[K] // using de-bruijin

(x => y => M) N K
(y => M)[x := N] K
((y => M) K)[x := N]
M[y := K][x := N] // names are preserved in order
// so
((x => y => M) N K) ≡ (x = N; y = K; M)


------------------
(x : Nat) & String

(x : String) & Nat


Γ |- S <: A  Γ, x : A |- S <: B
-------------------------------
Γ |- S <: (x : A) & B


(M[K] N) ≡ (M N[↑])[K]



0[N] ≡ N
M[↑][N] ≡ M

(M N)[K] ≡ M[K] N[K]

// theorems
M[K] N ≡ (M N[↑])[K]
(M[↑] N)[K] ≡ M N[K]

M[↑] N ≡

----------------------
M[↑]

Term ::= n | λM | M N | M[N];

(0)[N][Γ] |-> N
(1 + n)[N][Γ] |-> n[Γ]
((λM)[Δ] N)[Γ] |-> M[N[Γ]][Δ][Γ]

(λM)[Γ] ≡ (λM[0][Γ])[_]
(λ0 1)[N]

λ0[0][N] 1[0][N]
λ0 N[↑]

M[0][Γ] ≡ N[0][Δ]
-------------------------------------
(λM)[Γ] ≡ (λN)[Δ]

(M[K] : A)

M N |-> M[apply N]
(λM)[Δ][apply N] |-> M[N][Δ]

P 1 := _B[N]

(P 1)[↑][N] == _B[N]
(P 1)[↑] == _B
(P 1)[N] == _B[↑]

b
| true => (x : P true)
| false => (x : P false)

b (_ : true == true) _ : b == b

---------------
M N ⇐ B[x := N]


ind b _ _ : b == b

b == b == _B b
b == b == (λx. _M) b
b == b == _M[x := b]
(b == b)[b := x][x := b] == _M[x := b]
(x == x) == _M
b == b == _B b
_B := λ_M
b == b == (λ_M) b
b == b == _M[b]

_B := λ_M

n == n == (λ_M) n
n == n == _M[n]

(n == n) == _M[n]
(2 == 2) == _M[2]

(2 == 2)[0][1][2]

3 == 2 == (λ_M) 2
3 == 2 == (λ_M) 2

(λλλλ(3 == 2)) 3 2 1 0
(3 == 2)[0][1][2][3]
(1 == 0)[2][3] == _M[2]
(4 == 0)[2] == _M[2] // how

(λλλλ(3 == 2)) 4 2 1 0
(3 == 2)[0][1][2][3]
(3 == 2)[0][1][1][4][2]
(1 == 0)[1][4][2]
(0 == 1)[4][2]
(4 == 0)[2]

(4 == 0) == _M
(3 == 2)[2][3]


(1 == 0)[2][3] == _M[2]

(3 == 2)[0][1][2] == _M[2]
(1 == 0) == _M
(λ(1 == 0)) n
(λλλ(2 == 2)) 2 1 0 == _M[2]
(2 == 2)[0][1][2] == _M[2]
(2 == 2)[0][1] == _M
(0 == 0) == _M
(λλ(2 == 2)[0]) 1 2 == _M[2]

_M := (b == b)[↑]



b (_ : true == true) _ : b == b

_P true ≡ true == true

_P true ≡ true == true

(P true)

[N]|- (P 1)[↑] == _B

M[apply N :: Δ]
M[apply N][Δ]
C<0[N][Γ]> |-> C<N>
C<(1 + n)[N][Γ]> |-> C<n[Γ]>

(λM)[Γ] |-> λM[Γ]

(λ(0 1))[M][Γ]
λ(0 1)[0][M][Γ]
L<λM> N |-> L<M[N; |L|]>

M N


--------------------
Γ, x : A |- 0 ⇒ A[↑]
M[N] |-> M{N}

(λλ0 1)[k] N

(λ0 1)[0 := N]
(λ0[0 := N] 1[1 := N])
(λ0 1[1 := N])
(λ0)[↑] ≡ λ0


M[x := K] N ≡ (M N)[x := K]
M[↑] N ≡


M[K] N ≡ (M N)[x := K]

L<M>


------------------------
Γ, x : A |- M[↑] == N[_]



M N ≡ M[split][N] // beta
(λM)[split] ≡

M[K] N ≡ (M N[↑])[K]
(M[↑] N)[K] ≡ M N[K]

(x => M) N |-> M[x := N] // beta
M[x := K] N |-> (M N)[x := K] // extract
```

## Env + Context

```rust
Term ::= n | λM | M N | M[N] | M[↑] | M[Γ];
Env (Γ, Δ) ::= • | N :: Γ | Γ[↑];

M[N][Γ] |-> M[N :: Γ]
M[↑][Γ] |-> M[↑ :: Γ]

0[N :: Γ] |-> N
(1 + n)[N :: Γ] |-> n[Γ][↑]

n[Γ] |-> (1 + n)[Γ]

(λM)[Γ] |-> λM[0 :: Γ[↑]]

----------------------
Γ, x : A |- x\0 : A[↑]

M[K] N ≡ (M N[↑])[K]
(M[↑] N)[K] ≡ M N[K]


M N |-> M[apply][N]
(λM)[apply] |-> M

M[K][apply][N] ≡ M[apply][N[↑]][K]
M[↑][apply][N] ≡ M[apply][↑][N]


0[N :: S] |-> N
(1 + n)[N :: S] |-> n[S][↑]


M[N][Γ] |-> M[N[Γ] :: Γ]

(M[↑])[N :: S] |-> M[S][↑]
M[↑][N] |-> M


(1 2)[0 :: 1 :: 1 :: 1]

(M N)[↑]
(λM)[↑] ≡ λM[0 :: 1]

0[N :: S] |-> N
(1 + n)[N :: S] |-> N

(0 1)
(λ0 1)[↑] ≡ λ0 2

(λ(0 1))[1] ≡ λ(0 1)[0 :: 1]
(λ(0 1))[0]
0 N |-> 0[apply]


Term ::= 0 | M[↑] | M[N] | λM | M N;
Env (Γ, Δ) ::= • | N :: Γ | Γ[↑];

M[↑ @ Γ] |-> M[Γ]{↑}



0[N :: Γ] ≡ N
M[↑][N :: Γ] |-> M[Γ]
M[N][Γ] ≡ M[N :: Γ]
M[↑][N] ≡ M

(λM) N ≡ M[N]
M[K] N ≡ (M N[↑])[K]
(M[↑] N)[K] ≡ M N[K]

M[N][Γ] ≡ M[N :: Γ]

(M[K] N)[Γ] |-> (M N[↑])[K :: Γ]

(λM)[Γ] |-> λM[0 :: Γ[↑]]

(M[↑] N)[Γ] |-> (M N[↓])[Γ[↑]]

0[N :: Γ] |-> N
0[]

()
M N |-> M[apply][N]
(λM)[apply] |-> M


M[N][Γ] |-> M[N :: Γ]
M[↑][Γ] |-> M[↑ :: Γ]



f(x) = f(x)     f is injective
x = x
g(x) = g(x)


Term (M, N) ::= x | x => M | M N | M[x := N];

(x => M)L N |-> M[x := N]L // db
M[x := N] |-> M{x := N} // s

Term (M, N) ::= x | x => M | M N | M[x := N];

(x => M) N |-> M[x := N] // beta
M[x := N] |-> M{x := N} // s
M[x := K] N ≡ (M N)[x := K]  x ∉ fv(N) // move

Term (M, N) ::= n | λM | M N | M[N] | M[↑];

(λM) N |-> M[N] // beta
M[N] |-> M{N} // subst
M[↑] |-> M{↑} // shift

M[K] N ≡ (M N[↑])[K] // move

Term (M, N) ::= n | λM | M N | M[Γ];
Env (Γ) ::= • | ↑ :: Γ | N :: Γ;

(λM) N |-> M[N] // beta
M[Γ] |-> M{Γ} // shift+subst

M[K] N ≡ (M N[↑])[K]
M[K][Γ] ≡ M[K :: Γ]
M[↑][Γ] ≡ M[↑ :: Γ]

Term (M, N) ::= n | λM | M N | M[Γ];
Env (Γ) ::= • | ↑ :: Γ | N :: Γ | Γ @ ↑;

(λM) N |-> M[N] // beta
M[Γ] |-> M{Γ} // comp-shift+subst

(λM)[Γ] ≡ λM[0 :: Γ @ ↑]
M[K] N ≡ (M N[↑])[K]
M[K][Γ] ≡ M[K[Γ] :: Γ]
M[↑][Γ] ≡ M[↑ :: Γ]

Term (M, N) ::= n | λM | M N | M[Γ];
Env (Γ) ::= • | ↑ :: Γ | N :: Γ | Γ @ ↑;

(λM) N |-> M[N] // beta
M[Γ] |-> M{Γ} // comp-shift+subst

(λM)[Γ] ≡ λM[0 :: Γ @ ↑]
M[K] N ≡ (M N[↑])[K]
M[K][Γ] ≡ M[K[Γ] :: Γ]
M[↑][N] ≡ M


Term (M, N) ::= n | λM | M N | M[N];

L<λM> N |-> L<M[N]>
C<0>[N] |-> C<N{↑}>[N] // ls
M[N] |-> M{\downarr}  0 ∉ fv(M) // gc


(λC)<N> |-> λC<N[↑]>

Term (M, N, K) ::= n | λM | M N | M[Γ];
Env (Γ) ::= • | ↑ :: Γ | K :: Γ | Γ @ ↑;

(λM) N |-> M[N] // beta
M[Γ] |-> M{Γ} // comp-shift+subst

(λM)[Γ] ≡ λM[0 :: Γ @ ↑]
M[K :: •] N ≡ (M N[↑])[K]
M[K :: •][Γ] ≡ M[K[Γ] :: Γ]
M[↑ :: •][Γ] ≡ M[↑ :: Γ]


(↑ :: K :: Γ) ≡ Γ

↑ @ Γ, 0 |- M ≡ N
-----------------
Γ |- λM ≡ λN


Term (M, N, K) ::= n | λM | M N | M[Γ];
Env (Γ) ::= • | ↑ :: Γ | K :: Γ | Γ @ ↑;

M[0 :: Γ @ ↑] ≡ N[0 :: Δ @ ↑]
-----------------------------
(λM)[Γ] ≡ (λN)[Δ]


_M[0][K] ≡ _N[1][K]
_M[0] ≡ _N[1]
_M[K :: 0] ≡ _N[K :: 1]

M[K :: 0] ≡ _N[K :: 1]

M[↑][K :: 0]
M[↑][K :: 0]
M[↑][K :: 0]

_M[K :: 0] ≡ _N[K :: 1]

_M[0][K] ≡ _N[1]

0[0 :: 2] ≡ 0[1 :: 2]
_M[0] ≡ _N[1]

-----------
M[Φ] ≡ N[Δ]

-------------
• |- M[↑] ≡ N

0[↑][]
0[↑]
x => x
λ0

x => y => x
λλ1

x => y => y
λλ0

M ≡ N
-------
λM ≡ λN



(x => M)(N) |-> M{x := N}

(x => x + x)(1) |-> (x + x){x := 1} |-> (1 + 1) |-> 2

Term ::= n | λM | M N;

(λM) N |-> M{N} // beta

Term ::= n | λM | M N | M[N];

(λM) N |-> M[N] // beta
M[N] |-> M{N} // subst

Term ::= n | λM | M N | M[N];

(λM) N |-> M[N] // beta
M[K] N |-> (M N{↑})[K] // move
M[N] |-> M{N} // subst

M[x := K] N |-> (M N)[x := K]
(x => M)[y := N] |-> x => M[y := N]
M[x := N][y := K] |-> M[y := K][x := N]

(M[K] N)
(M{K} N)
(M N{↑})[K]
(x = K; y => M) N
(x = K; y => M) N
x = K; (y => M) N // move
(x = K; y = N; M)


Term (M, N, K) ::= n | λM | M N | M[Γ];
Env (Γ, Δ) ::= • | K :: Γ;

0[K :: Γ] |-> K
(1 + n)[K :: Γ] |-> n[Γ]{↑}

(λM)[Γ] |-> λM[0 :: Γ{↑}]
M[K][Γ] |-> M[K[Γ] :: Γ]

Term (M, N, K) ::= n | λM | M N | M[K];

(λM) N |-> M[N]
M[K] |-> M{K}


M[x := K] N |-> (M N)[x := K]

_M[K] N
(_M N)[K] // _M = λ_M2
(λ_M2 N)[K] // eta
_M2[N][K]

(_M N)

(λ((λ0) K) 0) N
  (λK 0) N
  ((λ0) K) N

(λ0[K] 0) N
  (0[K] 0)[N] // beta
  (λK 0) N // s

(M[x := K] N)[x := K] -(s)> M[x := K] N  x ∉ N
(M[x := x] N)[x := K] -(s)> M[x := K] N  x ∉ N

(M N[x := K])[x := K] -(s)> M N[x := K]  x ∉ M

M[x := K] N ≡ (M[x := K] N)[x := K]  x ∉ N  x ∉ K

(f = x => M; f) N |->

(f = x => M; f N) |->


Term (M, N, K) ::= 0 | λM | M N | M[K] | M[↑];

(λM) N |-> M[N]
M[K] N |-> (M N[↑])[K]


(M[↑] N)[K] |-> M N

0[K[Δ] :: Γ] |-> K[\Del]
(1 + n)[K :: Γ] |-> n[Γ][↑]

n[↑ :: Γ] |-> (1 + n)[Γ]

(λM)[Γ]

0[↑][K :: N :: _]
0[↑][K :: N :: _]
0[↑][K][N]

0[↑ :: K :: N]
1[K :: N]


Term (M, N, K) ::= n | λM | M N | M[K];

(λM) N |-> M[N] // beta
M[K] N |-> (M N{↑})[N] // move
M[K] |-> M{K} // subst



Term (M, N, K) ::= n | λM | M N | M[K] | M[↑];


------------------------------
0 | S | N :: E |-> N :: C | E

λM :: S | E ≡ M :: λ_ :: S | 0 :: E
M N :: S | E ≡ M :: _ N :: S | E
M N :: S | E ≡ N :: M _ :: S | E

M N |  | E |-> M | _ N :: S | E

M | _ N :: S | E |->

----------------
M N

(λM) N |-> M[N] // beta
M[K] N |-> (M N[↑])[N] // move
M[K] |-> M{K} // subst

(λM) N :: Γ |-> M{N} :: Γ // beta
(λM) N :: Γ |-> M{N} :: Γ // beta

-----------------
0 | Γ |  |->

-----------------
0 | Γ |  |->

------------------------
λM :: Γ |-> M :: λ_ :: Γ

M |-> λK
--------------
M N |-> λK

C<(λM) N> |-> M[N] // beta


(λ((λ0) K) 0) N
  ((λ0) K) N // beta
  (λK 0) N // beta

(M[K] N) |-> M_K N // subst
(M[K] N) |-> (M N{↑})[K] // move

(M N{↑})[K] |-> M_K N // subst


Term (M, N, K) ::= n | λM | M N | M[K] | M[↑];

M[K] N ≡ (M N)[K]  if ∅ ≡ fv(N)
M[↑] N ≡ (M N)[↑]  if ∅ ≡ fv(N)

M[K] N ≡ (M N[↑])[K]

M[↑] N ≡ (M N[↑])[1 :: ↑]

(M[↑] 0) ≡ (M N[↑])[0]

M[↑] N ≡ (M N[])[↑]


Term (M, N, K) ::= n | λM | M N | M[K] | M[↑] | M[_];

0[K] |-> K
(1 + n)[K] |-> n[K][↑]

M[K] N |-> (M N[↑])[K]
M[↑] N |-> (M N[↑])[_]
M[_] N |-> (M N[↑])[_]

M[_][K] |-> M[K]
M[↑][K] |-> M

M[K] |-> M{K}
M[↑] |-> M{↑}




(M[↑] N)

(M[↑] N) |-> (M N[↑])[_]

(0[↑] N)[T][U] |-> (U N[T][U])

(0[↑] N)[_]

M[_][T]
M[T]

(0[↑] N[↑])[_K]
(0[↑] N)
(0[↑][↑] N[↑])[_K]
(0 N[↑])[_K]

M[↑] N |-> (M N[↑])[_K]

(M N[↑])[0 / _K][0 / U]

M[↑] N |->

M[K] N ≡ (M N[↑])[K]
(M[↑] N)[K] ≡ M N[K]

(M[↑] N) ≡ (M N[↑])[_U]

(M[↑] N)[K] ≡ M N[K]


(M[↑] N)[_U] ≡ M N[_U]

(M[↑] N)[K]

M[_A] ≡ M[K]
_A ≡ K


((x = K; M) N) |-> (x = K; M N) // preserves
(M (x = K; N)) |-> (x = K; M N) // shrunken
(x = (y = K; N); M) |-> (y = K; x = N; M)

Term (M, N, K) ::= n | λM | M N | M[Γ];

0[N :: Γ] |-> N
(1 + n)[N :: Γ] |-> n[Γ][↑]

(λM)[Γ] |-> λM[0 :: Γ]
(λM)[Δ] N |-> M[N :: Δ]

M N



M[x := N][y := K]
M[y := K][x := N]

M[x := z][y := K][z := N]


M[0 :: 0 :: Δ][N][K] |-> M[N :: Δ]

M[0 :: 0 :: Δ][N :: K] |-> M[0 :: 0 :: Δ @ N :: K]

M[0 :: 0 :: Δ @ N :: K]

(N :: Δ @ K :: Γ) ≡ (N[K] :: Δ @ Γ)

0[0 :: 0 :: Δ][N][K]
N[K]

1[0 :: 0 :: Δ][N][K]
K

0[0 :: 0 :: Δ][N :: K]
N

1[0 :: 0 :: Δ][N :: K]
K

M[0 :: Δ] |->
M[0 :: Δ]


M[N][Γ] |-> M[N[Γ] :: Γ]


(λM)[Γ] |-> λM[0 :: Γ]


(λM)[0 :: Δ] N |-> M[N :: Δ]


Term (M, N, K) ::= n | λM | M N | M[Γ];
Context (Γ) ::= _ | ↑n | N :: Γ;

0[N :: Γ] |-> N
(1 + n)[N :: Γ] |-> n[Γ][↑]

(λM)[Γ] |-> λM[0 :: Γ]
(λM)[Δ] N |-> M[N :: Δ] // db



(x => M) N |-> N[x := M]
N[x := M] |-> (x => M) N
(x => M) N ≡ N[x := M]

Term (M, N, K) ::= n | λM | M N;
Env (Δ, Φ) ::= K :: Δ | ↑ :: Δ;

(λ0 1)[↑] N |-> (0 2)[N] |-> N 1

(λ0 1)[↑] N |-> (0 1)[N :: ↑]

((λ0 1)[↑] N)[K] |-> (λ0 1) N[K] |-> (0 1)[N[K]] |-> N[K] 1

(λ0 1)[↑][↑] N |-> (0 3)[N] |-> N 2

(λ0 1)[↑][↑] N |-> (0 1)[N :: ↑↑]

(λ0 1)[K][↑] N |-> K[↑][N]

(λ0 1)[K][↑] N |-> (0 3)[N] |-> N 2

0[N :: ↑] 1[N :: ↑]

(λM)[↑] N |-> M[N][↑]

(λ0 1)[↑] N |-> (0 1)[N][↑]

(M[↑] N)[K] |-> M N[K]

M N |-> (M N)[↑]

(λM) N |-> M[N] // beta
M[N] |-> M{N} // subst


f x ((x => M) N)


Term (M, N, K) ::= n | λM | M N | M[Δ | s];
Env (Δ, Φ) ::= • | N :: Δ;

M[• | s] |-> M

0[N | s] |-> N[• | s]
(1 + n)[N :: Δ | s] |-> n[Δ | 1 + s]

(λM)[Δ | s] |-> λM[0 :: Δ | s]
(λM)[Δ | s] N |-> M[N :: Δ | s]
(M N)[Δ | s] |-> M[Δ | s] N[Δ | s]

M[↑][K :: Δ | s] |-> M[Δ | 1 + s]

Γ |- M[N :: Φ] ≡ K[Δ]
-----------------------
Γ |- ((λM) N)[Φ] ≡ K[Δ]

Γ |- M[0 :: Φ] ≡ N[0 :: Δ]
--------------------------
Γ |- (λM)[Φ] ≡ (λN)[Δ]

N[0 :: Δ][Γ]
N[Δ][0 :: Γ]

Γ, 0 |- M ≡ N[0 :: Δ]
---------------------
Γ |- λM ≡ (λN)[Δ]


Γ |- M[↑] ≡ M
-----------------
Γ |- (λM)[↑] ≡ λM

(M[↑] )[K]
(λ0 1)[↑] N |-> (0 1)[N :: ↑]


Term (M, N, K) ::= n | λM | M N | M[Δ | s];


y : A;
x : A;
x = y;
y = x;
M


y : A;
x : A;
x = y;
y = x;
M


x : A;
y : A;
x = y;
y = x;

[0 / 0 : A];
[0 / 0 : A];
[0 / 1 : A];
[0 / 1 : A];
M



0[0 / 1][0 / 1]
1[0 / 1][0 / 1]

M[1][1][0][0]

M[1][0]

M[x := y][y := x]

M[1 :: 1]

M[0][2][2][3]
0[1 :: 0]

2[1 :: 2 :: 2 :: 3]
0[1 :: 1 :: 0 :: 0] |-> 1[1 :: 1 :: 0 :: 0]

1[1 :: 0] |-> 0[1 :: 0] |-> 1[1 :: 0]

1[1 :: 0] |-> 0[1 :: 0] |-> 1[1 :: 0]

1[Γ] |->

(λM)[Γ] |-> λM[0 :: Γ]

n[Γ] |-> n{Γ}[Γ]
0[K :: Γ] |-> K[|K| :: Γ]
(1 + n)[]
0[0 :: Γ] |-> 0[{0} :: Γ]
(λM)[Γ] |-> λM[0 :: Γ]
((λM)[Δ] N) |-> M[N[↑] :: Δ]


Term (M, N, K) ::= x | x => M | M N | M[x := N];

x[Γ] |-> x{Γ}[Γ]


Γ, x := z, y := z |- M ≡ N
--------------------------
Γ |- (x => M) ≡ (y => N)

Γ, x := z, y := z |- _A ≡ y
---------------------------
Γ |- (x => _A) ≡ (y => y)

Γ, x := z, y := z |- _A ≡ y
---------------------------
Γ |- (x => _A) ≡ (x => y)

Γ, x := z, y := z |- _A ≡ y
---------------------------
Γ |- (x => _A) ≡ (x => y)

Γ, x := z, y := z |- _A ≡ z[z := x]

Γ |- y => x == x => y
-----------------------------------
Γ |- (x => y => x) == (y => x => y)

Γ, y := a, x := a |-
---------------------
Γ |- y => x == x => y


x : A;
y : A;
x = y;
y = x;
M

x : A;
y : A;
x = y;
y = x;
M

M[0 / 1][1 / 0]
M[0 / 1][1 / 0]

M[1 :: 0]

(x : Nat) => (x == x + 1) => _

M[0 == N][1 == K]
M[0 == N][0 == K]

M[0 == N][2 == K]
M[0 == N | 1 == _ | 2 == K]
M[0 == N | 1 == K]
M[0 == 1][1 == 0][0][0]
M[0 == 1][1 == 0][0][0]

M[0 == 1][2 == 0]
M[0 == 1][1 == 0]

(λM)[0 == 1]
λM[↑ | 0 == 1]

M[0 == 1 | 2 == 0]
0[0 == 1][1 == 0][0][0]
1[0 == 1][1 == 0][0][0]

M[0][0][_][_]
M[0 :: 1 :: _][_]


x : Nat;
x = x + 1;

[_ : Nat]
[]

(x : Nat) => (x == x + 1) => _;

Γ |- (y => x) == (x => y)
-----------------------------------
Γ |- (x => y => x) == (y => x => y)


Term (M, N, K) ::= n | λM | M N;

x : A;
y : A;
y = x;
x = y;
M


n[Γ] |-> K[Δ]
-----------------------------------
(1 + n)[m == K][Γ] |-> K[m == K][Δ]

(λM)
(λM)[0 := 1][1 := 0]

M[0 / A][0 / B][0 / C]
M[0 / A :: 0 / B][0 / C]
(1 0)[0 / N][0 / K]
(1 0)[0 / N][0 / K]

x : A;
y : A;
y = x;
x = y;
M

(x : A) => (y : A) => (y == x) => (x == y) => _;

x : A;
y : A;
x = y;
y = x;
M

Term (M, N, K) ::= n | λM | M N | M[N; ...] | M[↑];
Env (Γ, Δ, Φ) ::= • | N :: Γ | Γ[↑ n] | Δ @ Γ[↑ n];


M[K][Γ] |-> M[K; Γ @ ↑]

M[Δ][Γ | n]

M[Δ][↑][Γ | n]

0[N :: _][↑][Γ | 0]
0[N :: _][Γ | 1]
N[↑][Γ | 0]
0[↑][Γ | 1]

0[↑][A :: B :: Γ]
1[A :: B :: Γ]
B[A :: B :: Γ]

0[↑][0 | A :: B :: Γ]
0[1 | A :: B :: Γ]
B[0 | A :: B :: Γ]


0[K][↑][0 | A :: B :: Γ]
0[K][1 | A :: B :: Γ]

0[N][K][A :: B :: Γ]
0[N][K][↑][A :: B :: Γ]
0[N][K][1 | A :: B :: Γ]

0[N][K][↑][A :: B :: Γ]
0[N[↑] :: K[↑] :: A :: B :: Γ]
N[N :: K[↑] :: A :: B :: Γ]

0[N][K][↑][A :: B :: Γ]
0[N][K][↑][A :: B :: Γ]

M[N][K][↑][Γ]
M[N][K][1 | Γ]
M[N][K][1 | K :: Γ]

M[N][K][Γ]
M[N][K :: Γ{↑}]
M[N :: K[↑] :: Γ{↑}]

N[K][↑]
0[N[↑] :: K[↑][↑] :: •][↑]

K[↑][N][K][↑]
1[N[↑] :: K[↑][↑] :: •][↑]

M[1][0]
M[N][K][0 | K :: Γ{↑}]
0[N][K][↑][Γ]
N[K][↑][Γ]
0[N][K][1 | Γ]
0[N][0 | K :: Γ{↑}]

0[0 | K[↑] :: A :: B :: Γ]
1[0 | K[↑] :: A :: B :: Γ]

0[K[↑]][↑][0 | A :: B :: Γ]

0[0 | (1 | K :: A :: B :: Γ)]
K[1 | K :: A :: B :: Γ]


M[N :: •][↑][Γ | 0]
M[N :: •][Γ | 1]
M[N :: Γ | 1]

M[Δ ][Γ] |-> M[Δ @ 0 | Γ]

n[Δ | d][Γ | g] |-> (n + d)[Δ][Γ | g]
N :: Δ[↑ n] @ n | Γ |-> Δ | n @ 1 + n | Γ
Δ | n @ Γ |->

• @ n | Γ |-> Γ @ ↑n

N :: Δ @ n | Γ |-> Δ @ 1 + n | Γ

M[↑][Γ | n] |-> M[Γ | 1 + n]

(eq : Nat == Bool) => not (eq _ 1)
(eq : Nat == Bool) => not 1

[eq] |- not 1

(eq : Nat == Bool) => not 1

(eq : Nat == Bool) => (not (1 : Bool))
(eq : Nat == Bool) => (not (1 : Bool))

M[0 := N; 1 := K]
0[A][0 := N; 1 := K]
0[0 := A; 1 := N[↑]; 2 := K[↑]]

(λM)[n := N] |-> λM[1 + n := N[↑]]
(λM[1 := N[↑]])

M[1 := N][Γ] |-> M[Γ + 1 := N]

m[Γ | n] |-> (m + n)[Γ]

m[↑][Γ | n] |-> m[↑][Γ | 1 + n]

1[1 := N][_ :: 1 :: Γ]

n[1 :: 0]
(x : Nat) => (x == )
(λM)[]


Term (M, N, K) ::= n | λM | M N | M[N; ...] | M[↑];
Env (Γ, Δ, Φ) ::= • | N :: Γ | Γ[↑ n] | Δ @ Γ[↑ n];


1[1 :: 0 :: Δ]

(x => M) N |-> M{x := N}

1[- 0 | 1 :: - 1 | 0 :: Δ]

1[- 0 | 1 :: - 1 | 0 :: Δ] |-> 0[- 0 | 1 :: - 1 | 0 :: Δ]

1[- 0 | 1 :: 0 | 0 :: Δ]


1[- 0 | 1 :: 0 | 0 :: Δ]


(M[↑] : A)[K]
(M[↑] N)

(M[↑] : A) |-> (M[↑] : A)

M[↑][n | A :: B :: Δ]
M[1 + n | A :: B :: Δ]

(M[↑] N)[0 | A :: B :: Δ]
(M N[↓])[1 | A :: B :: Δ]

(M[↑] N)[0 | A :: B :: Δ]

(M[↑] N)[K] |-> M N[K]


(M[↑] N)[0 | A :: B :: Δ]
(M[↑] N)[0 | A :: B :: Δ]
(M N[↓])[1 | A :: B :: Δ]

(M[↑] N)[1 | A :: B :: Δ]

Env ::= (n, N) :: (m, M) :: Δ


(M[↑] N)[0 | A :: B :: Δ] ≡ ( N)[0 | A :: B :: Δ]

((λM)[↑] N)[0 | Γ]
((λM)[↑][0 | Γ] N[0 | Γ])
((λM)[1 | Γ] N[0 | Γ])
(λM[0 :: Γ{↑ 1}]) N[0 | Γ]

((λM)[↑] N)[n | Γ] |-> M[N :: _ :: ]

(λM)[↑][n | Γ] |-> M[N :: _ :: ]

((λM)[↑] N)[n | Γ] |-> M[N :: _ :: ]


M[↑][Γ] |-> M[A :: B :: Δ]
M[K][↑][Γ] |-> M[K[A][↑] :: B :: Δ]
M[↑][Γ] |-> M[A | B :: Δ]

M[↑][A :: B :: Γ] |-> M[A | B :: Δ]
0[↑][A :: B :: Γ] |-> 0[A | B :: Γ]

0[A | B :: Δ] |-> B[A :: B :: Δ]



M[K][A | B :: Γ] |-> M[K :: B :: Γ]
M[K][A | B :: Γ] |-> M[A | B :: Γ]
[↑][Γ] |-> M[A | Γ]


M[K][↑][A :: B :: Γ] |-> M[K :: B :: Γ]

M[K][↑][A :: B :: Γ] |-> M[K :: B :: Γ]

M[↑][A :: Γ] |-> M[A | Γ]
M[K][A | Γ] |-> M[K :: Γ]

M[K][1 | Γ] |->
M[K][1 | Γ] |-> M[K :: Γ{↑}]
M[K][↑][A :: B :: C :: Γ] |-> M[K :: ↑ :: A :: B :: C :: Γ]

M[K][|Γ] |-> M[| K :: Γ]
M[K][A | B :: Γ] |-> M[A | Γ]


[A :: B :: C :: Γ]
[A; B :: C :: Γ]
M[↑][| A :: Γ] |-> M[A | Γ]
M[↑][| A :: Γ] |-> M[A | Γ]

M[K][n | A :: B :: Γ] |-> M[1 + n | A :: B :: Γ]

M[K][n | A :: B :: Γ] |-> M[1 + n | A :: B :: Γ]


x : A;
y : A;
x = y;
y = x;

Block ::= M; B | •
Env ::= M :: Γ | B :: Γ

M; N :: Γ

M[↑][N :: Γ] |-> M[Γ]
M[↑][A; Γ] |-> M[A | Γ]

M[↑][A :: Γ] |-> M[A | Γ]
M[K][A | Γ] |->

M[N][Γ] |-> M[N :: Γ]

0[N :: Γ] |-> N[Γ]

0[(N; B) :: Γ] |-> N[Γ]

0[Δ; N :: Γ] |-> N[Γ]
(1 + n)[Δ | N; B :: Γ] |-> N[Δ :: N | B :: Γ]

0[(0 1)[K] :: B :: Γ]
0[(0 1)[K] :: B :: Γ]
0[A :: B :: Γ]

M[K][N | B; Γ] |-> M[K :: N | B; Γ]

[0]

M[K][B[A]; Γ] |->
M[K][A | B; Γ] |->
M[K][A | B; Γ] |-> M[K :: B; Γ]

M[↑][|(N; B); Γ] |-> M[N :: •; B; Γ]
M[K][Δ | Γ] |-> M[Δ; K :: B; Γ]
M[↑][Δ; K :: B; Γ]
M[K][A :: • | B; Γ] |-> M[K :: B; Γ]
M[K][A | B; Γ] |-> M[K[0 := A] :: B; Γ]

0[A :: B :: Γ]
A[A :: B :: Γ]
[0 := 1][1 := 0][0, 0 :: 0, 0]
x =>
(x == N) =>
(x == M) =>

U = (A : Data, (A = A, A -> ()) -> ());
V = (A = U, A -> ());

Data (1 + _r) : Data (0 + _r)
Data (1 + 0 + _r) : Data (0 + _l)
_l := 1 + _r

U : Data 1 = (A : Data 0, (A = A, A -> ()) -> ());
V = (A = U, A -> ());

U : Data = (A : Data, (A = A, A -> ()) -> ());

(A = U, A -> ()) <: U


(A = U, A -> ()) <: U
(A = U, A -> ()) <: (A : Data 0, (A = A, A -> ()) -> ())
(A = U, A -> ()) <: (A : Data (0 + _l), (A = A, A -> ()) -> ())
(A = _r⇑U, A -> ()) <: (A : Data (0 + _l), (A = A, A -> ()) -> ())
(A = _r⇑U, A -> ()) <: (A = _r⇑U, (A = A, A -> ()) -> ())
_l := 1 + _r

(A = _r⇑U, A -> ()) <: (A = _r⇑U, (A = A, A -> ()) -> ())
(A = _r⇑U, A -> ()) <: (A : Data (0 + _r), (A = A, A -> ()) -> ())


n⇑M : Data l |-> Data (l + n)

(A = _r⇑U, A -> ()) <: (A = _r⇑U, (A = A, A -> ()) -> ())

(A = U, A -> ()) <: (A : Data (0 + _l), (A = A, A -> ()) -> ())

(A = U, A -> ()) <: U
(A = U, A -> ()) <: (A : Data (0 + _l), (A = A, A -> ()) -> ())

(A = _r⇑U, A -> ()) <: _e⇑U
(A = U, A -> ()) <: (A : Data (0 + _l), (A = A, A -> ()) -> ())
_l := 1 + _r
(A = _r⇑U, A -> ()) <: (A = _r⇑U, (A = A, A -> ()) -> ())
(A = _r⇑U, A -> ()) (A : Data (0 + _r), (A = A, A -> ()) -> ())

(A = U, A -> ()) -> U

(A : Data (0 + _l), (A = A, A -> ()) -> ()) <:=
(A = U, A -> ()) <: (A = U, (A = A, A -> ()) -> ())
(A = U, A -> ()) <: U

(A = U, A -> ()) <: (A = U, (A = A, A -> ()) -> ())
(A = U, A -> ()) <: (A = U, (A = A, A -> ()) -> ())
(A = U, A -> ()) <: U
V


----------------------------
(A : Data 1, x : A) : Data 1


(A : Data 0, x : A)

(A = U, A -> ()) <: U
(A = U, A -> ()) <: (A : Data 0, (A = A, A -> ()) -> ())

<r>(A = U<r>, A -> ()) <: U<e>
(A = U<_r>, A -> ()) <: (A : Data e, (A = A, A -> ()) -> ())
(A = U<_r>, A -> ()) <: (A : Data e, (A = A, A -> ()) -> ())

(A = U<e>, A -> ()) <: (A = U<e>, (A = A, A -> ()) -> ())


(A = U, A -> ()) <: U
(A = U, A -> ()) <: (A : Data : Univ, (A = A, A -> ()) -> ())
(A = U, A -> ()) <: (A : Data, (A = A, A -> ()) -> ())

A = U

(A = _r⇑U, A -> ()) <: (A : Data (0 + _e), (A = A, A -> ()) -> ())


x : Nat;
eq : x == 1;

x = 1;
eq = refl;

M[0 :: 0]

M[↑][A :: B :: Γ]
M[↑][A | B :: Γ]

M[K :: A :: B :: Γ]

0[K :: A :: B :: Γ]
K[K :: A :: B :: Γ]

M[K][A | B :: Γ]
M[A | K :: B :: Γ]

0[↑]


(x => M)

(x => x + x)
(fn x => x + x)

(x => y => x + y)

[] | (λ.λ. \2 + \1) 15 13
15 :: [] | (λ. \2 + \1) 13
13 :: 15 :: [] | \2 + \1
13 :: 15 :: [] | 15 + 13
13 :: 15 :: [] | 28
[] | 28

2 | [15; 13; M] | \2 + \1


[15; 13; M]

[15; [N; 13]; M]

[15; [N; 13]; M]
read (sp - i)
read (2 - 1)


print : (msg : String) -[IO]> ();

map : <A, B>(l : List A, f : (x : A) -> B) -[Alloc (length l)]> List B;

((x : Int) => (1 : Nat)) <: ((x : Nat) => (1 : Int))

((x :))



-------------------
Γ |- \0[Φ] == \0[Δ]


-------------------
(x => M) ≡ (y => N)


(
  Nat : Data;
  Nat = <A>(zero : A, succ : (pred : Nat) -> A) -> A;
  Nat
) = (
  Nat : Data;
  Nat = <A>(zero : A, succ : (pred : Nat) -> A) -> A;
  <A>(zero : A, succ : (pred : Nat) -> A) -> A
)

(A : +Type) -> (P : (z : A) -> Type) -> (x : P x) -> P y

(x => x x) (x => x x)

Type : Type
Data : Type
Prop : Type


(M[K] N) |-> (M N[↑])[K]
(M[↑] N)[K] |-> M N[K]

∀

((f x) y) ≡ (f z)


Id = <A>(x : A) -> A;
f : (id : Id) -> Id = _;
x = f; // Id -> Id, no binder



(x => M N) K
((x : A : Type) => (M : B : Type) (N : C : Type)) (K : D : Type)

(1 : String : Type 0)
(Type 0)

(A : Type 0) =>
(String : Type 0)


A = @self(x). T

A : Type;
A = (x : A) & T;


((x : Id A) : A) = _;


map : ;
map = (l, f) => l | [] => [] | el :: tl => f(el) :: map(tl, f);


Id : [l] -> Type (1 + l) =
  [l] => (A : Type l) -> (x : A) -> x;


id = (A : Prop) => (x : A) => x;

Type : Type

/M\
\M/
/M/
\M\

^

#macros
%macros
@

x%/* 1 */
```

## Symbols

33 !
34 "
35 #
36 $
37 %
38 &
39 '
40 (
41 )
42 \*
43 +
44 ,
45 -
46 .
47 /
58 :
59 ;
60 <
61 =
62 >
63 ?
64 @
91 [
92 \
93 ]
94 ^
95 \_
96 `
123 {
124 |
125 }
126 ~

## Symbol Table

37 %
94^

92 \

126 ~

Candidates :

33 !
63 ?

64 @

35 #
36 $

96 `

Confirmed :

32  
46 .

40 ( 60 < 91 [ 123 {
41 ) 62 > 93 ] 125 }

34 " 39 ' 124 |

42 \*
43 +
45 -
47 /

58 :
59 ;
61 =

38 &
44 ,

95 \_

## Typed Tree

```rust
#(M : A)
M#[x := N]
M#[l]
#Type

x#1
(M : A)

M # match # N

#print

f # a
f # a
(f #) a

x#1

(a #1)
f a # 1
f a # 1

(f a)#1

x (#1)

f # debug
(f #) debug
f (# debug)

#print "Hello"

concat #"Hello" "World"

#\"Hello"

∀A. (x : A) -> A

#forall A. ((x : A) -> A)
#"Hello" = 1;
f #A

M = [%graphql {
  users(id: 0) {
    name
    email
  }
}];

M = [%graphql { users (id : 0) { name email } }];

f = [%c {
  x: for (i = 0; i <= 10; i++) {
    print(i);
  };

  if (true) {
    print("a");
  } else {
    print("b");
  };

  goto x;
}];

#debug "Hello World"

(A : Type) => (x : A) => _


ind : <P : (b : Bool) -> Type>(b : Bool, then : P true, else : P false) -> P b;

Unit : Type;
unit : Unit;

Unit = (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u;
unit = P => x => x;

Bool : Type;
true : Bool;
false : Bool

Bool = (b : Bool) & (P : (b : Bool) -> Type) ->
  (x : P true) -> (y : P false) -> P b;
true = P => x => y => x;
false = P => x => y => y;

ind (b : Bool)
  : (P : (b : Bool) -> Type) -> (x : P true) -> (y : P false) -> P b
  = b
Unit : Type;
unit : Unit;

Unit = (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u;
unit = P => x => x;

Unit : Type = <A : Type>(x : A) -> A;
unit : Unit = x => x;


(x : A) & B

(x : Int) -> Int
(x : String) -> String


unit : ((x : Int) -> Int) & ((x : String) -> String) = x => x

f = (x : ((x : Int) -> Int) & ((x : String) -> String)) => _;
```

## Teej

```rust
Id = <A>(x : A) -> A;
id : Id = x => x;

x : String = id "a";

id = (A : Data) => (x : A) => x;
x = id String;

Id = (A : Data) => A;
x : Id String = "Hello";


refl : <A>(x : A) -> x == x = _;
add = (a, b) => a + b;

_ : add (1, 2) == 3 = refl _;

Array : {
  create : <A>(length : Int & length >= 0, initial : A) -> Array<A>;
} = _;

_ = (length : Int & (length >= 0) : Prop);

f = (length : Int) =>
  length >= 0
  | true => create
  | false => _


(u : (A : Type#1) -> A#3) =>
  (u : (A : Type#1) -> A#4)


[Type; String; u] + 0; [Type; String; u] + 0 |-
  (A : Type#1) -> A#4 ≡ ((A : Type#1) -> A#3)#2
 + 0; [Type; String; u] + 0 |-
  (A : Type#1) -> A#4 ≡ ((A : Type#1) -> A#3)#2


[Type; String; u];

[Type; String; u] + 0 | [Type; String; u] + 0 |-
  (A : Type#1) -> A#4 ≡ ((A : Type#1) -> A#3)#2
[Type; String; u] + 0 | [Type; String] + 1 |-
  (A : Type#1) -> A#4 ≡ (A : Type#1) -> A#3
[Type; String; u; A] + 0 | [Type; String; A + 1] + 1 |-
  A#4 ≡ A#3 // works


(A : Type : Meta)

((M : A : Data) : (A : Meta))
(1 : Meta)


((x : _a) => x + 1)
((x : int) => x + 1)

_A : Type <: Int

_A ≡ Int

_A := [Int]

_A ≡ String

_A := [Int; String]

_A := Int & String


P (not b) ≡ _F b
P (not b) ≡ (x => _M) b
P (not b) ≡ _M[x := b]
(P (not b))[b := x] ≡ _M[x := b][b := x]
(P (not b))[b := x] ≡ _M
_M := P (not x)

_F := x => _M

P (not b) ≡ (x => P (not x)) b
P (not b) ≡ P (not b)


STLC + Option + Int + String + List

(id : 'A. 'A -> 'A : Type 2)


(f : _B -> _) => f id
('K <: 'A. 'A -> 'A). (f : 'K -> _) => f id

'B. (f : ('B -> 'B) -> _) => f id

_A : Type 0

_A :=


   |--|
λ. λ.-1

|-----|
λ. λ.-2

   |--|
λ. λ.+1
|-----|
λ. λ.+0

Int#+1 -> ((A : Type) -> A#-1) -> ()

λA. λ(x : A#1 -> A#1). (x : A#2 -> A#2)
λA. λ(x : (∀A. A#2)). (x : (∀A. A#3))

5
id = (M : _A#6 -> _A#6);
id = (M : 'A. 'A -> 'A);

id = (M : 'A#-1 -> 'A#-1);

Mono (τ) := σ -> τ | A#+l
Poly (σ) := τ | ∀. σ

Term =
  | T_arrow
  | T_var of { level : int }

Type = { mutable desc : Desc; mutable level : int }
Desc =
  | T_arrow
  | T_var
  | T_link of { to_ : Type }

Type = { mutable desc : Desc;  }
Desc =
  | T_arrow of { param : Type; return : Type; }
  | T_link of { to_ : Type }
  | T_free_var of { mutable binder : int }
  | T_bound_var of { mutable binder : int }

∀. A#-1 -> B#-1 -> B#-1
∀. A#g -> B#g -> B#g



∀. (∀. A#-1 -> B#-2) -> B#-1

∀. (∀. A#0#-1 -> B#1#-2) -> B#1#-1

(∀. A#0#-1 -> _B#1) -> _B#1

(x => M) ⇐ ((x : Int) -> Int)

((x : Int) => (M : Int)) ⇒ (x : Int) -> Int

_A := _A -> _A



gen (_A -> _A)
gen 4 (_A#+5 -> _A#+5)
gen (_A#G -> _A#G)

∀. (∀. A#-2 -> B#-1) -> A#-


T =
  | (x : Int, tag == true, y : Int)
  | (x : Int, tag == false);


[Type; String; u] + 0 | [Type; String; u] + 0 |-
  (A : Type#1) -> A#4 ≡ ((A : Type#1) -> A#3)#2
[Type; String; u] + 0 | [Type; String] + 1 |-
  (A : Type#1) -> A#4 ≡ (A : Type#1) -> A#3
[Type; String; u; A] + 0 | [Type; String; A + 1] + 1 |-
  A#4 ≡ A#3

[Type; String; u] + 0 | [Type; String; u] + 0 |-
  (A : Type#1) -> x = A#4; x#5 ≡ (A : Type#1) -> (x = A#3; x#4)#2

[Type; String; u] + 0 | [Type; String; u] + 0 |-
  (A : Type#1) -> (x = A#4 Type#1; x#5) ≡ ((A : Type#1) -> (x = A#3 Type#1; x#4))#2
[Type; String; u] + 0 | [Type; String] + 1 |-
  (A : Type#1) -> x = A#4; x#5 ≡ (A : Type#1) -> x = A#3; x#4
[Type; String; u; A] + 0 | [Type; String; A + 1] + 1 |-
  x = A#4 Type#1; x#5 ≡ x = A#3 Type#1; x#4
[Type; String; u; A; x == A#4] + 0 | [Type; String; A + 1; x == A#3] + 1 |-
  x#5 ≡ x#4
[Type; String; u; A] + 0 | [Type; String; A + 1] + 1 |-
  A#4 ≡ A#3

[Type; String; u] + 0 | [Type; String] + 1 |-
  A#4 ≡ A#4

[Type; String; u] + 0 | [Type; String; u] + 0 |-
  x = Type#1; x#4 ≡ (x = Type#1; x#3)#2
[Type; String; u] + 0 | [Type; String] + 1 |-
  x = Type#1; x#4 ≡ x = Type#1; x#3
[Type; String; u; x == Type#1] + 0 | [Type; String; x == Type#1] + 1 |-
  x#4 ≡ x#3


(A : Type) => (A : Type)


1 : (A : Type) => (x : A) => id A x

2 : (A : Type) => (x : A) =>
  (((id : _A) (A : _B) : _C) (x : _D) : _E)

3 : (A : Type) => (x : A) =>
  (((id : (A : Type) -> (x : A) -> A) (A : Type) : (x : A) -> A) (x : A) : A)


(abc) => abd


f(f(f(f(f(f(M))))))


[Type; String; u] + 0 | [Type; String; u] + 0 |-
  (A : Type#1) -> (x = A#4 Type#1; x#5) ≡ ((A : Type#1) -> (x = A#3 Type#1; x#4))#2
[Type; String; u] + 0 | [Type; String] + 1 |-
  (A : Type#1) -> (x = A#4 Type#1; x#5) ≡ (A : Type#1) -> (x = A#3 Type#1; x#4)
[Type; String; u; A] + 0 | [Type; String; A + 1] + 1 |-
  x = A#4 Type#1; x#5 ≡ x = A#3 Type#1; x#4
[Type; String; u; A; x = A#4 Type#1] + 0 | [Type; String; A + 1; x = A#3 Type#1] + 1 |-
  x#5 ≡ x#4
[Type; String; u; A] + 0 | [Type; String; A + 1] + 1 |-
  A#4 Type#1 ≡ A#3 Type#1
[Type; String; u; A] + 0 | [Type; String; A + 1] + 1 |-
  A#4 Type#1 ≡ A#3 Type#1

[Type; String; u] + 0 | [Type; String; u] + 0 |-
  (A : Type#1) -> A#4 ≡ ((A : Type#1) -> A#3)#2
[Type; String; u] + 0 | [Type; String] + 1 |-
  (A : Type#1) -> A#4 ≡ (A : Type#1) -> A#3
[Type; String; u; A#4] + 0 | [Type; String; A#4] + 1 |-
  A#4 ≡ A#3
[Type; String; u] + 0 | [Type; String] + 1 |-
  A#4 ≡ A#4

[Type; String; u] + 0 | [Type; String; u] + 0 |-
  (A : Type#1) -> (B : Type#1) -> A#4 ≡ ((A : Type#1) -> (B : Type#1) -> (A#3)#3)#2

[Type; String; u] + 0 | [Type; String] + 1 |-
  (A : Type#1) -> (B : Type#1) -> A#4 ≡ (A : Type#1) -> (B : Type#1) -> (A#3)#3
[Type; String; u; A#4] + 0 | [Type; String; A#4] + 1 |-
  (B : Type#1) -> A#4 ≡ (B : Type#1) -> (A#3)#3
[Type; String; u; A#4; B] + 0 | [Type; String; A#4; B] + 1 |-
  A#4 ≡ (A#3)#3
[Type; String; u; A#4; B] + 0 | [Type; String; A#4] + 1 |-
  A#4 ≡ A#3


3 | [Type; String; u] | [Type; String; u] |-
  (A : Type#1) -> (B : Type#1) -> A#4 ≡ ((A : Type#1) -> (B : Type#1) -> (A#3)#3)#2
3 | [Type; String; u] | [Type; String; u] |-
  (A : Type#1) -> (B : Type#1) -> A#4 ≡ ((A : Type#1) -> (B : Type#1) -> (A#3)#3)#2
3 | [Type; String; u] | [Type; String] |-
  (A : Type#1) -> (B : Type#1) -> A#4 ≡ (A : Type#1) -> (B : Type#1) -> (A#3)#3
4 | [Type; String; u; A#4] | [Type; String; A#4] |-
  (B : Type#1) -> A#4 ≡ (B : Type#1) -> (A#3)#3
4 | [Type; String; u; A#4; B] | [Type; String; A#4; B] |-
  A#4 ≡ (A#3)#3
4 | [Type; String; u; A#4; B] | [Type; String; A#4] |-
  A#4 ≡ A#4

[Type; String; u]#3 | [Type; String; u]#3 |-
  (A : Type#1) -> (x = _; A#4) ≡ ((A : Type#1) -> A#3)#2
[Type; String; u]#3 | [Type; String]#3 |-
  (A : Type#1) -> (x = _; A#4) ≡ (A : Type#1) -> A#3
[Type; String; u; A#4]#4 | [Type; String; A#4]#4 |-
  (x = _; A#4) ≡ A#3
[Type; String; u; A#4; x]#5 | [Type; String; A#4]#4 |-
  (A#4) ≡ A#3


[Type; String; u]#3 | [Type; String; u]#3 |-
  x = _; (A : Type#1) -> A#5 ≡ ((A : Type#1) -> A#3)#2
[Type; String; u]#3 | [Type; String]#3 |-
  x = _; (A : Type#1) -> A#5 ≡ (A : Type#1) -> A#3
[Type; String; u; x]#4 | [Type; String]#3 |-
  (A : Type#1) -> A#5 ≡ (A : Type#1) -> A#3
[Type; String; u; x]#5 | [Type; String]#3 |-
  (A : Type#1) -> A#5 ≡ (A : Type#1) -> A#3


[Type; String]#2 | [Type; String]#2 |-
  x = _; (A : Type#1) -> A#4 ≡ (A : Type#1) -> A#3
[Type; String; x = _; ]#3 | [Type; String]#2 |-
  (A : Type#1) -> A#4 ≡ (A : Type#1) -> A#3
[Type; String; x = _; A] | [Type; String; A] |-
  A#4 ≡ A#3

[Type; String]#2 | [Type; String]#2 |-
  x = _; (A : Type#1) -> A#4 ≡ (A : Type#1) -> A#3

[Type; String]#2 | [Type; String]#2 |-
  x = (A : Type#1) -> A#3; (B : Type#1) -> x#3  ≡
    (B : Type#1) -> (A : Type#1) -> A#4
[Type; String; x = (A : Type#1) -> A#3; B#3]#4 | [Type; String; B#3]#4 |-
  x#3 ≡ (A : Type#1) -> A#4

| [Type; String]#4
| [Type; String; B#3]#4 |-
  (A : Type#1) -> A#3 ≡ (A : Type#1) -> A#4
| [Type; String; A#4]#5
| [Type; String; B#3; A#4]#5 |-
  A#3 ≡ A#4
| [Type; String; A#4]#5
| [Type; String; B#3; A#4]#5 |-
  A#4 ≡ A#4


B#3#{((C : Type#1) -> C#4)}#3


received : B#3#{((C : Type#1) -> C#4)}#3
expected : (D : Type#1) -> D#4

((B : Type) -> B#3)#3

((x => x\0)[↑] N)[K] |-> x\0[N[K]]

(f : (B : Type) -> B#3) =>
  (f#3 : ((B : Type) -> B#3)#3) ((C : Type#1) -> C#4)

B#3#(C : Type#1) -> C#4][↑]

B#3#{((C : Type#1) -> C#4)}#3


M : ((x : A) -> B)#{Δ}
----------------------
M N : B#{N}#{Δ}

M : ((x : A) -> B)#{Δ}
--------------------------
M N : ((x : A) => B)#{Δ} N

M : ((x : A) -> B)#{Δ}
--------------------------
M N : _T N

((x : A) => B)[x := N][Δ] ≡ ((x : _C) => _D)[x := N][_Φ]

M : ((x : A) -> B)[Δ]  ((x : A) => B)[Δ] N ≡ _T N
-------------------------------------------------
M N : _T N

M N : _B#{x : _A := N}

M : ((x : A) -> B)#{Δ}
----------------------
M N : B#{Δ}#{←}#{N}

B#{N}#{Δ}

Γ |- M ⇔ B
-------------------
Γ, A |- M[↑] ⇔ B[↑]
```

## Memory

Header

| size | color | tag |
| ---- | ----- | --- |
|      | 2     | 5   |

| color | tag | extend |
| ----- | --- | ------ |
| 2     | 5   | true   |

```ocaml
type 'a list =
  | Nil
  | Cons of ('a * 'a list)

// to
List : (A : Data) -> A;
List = A =>
  Nil | Cons (hd : A, tl : List A);

```

## Debugger

```rust
x = #debug 1;
```

## Effect

```rust


// simple, single monad effects
Γ, x : A |- B : Type  Γ, x : A |- E : (y : Type) -> Type
--------------------------------------------------------
Γ |- (x : A) -[E]> B : Type

// generalized unions
Γ |- A : Type  Γ |- M : A  Γ |- N : A
-------------------------------------
Γ |- M | N : A

// symbols instead of String would be dope
Log = K => (tag : String, witness : tag | "log" => K == Unit | _ => Never);
Read = K => (tag : String, witness : tag | "read" => K == String | _ => Never);

// handler just applies effect
handler : <A, E>(
  init : () -[E]> A,
  cb : <K>(eff : E K, k : (x : K) -> A) -> A
);
f : () -[Log | Read]> String = _;

() = handle f (<K>(eff : (Log | Read) K, k) -> _);

// what is a (Log | Read) K?
(Log | Read) K == (
  tag : String,
  witness :
    tag
    | "log" => K == Unit
    | "read" => K == String
    | _ => Never
)


(Log | Read) K
(x => Log x | x => Read x) K // eta
(x => Log x | Read x) K // extrusion
Log K | Read K // beta
// delta
(K => (tag : String, witness : tag | "log" => K == Unit | _ => Never)) K
| (K => (tag : String, witness : tag | "read" => K == String | _ => Never)) K
// beta
(tag : String, witness : tag | "log" => K == Unit | _ => Never)
| (tag : String, witness : tag | "read" => K == String | _ => Never)
// extrusion + pattern merge
(tag : String, witness : tag | "log" => K == Unit | "read" => K == String | _ => Never)

(Log | Read) K == (
  tag : String,
  witness :
    tag
    | "log" => K == Unit
    | "read" => K == String
    | _ => Never
)


// symbols instead of String would be dope
Log = K => (tag : Bool, tag (K == Unit) Never);
Read = K => (tag : Bool, tag Never (K == String));

// handler just applies effect
handler : <A, E>(
  init : () -[E]> A,
  cb : <K>(eff : E K, k : (x : K) -> A) -> A
);
f : () -[Log | Read]> String = _;

() = handle f (<K>(eff : (Log | Read) K, k) -> _);

// what is a (Log | Read) K?
(Log | Read) K == (
  tag : String,
  witness :
    tag
    | "log" => K == Unit
    | "read" => K == String
    | _ => Never
)


Log = K => (tag : Bool, tag (K == Unit) Never);
Read = K => (tag : Bool, tag Never (K == String));

(Log | Read) K
// eta
(x => Log x | x => Read x) K
// extrusion
(x => Log x | Read x) K
// beta
Log K | Read K
// delta
((K => (tag : Bool, tag (K == Unit) Never))
| (K => (tag : Bool, tag Never (K == String)))) K
// beta
(tag : Bool, tag (K == Unit) Never)
| (tag : Bool, tag Never (K == String))
// extrusion
(tag : Bool, (tag (K == Unit) Never) | (tag Never (K == String)))
// extrusion
(tag : Bool, (tag (K == Unit) | tag Never) (Never | K == String))
// extrusion
(tag : Bool, tag ((K == Unit) | Never) (Never | K == String))
// refute
(tag : Bool, tag (K == Unit) (K == String))


K M | K N |-> K (M | N)
M K | N K |-> (M | N) K

(tag (K == Unit) Never) | (tag Never (K == String))

((tag (K == Unit)) Never) | ((tag Never) (K == String))
((tag (K == Unit | Never)) Never) | ((tag (K == Unit | Never)) (K == String))
tag (K == Unit | Never) (Never | K == String);

  tag (K == Unit) | tag Never |-> tag (K == Unit | Never)


(Log | Read) K == (tag : Bool, tag (K == Unit) (K == String))




M + N
M |-> VM     N |-> VN

VM + VN

(b => x => b | true => x | false => 1) true (1 + 2 + 3 + 4)

(x => x + x + 10) (1 + 2 + 3 + 4)
10 + 10 + 10

(x => x + x + x) (1 + 2 + 3 + 4)
(1 + 2 + 3 + 4) + (1 + 2 + 3 + 4) + (1 + 2 + 3 + 4)

(x = 1 + 2 + 3 + 4; x + x + x)


(x => M) N |-> (x = N; M)
(x = N; C<x>) |-> (x = N; C<N>)

(x = 1 + 2 + 3 + 4; x + x + x)
(x = 1 + 2 + 3 + 4; (_ + x + x)<x>)
(x = 1 + 2 + 3 + 4; (_ + x + x)<1 + 2 + 3 + 4>)
(x = 1 + 2 + 3 + 4; (1 + 2 + 3 + 4) + x + x)
b => (x = 1 + 2 + 3 + 4; x + x + (b | true => x | false => 0))

Γ |- A : Type
--------------------------
Γ, x : A |- @typeof x == A

Γ, x : A |- B : Type
-------------------------------------------
Γ |- @typeof ((x : A) => M) == (x : A) -> B

Γ, x : A |- @typeof x : Type
---------------------
Γ |- x : @typeof x


M : ((x : A) -> B)[Δ]  ((x : A) => B)[Δ] N ≡ _T N
-------------------------------------------------
M N : _T N

Γ |- M ⇒ ((x : A) -> B)[Δ]  Γ |- N ⇐ A[Δ]
-----------------------------------------
Γ |- M N ⇒ ((x : A) => B) N

Γ |- M ⇒ ((x : A) -> B)[Δ]  Γ |- N ⇐ A[Δ]
-----------------------------------------
Γ |- M N ⇒ ((x : A) => B) N

Γ |- M ⇒ ((x : A) -> B)[Δ]  Γ |- N ⇐ A[Δ]
-----------------------------------------
Γ |- M N ⇒ (_T : (_ : _A) -> Type) N


M : ((x : _A) -> B)[Δ]  N : _A[Δ]
-----------------------------------------
M N : (_T : (_x : _A) -> Type)[_Δ] N

M N : _x : _A[_Δ] = N; (_T : (_x : _A) -> Type)[_Δ[↑]]

M N : (_T : (_x : _A) -> Type)[_Δ] N


M : ((x : A) -> B)[Δ]  N : A[Δ]
-----------------------------------------
M N : ((x : A) => B)[Δ] N


M : ((x : A) -> B)[Δ]  N : A[Δ]
-------------------------------
M N : ((x : A) => B)[Δ] N



failwith "a"

x.y <- failwith "a"
setfield (goto 1)


x.y <- failwith "a"


map f l = map f l;

map f l =
  l
  | [] => []
  | hd :: tl => f hd :: map f tl;


rec : <T, a>(f : (self : <b>(l : b < a) -> T b) -> T a) -> T a;

match x with
| l -> _
| exception Not_valid -> _

(String | Number) & Number == Number?

(String | Number) & Number == Number?


F = <A>(x : Number & A) -> A;
G = <A <: Number>(x : A) -> A;

id = x => x;

(id : <A>(x : A) -> A)<Number>

id : A -> A
(x : 'a) -> 'a l


f : () -> String & Int = _;


∀a. ∀b. a -> (a -> b) -> b

// de-bruijin indices
∀. ∀. 2 -> (2 -> 1) -> 1

// many variables per binder
∀a b. a -> (a -> b) -> b

// many variables per indice, binder : id
∀. 1:a -> (1:a -> 1:b) -> 1:b

// split variable introduction of variable usage
// n! means introduces variable at binder offset n
// n# means variable at stack offset n
// leftmost first
∀. 1! -> (1# -> 1!) -> 1


∀a. ∀b. a -> (a -> b) -> b

'a -> ('a -> 'b) -> 'b

(M : 'b. 'a. 'a -> ('a -> 'b) -> 'b)

arr 'a (arr (arr 'a 'b) 'b);



(() => {
  console.log('a');
  return () => console.log('b')
})((() => console.log('c'))())((() => console.log('d'))())



(f M) N

f 1 + g 2

f 1 $ x => g 2 $ y => k (x + y)
g 2 $ y => f 1 $ x => k (x + y)

f () $ () => g 1 k

(f (), g ())


pair (f ()) (g ())

map f l =
  l
  | [] => []
  | hd :: tl => (
    tl = map f tl;
    f hd :: tl
  )

map k f l =
  l
  | [] => k []
  | hd :: tl => (
    hd = f hd;
    map (tl => hd :: tl) f tl
  )

sum l =
  | [] => 0
  | hd :: tl => hd + sum tl;


(x => 1) ((x => x x) (x => x x))


Term (M, N)
Type (A, B) =
  | Data
  | Type
  | Prop
  | x
  | P => M
  | (P : A) -> B
  | M N
  | P : A; B
  | P = M; B
  | (P : A) & B
  | (M, ...)
  | { P : A; ... }
Pattern =
  | x
  | (M, ...)
  | { P; ... };


map : _;
map = (f, l) =>
  l
  | [] => []
  | hd :: tl => f hd :: map(f, tl);

make : <A>(l : Int & l >= 0, initial : A) -> Array A;
id = x => x;

Nat = @nominal {
  Nat : Data;
  zero : Nat;
  succ : (pred : Nat) -> Nat;

  Nat = (n : Nat) & (P : (n : Nat) -> Data) ->
    (z : P zero) -> (s : (pred : Nat) -> P (succ pred)) -> P n;
  zero = P => z => s => z;
  succ = pred => P => z => s => s pred;
};

@nominal Nat = <A>(z : A, succ : (x : A) -> A) -> A;

User = {
  id : Nat;
  name : String;
};

(f : { id : Nat; _ }) => (f : User).x

Show = {

};


Γ |- A : Type  Γ |- M : A  Γ |- B : A
-------------------------------------
Γ |- M | N : A

Γ, x : A |- B : Type  Γ, x : A |- M : Type -> Type
--------------------------------------------------
Γ |- (x : A) -[M]> B : Type


print : (msg : String) -[Log]> ()
read : (file : String) -[Read]> String

Read = K => (tag == "read", K == String);
Log = K => (tag == "log", K == Unit)

K =>
  | (tag == "read", K == String)
  | (tag == "log", K == Unit);

M => x =[Read | Log | M]> (
  data = read("tuturu.txt");
  print(data)
);

Either = (A, B) =>
  (tag : Bool, payload : tag | true => A | false => B);
Either =
  | (tag == true, payload : Int)
  | (tag == false, payload : String);

((x => x + 1) | (x => x + 2) : (x : Nat) -> Nat)

(x => x + 1 | x + 2) 1

call x9;

1 + 1 | 1 + 2
2 | 3


Type : Type;
Data : Type;
STLC : Type;

(A : Data : Type) => 1

T = (A <: {}) => { x : Int } & A;
T { y : Int; }
T Never

ω
∞
{ x : Int }

() =(IO | IO)> ()


// infers (initial : Int, final : Int) -> Int
delta = ?(
  initial = _;
  final = _;

  final - initial;
);


delta : {
  (initial : Int) =>
  (final : Int) =>

  x : Int;
} = ?(
  initial : Int;
  final : Int;

  x = final - initial
);
i


f => x => f x


     |------|
f => x => f x

|---------|
f => x => f x



f => x => (
  z = f x;
  f z
);

(f, x) => (

  z = f x;

  f z

);

|-|
| f => x => (
|
|   z = f x;
|---|
    f z

  );

  |-|
λ.λ.0

|---|
λ.λ.1

 |----|
λx.λy.x

 |----|
λx y. x + y

Ω
Opacity aka negative highlighting of everything else



Term =
  | Type
  | x
  | (x : A $ n) -> B
  | (x : A $ n) => M
  | M N;


((x : A $ 0) => M) N |-> M // beta-0
((x : A $ 1) => M) N |-> M[x := N] // beta-1


(f => f M) N

(f => f M) (N (x => x))
N (f => f M)

----------------------
Γ, x == N2 |- x |-> N2


Γ |- N |-> N2  Γ, x == N2 |- M |-> M2
------------------------------
Γ |- (x => M) N

Γ |- M |-> x => M2  Γ, x == N2 |- M2 |-> K
------------------------------------------
Γ |- M N |-> K


Γ |- M¹ : (x : ); m  n <= small
Γ |- M¹ |-> x => M²
------------------------------------------
Γ |- M N |-> M²[x := N]

Γ |- M¹; n  n <= small
Γ |- M¹ |-> x => M²  Γ |- N²[x := N] |-> K
------------------------------------------


M N K

(x $ 1 : A; n) -> B

(f => x => f M) N

(f => x => f M) N

(x => M) N K

Check Ready, Atomic
Consume Queue, Up to Gas
Check Ready, Atomic
Wait Mutex? Consume Queue, Up to Gas


Data : Type

Γ |- A : Data  Γ, x : A |- B : Data
-----------------------------------
Γ |- (x : A) -> B : Data

// erasable, System F like
Γ |- A : Type  Γ, x : A |- B : Data
-----------------------------------
Γ |- (x : A) -> B : Data

Id : Data = (A : Data) -> (x : A) -> A;

// monomorphical, HM like
Γ |- A : Type  Γ, x : A |- B : Data
-----------------------------------
Γ |- (x : A) -> B : Type

Id : Type = (A : Data) -> (x : A) -> A;
id : Id = A => x => x;

call_id = (id : (A : Data) -> (x : A) -> A) => ()
main = (x : String) => (
  y = id String x;
  y
);


(x => M) N |-> M[x := N] // beta

Γ, x |- M |-> Γ |-> M // weakening
Γ¹, x, Γ², y, Γ³ |- M |-> Γ¹, y, Γ², x, Γ³ |-> M // exchange

Γ |- (x => M) N |-> Γ |- M[x := N] // beta



F = R => (Int, Int, ...R) & (\)
(Int, Int, ...R)

A -> B

...A;
...B;
ARROW;


∀. A -> B;

...A;
...B;
ARROW;
FORALL;

#0 -> 1 // 'a -> 'a

NEW_VAR 0;
BOUND_VAR 1;
ARROW;

PUSH




Term =
  | #n
  | n
  | M N;

λf. λv. f v

(f v)


(λM)[Δ] N |-> M[⇑Δ][N]
(λM)[Δ] N |-> M[N[↑|Δ|]][Δ]
(λM)[Δ] N |-> M[N][Δ]

(λM)[Δ] N |-> M[|Δ|][Δ][N]

(M N)[Δ]

∀x. ∀y : _A -> x\-2
∀x. ∀y : _B -> _B

(λ(M N)) K
(M N)[K]
(λM) ((λM) N)

M[Δ][N]
M[N . Δ]

n[Δ] N



(2[K] N)[M]
(2[K] N)[M]

(λM)[Δ] N |-> M[N[↑|Δ|]][Δ]
(λM)[Δ] N |-> M[N][Δ]
(λM)[Δ] N |-> M[Δ[↑]][N]

(λM)[Δ] N


(λM)[Δ] N |-> M[N][Δ]
(λM)[Δ] N |-> M[open N][Δ]

((λM)[Δ] N)[Γ] |-> M[N][Δ][Γ]

Γ |- A : Type
-------------
Γ |- M : A

Γ |- M : A
--------------------
Γ, K |- ↑; M : ↑; A

(λλ((λM) (-1 -2))) N K
N; (λ((λM) (-1 -2))) K
N; K; (λM) (-1 -2)
N; K; (-1 -2); M
(λ((λM)[N] 0)) K

K; (λM)[N] 0
K; M[0][N]

LAMBDA ...M;
PUSH ...N;
APPLY;
C;

((λ2) N)[K]
C | N :: LAMBDA M :: S |-> M | N :: S

Γ |- M : A
-------------------
Γ, K |- M[↑] : A[↑]


(λM)[Δ] N |-> M[N[↑|Δ|]][Δ]

LAMBDA M;
APPLY N;
C;


VAR 0;
APPLY N;
C;



VAR n   ; C | S             | E |-> E n; C | S                 | E
LAMBDA M; C | S             | E |-> C      | LAMBDA M :: S     | E
APPLY N ; C | LAMBDA M :: S | E |-> M  ; C | S                 | N :: E
APPLY N ; C | VAR n :: S    | E |-> C      | N :: VAR n :: S   | E
APPLY N ; C | APPLY K :: S  | E |-> C      | N :: APPLY K :: S | E



A B != C D

βA B != βC D

y = 2;
f = x => (
  y = x + 1;
  a;
);

(λM)[Δ] N |-> M[N][Δ]


A B != C D

βA B != βC D

M _A

(λ1) N

M[↑] N
(x => x) (x => x)
(x => x)[x := x]
(y => y)[x := x]

// uncommon
(λM)[↑] N

// created by
(λM)[K] N

λM;
(M[↑] N)[K]


(x = M; y => f x y) N
(x = M; y = N; f x y)

λ1 | 0 == K; λ2 | 0
λ1 | 0 == λ2 | 1

λ1 | 0 == K; λ(N; 2) | 0

λ1 | 0 == λ(N; 2) | 1
1 | 0 == N; 2 | 1
1 | 0 == 2 | 2

λ1 | 0 == K; λ(N; 2) | 0


(M[↑] N)[K] |-> M N[K]

(λ. λ. ((λ. M) (-1 -2))) N K
N; (λ. ((λ. M) (-1 -2))) K
N; K; -1 -2; M

(λ. λ. (λ. λ. -2) -2) 4 5
4; 5; (λ. λ. -2) -2 -1
4; 5; -2; -1; 4


(λ. λ. (λ. λ. -2) -2) 4 5
4; 5; (λ. λ. -2) -2
4; 5; -2; λ. -2



|- λ1 == K |- λ(N; 2)
λ |- 1 == K; λ; N |- 2

M[0 := x] == N[0 := x]
----------------------
λM == λN

|- λ1 | 0 == K |- λ(N; 2) | 1

|- λ1 | 0 == K |- λ(N; 2) | 1
λ |- 1 | 0 == K |- λ(N; 2) | 1
λ |- 1 | 0 == K; λ |- N; 2 | 1
λ |- 1 | 0 == K; λ; N |- 2 | 2
λ |- 1 | 0 == K; λ |- 1 | 1

|- λ1 == K |- λ(N; 2)
x |- 1 == K; x; N |- 2
x |- x == K; x; N |- x


zone : (cb : <K, 'l>() -['l]> K) -> K;

zone @@ () =>
f : &'l A -['l]> B

// most apply are not at a distance

g = y => y;
f (g x)

// most arguments have no beta's
// most arguments have no lambdas

// ? arguments have no free variables


(λM)[Δ] N |-> M[N][Δ]

((λM)[↑] N)[K] |-> M[N][Δ]

((λM)[↑] N)[K]


x = K; (y => M) N
x = K; y = N; M

x = K; (y => M) N

(y => M) N


K; (λM)[↑] N
K; N; M[⇑↑]
K; N; (λM)


(λM) N |-> M[N]


(λ0 0[↑]) N
(0 0[↑])[N]
0[N] 0[↑][N]
N 0


(λ0[↑] 0) N

K; (λ1 0) N
K; N; 1 0
K; N; N[↑1] K[↑2]

K; (λ1 0) (λ1)
K; λ1; 1 0
K; λ1; (λ1)[↑1] K[↑2]

K; (λ0)[↑] (λ1)
K; λ1; 0[↑]

A; B; (λ0 1)[↑] N
A; B; N; (0 1)[↑]
K; N; (0)[↑]

(1 0)[N 1]
1[N 1] 0[N 1]

K; (λM)[↑] N | 1

K; (λ2 1)[↑] N

(λλ1 2) (λ1)
(λ(λ2) 1)

(λλ1 2) (λ1)

K; (λ1 1) N
K; N; 1 1
K; N; (λ0 1)[↑]


f. y => x => y f x

(f. y => x => y + f x) a b;

f. y => x => y + (x2 => x + f x2)


(f. y => x => y + f x) a b;

f = y => x => y + x;
y = a;
x = b;
y + f x;



x => 1 + 1 + x

(x => M) N |-> x = N; M // beta
(x => M)[Δ] N |-> M[x := N][Δ] // dB


(y = 1; x => x + y) 2
y = 1; x = 2; x + y


f = x => x + 1;
a = f 1;
b = f 2;
a + b;



f = x => x + 1;
a = (x => x + 1) 1;
b = (
  x = 2;
  x + 1
);
a + b;




(x => M) N |-> x = N; M
x = N; M |-> M{x := α N}

(x => x + 1) 1 == (x => x + 1) 2

(λ\1 + 1) 2

x = 1; y = 2 |- x + 1 == y + 1

x = 1 |- x + 1 == 2
x = 1 |- x + 1 == 2

Option Int -> Int

pow 2 32 == 2 * pow 2 16

Term =
  | M N
  | x | x => M
  | n | λM;

x => M

x => _A == y => y
_A[x := z] == y[y := z]
_A[x := z] == z
_A == z[z := x]


t
| A {eq} => _
| B {eq} => refute ()

(x => M) N |-> x = N; M // beta
x = N; M |-> M{x := N} // subst

x |- (x => M) N


Int64 : |{

}| = {
  T = 1;
}

get () : () -[Get]> String;

get () == get ()


(f. y => x => y + f x) a b;

f = y => x => y + f x;
y = a;
x = b;
y + (y => x => y + f x) x

f = y => x => y + f x;
y = a;
x = b;
y + (
  y = x;
  x2 => (
    x = x2;
    y + f x
  )
);

f = y => x => y + f x;
y = a;
x = b;
y + (
  y = x;
  x2 => (
    x = x2;
    y + (
      y = x;
      x3 => (
        x = x3;
        y + f x
      )
    )
  )
);

f = y => x => y + f x;
y = a;
x = b;
y + (
  y = x;
  x2 => (
    x = x2;
    y + (
      y = x;
      x3 => (
        x = x3;
        y + f x
      )
    )
  )
);

f = y => x => y + f x;
y = a;
x = b;
y + (
  y = x;
  x2 => (
    x = x2;
    y + (
      y = x;
      x3 => (
        x = x3;
        y + f x
      )
    )
  )
);


f = y => x => y + f x;
y = a;
x = b;
y + (
  y = x;
  x = x2;
  x => y + f x
) x

f = y => x => y + f x;
y = a;
x = b;
y + (
  y = x;
  x = x2;
  x2 => y + (
    y = x;
    x => y + f x
  ) x
) x




Bool = <A>(then : A, else : A) -> A;

not : Bool -> Bool = _;


0 | x => _A[1] == y => y

1 | _A[1] == y[y := z[1]]

_A[x = z] == z

_A[x = z] == z
_A := x
x = z; y = z; _A[x] == z
x = z; y = z; _A == z


beta : Term -> Term;
beta_reduces : size Term > size (beta Term);

(x => x) M > M

(x => y => x y) M > (y => M y)
(size M) * ω + 2 > size M + 1

(x => x x) M > M M


1 + 2 = 3
3 = 3  modulo evaluation



(x : A) -> B

B[x := N]


M : N : ... : S

M : A : K

Sort S =
  | Data
  | Prop;
Term (M, N) =
Type (A, B) =
  | S
  | x
  | (x : A) -> B
  | (x : A) => M
  | M N;

false = x => y => y;
true = x => y => x;

b |>
  | (eq : b == false) => 0
  | (eq : b == true) => 1
f = (b : Bool) =>
  | (eq : b == false) => 0
  | (eq : b == true) => 1;

f true == ((b : Bool) =>
  | <eq : b == false> => 0
  | <eq : b == true> => 1
) true;
f true == // beta
  | (eq : true == false) => 0
  | (eq : true == true) => 1;
f true == // refute
  | (eq : true == true) => 1;
f true == (
  | (eq : b == false) => 0
  | (eq : b == true) => 1
) true;


// Π = explicit / dynamic
// ∀ = implicit / static

Bool = ∀(A) -> Π(_ : A) -> Π(_ : A) -> A;
true : Bool = ∀A => Π(x) => Π(y) => x;
false : Bool = ∀A => Π(x) => Π(y) => y;

f : Π(b : Bool) -> (∀(_ : b () ⊥). Nat) | (∀(_ : b ⊥ ()). Nat)
  = Π(b) => (∀(_) => 0) | (∀(_) => 1);

f true == (Π(b) => (∀(_) => 0) | (∀(_) => 1)) true // expand
f true == (Π(b) => (∀(_ : b () ⊥) => 0) | (∀(_ : b ⊥ ()) => 1)) true // types
f true == (∀(_ : true () ⊥) => 0) | (∀(_ : b ⊥ ()) => 1) // beta
f true == (∀(_ : ()) => 0) | (∀(_ : ⊥) => 1) // beta
f true == ∀(_ : ()) => 0 // refute
f true == 0 // instantiate


f()
| <_ : b Unit Never> -> Nat
| <_ : b Never Unit> -> Nat
f true ==
  | (eq : true == false) => 0
  | (eq : true == true) => 1;

f true == (eq : true == true) => 1;

f true refl == 1

x = 1 | 2;


(f. y => x => y + f x) a b;

f = f. y => x => y + f x;
y = a;
x = b;
y + f x;

f = f. y => x => y + f x;
y = a;
x = b;
y + (f. y => x => y + f x) x;

3
x | 3 = a;
x | 4 = b;
y => y + x

(x = a; x => x)
(x = a; y = x; x => y x)

(x = a; y = x; (x => y x) b)

(x = a; y = x; x = b; y x)
(x => y x)

```

Simple types, no bound variables, rank-1, nominal constructors

- Goal:
  - O(n), over type size
  - single traverse
- Detection:
  - Tag it on every traverse
- Issues:
  Aliases seems common, but also annoying

Types with constructors, no bound variables, rank-1
Goal:

## Linear

```rust
Data : Kind;
Resource : Kind;
Prop : Kind;

Type : Type;

Kind = {
  Type : Type;
  Data : Type;
  Linear : Type;
}

```

## Check

```rust


Γ |- M ⇒ ((x : A) -> B)[Δ]  Γ |- N ⇐ A[Δ]
-----------------------------------------
Γ |- M N ⇒ B[x := N][Δ]


Type<l>


x = N; M : x = N; B;

left = String
right = (A : Type) = "a"; (x : A) = \"a\"; A\n"
(A : Type) = ("a" : String);
(x : A : Type) = ("a" : String);
(y : A : Type) -> (A : Type) : Type

M : A : S

x = M;
y = x;
x = N;
x + y

// naive
x2 = M;
y2 = x2;
x3 = N;
x3 + y2

// better
x = M;
y = x;
x2 = N;
x2 + y


Nat : Type;
Nat = (A : Type) -> (zero : A) -> (succ : (pred : Nat) -> A) -> A;

Nat == Nat

Γ |-
  (L : Type; L = (A : Type) -> (zero : A) -> (succ : (pred : Nat) -> A) -> A; L) ==
  (A : Type) -> (zero : A) -> (succ : (pred : Nat) -> A) -> A


Γ |- Nat == Nat


A : Type;
A = A;

B : Type;
B = B;

Γ, A == B |- A == B

Γ |- (A : Type; A = A; A) == (B : Type; B = B; B)
Γ |- (C = C; C) == (C = C; C)
Γ |- A == B

Fix : Type;
Fix = (f : Fix) -> ();

Γ |- Fix == (f : Fix) -> ();
[Γ] |- (L1 : Type; L1 = (f : L1) -> (); L1) == (f : Fix) -> ();
Γ |- (f : L1) -> () == (f : Fix) -> ();
Γ |- L1 == Fix;
[Γ] |-
  (L : Type; L = (f : L) -> (); L) ==
  (L : Type; L = (f : L) -> (); L);
[Γ] |-
  (L : Type; L = (f : L) -> (); L) ==
  (L : Type; L = (f : L) -> (); L);

Γ |- Fix == (f : (f : Fix) -> ()) -> ();
[Γ] |- (L1 : Type; L1 = (f : L1) -> (); L1) == (f : (f : Fix) -> ()) -> ();
Γ |- (f : L1) -> () == (f : (f : Fix) -> ()) -> ();
Γ |- L1 == (f : Fix) -> ();
[Γ] |- (L2 : Type; L2 = (f : L2) -> (); L2) == (f : Fix) -> ();
Γ |- (f : L2) -> () == (f : Fix) -> ();

A : Type;
A = (f : A) -> B;

B : Type;
B = (f : B) -> A;

Γ |- A == B
[Γ] |- (L : Type; L = (f : L) -> B; L) == (R : Type; R = (f : R) -> A; R)
Γ |- (f : T) -> B == (f : T) -> A

[Γ], A == B |- (f : A) -> B == (f : B) -> A

Γ |- Fix == Fix -> ()
  Γ |- Fix -> () == Fix -> ()
  Γ |- Fix == Fix

A : Type;
A = A;

B : Type;
B = B;

Γ |- A == B;
Γ, A == B |- A == B;
Γ, A == B |- A == Int;
Γ, A == B |- A == Int;




A : Type;
A = (f : A) -> ();

B : Type;
B = (f : B) -> ();

Γ |- A == (f : B) -> ()
[Γ, A == (f : B) -> ()] |- (f : A) -> () == (f : B) -> ()
Γ, A == (f : B) -> () |- A == B
Γ, A == (f : B) -> () |- (f : B) -> () == B
[Γ, A == (f : B) -> (), B == (f : B) -> ()] |- (f : B) -> () == (f : B) -> ()
Γ, A == (f : B) -> (), B == (f : B) -> () |- B == B

Γ |- (f : A) -> () == (f : B) -> ()
[Γ, A == B, B == A] |- (f : A) -> () == (f : B) -> ()
Γ, A == B, B == A |- A == B

Γ |- A == (f : B) -> ()
[Γ, A == (f : B) -> ()] |- (f : A) -> () == (f : B) -> ()
Γ, A == (f : B) -> () |- A == B
Γ |- (f : B) -> () == B


Γ, A |- A == B

Γ |- A == (f : (f : B) -> ()) -> ()
[Γ, A == (f : B) -> ()] |- (f : A) -> () == (f : (f : B) -> ()) -> ()
Γ, A == (f : B) -> () |- A == (f : B) -> ()
Γ, [A] |- (f : B) -> () == (f : B) -> ()
Γ, A == (f : B) -> (), |- (f : B) -> () == (f : B) -> ()



Γ, [x == N] |- M == y
---------------------
Γ, x == M |- x == y


Γ |- A == (f : (f : B) -> ()) -> ()



A = (f : A) -> ();
B = (f : B) -> ();

Γ; A == (f : A) -> (); B == (f : B) -> ()
  |- A == (f : B) -> ()

Γ; A == (f : A) -> (); B == (f : B) -> ()
  |- A == (f : B) -> ()

Γ; A == @fix(A). (f : A) -> (); B == @fix(B). (f : B) -> ()
  |- @fix(C). (f : C) -> () == (f : B) -> ()
Γ; A == @fix(A). (f : A) -> (); B == @fix(B). (f : B) -> ();
  ; C == @fix(C). (f : C) -> ()
  |- (f : C) -> () == (f : B) -> ()
Γ; A == @fix(A). (f : A) -> (); B == @fix(B). (f : B) -> ();
  ; C == @fix(C). (f : C) -> ()
  |- C == B
Γ; A == @fix(A). (f : A) -> (); B == @fix(B). (f : B) -> ();
  ; C == @fix(C). (f : C) -> ()
  |- @fix(D). (f : D) -> () == @fix(D). (f : D) -> ()

Γ; A == @fix(A). (f : A) -> (); B == @fix(B). (f : (f : B) -> ()) -> ();
  |- (f : C) -> () == (f : D) -> ()


-----------------
Γ |- (M : A) :> B

A = (f : A) -> ();
B = (f : B) -> ();

(x : (f : Fix) -> ()) :> (f : Fix) -> ()
((f : Fix) => f x : Fix) :> (f : Fix) -> ()
((f : Fix) => f x : Fix) :> (f : Fix) -> ()


(x : Fix) :> (f : Fix) -> ()

(x : (f : Fix) -> ()) :> (f : Fix) -> ()

(x : A, y : B) => x + 1

(a : (x : A, y : B)) -> T
f : (x : A, y : B) -> T

apply f p
f => (1, 2)

f(1, 2)


(K : Type) -> (k : K) -> T




f (1, 2);


(x = 1; x + 1 : Int)
(x : y : a = _; _)
(x : (y : a = _; _))
(x : y : (a = _; _))

(x : y : a = _; _)


x : A = 1;
(x : A) = 1;

(x : A, y : B);

f = (
  (x : Int, y : Int);
  print "a";
  1
);



(P; N)
(x = 1; x + 1)

incr = (x) => (
  x = x + 1;
  y = y + 1;
  x + y
);

incr = (x) =>
  (x = x + 1);
  (y = y + 1);
  x + y;

incr = (x) =>
  (x = x + 1);
  (y = y + 1);
  x + y;

and =
  | (a = false, b = false) => false
  | (a = false, b = true) => false
  | (a = true, b = false) => false
  | (a = true, b = true) => true;

and =
  | (:false, :false) => false
  | (:false, :true) => false
  | (:true, :false) => false
  | (:true, :true) => true;

Nat = {
};

:atom

incr = (x) => (
  x = x + 1;
  log x;
  x
);

incr = (x) =>
  (x = x + 1);
  (log x);
  x;

(map : )

f = (
  (x : Int);

  x = 1;

  x : Int;
  x = 1;

  x + 1
);

incr = ?(1 + _);
incr = x => 1 + x;

Bool = {
  @ = <A>(then : A, else : A) -> A;
};
Int32 = {};

User = {
  id : Nat;
  name : String;
};

Company = {
  id : Nat;
  name : String;
};

f = (x : User) => (x : Company);


f : (x : &mut i32) -> ()


let mut n = 1;
f(n);

print(n); // 2

f : (x : i32) -> (x : i32)


f = () => (
  x : Int;
  x + 1
);
f = () => (
  x : Int;
  x = 1 + x;

  log x;

  x + x
);


Γ |- M : A  Γ |- N : A
----------------------
Γ |- M | N : A


Γ |- A : Type α  Γ, x : A |- B : Type α  Γ |- α < β
---------------------------------------------------
Γ |- (x : A) -> B : Type β



b = (
  x = 1;
  log x;

);


incr = x : Nat; x + 1;

incr = x; x + 1;

id : A; x : A; A;
id = A; x : A; x;

id : (A : Type) -> (x : A) -> A;
id = (A : Type) => (x : A) => x;

add
  = x; y; x + y;

incr = x; x + 1;

incr(x) =
  y = x;
  x + 1;

map : <A, B>(f : (x : A) -> B, l : List A) -> List B;
map = (f, l) =>
  l |>
  | [] => []
  | hd :: tl => f(hd) :: map(f, tl);


f = x : Nat; x;

f (x : Nat) = (f x);

f = (x : Nat) => x;

id : (A; x : A; A);
id = (A; x; x);

id : \A. \x : A. A;
id = \A. \x. x;

id : \A. \x : A. A;
id = \A. \x. x;

id : \(A, x : A). A;
id = \(A, x). x;

id : \A, x : A. A;
id = \A, x. x;

incr = x => (
  x + 1
);

incr x = x + 1;

f (Cons x) = x + 1
f Nil = 0

x : Nat;
x = x + 1;
x

(P1 = E1; E2)
f =
  x : Nat;
  x = 1;
  x + 1;

Block (K) =
  | P : A; K
  | P = M; K
  | M

id : (A; x : A; A);
id = (A; x; x);

id : (A; x : A; A);
id = (A; x : A; x);


M |> | P => (N |> | Q => _ | X => _)
M |> | P => (N |> | Q => _) | X => _
M |> | (P => N |> | Q => _ | X => _)
M |> | (P => N |> | Q => _) | (X => _)


id = A => x => x;

add = (x, y) => x + y;

add = x => y => x + y;
add =
  x =>
  y =>
  x + y;

add =
  x : Nat;
  y : Nat;
  x + y;

add = x; y; x + y;

id : (A; x : A; A);
id = (A; x : A; x);

id = (A => x => x);

id = (A: x: x);
id = (A: x: x);

double = x \ x + x;
double = x => x + x;

Γ, x : A |- B : Type
------------------------------------------
Γ |- (x : A) => B : Type & (x : A) => Type

Γ, x : A |- M : B
--------------------------------
Γ |- (x : A) => M : (x : A) => B


id : (A : Type) => (x : A) => A;
id = (A : Type) => (x : A) => x;

f =
  (x : A);
  (x = 1);
  (log x);
  (x + 1);

(x \ x + 1)

m = (x + 1) (x : A);

((x : A) => x + 1) N

M (x = N)

f = (x, y) =>
  z = x + 1;
  z + y;


f = (x, y) => (
  (a : (n : Nat) -> Nat);
  (a = n => a(n));
  () = (
    log x;
    log y;
  );
  z = x + 1;
  z + y
);


true ~
| (b = true) => 1
| (b = false) => 0

f = (x, y) =>
  x =
    y = 1;
    y;
  x;

f = (x, y) => #imp (
  (a : (n : Nat) -> Nat);
  (a = n => a(n));

  z = x + 1;
  z + y
);

(x = 1; x + 1 : Int)

(
  eq : _A;
  x : Nat;
  (eq : x == x);
) == (
  y : Nat;
  eq : y == y;
  y = x + 1;
)

(
  map(f, l) =
  z = x + y;

  x = x + 1;

)

(Fix = )


type Bool(T, t : T, f : T, k : ~T) in
let true(T, t : T, f : T, k : ~T) = k(t);

let g(T : *, x : T, k : ~T) = k(x);
let f(b : Bool, k :) = b()



A = A -> ();
B = B -> ();


Γ |- A == B
Γ |- μA. A -> () -> μB. () == B


Γ |- (x : A) :> B



-----------------
Γ |- (M : x) :> x

Γ |- (x : A²) :> A¹  Γ |- (M : B¹) :> B²
------------------------------------------------
Γ |- (x => M : (x : A¹) -> B¹) :> (x : A²) -> B²

Γ |- P¹ : (x : A¹) -> B¹  Γ |- P² : (x : A²) -> B²

--------------------------------------------------
Γ |- (M : P¹ N¹) :> P² N²


Γ |- (x : A²) :> A¹  Γ |- (M : B¹) :> B²
------------------------------------------------
Γ |- (x => M : (x : A¹) -> B¹) :> (x : A²) -> B²

(x : Int) -> () :> (x : Nat) -> ()

A = A -> ();
B = B -> ();

Γ |- A == B
Γ |- μA. A -> () == μB. B -> ()

Γ |- A == B -> ()
Γ |- μA. A -> () == B -> ()
Γ |- A -> () == B -> ()
Γ |- A == B

Γ |- A == B
Γ |- C -> () == C -> ()

A = A -> ();
B = (B -> ()) -> ();

Γ |- A == B;
Γ |- A -> () == (B -> ()) -> ();
Γ |- A == B -> ();
Γ |- A -> () == B -> ();

Γ |- A == Int;
Γ |- A == Int;

A = A;

(x => x) (x => x)


A = A -> ();
B = B -> ();

Γ |- A == B -> ();
Γ |- A -> () == (B -> ()) -> ();

[Nat];
[0 := 1 + 2];

X = X -> ();
A = A -> ();

X == A
 == A -> ()


A : Type;
A = Type;

B = A;

A = A;
B = B;

x = A; y = A; |- x == y


(A : Type) -> A == (B : Type) -> B

A = C; B = C; |- A == B

A = B; B = A; |- A == B


A = A;
B = B;

A == B
A = C; B = C; A == B
A = C; B = C; A == B

A = A;
B = A -> ();

A = (x : A) -> ();
B = B -> ();

A -> () == B -> ();

A = (x : A) -> ()

A = (x : A) -> ();
B = (x : (x : B) -> ()) -> ();

A == B

(x : A) -> () == (x : (x : B) -> ()) -> ()
A == (x : B) -> ()

A == (x : A) -> ();


A = (Nat, A);
B = (Int, A);




A :> B

A :> B


A = A;
B = B;

T = T & U;
U = U & T;

(M : A) :> T;
(M : A) :> T & U;
  (M : A) :> T;
  (M : A) :> U;
    (M : A) :> U & T;
    (M : A) :> U;
    (M : A) :> T;

(M : T) :> A;
  (M : T & U) :> A;
    (M : T) :> A;
    (M : U) :> A;
      (M : U) :> A;
      (M : T) :> A;


T_False : Type;
T_False = (False : T_False) & (f : False) -> Type;

(False : (f : False) -> Type) :> T_False;
(False : (f : False) -> Type) :> (False : T_False) & (f : False) -> Type;
  (False : (f : False) -> Type) :> T_False;

Unit : Type;
unit : Unit;


T_unit : Type;
unit : T_unit;

T_unit = (P : (u : T_unit) -> Type) -> (x : P unit) -> P unit;
unit = (P : (u : T_unit) -> Type) => (x : P unit) => x

Unit : Type;
unit : Unit;

Unit = (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u;


(unit : T_unit) :> Unit

(unit : T_unit) :> (unit : T_unit) & T_unit;
(unit : (P : (u : T_unit) -> Type) -> (x : P unit) -> P unit) :>
  (u : T) & (P : (u : T) -> Type) -> (x : P unit) -> P u

(unit : T_unit) :> Unit

(unit : (unit : T_unit) & T_unit) :> Unit
(unit : (unit : T_unit) & T_unit) :> (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u


(unit : T) :> (u : T) & (P : (u : T) -> Type) -> (x : P unit) -> P u
(unit : T) :> (P : (u : T) -> Type) -> (x : P unit) -> P unit


(unit : (P : (u : Unit) -> Type) -> (x : P unit) -> P unit) :> Unit

(unit : (P : (u : Unit) -> Type) -> (x : P unit) -> P unit) :>
  (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u

(unit : Unit) :> (P : (u : Unit) -> Type) -> (x : P unit) -> P unit
(unit : (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u) :>
  (P : (u : Unit) -> Type) -> (x : P unit) -> P unit
(unit : Unit) :> (P : (u : Unit) -> Type) -> (x : P unit) -> P unit




Unit : Type;

T_unit : Type;
unit : T_unit;

T_unit = (u : T_unit) & (P : (u : T_unit) -> Type) -> (x : P unit) -> P u;
Unit = (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u;

(unit : (P : (u : Unit) -> Type) -> (x : P unit) -> P unit) :> T_unit
(unit : (P : (u : T) -> Type) -> (x : P unit) -> P unit) :>
  (P : (u : T) -> Type) -> (x : P unit) -> P unit
(unit : T) :> (P : (u : T) -> Type) -> (x : P unit) -> P unit

(unit : (P : (u : Unit) -> Type) -> (x : P unit) -> P unit) :>
  Unit


(unit : T_unit) :> Unit


Unit = (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u;
T_unit = (P : (u : Unit) -> Type) -> (x : P unit) -> P unit;


(unit : (P : (u : Unit) -> Type) -> (x : P unit) -> P unit) :>
  (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u

(unit : (P : (u : T) -> Type) -> (x : P unit) -> P unit) :>
  (u : T) & (P : (u : T) -> Type) -> (x : P unit) -> P u

(unit : (u : T) & (P : (u : T) -> Type) -> (x : P unit) -> P unit) :>
  (u : T) & (P : (u : T) -> Type) -> (x : P unit) -> P u

(unit : (P : (u : Unit) -> Type) -> (x : P unit) -> P unit)
  :> (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u

μT. T

Bool : Type;


Unit : Type;
unit : Unit;

Unit = (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u;
T_unit = (P : (u : T_unit) -> Type) -> (x : P unit) -> P unit;

(unit : (P : (u : Unit) -> Type) -> (x : P unit) -> P unit) :>
  (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u


(unit : T) :> (u : T) & (P : (u : T) -> Type) -> (x : P unit) -> P u

(unit : T) :> (u : T) & (P : (u : T) -> Type) -> (x : P unit) -> P u
(unit : T) :> (P : (u : T) -> Type) -> (x : P unit) -> P unit
(unit : (P : (u : T) -> Type) -> (x : P unit) -> P unit) :> Unit

(unit : (P : (u : Unit) -> Type) -> (x : P unit) -> P unit; T_unit) :>
  (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u;

(unit : (P : (u : Unit) -> Type) -> (x : P unit) -> P unit; T_unit) :>
  (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u;

(unit :
  (P : (u : Unit) -> Type) -> (x : P unit) -> P unit; T_unit) :>
  (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u;

(unit : (P : (u : T) -> Type) -> (x : P unit) -> P unit) :>;

(unit : (P : (u : Unit) -> Type) -> (x : P unit) -> P unit) :>
  (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u


Unit = (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u;

(unit : (P : (u : Unit) -> Type) -> (x : P unit) -> P unit) :>
  (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u

(unit : Unit) :>
  (P : (u : Unit) -> Type) -> (x : P unit) -> P unit

(unit : Unit) :> T_unit

(unit : (P : (u : Unit) -> Type) -> (x : P unit) -> P unit) :>
  (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u


Unit = unit => (P : (u : Unit unit) -> Type) -> (x : P unit) -> P u;

unit : Unit

False = (f : False) & (P : (f : False) -> Type) -> P f;


T_Unit : Type;
T_unit : (Unit : T_Unit) -> Type;

T_Unit =

Γ◃ |- A : Type  Γ, x : A |- B : Type
------------------------------------
Γ |- (x : A) & B : Type

Γ |-


Nat = (n : Nat) & ((A : Type) -> (z : A) -> (s : (x : Nat) -> A) -> A);

Unit = (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u;

Unit : Type;
Unit = (u : Unit) & Unit


(unit : T_unit) :> Unit

(unit : )

Γ◃ |- A : Type  Γ, x : A |- B : Type
------------------------------------
Γ |- (x : A) & B : Type

T : Type;
U : Type;

T = T & U;
U = U & T;

|- (x : Int) :> T
[(x : Int) :> T] |- (x : Int) :> T & U
  (x : Int) :> T |- (x : Int) :> T
  [(x : Int) :> T] |- (x : Int) :> U
  [(x : Int) :> T], [(x : Int) :> U] |- (x : Int) :> U & T
    (x : Int) :> T, (x : Int) :> U |- (x : Int) :> U
    [(x : Int) :> T], [(x : Int) :> U] |- (x : Int) :> T

(b : Bool) :> Unit

Unit : Type;
unit : Unit;
tmp : Unit;

Unit = (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u;
unit = tmp;
tmp = (P : (u : Unit) -> Type) => (x : P unit) => (x : P tmp);

(tmp : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P tmp
== (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u

Bool : Type;
true : Bool;
false : Bool;
true_b : Bool;
false_b : Bool;


Bool = (b : Bool) & (P : (b : Bool) -> Type) -> (x : P true) -> (y : P false) -> P b;
true = true_b;
false = false_b;
true_b = (P : (b : Bool) -> Type) => (x : P true) => (y : P false) => (x : P true_b);
false_b = (P : (b : Bool) -> Type) => (x : P true) => (y : P false) => (y : P false_b);

(true_b : Bool) & (P : (b : Bool) -> Type) -> (x : P true) -> (y : P false) -> P true_b
==

Unit = (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u;
unit = tmp;
tmp = (P : (u : Unit) -> Type) => (x : P unit) => (x : P tmp);



Unit : Type;
unit : Unit;
tmp : Unit;

Unit = (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u;
unit = tmp;
tmp = (P : (u : Unit) -> Type) => (x : P unit) => x;

(tmp : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P tmp
== (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u

  (tmp : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P tmp
== (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u

----------------------
Γ |- (x : A) & A |-> A

Γ, x : A |- N : B  Γ |- A == (x : A) & B
----------------------------------------
Γ, x : A |- x = N -| Γ, x == N : A


-------------------
(x : (A ! •)) ->

incr : (x : Int) -(IO)> Int;

t : Type ! IO = (P : (x : Int) -> Type) -> P (incr x)



A = (x : A) -> ();
B = (x : B) -> ();

A == B

L = A => (x : T) & A;
R = A => (x : T) & A;

L () == R ()


L<x>
L<x => M> |-> x => L<M>
L<x N>
L<(x => M) N> |-> L<M[x := N]>
L<(M K) N> |-> L<M K> L<N> ?

x ...N


L_A = (x : L_B) -> ();
L_B = (x : L_A) -> ();

R_A = (x : R_B) -> ();
R_B = (x : R_A) -> ();

L_A == R_A
(x : L_B) -> () == (x : R_B) -> ()
L_B == R_B
(x : L_A) -> () == (x : R_A) -> ()
L_A == R_A


A = (x : A) -> ();
B = (x : B) -> ();


|- A == B
A = K; B = K |- (x : A) -> () == (x : B) -> ()
A = K; B = K |- (x : K) -> () == (x : K) -> ()

A == (x : A) -> ()
A == A


L = A => (x : L A) & A;
R = A => (x : R A) & A;

L () == R ()
(A => (x : K A) & A) () == (A => (x : K A) & A) ()

id id
(x => x) id
x[x := id]

id\0 id\0
(x => x) id\0
x\1[x := id\0]


A = (x : A) -> ();
T = Nat;
A = T;

T == A
K == K

(x : A) ->  == B

U = (T, T);
T == U

L = A => (x : L A, y : A);
R = (x : R, y : ());

L () == R
(x : R, y : ()) == R
(x : L (), y : ()) == R // optional


A = (x : A) -> ();
B = (x : B) -> ();


(x : L ()) & () == (x : R) & ();
L () == R


L () == R

L = x => (K, x);
R = (K, ()); |-

(A => (x : L A, y : A)) () == (x : R, y : ())
L () == R
(K, ()) == (K, ())

(L ())[L := x => (K, x)] == R
(x => (K, x)) == (K, ())


L = A => (x : L A, y : L Nat);
R = A => B => (x : R A B, y : R A B);

L () == R () Nat // true?

L = x => (K, x, Nat);
R = x => y => (K, x, y); |-

(x : L (), y : L Nat) == (x : R () Nat, y : R () Nat)
L Nat == R () Nat

L () == R () Nat
L Nat == R () Nat


L = A => (x : L A, y : L Nat);
R = A => B => (x : R A B, y : R A B);

H = A => (x : H A, y : R A Nat);

L () == H ()
(x : L (), y : L Nat) == (x : H (), y : R () Nat)
L Nat == R () Nat


(x : L Nat, y : L Nat) == (x : R () Nat, y : R () Nat)

(x : L (), y : L Nat) == (x : H (), y : R () Nat)

H () == R () Nat
(x : H Nat, y : R A Nat) == (x : R () Nat, y : R () Nat)
R () Nat == (x : R () Nat, y : R () Nat)


L Nat == R () Nat

L = A => (x : L A, y : L Nat);
R = A => B => (x : R A B, y : R A B); // L == R?



L = A => (x : L A, y : L Nat);
R = A => B => (x : R A B, y : R A B);

L () == R () ()
L Nat == R () ()
H = A => (x : L A, y : H Nat);

L () == H Nat
(x : L (), y : L Nat) == (x : L (), y : H Nat)


L = x => (K, x);
H = x => (K, x); |-

L Nat == R () Nat


L = b =>
  b
  | true => (x : Nat, y : L false)
  | false => (x : String, y : L true);
R = b =>
  b
  | true => (x : String, y : R false)
  | false => (x : Nat, y : R true);

L true == (x : Nat, y : L false)
R false == (x : Nat, y : R true)

l_t_eq_r_f : L true == R false;
l_

H = b =>
  b
  | true => (x : Nat, y : H false)
  | false => (x : String, y : L true);

L = b => b | true => L false | false => L true;
R = b => b | true => R true | false => R false;

L true == L false

L true == R false

LL = b => b | true => LL false | false => L true;
LR = b => b | true => L false | false => LR true;


|- LL true == LR false
|- LL false == LR true


L = ;
|- L false == R true


|- L true == R false

L = x => (K, x) | L x;

(K, true) | L true == (K, false) | R false


L = b => b | true => L false | false => L true;
R = b => b | true => R true | false => R false;


|- L true == R false
|- L false == R true
|- L true == R false

|- L false[L true := K] == R true[R false := K]
|- L true[L true := K] == R false[R false := K]
|- K == K

L = b => K | L b;
R = b => K | R b;
|- L false <: R true


received : K & L false
expected : K | R true
K & L false <: K | R true

|- L false <: R true
L false ==


f : <A>(x : Int & A) -> A = _;

received : Nat | _B
expected : Int & _A

Nat

Γ |- A <: C  Γ |- B <: C
------------------------
Γ |- A | B <: C



f : (x : Int) -> Effect<R, E, Ctx>

f : (x : Int) -[Error]> R;
g : (x : Int) -[Error]> R;

let%bind x = f();
let%bind y = g();
x + y


L = @fix(L). b => b | true => L false | false => L true;
R = @fix(R). b => b | true => R true | false => R false;


L true == R false


L true == L false
L false == L true

A = A;
B = B;

μA. A == Nat // False

A = (x : A) -> ();
B = (x : (x : B) -> ()) -> ();

A == B
A == (x : B) -> ()
A == B

A == Nat
Unit : Type;
Unit = [α] -> (u : Unit [α]) &
  (P : (u : Unit [α]) -> Type) -> (x : P (unit [α])) -> P u;


False : Type;
False = [α]. (f : False [α]) & (P : (f : False [α]) -> Type) -> P f;

Unit : Type;
Unit = [α] -> (u : Unit [α]) &
  (P : (u : Unit [α]) -> Type) -> (x : P (unit [α])) -> P u;


A = () -> () -> A;
B = () -> () -> () -> B;

A == B
A == () -> B
() -> A == B
A == () -> () -> B
A == B

x => x : Prop -> Prop : Type

M : A : S

((M : A) : B)

(x => x : <A>(x : A) -> A)

Unit : Type;
unit : Unit;

Unit = (u : Unit) & (P : (x : Unit) -> Type) -> (x : P unit) -> P u;


(unit : (P : (x : Unit) -> Type) -> (x : P unit) -> P unit) :> Unit;

μl. Type l : Type

U : Type 1 = (A : Type 0, (A = A, A -> ()) -> ());

(A = U, A -> ()) <: U;

(A = U, A -> ()) <: (A : Type, (A = A, A -> ()) -> ());
U -> () <: (A = U, A -> ()) -> ();
(A = U, A -> ()) <: U;

U : Type = (A : Type, (A = A, A -> ()) -> ());
V = (A = U, A -> ());

((T : Data = (x : Nat) -> Nat; T) : Type)


U : Type 1 = (A : Type 0, (A = A, A -> ()) -> ());

Type (1 + _A) == Type (0 + _B)
(1 + _A) == (0 + _B)
(1 + _A) == _B

(A = U[⇑ _A], A -> ()) <: [⇑ _B]U
(A = U[⇑ _A], A -> ()) <: (A : Type (0 + _B), (A = A, A -> ()) -> ())
_B := 1 + _A
U[⇑ _A] -> () <: (A = U[⇑ _A], A -> ()) -> ()
(A = U[⇑ _A], A -> ()) <: U[⇑ _A]
(A = U[⇑ _A], A -> ()) <: (A : Type (1 + _A), (A = A, A -> ()) -> ())


A = () -> A;
B = () -> () -> B;

A == B
() -> A == () -> () -> B
() -> A == () -> () -> B
A == () -> B
A == B



map : _;
map = (f, l) =>
  (l) |>
  | [] => []
  | hd :: tl => f(hd) :: map(f, tl);


L = b => b | true => L false | false => L true;
R = b => b | true => R true | false => R false;

L true == R false

Unit : Type;
unit : Unit;

Unit = (u : Unit & (P : (u : Unit) -> Data) -> (x : P unit) -> P u);
unit =

(P : (u : Unit) -> Data) -> (x : P unit) -> P unit :>
  (u : Unit & (P : (u : Unit) -> Data) -> (x : P unit) -> P u)

(unit : (P : (u : Unit) -> Data) -> (x : P unit) -> P unit) :>
  (P : (u : Unit) -> Data) -> (x : P unit) -> P unit





A = x => x;
B = y => y;

----------------
x => x == y => y

Fix\0 : Type;
Fix\0 = Fix\0 -> ();
(Fix\1 -> ())[Fix\0 := Fix\1];

λ(x : Nat) => (
  y = 1 + x;
  [x := y];
  1 - y
)

#hole


----------------------------------
k = 1 |- (x => _A) == (y => y + k)

---------------- // univ
Γ |- Type : Type

Γ |- A : Type  Γ, x : A |- B : Type
----------------------------------- // forall
Γ |- (x : A) -> B : Type

Γ |- (x : A) -> B : Type  Γ, x : A |- M : B
------------------------------------------- // lambda
Γ |- (x : A) => M : (x : A) -> B

Γ |- M : (x : A) -> B  Γ |- N : A
--------------------------------- // apply
Γ |- M N : B[x := N]

Γ, x : A |- M : B
-------------------------- // subst
Γ |- M[x := N] : B{x := N}

Γ |- M : A
----------------- // coerce
Γ |- M % W : W(A)

Γ |- β < α
-------------------- // univ
Γ |- Type β : Type α

Γ |- A : Type α  Γ, x : A |- B : Type α
--------------------------------------- // forall
Γ |- (x : A) -> B : Type α



Term (M, N)
Type (A, B) := Type | (x : A) -> B | (x : A) => M | M N;
Path (W) := []

Size [α] =
  | Succ : [β < α] -> (pred : Size [β]) -> Size [α];


map : [α]<A, B>(f : (x : A) -> B, l : List [α] A) -> List [_] B;
fold = [α] => (f, l) =>
  (l) |>
  | [] => []
  | [β] (hd :: tl) =>
    f(hd) :: map[β](f, tl);


<A>(
  f : (x : A) -[Fix]> A,
  measure : (x : A) -> Size,
  termination : (x : A) -> measure (f x) < measure x
) -> (x : A) -> A;

Subject Reduction + Strong Normalization = Logical Consistency

((1 : Int) : String)
(x => x x) (x => x x)

A = (x : A) -> ();

A == (x : A) -> ();

Γ
-----------------------------------------
Γ |- x => f _A[x] _A[x] == y => f y _A[y]

---------------------------------------------
Γ | 5 |- x => f _A[6] _A[6] == y => f y _A[6]


Γ, _A[x] := x |-

_A[y] == _A[x][x := y]

T = a => f _A[a];
Γ |- x => f _A[x, y] == y => f y

_A[]
_A := z

x => f z

Γ |- M == N
-----------------
Γ |- λ. M == λ. N

Γ | +3 |- λ. f _A\+4 (λ. _A\+4) == λ. f \-1

(λ. \-1) M

_A := \-1

_A := x[close x]

id = λ. x[close x];

id id |->



Γ |- λ. f _A (λ. _A) == λ. f y

T = a => f _A[a];
Γ |- x => f _A[x] == y => f y

_A[x] := z[x]

names + indices
levels + indices

x == y

Γ | +3 |- λ. f _A\+4 == λ. f \-1

Γ | +4 |- f _A\+4 == f \-1
Γ | +4 |- (_A\+4)L == (\-2)R

Γ | +4 |- (_A\+4)[N] == (\-2)[K]

(_A\+4)L == (\-2)R

(_A\+4)[N] == (\-2)[K]
(_A\+4)[N] == (\-2)[K][↑]

Term (M, N) ::= x | \-n | λ. M | M N | M[N];


--------------------- // beta
((λ. M) N)L |-> M[N]L

-------------------- // free var
l | (\+n)L == (\+n)R


1 + l | (M)L == N[\+l]R
-------------------------- // lambda
l | (λ. M)L == (λ. N)R

l | (M)L == (M)R  l | (N)L == (N)R
---------------------------------- // apply
l | (M N)L == (M N)R

-------------------- // short-bound var
l | (\-n)L == (\-n)L


------------------
(x => M) == y => N

(λ. _A\+4)

0 | λ. -2 -1
1 | -2 -1

_A := (M)L == _A := normal (M)L

_A := (M)L

rename (x => M) |-> z => M2
rename (y => N) |-> y => N2


| term | heap      | env |
| x    | x :: heap | | -

(M)⇑L == (N)⇑R
------------------
(λ. M)L == (λ. N)R

(\1)⇑[K] |-> 1
(\2)⇑[K] |-> K

(M)⇑L == (N)⇑R
------------------
(λ. lM)L == (λ. N)R

(\1)L == \2[]


x = K; x


λ. M
λ. M[close x]

C<x>[close x] |-> C<\#>[close x]

```

| Context | Stack  | R    | E    |     | Stack |
| ------- | ------ | ---- | ---- | --- | ----- |
| Γ       | T :: S | x[Δ] | x[Φ] |     |       |

## Import Beer

```rust


Term ::=
  | x /* Var */
  | x => M /* Function */
  | M(N) /* Apply */
  | do { const x = N; M; } /* Let Block */;

do { const x = M; } == do { const x = M; undefined }
do { M; N } == do { const x = M; N }

(x => M)(N) |-> do { const x = N; M; } // beta
do { const x = N; C<x> } |-> do { const x = N; C<N> } // subst
do { const x = N; M }  x ∉ M |-> M // gc

do {
  const double = x => x + x;
  double(1 + 2)
};
// subst, then
do {
  const double = x => x + x;
  (x => x + x)(1 + 2)
};
// gc, then
(x => x + x)(1 + 2)


(x => x + x)(1 + 2)
  // beta, right
  (x => x + x)(3)

  // beta, left
  do { const x = 1 + 2; x + x }
    // subst, left
    do { const x = 1 + 2; x + x }

(x => x + x)(1 + 2)
do { const x = 1 + 2; x + x } // beta, left
  do { const x = 3; x + x } // beta, left
  do { const x = 3; 3 + x } // subst, left
  do { const x = 3; 3 + 3 } // subst, right
  do { const x = 3; 6 } // beta
  6 // gc

  do { const x = 1 + 2; (1 + 2) + x } // subst, left
  do { const x = 1 + 2; (1 + 2) + (1 + 2) } // subst, right
  (1 + 2) + (1 + 2) // gc

(do {
  console.log("a");
  x => do {
    console.log("b");
    y => do { console.log("c") };
  };
})(do { console.log("d"); })(do { console.log("e"); })


(do {
  const a = console.log("a");
  x => do {
    const b = console.log("b");
    y => do {
      const c = console.log("c");
      undefined
    };
  };
})(do {
  const d = console.log("d");
  undefined
})(do {
  const e = console.log("e");
  undefined
})

(x => do {
  const b = console.log("b");
  y => do {
    const c = console.log("c");
    undefined
  };
})(do {
  const d = console.log("d");
  undefined
})(do {
  const e = console.log("e");
  undefined
}) // beta, gc, "a"

(x => do {
  const b = console.log("b");
  y => do {
    const c = console.log("c");
    undefined
  };
})(undefined)(do {
  const e = console.log("e");
  undefined
}) // beta, gc, "d"

(do {
  const x = undefined;
  const b = console.log("b");
  y => do {
    const c = console.log("c");
    undefined
  };
})(do {
  const e = console.log("e");
  undefined
}) // beta

(do {
  const b = console.log("b");
  y => do {
    const c = console.log("c");
    undefined
  };
})(do {
  const e = console.log("e");
  undefined
}) // beta, gc

(do {
  const x = undefined;
  const b = console.log("b");
  y => { console.log("c"); };
})(do {
  const e = console.log("e");
  undefined
}) // beta

(do {
  const b = console.log("b");
  y => { console.log("c"); };
})(do {
  const e = console.log("e");
  undefined
}) // gc

(y => { console.log("c"); })(do {
  const e = console.log("e");
  undefined
}) // beta, gc, "b"

(y => { console.log("c"); })(
  undefined
) // beta, gc, "e"

do {
  const y = undefined;
  console.log("c");
} // beta

console.log("c") // gc

undefined // beta, "c"



(x => do {
  const b = console.log("b");
  y => { console.log("c"); };
})(do {
  const d = console.log("d");
  undefined
})(do {
  const e = console.log("e");
  undefined
})





// https://github.com/tc39/proposal-do-expressions

do { const x = N; M }

// same as
(() => { const x = N; return M; })()


((() => {
  console.log("a");
  return x => {
    console.log("b");
    return y => { console.log("c"); };
  };
})())(
  (() => { console.log("d"); })()
)(
  (() => { console.log("e"); })()
)


(do {
  console.log("a");
  x => do {
    console.log("b");
    y => { console.log("c"); };
  };
})(
  do { console.log("d"); }
)(
  do { console.log("e"); }
)

const f = x => do {
  console.log("a");
  y => do { console.log("b"); };
};
f(do { console.log("c"); })(do { console.log("d"); })






(() => {
  console.log("a");
  return x => {
    console.log("b");
    return y => {
      console.log("c");
      return x(y());
    };
  }
})((() => {
  console.log("d");
  return a => {
    console.log("e");
    return a();
  }
})())((() => {
  console.log("f");
  return () => {
    console.log("g");
    return () => {
      console.log("h");
    };
  };
})());

(x => do {
  console.log("a");
  y => do {
    console.log("b");
    x(y());
  };
})(do {
  console.log("c");
  a => do {
    console.log("d");
    a();
  }
})(do {
  console.log("e");
  () => do {
    console.log("f");
    () => do {
      console.log("g");
    };
  };
});

(y => {
    console.log("b");;
  })((() => {
  console.log("d");
})())

const f = x => do {
  const y = 2;
  if (x > 5) { x + 1 }
  else { x + y }
};


```

## Univ

```rust
Data : Type
Prop : Type

Id : Data = (A : Data) -> Data;


fix : (wrap : (self : Fueled T) -> Fueled T) -> Fueled T;

x = •;
id = x => x;
id id

x = •;
id = x => x;
(x => x) id

x = id;
id = x => x;
x

x = id;
id = x => x;
id

id id


fix : (wrap : (self : A) -[Fix]> A) -[Fix]> A;
fix : (wrap : (self : Fueled T) -> Fueled T) -> Fueled T;

fix : (wrap : (fuel : Fuel) -> (self : Fuel_data T fuel) -> Fuel_data T fuel) ->
  Fueled T;


fix (self => )


App = <A, K>(x : A, f : (y : A) -> K y) -> K x;



(I I) (I I)
I I
I


Γ |- A : Type  Γ, x : A |- B : Type
----------------------------------- // Self
Γ |- (x : A) @-> B


(x => y => y) ((x => x) (x => x))
(x => y => y) (x => x)
(y => y)

(x => M) N |-> M[x := N]

(x => x + x) 2
(x + x)[x := 2]
(2 + x)[x := 2]
(2 + 2)[x := 2]
2 + 2

((x $ 2) => x + x)

x = A;
y = B;
z = C;
x + y

M N |->

f (g 1) (g 2)

(f _A)[x := z] == (f y)[y := z]
-------------------------------
x => f _A == y => f y

(f _A)[x := z] == (f y)[y := z]
-------------------------------
x => f _A == y => f y


(_A[N])L == (\-1)R
----------------------
(λ._A[N])L == (λ.\-1)R

(_A[N])L == (\-1)R
----------------------
(λ._A[N])L == (λ.\-1)R

------------
_A[N] == \-1

(λ._A[N])L == (λ.\-1)R

(_A[N][x])L == (\-1[x])R
------------------------
(λ._A[N])L == (λ.\-1)R

_A[N][x] == \-1[x]


_A[N][x] == x

_A := x


λ. _A[N] == λ. \+1
_A := \+1

0 |- λ. _A[N] == λ. \-1

x := 1 |- _A[N] == \-1

x := 1 |- _A[N][x] == \-1[x]
x := 1 |- _A[N][x] == x
x := 1 |- _A == x

0 |- (λ. _A)[N] == λ. \+1

1 | λ. _A == 0 | λ. \+1
2 | _A == 1 | \+1

λ. _A[N] == λ. \+1

(_A _A)[x := z] == (y _A)[y := z]
---------------------------------
x => _A _A == y => y _A

_A := y[y := z]
_A := x[x := z]


(λ. _A)[N] == λ. \+1

(λ. _A)[N] == λ. \+1
λ. \-1[N] == λ. \-1

λ. _A[N] == λ. \-1
_A[N] == \+1

λ. _A\+1[N] _A == λ. \1 _A
```

## Unify

```rust
_A\+1[N] _A == \+1 _A

(λ. _A\+2 _A)[N] == λ. \+1 _A

0 | (λ. _A\+2 _A)[N] == λ. \-1 _A


x => _A _A == y => y _A


(_A[x] _A[x])[x := z] == (y _A[y])[y := z]
------------------------------------------
x => _A[x] _A[x] == y => y _A[y]

(_A[x] _A[x])[x := z] == (y _A[y])[y := z]
------------------------------------------
x => _A[x] _A[x] == y => y _A[y]

_A[x][x := z] == y[y := z]

_A[x] == x

_A[z] == z
_A[z]
_A[x] == _A[z][z := x]


(_A _A)[x := z] == (y _A)[y := z]
---------------------------------
x => _A _A == y => y _A

_A[x := z] == y[y := z]
_A == y[y := z][z := x]
_A == x
_A == \-1

(_A[N] _A) == \-1 _A
---------------------------------
λ. _A[N] _A == λ. \-1 _A

_A[N] == \-1
_A == \-2

_A[a := N] == x

(_A _A)[x := z] == (y _A)[y := z]
---------------------------------
x => _A _A == y => y _A

M[]

(λ. )[\-1][N]

(_A[N] _A) == \-1 _A
------------------------
λ. _A[N] _A == λ. \-1 _A

x[↑] | args | K :: substs |-> x | [K] :: args | substs

(x[↑] ...Arg) |->

(λ. M)[K] N |-> M[N[↑]][K]
((λ. M)[↑] N)[K] |-> MN[K]]

M[\-1][N]

M[\-1][N][K]
M[\+1][N][K]

(λ.\-2)[\-1][N][K]
(λ.\-2)[\+2][N][K]
(λ.\+2)[N][K]
\-2
main = () => print "Hello World";


M[K] N |-> (M N[↑])[K]
(M[↑] N)[K] |-> M N[K]


(λ. λ. M) N K
(λ. M)[N] K
(λ. M)[N] K
M[N][K]

(λ. M)[N]K

(λ. M)[L] N |-> M[N[↑L]][L]
(λ. \-2)[N] K

M[K] N |-> (M N[↑])[K]
(M[↑] N)[K] |-> M N[K]


M[K] N N2 |-> (M N[↑] N2[↑])[K]

M[K] N N2 |-> (M N[↑])[K] N2

M[A] N K |->

M[]

(λ. λ. λ. M) A B C
(λ. λ. M)[A] B C
(λ. M)[B[↑]][A] C
M[C[↑][↑]][B[↑]][A]

((λ. λ. M) B[↑] C[↑])[A]

((λ. M) C[↑][↑])[B[↑]][A]
M[C[↑][↑]][B[↑]][A]

(λ. λ. λ. M) A B C
((λ. λ. M)@[↑] B C)[A]
((λ. M)@[↑][↑] C)[B[↑]][A]
M[C[↑][↑]][B[↑]][A]

(λ. λ. λ. M) A B C
((λ. λ. M)@[↑] B C)[A]
((λ. M)@[↑][↑] C)[B[↑]][A]
M[C[↑][↑]][B[↑]][A]

((λ. λ. λ. M)[↑][↑] A B C)[K][N]
(M[↑]@[K] A B C)[N]
(M@[K][N] A B C)

(λ. λ. λ. M) A B C

(λ. M)[N] K
(λ. M)[K[↑]][N]


(λ. λ. λ. M) A B C
(λ. λ. M)[A] B C

(λ. M)[...L] A |-> M[A][...L]


(M[A] N)
(M N[↑])[A]

(M[↑] N)

(λ. M) N ...R |-> (M ...R[↑])[N]

M[close 5][close 2]

l | (λ. M)[...L] A |-> M[A | l][...L]

((λ. M)[B] A)[C]
((λ. M)[B] A)[C]

(λ. \-2[K] \-1) I
(\-2[K] \-1)[I]
\-1


id
  : <A>(x : A) -> A
  = <A>(x : A) => x;

x : String = "A";

f = (b : Bool, x : b |> | true => Int | String) =>
  b |>
  | true => Int.to_string x
  | false => x;


((λ. λ. λ. M)[↑][↑] A B C)[K][N]
((λ. λ. λ. M)[↑]@[K] A B C)[N]
((λ. λ. λ. M)@[N][K] A B C)
((λ. λ. M)@[N][K] B C)[A[K][N]]
((λ. M)@[N][K] C)[B[K][N]][A[K][N]]
M[C[K][N]][B[K][N]][A[K][N]]

((λ. λ. \-2 \-1)[↑][↑] I V)[K][N]
((λ. \-1 (λ. \-1))[↑][↑] I V)[K][N]
(λ. λ. \-2 \-1)@[N][K] I V
((λ. \-2 \-1)@[↑][N][K] V)[I[K][N]]
(λ. λ. \-2 \-1)[V[K][N][↑]][I[K][N]]

((λ. \-1 (λ. \-1))[↑][↑] I V)[K][N]
((λ. \-1 (λ. \-1))[↑][↑] I V)[K][N]

(((λ. λ. M)[↑] I)[↑] V)[K][N]
(((λ. λ. M)[↑] I)@[K] V)[N]
(((λ. λ. M)@[N] I)@[K] V)
((λ. M)@[↑][N]@[K] V)[I[N]]
M[V[K][N][↑]][I[N]]

((λ. M)@[↑][K] V)[I[N]]
M[V[↑][K]][I[N]]

(x => y => x + y) 1
x = 1; y => x + y

l | λ. λ. \-1 \-2 \+1+l

l | 0 | λ. λ. \-1 \+1+l \-3
l | 2 | \-1 \+1+l \-3
l | 2 | \(-1 + 2 + l) \+1+l \-3

l | 2 | \-1 \-2 \(-3 + 1 + 2 + l)

l | 0 | λ. λ. \+l \+(1 + l) \+(2 + l)
l | 2 | \+l \+(1 + l) \+(2 + l)

l | 2 | \-1 \-2 \+(1 + l)

l | 2 | \+0 \+1 \+2
l | 2 | \+0 \+1 \-l

l | 0 | λ. λ. \+0 \+1 \+2
l | 2 | \+0 \+1 \+2
l | 2 | \+0 \+1 \-l

l | 0 | λ. λ. \+1+l \+2+l
l | 2 | \+1+l \+2+l
l | 2 | \-1 \+2+l

l | 0 | λ. \+1+l
l | 1 | \+1+l
0 | 0 | \+0


l | 0 | λ. λ. \-1 \+1+l \-3
l | 2 | \-1 \+1+l \-3
l | 2 | \-1 \-(1+l+1-2-l) \+(3-1-2+l)

l | 0 | λ. λ. \-1 \+1+l \-3
l | 2 | \-1 \+1+l \-3
l | 2 | \-1 \-2 \+l

f(l, 2, -1) = -1
f(l, 2, +1+l) = -2
f(l, 2, -3) = +l

f(l, d, i) = (d > i)

l | 2 | \-0 \+1+l \-2
l | 2 | \-0 \-1 \-2

l | 2 | \-1 \+1+l \-3

A == B, under Open CBV -> A == B, Strong CBN

(M : A)

1 | +2[N] |-> N

M[...L]

(y = K; x => M) N
y = K; x = N; M

f (() => a) b c

1024 * 8

x : False;
x = x;

Either = (A, B) =>
  | (tag : "left", payload : A)
  | (tag : "right", payload : B);

succ = n => z => s => s (n z s);
four = z => s => s (s (s (s z)));

s = succ;
z = zero;
n = succ (succ (succ (succ z)));

------------
M ⇐ x = A; B

f = (x, y) => ((1 + x, 1 + y) : _ : Linear Data);

x => x == y => _A[y]

M _H == (M N)[A][B][C]

_H == N[A][B][C]

Γ |- L<M> == R<M>

\-1[C][\-0][A]

0 | (\2 \0)[D | 4][C | 3][λ. \2 \0 | 2][B | 1][A | 0]
0 | \2@[\0 | 5][D | 4][C | 3][λ. \2 \0 | 2][B | 1][A | 0]
3 | (λ. \2 \0)@[\0 | 5][D | 4][C | 3][λ. \2 \0 | 2][B | 1][A | 0]
3 | (\2 \0)[\0 | 5][D | 4][C | 3][λ. \2 \0 | 2][B | 1][A | 0]
3 | \2@[\0 | 6][\0 | 5][D | 4][C | 3][λ. \2 \0 | 2][B | 1][A | 0]

0 | (\2 \1)[C | 3][λ. \2 \1 | 1][A | 0]
0 | \2@[\1 | 3][C | 3][λ. \2 \1 | 1][A | 0]
2 | (λ. \2 \1)@[\1 | 3][C | 3][λ. \2 \1 | 1][A | 0]
2 | (\2 \1)[\1 | 3][C | 3][λ. \2 \1 | 1][A | 0]
2 | \2@[\1 | ?][\1 | 3][C | 3][λ. \2 \1 | 1][A | 0]
2 | A@[\1 | ?]

0 | (\2 \1)[C | 3][λ. \2 \1 | 1][A | 0]

| (\3 \0 \1)[A][B][_][λ. λ. \2 \1 \0][C]
| \3@[\0]@[\1][A][B][_][λ. λ. \2 \1 \0][C]
3 + 0 | (λ. λ. \2 \1 \0)@[\0]@[\1][A][B][_][λ. λ. \2 \1 \0][C]
3 + 1 | (λ. \2 \1 \0)@[\1][\0][A][B][_][λ. λ. \2 \1 \0][C]
3 + 2 | (\2 \1 \0)[\1][\0][A][B][_][λ. λ. \2 \1 \0][C]



\3@[\0][A][_][_][λ. \2 \0][_][B]


call = (id : <A>(x : A) -> A) => id;
id = <A>(x : A) => x;

id = (A : Data : Type) => (x : A : Data) => x;
id = <A>(x : A) => x;

make : <A>(len : Int & len >= 0, initial : A) -> _ = _;

Type : Type;
Data : Type;
Prop : Type;
Resource : Type;

f = (n : Nat & n > 0) => _;

Either = (A, B) =>
  | (tag == true, payload : A)
  | (tag == false, payload : B);

Either = (A, B) =>
  (tag : Bool, payload : tag | true => A | false => B);

Nullable = (A : Type & A != Null) => A | Null;

Id = (A : Type) => A;
Both = A => (x : A, y : A);


(n > 0) |>
| true => f(n)
| false => _;

id = <A>(x : A) => x;
(x : Local Data) =>

<A : Type, B : Type>(x : A == B, y : A == B) =>
  (refl : x == y)

<A : Prop>(x : A, y : A) => (refl : x == y);

Nat = (x : Int, x == 1);

(a : Nat, b : Nat) => _;
a : Nat = (x = 1, w : 1 == 1 = _);
b : Nat = (x = 1, w : 1 == 1 = _);

Socket : Resource & {

} = _;

Either = (A, B) =>
  | (tag : "left", payload : A)
  | (tag : "right", payload : B);
dup = (x : Socket) => x;

id = (A : Size (16, Erasable (Linear Type))) => (x : A) => x;


UserA = {
  id : Nat;
  name : String;
};

UserA = {
  id : Nat;
  name : String;
};

UserB = {
  name : String;
  id : Nat;
};

((user : UserA) : UserB)

(x : A) = M;

refl : <A>(x : A) -> x == x = _;

incr = x => x + 1;
incr_four_is_five = #[test (incr 4 == 5)];

map : _;
map = (f, l) =>
  l
  | [] => []
  | hd :: tl => f(hd) :: map(f, tl);

Y : <A>(x : (self : A) -> A) -> A;

wasm_f = [#wasm f];
worker_f = [#worker [#wasm f]];

m : <A>() -[Read]> ();



zone : <A, L>(k : () -[Life L]> A) -> A = _;

NotInitializedArray : {
  length : <A>(arr : Array<A>) -> Nat = _;
  make : <A>(len : Nat) -> (arr : Array<A>, w : length arr = len);

  set : <A>(arr : Array<A>, index : Nat, value : A) ->
    (arr : Array<A>, written : Written arr index);


  cast : (witness :
    (arr : NotInitializedArray<A>, i : Nat < length arr) -> Written arr i) -> Array<A>;
  get : <A>(
    arr : &Array<A>,
    index : Nat,
    written : Written arr index,
  ) -> A;
} = _;

user : User = _;
f : (user : User) -> () = _;

f = (mem : Memory, witness : Memory.type mem 0 == String) =>
  (Memory.get mem 0 witness : String);

Array : {
  length : <A>(arr : Array<A>) -> Nat = _;
  make : <A>(len : Nat, initial : A) ->
    (arr : Array<A>, w : length arr = len);

  get : <A>(
    arr : &Array<A>,
    index : Nat,
    witness : index < length arr
  ) -> A;

} = _;

strcpy : <S, D>(src : &<S>CharPtr, dst : CharPtr<D>) ->
  (dst : CharPtr<D>) = _;


M : {
  id
} : STLC = {

};
x = M.x;
(x : A : Type) => _

W : Type;
S : (f : W) -> Type;

W = (f : W) & (self : S f) -> (b : Fuel) -> _;
S = (f : W) => (a : Fuel) -> (lt : a < b) -> _;

S : Type;
W : (a : Fuel) -> Type;

f : (self : S) -> (a : Fuel) -> S = W a -> _;

f : (a : Fuel) -> (self : (b : Fuel) -> _) -> _;

x = b => f (a => )
Y : <A>(f : <α>(self : <β < α> -> A) -> A) -> A


f : (self : (b : Fuel) -> (lt : b < a) -> _) -> (a : Fuel) -> _;


W : Type;
S : (f : W) -> Type;

W = f & (self : S f) -> (a : Fuel) -> _;



\3@[\0][A][_][_][λ. \2 \0][_][B]


(λx.λy.x y) λz.y
(λx.λy1.x y1) λz.y
(λy1.y)

(x x)[x := λy. y]
(x x)[x := λy. y]
((λa. a) x)[x := λy. y]
((λa. a) (λb. b))
(λy.x y)[x:=λz.y]
(λy1.(x y)[y := y1])[x:=λz.y] // rename
(λy1.x y1)[x:=λz.y]
(λy1.(λz.y) y1)
(λy1.y[z := y1])
(λy1.y)
λy1.x y1[x:=λz.y][y:=y1]
λy1.(λz.y) y1[x:=λz.y][y:=y1]
λy1.y[x:=λz.y][y:=y1]


1 + l |- _A[\1+l] == \-1[\1+l]
------------------------------
l |- λ. _A == λ. \-1

l |- _A[\1+l] == \-1[\1+l]
------------------------------
l |- x => _A == y => \-1

_A == \1+l



(x => y => M) N K
(x = N; y => M) K
(x = N; y = K; M)


Data : Type;
Resource : Type;

Γ |- L<M> == R<N>



_A == \-1

---------------------------------
Γ |- x => _A[K] _A == y => \-1 _A

level env
  left left_args left_shifts left_substs
  right right_args right_shifts right_substs

l |- (_A[K] _A)[x] == (\-1 _A)[x]
---------------------------------
l |- λ. _A[K] _A == λ. \-1 _A


(x => _A[a := K] _A == y => y _A)

λ. _A[K] _A == λ. \-1 _A

l | λ. (\1+l)[K] (\1+l) == λ. \-1 (\1+l)

λ. \-2[K] \-1 == λ. \-1 \-1




|-------|
λ. λ. \+1 // levels

   |----|
λ. λ. \-1 // indices

x = \+1 (λ. \-1);
y = _A;
y + _A

_A := \+1 (λ. \-1)



l | λ. \+1+l
l | \-1


l | λ. _A == λ. \-1
l | _A[1+l] == \-1[1+l]
l | _A := \1+l

l | λ. \1+l == λ. \-1

x = λ. \1+l;
y + x

x = λ. \-1;
y + x

l | λ. _A[N] _A == λ. \-1 _A

l | λ. _A[N] _A == λ. (\1+l _A)[M]

_A\1+l

λ. _A _A == λ. \-1 _A

L<M> == R<N>


_A _A == _A


λ. _A[N] _A == λ. \-1 _A

M[x := a] == N[y := a]
----------------------
x => M == y => N

x => _A[x][z := _] _A[x] == y => y _A[y]
x => _A[x] _A[x] == y => y _A[y]

(_A[x] _A[x])[x := a] == (y _A[y])[y := a]
_A[a] _A[a] == a _A[a]j

_A[a] == a
_A[x] == _A[a][a := x]

_A;
T = _A;
_A := T

l | λ. _A[1+l][_] _A[1+l] == λ. \1+l _A[1+l]

l | _A[1+l][_] _A[1+l] == \1+l _A[1+l]


Γ |- M[x := a] == N[y := a]
--------------------------
Γ |- x => M == y => N

x => _A[x][z := Z] _A[x] == (y => y k _A[y])[k := K]
_A[x][z := Z] _A[x] == (a k _A[a])[k := K]

_A N == |λ. M| N

_A[x := N]

------------------
L<λ. M> == R<λ. N>


x => _A[x][z := Z] _A[x] == y => y _A[y]

x => _A[x] == y => y

_A[x][x := a] == y[y := a]
_A[a] == y[y := a]

_A := (a => y[y := a])

(x => _A[x])[_A[x] := y[y := a]]
_A[x] == _A x
_A[x] == y[y := a][a := x]


x => _A[x][z := Z] _A[x] == y => y _A[y]

l | λ. _A\1+l[N] _A == λ. \-1 _A

_A\1+l[N] == \-1

_A\1+l == \-1[↑]

_A ==

Γ |- L<M> == R<N>


Γ |- M == N

_A[N] _A == \-1 _A

(_A[N] _A)[K] == \-1 _A



L<_A[N] _A> == R<\+l _A>

l | λ. _A[N] _A == λ. \-1 _A
_A[N] _A == \!1+l _A

_A := x

λ. \!1+l[N] \!1+l == λ. \-1 \!1+l

-------------------
Γ |- L<M> ⇐ R<B[A]>


main = () => print("Hello World");


λ. _A[N] _A == λ. \-1 _A
l | λ. _A[N] _A == λ. \-1 _A
l | _A[N] _A == \1+l _A
l | _A == \1+l

l | λ. _A _A == λ. (\1+l _A)[K]
l | λ. _A _A == λ. (\1+l _A)[K]

Γ |- L<M> == R<N>

l | Γ, x | y |- L<M[\1+l]> == R<N[\1+l]>
----------------------------------------
l | Γ |- L<λx. M> == R<λy. N>


λ. (_A[N] _A)[\1+l] == λ. (\-1 _A)[\1+l]
_A := \-1[\1+l]
_A := \1+l


λ. _A[N] _A == λ. \-1 _A

λ. _A _A == λ. (\-1 \-1)[K] _A

_A := (\-1 \-1)[K]
_A := K K

λ. _A[N] _A == λ. K _A
λ. _A[N] K1 == λ. K K2

_A == \-1

f : (T = Nat; (x : T) -> T);

M[N][↑][A][B]

(\-1)[N][↑][A][B]

(M[K] N) |-> (M N)[K]

M N

(M[↑] N) |-> (M N)[↑]

\-2[A][(\-1 \-2)[N]][B][C]
(\-1 \-2)[N][↑][↑][A][(\-1 \-2)[N]][B][C]
(\-1 \+1)[N][↑][↑][A][(\-1 \-2)[N]][B][C]
(\-1 B[↑][↑])[N][↑][↑][A][(\-1 \-2)[N]][B][C]
(\-1 -1[↑][↑])[N][↑][↑][A][(\-1 \-2)[N]][B][C]


(\-1 \-2)[N][↑][↑][A][(\-1 \-2)[N]][B][C]


(\-1 \-1)[M[N]] |-> (\-2 \-2)[N][M[↓]]

(\-1 \-1)[M[N]] |-> (M[N] M[N])
(\-1 \-1)[M[N]] |-> (M M)[N]

(M[N] \-1)

(M[N][↑] \-1)[M[N]]


(\-1 \-2)[N][↑][↑][A][(\-1 \-2)[N]][B][C]
(\-1 \-2)[+2 | 4][N][A][(\-1 \-2)[N]][B][C]

s | M[↑] N
s | M[↑]@[N | s]
1 + s | (M@[N | s])[↑ | s]
f(x, y)




Γ |- (M ...A_M)[...S_M | L_M] == (N ...A_N)[...S_N | L_N]


(\-1 \-2)[N][↑][↑][A][(\-1 \-2)[N]][B][C]
=~=
(\-1 \-2)[↑ | 5][↑ | 4][N | 6][A | 3][(\-1 \-2)[N] | 2][B | 1][C | 0]

\-1[N][↑][↑][A][(\-1 \-2)[N]][B][C]
N[↑][N][↑][↑][A][(\-1 \-2)[N]][B][C]

\-2[N][↑][↑][A][(\-1 \-2)[N]][B][C]
B[↑][↑][N][↑][↑][A][(\-1 \-2)[N]][B][C]

\-2[N][↑][↑][A][(\-1 \-2)[N]][B][C]

\-1[N][↑][↑][A][(\-1 \-2)[N]][B][C]


(\-4)[N | 6][A | 3][(\-1 \-2)[N] | 2][B | 1][C | 0]

(\-2)[↑ | 5][↑ | 4][N | 6][A | 3][(\-1 \-2)[N] | 2][B | 1][C | 0]
(\-3)[↑ | 4][N | 6][A | 3][(\-1 \-2)[N] | 2][B | 1][C | 0]
(\-4)[N | 6][A | 3][(\-1 \-2)[N] | 2][B | 1][C | 0]


var > distance

distance = 1
var = 1

var < distance
distance < var


(\-1 \-2)[N][↑][↑][A][(\-1 \-2)[N]][B][C]
=~=
(\-1 \-2)[↑ | 5][↑ | 4][N | 6][A | 3][(\-1 \-2)[N] | 2][B | 1][C | 0]

(\-1 \-2)[N][↑][↑][A][(\-1 \-2)[N]][B][C]


M[N][K]
(\-2[N][K]) |-> \-1[K] |-> K

\-1[↑][N][K] |-> \-2[N][K] |-> \-1[K] |-> K

M[N][↑][↑][A][(\-1 \-2)[N]][B][C]
M[↑ | 5][↑ | 4][N | 6][A | 3][(\-1 \-2)[N] | 2][B | 1][C | 0]

\-3[N][↑][↑][A][(\-1 \-2)[N]][B][C]
\-2[↑][↑][A][(\-1 \-2)[N]][B][C]
\-4[A][(\-1 \-2)[N]][B][C]
\-3[(\-1 \-2)[N]][B][C]
\-2[B][C]
\-1[C]

\-3[↑ | 6][↑ | 5][N | 7][A | 4][(\-1 \-2)[N] | 3][B | 2][C | 1]
\-5[N | 7][A | 4][(\-1 \-2)[N] | 3][B | 2][C | 1]
C[↑3 | 8][↑ | 6][↑ | 5][N | 7][A | 4][(\-1 \-2)[N] | 3][B | 2][C | 1]

\-3[↑ | 5][↑ | 4][N | 6][A | 3][(\-1 \-2)[N] | 2][B | 1][C | 0]
\-5[↑ | 5][↑ | 4][N | 6][A | 3][(\-1 \-2)[N] | 2][B | 1][C | 0]
C[↑5 | 7][↑ | 5][↑ | 4][N | 6][A | 3][(\-1 \-2)[N] | 2][B | 1][C | 0]


M[N][↑][↑][A][(\-1 \-2)[N]][B][C]
M[↑ | 6][↑ | 5][A | 4][(\-1 \-2)[N] | 3][B | 2][C | 1]

M[N][↑][↑][A][(\-1 \-2)[N]][B][C]

\-4[↑ | 6][↑ | 5][A | 4][(\-1 \-2)[N] | 3][B | 2][C | 1]

x = 1 + y
f(x) = f(1 + y)
x - 1 = 1 + y - 1
x - 1 = y

1 + y - 1 |-> y + 1 - 1
y + (1 - 1) |-> y

f(x) = x - 1


x = M; x + x
M + M

x = M; x + x
M + M


id = <A>(x : A) => x;

Nat_or_string = (b : Bool) =>
  b |>
  | true => Nat
  | false => String;

f = (b : Bool, x : Nat_or_string b) => b;

print : (msg : String) -(IO)> ();

x : (
  () = print("hi");
  String
) = "x";


incr = x => 1 + x;
#test zero_incr_is_one = incr 0 == 1;

f : (x : incr 0 == 1) => _;

_A := (T = Nat; (x : T) -> T)

size T * 2
size T * 1

_A := (T = Nat; (x : T) -> T)

f : (T = Nat; (x : T) -> T);
f : (x : Nat) -> Nat;

id<number>

x : String = "a";
x : Id<String> = ("a" : String);

X = Option(Option(Option(Option(Int))));
T = (x : X, y : X);

T = (
  x : Option(Option(Option(Option(Int)))),
  y : Option(Option(Option(Option(Int))))
);


M[N][↑][↑][A][(\-1 \-2)[N]][B][C]
=~=
M[↑ | 5][↑ | 4][N | 6][A | 3][(\-1 \-2)[N] | 2][B | 1][C | 0]

f : (id : User_id, user : User, timeout : Nat) -> ();

k = x => (
  x = x
    |> a
    |> b
    |> c
    |> d
    |> e;
  1 + x;
);
k = x => (
  x = a(x);
  x = b(x);
  x = c(x);
  x = d(x);
  x = e(x);
  1 + x;
);

(| true => 1 | false => 0) : (b : Bool) -> Nat

f =
  | true => 1
  | false => 0;

Eff : (A )
(x : A) -(Eff)> B
(x : A) -> IO B

(x : A) -(DB | IO)> B
(x : A) -> (DB | IO) B

IO : (A : Type) -> Type;
DB : (A : Type) -> Type;

(DB | IO) : (A : Type) -> Type

(DB | IO) String == (A => (DB String | IO String))


main = () => (
  () = print("a");

)
Fail = A =>
  (x : A)

f = b =>
  b |>
  | true => 1
  | false => 0;

M =>

Unit : Type;
Unit = _;

Term :=
  | x
  | x => M
  | x = M; N
  | (x = M, y = N)
  | { x = M; y = N }
  | <A> => B
  | 1..n
  | "a..z";
Type :=
  | Type
  | Data
  | Prop
  | Resource
  | (x : A) -> B
  | (x : A & B)
  | (x : A, y : B)
  | { x : A; y : B; }
  | <A> -> B
  | A & B
  | A | B
  | Nat
  | String;

Term :=
  | x
  | x => M
  | x : A; N
  | x = M; N
  | M N;
Type :=
  | Type
  | (x : A) -> B
  | (x : A & B);


x : String & Int = _;


f : (x : String & Int) -> _;

id : ((x : String) -> String) & ((x : Int) -> Int) = <A>(x : A) => x;


case : <A>(b : Bool, then : A, else : A) -> A;
Bool = <A>(then : A, else : A) -> A

ind : <P : (b : Bool) -> Type>(b : Bool, then : P true, else : P false) -> P b;


Bool : Data;
true : Bool;
false : Bool;

Bool = (b : Bool & <P>(then : P true, else : P false) -> P b);
true = <P>(then : P true, else : P false) => then;
false = <P>(then : P true, else : P false) => else;

Type :=
  | σ
  | A & B
  | A -> B;
Term :=
  | x
  | x => M
  | M N;

Term :=
  | x
  // types
  | Type
  // lambda
  | (x : A) -> B
  | (x : A) => M
  | M N
  // let
  | x : A; N
  | x = M; N
  // self
  | (x : A) & B;


Type 0 : Type 1
Type l : Type (1 + l)

id = (A : Type) => (x : A) => x;
Bool = (A : Type) -> (then : A) -> (else : A) -> A;
true : Bool = A => then => else => then;


(true : Bool)
(true : <P>(then : P true, else : P false) -> P true)
expected : (b : Bool & <P>(then : P true, else : P false) -> P b)
ind

id(1)
id("a")

Term :=
  | x
  // types
  | Type
  // lambda
  | (x : A) -> B
  | (x : A) => M
  | M N

A & B
A | B

g : (f : <A>() -> Never) -> Never = f => f();

T : Type = (x : Nat) -> Nat;

(x : Nat > 0) =>
  x |>
  | 0 => _
  | S _ => _

ind : <P>(
  n : Nat,
  zero : P 0,
  succ : (pred : Nat, H : P pred) -> P (1 + pred)
) -> P n;


incr = x => 1 + x;
n_eq_m_then_s_n_eq_s_m : <n, m>(eq : n == m)
  -> 1 + n == 1 + m;

1_plus_n_eq_n_plus_1 : (n : Nat) -> 1 + n == n + 1 =
  n => ind(n, refl, (pred, H) => n_eq_m_then_s_n_eq_s_m(H));


1 + 2 == 3 modulo reduction

(4 == 0) modulo 4

4 % 4 == 0

Nat =
  | Z
  | S (pred : Nat);

Z3 =
  | Z : Nat
  | S (pred : Nat) : Nat
  | mod3 : S S S Z =~= Z;

caneca =~= rosquinha

bubblesort == quicksort
bubblesort =~= quicksort

Type : Type
Data : Type
Prop : Type


A == B : Prop

Type : Type;
Prop : Type;

Type l : Type (1 + l);
Data l : Type (1 + l);
Prop l : Type (1 + l);

T : Data = (A : Data 0) -> A;

id = <A>(x : A) => x;
id = A => x => x;

M[N][↑][↑][A][(\-1 \-2)[N]][B][C] =~=
M[↑ | 5][↑ | 4][N | 6][A | 3][(\-1 \-2)[N] | 2][B | 1][C | 0]


\-2[↑ | 5][↑ | 4][N | 6][A | 3][(\-1 \-2)[N] | 2][B | 1][C | 0]
\-4[↑ | 5][↑ | 4][N | 6][A | 3][(\-1 \-2)[N] | 2][B | 1][C | 0]
B[↑4 | 8][↑ | 5][↑ | 4][N | 6][A | 3][(\-1 \-2)[N] | 2][B | 1][C | 0]

-
(\-1)[↑ | 5][↑ | 4][N | 6][A | 3][(\-1 \-2)[N] | 2][B | 1][C | 0]

-
(\-2)[↑ | 5][↑ | 4][N | 6][A | 3][(\-1 \-2)[N] | 2][B | 1][C | 0]
(\-3)[↑ | 5][↑ | 4][N | 6][A | 3][(\-1 \-2)[N] | 2][B | 1][C | 0]

M[A][↑][B][↑][C]

M[A | 5][↑ | 4][B | 3][↑ | 2][C | 1] =~=
M[↑ | 4][↑ | 2][A | 5][B | 3][C | 1]

(\-1)[↑ | 4][↑ | 2][A | 5][B | 3][C | 1]
A[↑ | 6][↑ | 4][↑ | 2][A | 5][B | 3][C | 1]

(\-2)[↑ | 4][↑ | 2][A | 5][B | 3][C | 1]
(\-1 | )[↑ | 4][↑ | 2][A | 5][B | 3][C | 1]

↑

M[A | 4][↑ | 3][B | 2][↑ | 1][C | 0]

(\-1)[A | 4][↑ | 3][B | 2][↑ | 1][C | 0]

(\-1)[↑ | 3][↑ | 1][A | 4][B | 2][C | 0]

(\-1 | 5)[↑ | 3][↑ | 1][A | 4][B | 2][C | 0]
A[↑ | 5][↑ | 3][↑ | 1][A | 4][B | 2][C | 0]

(\-2 | 5)[↑ | 3][↑ | 1][A | 4][B | 2][C | 0]
(\-1 | 4)[↑ | 3][↑ | 1][A | 4][B | 2][C | 0]
(\-2 | 3)[↑ | 3][↑ | 1][A | 4][B | 2][C | 0]
B[↑ | 6][↑ | 5][↑ | 3][↑ | 1][A | 4][B | 2][C | 0]

M[A][↑][B][C][↑][D][E]

(\-2)[A][↑][B][↑][C]
(\-1)[↑][B][↑][C]
(\-2)[B][↑][C]
(\-1)[↑][C]

M[A | 6][↑ | 5][B | 4][C | 3][↑ | 2][D | 1][E | 0]

(\-1 | 7)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
A[↑ | 7][↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]

(\-2 | 7)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
(\-3 | 5)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
C[↑2 | 7][↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]


(\-3 | 7)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
(\-2 | 6)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
(\-3 | 5)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
(\-2 | 4)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
(\-1 | 3)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]

\-3[A][↑][B][C][↑][D][E]
\-2[↑][B][C][↑][D][E]
\-3[B][C][↑][D][E]
\-2[C][↑][D][E]
\-1[↑][D][E]
\-2[D][E]
\-1[E]
E

(\-1 | 7)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
A[↑1 | 7][↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]

(\-2 | 7)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
(\-1 | 6)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
(\-2 | 5)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
C[↑2 | 7][↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]

(\-3 | 7)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
(\-2 | 6)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
(\-3 | 5)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
(\-2 | 4)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
(\-1 | 3)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
(\-2 | 2)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
(\-1 | 1)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
E[↑3 | 7][↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]


x[A]



(\-1 | 7)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
A[↑1 | 7][↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]

(\-2 | 7)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
(\-3 | 5)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
C[↑2 | 7][↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]

(\-3 | 7)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
(\-4 | 5)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
(\-5 | 2)[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]
E[↑3 | 7][↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]


((x : A) -> B) == ((y : A) -> B)

(x => x + x) == λ. \1 + \1

 |---------|     |------|
(x => y => x) == λ. λ. \2

      |----|        |---|
(x => y => y) == λ. λ. \1


(x => x) == (y => y)

\3[A][↑][B][C][↑][D][E]
\2[↑][B][C][↑][D][E]
\3[B][C][↑][D][E]
\2[C][↑][D][E]
\1[↑][D][E]
\2[D][E]
\1[E]

\n[↑]


(x = A; x) |-> A

\1[A] |-> A

\(1 + n)
Term ::= | x ;

M[A][↑][B][C][↑][D][E]

M[A | 6][↑ | 5][B | 4][C | 3][↑ | 2][D | 1][E | 0]
=~=
M[↑ | 5][A | 6][B | 4][C | 3][↑ | 2][D | 1][E | 0]


M[A][↑][B][C][↑][D][E] =~=
M | [↑ | 5][↑ | 2] | [A | 6][B | 4][C | 3][D | 1][E | 0]


Id = a => (a, a);

f :: (x : String) -> ();

(y : Nat) -> Nat == (x : Nat) -> Nat

(a => (a, a)) String == String
(String, String) == String


f("a" : String)



M[A][↑][B][C][↑][D][E]
M[a := A][↑][b := B][z := C][↑][y := D][x := E]


x = N; M
M[x := N]

\1[N] |-> N // subst
\(1 + n)[N] |-> \n // skip
\n[↑] |-> \(1 + n) // shift

\1[A][↑][B][C][↑][D][E]
A[↑][B][C][↑][D][E]

\2[A][↑][B][C][↑][D][E]
\1[↑][B][C][↑][D][E]
\2[B][C][↑][D][E]
\1[C][↑][D][E]
C[↑][D][E]

\3[A][↑][B][C][↑][D][E]
\2[↑][B][C][↑][D][E]
\3[B][C][↑][D][E]
\2[C][↑][D][E]
\1[↑][D][E]
\2[D][E]
\1[E]
E

\1[A][↑][B][C][↑][D][E]

\1[↑][A][B][C][↑][D][E]

\1[A][B][C][D][E]
A[B][C][D][E]

\2[A][B][C][D][E]
B[C][D][E]

\1[A][B][C][D][E]
A[↑][A][B][C][D][E]

\1[↑][A][B][C][D][E]

M | [A][↑][B][C][↑][D][E]
M[A | 6][↑ | 5][B | 4][C | 3][↑ | 2][D | 1][E | 0]

M | [↑ | 5][↑ | 2] | [A | 6][B | 4][C | 3][D | 1][E | 0]

M | | [A][B][C][D][E]

M | [↑1 | 5][↑1 | 4] | _
M | [↑2 | 5][↑1 | 4] | _

M[A][↑][B][C][↑][D][E] =~=
M[↑ | 5][↑ | 2][A][B][C][D][E]


M[↑ | 5][↑ | 2][A][B][C][D][E]

(λ. \-1 \-2)[K] N | S | | |

M[K] N | S | | |
M[K]@[N | S] | 1 + S | | |
M@[N | S] | 2 + S | | [K | 1 + S] |

(λ. \-1)[K] N | S | | |
(λ. \-1)[K]@[N | S] | 1 + S | | |
(λ. \-1)@[N | S] | 2 + S | | [K | 1 + S] |
\-1 | 2 + S | | [N | S][K | 1 + S] |
N | 3 + S | [↑ | 2 + S] | [N | S][K | 1 + S] |
N[↑2][N[↑]][K]


\-1[N[↑]][K]
N[↑2][N[↑]][K]


id = x => x;

incr = (
  T = Nat;
  (x : T) : T => 1 + x
);

incr : (
  T = Nat;
  (x : T) -> T;
) = (
  T = Nat;
  (x : T) : T => 1 + x
);

T = (x : Nat, y : Nat) -> Nat;

f : (x : T) -> T;
x : T;

f : (x : (x : Nat, y : Nat) -> Nat) -> (x : Nat, y : Nat) -> Nat;
x : (x : Nat, y : Nat) -> Nat;


(x : Nat, y : Nat) -> Nat == (x : Nat, y : Nat) -> Nat

(\1)[A] |-> A
(\(1 + n))[A] |-> \-n
(\n)[↑] |-> \(1 + n)

M[A] |->
M[↑] |->

M[A][↑][B][C][↑][D][E]
\1[A]
A[↑][A]
A

\1[\1][B]
\1[B]
\1

\1[\1][B]
\1[↑][\1][B]
\2[\1][B]
B[↑][↑][\1][B]

\1[A][↑][B][C][↑][D][E]
A[↑][B][C][↑][D][E]

\2[A][↑][B][C][↑][D][E]
\1[↑][B][C][↑][D][E]
\2[B][C][↑][D][E]
\1[C][↑][D][E]
C[↑][D][E]

\3[A][↑][B][C][↑][D][E]
\2[↑][B][C][↑][D][E]
\3[B][C][↑][D][E]
\2[C][↑][D][E]
\1[↑][D][E]
\2[D][E]
\1[E]
E

M[A][↑][B][C][↑][D][E] =~=
M[A | 6][↑ | 5][B | 4][C | 3][↑ | 2][D | 1][E | 0]

M[A][↑][B][C][↑][D][E] =~=
M[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]

M | [↑ | 5][↑ | 2] | [A | 6][B | 4][C | 3][D | 1][E | 0]

\1 | 7 | [↑ | 5][↑ | 2] | [A | 6][B | 4][C | 3][D | 1][E | 0]
A | 8 | [↑ | 7][↑ | 5][↑ | 2] | [A | 6][B | 4][C | 3][D | 1][E | 0]
var = 1; final = 1; length = 7; shift = 0; at_ = 6; 1

\2 | 7 | [↑ | 5][↑ | 2] | [A | 6][B | 4][C | 3][D | 1][E | 0]
\3 | 5 | [↑ | 5][↑ | 2] | [A | 6][B | 4][C | 3][D | 1][E | 0]
C | 8 | [↑2 | 7][↑ | 5][↑ | 2] | [A | 6][B | 4][C | 3][D | 1][E | 0]
var = 2; final = 3; length = 7; shift = 1; at_ = 3; 2

\3 | 7 | [↑ | 5][↑ | 2] | [A | 6][B | 4][C | 3][D | 1][E | 0]
\4 | 5 | [↑ | 5][↑ | 2] | [A | 6][B | 4][C | 3][D | 1][E | 0]
\5 | 2 | [↑ | 5][↑ | 2] | [A | 6][B | 4][C | 3][D | 1][E | 0]
E | 8 | [↑3 | 7][↑ | 5][↑ | 2] | [A | 6][B | 4][C | 3][D | 1][E | 0]
var = 3; final = 5; length = 7; shift = 2; at_ = 0; 3

\1 | 2 | | [N | 0][K | 0]
N | 3 | [↑2 | 2] | [N | 0][K | 0]
var = 1; final = 1; length = 2; shift = 0; at_ = 0; 2

\2 | 2 | | [N | 0][K | 0]
K | 3 | [↑2 | 2] | [N | 0][K | 0]
var = 2; final = 2; length = 2; shift = 0; at_ = 0; 2

\1 | 3 | [↑ | 1] | [N | 0][K | 0]
N | 4 | [↑ | 3][↑ | 1] | [N | 0][K | 0]
var = 1; final = 1; length = 3; shift = 0; at_ = 0; 1



var = 1; final = 1; length = 7; shift = 0; at_ = 6; 1
var = 2; final = 3; length = 7; shift = 1; at_ = 3; 2
var = 3; final = 5; length = 7; shift = 2; at_ = 0; 3


var = 1; final = 1; length = 2; shift = 0; at_ = 0; 2
var = 2; final = 2; length = 2; shift = 0; at_ = 0; 2
var = 1; final = 1; length = 3; shift = 0; at_ = 0; 1

\1 | 2 | | [N | 0][K | 0]
\2 | 2 | | [N | 0][K | 0]
\1 | 3 | [↑ | 1] | [N | 0][K | 0]


(λ. M)[K] N | | 0 | | |
(λ. M)[K] | N@0 | 0 | | |
(λ. M) | N@0 | 1 | | K@0 |
M | | 2 | | N@1 :: K@0 |

(λ. M)[↑][K] N

length - at - shift - shift

\1[A][↑][B][C][↑][D][E]; 0

(x : Nat) -> Nat | | [A | 6][B | 4][C | 3][D | 1][E | 0]

| call | context
| l_args | l_shifts | l_substs
| r_args | r_shifts | r_substs


call = 8k
context = 8k
l_args = 1k
r_args = 1k
l_shift = 1k
r_shift = 1k
l_substs = 1k
r_substs = 1k


M[↑ | 5][↑ | 2][A][B][C][D][E]



(M[K] N)[A][↑][B][C][↑][D][E]

((λ. \1)[K] N)[A][↑][B][C][↑][D][E] |  | 0 |  |  |
((λ. \1)[K] N) |  | 7 | ↑@5 :: ↑@2 | A :: B :: C :: D :: E |
(λ. \1)[K] | N | 7 | ↑@5 :: ↑@2 | A :: B :: C :: D :: E |
λ. \1 | N | 8 | ↑@5 :: ↑@2 | K :: A :: B :: C :: D :: E |
\1 | | 9 | ↑@5 :: ↑@2 | N :: K :: A :: B :: C :: D :: E |
N | | 10 | ↑@9 :: ↑@5 :: ↑@2 | N :: K :: A :: B :: C :: D :: E |

\1 | | 10 | ↑@9 :: ↑@5 :: ↑@2 | N :: K :: A :: B :: C :: D :: E |


((λ. \1)[K] N)[A][↑][B][C][↑][D][E] |  | 0 |  |  |
((λ. \1)[K] N) |  | 7 | ↑@5 :: ↑@2 | A@6 :: B@4 :: C@3 :: D@1 :: E@0 |
(λ. \1)[K] | N@7 | 7 | ↑@5 :: ↑@2 | A@6 :: B@4 :: C@3 :: D@1 :: E@0 |
λ. \1 | N@7 | 8 | ↑@5 :: ↑@2 | K@7 :: A@6 :: B@4 :: C@3 :: D@1 :: E@0 |
\1 | | 9 | ↑@5 :: ↑@2 | N@7 :: K@7 :: A@6 :: B@4 :: C@3 :: D@1 :: E@0 |
N | | 10 | ↑↑@9 :: ↑@5 :: ↑@2 | N@7 :: K@7 :: A@6 :: B@4 :: C@3 :: D@1 :: E@0 |


// push
M[N] | A | i | S | L |-> M | A | 1 + i | S | N@i :: L
M[↑] | A | i | S | L |-> M | A | 1 + i | ↑@i :: S | L
M N | A | i | S | L |-> M | N@i :: A | i | S | L
// move
λ. M | N@a :: A | i | S | L |-> M | A | 1 + i | N@a :: S | L



(λ. M)[K] N
M[N[↑]][K]

(λ. M)[↑][K] N
M[N][↑][K]


(λ. M)[↑][K] N | | 0 | |
(λ. M)[↑][K] | N@0 | 0 | |
(λ. M)[↑] | N@0 | 1 | | K@0
λ. M | N@0 | 2 | ↑@1 | K@0
M | 2 | ↑@1 | N@0 :: K@0


((λ. M)[↑] N)[A] | | 0 | |
(λ. M)[↑] N | | 1 | | A@0
(λ. M)[↑] | N@1 | 1 | | A@0
λ. M | N@1 | 2 | ↑@1 | A@0
M | | 2 | ↑@1 | N@1 :: A@0

\1 | | 2 | ↑@1 | N@1 :: A@0
N | | 3 | ↑@2 :: ↑@1 | N@1 :: A@0

\1 | | 3 | ↑@2 :: ↑@1 | N@1 :: A@0

((λ. M)[↑] N)[A | 0]
M[↑ | 1][N@1][A@0]

3 | M[↑ | 1][\1@1][A@0]
3 | \1[↑ | 1][\1@1][A@0]
4 | \1[↑ | 3][↑ | 1][\1@1][A@0]
4 | A[↑ | 4][↑ | 3][↑ | 1][\1@1][A@0]


Term :=
  | \n | λ. M | M N
  | M[N] | M[↑] // substs
  | M/λ | M/n // notes
  ;

L<λ. M> N ≡ L<N>/λ[N] // beta
C<\#>[N] ≡ C<N/#[↑#]>[N] // zeta

(λ. λ. \2 \1) N K
(λ. \2 \1)/λ[N] K
(\2 \1)/λ[K[↑]]/λ[N]
(\2 K[↑][↑]/1)/λ[K]/λ[N]
(N[↑][↑]/2 K[↑][↑]/1)/λ[K]/λ[N]


(λ. M)[↑][K] N

((λ. M)[↑] N)[A] |-> (λ. M) N[A]

(λ. M)[↑] N


((λ. M)[↑]@N)[A]

M[N[A]][↑][A]

M | 3 | ↑@1 | N@1 :: A@0

M[N[A]][↑][A]
M | 3 | ↑@1 | N@1 :: A@0

\1 | 7 | [↑ | 5][↑ | 2] | [A | 6][B | 4][C | 3][D | 1][E | 0]
A | 8 | [↑ | 7][↑ | 5][↑ | 2] | [A | 6][B | 4][C | 3][D | 1][E | 0]
var = 1; final = 1; length = 7; shift = 0; at_ = 6; 1

\2 | 7 | [↑ | 5][↑ | 2] | [A | 6][B | 4][C | 3][D | 1][E | 0]
\3 | 5 | [↑ | 5][↑ | 2] | [A | 6][B | 4][C | 3][D | 1][E | 0]
C | 8 | [↑2 | 7][↑ | 5][↑ | 2] | [A | 6][B | 4][C | 3][D | 1][E | 0]
var = 2; final = 3; length = 7; shift = 1; at_ = 3; 2

\3 | 7 | [↑ | 5][↑ | 2] | [A | 6][B | 4][C | 3][D | 1][E | 0]
\4 | 5 | [↑ | 5][↑ | 2] | [A | 6][B | 4][C | 3][D | 1][E | 0]
\5 | 2 | [↑ | 5][↑ | 2] | [A | 6][B | 4][C | 3][D | 1][E | 0]
E | 8 | [↑3 | 7][↑ | 5][↑ | 2] | [A | 6][B | 4][C | 3][D | 1][E | 0]
var = 3; final = 5; length = 7; shift = 2; at_ = 0; 3

\1 | 2 | | [N | 0][K | 0]
N | 3 | [↑2 | 2] | [N | 0][K | 0]
var = 1; final = 1; length = 2; shift = 0; at_ = 0; 2

\2 | 2 | | [N | 0][K | 0]
K | 3 | [↑2 | 2] | [N | 0][K | 0]
var = 2; final = 2; length = 2; shift = 0; at_ = 0; 2

\1 | 3 | [↑ | 1] | [N | 0][K | 0]
N | 4 | [↑ | 3][↑ | 1] | [N | 0][K | 0]
var = 1; final = 1; length = 3; shift = 0; at_ = 0; 1

(λ. M)[↑][K] N

M[N][↑][K]

((λ. M)[↑] N)[A]
M[N[A]][↑][A]

M[N][↑][K]

M[N][↑][K]

M[N][↑][K] ≡ (M | 3)[N@2][↑@1][K@0]
M[N[K]][↑][K] ≡ (M | 3)[N@1][↑@1][K@0]

\1[N[K]][↑][K] |-> N[K][↑][N[K]][↑][K]

(N | 1)[N@1][↑@1][K@0]

(N | 1)[N@1][↑@1][K@0]

M[N][↑@1][K]

M | 3 | [↑ | 1] | [N][K]

(x => M) N |-> M[x := N]

M[N[K]][↑][K]

((λ. M)[↑] N)[K]


M[N][↑][K] ≡ (M | 3)[N@2][↑@1][K@0]
M[N[K]][↑][K] ≡ (M | 3)[N@1][↑@1][K@0]


\1 | | 3 | [(M N)[A]@1][↑@1][K@0]
(M N)[A] | | 1 | [(M N)[A]@1][↑@1][K@0]
(M N) | | 1 | [A@1][(M N)[A]@1][↑@1][K@0]
M N | | 1 | [M N@1][↑@1][K@0]
M | | 1 | [M N@1][↑@1][K@0]
(M N | | 1 |[M N@1][↑@1][K@0]
\1 | 1 | K@0
K | 0 | K@0

\1 | | 3 | [M[A]@1][↑@1][K@0]
M[A] | | 1 | [M[A]@1][↑@1][K@0]
M | | 1 | [M[A]@1][↑@1][K@0]

(λ. M)[K] N | 0 |
M | 1 | N@0 :: K@0

\1 | 1 | N@0 :: K@0
N | 0 | N@0 :: K@0

((λ. M) N)[K]



(λ. M)[K] N | 0 |
M | 2 | N@0 :: K@0



\1
N | 0 | N@0 :: K@0

M (f N)


((λ. M)[↑] N)[K] | | 0 | |
(λ. M)[↑] N | | 1 | | K@0
(λ. M)[↑] | N@1 | 1 | | K@0
(λ. M) | N@1 | 2 | ↑@1 | K@0


M[N@1][K]

M[N][@0][K]

((λ. M)[0!] N)[K] | | |
(λ. M)[0!] N | | | K@0
(λ. M)[0!] | N@1 | | K@0
λ. M | N@1 | 0!1 | K@0
M | | 0!1 | N@1 :: K@0

\-1 | | 0!1 | N@1 :: K@0
 | | 1!3 :: 0!1 | N@1 :: K@0

M[N][↑][A] =~= M | ↑@1 | N@2 :: K@0
M[N[K]][↑][A] =~= M | ↑@1 | N@1 :: K@0

M[↑@1][N][A] |->
M[↑@1][N][A] |->

\1[N[K]][↑][K] |-> N[K][↑][N[K]][↑][K]

M[C][↑][B][A]

M[↑@1][C][B][A]


M[N]

\1[↑][↑]

Term ::=
  | \-n
  | M[N]
  | M[↑];

M[↑][N]

\1[↑][N]
```

## Money

GUI builder
Something something mobile?
Laravel / Ruby on Rails

Serverless magic deploy

### Platform

Complexity analysis

## Reduce

```rust
((λ. M)[↑] N)[K]

(M[↑] N)[K]
M N[K]

(M[↑] N)[K]

M[↑] N

M | S |
(M[↑] N)[K]

M | 0 |

N[N[X][K]][↑][K]

\1 \2 | | [N[X][K]][Y][↑][K]

(λ. M)[B] A
(λ. M[⇑B]) A
M[⇑B][A]

M[A[↑]][B]

(λ. M)[]

O<L<(λ. M)> A> |-> O<L<M[A[pack #O]]>>


M | S |

λ.

Type 0 :


(λ. M)[pack #O] A |->
M | S |

((λ. M) A)[N] | | | N@0
(λ. M) A | | | N@0
M | A@1 | | A@1; N@0

\0 | A@1 | | A@1; N@0
A | A@1 | @1 | A@1; N@0

(λ. M) A | | |
λ. M | A | |


((λ. M)[↑] A)[B] | |
((λ. M)[↑] A) | | [B@0]
(λ. M)[↑] | A@1 | [B@0]
λ. M | A@1 | [B@0]
M |  | [A@1][B@0]

(λ. \0 \1)[\0][B]
(λ. \0 \1)[\0][B]

(λ. \0 \1)[A][B]


(x : Int) -> Int;


Unit =
  | unit;

id : <A>(x : A) -> A = x => x;

Bool = <A>(x : A, y : A) -> A;

O(n**2)


Term ::=
  | \x // context bound
  | \+n // substs bound
  | \-n // term bound
  | λ. M | M N | M[N]

Γ |- L | M

x, y, z |- \z | (\x \-1 \+1)[\y]
x, y, z |- \z; \y | \x \-1 \+1
x, y, z |- \z; \y | \x \y \+1
x, y, z |- \z; \y | \x \y \z


x[x := a b] == y[y := a b]

x[x := A]


(x => (y => x + y) (b U)) (a T)
(x => y => x + y) (a T) (b U)

x = a T; y = b U; x + y
x = a T; y = b U; x + y

(λ. M)[A] N

(x => y => x + y) (f A)

(T = )


Γ; x = N |- M ⇒ A
------------------------
Γ |- x = N; M ⇒ x = N; A

Γ |- M ⇐ L[N]<A>
--------------------
Γ |- M ⇐ L<x = N; A>


Γ; x : L<A> |- M ⇐ L[x]<B>
-----------------------------
Γ |- x => M ⇐ L<(x : A) -> B>

Γ; x : _L<_A> |- M ⇐ _L[x]<_B>
--------------------------------
Γ |- x => M ⇐ _L<(x : _A) -> _B>

Γ |- M ⇐ _L<_A>
---------------
Γ |- M ⇒ _L<_A>

Γ |- L<x => M>


-----------------
Γ |- L<M> == R<N>


[K][↑][N[K]][↑][K]

M[A][↑][B][C][↑][D][E] =~=
M[↑ | 5][↑ | 2][A | 6][B | 4][C | 3][D | 1][E | 0]

M | [A | 6][B | 4][C | 3][D | 1][E | 0]


M[A][↑][B][C][↑][D][E] | 0 | |
M[A][↑][B][C][↑][D] | 1 | | [E]
M[A][↑][B][C][↑] | 2 | | [D][E]
M[A][↑][B][C] | 3 | [↑@2] | [D][E]
M[A][↑][B] | 4 | [↑@2] | [C[↑@2]][D][E]
M[A][↑] | 5 | [↑@2] | [B[↑@2]][C[↑@2]][D][E]
M[A] | 6 | [↑@5][↑@2] | [B[↑@2]][C[↑@2]][D][E]
M | 6 | [↑@5][↑@2] | [A[↑@5][↑@2]][B[↑@2]][C[↑@2]][D][E]

M | 6 | [↑@5][↑@2] | [A[↑@5][↑@2]][B[↑@2]][C[↑@2]][D][E]

M[A][↑][B][C] | ↑ | [D | ][E | ]
M[A][↑][B][C] | ↑ | [C | ↑]; D, []; E, []


M[A][↑][B][C][↑][D][E] | 0 | |
M[A][↑][B][C][↑][D] | 1 | | [E]
M[A][↑][B][C][↑] | 2 | | [D][E]
M[A][↑][B][C] | 3 | ↑@2 | [D][E]
M[A][↑][B] | 4 | ↑@2 | [C | ↑@2][D][E]
M[A][↑] | 5 | ↑@2 | [B | ↑@2][C | ↑@2][D][E]
M[A] | 6 | ↑@5; ↑@2 | [B | ↑@2][C | ↑@2][D][E]
M | 6 | ↑@5; ↑@2 | [A | ↑@5; ↑@2][B | ↑@2][C | ↑@2][D][E]

\1 | 6 | ↑@5; ↑@2 | [A | ↑@5; ↑@2][B | ↑@2][C | ↑@2][D][E]
A | ? | ↑@?; ↑@5; ↑@2 | [A | ↑@5; ↑@2][B | ↑@2][C | ↑@2][D][E]

\2 | 6 | ↑@5; ↑@2 | [A | ↑@5; ↑@2][B | ↑@2][C | ↑@2][D][E]
\2 | ? | ↑2@?; ↑@2 | [A | ↑@5; ↑@2][B | ↑@2][C | ↑@2][D][E]

M[A][↑][B][C][↑][D][E] | 0 | |
M[A][↑][B][C][↑][D] | 1 | | [E]
M[A][↑][B][C][↑] | 2 | | [D][E]
M[A][↑][B][C] | 3 | ↑@2 | [D][E]
M[A][↑][B] | 4 | ↑@2 | [C | ↑@2][D][E]
M[A][↑] | 5 | ↑@2 | [B | ↑@2][C | ↑@2][D][E]
M[A] | 6 | ↑@5; ↑@2 | [B | ↑@2][C | ↑@2][D][E]
M | 6 | ↑@5; ↑@2 | [A | ↑@5; ↑@2][B | ↑@2][C | ↑@2][D][E]


(λ. M)[A] N |-> M[N[↑]][A]

(λ. M)[↑][K] N

(λ. \1)[↑][K] N
(λ. \2)[↑][K] N
(λ. \3)[↑][K] N

(λ. _M)[↑][K] N | | |
(λ. _M)[↑][K] | [N@0 |] | |
(λ. _M)[↑] | [N@0 |] | | [K@0 |]
_M | | ↑@1 | [N@0 |][K@0 |]

\1 | | ↑@1 | [N@0 |][K@0 |]
N | ↑(1+1)@2 | | [N@0 |][K@0 |]

\2 | | ↑(2+0)@1 | [N@0 |][K@0 |]

λ. \1 | [N |] | ↑@1 | [K |]

(λ. M)[↑][C][B] N

((λ. M)[↑][C][B] N)[A] | | |
(λ. M)[↑][C][B] N | | | [A@0 |]
(λ. M)[↑][C][B] | [N@1 |] | | [A@0 |]
(λ. M)[↑][C] | [N@1 |] | | [B@1 |][A@0 |]
(λ. M)[↑] | [N@1 |] | | [C@2 |][B@1 |][A@0 |]
λ. M | [N@1 |] | ↑@3 | [C@2 |][B@1 |][A@0 |]
M | | ↑@3 | [N@1 |][C@2 |][B@1 |][A@0 |]

\1 | | ↑@3 | [N@1 |][C@2 |][B@1 |][A@0 |]
N | | ↑(1+2)@4 | [N@1 |][C@2 |][B@1 |][A@0 |]

\2 | | ↑@3 | [N@1 |][C@2 |][B@1 |][A@0 |]
\3 | | | [N@1 |][C@2 |][B@1 |][A@0 |]
B | | ↑(3+0)@4 | [N@1 |][C@2 |][B@1 |][A@0 |]

\3 | | ↑@3 | [N@1 |][C@2 |][B@1 |][A@0 |]
\4 | | | [N@1 |][C@2 |][B@1 |][A@0 |]
A | | ↑(4+0)@4 | [N@1 |][C@2 |][B@1 |][A@0 |]


\1 | | ↑@3 | [N@1 |][C@2 |][B@1 |][A@0 |]
N | | ↑3@4 | [N@1 |][C@2 |][B@1 |][A@0 |]

\2 | | ↑@3 | [N@1 |][C@2 |][B@1 |][A@0 |]
\3 | | | [N@1 |][C@2 |][B@1 |][A@0 |]
B | | ↑3@4 | [N@1 |][C@2 |][B@1 |][A@0 |]

\3 | | ↑@3 | [N@1 |][C@2 |][B@1 |][A@0 |]
\4 | | | [N@1 |][C@2 |][B@1 |][A@0 |]
A | | ↑4@4 | [N@1 |][C@2 |][B@1 |][A@0 |]

((λ. M)[C][↑][B] N)[A] | | |
(λ. M)[C][↑][B] N | | | [A@0 |]
(λ. M)[C][↑][B] | [N@1 |] | | [A@0 |]
(λ. M)[C][↑] | [N@1 |] | | [B@1 |][A@0 |]
(λ. M)[C][↑] | [N@1 |] | ↑@2 | [B@1 |][A@0 |]
(λ. M)[C] | [N@1 |] | ↑@2 | [B@1 |][A@0 |]
λ. M | [N@1 |] | ↑@2 | [C@2 | ↑@2][B@1 |][A@0 |]
M | | ↑@2 | [N@1 |][C@2 | ↑@2][B@1 |][A@0 |]

\1 | | ↑@2 | [N@1 |][C@2 | ↑@2][B@1 |][A@0 |]
N | | ↑3@4 | [N@1 |][C@2 | ↑@2][B@1 |][A@0 |]

\2 | | ↑@2 | [N@1 |][C@2 | ↑@2][B@1 |][A@0 |]
C | | ↑2@4; ↑@2 | [N@1 |][C@2 | ↑@2][B@1 |][A@0 |]
\1 | | ↑2@4; ↑@2 | [N@1 |][C@2 | ↑@2][B@1 |][A@0 |]
\3 | | ↑@2 | [N@1 |][C@2 | ↑@2][B@1 |][A@0 |]
\4 | | | [N@1 |][C@2 | ↑@2][B@1 |][A@0 |]

\3 | | ↑@2 | [N@1 |][C@2 | ↑@2][B@1 |][A@0 |]
\4 | | | [N@1 |][C@2 | ↑@2][B@1 |][A@0 |]
A | ↑4@4 | ↑@2 | [N@1 |][C@2 | ↑@2][B@1 |][A@0 |]

((λ. M)[C][↑][B] N)[A]

((λ. \2)[\1] N)[A]
\2[N[↑]][\1][A]
\1[\1][A]
\1[A]

((λ. \3)[C] N)[A]
\3[N[↑]][C][A]
\2[C][A]
\1[A]

offset hole


\2[A][B[C]]
B[↑2][A][D][C]



x = 1 + 1;
x + x

(1 + 1) + (1 + 1)

x = 2;
x + x

2 + 2

(λ. M)



add = (a, b) => a + b;

x = add (1, 2);

main = () => (
  msg = "Hello";
  print (msg);
);


(x => M) (1 + 1)

x = 1 + 1;
x + x

(1 + 1) + (1 + 1)

x = 2;
x + x

x = 2;
2 + w

x = 1 + 1;
3 + 4

x = 1 + 1;
3 + 4

((M : P (256 * 256)) : P 65536 [#fuel])

(x => 1) Ω

(x => 1) N

(x => y => z => M) A B C

map : _;
map = _;

Bool = {|
  @Bool = <A>(t : A, f : A) -> A;
  true : @Bool = (t, f) => t;
  false : @Bool = (t, f) => f;
|};

Nat : Type;
Int : Type;
Rat : Type;

Int32 : Type;
Float32 : Type;


Low_level = {
  Size : Type;
  Array : {
    make : <A>(len : Size & len >= 0, initial : A) -> Array<A>;
  };
};

Array : {
  make : <A>(len : Int & len >= 0, initial : A) -> Array<A>;

};

second_true : Bool = (t, f) => t;

Type : Type;
Data : Type;
Prop : Type;
Resource : Type;


Type : Type;
Prop : Type;
CPS : Type;
Machine : Type;


f = (mem : Memory, stack : Stack, reg : Register) => ();


(λ. M[close x])


|---|
(λ. \1 \2)

(x => x y);

y = M;
x = λ. \1 \2;
z = N;
x

x = λ. \1 \2;
z = N;
λ. \1 \4

Term ::=
  | x | \n
  | λ. M | M N | M[N];

y => (
  x = λ. \1 \2;
  z = N;
  x
);

(
  x = λ. \1 \2;
  z = N;
  x
)[open y]

(λ. M)[A] N
M[N][A]

((λ. \1 \2)[A] N)[...S]

((λ. \1 \2)[A] N)[...S]

(\1 \2)[N[...S]][A][...S]
(N[...S] \1)[A][...S]
(N[...S][A] A)
N A


(
  x = (λ. \1 y)[A];
  z = N[A];
  x[A]
)

Term ::=
  | x[...A] | \n[...A]
  | (λ. M[...A])[...B] | M[...A] N[...B] | M[...A][N[...B]];


(λ. M)

\1[x]
x

(λx. x) N
\1[x][N]
x[N]

M[...A] N[...B]

x = 1 + 1;
x + x

x = 1 + 1;
x + x

(1 + 1) + (1 + 1)

x = M; x x

x = 2; x x


x = M; x x

M M

M M

M M

x = M; x x
x = M; M x // subst
x = M; M M // subst
M M // gc


((λ. M)[C][↑][B] N)[A]
((λ. M)[C][↑] N[↑])[B][A]
((λ. M)[C] N[↑][B])[A]
((λ. M) N[↑][B][↑])[C][A]
((λ. M) N[↑])[C][A]
M[N[↑]][C][A]

M[N[↑]][C][A]

M[A][↑][B][C][↑][D][E] =~=
M[↑@5][↑@2][A][B][C][D][E]



PUSH 4; PUSH 3; PUSH 2; PUSH 1; ADD 3;

(λ. \1 + \1)

[|LAMBDA 3; ACCESS 1; ACCESS 1; ADD; |]

(x => M) N

(x = N; M)

(λ. M[...S])

without


λ. M ⇐

(M[A] N)[K] | | |
M[A] N | | | K@0
M[A] | N@1 | | K@0
M[A] | N@1 | | A@1; K@0

(M N[↑])[A][K]

: (A : Type) -> (x : A) -> (...S; A)
= (A : Type) => (x : A) => x


M : ((x : A) -> B)



Γ; L |- M ⇒ A
----------------
Γ |- L<M> ⇒ L<A>

Γ; L |- M ⇐ A
-------------
Γ |- L<M> ⇐ A

Γ; L |- M ⇐ A
-------------
Γ |- M ⇐ L<A>

Γ |- L<M N> ⇒ T
---------------
Γ |- L<M> N ⇒ T


Γ |- M ⇐ L<B>


Γ |- M ⇒ L<(x : A) -> B>
---------------------------------
Γ |- M ⇐ (x : _A) -> _B[x := \-1]

_A := L<A>
_B := L<[?]B>

L<A> == _A  L<[x : A) -> B>
---------------------------------
L<(x : A) -> B> == (x : _A) -> _B


Γ; z |- L<_B> == R<C>
-----------------------------------
Γ |- L<(x : A) -> _B> == R<(y : A) -> C>


L<M[A]> == R<M[A]>

_C[y : A] == B[x : A]

_B == C
-------------------
∀: A. _B == ∀: A. C

(x : A) -> B == (x : _A) -> _B

(M : (Nat; \1 -> \1))
(M : Nat -> Nat)

((x : (Nat; \1 -> \1)) => (x : (Nat; \1 -> \1)))

Nat;
(x : \1 -> \1) => (x : \2 -> \2)


Term ::=
  | x | \n
  | λ. M | M N;


T = Nat;

T = Nat |- (x : T -> T) => (x : T -> T)

T = Nat;
((x : T -> T) => (x : T -> T))[close T]

((x : T -> T) => (x : T -> T))[close T] |->
((x : \1 -> \1) => (x : \2 -> \2))

((x : T -> T) => (x : T -> T))[close T] |->
((x : \1 -> \1) => (x : \2 -> \2))

x => x + x
T = Nat;
((x : \1 -> \1) => (x : \2 -> \2))

(f = x\1 => x\1; f f)

f = x\1 => (a = x\1; M);

x\1 := f
x\2 => (a = x\2; M{x\1 := x\2})

Term ::=
  | x | \n
  | λ. M | M N
  | M[open N] | M[close x];

λx. \1 + \1

x + x


x : A = N;
M[close x]

incr : (x : Nat) -> Nat;

(incr 1 : Nat)

incr : (T = Nat; (x : T) -> (T, T));

(incr 1 : (T = Nat; (T, T)))
(incr 1 : (Nat, Nat))

Term ::= | x | x => M | M N | x = N; M;

(x => M) N |-> x = N; M
x = N; M<x> |-> x = N; M<N>
x = N; M |-> M  x ∉ fv M

x = N; M |-> M{x := N}


(x => y => z => M) A B C

(x => y => z => y) A B C

(x = 1 + 1; x + x)
(x = 2; x + x)
(x = 2; 2 + x)

(x = 1 + 1; (1 + 1) + x)
(x = 2; 2 + x)

(x = 2; x + x)
(x = 2; 2 + x)
(x = 2; 2 + 2)

L<M> == R<A>  L<N> == R<B>
--------------------------
L<M N> == R<A B>

(x => x x x) M |-> M M M // beta

x = M; x x x |-> x = M; M x x // subst
x = M; x x x |-> x = M; M M x // subst
x = M; x x x |-> x = M; M M M // subst


y = 1; (x => y + x) == x => 1 + x


M[A] // lets
M[x := A]
x = A; M

M[A][↑][B][C][↑][D][E] =~=
M[↑@5][↑@2][A][B][C][D][E]

\n[A][B][C][D][E]


x == x

Γ; x = A |- M ⇐ B
-----------------
Γ |- M ⇐ x = A; B


Nat : Type;
Nat = <A>(zero : A, succ : (pred : Nat) -> A) -> A;

↑

Γ |- A ≂ B
--------------
Γ |- M ⇒ A ⇐ B

(f : (A : Type) -> A) M

1@↑; ((P : (z : \-1) -> \-2) -> (l : \-0 \-2) -> (1@↑; (\-0 \-2))), zight : (P : (z : \-2) -> \-3) -> (l : \-0 \-3) -> \-1 \-4

Term ::=
  | \n | λ. M | M N
  | M[N] | M[↑]
  | M#λ | M#n
  | M@[N@n] | M[↑@n] | M[N@n];


(M[A] N) ≡ (M N[↑])[A]

(λ. \1)[A]@[N@0]
(λ. \1)@[N@0][A]
\1[N@0][A]
N[↑]#1[N@0][A]
N[↑]#1[N@0][A]

M@[N@0][A]


(M[K] A B C)[n | ...L] ≡ (M@[A@n][B@n][C@n])[K@n]

(M[K] A B C) == (M A[↑] B[↑] C[↑])[K]

(M[K] A B C) ≡ M[K] A B C



M[K] A B C

(x => y => M) A B

x = A; y = B; M

(T = A; T)
T[T = A]

A | #T; T = A
T | T = A
T = A; T |

T[T = A]
T = A; T

L<(x : A) -> B>

L<(x : A) -> B>[.return]
(x : L<A>) -> L<B[\1[↑L]]>

((x : \1) -> \2 \1)[A];
(x : \1[A]) -> (\2 \1)[\1[↑]][A]
(x : \1[A]) -> (\1[A] \1)


Γ |- M ⇒ T  Γ |- N ⇐ T[param]
-----------------------------
Γ |- M N : T[body N]


Γ |- M ⇒ L<(x : A) -> B>  Γ |- N ⇐ L<A>
---------------------------------------
Γ |- M N ⇒ L<B[x = N]>



Γ |- M ⇒ (x : A) -> B -| Γ; Δ
Γ; Δ |- N ⇐ A -| Γ; Δ; Φ
------------------------------
Γ |- M N ⇒ B[x = N] -| Γ; Δ; Φ


_A := x = _A; _B[x]


_A == M N


f : (x : A : Linear) -> (y : B : Data) -> B = _;


Γ; A |- (x : A) -| Δ
--------------------
Γ |- (x : L<A>) -| Δ

Γ |-
---------------
Γ |- (L<M> : A)

(x = L<N>; M)

L<(x = N; M)>

Id = A => A;
(x : Id Nat) => x

Id = A => A;
A = Nat;
(x : A) => x



Γ |- (x : A) => L<M> ⇒
Γ; [x : A] |- L<M> ⇒ B
Γ; [x : A]; L |- M ⇒ B
Γ; [x : A]; L |- (x : A[↑L][↑]) -> B[?]

---------------------------------------
Γ; [x : A]; L |- (x : A) -> B[?]

Γ |- (x : A) -> B


(x : A) -> L<B>

((x : A) -> L<B>)

M[A][B][C]


L[A]<M == N[↑]>
---------------
L<M[A] == N>

L[A]<M == N>
---------------
L<M == N[A]>





Id = (A : Type) => A;

incr = (x : Int) => 1 + x;
two = (
  x = 1;
  x + 1
);

div = (num : Int, den : Int & den != 0) => _;

Array : {
  update : <A>(arr : Array A, i : Int, new_value : A) ->
    (arr : Array A, old_value : A);
} = _;
connect = (ip : IPv4) -> (sock : Socket)

main = () => print ("Hello");



Term ::=
  | x | x => M | M N | x = N; M
  | M#(β L) | M#x;

L<x => M> N ≡ L<(x = N; M)#(β L)> // dB
x = N; C<x> ≡ x = N; C<N#x> // subst


incr : (
  Level = Int;
  (x : Level) -> Level
) = _;

(
  Level = Int;
  (x : Int#x) -> Int#x
)

(incr 1 : (
  Level = Int;
  Int#Level
));

(
  Level = Int\1;
  (x : Level\1) -> Level\2
)



(x : Int\1) -> Int\2

Term ::=
  | \n | λ. M | M N | M[N]
  | M[L ↑ β] | M[n ↑];

L<λ. M> N ≡ L<M[N[L ↑ β]]>
C<\n>[N] ≡ C<N[n ↑]>[N]  n == depth C

L<(λ. M) N> ≡ L<M[N[0↑#β]]>

L<M[N[L↑#β]]>


-----------
Γ |- \n : A

Γ |- \n : A
-----------------------
Γ, T |- \(1 + n) : A[? ↑]

(M[A] N) ≡ (M#[\1] N[↑])[A]

(M#[\1] N[↑])[A] K

((M#[\1] N[↑])#[\1] K[↑])[A]
((M#[\1] N[↑])#[\1] K[↑])[A]

(M#[\1] N[↑])[A] K
M[A] N K


M[A] N K
(M[A] N)[@K]
M[A][@N][@K]
M#[\1][A][@N][@K]
(M#[\1] N[↑])[A][@K]

(M#[\1] N[↑] K[↑])[A]


--------------
Γ |- M[A] == N

Either = (A, B) =>
  | (tag == true, payload : A)
  | (tag == false, payload : B);

Either = (A, B) =>
  (
    tag : Bool,
    payload : tag |> | true => A | false => B
  );

(x = 1; x + x) ≡ ((x => x + x) 1)


(x = 1; x) |-> 1
((x => x) 1) |-> 1


(x = 1; x + x) ≡ 1#x#(x = 1)
((x => x) 1) ≡ 1#x#βx

(x = 1; x + x) ≡ 1#x#(x = 1)
((x => x) 1) ≡ 1#x#βx

incr : (
  T = Nat;
  (x : T) -> (T, T)
) = x => (1 + x, 1 + x);

dup : (
  T = (x : Nat) -> Nat;
  (x : T) -> (T, T, T, T)
) = x => (x, x);


x : (
  (x : Nat) -> Nat,
  (x : Nat) -> Nat,
  (x : Nat) -> Nat,
  (x : Nat) -> Nat
) = dup _;

x : (
  T = (x : Nat) -> Nat;
  (T, T, T, T)
) = dup 1;


received : Nat
expected : (
  (x : Nat) -> Nat,
  (x : Nat) -> Nat,
  (x : Nat) -> Nat,
  (x : Nat) -> Nat
)

T = (x : Nat) -> Nat;
received : Nat;
expected : (T, T, T, T);


Type : Type;
Data : Type;
Prop : Type;
Resource : Type;

STLC : Type;

Type : Type;

Prop : Type;

CPS : Type;


id = <A : Type>(x : A) => x;

x = id Int;

Set : Type;
Prop : Type;

Type : Type;
Set : Type;

Type : Type;
Mono : Type;

id : (A : Mono) -> (x : A) -> A
  = (A : Mono) => (x : A) => x;


Γ |- M : T
Γ |- T : (T : Type) & { x : (self : T) -> A }
---------------------------------------------
Γ |- M.x : A[self := M]

Γ |- M : T
---------------------
Γ |- M.x |-> T::x(M);


Γ |- M : fst T
Γ |- T : (T : Type, { x : (self : T) -> A })
---------------------------------------------
Γ |- M.x : A[self := M]

Γ |- M : fst T
---------------------
Γ |- M.x |-> (snd T)::x(M);


(M[A] N)
M[A][@N]

(λ. M)[A] N
(λ. M)[A][@N]
M[N[↑]][A]

M[A][@N] == M[A][@N]
(M N[↑])[A]

(λ. \1)[A][@N]

(M[A] N)
(M N[↑])[A]

(λ. M)[A] N |  |
(λ. M)[A] | N |
λ. M | N@0 | A
M | | N[↑ β] :: A

λ. M | N@0 | A
λ. M | N@0 | A

λ. M | N@0 | A

M | | N[↑]@0[β]; A@1

M | | N[↑]@0[β]; A@1

M | | N[↑]@0 :: A@1 ≡

M | N[↑]@0 | A@1


λ. M | N@0 | A
(λ. M)[A] | N@0 |
(λ. M)[A] N | |


(x : A[K], y : \1 \2)[B]
(x : A, y : \1 \2)[K][B]
(x : A, y : B[↑])[K]

((x : A[K]) -> B)
((x : A) -> B)[K]

(x : T[A], y : \1 \2)[B]
(x : T, y : (\1 \2)[↑ : 1])[A][B]

(x : T[A], y : (λ. )\1 \2)[B]

(x : T[A], y : U)[B]

(x : T, y : (λ. U)[↑] \1)[A][B]

((λ. M)[↑] N)[A]
((λ. M) N[A])

(x : T[A], y : U)[B]
(x : T, y : U[↑ : 1])[A][B]

B; (x : T[A], y : U)

B; A; (x : T, y : U[↑ : 1])

[↑ : 1]

M | | N[↑ β] :: A
λ. M | N@0 | A
(λ. M)[A] | N@0 |
(λ. M)[A] N | |



(λ. M)[A] | N |
λ. M | [↑][N] | [A]
λ. M | [N[↑]] | [A]
M | | [N[↑] | β][A]

λ. M | [N[↑]] | [A]
λ. M | [↑][N] | [A]
(λ. M)[A] | [N] |
(λ. M)[A] N | |

((λ. M)[↑] N)[A] | |
(λ. M)[↑] N | | [A]
(λ. M)[↑] | [N] | [A]
λ. M | [A][N] | [↑][A]
M | [N] | [↑][A]

((λ. M)[↑] N)[A] | |
(λ. M)[↑] N | | [A]
(λ. M)[↑] | N | [A]
λ. M | [A]; N | [↑][A]

(λ. M)[↑] N | | | [A]
(λ. M)[↑] | N | | [A]
λ. M | N | [↑] | [A]

M | | [↑] | [N β][A]

((λ. M)[↑] N)[A] | | |
(λ. M)[↑] N | | | [A]
(λ. M)[↑] | N | | [A]

((λ. M)[↑] N)[A] | | |
(λ. M)[↑] N | | | [A]
(λ. M)[↑] N | | | [A]

(λ. M)[A] N

((λ. M)[↑] N)[A]

(λ. M)[A] N
((λ. M) N)[A]

(λ. M)[A][@N]
(λ. M)[A] N

λ. M | [A][@N]

M[N[↑ β]][A]

M | [N | ↑ β][A] | 0 |
λ. M | [A] | 1 | N
(λ. M)[A] | | 0 | N
(λ. M)[A] N | | |



M | [↑] |


(λ. M) N


\n |-> VAR n
M N |-> M; (APPLY; N; LET);
λ. M |-> (LAMBDA; M)
M[N] |-> (N; LET); M

0     | N.L | S |-> N | L | S
1 + n | N.L | S |-> n | L | N.S

(LAMBDA; M); K | L | S |-> K | L | (LAMBDA; M).S
LET; K | L | V.S |-> K | V.L | S

APPLY N; M | L | S       |-> M     | L | LET N.S      // apply
(LAMBDA; M); K    | L | LET N.S |-> LET N | L | M.K.S    // beta
LET N; M       | L | S       |-> N     | L | M.S     // zeta


M A B |-> M; APPLY; A; LET; APPLY; B;


0     | N.L | S |-> N | L | S
1 + n | N.L | S |-> n | L | N.S

APPLY N; M | L | S       |-> M     | L | LET N.S      // apply
LAMBDA M; K    | L | LET N.S |-> LET N | L | M.K.S    // beta
LET N; M       | L | S       |-> N     | L | M.S     // zeta


λM | L | @N.S |-> LET N | L | M@.S // beta

λM | L | @N.S |-> N | L | M@.S // beta
λN | L | M@.S |-> M | λN.L | S // lambda-apply


λM | L | •.S |-> λ; M | L | •.S // lambda-halt


@B; @A; λλM;

λλM | L | @A.@B.S |->


(λ. λ. M) A B
λ.  | |  |->

@; N | L | V |-> N | L


M[A] N[B]
(M N[B][↑])[A]
(M N[B])[↑][A]

(M N[↑])[A[↑]][B]


(M[x := A] N[y := B])
(M N)[y := B][x := A]

(M[A] N[B])
(M N[B][↑])[A] // stuck here

/* but by carrying the "depth" of the shift
  the shift can be through the binder
  unlocking a traditional lifting of substs */

(M N[↑ : 1][B[↑]])[A]
(M[↑] N[↑ : 1])[B[↑]][A]


Term (M, N) := \n | λ. M | M N | M[S];
Subst (S) := N | ↑ | ⇑S;


(λ. M)[A]
λ. M[A : n]

Term (M, N, K) :=
  | \n | λ. M | M N
  | M[N] | M[↑ : n];


(M[A] N[B])
(M N[B][↑ : 0])[A]
(M[↑] N[↑ : 1])[B[↑ : 0]][A]

((λ. M) N[B])

\0[N] |-> N
\(1 + n)[N] |-> \n

\n[↑ : m] |-> \(1 + n)    n >= m
\n[↑ : m] |-> \n          n < m

(λ. M)[↑ : n] |-> λ. M[↑ : 1 + n]

(M N)[K] |-> M[K] N[K]
(M N)[↑ : n] |-> M[↑ : n] N[↑ : n]

M[A][↑ : n] |-> M[↑ : 1 + n][A[↑ : n]]

M[↑ : 0][A]


M[A][↑ : n] |-> M[↑ : 1 + n][A[↑ : n]]

(λ. M)[↑] |-> λ. M[↑ : 1]

(λ. M)[A] ≡
(λ. M)[A] ≡ λ. M[↑ : 1][A[↑]]

((λ. t) v)[u]

(λ. t)[u] v[u]
t[v[u][↑]][u]

t[v][u]

M[N[K][↑]][K] ≡ M[N][K]

M[v][u]
M[v][u]

t[↑ : 1][u[↑ : 0]][v[u]]

Γ; A |- M == N[↑]
-----------------
Γ |- M[A] == N

Γ; A[↑] |- M[↑ : 1] == N[↑]
---------------------------
Γ |- M[A][↑] == N


(M N[↑ : 1])[B[↑]][A]
M[↑] N[↑ : 1][B[↑]][A]

(M N[?])[B[↑]][A]

x |-> VAR x
\n |-> ACCESS n
M N |-> APPLY N; M
λ. M |-> LAMBDA M
M[N] |-> LET N; M


ACCESS n; K | L | S |-> L<n> | L | JMP K.S
APPLY N; M | L | S |-> M | L | LET N.S
LET N; M | L | S |-> N | L | JMP M.S
LAMBDA M; K | L | LET N.S |-> N | L | IN M.S
LAMBDA N; K | L | JMP M.S |-> M | LAMBDA N.L | S

// halt
VAR x; K | L | S
LAMBDA N; K | L | •


x |-> VAR x
\n |-> ACCESS n
M N |-> APPLY M; N
λ. M |-> LAMBDA M
M[N] |-> LET N; M

ACCESS n; K | L | S |-> L<n> | L | S

APPLY M; N | L | S |-> M; N | L | APPLY N.S
LET N; M | L | S |-> N; M | L | LET M.S

LAMBDA M; K | L | APPLY N.S |-> N | L | LET M.S
LAMBDA N; K | L | LET M.S |-> M | LAMBDA N.L | S


// TODO: two types of lambdas, LAMBDA M; N and LAMBDA N; M
x |-> VAR x
\n |-> ACCESS n
M N |-> APPLY M; N
λ. M |-> LAMBDA M
M[N] |-> LET N; M

ACCESS n; K | L | S |-> L<n> | L | S

APPLY M; N | L | S |-> M; N | L | APPLY N.S
LET N; M | L | S |-> N; M | L | LET M.S

LAMBDA M; K | L | APPLY N.S |-> N | L | LET M.S
LAMBDA N; K | L | LET M.S |-> M | LAMBDA N.L | S



Γ |- M : T  Γ |- T : A
-----------------------
Γ |- [#typeof M] : A

Γ |- M : T
----------------------
Γ |- [#typeof M] |-> T


\n |-> VAR n
M N |-> APPLY M; N
λ. M |-> LAMBDA M
M[N] |-> LET M; N

VAR n; K | L | S |-> L<n> | L | S

APPLY M; N | L | S |-> M; N | L | N.S
LET M; N | L | S |-> M; N | N.L | S
LAMBDA M; K | L | N.S |-> M | N.L | S


LAMBDA N; K | L | LET M.S |-> M | LAMBDA N.L | S

Term (M, N, K) :=
  | \n | λ. M | M N
  | M[N] | M[↑ : n];

M[x] == N[x]
------------
λ. M == λ. N

((λ. t) v)[u]

(λ. t)[u] v[u]

((λ. t) v[u][↑])[u]
t[v[u][↑]][u]


t[v[u][↑]][u]

((λ. t) v)[u]
t[v][u]


Γ, A |- _T[↑] == N[B]
Γ, A, B |- _T[↑][↑] == N

Γ, A, B |- _T[↑][↑] := N
Γ, A |- _T[↑] := N[B]
Γ |- _T := N[B][A]


Γ, A, B |- _T[↑ : 1] == N


Γ, A |- _T[↑ : 1][B] == N[B]


M[↑ : 1][\0] == M


Γ, A, B |- _T[↑ : 1] := N



Term (M, N) :=
  | x | \n | λ. M | M N
  | M[N : n] | M[↑ : n];

(λ. M) N |-> M[N : 0] // beta

M[K : n] N |-> (M N[↑ : n])[K : n]
M N[K : n] |-> (M[↑ : n] N)[K : n]

M[N : m][↑ : n] |-> M[↑ : 1 + n][(N : m)[↑ : n]]

M[N[↑]] |-> M[N[↑]]


t[v[u][↑]][u]

t[v][u] == t[v[u][↑]][u]


Γ, A |- _A[↑] := N
Γ |- _A := N[A]

M[↑ : 1][A : 0]

N[A][↑] :=

Γ |- _A[↑ : 1] := N

Γ |- _A[↑ : 1][A : 0] := N[A : 0]

Γ |- _A[↑ : 1][A : 0] := N[A : 0]

M[↑] == N[↑]

Γ |- _A[↑ : 1][\0] := N[\0]

Γ |- _A[↑ : 1][A : 0] := N[A : 0]

Γ |- _A[↑ : 1] := N
Γ |- _A := N[\0]


_A[↑][A] == N[A]
_A == N[A]


_A == N[\0]

N[A][↑][A] == N[A]
N[A][↑] == N

N[\0][↑ : 1] := N

Γ |- _A := N[\0]

Γ |- N[\0][↑ : 1] := N



Γ |- _A[↑ : 1][\0] := N

M[↑ : 1][\0] == M

_A[↑ : 2][\0] := N[\0]
_A[\0] := N[\0][\0]

N[\0][\0][↑ : 1][A : 0] := N[A : 0]

L<M[x]> == R<N[x]>
------------------
L<λ. M> == R<λ. N>

((λ. t) v)[u]
t[v][u]

(λ. t)[u] v[u]
((λ. t) v[u][↑])[u]

(λ. t)[u] v[u]



t[v[u][↑]][u]
t[v][u]

(λ. t)[u] v[u]



Term (M, N) ::= \n | λ. M | M N

M N | A |-> M | N.A
λ. M | N.A |-> M{N} | A

Term (M, N) ::= \n | λ. M | M N | M[N];

(\n)@m | A | L |-> L<m - n> | A | L

(M[N])@m | A | L |-> M |

(M N)@m | N@m.A | L |->

M N | A | L |-> M | N.A | L

λ. M | N.A | L |-> M | ↑.A | N.L

M | ↑ | L |-> M |
M | ↑.N.A | L |-> M |


Term (M, N) ::= x | x => M | M N;

x | A | L | U |-> L<x> | A | L | x.U // subst
M N | A | L | U |-> M | N.A | L | @.U // apply
x => M | N.A | L | U |-> M | A | [x = N].L | β.U // beta


((λ. t) v)[u]
t[v][u]

(λ. t)[u] v[u]

((λ. t) v[u][↑])[u]


t[v][u] == t[v[u][↑]][u]

t[v[u] . u]
t[v[u] . u]

t[v[u][↑]][u]

_A[v][u] == _B[v[u][↑]][u]


Γ |- M
--------------
Γ |- M[A] == N

Term (M, N) ::=
  | x | \n | λ. M | M N
  | M[N] | M[close x];

(λ. M)[N]
(λ. M[y][N][close y])

M[x][...L] == N[x][...R]
------------------------------------------
M[x][...L][close x] == N[x][...R][close x]
------------------------------------------
(λ. M)[...L] == (λ. N)[...R]

Term (M, N) ::=
  | \n | λ. M | M N
  | M[N] | M[↑];


(λ. M)[N]
(λ. M[↓][N][↑])

_A[B] == C
_A[B] == C

\+1[N] |-> \0[N][↑] |-> N[↑]

\+1[↓][N][↑] |-> \0[N][↑] |-> N[↑]

\0[↓][N][↑]
\0[↓][N[↑]]

M[↓][N][K] |-> M


Γ, B |- _A[↑ : 1] == C
Γ, B |- _A[↑ : 1] == C


[B : 1] == [id . B]

Γ |- _A[id . ↑][B : 0] == C[B : 0]

Γ, B |- _A[id . ↑] == C
Γ, B |- _A[id . ↑] == C[]

Γ, B |- _A[↑ ] == C

_A[B] == C

Γ, B |- _A == C[\0 : 1]

[id . ↑] * -1 == [id . \0]

Γ |- _A[id . ↑][B] == C[B]


[↑ : 1][\0]

C[\0 : 1][↑ : 1] == C



x = y
f(x) = f(y)

x - 2 = y

f(z) = z + 2

f(x - 2) = f(y)
x - 2 + 2 = y + 2
x = y + 2


(+) : (x : Int) -> (y : Int) -> Int;

Int = Int
_A = Int

incr : (x : Int) -> Int
  = (x : Int) => 1 + x;

f = (x : Int) => incr x

_A = Int


Either = (A, B) =>
  | (tag == false, payload : A)
  | (tag == true, payload : B);

Either = (A, B) =>
  (tag : Bool, payload : tag |> | false => A | true => B);

ind : <P>(b : Bool, then : P true, else : P false) -> P b;

(x : Either(Int, String)) : String => {
  (tag : Bool, payload : tag |> | false => Int | true => String) = x;
  ind(tag,
    (payload : String) => payload,
    (payload : Int) => Int.to_string(payload),
  )(payload)
};


<P>(b : Bool, then : P true, else : P false) -> P b



_P true = (payload : Int) -> String
_P false = (payload : String) -> String
_P tag = (payload : tag |> | false => Int | true => String) -> String

_P = (b : Bool) => _R

((b : Bool) => _R) tag = (payload : tag |> | false => Int | true => String) -> String

_R[b := tag] = (payload : tag |> | false => Int | true => String) -> String
_R[b := tag][tag := b] = ((payload : tag |> | false => Int | true => String) -> String)[tag := b]



Term (M, N) ::=
  | \n | λ. M | M N
  | M[N] | M[↑];


_A[B] == C
_A == C[↑]

_A[x := B] == C

(x => _A) B == C
_A == C

_C[x := B] == _C
L<M> == R<N>


Term (M, N) ::= | x | x => M | M N | M[x := N];

_A[x := B] == _C
_A == _C
_C[x := B] == _C


Term (M, N) ::= x | x => M | M N | x = N; M;
Reduction (R) ::= x | @ | β | ζ;

x | A | L | R |-> L<x> | A | L | x.R // subst
M N | A | L | R |-> M | N.A | L | @.R // apply
x => M | N.A | L | R |-> x = N; M | A | L | β.R // beta
x = N; M | A | L | R |-> M | A | [x = N].L | ζ.R // zeta

(A; λ. M) N | | |
A; λ. M | N | | @
λ. M | N | A | ζ.@
M | | N.A | β.ζ.@

A; (λ. M) N | | |
(λ. M) N | | A | ζ
λ. M | N | A | @.ζ
M | | N.A | β.@.ζ

Term (M, N) ::= \n | λ. M | M N | M[N];
Receipt (R) ::= \n | @ | β | ζ;

// Expand |->
// Reverse <-|
\n | A | L | R <-|-> L<n>{↑n} | A | L | \n.R // subst
M N | A | L | R <-|-> M | N.A | L | @.R // apply
λ. M | N.A | L | R <-|-> M | A{↑} | N.L | β.R // beta
M[N] | A | L | R <-|-> M | A{↑} | N.L | ζ.R // zeta

// expanding
(λ. M)[K] N | | |
(λ. M)[K] | N | | @
(λ. M) | N{↑} | K | ζ.@
M[N{↑}] | | K | β.ζ.@
M | | N{↑}.K | ζ.β.ζ.@

// reversing but only zetas
M | | N{↑}.K | ζ.β.ζ.@
M[N{↑}] | | K | β.ζ.@
M[N{↑}] | | K | ζ.⇑β.@ // lift beta
M[N{↑}][K] | | | ⇑β.@

(λ. M[B])[A] | | |
λ. M[B] | | A | ζ
M[B] | | A | λ.ζ
M | | B.A | ζ.λ.ζ

M[ζB][λn][ζA]


(x : A) => (
  y = N;
  M
);
(x : A) -> B{y := N}

M[N[↑]][K]

Γ |- M ⇒ (x : A) -> B  Γ |- N ⇐ A
---------------------------------
Γ |- M N ⇒ x = N; B

(x => M) N

x = N; M

id = x => #debug x;
y = id 1;

(#debug M : #debug A)
(x => #debug M : #debug ((x : A) -> B))
((x => #debug M) : ((x : A) -> #debug B))


Γ |- M ⇒ (x : A) -> B  Γ |- N ⇐ A
---------------------------------
Γ |- M N ⇒ x = N; B


M[x := K] N ≡ (M N)[x := K]




x ∉ fv M
// not linear
(M N)[x := K] ≡ M N[x := K]
M[y := N][x := K] ≡ M[y := N[x := K]]

// stil linear
M N[x := K] |-> (M N)[x := K]
M[y := N[x := K]] |-> M[y := N][x := K]


received : (A : \-1) = \-1; (x : \-0) = \-1; (P : (z : \-1) -> \-4) -> (l : \-0 \-1) -> \-1 \-2,
expected : (P : (z : \-1) -> \-2) -> (l : \-0 \-2) -> \-1 \-3



((x => x x) N)[y := K]
(x => x x) N[y := K]
x = N[y := K];
x x
N[y := K] N[y := K]

((x => x x) N)[y := K]

V-LSC



Term (M, N) ::= x | x => M | M N | M[x := N]

L<x => M> N |-> L<M[x := N]>                        // beta
C<x>[x := L<N>] |-> L<C<N>[x := N]>                 // subst

M[x := N][y := K] ≡ M[y := K][x := N]   (x ∉ fv K) and (y ∉ fv N)
(x => M)[y := N]  ≡ x => M[y := N]      (y ∉ fv N)
M[x := K] N       ≡ (M N)[x := K]       (x ∉ fv N)
M N[x := K]       ≡ (M N)[x := K]       (x ∉ fv M)
M[x := N[y := K]] ≡ M[x := N][y := K]   (y ∉ fv M)


u = (z z)[z := y];
// LSC
u[x := u]
(z z)[z := y][x := u]
(z z)[z := y[x := u]]
(y[x := u] z)[z := y[x := u]]
(y[x := u] y[x := u])[z := y[x := u]]
(y y)[x := u][x := u][z := y[x := u]]
(y y)[x := u[x := u]][z := y[x := u]] // loop

// Edu's LSC
u[x := u]
(z z)[z := y][x := u]
(z z)[z := y[x := u]]
(y z)[z := y][x := u]
(y z)[z := y[x := u]]
(y y)[z := y][x := u]


(y z)[z := y][x := u]
(y y)[z := y][x := u]




((x => x x) N)[y := K]
(x x)[x := N][y := K] // beta
(N x)[x := N][y := K] // subst
(N N)[x := N][y := K] // subst

((x => x x) N)[y := K]
(x => x x) N[y := K]  // ≡
(x x)[x := N[y := K]] // beta
(N x)[x := N][y := K] // subst
(N N)[x := N][y := K] // subst


u[x := u]
(z z)[z := y][x := u]
(z z)[z := y[x := u]]
(z1 z2)[z1 := y[x := u]][z2 := y[x := u]]
(y[x := u])(y[x := u])
(y y)[x1 := u][x := u]
(y y)[x1 := u[x := u]]

(y y)[z := y][x := u]
(y y)[z := y][x := u]

Term (M, N) ::= x | x => M | M N | M[x := N]

L<x => M> N |-> L<M[x := N]>   // beta
C<x>[x := N] |-> C<N>[x := N]> // subst

M[x := N][y := K] ≡ M[y := K][x := N]   (x ∉ fv K) and (y ∉ fv N)
(x => M)[y := N]  ≡ x => M[y := N]      (y ∉ fv N)
M[x := K] N       ≡ (M N)[x := K]       (x ∉ fv N)
M N[x := K]       ≡ (M N)[x := K]       (x ∉ fv M)
M[x := N[y := K]] ≡ M[x := N][y := K]   (y ∉ fv M)


(y = N; x => M) K
y = N; x = K M


Γ |- M ⇒ (x : A) -> B  Γ; Δ |- N[↑Δ] ⇐ A
---------------------------------------
Γ |- M N ⇒ Δ; x = N



Term (M, N) ::= x | x => M | M N | x = N; M;

x | A | L |-> L<x> | A | L // subst
M N | A | L |-> M | N.A | L  // apply
x => M | N.A | L |-> M | A | [x = N].L // beta
x = N; M | A | L |-> M | A | [x = N].L // zeta


\1[A][↑][B]
\0[↑][B]
\1[B]
\0

\1[A]
\0

\1[↑ : 1][A][B]

\2[A][B]
\1[B]
\0


_A[↑ : 1][B][A] == C[B][A]
_A[B] == C[B][A]
[B] == C[B][A]

C[B][A][↑][↑ : 1][B][A] == C[B][A]
C[B][A][↑][B][↑][A] == C[B][A]
C[B][A] == C[B][A]

C[↑ : 1][B][B] == C[B]
C[↑ : 1][B][B] == C[B]
C[↑ : 1][B] == C[B]
C[B][↑][A] == C[B][A]
C[B] == C[B][A]

A, B |- _A[↑ : 1] == C
A, B |- _A[↑ : 1] == C
A |- _A[B][↑] == C[B]

_A[↑ : 1][B][A] == C[B][A]
_A[B][↑][A] == C[B][A]
_A[B] == C[B]

_A[B] == C
_A[B] == C[↑][B]
_A == C[↑]


M[↑ : 1][A][B] ≡ M[A][↑ : n][B]


M[↑ : 1][B][A] ≡ M[B[A]][↑ : 0][A]

_A[↑ : 1][B][A] == C[B][A]
_A[B[A]][↑][A] == C[B][A]
_A == C[B][A][↑]

C[B][A][↑][B[A]][↑][A] == C[B][A]
C[B][A] == C[B][A]

M[A][↑ : n][B] ≡ M[↑ : (1 + n)][A[B]][↑ : n][B]


_A[↑ : 1][B][A] == C[B][A]
_A[↑ : 1][B][↑ : n][A] == C[B][A]


M N |-> M[@N] // beta
C<x => M>[@N] |-> C<x = N; M>


(x = N; y => M) A B


(x = N; y => z => M) A B |-> (x = N; y => z = B; M) A


(x = N; y => M)[@K]
(x = N; y => M)[@K]


(λ. M)[@A]


M[↑ : 1][B][A] ≡ M[B[A]]
M[↑ : 2][B][A][K] ≡ M[B[A][K][↑]][A[K]]

M[↑ : 2][B[A][K][↑ : 0][↑ : 1]][A][K]
M[B[A][K][↑ : 0]][↑ : 1][A][K]

M[B[A][K][↑]][A[K]]

M[⇑⇑↑][B][A][K]


M[⇑↑][B][A] ≡ M[B[A]][↑][A]

M[⇑K][B][A] ≡ M[B[↑]][K][A]

M[⇑S][B][A] ≡ M[B[S⁻]][S][A]

M[↑ : 2][B][A][K] ≡ M[B[A[↑ : 0]]][↑ : 1][A][K]

M[B[A][↑ : 0]][↑ : 1][A][K]

M[B][A][↑ : 0][K]
M[B][↑ : 1][A[↑ : 0]][K]
M[↑ : 2][B[↑ : 1]][A[↑ : 0]][K]

M[C][↑ : 0][B][A]

M[↑ : 1][C][↑ : 0][B][A]

(x = N; y => (z = B; M)) A

↑ : n



(f x) => (f y)
(f x) => (f y)



-----------------------
Γ |- M¹ ⇒ Δ |- M² ⇒ A

Γ; x = b |- M ⇐ P b
-------------------
Γ |- x = b; M ⇐ P b

x = b;
ind : <P>(then : P true, else : P false) -> P x

x = b; ind : (x : b |> | true => _ | false => _) => _;

Γ |- M¹ ⇒ Γ; Δ |- M² : (x : A) -> B
Γ; Δ |- N¹ ⇐ Γ; Δ; Φ |- N² A
---------------------------------------------
Γ |- M N ⇒ Δ |- x = (↑Δ; N); B


Γ |- M ⇒ (x : A) -> B  Γ |- N ⇐ A
---------------------------------
Γ |- M N ⇒ B[x := N]







// example
M[A][B][C][↑ : 0]
M[A][B][↑ : 1][C[↑ : 0]]
M[A][↑ : 2][B[↑ : 1]][C[↑ : 0]]
M[↑ : 3][A[↑ : 2]][B[↑ : 1]][C[↑ : 0]]


((λ. t) v)[u]
(λ. t)[u] v[u]

t[v[u][↑]][u]
t[v][u]

v[↑ : 1][u][u]
v[u]

_A[↑ : 1][u][u] == _B[u]
_A == _B[u][↑]

_A[u] == _B[u]
_A[↑ : 1][u] == _B[u][↑]

M[↑][u] = M
_A[↑ : 1][u][u] == _B[u]


t[v[u][↑]][u] == t[v][u]
[u] |- t[v[u][↑]] == t[v][u][↑]
[u][v[u][↑]] |- t == t[v][u][↑2]
[u][v[u][↑]] |- t == t[v][↑2 : 1][u[↑2]]
[u][v[u][↑]][u[↑2]] |- t[↑] == t[↑2 : 2][v[↑2 : 1]]
[u][v[u][↑]][u[↑2]][v[↑2 : 1]] |- t[↑2] == t[↑2 : 2]

[u][v[u][↑]] |-

_A == _B[↑2 : 2][v[↑2 : 1]][u[↑2]]

[u][v[u][↑]][u[↑2]][v[↑2 : 1]] |-
_B[↑2 : 2][v[↑2 : 1]][u[↑2]][↑2] == _B[↑2 : 2]

[u][v[u][↑]] |-
_B[↑2 : 2][v[↑2 : 1]][u[↑2]] == _B[↑2 : 2][v[↑2 : 1]][u[↑2]]

[u][v[u][↑]][u[↑2]][v[↑2 : 1]] |- t[↑2] == t[↑2 : 2]

t[v[u][↑]][u] == t[v][u]

v[u][↑][u] == v[u]

_A[a][v[u][↑]][u] == _A[v][u]

_A[v[u][↑]][u] == _A[v][u]

[u][v[u][↑]][u[↑2]][v[↑2 : 1]] |- _T[a][↑2] == _T[↑2 : 2]
[u][v[u][↑]][u[↑2]][v[↑2 : 1]][a] |- _T[↑2 : 1] == _T[↑2 : 2]

[u][v[u][↑]][u[↑2]][v[↑2 : 1]][a] |- \1[↑2 : 1] == \1[↑2 : 2]
[u][v[u][↑]][u[↑2]][v[↑2 : 1]][a] |- \3 == \1
[u][v[u][↑]][u[↑2]][v[↑2 : 1]][a] |- \2 == \0
[u][v[u][↑]][u[↑2]] |- \1 == v[↑2 : 1]
[u][v[u][↑]] |- \0 == v[↑2 : 1][u[↑2]]

v[u][↑][u] == v[↑2 : 1][u[↑2]][v[u][↑]][u]
v[u][↑][u] == v[u][↑2][v[u][↑]][u]
v[u] == v[u]


_T[↑2 : 1] == _T[↑2 : 2][↑]

[u] |- t\(1 + n)[a][v[u][↑]] == t\n[v][u]

t\(1 + n)[a][v[u][↑]] == t\n[v][u]

v[u][↑][u] == v[u]  u == u
--------------------------
_A[v[u][↑]][u] == _B[v][u]


v[u][↑][u] == v[u]

\1[v][u]


Term ::= \n | λ. M | M N | M[N] | M[↑b : d]

L<(λ. M)> N |-> L<M[N[↑L : 0]]>

// substs
C<\#>[A] |-> C<A[↑# : 0]>[A]
M[A] N |-> (M N[↑])[A]
M[A[B]] |-> M[↑ : 1][A][B]

// shifts
\n[↑b : d] |-> \n        (d > n)
\n[↑b : d] |-> \(b + n)  (d <= n)

(λ. M)[↑b : n] |-> λ. M[↑b : 1 + n]
(M N)[↑b : n] |-> M[↑b : n] N[↑b : n]
M[A][↑b : n] |-> M[↑b : 1 + n][A[↑b : n]]


M[C][B][A][↑] | |
M[C][B][A] | [↑ : 0] |
M[C][B][A] | [↑ : 0] |


Term ::= \n | λ. M | M N | M[N : d] | M[↑b : d]



(#debug M : #debug A)
(x => #debug M : #debug ((x : A) -> B))
((x => #debug M) : ((x : A) -> #debug B))

#normalize (#debug M : #debug A)
#normalize (x => #debug M : #debug ((x : A) -> B))
#normalize ((x => #debug M) : ((x : A) -> #debug B))

Term ::=
  | x
  | (x : A) => M
  | (x : A) -> M
  | M N
  | x = N; M;

(L<M> : R<A>)


((x : A) = N; y => y) : (y : Int) -> Int

(((x : A) => y => y) N) : (y : Int) -> Int


Γ |- N ⇒ A  Γ, x = N |- M ⇐ T
-----------------------------
Γ |- x = N; M ⇐ T

Γ |- M ⇐ (x : A) -> T  Γ |- N ⇐ A
---------------------------------
Γ |- M N ⇐ T


Γ |- N ⇒ A  Γ, x = N |- M ⇐ T
-----------------------------
Γ |- x = N; M ⇐ _A[x := ]


ind : (b : Bool) -> <P>(then : P true, else : P false) -> P b;

ind b (_, _) : (x : T b) -> _;

_P b == (x : T b) -> _
_P[x := b] == (x : T b) -> _

Γ |- M ⇐ (x : A) -> T  Γ |- N ⇐ A
---------------------------------
Γ |- M N ⇐ T



Term ::=
  | (M : A) | Type | x | x : A = N; M
  | (x : A) => M | (x : A) -> M | M N
  | x = N; M | x => M;

B ≡ A
------------------------- // check
((M : B) : A) |-> (M : A)

L<(x : A) => M> N |-> L<x = (N : A); M>   // beta
x : A = N; C<x> |-> x : A = N; C<(N : A)> // subst

(x : A) => C<x> |-> (x : A) => C<(x : A)>
(x : A) -> C<x> |-> (x : A) -> C<(x : A)>
x : A = N; C<x> |-> x : A = N; C<(x : A)>

Type |-> (Type : Type)
(x : A) -> B |-> (x : (A : Type)) -> (B : Type)

(M : (x : A) -> B) N |-> (M N : (x : A = N; B))

x = (N : A); M |-> x : A = N; M // infer let
(x => M : (x : A) -> B) |-> (x : A) => (M : B) // infer lambda
(M (N : A) : B) |-> ((M : (x : A) -> B{N := x}) N) // higher order



x = N; M


Γ; x =  |- M : A
-------------------------
Γ; x = N |- M :


↑
Γ |- M ⇒ T  Γ |- N ⇐ T.param
----------------------------
Γ |- M N ⇒ T.return



incr = x => #debug 1 + x;
incr "a"


M[N : d][A][B][C]

\0[A[↑]]
\0[A[↑]]

⇑


1 + 2 = 3





3 = 3


0
1 + n

0 = 0
1 + 0 = 1
1 + 1 + 0 = 2
1 + 1 + 1 + 0 = 3
1 + 1 + 1 + 1 + 0 = 4




0 + 1 = 1
1 + 1 = 2 = 0
mod2 : 0 == 2

0 + 1 = 1
1 + 1 = 2
2 + 1 = 3 = 0
mod3 : 0 == 3

true = not false
true = true



Term ::=
  | Type
  | Data
  | x
  | (x : A) -> B
  | (x : A) => B
  | M N;

refl : ∀x. x == x;
univalence : ∀x y. x =~= y -> x == y;
uip_refl : ∀x. ∀eq : x == x -> eq == refl;

Data : Type
Type : Type

S = <A, B>(x : A, y : B) -> A;
T = <A, B>(x : A, y : B) -> B;

(n : Nat) => ()

\0 |-> \0[A]


Γ |- α : Size
Γ |- α : Size
--------------
Γ |- M ⇒ A $ α

Γ |- M ⇒ B $ β
Γ |- β < α  Γ |- B ≡ A
----------------------
Γ |- M ⇐ A $ α

--------------------
Γ |- Type ⇐ Type $ s

-------------------------
Γ, x : A $ s |- x : A $ s

Γ |- A : Type $ sA
Γ, x : A $ s |- B : Type $ sB
----------------------------------------
Γ |- (x : A $ s) -> B : Type $ sA + sB

Γ |- A : Type $ A_s
Γ, x : A $ s |- B : Type $ B_s
Γ, x : A $ s |- M : Type $ M_s
----------------------------------------------------------
Γ |- (x : A $ s) => M : (x : A $ s) -> B $ 1 + M_s

Γ |- M : (x : A $ s) -> B $ sM  Γ |- N ⇐ A $ s
-----------------------------------------------
Γ |- M N : B{x := N} $ sM

---------------------------------------
Γ |- ((x : A $ s) => M) N |-> M{x := N}



λ. M[A[↑]] |-> (λ. M)[A]
λ. M[A] |-> (λ. M)[A[↓]]
```

## System U

```rust
S = { ∗, , ∆ }
A = { (∗, ),(, ∆) }
R = {
  (k, ∗, ∗, ∗),
  (k, □, □, □),
  (e, □, ∗, ∗),
  (e, ∆, □, □) | k ∈ {n, e}
}

℘ S === (x : S) -> Type 0;

U : Type 2 = (X : Type 1 $ 0) -> (f : ℘℘X -> X) -> ℘℘X;
τ (t : ℘℘U) : U = (X : Type 1 $ 0) => (f : ℘℘X -> X) => (p : ℘X) =>
  t ((x : U) => p (f (x X f)));

σ (s : U) : ℘℘U = s U τ;
Ω : U = τ ((p : ℘U) => (x : U) -> σ x p -> p x);

℘ S === (x : S) -> Type 0;

U : Type 1 = (X : Type 1) -> (f : ℘℘(Type 0) -> X) -> ℘℘X;
τ (t : ℘℘U) : U = (X : Type 1) => (f : ℘℘X -> X) =>
  (p : ℘X) => t ((x : U) => p (f (x X f)));

σ (s : U) : ℘℘U = s U τ;
Ω : U = τ ((p : ℘U) => (x : U) -> σ x p -> p x);

tapp : (K : Kind) -> (T : (α : K) -> Type) ->
  (e : (α : K) -> T α) -> (t2 : K) -> T t2;

-------------
(x : A) ->

Γ |- A : Kind  Γ, x : A, • |- B : Type
--------------------------------------
Γ, • |- (x : A) -> B : Type

Γ |- A : Type _  Γ, x : A, • |- B : Type l
------------------------------------------
Γ, • |- (x : A) -> B : Type l

(x : A) => B

Id : Type 0 = (A : Type 0 : Type 1) -> (x : A : Type 1) -> A


// level universe, no computing(maybe???)
Size : USize;
zero : Size;
(1 +) : (pred : Size) -> Size;
max : (left : Size) -> (right : Size) -> Size;
ω : Size;

// type universe
Γ |- l : Size
--------------------------
Γ |- Type l : Type (1 + l)

// traditional functions
Γ |- A : Type a  Γ, x : A |- B : Type b
---------------------------------------
Γ |- (x : A) -> B : Type (max a b)

// connect the universes
Γ, x : USize |- B : Type ω
--------------------------
Γ |- (x : A) -> B : Type ω

Γ, x : Size |- B : Type s
----------------------------------------
Γ |- !(x : Size) -> B : Type (s[x := 0])



Γ, x : Size |- B : Type α
----------------------------------------
Γ |- !(x : Size) -> B : Type β


Γ, x : Size |- B : Type s
----------------------------------------
Γ |- !(x : Size) -> B : Type (s[x := 0])


// example
Id : (x : Size) -> Type x : Type 1;
Id : Type 1 = (x : Size) -> (A : Type x) -> (v : A) -> A;



```

## De-bruijn

```rust
Term ::= | \n | λ. M | M N | M[↑]


l |- (λ. \(2 + l) \(1 + l))[↑][A][B]
l |- (λ. \(2 + l) \(1 + l))[↑][A][B]
l |- (λ. \(2 + l) \(l))[A][B]

l |- (λ. \(1 + l) \( l))[B]


(λ. \(2 + l) \(1 + l))[↑][A][B]


M[↑d : b]

λ. M[A] |-> (λ. M)[A]


Term ::=
  | (M : A)
  | x
  | (x : A) -> B
  | (x : A) => M
  | M N
  | x : A; M
  | x = N; M

Mtree -> JS

Mtree + Htree -> Ttree

Term (M, N) ::=
  | (x : S) => M
  | (x : S) -> M
  | x : S; M
  | x = S; M
  | S
Simple (S, T) ::=
  | x
  | S T
  | M // close


M |-> ((x => M) x)[η]
((x => M) x)[η]


x => (
  A = B;
  (y : A) => M
)



free vars + highest free var
free meta vars + highest meta var
injectivity? variance?

escape + occurs
head expand + equal

String -> Bytecode + Constraint



x => M
ctx =>
  load ctx;
  x => M



Block (B) ::=
  | ANNOT S; LAMBDA; B
  | ANNOT S; FORALL; B
  | LET S; B
  | S
Simple (S, T) ::=
  | GLOBAL x
  | LOCAL n
  | APPLY T; S;
  | BLOCK; B; END_BLOCK

(λ. M) |-> LAMBDA; M
(M N) |-> M; APPLY; N

f = λ. M; f N

f = λ. λ. M;

f ctx N

M; N; CALL;


(1)(console.log("a"))

CALL; N | E | P.S |-> P | E | N.S



f x y == f x

LAMBDA B; P | E | S |-> P | E | LAMBDA B.S



LOCAL n; P | E | S |-> P | E | E(n).S
LOAD n; P | E | C.S |-> P | C(n).E | S

CONTEXT n; P | C | E | S |-> P | C | E | C(n).S
LOCAL n; P | C | E | S |-> P | C | E | E(n).S
STORE n; P | C | E | S |-> P | E(n).C |


LAMBDA B; P | C | E | S |-> P | C | E | LAMBDA B.S
APPLY; A | C | E | LAMBDA B.S |-> B | C | A.C.E | S

CAPTURE; P | C | E | S |-> P | E | | CAPTURE.C.S

LAMBDA _; P | C | E | A.S |-> P | C | A.E | S


// TODO: thunk for lets in a pure setting?

Name solving +
Constraint generation +
Closure conversion +
Bytecode generation

Term (M, N) ::=
  | x
  | x => M
  | M N
  | x = N; M
  | A
Type (A, B) ::=
  | x
  | (x : A) -> B
  | M


Block (B) ::=
  | x => B
  | (x : S) -> B
  | x = S; B
  | S
Simple (S, T) ::=
  | x // global
  | \n // local
  | B // closure
  | S T

x => M |-> LAMBDA; M
(x : A) -> B |-> FORALL |A|; A; B


Term (M, N) ::=
  | _X // hole
  | x // var
  | (x : A) => M // lambda
  | (x : A) -> B // forall
  | M N // apply
  | x = N; M // let
  | (M : A) // annot

Parser -[Compile]> Bytecode + Constraint + Notes
Bytecode + Constraint -[Check]> Substituion

Parser -[Compile]> Typed Tree + Constraint + Notes
Typed Tree + Constraint -[Check]> Substituion

Bytecode + Notes -> Typed Tree

Term (M, N) ::=
  | _X
  | x
  | x => M
  | (x : A) -> B
  | M N
  | x = N; M


M N |-> M; APPLY; N
        (typeof M).param == (typeof N)

(x => M : (x : A) -> B) == (y => N : (x : C) -> D)
((x : A) -> B : Type) == ((x : C) -> D : Type)


Term ::=
  | ACCESS x
  | FORALL
  | LAMBDA

_A == x => M
_A == LAMBDA; M


HOLE _A |->

λ. λ. M
λ. λ. N

(x => y => x) == (x => y => y)

(x => M) A |-> x = A; M

LAMBDA; LAMBDA; M | E | S | P |-> M | [•].[•].E | S | λ.λ.P
LAMBDA; LAMBDA; N | E | S | P |-> N | [•].[•].E | S | λ.λ.P

PC | LAMBDA; LAMBDA; ACCESS 1 |
1 + PC | LAMBDA; ACCESS 1 | λ(1 + PC)
2 + PC | ACCESS 1 | λ(2 + PC).λ(1 + PC)
3 + PC | | \1.λ(2 + PC).λ(1 + PC)

LAMBDA; LAMBDA; ACCESS 1 |
LAMBDA; ACCESS 1 | λ
ACCESS 1 | λ.λ
| \1.λ.λ


x N K
_A N K
_A K
_A




K.N.\nx.@.@ == K.N._A.@.@


K.N.x.@.@ == K._A.@

x _A K
x N _A

K.N.x
_A


_A == ((x : A) -> B)
_A == FORALL |A|; A; B

_A == A; FORALL; B


GLOBAL x; CALL; N; GLOBAL x_capture; CALL;


Term ::=
  | Type
  | x
  | (x : A) -> B
  | x => M
  | M N
  | x : A; M
  | x = N; M
  | (x : A) & B

Term ::=
  | x
  | x => M
  | M N
  | x = N; M
  | (x : A) -> B;

(x = N; M) K

Bytecode (B) ::=
  | VAR \n
  | LAMBDA
  | APPLY | RETURN
  | LET | DROP
  | FORALL

Constraint (C) ::=
  | true
  | false
  | C¹ && C²
  | M == N
  | ∀(x : A); C
  | ∃(α : A); C
  | x == M; C

LAMBDA;

M N : A

M; N; APPLY; A;

A.@.N.M

// TODO: for infer rules, store produced and expected type

T(x, A) ==
  A
T(x => M, (x : A) -> B) ==

T(M N, T) ==
  A = hole();
  B = hole();
  (M, C_M) = T(M, (x : A) -> B);
  (N, C_N) = T(N, A);
  (M; N; APPLY, ∃A; ∃B; C_M && C_N)

T(M : A, B_B) ==
  (B_A, C_A) = T(A, TYPE);
  (B_M, C_M) = T(M, B_A);
  (B_M, C_A && B_A == B_B && C_M)

((x : A) => M) == A; LAMBDA; M


APPLY |N|; M; N;

LAMBDA; PC | E | S |-> PC

APPLY; PC | E | λM.N.S | C |-> M | N.E | PC.S | C; BETA
APPLY; PC | E | S | C |-> PC | E | S | C; APPLY




Name solving +
Constraint generation +

Closure conversion +
Scope analysis, free vars, closed terms ... +
Bytecode generation

Codegen for free vars

Γ; L |- M | [↑L]S1 ⇐ N | S2
--------------------------
Γ |- M | S1 ⇐ L<N> | S2

Declaration (D) ::=
  | (x => D) : T
  | (x = E; D) : T
  | E : T

Expression (E, T) ::=
  | x | S T;

Term (M, N) ::=
  | x
  | (M N : A)
  | (x => M : A)
Typed (T) ::=
  | (x : A)
  | (T N : A)
  | (x => T : A)


2 PASS

Value (V) ::=
  | x ...VS
  | x => V;y

bv V
  | (x ...VS) => {x} + bv VS
  | (x => V) => bv V;

CPS

[↑]

M[A][B][↑ : 0][C]
M[A][↑ : 1][B[↑ : 0]][C]
M[↑ : 2][A[↑ : 1]][B[↑ : 0]][C]

\1[A[↑ : 1]][B[↑ : 0]][C]
A[↑ : 1][↑ : 0][A[↑ : 1]][B[↑ : 0]][C]

[↑b : d]; M
A[↑ : 1][↑ : 0][A[↑ : 1]][B[↑ : 0]][C]


Nat : (s : Size) -> Type =
  | Z : [a] -> Nat a
  | S : [a] -> [b < a] -> (pred : Nat b) -> Nat a;

fold : [s] -> (n : Nat s) -> <A>(z : A, s : (acc : A) -> A) -> A;

fold : [s] -> (n : Nat s) -> <A>(z : A, s : (acc : A) -> A) -> A;
fold [s] =
  fold : [s2 < s] -> (n : Nat s2) -> <A>(z : A, s : (acc : A) -> A) -> A

Type b : Type a  [b < a]

Type l : Type (1 + l)

id = (A : Type 0) => (x : A) => x;

((id : (A : Type 0) -> (x : A) -> A) =>
  M);
⇑id ((A : Type 0) -> (x : A) -> A ) id

Type b : Type a  b < a

Type : Type. Type;
Type = Type;

Type : Type. [l] -> Type (1 + l)
Type = l => Type l;


((A : Type) => (x : A) => x)
((A : x.x) => (x : A) => x)

x & x == Type

Type = Type. Type;

Type =
  | Self (T : (x : x. T) -> Type)
  | Forall (A : Type) (B : (x : A) -> Type);

Type : Type;
(x. _) : (T : (x : x. T) -> Type) -> Type;
((x : _) -> _) : (A : Type) -> (B : (x : A) -> Type) -> Type;

Type = T.
  (P : (T : Type) -> Type) ->
    (Self : (T : (x : x. T) -> Type) -> P (x. T)) ->
    (Forall : (A : Type) -> (B : (x : A) -> Type) ->
      P ((x : A) -> B x)) ->
  P T;
(x. _) = T => P => Self => Forall => Self T;
((x : _) -> _) = A => B => P => Self => Forall => Forall A B;

(x =>) = A => B

x | x => M | M N

(x => M) N |-> M{x := N}

Type : Type;
Data : Type;
Prop : Type;
Line : Type;

univalence : <A, x, y>(iso : x =~= y) -> x ==
Data = Type $ 0;

(A : Type $ 0) => A

id = (A : Data) => (x : A) : A => x;

Eq<A>(x, y) = (P : (z : A) -> Type) -> P x -> P y;
refl<A>(x) = (P : (z : A) -> Type) => (v : P x) => v;

<A, B>(x : A, y : B) -> B
<A, B>(x : A, y : B) -> A

((x : A) -> B)

id = (A : Type) => (x : A) =>
  A (x => x) (T => _) (A => B => _);

(x => x) A

Id<Type> ==
Id<x. T>(l, r) ==
  ???(x : x. T) -> Id<T>(l, r);
Id<(x : A) -> B>(f0, f1) ==
  (x0 : A) -> (x1 : A) -> (eq_x : Id<A>(x0, x1)) -> Id<B>(f0 x0, f1 x1);

Id<(x : A, y : B)>(p0, p1) == (
  eq_x : Id<A>(x0, x1),
  eq_y : Id<A>(y0, y1)
);
Id<(x : A) -> B>(f0, f1) ==
  (x0 : A) -> (x1 : A) -> (eq_x : Id<A>(x0, x1)) -> Id<B>(f0 x0, f1 x1);

Γ, x : x. T |- T ⇐ Type
-----------------------------
Γ |- x. T ⇒ Type

Γ |- M ⇒ T[x := M]
------------------
Γ |- M ⇐ x. T

Γ |- M ⇒ x. T
------------------
Γ |- M ⇐ T[x := M]

Γ, x : A |- B : Type
------------------------------
Γ |- (x : A) -> B : Type

Γ, x : A |- M : B
---------------------------------
Γ |- (x : A) => M : (x : A) -> B

Γ |- M : (x : A) -> B  Γ |- N ⇐ x. A
------------------------------------
Γ |- M N : B[x := N]


T_Unit : Type;
T_unit : (Unit : T_Unit) -> Type;

T_Unit = (Unit : T_Unit) & (unit : T_unit Unit) -> Type;
T_unit = Unit => T_unit Unit;

Unit : Type;
unit : Unit;

Bool : Type;
true : Bool;
false : Bool;


(x : A) =>

((x : Bool) => M) ((P : (b : Bool) -> Type) => (x : P true) => (y : P false) => y);


ind_bool = (b : Bool) ->
  (P : (b : Bool) -> Type) ->
  (x : P true) -> (y : P false) -> P b;

Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool) & (P : (b : Bool) -> Type) -> (x : P true) -> (y : P false) -> P b

Γ, x : x & x |- x : y & y
------------------------
Γ |- x & x : y & y

Type = Type & Type;
Type : Type

Γ |- (x : A) -> B : Type

(A : Type. Type) => (x : A) => A;

Type : Type;
Type = Type;

(x : Type) & Int :> (x : Type) & x


x : A;


----------------
Γ |- Type : Type

-------------------------------------
Γ |- Type ≡
  (P : (T : Type) -> Type) ->
  (forall : (A : Type) -> (B : (x : A) -> Type) -> P ((x : A) -> B)) ->
  P Type

Type =
  T.
  (P : (T : Type) -> Type) ->
  (forall : (A : Type) -> (B : (x : A) -> Type) -> P ((x : A) -> B)) ->
  P T

Π(x : A). B ≡
  (P : (T : Type) -> Type) =>
  (Self : ())

#(↑b : d; M)

Type : T. T;
Type =
  (P : (T : Type) -> Type) =>
  (univ : P Type)
  (self : (T : (x : A) -> Type) -> ) =>
  (forall : (A : Type) -> (B : (x : A) -> Type) -> P ((x : A) -> B)) =>
  univ;

Type : Type
Type : T. T
Type : Type

Type [a] : Type [1 + a] =
  | Univ : [b < a] -> Type a
  | Self : ;

Type 0 : Type 1
Univ 1 0 : Type 1

Bool : [a] -> Type;
true : [a] -> Bool a;



Type = [a] => [b < a] -> Type b;

(A : Type 1) => (x : A)

Type b : Type a  [b < a]

Type 0 : Type 1

----------------
Γ |- Type : Type

Type : Type. [a] -> Type (1 + a);
Univ : [a] -> Type a;
Self : [a] -> (T : (x : x. A) -> Type a) -> Type a;
Forall : [a] -> (A : Type a) -> (B : (x : A) -> Type a) -> Type a;

Type [a] = T. (P : (T : Type a) -> Type a) ->
  (univ : P (Univ a)) ->
  (self : (T : (x : x. A) -> Type a) -> P (Self a T)) ->
  (forall : (A : Type a) -> (B : (x : A) -> Type a) -> P (Forall a A B)) ->
  P T;
Univ [a] = P => univ => self => forall => univ;
Self [a] T = P => univ => self => forall => self T;
Forall [a] A B = P => univ => self => forall => forall A B;


(x : )

(M N : A)

Sort ::=
  | Type
  | Prop

Term (M, N)
Type (A, B) ::=
  | x
  | (x : A : S) -> B
  | x => M
  | T N

Typed (T) ::=
  | x
  | (x => M : (x : A : S) -> B : S);

Term ::=
  | VAR
  | PAIR
  | ARROW
  | APPLY
  | NOP




HOLE; 'b; VAR; t; APPLY;
HOLE; 'b; HOLE; 'a; ARROW; ARROW;
HOLE; 'a; VAR; t; APPLY; ARROW;

bind : t 'a -> ('a -> 'b) -> t 'b;
[t]['a]['b]; \3 \2 -> (\2 -> \1) -> \3 1

96
// header
t; 'a; 'b;
// structure
VAR 1; VAR 1; APPLY;
VAR 1; VAR 2; ARROW; ARROW;
VAR 2; VAR 3; APPLY; ARROW;


M[N]

M N


Value (V) ::=
  | x ...V
  | x => E

Expr (E) ::=
  | V
  | x = V; E // let
  | x = V1 V2; E // apply

Expr (E) ::=
  | V
  | x = V; E // let
  | x = V1 V2; E // apply

Term (M, N) ::=
  | n
  | M N
  | λ. M
  | M[N]

λ. M |-> LAMBDA M

VAR n;
M; N; APPLY;
LAMBDA (M; RETURN);
N; LET; M; END_LET;

V1; V2; APPLY; E

M; N
N; LET; M

APPLY; PC | E | S |-> PC | E | @.S
LAMBDA; PC | E | S |-> PC | E | S

x : A = N; M

((x : A) => M) N |-> β x : A = N; M // beta

β x : A = N; M |-> ((x : A) => M) N // rev-beta

((x : A) => M) N |-> x : A = N; M // beta
x : A = N; C<x> |-> x : A = N; C<ζx N> // subst
x : A = N; M |-> # x : A = N; M    x ∉ fv M  // gc

String ->
  Naming solving +
  Bytecode generation +
  Constraint generation

Bytecode ->
  JS / Assembly

2 PASS


Term ::=
  | x | x => M | M N | x = N; M
  | #x; M | #β; M | #ζ; M
  ;

Instr ::=
  | VAR n
  | LAMBDA M
  | APPLY | RETURN
  | LET | END;

(λ. M) N |-> M[N][#β]

(λ. f \0) N |-> (f \0); [N][#β]

BETA; N; LET; M; END;

N; LET; VAR f; VAR 0; APPLY; END;

VAR f; N; APPLY
N AT 2; \0 AT 1

VAR f; N; SUBST 0; APPLY; GC N

@.N.f
N; LET; VAR f; VAR 0; APPLY; END;



#x; M |-> x

x = M; x |-> x = M; #x; M

β; x = M

// (x : A) -> T

Γ, A : Type |- B : Type
-----------------------
Γ |- @(x : A) -> B : Type

Γ, x : A |- M : B
----------------------------------
Γ |- @(x : A) => M : @(x : A) -> B

Γ |- M : A  Γ |- A ≡ @(x : A) -> B
--------------------------------
Γ |- @M : B[x := M]


true : Bool true;
false : Bool false;






roll : <A, B>(M : (x : A) -> B x) -> Self A B;




Bool : Type;
true : Bool;
false : Bool;

Bool = Self Bool (self => <P>(then : P true, else : P false) -> P self);

true = (self : Bool) => (eq : true == self) =>
  <P>(then : P true, else : P false) => eq P then;
false = (self : Bool) => (eq : false == self) =>
  <P>(then : P true, else : P false) => eq P else;

Bool = Self Bool (b => <P>(then : P true, else : P false) -> P b);
true : Bool = roll(b => (then : P b, else : P false) => then);

inst : <A, B>(M : A) -> (eq : A == Self A B) -> B M;

inst = <A, B>(eq : A == Self A B) => (M : A) => eq Id M M;

Self : <A>(B : (x : A) -> Type) -> Type;
Self = <A>(B : (x : A) -> Type) => (x : A) -> B x;

False : Type;
False = Self(f => (P : (f : False) -> Type) -> P f);

T_Unit : Type;
Unit : Type;

T_unit : Type;
unit : T_unit;

T_Unit =
  Self((Unit : T_Unit) => (unit : T_unit Unit) -> Type);



Unit =
  Self((u : Unit) => (P : (u : Unit) -> Type) -> (x : P unit) -> P u);

T_unit = Unit =>
  Self((unit : T_unit Unit) =>
    (I_unit : T_I_Unit Unit unit) => Unit Unit unit);
T_I_unit = Unit => unit =>
  Self((I_unit : T_I_Unit Unit unit) => _);



unit : T_unit Unit;
unit = (u : T_unit Unit) => (I_unit ) =>
  (P : (u : Unit) -> Type) => (x : P unit) => I_unit I_unit P x

Unit = @(Unit : Type) -> (unit : @unit)


T_Unit0 : Type;
T_unit0 : Type;
T_u : Type;

Unit0 : T_Unit0;
unit0 : T_unit0;

T_Unit0 = (u : T_u) -> Type;
T_unit0 = Unit0 unit0;
T_u = Self(Unit);
Unit0 = u => (P : (u : Self(Unit)) -> Type) ->
  (x : P (unit unit)) -> Self(Unit);


T_unit : Type;
unit : T_unit;



T_Unit = ()
unit : Unit unit;

A ≡ B[x := M]



Self : <A>(B : (x : A) -> Type) -> Type;

roll : <T, A, B>(M : T) ->
  (eq :
    T_eq : Type;
    T_eq = T == (eq : T_eq) -> B (roll M eq);
    T_eq) -> Self<A>B;
Self = <A>(B : (x : A) -> Type) =>
  <T>(x : A) -> (eq0 : A == Self B) ->
  (M : T) ->
  (eq : x == roll )
  B x;

Unit : Type;
T_unit0 : Type;
unit0 : T_unit0;
unit : Unit;

Unit = Self((u : Unit) => (P : (u : Unit) -> Type) -> P unit -> P u);

T_unit0 = (P : (u : Unit) -> Type) -> P unit0 -> P unit0;
unit0 = roll<T_unit0, Unit, _>((P => x => x) : T_unit0);

unit = unit0 refl;


Unit = _;
unit = u => I => P => x => I P x;


@(f). (P : (f : False) -> Type) -> P f;
@(x). T
Self : (T : (x : Self T) -> Type) -> Type;
Self : Self(Self => (T : Self(T => (x : Self T) -> Type)) -> Type);



T_Self : Type;
T_Self_T0 : Type;
T_Self_T1 : Type;
Self : T_Self;


T_Self = (T : T_Self_T0) -> Type;
T_Self_T0 = Self()(x : T_Self_T1) -> Type;
T_Self_T1 = Self()

T_Self : Type;
T_Self_T : (A : Type) -> Type;
Self : T_Self;


T_Self = (A : Type) -> (T : T_Self_T A) -> Type;
T_Self_T = A => (x : A) -> (eq : A == Self A B) Type

T_Self : Type;
T_Self_T_eq : (A : Type) ->  Type;
Self : T_Self;

T_Self = (A : Type) ->
  (T : (x : A) -> (eq : T_Self_T_eq A x) -> Type) -> Type
T_Self_T = (x : A) -> (eq : A == Self A T) -> Type
Self : (A : Type) ->
  (T : (x : A) -> (eq : A == Self A T) -> Type) -> Type;


Γ, x : @(x) -> T |- T : Type
----------------------------
Γ |- @(x) -> T : Type

Γ, x : @(x) -> T |- M : T
--------------------------
Γ |- @(x) => M : @(x) -> T

Γ, x : A |- B : Type
-------------------------
Γ |- @(x : A) -> B : Type

Γ, x : A |- M : B  Γ |- A ≡ @(x : A) -> B
-----------------------------------------
Γ |- @(x : A) => M : @(x : A) -> B

Γ |- M : A  Γ |- A ≡ @(x : A) -> B
----------------------------------
Γ |- @M : B[x := M]


Self : (A : Type) -> (B : (x : A) -> Type) -> Type;
fix : <A, B>(M : (x : A) -> B x) ->
  (eq : A == Self A B) -> Self A B;
unroll : <A, B>(M : A) -> (eq : A == Self A B) -> B M;

unroll = <A, B>(M : Self T) => (eq : A == Self A B) -> B M
Self
fix = <A,B>M => eq =>
Self : (T : T_Self_T) -> Type;

T_Self_T = (x : T_Self_T) -> Type;


Unit : Type;
unit : Unit;

Unit = (u : Unit) ->
  (P : (u : Unit) -> Type) -> (x : P unit) -> P u;

unit0 : Unit0 unit;
unit0 = (u : Unit) => (eq : u == unit) =>
  P => x => eq P x;

Unit = Unit0

Self = T =>





Decl (D) ::= x => M | V
Value (V) ::= x ...V
Term (M, N)
Type (A, B) ::=
  | (x : A : S) -> B
  | x : A : S = D; M
  | x ...N

x = N; M

(x : A) => y = N; M |->
y = (x : A) => N;
(x : A) => y = y x; M
C<x => M> |-> z = x => M; C<z>

((x : A) -> B) : Line

f : (x : Nat, y : Nat) -> Nat;
f : (x $ 1) -> (y $ 2) -> (z $ 1);

(x => V) N |-> x = z = V; z

Sort (S) ::= | Type | Data | Prop | Line;

Term (M, N)
Type (A, B) ::=
  | (x : A) -> B
  | x = M; M
  | M N
  | (x : A) => M

Constraint (C) ::=
  | true
  | false
  | C¹ && C²
  | M == N
  | ∀(x : A); C
  | ∃(α : A); C
  | x == M; C

C¹ && C² |-> C¹; C²; AND

Sort ::= USize | UType
Term ::=
  | []


----------------
Γ |- M & N :

Γ |- A : Type  Γ |- B : Type
----------------------------
Γ |- A | B : Type

Γ |- A : Type  Γ |- M : A  Γ |- N : A
-------------------------------------
Γ |- M | N : A


(M : String) | (N : Int)
(M : String | Int) | (N : String | Int) : String | Int

(M : String) | (N : Never) |-> (M : String)

String_or_never = b => b |> | true => String | false => Never;

f : (b : Bool) -> String_or_never b;

(f true : String | f false : Never) |-> f true

x : Int = f 0 | f 1 | f 2;
y : Option Bool =
  x |>
  | 0 => ("some", true)
  | 1 => ("some", true)
  | _ => ("none", ());

IO : (A : Type) -> Type;
DB : (A : Type) -> Type;

f : () -[IO | DB]> ()

(IO | DB) String
(A => IO | A => DB) String
(A => (IO A | DB A)) String
(IO String | DB String)

(x => M) | (y => N)

M[N] |-> M[↓]  0 ∉ fv M  // gc

(λ.M)[N]
M[⇑N]
(λ.\1)[⇑N]
\1[⇑⇑N]

(A : Type) -> Type
ω

Type 0 :

exists n. size (A : Type 0) = n

size (A : Type 0) < ω
size (A : Type 1) < 2ω
size (A : Type 2) < 3ω


M[↑ : ]
Type

(l : \0 \2) -> \1 \3 ==
  (x : \2) = \2;
  (y : \3) = \3;
  (l : \2 \1) -> \3 \1

(l : \0 \2) -> \1 \3 ==
(l : \0 \2) -> \1 \3

\1[N][M[S]]
\1[N][M[S]]


elaborate : String -> AST -> Constraints;
check : Constraints -> ();

normalize : AST -> (Value, Receipt);
reverse : (Value, Receipt) -> AST;

reversible : reverse (normalize AST) == AST;


((x : A) -> B)[...L]

B[]
P : Code;

Code = Substs -> (Value, Receipt);

x : A;
y : B;
x = A;
y = B;
M

C<\#>[N] |-> C<N[↑(1 + #)]>[N] // subst

\0[N[↓]]
N[↓][↑][N[↓]]
N[N[↓]]


\0[N[↓]] |-> N[N[↓]]

\0[M[↓]] |-> \0[M[↓]]

M[N][↓][K] |-> M[K][N]

M[↑][N] |-> M
M[↓][↑] |-> M
M[↑][↓] |-> M

_A[↓] == M
_A == M[↑]

_A[N] == M[↑]

_A
_A[↓] == M[↑]
λ. \0

(λ. λ. \0 \1) \-1
λ. \0 \0

(x => y => x y)


M[capture x] |-> M // capture
C<x>[x := N] |-> C<N>[x := N] // subst
C<x>[x := N] |-> C<N>[x := N] // subst

(x + x)[x := (1 + y)[y `capture` x]]
((1 + x)[y `capture` x] + (1 + x)[y `capture` x])[x := (1 + x)[y `capture` x]]
((1 + x)[y := x] + (1 + x)[y := x])[x := (1 + x)[y `capture` x]]
((1 + x) + (1 + x))[x := (1 + x)[capture x]]



M[y `capture` x] |-> M[y := x] // capture

M[x := N[y `capture` x]];

M[|N; K]

M[|N; K] |-> M[N↑][K]


x = N;
y = K;
M |->
x : A;
y : B;
x = N;
y = K;
(
  x_tmp = x;
  x = (
    x = x_tmp;
    N
  );
  y =
  M
)


M[\1]

C<\(l + #)>[N] | l |->

l | M[f \(1 + l)]


l | \(1 + l)[f \(1 + l)]
l | f \(1 + l)[f \(1 + l)]



(x : A; x = N; M)

((x : A) => (x == N) => M) N refl

M[N]



Self : (T : (x : Self T) -> Type) -> Type;
roll : <T>(f : (x : Self T) -> T x) -> Self T
unroll : <T, K>(x : Self T) -> T x;

Self = T => <K>(x : Self T) ->
  (r : T x, w : r == unroll x);
roll = <T>(f) => f (roll f);
unroll = <T, K>(x) => fst (x x);

Unit_T : (u : Self Unit_T) -> Type;
unit : Self Unit_T;

Unit_T = u => (P : (u : Self Unit_T) -> Type) -> (x : P unit) -> P u;
unit = roll<Unit_T>(u => P => x => (x : P u));


(x : Self Unit_T) => (y : Self Unit_T) =>
  (y x : (r : Unit_T x, w : r == unroll x));

(x : Self Unit_T) => (y : Self Unit_T) =>
  (unroll x : (r : Unit_T x, w : r == unroll x));

Self : (A : Type) -> (B : (x : A) -> Type) -> Type;
roll : <A, B>(f : (x : A) -> B x) -> (eq : A == Self A B) -> A;
unroll : <A, B>(x : A) -> (eq : A == Self A B) -> B x;

Self = A => B =>
  (self : A) -> (eq : A == Self A B) -> (r : B self, w : r == unroll self eq);
unroll = <A, B>(x) => (eq) => fst ((eq x : Self A B) x eq);


(x : Self A B) => (y : Self A B) =>
  (x y refl : (r : B y, w : r == unroll y eq))

unroll : <T>(M : Self T) -> B M;
Self = (T : (x : Self T) -> Type) =>
  (self : Self T) -> (r : T self, w : r == unroll self);
unroll = <T>(M : Self T) => M M;

Bool : Type;
Bool = (b : Bool, c : O_Bool b)

T_Unit0 : Type;
T_unit0 : (Unit0 : T_Unit0) => Type;

T_Unit0 = (Unit0 : T_Unit0) -> (unit0 : T_unit0 Unit0) -> Type;
T_unit0 = (Unit0 : T_Unit0) => (unit0 : T_unit0 Unit0) -> Unit0 Unit0 unit0;

Unit0 : (u : _) -> Type;
Unit0 = u =>
  (P : (u : Self Unit0) -> Type) ->

Unit : Type;
unit : Unit;

Unit = (u : Unit, (P : (u : Unit) -> Type) -> (x : P unit) -> P u);
unit = (u = unit, P => x => x);


(u : Unit) => snd u

Self : (T : (x : Self T) -> Type) -> Type;
unroll : <T>(x : Self T) -> T x;

Self = T => (x : Self T) -> (r : T x, w : r == unroll x);
unroll = <T>(x) => fst (x x);

Self : (A : Type) -> (B : (x : A) -> Type) -> Type;
unroll : <A, B>(x : A) -> (a_w : A == Self A B) -> B x;

Self = A => B => (x : A) -> (a_w : A == Self A B) ->
  (r : B x, w : r == unroll x a_w);
unroll = <A, B>(x) => (a_w) => fst ((a_w x : Self A B) x a_w);

Unit : Type;
unit : Unit;

Unit = Self((u : Unit) => (P : (u : Unit) -> Type) -> (x : P unit) -> P u);

(a : Unit) => (b : Unit) => (
  a_b : (r : B b, w : r == unroll b a_w) = a b refl;
  (unroll b refl)

  fst (unit unit refl) : ((P : (u : Unit) -> Type) -> (x : P unit) -> P unit)
)

Self : (A : Type) -> (B : (x : A) -> Type) -> Type;
unroll : <A, B>(x : A) -> (a_w : A == Self A B) -> B x;

Self = A => B => (x : A) -> (a_w : A == Self A B) ->
  (r : B x, w : r == unroll x a_w);
unroll = <A, B>(x) => (a_w) => fst ((a_w x : Self A B) x a_w);


Self : (T : Self(roll ? (T => (x : Self T) -> Type))) -> Type;

Self_T : Type;
Self_T_R : Self_T;
Self : (T : Self_T) -> Type;
roll : (T : Self_T) -> (f : (x : Self T) -> T _) -> Self T;

Self_T = Self(roll Self_T_R ((T : Self_T_R) => (x : Self T) -> Type));

Self_T1 : Self(Self_T1) =



unroll : <T>(x : Self T) -> B x;




Self = A => B => (x : A) -> (a_w : A == Self A B) ->
  (r : B x, w : r == unroll x a_w);
unroll = <A, B>(x) => (a_w) => fst ((a_w x : Self A B) x a_w);

Self_W : (A : Type) -> (B : (x : A) -> Type) -> Type;
Self : (A : Type) -> (B : (x : A) -> Type) -> (w : Self_W A B) -> Type;

Self_W = (A) => (B) => A == Self A B w;


unroll : <A, B>(x : A) -> B x;

Self = A => B => (x : A) -> (a_w : A == Self A B) ->
  (r : B x, w : r == unroll x a_w);
unroll = <A, B>(x) => (a_w) => fst ((a_w x : Self A B) x a_w);

T_Unit0 : Type;
T_unit0 :

Self : (A : Type) -> (B : (x : A) -> Type) -> Type;
unroll : <A, B>(x : A) -> (a_w : A == Self A B) -> B x;

Self = A => B => (x : A) -> (a_w : A == Self A B) ->
  (r : B x, w : r == unroll x a_w);
unroll = <A, B>(x) => (a_w) => fst ((a_w x : Self A B) x a_w);


SSelf_T = Type;
SSelf : (T : SSelf_T) -> Type;

SSelf_T = Self SSelf_T ((T : SSelf_T) -> (x : SSelf T) -> Type);

Γ |- A : Type  Γ, x : A |- B : Type
Γ |- A == @(x : A) -> B
-----------------------------------
Γ |- @(x : A) -> B : Type

Self_W : (A : Type) -> (B : (x : A) -> Type) -> Type;
Self : (A : Type) -> (B : (x : A) -> Type) -> (a_w : Self_W A B) -> Type;

self_W_w : Self_W Self_W (a_w => A == Self A B a_w);
Self_W = (A) => (B) => Self Self_W (a_w => A == Self A B a_w) self_W_w;

Self : (T : Self T) -> Type;


unroll = <T>(x : A) =>

T_Unit : Type;
T_unit : (Unit : T_Unit) -> Type;

T_Unit = (Unit : T_Unit) -> (unit : T_unit Unit) -> Type;
T_unit = (Unit : T_Unit) => (unit : T_unit Unit) -> Type

T_Unit : Type;
T_u : Type;
T_unit : Type;

Unit : T_Unit;
unit : T_unit;

T_Unit = (u : T_u) -> Type;
T_u = Unit;
T_unit = Unit unit;

Unit = (u : Unit =>

Unit = Unit => unit =>
  (u : Unit Unit unit) ->
    (P : (u : Unit Unit unit) -> Type) ->

Unit : Type;
unit : Unit;
ind : (u : Unit) -> (P : (u : Unit) -> Type) -> (x : P unit) -> P u;

Unit = (u : Unit) -> (
  r : (P : (u : Unit) -> Type) -> (x : P unit) -> P u,
  w : r == ind u
);
unit = (u : Unit) =>

Bool : Type;
true : Bool;
false : Bool;
ind : (b : Bool) ->
  (P : (b : Bool) -> Type) -> (x : P true) -> (y : P false) -> P b;

Bool = (b : Bool) -> (
  r : (P : (b : Bool) -> Type) -> (x : P true) -> (y : P false) -> P b
  w : r == ind b
);
ind = b => fst (b b);

Unit : Type;
unit : Unit;

ind : (u : Unit) -> (P : (u : Unit) -> Type) -> (x : P unit) -> P u;

ind_unit_w : ind unit == (P : (u : Unit) -> Type) => (x : P unit) => x;

Unit = (u : Unit) ->  (
  r : (P : (u : Unit) -> Type) -> (x : P unit) -> P u,
  w : r == ind u
);
unit = (u : Unit) => (
  r = (P : (u : Unit) -> Type) => (x : P unit) => x;
  w = ind u (u => r == ind u)
    (unit_w (x => r == x) refl : r == ind unit)
);

ind = (u : Unit) => fst (u u);

unit = (u : Unit) => (
  r = (P : (u : Unit) -> Type) => (x : P unit) => x;
  w = ind u (u => r == ind u)
    (unit_w (x => r == x) refl : r == ind unit)
);
unit_w = refl;


T_Unit0 : Type;
T_u : Type;
Unit0 : T_Unit0;

T_Unit0 = (u : T_u) -> Type;

Unit0 = (A : Type) => (u : A) =>
  (w : A == Unit0 A u) -> (P : (A : Type) -> (u : A) -> Type) ->
    (x : P (Unit0 Unit0 unit0)) -> P A u;

T_u : Type;
Unit : (u : T_u) -> Type;
unit : T_u;

T_u = Unit unit;
Unit = (u : T_u) => (I : T_u == Unit unit) =>
  (P : (u : T_u) -> Type) -> (x : I1 P unit) -> I0 P u;
I_Unit =

Unit : Type;
unit : Unit;
ind : (u : Unit) -> (P : (u : Unit) -> Type) -> (x : P unit) -> P u;

Unit = (self : Unit) -> {
  u : Unit;
  i : u == self;
  w_i : i == ind u;
};
ind = u => (
  u_u = u u;
  u_u.w_u u_u.i
);
unit = self => {
  u = unit;
  i = P => x => x;
  w_u = self.w_u ;
  w_i = _;
};

ind = (u : Unit) => u.w (x => x) u.u;

(u : Unit) =>
  ind u;

(T_x : Type) => (x : T_x) => (w : T_x == I_Unit x) => (
  x : Unit = ;
)
T_u = (u : T_u) -> Unit u;
unit = (u : T_u) => (P : (u : T_u) -> Type) => (x : P unit) => _;

Unit = (P : (x : _) -> ) -> (x : P unit unit) -> P u u

unit0 : T_u;
unit = Unit unit0 unit0;
unit = P => x => x;
unit0

unit = (P : (u : Unit unit) -> Type) => (x : P unit) => x;


(u : A) => (w : A == Unit u) =>
T_unit : Type;
unit : T_unit;

(A : A)
T_unit =
unit0

C_Unit = (A : Type) -> (x : A) -> A;
c_unit : C_Unit = A => x => x;

Unit : Type;
unit : Type;

Unit = (
  c : C_Unit,
  i : (P : (u : C_Unit) -> Prop) -> (x : P c_unit) -> P c
);


()

(x : )
T_unit


Unit : Type;
unit : Unit;

unit =
  (P : (u : Unit) -> Type) => (x : P unit) => x;

Unit = (u : Unit) -> (w : u == unit) ->
  (P : (u : Unit) -> Type) -> (x : P unit) -> P u;


(x : Unit) =>
ind : (u : Unit) -> (P : (u : Unit) -> Type) -> (x : P unit) -> P u;



(λ.\1)[N[S]]
(λ.N[S][↑])

M[⇑S][↑]

M[↑ : n]

x : A;
y : B;
_A := N;
y = M;

x = _A;
(λ. \1)[↑ : 0] | 1
(λ. \1) | 1
(λ. (λ. \1)[λ. \1])
(λ. (λ. (λ. \2)[↑ : 2]))
(λ. (λ. \0)[↑ : 1])

(λ. \1)[↑] | 1
(λ. (λ. (λ. 1)[↑])) | 0
(λ. (λ. 1)[↑]) | 1

M[N][↑ : l] | 1 + l

\l[↑ : l][N[↑ : l]] | 1 + l


\l[↑ : l][N[↑ : l]] | l

\l[↑ : l][N[↑ : l]] | l

(λ. \0)[↑ : 0] | 1
(λ. \0[↑ : 0]) | 1
(λ. \1) | 1

(λ. λ. \0) (λ. \0)
(λ. \0)[λ. \0]
(λ. (λ. \0)[↑ : 0])

M[•]

\0[_ : A[S]]
(\0 : A[↑ : S][S])[_ : A[S]]

M[↑d : b][S]

⇑
•

n | C<\n>[N] |-> n | C<N[↑(bv C) | ?]>[N] // subst
n | M[N] |-> n | M{↓}    if (n ∉ fv M) // gc

n | L[n : A]<M> ⇐ R[n : A]<B>
-----------------------------
n | L<λ. M> ⇐ R<∀:A. B>

1 + n | L[n : A]<M> ⇒ R<B>
----------------------------
n | L<λ:A. M> ⇒ ∀:L<A>. R<B>

Γ; n : A |- M ⇒ B
--------------------
Γ |- λ:A. M ⇒ ∀:A. B

n | L[N]<M> ⇒ T
---------------
n | L<M[N]> ⇒ T


(M[A] N)

\n[N] | n |-> N

\+(1 + m)[N] | n when m > n |-> \


Γ |- x = z; C<M> == y = z; C<N>
-------------------------------
Γ |- x = M; C<x> == y = N; C<y>

-----------------------
n | L[M : A]<\0> ⇒ L<A>

n | L<\n> ⇒ T
--------------------------
n | L[M : A]<\(1 + n)> ⇒ T

Fix : Type;
Fix = (x : Fix) -> ();

Fix == (x : Fix) -> ()
(x : Fix) -> () == (x : Fix) -> ()

Γ |- M ⇒ A
----------
Γ |- M ⇐ A

L<M> ⇒ T
--------
L<M> ⇐ T


x : Nat;
x = 1 + x;

Sort (S) ::=
  | Type
  | Prop
  | Data
  | Line
Term (M, N)
Type (A, B) ::=
  | (M : A)
  | x
  | (x : A) -> B
  | (x : A) => M
  | M N
  | x : A; M
  | x = N; M
  | (x : A, y : B)
  | { x : A; y : B; }
  | (x : A) & B
  | (A | B)
  ;

Unit == Bool

Bool = <A>(x : A) -> (y : A) -> A;
false : Bool = x => y => y;

Fst = <A, B>(x : A) -> (y : B) -> A;


f = (fst : Fst) : Nat => fst 1 "a";

f (false : Bool)

Fst <: Bool

Bool <: Fst // fails

<A, B>(x : A) -> (y : B) -> A <: <A>(x : A) -> (y : A) -> A

(x : _A) -> (y : _B) -> _A <: <A>(x : A) -> (y : A) -> A // valid
  (x : _A) <: (x : A)
  (y : _B) <: (y : A)
  _A <: A

<A>(x : A) -> (y : A) -> A <: <A, B>(x : A) -> (y : B) -> A // fail
(x : _A) -> (y : _A) -> _A <: <A, B>(x : A) -> (y : B) -> A
  (x : _A) <: (x : A) // _A := A
  (y : A) <: (y : B) // fail
  A <: A

(x : _A) -> (y : _A) -> _A == (x : _C) -> (y : _B) -> _C


Bool : Type;
true : Bool;
false : Bool;
case : <A>(b : Bool) -> (x : A) -> (y : A) -> A;

<Bool>(true : Bool) => (false : Bool) =>
  (case : <A>(b : Bool) -> (x : A) -> (y : A) -> A) => _??

x : A  ∈ fv Γ
---------------
Γ |- x : inst A


id : <A>(x : A) -> A = _;

(id : (x : _A) -> _A)

Nat <: Int
(x : Int) -> () <: (x : Nat) -> ()

(x : Int) -> () <: (x : Nat) -> ()
  Nat <: Int

(x : -A) -> +B

f : (w : (x : Nat) -> ()) -> () = w => w 1;

f ((x : Int) => ())

_A := _C
_C := _B

λ:A. λ:B. (\l : A)[N] | l

λ:Type.λ:\0. (\1 : \0[from \1])[N]


Substs ::= • | (M : A)[S] | ↑

\0[(M : A)[S]]
M[S]

L[M : A]<\1> :


-----------
Type ⇒ Type

---------------
[N : A](\0) ⇒ A

\n ⇒ A
-------------------
[N : A](\1 + n) ⇒ A

[\0 : A](B) ⇐ Type
------------------
∀:A. B ⇒ Type

[\0 : A](M) ⇒ B
---------------
λ:A. M ⇒ ∀:A. B

M ⇒ L<(x : A) -> B>  N ⇐ L<A>
-----------------------------
M N ⇒ L<(x : A) => B> N

M ⇒ L<(x : A) -> B>  N ⇐ L<A>
-----------------------------
M N ⇒ [L . N]<B>

[\0 : L<A>](M) ⇐ L[\0 : A]<B>
-----------------------------
λ. M ⇐ L<∀:A. B>

A ⇒ Type  N ⇐ A  [N : A](M) ⇒ T
-------------------------------
[N : A](M) ⇒ T

M ⇒ A  A ≡ B
------------
M ⇐ B

[K](M) N ≡ [K](M N[↑])

(λ. \0 \0) [K](N) |-> [K](M[↑] N)

(λ. [N](M)) |-> [N{↓}](λ. M)  (\0 ∉ fv N)

(λ. [N](M)) |-> [λ. N](λ. M[\1 \0])


[N](C<\#>) |-> [N](C<N[↑1 + #]>) // subst
[N](M) |-> M  (\0 ∉ fv M) // gc

\0[K] |-> K
\(1 + n)[S] |-> \n
(M N)[S] |-> M[S] N[S]
(λ. M)[S] |-> λ. M[⇑S]

[K](N) [K](N)

(λ. M) N |-> [N]<M>

M ⇒ L<(x : A) -> B>  N ⇐ L<A>
-----------------------------
M N ⇒ L<(x : A) => B> N

infer : (substs : Substs, term : Term) ->
  (substs : Substs, type : Type);

L<λ. M> N |-> L[N[↑L]]<M>



Substs ::= • | M . S
Term ::= \n | λ. M | M N | M[S]

\0[]

M[N . S1][S2]

(λ. M)[S] N |-> M[N . S]

[[S](N : A)](M)

(λ. M)[N] K
(λ. M)[K . N]

(λ. M)[S] N |-> M[N . S]

((λ. M)[K] N)[S]
((λ. M)@(N[S]))[K . S]
M[N[S] . K[S] . S]

((λ. M) K[↑])[N]
M[K[↑]][N]

M ⇒ A
-----------
L<M> ⇒ L<A>

--------------------------
(Γ |- M ⇒ N) ≡ L<M> ⇒ L<A>

---------------
[N : A](\0) ⇒ A
----------------------
[N : A][K : B](\1) ⇒ A

--------------------------
[N : A] |- [K : B](\0) ⇒ A
-------------------------------
[N : A][K : B](\0) ⇒ [N : A](A)



(λ:\0. (\1 : \0@1)[N])[A]

M[↑ : 1][N][K]
M[N[K]][↑][K]
M[N[K]]


-------------
_A[↑] == K[N]

----------------------
_A[N][↑][N] == K[↑][N]

_A[↑][N] == K

[N] |- _A[↑] == K[↑]
|- _A == K[↑][N]


Γ[N] |- _A[↑] == K[↑]
Γ |- _A == K[↑][N]


---------------
[N : A](\0) ⇒ A

\n : B
----------------------
[N : A](\(1 + n)) ⇒ B

M ⇒ L<(x : A) -> B>  N ⇐ L<A>
-----------------------------
M N : L[N[↑L] : A]<B>


[N : A](_)<[K : B](\0) ⇒ B>
-------------------------------
[N : A][K : B](\0) ⇒ [N : A](B)

Term ::= \n | λ. M | M N | [N](M);

Context := _ | λ. C | C N | M C | [C](M) | [N](C);




f ((λ. M) N)

(λ. M) N |-> [N](M)
-------------------
(f _)<(λ. M) N> |-> (f _)<[N](M)>

f <| x =>

M : {|
  T : Type;
  x : T;
|} = {
  T = Nat;
  x = 1;
};

<A>(x : A) -> B;
(<A>, x : A) -> B;

[A](x : A) => A

f(<A>)

M.x : M.T


Γ |- A :
-----------------------
Γ |- (x : A) & B :


x : Nat;
x = 1 + x;


Γ, x : A |- B : Type
-----------------------
Γ |- (x : A) & B : Type


Γ |-

Self : (T : (x : Self T) -> Type) -> Type;



Nat : [a] -> Type;
Nat = [a] => (A : Type) ->
  (z : A) -> (s : [b < a] -> (pred : Nat b) -> A) -> A;

Unit : [a] -> Type;
unit : [a] -> Unit a;

Unit = [a] => [b < a] -> (u : Unit b) &
  (P : (u : Unit b) -> Type) -> (x : P (unit b)) -> P u;
unit = [a] => [b < a] => P => x => x;

(x : Unit 1) => x 0 :
  (P : (u : Unit 0) -> Type) -> (x : P (unit 0)) -> P (x 0)


Unit : [a] -> Type;
unit : [a] -> Unit a;

Unit = [a] => [b < a] -> (u : Unit b) &
  (P : (u : Unit b) -> Type) -> (x : P (unit b)) -> P u;
unit = [a] => [b < a] => P => x => x;



T_Unit : Type;
T_unit : (Unit : T_Unit) -> Type;

T_Unit = (Unit : T_Unit) & (unit : T_unit Unit) -> Type;
T_unit = (Unit : T_Unit) => (unit : T_unit Unit) & Unit unit;

T_Unit : Type;
T_Unit = (Unit : Type) & (
  T_unit : Type;
  T_unit = (unit : T_unit) & T_Unit unit;
  (unit : T_unit) -> Type
);



T_T_unit : Type;
T_T_unit = (T_unit : T_T_unit) & Type;

T_Unit : Type;
T_unit : (Unit : T_Unit) -> Type;

T_False : Type;
T_False = (False : T_False) & Type;

T_Unit : Type;
T_Unit = (T_unit : (Unit : T_Unit) -> Type) ->
  (Unit : T_Unit) & (unit : T_unit Unit) -> Type;

T_unit : (Unit : T_Unit) -> Type;
T_unit = (Unit : T_Unit) =>
  (unit : T_unit Unit) & Unit T_unit unit;

Unit : T_Unit;
Unit = (T_unit : (Unit : T_Unit) -> Type) =>
  (unit : T_unit Unit) => (u : T_unit Unit) &
    (P : (u : T_unit Unit) -> Type) -> (x : P unit) -> P u;

unit : T_unit Unit;
unit = (P : (u : T_unit Unit) -> Type) => (x : P unit) => x


(unit : T_unit Unit) & (P : (u : T_unit Unit) -> Type) -> (x : P unit) -> P u ==
  (u : T_unit Unit) & (P : (u : T_unit Unit) -> Type) -> (x : P unit) -> P u




T_Unit : Type;
T_Unit = (Unit : T_Unit) & (
  T_W : Type;
  T_W = (w : T_W) &
    (T_unit : (w : T_W) -> Type) ->
    T_Unit == (w : T_W) -> (unit : T_unit w) -> Type;
  (w : T_W) -> (unit : w I Unit w) -> Type;
);

T_Unit : Type;
T_Unit = (T_unit : (Unit : T_Unit) -> Type) ->
  (Unit : T_Unit) & (unit : T_unit Unit) -> Type;

T_unit : (Unit : T_Unit) -> Type;
T_unit = (Unit : T_Unit) =>
  (unit : T_unit Unit) & Unit T_unit unit;

Unit : T_Unit;
Unit = (T_unit : (Unit : T_Unit) -> Type) =>
  (unit : T_unit Unit) => (u : T_unit Unit) &
    (P : (u : T_unit Unit) -> Type) -> (x : P unit) -> P u;

unit : T_unit Unit;
unit = (P : (u : T_unit Unit) -> Type) => (x : P unit) => x



Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool) &
  (P : (b : Bool) -> Type) -> (x : P true) -> (y : P false) -> P b;
true = (P : (b : Bool) -> Type) => (x : P true) => (y : P false) => x;
false = (P : (b : Bool) -> Type) => (x : P true) => (y : P false) => y;


T_Bool : Type;
T_true_and_false : (Bool : T_Bool) -> Type;

T_Bool = (Bool : T_Bool) &
  (true_and_false : T_true_and_false Bool) -> Type;
T_true_and_false = (Bool : T_Bool) =>
  (true_and_false : T_true_and_false Bool) & Bool true_and_false;

T_Unit : Type;
T_Unit = (T_unit : (Unit : T_Unit) -> Type) ->
  (Unit : T_Unit) & (unit : T_unit Unit) -> Type;

T_unit : (Unit : T_Unit) -> Type;
T_unit = (Unit : T_Unit) =>
  (unit : T_unit Unit) & Unit T_unit unit;

Unit : T_Unit;
Unit = (T_unit : (Unit : T_Unit) -> Type) =>
  (unit : T_unit Unit) => (u : T_unit Unit) &
    (P : (u : T_unit Unit) -> Type) -> (x : P unit) -> P u;

unit : T_unit Unit;
unit = (P : (u : T_unit Unit) -> Type) => (x : P unit) => x

Unit : (A : Type) -> (u : A) -> Type;
T_unit : Type;
unit : T_unit;

T_unit = Unit T_unit unit;
Unit = (A : Type) => (u : A) => (w : A == Unit A x ?) ->
  (P : (A : Type) -> (u : A) -> (w : A == Unit A x ?) -> Type) ->
    (x : P T_unit unit refl) -> P A u w;

(Unit : Type) =>
  (unit : Unit) =>
  (x : Unit) =>
ind : (A : Type) => (u : A) => (w1 : A == Unit A u) =>
  w1 (x => x) u
    (A => x => w2 =>
      (w2 (x => x) x : Unit A x) == unit)
    ()
    w1
```

## Better Rules

```rust
-----------
Type ⇒ Type

---------------
[N : A](\0) ⇒ A

\n ⇒ A
-------------------
[N : B](\1 + n) ⇒ A

[\0 : A](B) ⇐ Type
------------------
∀:A. B ⇒ Type

Γ<M> ⇒ L<(x : A) -> B>  Γ<N> ⇐ L<A>
-----------------------------------
Γ<M N> ⇒ [L . Γ<N>]<B>

[Γ . \0 : L<A>](M) ⇐ L[\0 : A]<B>
-----------------------------
Γ<λ. M> ⇐ L<∀:A. B>

A ⇒ Type  N ⇐ A  [N : A](M) ⇒ T
-------------------------------
[N : A](M) ⇒ T

M ⇒ A  (M : A) <: B
-------------------
M ⇐ B


Γ |- M ⇒ L<(x : A) -> B>  Γ |- N ⇐ L<A>
---------------------------------------
Γ |- M N ⇒ [L . N]<B>


Γ |- M ⇒ L<(x : A) -> B>  Γ |- N ⇐ L<A>
---------------------------------------
Γ |- M N ⇒ L<(x : A) => B> N

Γ[\0 : A]<M> ⇒ L<B>
---------------------
Γ<λ:A. M> ⇒ ∀:Γ<A>. B

Γ<M> ⇒ Γ<L<A>>
--------------
Γ |- M ⇒ L<A>


[λ. M : T] ≡ ∃A; ∃B;
  (∀:A; [M : L<B>]) && (T ≡ ∀:A. B);

[M N : T] ≡ ∃A; ∃B;
  [M : L<∀:A. B>] && [N : L<A>] && (T ≡ [L . N]<B>);


Γ<M> ⇒ Γ<A>
-----------
Γ |- M ⇒ A

A ⇐ Type  N ⇐ A
-----------------
[N : A] : Context

---------------
[N : A](\0) ⇒ A

\n ⇒ A
-------------------
[N : B](\1 + n) ⇒ A

[\0 : A](B) ⇐ Type
------------------
∀:A. B ⇒ Type

[\0 : A] |- M ⇒ B
-----------------
λ:A. M ⇒ ∀:A. B

Γ |- M ⇒ L<∀:A. B>  Γ |- N ⇐ L<A>
---------------------------------
Γ |- M N ⇒ [L . N]<B>


[L][N](M) |-> [L . [L](N)](M)
L<λ. M> N |-> [L . N](M)

[L . N](C<\#>) |-> C<N[↑[1 + # + L]]>


[S . N](M) ≡ [N][⇑S](M)
[S][[↑S](N)](M) ≡ [N][⇑S](M)

[S][[↑S](N)](M) ≡ [N][S][[↑S](\0)](M)

[S][[↑S](N)](M) ≡ [N][S][[↑S](\0)](M)

[S](λ. M) ≡ λ. [S][[↑S](\0)](M)
λ. [S][\S](M)

L<λ:A. M>
λ:L<A>. L<M>

Γ[\0 : A] |- M ⇒ B
--------------------
Γ |- λ:A. M ⇒ ∀:A. B

Γ[Δ][\0 : A] |- [⇑(↑Δ)](M) ⇐ B
------------------------------
Γ |- λ. M ⇐ [Δ](∀:A. B)

Γ |- M ⇒ [Δ](∀:A. B)  Γ |- N ⇐ [Δ](A)
-------------------------------------
Γ |- M N ⇒ [Δ . N](B)

Term ::=
  | \n
  | ∀:A. B
  | λ. M
  | M N
  | [N](M)



f = x => B; C<f N> |->
f = x => B; g = (x = N; f x); C<g>

[λ. B](C<\# N>) |->
[λ. B][[N](B)](C<\#>)


id = x => x;

kid = k => x => k x;

Term (N, M) ::=
  | _X
  | \n ...N
Block (B) ::=
  | ∀:A. B
  | λ. B
  | [N : A](B)
  | M

_M := λ. _MB
(f : A -> _X) == (g : A -> B -> C)

λ. [N](_MB) == g

λ. [\0](_MB) == λ. λ. \0
λ. [\0](g \0) == λ. λ. \0

λ. [\0](_MB) == λ. λ. \1


0 -> 1
1 -> 0
2 -> 1
3 -> 2

[\2](_M) == g

[\2](_M) == \2

_M == [\0](\1)
_M == \0

[\2](_M) == \2

[\1](_M) == g  0 ∉ fv g
_M == [\0](g)

[\1][\0](g) == g

[\1][↑](g) == g

[x := y](_M) == g  x ∉ fv g
_M == [y := x](g)


[\n](_M) == g
[↑][\n](_M) == [↑](g)


[↑][\n](_M) == [↑](g)


[\n](_M) == \0
[\n](_M) == f \0

[\1](_M) == (\0 \1)
_M := (\0 \2)
[\0](\0 \2) == (\0 \1)

[\1](_M) == (\0 \1)
_M == (\1 \0)
[\1](\1 \0) == (\0 \1)

[\1](_M) == (\0 \1)
_M == [\1](\0 \1)
_M == (\1 \0)

[\2](_M) == (\0 \1 \2)
_M == (\1 \2 \0)

[\3](_M) == (\0 \1 \2 \3)
[\3](_M) == (\0 \1 \2 \3 \4)
_M == (\1 \2 \3 \0 \5)

[\3][\3][\3][\3](M) |-> M

[\0 . \3 . \2 . \1](\1 \2 \3 \0 \5)

[\4 . \3 . \2 . \1](M)
[\3][\3][\3](M)


[\3](\1 \2 \3 \0)
[\3][\3][\3](\0 \1 \2 \3) |-> (\1 \2 \3 \0)

[\3][\3][\3](\0 \1 \2 \3)
[\3][\3](\3 \0 \1 \2)
[\3](\2 \3 \0 \1)
(\1 \2 \3 \0)

[\0][\1][\2](\3 \0 \1 \2)
[\0][\1][\2](\ \0 \1 \2)

1 -> 0
2 -> 1
3 -> 2

_M x y z == T

λ. [N](_MB) == g

_MB := [↑](g \0)

λ. [\0](_MB) == g

λ. g \0 == λ. λ. \0

[N](_MB) := λ. λ. \0

λ. g N == λ. λ. \1
_M := g

λ. [\0][N](M) == λ. [N](M)


λ. λ. \0 == λ. λ. \0

id = x => (
  log(x);
  x;
);
keid =  => x =>
  log(e, x, () => x);

(f : A -> _X) == (g : A -> B -> C)

λ. [N](_MB) == g
_MB == [↑](g \0)
λ. g \0 == g



Value (V) ::=
  | _X
  | \n ...V
  | A -> B
  | (A, B)
  | A & B
  | A | B
  ;
Block (B) ::=
  | ∀:V. B
  | λ. B
  | [V]B
  | V

Value (V) ::=
  | \n ...V
  | λ. V;
(f V)

f = x => y => x;
f N K

f = x => y => y;
g = x => y => y;
f_0 = y => y;
g_0 = y => y;
(f \0) == (g \0)

f = x => y => y;
g = x => y => y;
f == g


-----------------
H |- L<M> == R<N>

f = λ. _M;

l | f == g
l | λ. _M\(l + 1) == λ. ∀:\0. \0
l + 1 | _M\(l + 1) == ∀:\0. \0

_M == (g \0)

l | λ. [\0][↑](g \0) == λ. λ. \0

f == g

λ. [\0](DROP; VAR \0; JMP g)


id = (x : Type) => x;


f : (A -> A) & (B -> B);

∀A. Int

forall (F : Type -> Type) (f g : forall t, F t) (t : Type),
  f t = g t -> f = g.
forall (F : Type -> Type) (f g : forall t, F t),
  (forall (t : Type), f t = g t) -> f = g.

CST: forall (F: Type -> Type) (f: forall t, F t)
  (t1 t2: Type), f t1 = f t2


id = x => x;

Fix = (x : Fix) -> ();

(x : Fix) -> () == Fix

Fix == (x : Fix) -> () |- (x : Fix) -> () == Fix

A = (f : A) -> ();
B = (f : B) -> ();

(λ. _M \0)
_M := λ. _MB
(λ. [λ. \0](_MB))

(λ. [λ. \0](_MB))

[λ. \0](_MB) == ?
_MB := (\0 \0)

[λ. \0](\0 \0) == (λ. \0) (λ. \0)

(λ. (\0 \0))

_M := (\0 \0)

[λ. \0](_MB) == ?

x = _M;
a = [N](x);
b = x;


Lift all lambdas
Erase closures
Erase types to blocks
Preserve evaluation order

Term (M, N) ::=
  | _X
  | \n
  | λ. M
  | M N
  | [N](M)
  | [↑](M)
  | (M, N)
  ;

Just avoid substs

[↑](_X) N

_X := _XB;

[N][↑ : 1](_XB)

Global (G) ::=
  | x = D; G
  | D
Declaration (D) ::=
  | λ. D
  | B
Computation (C) ::=
  | [N](C)
  | C
Atom (A)
  | [...(↑ : n)](_X)
  | \n ...\n??
  ;

λ. [N](M)

Sometimes you actually know the closure pointer, but not data
Just skip footer when calling the function directly


f_closure:
  call f
f_footer:

f:
  ret



Declaration (D) ::=
  | x => D
  | M
Term (M, N) ::=
  |

T(f); T(g(x)); APPLY; T(h(y)); APPLY

a = f;
b = g(x);
c = h(y);
a(b, c)

x = g h;
y = g h;

x == y

x : A : Type;
x : (x : A) -> B : Data;
x : A : Prop;

f = λ. M; C<f N> |->
g = f@N; C<g>

f = x => y => y;
g = x => y => y;
f 1 == g 1

x = 1;
y => y

x = 1;
y => y

y => f 1 y == y => g 1 y
y => y == y => y




l | x => (
  a = f x;
  b = _A;
  _A;
  M
);

Value ::=
  | x ...V
  | (V1, V2)

f => x => f x == f => x => _A


L<x => M> N |-> x = N; M // beta
x = N; C<x> |-> x = N; C<N> // subst

f = x => M; C<f N> |->
f = x => M;
r = x = N; M;
C<r> // beta

f = _A; C<f N> |->
// _A := x => _AB
f = x => _AB; C<f N>

C<_A N> |->
// _A := x => _AB
f = x => _AB; C<f N>


_A N |-> (λ. _A2) N
// _A := λ. _A2

// hole

_X N |-> (λ. _B) N
_X == λ. _B



Global (G) ::=
  | x = D; G
Declaration (D) ::=
  | λ. D
Block (B) ::=
  | [C](B)
Compute (C) ::=
  | x ...A
  | \n ...A
Atom (A) ::=
  | \n
Hole (H) ::=
  | _X
  | [↑ : n](H)
Value (V) ::=
  | H
  | \n ...A

// lift lambdas
(λ. M) N |-> [N](M) // beta
M (λ. K) |-> [λ. N]([↑](M) \0)

// flatten apply

// lift substs
λ. [K](M) |-> [λ. K](λ. [\1 \0](M))
[K](M) N |-> [K](M [↑](N))
V [K](N) |-> [K]([↑](V) N) // evaluation order?
[[K](N)](M) |-> [K][N][↑ : 1](M)

// push shifts
[↑ : n](\m) |-> \(1 + m)  (n <= m)
[↑ : n](\m) |-> \m  (n > m)
[↑ : n](M N) |-> [↑ : n](M) [↑ : n](N)
[↑ : n](λ. M) |-> λ. [↑ : 1 + n](M)
[↑ : n][N](M) |-> [[↑ : n](N)][↑ : 1 + n](M)


[↑]((λ. M) N)
[↑][N](M)
[[↑](N)][↑ : 1](M)

[[↑](N)][↑ : 1](M)

[↑](λ. M) [↑](N)
(λ. [↑ : 1](M)) [↑](N)
[[↑](N)][↑ : 1](M)

((x => t) v)[y := u]
t[x := v][y := u]
t[y := u][x := v[y := u]]

[K]((λ. M) N)
[K][N](M)l

[K](λ. M) [K](N)
(λ. [K](M)) [K](N)
[[K](N)][K : 1](M)

[[K](N)][K : 1](M)


[↑](_X N)
[↑](_X) [↑](N)
(λ. M) N


(A = T; x : A) <: (A : Type; x : A)

K = new key;
(A = T; x : A)

f : (x : A) -> B;
x : A;

x = 1 + x;


| (b == true) => _
| (b == false) => _



Γ |- (M : (x : False) -> T) |-> ⊥
Γ |- M | ⊥ |-> M

f : (b : Bool) -> (w : b == true) -> ();
g : (b : Bool) -> (w : b == false) -> ();


(b : Bool) => f b | g b
// case analysis on b, trait like
(b : Bool) => ind b (
  | (f b : (w : true == true) -> ())
  | (g b : (w : true == false) -> ())
) (
  | (f b : (w : false == true) -> ())
  | (g b : (w : false == false) -> ())
)
// normalize some types
(b : Bool) => ind b (
  | (f b : (w : True) -> ())
  | (g b : (w : False) -> ())
) (
  | (f b : (w : False) -> ())
  | (g b : (w : True) -> ())
)
// trim unaccessible cases
(b : Bool) => ind b
  ((f b : (w : True) -> ()))
  ((g b : (w : True) -> ()))

f : _;


(M<A> | N) |->

F a

Nat : [a] -> Type;
Nat = (A : Type) -> (z : A) ->
  (s : [b < a] -> (p : Nat b) -> A) -> A;

T_Unit : Type;
T_unit : (Unit : T_Unit) -> Type;

T_Unit = (Unit : T_Unit) & (unit : T_unit Unit) -> Type;
T_unit = (Unit : T_Unit) => (unit : T_unit Unit) & Unit unit;

T_T_unit =
  (T_Unit : [a] -> Type)
  [a] -> (Unit : T_Unit a) -> Type;

T_Unit : [a] -> Type;
T_Unit = [a] =>
  [b < a] -> (Unit : T_Unit b) & (unit : T_unit b Unit) -> Type;


T_unit : [a] -> (Unit : T_Unit a) -> Type;


T_unit = [a] => (Unit : T_Unit a) =>
  [b < a] -> (unit : T_unit b Unit) & Unit b unit;

f
Unit : [a] -> [b < a] -> Type;
unit : [a] -> [b < a] -> Unit b;

Unit = [a] => [b < a] => (u : Unit b) &
  (P : (u : Unit b) -> Type) -> (x : P (unit a b)) -> P u;
unit = [a] => [b < a] =>
  (P : (u : Unit b) -> Type) => (x : P (unit a b)) => x;


CNat : Type =
  (A : Type) -> (z : A) -> (s : (x : A) -> A) -> A;
czero : CNat =
  A => z => s => z;
csucc : (pred : CNat) -> CNat =
  pred => A => z => s => s (pred A z s);

// stratified induction but negative
Unit : Type;
unit : Unit;

Unit = (
  u : Unit,
  i : (P : (u : Unit) -> Type) -> (x : P unit) -> P u,
);
unit = (
  u = unit;
  i = (P : (u : Unit) -> Type) => (x : P unit) => x;
);

ind_unit : (u : Unit) ->
  (P : (u : Unit) -> Type) -> (x : P unit) -> P (fst u) =
  u => snd u;


T_Unit : Type;
T_Unit = (Unit : T_Unit) &
    (T_unit : (Unit : T_Unit) -> Type) -> (unit : T_unit Unit) -> Type;

Unit : T_Unit;
Unit = (T_unit : (Unit : K) -> Type) => (unit : T_unit Unit) =>
  (u : T_unit Unit) & (P : (u : T_unit Unit) -> Type) -> (x : P unit) -> P u;

Γ |- A ≡ (x : A) & B  Γ, x : A |- M : B
---------------------------------------
Γ |- @fix(x : A). M : A

(Unit :> )

T_Unit = (K : Type) -> (w : (d : MT_Unit K) -> K) -> K;
MUnit = K => (Unit : K) =>
  (T_unit : (Unit : K) -> Type) => (unit : T_unit Unit) =>
  (u : T_unit Unit) & (P : (u : T_unit Unit) -> Type) -> (x : P unit) -> P u;


(MUnit K Unit :
  (T_unit : (Unit : MT_Unit K) -> Type) -> (unit : T_unit Unit) -> Type) :>
  (Unit : K) & (T_unit : (Unit : K) -> Type) -> (unit : T_unit Unit) -> Type
Unit : T_Unit = K => w =>
  w ((Unit : MT_Unit K) => w (MUnit K Unit));


T_unit : (Unit : T_Unit) -> Type;
T_unit = (Unit : T_Unit) =>
  (unit : T_unit Unit) & Unit T_unit unit;

Unit : T_Unit;
Unit = T_unit => unit =>
  (u : T_unit Unit) &
    (P : (u : T_unit Unit) -> Type) -> (x : P unit) -> P u;

unit : T_unit Unit;
unit = (P : (u : T_unit Unit) -> Type) => (x : P unit) => x

Unit : Type;
unit : Unit;

Unit = (
  u : Unit,
  i : (P : (u : Unit) -> Type) -> (x : P unit) -> P u,
);
unit = (
  u = unit;
  i = (P : (u : Unit) -> Type) => (x : P unit) => x;
);

Γ, x : A |- B : Type
-----------------------
Γ |- (x : A) & B : Type

Γ |- M : T
-----------------------
Γ |- M ⇐ (x : A) & B

f = ((x, y) => x(y, a, b))
// extrude all free vars, partially applied
f = ((a, b) => (x, y) => x(y, a, b)) (a, b)
// a more concrete representation, pair with args
f = [(a, b) => (x, y) => x(y, a, b), (a, b)]
// then you can fuse it to go back to simple functions
f = [((a, b), x, y) => x(y, a, b), (a, b)]

// now let's look at call site
z = f(x, y);
// goes to
z = f[0](f[1], x, y);

Global (G) ::=
  | x = D; G
Declaration (D) ::=
  | λ. D
Block (B) ::=
  | [C](B)
Atom (C) ::=
  | x ...A  // call
  | \n ...A // apply
Hole (H) ::=
  | _X
  | [↑ : n](H)
Value (V) ::=
  | H
  | \n ...A

f => (
  x = f 1;
  f 2 + x;
)

x = f => f 1;
f => f 2 + x f;
// IR
Heap Value (HV) ::=
  | \+l ...HV // heap var + apply
Stack Value (SV) ::=
  | \-n ...LV // stack var + apply
  | HV




M N
(M, N)


l | [λ. λ. N][\+l K](\+(1 + l))
l | [λ. λ. N][(\+l, K)](\+(1 + l))
f[0](f[1],)

[SV]D

[N][K](\-1 \-2)
[N][K]\-1; \-3; @
[N][K][K]; \-2; @

L<λ. M> N |-> L[N]<M>
[N]C<\#> |-> [N]C<N>

[K]M N |-> [K](M N)
M [K]N |-> [K](M N)
[[K]N]M |-> [K][N]M

l | [K]((λ. M) \-1)
l | [K][\-1](M)

L<[N]M> |-> L[N]<M> // pack

Γ |- A : Type  Γ, x : A |- B : Type
-----------------------------------
Γ |- ∀((x : A). B) : Type

Γ |- A : Type  Γ, x : A |- B : Type
-----------------------------------
Γ |- (x : A) => M : ∀((x : A). B)

C : ∀() = (x : A) => B;
T : Type = ∀C;


(x : A) -> (M : (x : A) -> Type) x
(∀, (x : A) => M x)

Γ |- M : (∀, (x : A) => Type)
-----------------------------
Γ |- (∀, M) : Type

(∀, (A : Type) => )

(x : A, y : B x)
(x = M, y = N x);

l | (λ. \(1 + l))
// lift substs
l | λ. [N]M |-> [λ. N](λ. {\1 \0}M) // out-lambda
l | [K]M N |-> [K](M {↑}N) // out-apply-l
l | M [K]N |-> [K]({↑}M N) // out-apply-r
l | [[K]N]M |-> [K][N]{↑ : 1}M // out-let



// lift lambda
l | M (λ. N) ≡ [λ. N]([↑]M \0) // closure conversion

[λ. N]([↑]M (λ. N)) // rev-gc
[λ. N]([↑]M (λ. N)) // rev-gc

l | [L<λ. D>]C<\+l V> |->
  [L<λ. D>][L<[N]D>]C<\(1 + l)>  (N is LC) // very distant beta

l | [L<λ. D>]C<\+l N> |->
  [L<λ. D>][L<[N]D>]C<\(1 + l)>  (N is LC) // very distant beta


l | [D]M |-> M  (\+l ∉ fv M) && (\-1 ∉ fv M) // gc


[N](λ. M) K
[N]((λ. M) [↑]K)
[N][[↑]K](M)


[λ. D](λ. \+0 \+0) |->
(λ. M, N) K |->

Γ; Δ |- N ⇐ A -| Φ
-----------------------
Γ |- M N ⇒ Δ; Φ -| {N}B

Γ |- M ⇒ T -| Δ
l | Γ; ϕ |- T ≡ ∀

Γ; Δ |- N ⇐ A -| Φ
-----------------------
Γ |- M N ⇒ Δ; Φ -| {N}B

l | [λ. M]C<\l N> |-> [λ. M]C<{N}M> // beta
l | [N]C<\l> |-> [N]C<N>            // subst

Receipt (R) ::=
  | beta
  | var d
  | let N
  | r-lambda
  | r-apply-l
  | r-apply-r
  | r-let
Term (M, N) ::=
  | \n // var
  | λ. M // lambda
  | M N // apply
  | [N]M // let
  | [#R]M // receipt

(λ. M) N |-> [#beta][N]M  // beta
[N]M |-> [#let N]{[#var]N}M  // subst

λ. [#R]M |-> [#r-lambda][#R](λ. M) // r-lambda
[#R]M N |-> [#r-apply-l][#R](M N) // r-apply-l
M [#R]N |-> [#r-apply-r][#R](M N) // r-apply-r
[[#R]N]M |-> [#r-let][#R][N]M  // r-let

(λ. λ. \0 \1) N K
[#beta][N](λ. \0 \1) K

[#beta][#let N](λ. [#var 1]N \0) K
[#beta][#let N][#beta][K]([#var 1]N \0)
[#beta][#let N][#beta][#let K]([#var 1]N [#var 0]K)
[#beta :: #let N :: #beta :: #let K]([#var 1]N [#var 0]K)
[#beta :: #let N :: #beta :: #let K]([#var 1]N [#var 0]K)


Term (M, N) ::=
  | \+x // global var
  | \-n // local var
  | M N // apply
  | λ. M // lambda
  | [N]M // let

l | L<λ. M> N |-> L<[N]M> // beta
l | [N]C<\#> |-> [N]C<N> // subst
l | [N]M |-> M  (\-0 ∉ fv M) // gc



l | λ. \+(1 + l)
l | λ. \-0

l | \+(1 + l)
l | λ. \-0

x : Nat;
x = f x;

2 + l | [\+(1 + l)][f \+l] |- M
2 + l | [\+(1 + l)][f \+l] |- \-0
2 + l | [\+(1 + l)][f \+l] |- f \+l
2 + l | [\+(1 + l)][f \+l] |- f (\+(1 + l))
2 + l | [\+(1 + l)][f \+l] |- f (f \+l)



Term (M, N) ::=
  | \l // var
  | λ. M // lambda
  | M N // apply
  | [N]M // let

Atom (A) ::=
  | \l
  | \n
Compute (C) ::=
  | A
  | C A

// lift lambdas
(λ. M) N |-> [N]M // beta
M (λ. N) |-> [λ. N]([↑]M \l) // closure conversion

// flatten apply
l | M N K |-> [M N](\l [↑]K) // anf-l
l | M (N K) |-> [N K]([↑]M \l) // anf-r

// lift substs
l | λ. [λ. N]M |-> [λ. λ. N](λ. [\l \(1 + l)]M)  // out-lambda
l | [K]M C |-> [K](M C)                          // out-apply-l
l | C [K]N |-> [K](C N)                          // out-apply-r
l | [[K]N]C |-> [K][N]C                          // out-substs

([A]M [B]N)
[A](M [B]N)
[A][B](M N)

Atom (A) ::=
  | \+l // heap var
  | \-n // stack var
Compute (C) ::=
  | A A // apply
  | A
Body (B) ::=
  | [C]B // stack let
  | C
Declaration (D) ::=
  | λ:A. B // lambda
  | ∀:A. B // forall
  | B
Program (P) ::=
  | [D]P // heap let
  | D

[C]

x : Nat;
x = 1 + x

\n[↑b : d] |-> \(n + b)  (d <= n)
\n[↑b : d] |-> \n  (d > n)

Type : Type
Prop : Type

KForall : (Kind -> Kind) -> Kind;

∀A. K A

Forall : (Type -> Type) -> Type;
Forall = K =>
  (f : Forall K) & Type -> f;

T_id : Type;
T_id = (id : T_id) & ((A : Type) & Type) -> ;

Exists : A -> B -> Type;
Exists =

(A : Type) & A == String



False : Type;
False = (f : False) &

(f : False) => (
  (f_t, f_v) = f;
)
(A : Type) -> A

id = (A : Type) => ()

KNat : Kind;
kzero : KNat;
ksucc : KNat -> KNat;

KNat = (n : Nat) &
  ((n == kzero) -> Type) ->
  () -> ;


(x : A) -> B : Type

(x : ) -> Nat

Γ |- A : Type  Γ |- B : Type
----------------------------
Γ |- A -> B : Type


Atom (A) ::=
  | \+l
  | \-n
Compute (C) ::=
  | A
  | A A

[A]B |-> A; B
[A¹ A²]B |-> A²; A¹; APPLY; B

add = λ. λ. \-1 + \-2;

[A B]M
(λ. )

(λ. λ. M)

λ:A. B
ω

Stream : Type;
Stream = (A : Type) -> (w : (next : Stream) -> A) -> A;

inf : Stream;
inf = A => w => w inf;


Term (M, N) ::=
  | x | x => M | M N
  | x = N; M // let
  | M := N // mutate

L<x => M> N |-> L<x = N; M> // beta
x = L<N; C<x> |-> x = N; C<N> // subst
M := K |-> K // ignore
x = N; M |-> M  (x ∉ fv M) // gc

x = N; C<x := K> |-> x = K; C<x>  (x ∉ fv C) // reuse

Term (M, N) ::=
  | x | x => M | M N
  | x = N; M // let

L<x => M> N |-> L<x = N; M> // beta
x = L<N; C<x> |-> x = N; C<N> // subst
x = N; M |-> M  (x ∉ fv M) // gc

x = N; C<K> |-> x = K; C<x>  (x ∉ fv C) // reuse
x = N; C<y = K; M> |-> y = K; C<M>  (x ∉ fv C) // reuse


K ≡ x = K; x


x = N; (x := K)
y = K; (x := K)

y = N; (x => (x := K)) y
y = N; x = y; (x := K)

y = N; x = y; K
y = N; x = y; (y := K)
y = K; x = y; y

Term (M, N) ::=
  | x | x => M | M N
  | M[x := N] // subst


f = x => M; L<x> N |-> f = x => M; L<M[x := N]> // beta
x => M |-> f = x => M; L<M[x := N]> // lift
C<x>[x := L<N>] |-> L<C<N>[x := N]> // subst

(x => M) N |-> M[x := N] // beta

C<x>[x := N] ≡ C<N>[x := N] // subst
M[x := N] ≡ M  (x ∉ fv M) // gc
M[x := x] ≡ M

M[x := N][y := K] ≡ M[y := K][x := N]   (x ∉ fv K) && (y ∉ fv N) // comm

x => M[y := N]    ≡ (x => M)[y := N]    (y ∉ fv N)
M[x := K] N       ≡ (M N)[x := K]       (x ∉ fv N)
M N[x := K]       ≡ (M N)[x := K]       (x ∉ fv M)
M[x := N[y := K]] ≡ M[x := N][y := K]   (y ∉ fv M)

M[x := N][y := K]
M[x := N[y := y]][y := K]
M[x := N[y := K]][y := K]
M[x := N[y := K]]

M[x := K] N
(M[x := K] N)[x := K]
(M[x := x] N)[x := K]
(M N)[x := K]

// TODO: deduplicate

(x => x x) (x => x x)
f = x => x x; f (x => x x)
f = x => x x; f f

---------------------------------------------
Γ[x : (A, B)] |- fst x : A -| [x : (True, B)]

---------------------------------------------
Γ[x : (A, B)] |- snd x : B -| [x : (A, True)]

------------------------------------
[x : (True, True)] |- drop : True -|

(λ. \-0 \-1) \+l
(λ. \-0 \-1)

[A]((λ. M) N) |-> [A]((λ. M) N)

[A][[K]N]M N ≡ [A](•)<[[K]N]M N>

[[K]N]M N |->


{ x : A; y : A } | { z : A }

a = (x : A) -> B;
{ x : a; y : a } | { z : a }

<|

f <| x

| true => 1
| false => 0

[A] | [K]M N
[A][K] | M N

C<(λ. λ. M) N>
C<(λ. λ. M) N K>
C<[N][K]M>

[f := x => M]<f z> == [g := y => N]<g z>
----------------------------------------
[f := x => M]<f> == [g := y => N]<g>

f = x => _A[x];
g = y => z => y;

f == g
f a == g a
_A[a] == g a
_A[x] == g x


Var (x, y)

Term (M, N) ::=
  | x // var
  | x => M // lambda
  | M N // apply
  | x = N; M // let

(x => M) N |-> x = N; M // beta
x = N; C<x> |-> x = N; C<N> // subst
x = N; M |-> M  (x ∉ fv M) // gc

Value (V) ::=
  | x => M

Heap (H<•>) ::=
  | •
  | x = V; H

incr = p => (
  (x, y) = p;
  (1 + x, 1 v y)
);

p = (5, 12);
(x, y) = p;
(1 + x, 1 + y)

p = (5, 12);
p2 = (6, 13);
// stack
p2

p = (6, 13);
// stack
x = 5;
y = 12;
p

(s2 | r1 | p) = (5, 12);
(x, y) = p;
(1 + x, 1 + y)

f = x => (
  y = 1 + 2;
  M
);

y = 1 + 2;
f = x => M;

(x => y = N; M) |-> y = N; (x => M)  (x ∉ fv N)

H<M>

x = 2; x + x
x = 2; x + 2
x = 2; 2 + x
x = 2; 2 + 2
2 + 2

[2 | ]

Γ[M : A | B] |- M : A
------------------------
Γ[M : A | B] : Context

(x : A) => M

Type : Type;

(∀²) : (Type -> Type) -> Type;
(∀²) = B => Type -> B;
(∀²) = (B : Type -> Type) => Type ->



apply : (A : Type) -> (B : Type) ->
  ((x : A) -> B) -> A -> B;


lambda : forall (A => (Type -> B A) -> A -> B A);

ΠA. (A -> Type) -> Type
arrow
inter : (A : Type) ->
forall : (A : Type) ->
λ.


Kind (K) ::= Linear | Data
Type (A, B) ::=
  | A -> B
  | x
  | ∀x : K. B
Term (M, N) ::=
  | x
  | x => M
  | M N
  | <A> => M
  | M<A>
  | (M :> Data)
  ;


data ⊥ : Set where

SizeLt : (i : Size 0) -> Type 1;
SizeLt = i =>
  (A : Type 0) -> (w : (j : Size 0 < i) -> A) -> A;

eq! : ∀ (i j : SiBze 0) → i ≡ j
eq! _ _ = trustMe

cast : ∀ i j → SizeLt i → SizeLt j
cast i j = subst SizeLt (eq! i j)

foo : ∀ (i : Size 0) → SizeLt i → ⊥
foo i [ j ] = foo j (cast i j [ j ])

test : ⊥
test = foo ∞ [ ∞ ]


Stream : (i : Size) -> Type;
seq : (i : Size) -> Stream i;

Stream = (i : Size) =
  (s : Stream i) &
    (P : (s : (j : Size < i) -> Stream j) -> Type) ->
    (w : (s : (j : Size < i) -> Stream j) -> P (seq i)) -> P s;
seq = i => P => w => w seq;

Nat : Type;
Nat = (A : Type) -> (z : A) ->
  (s : (p : Nat) -> A) -> A;

Nat : (i : Size) -> Type;
Nat = (A : Type) -> (z : A) ->
  (s : (j : Size < i) -> (p : Nat j) -> A) -> A;

Stream : Type;
Stream = (A : Type) ->
  (s : (p : Lazy Stream) -> A) -> A;

Stream : (i : Size) -> Type;
Stream = (A : Type) ->
  (s : (n : (j : Size < i) -> Stream j) -> A) -> A;

Stream : Type;
Stream = (A : Type) ->
  (s : (p : () -> Stream) -> A) -> A;

Stream : (i : Size) -> Type;
Stream = (A : Type) ->
  (s : (n : (j : Size < i) -> () -> Stream j) -> A) -> A;

(M : A) == (N : B)

Value ::=
  | x ...L

f = x => x;
g = _A;
M

g = _A;
M{f := x => x}

N32 =
  | Z : N32
  | S : (pred : N32) -> N32
  | mod : 4294967296 == Z;


f = (b : Bool) => (x : b <| | true => Nat | false => String) =>
  b <|
  | true => Nat.to_string (1 + x)
  | false => x;
// monomorphization
f_true = (x : Nat) => Nat.to_string (1 + x);
f_false = (x : String) => x;

foldr = (f, b, xs) => (
  walk = l =>
    l <|
    | nil => b
    | (x :: xs) = f (x, walk xs);
  walk xs
);


#P = (f, b);
foldr = (f, b, xs) => walk #P xs;
walk = #P => l =>
    l <|
    | nil => b
    | (x :: xs) = f (x, walk xs);

foldr = (f, b, xs) => walk _P xs;
walk = _P => l =>
    l <|
    | nil => b
    | (x :: xs) = f (x, walk xs);

mul = (x, y) => (
  loop = z =>
    z = 0 <|
    | true => 0
    | false => add_to_x z;
  add_to_x = z => x + loop (z - 1);
  loop y
);

mul = (x, y) => mul_loop x y;
mul_loop = x => z =>
  z = 0 <|
  | true => 0
  | false => mul_add_to_x x z;
mul_add_to_x = x => z => x + loop (z - 1);

f = x => y => z => a => (
  g = a => a + x + y;
  h = a => g a + y + z;
  h a
);

// oh but quadratic
f = x => y => z => a => f_h (x, y, z) a;
f_g = (x, y, z) => a => a + x + y;
f_h = (x, y, z) => a => g (x, y, z) a + y + z;

// sharing dumbass
#P = (x, y, z);
f = x => y => z => a => f_h #P a;
f_g = #P => a => a + x + y;
f_h = #P => a => g #P a + y + z;

// specialization
#P = (x, y, z);
#P2 = (x, y);
f = x => y => z => a => f_h #P a;
f_g = #P2 => a => a + x + y;
f_h = #P => a => g #P2 a + y + z;


Unit : Type;
unit : Unit;

Unit = (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u;




_A == fv (x => M)

(x => f = N; M)

f = _P => N; // _P == fv N
x => M{f := f _P}

Term ::=
  | x
  | x => M
  | x N
  | x = N; M

f = x => M; C<f N> |-> f = x => M; C<x = N; M> // beta

Type : Kind;
Type = n => Type n & Type (1 + n);

Type : (n : Level) -> Type (1 + n);
Type = n => Type n & Type (1 + n);

-------------------
Type n & Type (1 + n) & (Type 2 + n) ... : Type (1 + n)

Type : (n : Level) -> Type (1 + n) & Type (2 + n);

Type 0

Int : Type 0

ω
⇑id id

(x => M) N |-> f = x => M; f N // lift
f = N; M |-> M  (f ∉ fv M) // gc

Unit : (i : Size) -> Type;
unit : (i : Size) -> Unit i;

Unit = i =>
  (u : (j : Size < i) -> Unit j) &
  (P : (u : (j : Size < i) -> Unit j) -> Type) -> (x : P unit) -> P u;
unit = i =>
  (P : (u : (j : Size < i) -> Unit j) -> Type) => (x : P unit) => x;

Unit 0 = (u : (j : Size < i) -> Unit j) &
  (P : (u : (j : Size < i) -> Unit j) -> Type) -> (x : P unit) -> P u

Unit : (i : Size) -> Type;
unit : (i : Size) -> Unit i;

Unit = (u : (j : Size < i) -> Unit j, i : <P>(x : P unit) -> P u);
unit = (u = unit, i = <P>(x) => x);

Z3 =
  | Z
  | S (pred : Z3)
  | mod3 : Z == S (S Z);


P u -> P (fst u)
eq : (u : Unit) -> u == fst u;

eq1 = u => (snd u)<u => u == fst u>(refl);

Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool, i : <P>(x : P true, y : P false) -> P b);
true = (b = true, i = <P>(x, y) => x);
false = (b = false, i = <P>(x, y) => y);



eq : u ≡ fst u;

Self : (A : Type) -> (B : (x : A) -> Type) -> Type
  = (x : A, r : B l)
T_Unit : Type;
T_Unit =
  (Unit : T_Unit,
    (T_unit : Type) -> (unit : T_unit) -> Type);

Unit : Type;
Unit = (T_unit : Type) => (unit : T_unit) =>
  (u : T_unit, i : <P>(x : P unit) -> P u);

T_unit : Type;
T_unit = (unit : T_unit, Unit T_unit unit);

unit : T_unit;
unit = (unit = unit, (u = unit, i = <P>(x : P unit) => x));


Unit : Type;
unit : Unit;

Unit = (u : Unit, i : <P>(x : P unit) -> P u);
unit = (u = unit, i = <P>(x : P unit) => x);

ind_ind : (
  A : (A : Type) -> Type,
  B : (B : (x : A) -> ) ->
)

ind_ind : (
  A : (A : Type) -> Type,
  B : (B : ) -> Type,
)

Self_W : <A, B>(
  x : (x : A) & B x,
  w : A == (x : A) & B x
) -> x == x.0;

f = (u : Unit) => (eq : u == u.0) => M;
f u eq


(u : Unit) => (M eq u : P eq u)
(u : Unit) => (M refl (fst u) : P refl (fst u))

eq1 : (b : Bool) -> fst b == fst (fst b);
eq2 : (b : Bool) -> snd b (x, y) == snd (fst b);

P (fst b)

C_Bool = <A>(x : A, y : A) -> A;
c_true : C_Bool = <A>(x, y) => x;
c_false : C_Bool = <A>(x, y) => y;

Bool : Type;
true : Bool;
false : Bool;

Bool = (
  b : <A>(x : A, y : A) -> A,
  i : <P>(x : P ctrue, y : P false) -> (P b : Prop)
);
true = (b = <A>(x, y) => x, i = <P>(x, y) => x);
false = (b = false, i = <P>(x, y) => y);

eq0 : (b : Bool) -> b == fst b;
eq1 : (b : Bool) -> fst b == fst (fst b);
eq2 : (b : Bool) -> snd b == snd (fst b);

eq1 = b => (snd b)<b => b == fst b>(refl, refl);
eq =


eq2 : <b> -> snd (fst b) == snd (fst (fst b))
  = <b> => snd b

case : <A>(b : Bool, x : A, y : A) -> A
  = <A>(x, y) => snd b (x, y);
ind : <P>(b : Bool, x : P true, y : P false) -> P (fst b)
  = <P>(b, x, y) => snd b (x, y);


------------------
Γ |- ⇑x :

(u : Unit) => snd u : <P>(x : P unit) -> P (fst u)


Nat : Type & {
  zero : Nat;
  succ : (pred : Nat) -> Nat;
  fold : <A>(z : A, s : (pred : Nat) -> Nat) -> Nat;
};
Nat = <A>(z : A, s : (pred : Nat) -> Nat);
zero = <A>(z, s) => z;

Nat = (
  zero = <A>(z, s) => z;
  succ = pred =>
  Nat & {

  }
);

id : (x : ⊤) -> ⊤ = (x : ⊤) => x;


⇑id : (x : ⊤) -> ⊤

Kind (K) ::=
  | Type
  | K -> K
  | A == B
  | (x : K) & K
Type (A, B) ::=
  | x
  | A -> B
  | <x : K> -> B
  | (x : K) => B
  | A B
  | subst P A
Term (A, B) ::=
  | x
  | (x : A) => M
  | M N
  | <x : K> => M
  | M<x>


Universe (U) ::=
  | Type l

Type (A, B) ::=
  | x
  | A -> B
  | <x : U> -> B
  | <x : U> => B
  | A<B>

Term (M, N) ::=
  | x
  | (x : A) => M
  | M N
  | <x : U> => M
  | M<A>

// type level
-----------------
Γ, x : U |- x : U

Γ |- A : Type a  Γ |- B : Type b
--------------------------------
Γ |- A -> B : Type (max a b)

Γ, x : U |- T : Type l
--------------------------
Γ |- <x : U> -> T : Type l

Γ, x : U |- B : T
--------------------------------
Γ |- <x : U> => B : <x : U> -> T

Γ |- A : <x : U> -> T  Γ |- B : U
---------------------------------
Γ |- A<B> : T[x := B]

// term level
-----------------
Γ, x : A |- x : A

Γ, x : A |- M : B
--------------------------
Γ |- (x : A) => M : A -> B

Γ |- M : A -> B  Γ |- N : A
---------------------------
Γ |- M N : B

Γ, x : U |- M : A
--------------------------------
Γ |- <x : U> => M : <x : U> -> A

Γ |- M : <x : U> -> B  Γ |- A : U
---------------------------------
Γ |- M<A> : B[x := A]


KBool : Kind;
ktrue : KBool;
kfalse : KBool;

KBool : Type 1 =
  <x : Type 0> -> <y : Type 0> -> Type 0;
ktrue : KBool = <x> => <y> => x;
kfalse : KBool = <x> => <y> => y;

Bool : KBool -> Type 0 = kb =>
  <P> -> P<ktrue> -> P<kfalse> -> P<kb>;


Bool = (T : KBool): KBool -> Type 0 = kb =>
  <P> -> P<ktrue> -> P<kfalse> -> P<kb>;

Bool = <A, B, R, K> ->
  (A == R -> K) -> (B == R -> K) -> K

f : (b : Bool Int String R) => (x : R) => x;

g = (b : Bool) => (
  x = f ;
  1
)

kb : Type -> Type -> Type;
x : KBool =
  x => y => kb (false x y) (true x y);

TBool : Type = <P> -> P ktrue -> P kfalse -> P kb;

Unit : Type;
Unit = (u : Unit, c : <A>(x : A) -> A);

Unit : (i : Size) -> Type;
Unit = i => (u : (j : Size < i) -> Unit j, c : <A>(x : A) -> A);

unit : (i : Size) -> Unit i;
unit = i => (u = unit, c = <A>(x) => x);

ω

Type = l => Type 0 &

Cumulative : (l : Level) -> Type ω
Cumulative = l => (
  Type l & Cumulative l
);

(A : Type 0) =>
(A : Type 0 & Type 1)


[[K]N]M
[K][N]M

[N](λ. M) K
[N][K]M

(b : Bool) => (x : b <| | true => Int | false => String) =>
  x

eq : 1 + 1 == 2 = _;

Term (M, N) ::=
  | x
  | (x : A) -> B
  | (x : A) => M
  | M T
  | x = M; N
Checked (T) :=
  | x => T

Value (V) ::=
  | x ...V
  | x => V

(V : _ : _K)

f : (x : _A) -> _B;

Γ |- (f (x : _A : _K)) == (f (y : _B : _K))
Γ |- (f _H1) == (f _H2)


Γ |- A1 == A2  Γ |- B1 == B2
-------------------------------------
Γ |- (x : A1) -> B1 == (x : A2) -> B2



_H1 == (if _K == Prop then • else x)
_H2 == (if _K == Prop then • else y)
Γ |-
  (f (if _K == Prop then • else x)) ==
  (g (if _K == Prop then • else y))

(f, x, g, y) => (x : P (f x)) => (x : P (g y));

f (B x)

isEven n = n (\p -> p (isEven p) true)
isEven (n : _C -> _D) = n (\(p : _C) -> p (isEven p) true)


fix = x => x x;
∀B. μ(R). R -> B
_C
_A == (_C -> _D)



f == (x => f x)
(x => f x) == (x => g x)

_K[x, y]

(f => f ...V)
(f => (x => f x) ...V)
f M == f N
(f (M : A : Prop) : Int) == (f (N : A : Prop) : Int)


record User where
  id : Nat
  name : String

eduardo : User;

eduardo = {
  ...eduardo;
  id = 0;
  name = "Eduardo";
};

x = 1

| x
| 1 => "switch 1" echo
| 2 => "switch 2" echo
| => "what?" echo


Term (M, N) ::=
  | x
  | x => M
  | M N
  | x : A; M
  | x = N; M
  | (x : A) & B
  | (M : A)
  | <A> -> M
  | <A> => M
  | M<A>
  | A & B
  | A | B
  | { x : A; ...y : B; }
  | { x = M; ...y = N; }
  | (x : A, ...y : B)
  | (x = M, ...y = N)
  | M |> N
  | M <| N


Type : Type
Prop : Type
Data : Type
Line : Type

Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool) & <P>(x : P true, y : P false) -> P b;
true = <P>(x, y) => x;
false = <P>(x, y) => y;


Unit : Type;
unit : Unit;

Unit = (u : Unit) & <P>(x : P unit) -> P u
unit = <P>(x) => x;

Unit : (i : Size) -> Type;
unit : (i : Size) -> Unit;

Unit = (u : (j : Size < i) -> Unit j, i : <P>(x : P unit) -> P u)
unit = (u = unit, i = <P>(x) => x);

u == fst u

snd u : <P>(x : P unit) -> P (fst u)


f = (b : Bool) =>
  (x : b <| | true => Int | false => String) =>
  x

f a == λ. f a \-1
f a x == f a x
x => f a x == x => f a x

_A\0
(x\0) =>


foo : () -> foo () -> Type;
foo () = Int

T_foo : Type;
foo : () -> (x : P foo) -> ();


T : Type;
x : T;

T = (P : (x : T) -> Type) -> (x : P x) -> P x;
x = (P : (x : T) -> Type) => (x : P x) => x;

T_foo = foo ();

T = foo @-> () -> foo ();

foo () == foo ()

foo

A : Type;
A = A;

Unit : Type;
unit : Unit;

Unit = (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u;
unit = (P : (u : Unit) -> Type) => (x : P unit) => x;

(P => x => x : (P : (u : Unit) -> Type) -> (x : P unit) -> P unit) :>
  Unit
(P => x => x : (P : (u : Unit) -> Type) -> (x : P unit) -> P unit) :>
  (u : Unit) & (P : (u : Unit) -> Type) -> (x : P unit) -> P u
(P => x => x : (P : (u : Unit) -> Type) -> (x : P unit) -> P unit) :>
  (P : (u : Unit) -> Type) -> (x : P unit) -> P (P => x => x)
(x => x : (x : P unit) -> P unit) :>
  (x : P unit) -> P (P => x => x)
(x : P unit) :>
  P (P => x => x)
(x : P (P => x => x)) :>
  P (P => x => x)


Γ |- @fix()

(P => x => x : (P : (u : Unit) -> Type) -> (x : P unit) -> P unit) :>
  (P : (u : Unit) -> Type) -> (x : P unit) -> P (P => x => x)

(x, y as z) = M;

(x, y as z) = M;


Unit : Type;
fst : (u : Unit) -> Unit;
unit : Unit;

Unit = (K : Type) -> (w : (u : Unit) -> (H : u == fst u) ->
  (i : (P : (u : Unit) -> Type) -> (x : P unit) -> P u) -> K) -> K;
fst = (u : Unit) => u Unit (u => H => i => u);
unit = K => w => w unit refl (P => x => x); // internal fix expansion

Unit : Type;
unit : Unit;

Unit = (K : Type) -> (w : (u : Unit) ->
  (i : (P : (u : Unit) -> Type) -> (x : P unit) -> P u) -> K) -> K;
unit = K => w => w unit (P => x => x);
fst : (u : Unit) -> Unit;


f = (u : Unit) => (H : u == fst u) => _;
Sigma : (A : Type) -> (B : (x : A) -> Type) -> Type;
pair : <A, B>(x : A, y : B x) -> Sigma A B;
fst : <A, B>(x : Sigma A B) -> A;

Sigma = A => B =>
  (K : Type) -> (w : (x : A) -> (y : B x) -> K) -> K;


Unit : Type;
unit : Unit;

Unit = (u : Unit, i : (P : (u : Unit) -> Type) -> (x : P unit) -> P u);
unit = (u = unit, i = P => x => x);





same : (u : Unit) -> fst u == fst (fst u) =
  u => u (u => u == fst u) x refl;

u  Unit |- M u : A u |-> M (fst u) : A (fst u)

(u : Unit) -> (W : fst u == fst (fst u))

f = (u : Unit) =>

(x : (eq : fst u == unit) -> False) -> fst u == unit

(x : (eq : u == unit) -> False) -> u == unit
unit = K => w => w unit (P => x => x);


ind : (u : Unit) -> Typ

Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool) & <P>(x : P true, y : P false) -> P b;
true = <P>(x, y) => x;
false = <P>(x, y) => y;

Bool : Type;
true : Bool;
false : Bool;

Bool = (
  b : Bool,
  i : (P : (b : Bool) -> Type) -> (x : P true) -> (y : P false) -> P b
);
true = (b = true, i = P => x => y => x);
false = (b = false, i = P => x => y => y);

Bool = (b : Bool, w : b == fst b : Prop);



ind = (u : UnitH) => fst

_M N == _B

_M := λ. _B

Γ[(M : C) :> A] |- (M : C) :> A
Γ |- (M : C) :> B[x := (M :> A)]
--------------------------------------- // Self Gen
Γ |- (M : C) :> (x : A) & B

Stream : (a : Size) -> Type;
seq : (a : Size) -> (n : Stream a) -> Stream a;

Stream = a => (s : Stream a) &
  <P>(x : (n : Stream a) -> P (seq a n)) -> P s;
seq = a => n =>
  <P>(x : (n : Stream a) => P (seq a n)) => P x;






// works by coercion
term : seq;
received : (a : Size) -> (n : Stream a) ->
  <P>(x : (n : Stream a) -> P (seq a n)) -> P (seq a n);
expected : (a : Size) -> (n : Stream a) -> Stream a;

term : seq a n;
received :
  <P>(x : (n : Stream a) -> P (seq a n)) -> P (seq a n);
expected : <P>(x : (n : Stream a) -> P (seq a n)) -> P s;
Inf =



Bool = <A>(x : A, y : A) -> A;
true : Bool = <A>(x, y) => x;
false : Bool = <A>(x, y) => y;

Bool =
  | true
  | false;

ind : (b : Bool) -> <P>(x : P true, y : P false) -> P b;

Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool) & <P>(x : P true, y : P false) -> P b;
true = <P>(x, y) => x;
false = <P>(x, y) => y;
ind : (b : Bool) -> <P>(x : P true, y : P false) -> P b
  = b => b;

Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool, i : <P>(x : P true, y : P false) -> P b);
true = (b = true, i = <P>(x, y) => x);
false = (b = false, i = <P>(x, y) => y);

ind : (b : Bool) -> <P>(x : P true, y : P false) -> P (fst b);

Bool1 = (b : Bool, w : b == fst b : Prop);

(b : Bool, w : b == fst b : Prop)

(a : Bool1) => (b : Bool1) => (H : fst a == fst b) =>
  (_ : a == b);

Γ |- M : (x : A, y : B x)
-------------------------
Γ |- snd M : B (fst M)

(x : A : Prop) => (y : A : Prop) => (refl : x == y);

CCω

Unit : (i : Size) -> Type;
unit : (i : Size) -> Unit i;

Unit = i => (j : Size < i) ->
  (u : Unit j) & <P>(x : P (unit j)) -> P u;
unit = i => j => <P>(x : P (unit j)) => x;


fst u i == fst unit i
snd u == snd u

unit == fst unit

(P : (i : Size) -> (x : A i) -> Type) ->

unit = fst unit

βη



T_Bool : (T_b : (Bool : Type) -> Type) -> Type;
T_Bool = T_b => (Bool : T_bool T_b) &
  (true : T_b, false : T_b) -> Type

T_b :
Bool : T_Bool ;
Bool = true => false => (b : T_b Bool) &
  <P>(x : P true, y : P false) -> P b;


T_Bool : Type;
T_Bool = (
  T_true : (Bool : T_Bool) -> Type;
  T_false : (Bool : T_Bool) -> (true : T_true Bool) -> Type;

  T_Bool = (Bool : T_Bool) &
    (true : T_true Bool) -> (false : T_false Bool true) -> Type;
  T_true = Bool => (true : T_true Bool) &
    (false : T_false Bool true) -> Bool true false;
  T_false = Bool => false => (false : T_false Bool true) &
    Bool true false;
)

Bool = true => false => (b : Bool true false) &
  <P>(x : P true, y : P false) -> P b;


Bool : Type;
Bool = (
  true : (false : Bool) -> Bool;
  true = (false : Bool) =>
    <P>(x : P (true false), y : P false) => x;

  false : Bool;
  false = <P>(x : P (true false), y : P false) => y;

  (b : Bool) & <P>(x : P (true false), y : P false) -> P b
);


Unit : Type;

T_H2 : (unit : Unit) -> Type;

H1 : (unit : Unit) -> (H2 : T_H2 unit) ->
  (x : <P>(x : P unit) -> P unit) -> Unit;
H2 : (unit : Unit) -> T_H2

Unit = (
  unit : Unit;
  unit = H1 (<P>(x : P unit) => x);

  <P>(x : P unit) -> P unit
);





Bool : Type
true : Bool
false : Bool

Bool = (b : Bool) &
  <P>(x : P true, y : P false) -> P b;
true = <P>(x, y) => x;
false = <P>(x, y) => y;


Bool : Type
true : Bool
fst :
Unit = <K>(w :
  (u : Unit) -> (H : u == fst)
  (i : <P>(x : P unit) -> P u) -> K) -> K
unit =

Unit = (
  u : <A>(x : A) -> A,
  i :
);
```

## Self Again

```rust
Bool : (n : Nat) -> Type;
true : (n : Nat) -> Bool n;
false : (n : Nat) -> Bool n;

Bool = n =>
  n Type ()
```

This is a demo of how self types / dependent intersections can be erased to coinduction + induction-induction + UIP

The initial goal code is, where `&` is a dependent intersection.

```rust
Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool) & (P : (b : Bool) -> Type) ->
  (then : P true) -> (else : P false) -> P b;
true = P => then => else => then
false = P => then => else => else

ind : (b : Bool) -> (P : (b : Bool) -> Type) ->
  (then : P true) -> (else : P false) -> P b;
ind = b => b;
```

But it does a detour by stratified induction with an infinite negative hierarchy, which can be done by having coinductive pairs.

```rust
Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool, i : (P : (b : Bool) -> Type) ->
  (then : P true) -> (else : P false) -> P B);
true = (b = true, P => then => else => then)
false = (b = false, P => then => else => else);

ind : (b : Bool) -> (P : (b : Bool) -> Type) ->
  (then : P true) -> (else : P false) -> P (fst b);
ind = b => b;
```

This encodes self types in Agda
There is a couple ideas,

- self types can be seen as a recursive dependent intersection
  A : Type;
  A = (x : A) & B; // this is the same as a self
- self types are like coinductive pairs equipped with an equality
  stating that the p = fst p
  A0 : Type;
  A0 = (x : A0, y : B);

  A = (x0 : A0, w : A0 == fst x0);

- pairs where the right side is irrelevant are
  defined by the left side, such that fst a = fst b -> a = b

The equality needs to be a strict equality as it needs to be irrelevant.

Induction-induction is likely not needed if there is sigma
Sigma is likely not needed if there is induction-induction
Agda doesn't have universe lifting so large elimination
will fail without --type-in-type

The major insight is that coinduction is how you introduce
infinite self references in types.
The equality needs to be a strict equality as it needs to be irrelevant.

If I'm correct and sigma is not needed
this leads to a very nice proof of
strong normalization of a bunch of things.

- Church-style annotated self types
- Church-style dependent intersections
- A bunch of PTS + Fix + ADTs, like the CIC
  are reduced to the PTS + Fix side.

As you can then erase those to a theory with
a very powerful fixpoint, strict equality
and universes with lifting for large elimination.

```agda
{-# OPTIONS --guardedness #-}

module stuff where

open import Agda.Builtin.Equality

symm : ∀ {l} {A : Set l} {n m : A} → n ≡ m → m ≡ n
symm refl = refl

record Pack {l} (A : Set l) (fst : A → A) : Set l where
  constructor pack
  field
    x : A
    w : x ≡ fst x

uip : ∀ {l} {A : Set l} {m n : A} (a b : m ≡ n) → a ≡ b
uip refl refl = refl

eq_x_is_enough : ∀ {l} {A : Set l} {fst} (l r : Pack A fst) →
  Pack.x l ≡ Pack.x r → l ≡ r
eq_x_is_enough (pack l l_w) (pack r r_w) H
  rewrite H rewrite uip l_w r_w = refl

lower_w : ∀ {l} {A : Set l} (fst : A → A) {x} →
  x ≡ fst x → fst x ≡ fst (fst x)
lower_w {_} {_} fst H rewrite symm H = H

lower : ∀ {l} {A : Set l} {fst} → Pack A fst → Pack A fst
lower {l} {A} {fst} (pack x w) = pack (fst x) (lower_w fst w)

lower_is_same : ∀ {l} {A : Set l} {fst} (P : Pack A fst) → P ≡ lower P
lower_is_same (pack x w) = eq_x_is_enough _ _ w

record W_Bool : Set₁
w_true : W_Bool
w_false : W_Bool

{-# NO_POSITIVITY_CHECK #-}
{-# NO_UNIVERSE_CHECK #-}
record W_Bool where
  coinductive
  field
    -- ideally this should also be erasable
    b : W_Bool
    i : ∀ {l} (P : W_Bool → Set l) → P w_true → P w_false → P b
W_Bool.b w_true = w_true
W_Bool.i w_true P then else = then

W_Bool.b w_false = w_false
W_Bool.i w_false P then else = else

Bool : Set₁
Bool = Pack W_Bool W_Bool.b

true : Bool
true = pack w_true refl

false : Bool
false = pack w_false refl

ind_lower : ∀ b {l} (P : Bool → Set l) → P true → P false → P (lower b)
ind_lower (pack x w) P then else = W_Bool.i x
  (λ { x → ∀ w → P (pack x w) })
  (λ { refl → then }) (λ { refl → else }) _

ind : ∀ b {l} (P : Bool → Set l) → P true → P false → P b
ind b P then else rewrite lower_is_same b = ind_lower b P then else
```

```rust
Unit : (i : Size) -> Type;
unit : (i : Size) -> Unit i;

Unit = i => (
  u : (j : Size < i) -> Unit j,
  i : <P>(x : P unit) -> P u
);
unit = i => (
  u = unit,
  i = <P>(x : P unit) => x
);

// very bounded and well founded
Unit : (i : Size) -> Type;
unit : (i : Size) -> (j : Size < i) -> Unit j;

Unit = i => (
  u : (j : Size < i) -> Unit j,
  i : <P>(x : P (unit i)) -> P u
);
unit = i => j => (
  u = unit,
  i = <P>(x : P (unit j)) => x
);

Unit = i =>
  (u : Unit i, w : (i : Size) -> u == fst (u i))
ind : (u : (i : Size) -> Unit i) ->
  (i : Size) -> <P>(x : P unit) -> P (fst (u i))

u : (i : Size) -> Unit i
fst (u i) : (j : Size < i) -> Unit j

u : (j : Size < i) -> Unit i
fst (u i) : (j : Size < i) -> Unit j

u (j :> Size) == fst (u i) j
u == fst (u i)

fst (u k) : (j : Size < i) -> (
  u : Unit j,
  i : <P>(x : P (unit j)) -> P u
);

PUnit = i => (u : Unit i, w : )


S_Unit : (i : Size) -> Type;
s_unit : (i : Size) -> S_Unit i;

S_Unit = i => (j : Size < i) ->
  (u : Unit j) & <P>(x : P (unit j)) -> P u;
s_unit = i => j =>
  <P>(x : P (s_unit j)) => x;

Unit = (i : Size) -> S_Unit (1 + i);
unit : Unit = (i : Size) => s_unit (1 + i) i;

ind : (u : Unit) -> <P>(x : P unit) -> P u
  = u => u (1 + i) i;




Sigma : (B : (x : Type) -> Type) -> (x : Type) -> Type;
Sigma = B => x =>
  (P : (x : Type) -> Type) ->


Level : Univ;
Type l : Univ;

KUnit : Type;
KUnit =

C_Unit : Type = <A>(x : A) -> A
c_unit : C_Unit = <A>(x) => x;

I_Unit : (c_u : C_Unit) -> Type = c_u => (
  x : <P>(x : P c_unit) -> P c_u
  w : (A : Type) -> fst u A == snd u (_ => A);
  y : x
);
c_unit : I_Unit c_unit = <P>(x) => x;


f : <A, B>(x : A, y : B) -> A

Unit = (
  c_u : C_Unit;
  i_u : (P : (u : C_Unit) -> Type) -> (x : P c_unit) -> P c_u;
  w1 : (A : Type) -> fst u A == snd u (_ => A);
  w2 : fst u Type
    (i_u ~~~ i_true)
    (i_u ~~~ i_false)
);

I_Unit u : isProp
w : fst l == fst r -> l == r

i_u : (P : (c_u : C_Unit) -> Type) -> (x : P c_unit) -> P c_u
i_u (c => (refl : c == c_u) -> P (c_u, i_u))
  ((eq) => )
  refl

i_u : (P : (c_u : C_Unit) -> Type) -> (x : P c_unit) -> P c_u
i_u (c_u => (i_u : _) -> P (c_u, i_u))
  (i_u => (_ : P (c_unit, i_u)))
  i_u

IUnit = (
  u : Unit,
  w : (A : Type) -> (x : A) => x == snd u (_ => A) x
);

fst l == fst r ->
M : A
fst M : B
B[x := fst M]

B[x := M]

Type : Type
Prop : Type
Data : Type
Size : Type
Line : Type

(M : A : Data)
(M : A : Type)

id = <A>(x : A) => x;

to_nat =
  | false => 0
  | true => 1;

Either = (A, B) =>
  | (tag == "left", payload : A)
  | (tag == "right", payload : B);

n = 1 + n;
(n > 0) <|
| true => _
| false => _

read : () -(Disk | Log)> String;

Nat : (i : Size < ∞) -> Type;
Nat = i => <A>(z : A, s : (j : Size < i) -> (pred : Nat j) -> A) -> A;

Stream : (i : Size) -> Type;
Stream = i => <A>(
  s : (pred : (j : Size < i) -> Stream j) -> A
) -> A;

fold : (i : Size) -> <A>(n : Nat i, z : A, s : (acc : A) -> A) -> A;
fold = i => <A>(n, z, s) =>
  n(z, j => pred => s(fold(pred, z, s)));

b : (b : Bool) & (P : (b : Bool) -> Type) ->
  (x : P true) -> (y : P false) -> P b

⇑b : (b : ⇑Bool) & (P : (b : ⇑Bool) -> Type) ->
  (x : P ⇑true) -> (y : P ⇑false) -> P b

⇑b : (P : (b : ⇑Bool) -> Type) ->
  (x : P ⇑true) -> (y : P ⇑false) -> P ⇑b


T_Bool : Type;
T_Bool = Prop -> Prop -> Prop;
t_true =

P_Unit : Prop;
P_unit :

Bool = (x : A) -> (y : A) -> A;
GBool = (G : Grade) -> ((x : A) -> (y : A) -> A) $ G;

Many : (A : Line) -> Line;
Many = A => (m : Many A) & (l : Many A, r : A, w : l == m);

many : <A>(g : A) -> (i : Size) -> Many A i;
many = <A>(g) => i => (
  g = (G : Grade) => g (1 + g);
  (l = many , r = g (1 + n))
)

Bool = (G : Grade) -> ((x : A) -> (y : A) -> A) $ G;

Bool : (i : Size) -> Line;
Bool = (l : (j : Size < i)  Bool j, r : (x : A) -> (y : A) -> A);

Fix : Line;
Fix = (l : Fix, r : (x : Fix) -> False);

fix = (x : Fix) => x

CBool = (i : Size) -> Bool i;

gtrue : GBool;
gtrue = (l = gtrue, r = true);

Bool : Type;
Bool = (b : Bool, )
f : () ->


---------------
Γ |- ⊥ : Type 0

Γ |- T : Type 0  Γ |- M : T  Γ |- A : Type l
--------------------------------------------
Γ |- M⇑A : T[⊥ := A] : Type l

Bool : Type 1;
true : Bool;
false : Bool;

Bool = (b : Bool) &
  (P : (b : Bool) -> Type 0) -> P true -> P false -> P b;
true : Bool = x => y => x;
false : Bool = x => y => x;

(b : Bool) &
  (P : (b : Bool) -> Type 0) -> P true -> P false -> P b

not = (b : Bool) => b@Bool true false;


Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool) & (P : (b : Bool) -> ⊥) ->
  (x : P true) -> (y : P false) -> P b;


(b : Bool) =>
  ⇑b

Bool : Type =
  Prop -> Prop -> Prop &
  Bool -> Bool -> Bool;

Unit : Type = (A : Type) -> A -> A;
Bool : Type = (A : Type) -> A -> A -> A;

(b : Bool) => b Bool l r
(b : Bool) => A => x =>
  b ((A : Type) -> A -> A)
    (A => x => l A x)
    (A => x => r A x)
    A x
(b : Bool) => A => x =>
  b (A -> A)
    (x => l A x)
    (x => r A x)
    x

// eta
(b : Bool) => A => B => b.1 l r A B
(b : Bool) => A => B => b.1
  (A => B => l A B)
  (A => B => r A B) A B
// why???
(b : Bool) => A => B => b (l A B) (r A B)


PBool : Prop = (A : Prop) -> A -> A -> A;

Bool : Type = (T : TBool, n : PBool);

Bool : Type = (T : TBool, n : PBool);

(b : Bool) => b Bool b b
// eta on pair
b =>
  (
    fst b TBool (fst b) (fst b),
    snd b PBool (snd b) (snd b)
  )
b =>
  (
    A => B => fst b TBool (A => B => fst b A B) (A => B => fst b A B) A B,
    snd b PBool (snd b) (snd b)
  )

Any : _;
Any = Empty &
  Empty -> Set &
  Set -> Empty &
  Set -> Set ...;

Bool = Any -> Any -> Any;

Eq : (A : Type) -> (x : A) -> (y : A) -> Type;
refl : <A>(x : A) -> Eq A x x;

Eq = A => x => y =>
  (eq : Eq A x x) &
  (P : (z : A) -> (x : Eq A x x) -> Type) ->
    (v : P x (refl x)) -> P y l;
refl = <A>(x) => (P) => (v) => v;

(eq : Eq A x x) =>
  (eq : (P : (z : A) -> (x : Eq A x x) -> Type) ->
    (v : P x (refl x)) -> P y eq)
eq.1
SEq = (
  eq : Eq A x y,
  w : (x : Eq A x y) -> eq == fst eq
);

eq.0 : (P : (z : A) -> (x : Eq A x x) -> Type) ->
  (v : P x (refl x)) -> P y (eq.0)
a => (left : Eq A x a) =>
    left (z => _ => (eq : Eq A x z) -> Type)
      (eq => (P : (eq : Eq A x x) -> Type) -> (v : P (refl A x)) -> P eq)
      eq

eq ()


G : (F : (A : Type) -> Type) -> Type;
G = F => F Prop & G F -> G F;

Prop -> Prop

Unit = G (A => A -> A -> A);

Unit = Prop -> Prop;


λ
Bool : =
------------------
Γ |- ∀(A : ).


(x : CNat) -> (y : INat x)
λPω

f x + f x

y = f x; y + y

[N : 0](λ. M)
(λ. [N : 1]M)


Sort (S) ::=
  | Prop
  | Type

Term (M, N) ::=
  | Prop
  | x
  | (x : A) -> B
  | (x : A) => M
  | M N;

(x : A : Prop) -> (B : Type)

(Prop : Type) |-> *
(x : A : Type) |-> •
((x : A : Type) -> B : Type) |-> A -> B
((x : A : Prop) -> B : Type) |-> () -> B
(x => M -> (x : A : Type) -> B : Type) |-> λ(x : A). M
(x => M -> (x : A : Prop) -> B : Type) |-> M
(x => M -> (x : A : Large) -> B : Type) |-> λ(xT : _, xP : _). M
(M (N : A : Type) : T : Type) |-> M N
(M (N : A : Prop) : T : Type) |-> M
(M (N : A : Large) : T : Type) |-> M NT

Fω
(x : A : Prop) |-> x
((x : A : Type) -> B : Prop) |-> ∀(x : A). B
((x : A : Prop) -> B : Prop) |-> A -> B

(x => M : (x : A : Type) -> B : Prop) |-> Λ(x : A). M
(x => M : (x : A : Prop) -> B : Prop) |-> λ(x : A). M
(M (N : A : Type) : A : Prop) |-> M [NL]
(M (N : A : Prop) : A : Prop) |-> M N
(M (N : A : Large)) |-> M NT NP


TBool = Prop -> Prop -> Prop;
PBool = (A : Prop) -> A -> A -> A;

Γ, x : Prop |- B : Prop
Γ, x = Prop |- B : Type
--------------------------
Γ |- (x : All) -> B : Type


(A : Prop) => (x : A) => x;

Unit = ⊥ -> ⊥;
Bool = ⊥ -> ⊥ -> ⊥;

Bool = ∀A. A -> A -> A;

TBool : Type = Prop -> Prop -> Prop;

degen : TBool = (A : Prop) => (B : Prop) => A -> A;

Eq A B = (P : Prop -> Prop) -> P A -> P B;

(A : Prop) => (eq : A == Int) => _;

Bool : Type = (b : TBool) &
  (P : TBool -> Prop) -> P true -> P false -> P b;

b.0 TBool true false == A => B => b.0 (true A B) (false A B)
(b : Bool) => (
  (A : Prop) => (B : Prop) => b.0 (true A B) (false A B),
  _
)
Bool : Type 0 = A = Prop; A -> A -> A;

Bool : Type 0 = (A : Type 0); A -> A -> A;

Γ, x : A |- B : Prop
------------------------
Γ |- (x : A) -> B : Prop

// Γ, x : Prop |- B : Type
// Γ, x = Prop |- B : Type l
// ----------------------------
// Γ |- (x : All) -> B : Type l


Γ, x : Prop |- M : B
Γ, x : Type |- M : Prop
------------------------------------
Γ |- (x : All) => M : (x : All) -> B

Γ |- M : (x : All) -> B  Γ |- N : K
-----------------------------------
Γ |- M N : B[x := N]


(A : Type) ->

T [M : Type] -> Type
T [M : Prop] -> Prop

C [(x : A : Large) -> B] = (x_T : T [A]) -> (x_P : P [A]) -> C [B]

C [x => M : (x : A : Large) -> B] = x_T => x_P => C [M]
C [(M : Large) N : T : Type] = (T [M]) (C [N])
C [M [N : Large]]
C [M : _] = M

C [Prop : Type] |-> Prop
C [x : A : Type] |-> x
C [(x : A : Type) -> B : Type] |-> (x : T [A]) -> T [B]

TT [x => M : (x : A : Type) -> B : Type] |-> x => T [M]

T [(x : A : Prop) -> B : Type] |-> (x : P [A]) -> T [B]
T [(x : A : Large) -> B : Type] |-> (x_T : LT [A]) -> (x_P : LP [A]) -> T [B]
T [x => M : (x : A : Type) -> B : Type] |-> x => T [M]
T [x => M : (x : A : Prop) -> B : Type] |-> x => T [M]
T [x => M : (x : A : Large) -> B : Type] |-> (x : P [A]) => T [B]

T [x : A : Prop] |-> x

P [x : A : Large] |-> x_P
P [(x : A : Type) -> B : Large] |-> (x : T [A]) -> P [B]
P [(x : A : Prop) -> B : Large] |-> (x : T [A]) -> P [B]
P [(x : A : Large) -> B : Large] |-> (x : T [A]) -> P [B]
P [(x : A : Type) -> B : Large] |-> (x : T [A]) -> P [B]

C [(x : A : Large) -> B] |-> (x : L [A]) -> C B

C [A : Large] = (LT [A], LP [A])


()

LT [(x : A) -> B] = (x : C [A]) -> B
LT [(x : A) => M] = (x : C [A]) => M

LT [(x : A : Large) -> B] = B

LP
C [(x : A : Large) -> B] |-> (x : L [A]) -> C B

S(x : A : Large) |-> ()



Bool : (A) -> A -> A -> A;



((A : Prop) => M, A = Prop; M)


(A : Type) -> A -> A -> A


P [x : A] |->

C [A : Large] = (LT [A], LP [A])
C [M : A : Large] = (LT [A], LP [A])

LT [(x : A) -> B] =

Bool : Type & {
  true : Bool;
  false : Bool;

  case : <P>(b : Bool, x : P(true), y : P(false)) -> P b;

  not : (b : Bool) -> Bool;
  and : (l : Bool, r : Bool) -> Bool;
};

Bool = _;
Bool = {
  true = <P>(x, y) => x;
  false = <P>(x, y) => y;
};


map : <A, B>(l : List(A), f : (el : A) -> B) -> List(B);
map = <A, B>(l, f) =>
  l <|
  | [] => []
  | hd :: tl => f(hd) :: map(tl, f);

Int_or_string = b => b <| | true => Int | false => String;
f = (b, x : Int_or_string b) =>
  b <|
  | true => Int.to_string x
  | false => x;


(==) : <A>(x : A, y : A) -> Prop &
(==) : <A : Equal>(x : A, y : A) -> Bool;


f = (n : Int & n == 0) => _;

g = (n : Int) =>
  n == 0 <|
  | true => f(n)
  | false => _;

safe_div : (num : Int, den : Int & den != 0) -> Int;

div : (num : Int, den : Int) -(Division_by_zero)> Int;
not = (b : Bool) =>
  b <|
  | true => false
  | false => true;

id = x => x;
incr = x => 1 + x;


T [b Prop] = b
T [b (A -> B)] = (x : A) => t => f =>
  [b B] t f x


((A : Large) => (x : A) => (y : A) => b A x y)

(A : Large) -> A -> A -> A

(x : Prop -> Prop) => (y : Prop -> Prop) =>
  b x y : Prop -> Prop

b (Prop -> Prop -> Prop)

x => y =>
  b (Prop -> Prop -> Prop) x y

(b : Prop -> Prop -> Prop) ->
(x : Prop -> Prop) -> (y : Prop -> Prop) -> Prop -> Prop

(b : Prop -> Prop -> Prop) =>
(x : Prop -> Prop) => (y : Prop -> Prop) =>
  (A : Prop) => b (x A) (y A)

(x : Prop -> Prop -> Prop) => (y : Prop -> Prop) =>
  (A : Prop) => b (x A) (y A)

(x : Prop -> Prop) => (y : Prop -> Prop) =>
  T (A => b (x A) (y A))
  (A : Prop) => b (x A) (y A)

l r

(x : Prop) => t => f => [b (Prop -> Prop)] t f x
(x : Prop) => t => f => ((y : Prop) => t => f => b Prop t f y) t f x

b (Prop -> Prop -> Prop) l r

T [(b ? l : r) : Prop] = b l r
T [(b ? l : r) : A -> B] = (x : A) => T [(b ? l x : r x) : B]
T [(b ? l : r) : Bool] = T [(b ? l : r) : Prop -> Prop -> Prop]

LT [(M : (A : Large) -> B) (N : Type)] = Mono [M : B[A := N]]

Mono [M : A -> B] =
  (x : A) => M (x)
LT [M : A -> B] [N : Type] =
  (x : A)

T [((A : Large) => M : (A : Large) -> B) (N : Type) : Prop] =
  (LT [M] [A]) (T [N])

LT [(A : Large) => (M : B)] = (A : True) => LT [M ]


T [(M : Large) (N : Type) : Prop] =
  (LT [M]) (T [N])
T [(M : Large) (N : Type) : A -> B] =
  (x : A) => (LT [M]) (T [N])

Bool : Type =
  ((A : True) -> Prop -> Prop -> Prop)) &
  ((A : Prop) -> A -> A -> A);

b : (A : Prop) -> A -> A -> A
b Prop : Prop -> Prop -> Prop
not = (b : Bool) => b ? false : true;

not = (b : Bool) => b ? Int : String;

n < d
d >= n

[↑b : 0]n

-
Γ |-


℘ S === (x : S) -> Type 0;

℘U -> Type 0

X = Type 0; (f : ℘℘X -> X) -> ℘℘X

// impredicative formation needs to return X
// formation needs to be parametric

// parameter should be preserved
U : Type 1 = (X : Type 1) -> (X -> Type 0) -> Type 0;

U : Type 1 = (X : Type 1) ->
  (f : ℘℘X -> X) -> (℘℘X -> X) -> X;

// impredicative introduction needs to be closed
τ (t : ℘℘U) : U = (X : Type 1) => (f : ℘℘X -> X) =>
  (p : ℘X) => t ((x : U) => p (f (x X f)));

σ (s : U) : (℘℘X -> U) -> U =
  s U τ;
Ω : U = τ ((p : ℘U) => (x : U) -> σ x p -> p x);

Γ, x : Type 1 |- B : Type 1
Γ, x : Type 0 |- B : Type 0
-------------------------------
Γ |- (x : Type 1) -> B : Type 1



False = (f : False, i : (P : (f : False) -> Type) -> P f);

False = (f : False) @-> (P : (f : False) -> Type) -> P f;

Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool) & <P>(x : P true, y : P false) -> P true;
true = <A, B>(x : A, y : B) => x;
false = <A, B>(x : A, y : B) => y;

(true : <A, B>(x : A, y : B) -> A) :>
  <P>(x : P true, y : P false) -> P true

K : (A : Type) -> (x : A) -> Type;
K = A => x =>
  k @-> (P : (x : A) -> Type) -> (x : P x) -> P x;

W_Eq = <A>(x : A, y : A) => <P>(p : P(x)) -> P(y);
w_refl = <A>(x : A) : W_Eq(x, x) => <P>(p) => p;

K_Eq = <A>(x : A, y : A) =>
  W_Eq(x, y) & (K : W_Eq(x, x)) &
  <P>(p : P(w_refl(x))) -> P K;
k_refl = <A>(x : A) : K_Eq(x, x) => <P>(p) => p;

Eq : <A>(x : A, y : A) -> Type;
refl : <A>(x : A) -> Eq(x, x);

Eq = <A>(x, y) => <P>(x : P(x)) -> P(y) &
  (K : Eq(x, x)) & <P>(x : P(refl(x))) -> P(K);
refl = <A>(x) => <P>(x) => x;


Bool = <A>(x : A, y : A) -> A;

transport = <A, B>(iso : A =~= B) =>
  (iso(refl(A)) : Eq(A, B));

(w : K_Eq(Bool, Bool)) : K_Eq(S, T) => _
swap

(x : )
(w : Eq(x, x))

(
  eq : W_Eq(x, y);

)

refl = A => x => <P>(x) => x;


(x : A) -> Eq _ (f x) (g x) = Eq _ f g;



(A : Type) -> (x : A)
  (eq : <P>(v : P x) -> P y) &
  ;

refl = P => (v : P x) => v

Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool) &
  (P : (b : Bool) -> Type) -> (x : P(true), y : P(false)) -> P(b);
true = P => (x, y) => x;
false = P => (x, y) => y;

W_Eq = <A>(x : A, y : A) => <P>(x : P x) -> P y;
w_refl = <A>(x : A) : W_Eq(x, x) => <P>(x) => x;

K_Eq = <A>(x : A, y : A) =>
  W_Eq(x, y) & (K : W_Eq(x, x)) &
  <P>(x : P (w_refl x)) -> P K;
k_refl = <A>(x : A) : K_Eq(x, x) => <P>(x) => x;

Eq : Type;
refl : Eq;

Eq = (self : Eq) & <P>(x : P(Bool, refl)) -> P(Bool, self);
refl = <P>(x) => x;



<P>(x : P Bool) =>

w : Bool =~= Bool
w<T =>
  Eq : Type;
  refl : Eq;

  Eq = (self : Eq) & <P>(x : P(T, refl)) -> P(T, self);
  refl = <P>(x) => x;
  Eq
>(refl) :
ω


Eq : <A>(x : A, y : A) -> Type;
refl : <A>(x : A) -> Eq(x, x);

Eq = (K : Eq(x, x)) &


eq : <P>(x : P(x, refl(x))) -> P(y, eq)
(eq : Eq(x, y)) =>

Bool : Type;
true : Bool;
false : Bool;

Bool = (
  self : Bool;
  ind : <P>(x : P(true), y : P(false)) -> P(self);
);
true = (self = true, ind = <P>(x, y) => x);
false = (self = false, ind = <P>(x, y) => y);

Bool : Type;
true : Bool;
false : Bool;

Bool = (
  self : Bool;
  ind : <P>(x : P(false), y : P(true)) -> P(self);
);
true = (self = true, ind = <P>(x, y) => y);
false = (self = false, ind = <P>(x, y) => x);

(b : Bool) => (H : b =~= b.self) => _;


Z3 : Type;
zero : Z3;
succ : (pred : Z3) -> Z3;
mod3 : zero =~= succ(succ(zero));


Z3 = (self : Z3) &
  <P>(
    z : P(zero),
    s : (pred : Z3) -> P(succ(pred)),
    mod3 : z =~= s(s(z)),
  ) -> P(self);


Bool = <A>(then : A, else : A) -> A;

Unit : Type;
unit : Unit;

Unit = (u : Unit) & <P>(x : P(unit)) -> P(u);
unit = <P>(x) => x;


Unit |-> (u : Unit) & <P>(x : P(unit)) -> P(u)
(u : Unit) & <P>(x : P(unit)) -> P(u) |-> Unit

Eq : Type;
refl : Eq;

Eq = (self : Eq) & <P>(x : P(Bool, refl)) -> P(Bool, self);
refl = <P>(x) => x;



Eq(Bool, Bool) == <P>(x : P(Bool)) -> P(Bool)

(heq : Heq<A, A>(x, y)) -> Eq<A>(x, y)

(A : Type) => _

Γ |- A ≡ B
----------------
Γ |- A & B |-> A

Bool & Bool |-> Bool

uni<T =>
  Eq : Type;
  refl : Eq;

  Eq = (self : Eq) & <P>(x : P(T, refl)) -> P(Bool, self);
  refl = <P>(x) => x;
>
f = (uni : Uni(A, B)) =>
  (uni<T => Eq(A, T)>(refl) : Eq(A, B));

f = (n : Nat) => (w : n == 1 + n) => (refl : n == n);

Γ |- M : T  Γ |- N : T
----------------------
Γ |- M | N : T

(x : Nat) => (H : x == 1) => M |
(x : Nat) => (H : x == 2) => N

(DB | Log) :

A : Type = M | N;


Prop : Type

KBool = Prop -> Prop -> Prop;
Bool =
  Prop -> Prop -> Prop &
  (A : Prop) -> A -> A -> A;

Log : Prop -> Prop;
DB : Prop -> Prop;

b : Prop -> Prop -> Prop = A => B => (x : Prop) -> x;

Γ, x : Prop |- B : Prop
Γ, x = Prop |- B : Type
-----------------------
Γ |- ∀x. B : Type

Γ, x : Prop |- B : Prop
Γ, x : Type |- B : Type
-----------------------
Γ |- ∀x. B : Type

Γ, x : Type l |- B : Type l
---------------------------
Γ |- ∀x. B : Type 0


Γ, x : A |- B : Prop
--------------------
Γ |- ∀x. B : Prop

Prop : Type;

Id : Type = (A : Type) -> A -> A;
Id : Prop = (A : Prop) -> A -> A;


(b : Bool) =>
  (x : (A => (b (Log A) (DB A)) Int)) => _

(Type l : Type (1 + l))

Nat : Type;
zero : Nat;
succ : (pred : Nat) -> Nat;

Nat = (n : Nat) & <P : (n : Nat) -> Type>(
  z : P(zero) $ 1,
  s : (pred : Nat) -> P(succ(zero)) $ 1
) -> P(n);


Fix : Type
Fix = (x : Fix $ 2) -> ()

fix : Fix = (x $ 2) => x(x);

(n : Nat) => n((), x => x $ n)


(n : Nat)

Bool : Type 1;
Bool = (b : Bool) &
  <l, P : (b : Bool) -> Type l>(x : P(true), y : P(false)) -> P(b);

(b : Bool : Type 1) => <P>(x : P(b) : Type 2) =>
  (b<2, P> : (x : P(true), y : P(false)) -> P(b))

(x : A : Type) =>
(A : Prop) => (x : A) => x;

Γ |- A ≡ (x : A) & B
----------------------
Γ |- (x : A) & B |-> A

(x : A) & A |-> A

(x : A) & B |->

(x : A) => (y : B) => (h : Heq<A, B>(x, y)) => _;


Bool = <A>(x : A, y : A) -> A;

uni(id)
uni(swap)

id = (b : Bool) => b;
(eq : Bool =~= Bool) =>
  transport<T => (b : T) -> Bool>(eq)(id)(swap(true))

(eq : Bool =~= Bool) =>
  transport<T => (b : T) -> T>(eq, id)(true) == true

eq : <P>(v : P(x)) -> P(y)
  = <P>(v) => v;

Γ |- A ≡ B
----------------
Γ |- A & B |-> A

Bool & Bool |-> Bool

K : <A, x>(eq : x == x : A) -> eq == refl;


Parametric Polymorphism
Subtyping Polymorphism
Ad-hoc Polymorphisml

id = <A>(x : A) => x;

(id<String>("a") : String)
(id<Nat>(1) : Nat);
(id<Bool>(true) : Bool)

id : (A : Type) -> (x : A) -> A
  = (A : Type) => (x : A) => x;

Nat : Type
String : Type
Bool : Type

id(String) : (x : String) -> String
id(String)("a") : String

id(Nat) : (x : Nat) -> Nat
id(Nat)(1) : Nat

(id(String)("a") : String)
(id(Nat)(1) : Nat);
(id(Bool)(true) : Bool)

Value -> Value
Type -> Value
Type -> Type
Value -> Type

CoC =~= Fω

Id : (A : Type) -> Type
  = A => A[];

f : (M : (A : Type) -> Type) -> (x : M(Nat)) -> M(Nat)
  = (M : (A : Type) -> Type) => (x : M(Nat)) => x;

f(Id) : (x : Id(Nat)) -> Id(Nat)

Nat_or_string : (b : Bool) -> Type
  = b => b ? Nat : String;

f : (b : Bool) -> (x : b ? Nat : String) -> String
  = b => x => b ? x.toString() : x;

Bool : Type 1 = (A : Type 0) -> (then : A) -> (else : A) -> A;
true : Bool = A => then => else => then;
false : Bool = A => then => else => else;

Bool =
  | true
  | false;


TBool = (then : Type) -> (else : Type) -> Type;
ttrue : TBool = then => else => then;
tfalse : TBool = then => else => else;

f : (b : TBool) -> (x : b(Nat)(Bool)) -> String
  = b => x => b(1)(0);

f : (b : Bool) -> (x : b <| | true => Nat | false => Bool) -> String
  = b => x => "a";



(Type l : Type (1 + l))
Term ::=
  | Type l
  | x
  | (x : A) -> B
  | (x : A) => M
  | M(N);

not = (b : Bool) => b(Bool)(false)(true);



TBool = (then : Type) -> (else : Type) -> Type;
ttrue : TBool = then => else => then;
tfalse : TBool = then => else => else;
tweird : TBool = then => else => ((A : Type) -> then);

VBool = (tb : TBool) =>
  (P : (tb : TBool) -> Type) ->
  (then : P(ttrue)) -> (else : P(tfalse)) -> P(tb);
vtrue : VBool ttrue = P => then => else => then;
vfalse : VBool ttfalse = P => then => else => else;

f : (tb : TBool) -> (vb : VBool tb) -> (x : tb(Nat)(Bool)) -> String
  = tb => vb => x => vb(Nat)(1)(0);

Bool =
  ((then : Type) -> (else : Type) -> Type) &
  ((A : Type) -> (then : A) -> (else : A) -> A);

Bool = ∀A. (then : A) -> (else : A) -> A;


(Type : Type)


Kind l : Kind (1 + l)

Bool : Type = (A : Type : Kind) -> (then : A) -> (else : A) -> A;
Bool : Kind 0 = (A : Kind 0 : Kind 1) -> (then : A) -> (else : A) -> A;

Bool = ∀A. (then : A) -> (else : A) -> A;

Γ, x : Type |- T : Type
Γ, x : Kind |- T : Kind
-----------------------
Γ |- ∀x. T : Kind

((A : Type) -> (x : A) -> A : Type)


Fix : Type;
Fix = (x : Fix) -> ()

fix : Fix = x => x(x);

Nat : Type;
Nat = (A : Type) -> (z : A) -> (s : (pred : Nat) -> A) -> A;


id = (A : Type 0) => (x : A) => x;

id((A : Type 0) -> (x : A) -> A)

⇑id((A : Type 0) -> (x : A) -> A)(id)

Type l : Type (1 + l)

Bool =
b(Type : Kind)

Bool =
  (tb : TBool) &
  (P : (tb : TBool) -> Type) -> (then : P(ttrue)) -> (else : P(tfalse)) -> P(tb);

f : (b : Bool) -> (x : b(Nat)(Bool)) -> String
  = b => x => vb(Nat)(1)(0);



f(tweird) : (vb : VBool tweird) -> (x : tweird(Nat)(Bool)) -> String


(b : (A : Type) -> (then : A) -> (else : A) -> A)
(b(Type) : (then : Type) -> (else : Type) -> Type)

g = f _A;
f = x => _A;

(x => _A) M N

A = (x : A) -> ();
B = (x : B) -> ();

M : A
M : (x : A) -> ()
M : (x : B) -> ()
M : B


{
  zero : Nat = zero;
  succ : (pred : Nat) -> Nat = succ;
};

@.zero : Nat;
@.succ : _;

@.zero = _;
@.succ = _;

Nat : _;
Nat = (
);
eduardo : User;
eduardo.id = "a";
eduardo.name = "a";


({ zero; succ } : {
  zero : Nat;
  succ : (pred : Nat) -> Nat;
})

Term :=
  | Prop
  | Data

[a] =>

x : [A];
x = !x;

[(M : A) : T] = _A; T == A; [M : A]

[x => M : T] = _A; T == (x : A) -> B && (x : A). [M : B]

case : <A>(pred : Bool, then : A, else : A) -> A;
Bool = <A>(then : A, else : A) -> A;

ind : <P>(b : Bool, then : P(true), else : P(false)) -> P(b);

Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool) & <P>(then : P(true), else : P(false)) -> P(b);


id = x => x;
sequence = a => b => b;

not = b =>
  b <|
  | true => 1
  | false => 0

Nat : Type & {
  zero : Nat;
} = {
  zero : Nat = 0;
};

x : Nat = Nat.zero;


Show = (S : Type) & {
  show : (x : S) -> String;
};

show = <S : Show>(x : S) => S.show(x);

Bool : Type & {
  true : Bool;
  false : Bool;

  case : <A>(pred : Bool, then : A, else : A) -> A;
} ~= _;

(b : Bool) => b


f : (x : A) -> B;
f : (x : A) -> Kont B;
ret : <T>(x : T) -> Kont T;
incr = x => ret (1 + x);

A -> M B

(x : A) -> B : Type l

(A : Type(ω))
Type(l) :> Type(ω)
Type 0 & Type 1

(A : Type(l + k)) -> A :> (A : Type(l)) -> A


℘ S === (x : S) -> Type 0;

U : Type 1 = (X : Type 1) -> (f : ℘℘(Type 0) -> X) -> ℘℘X;
τ (t : ℘℘U) : U = (X : Type 1) => (f : ℘℘X -> X) =>
  (p : ℘X) => t ((x : U) => p (f (x X f)));

σ (s : U) : ℘℘U = s U τ;
Ω : U = τ ((p : ℘U) => (x : U) -> σ x p -> p x);

id : (A : Prop) -> (x : A) -> A;
id : (A : Type 0) -> (x : A) -> A;


(A : Type 0) -> (x : A) -> A &

Bool : Type 1;
true : Bool;
false : Bool;

Bool = (b : Bool) &
  (P : (b : Bool) -> Type 0) -> (x : P true) -> (y : P false) -> P b;
true = P => x => y => x;
false = P => x => y => y;

P b : Type 1

Bool = (A : Type 0) -> (x : A) -> (y : A) -> A;

A : Type l
P : (x : A) -> Type (1 + l)
x : A

Id = (A : Type 0) -> (x : A) -> A;
(id : Id) -> (B : Type 1)
(id : Id) => id B M

M
P x
P (⇑x)



Γ |- T : Type l
----------------------
Γ |- ⇑T : Type (1 + l)

Γ |- M : T
----------------
Γ |- M⇑ : T & ⇑T

(x : )

Z0 =
  | Z
  | S of Z0
  | Z == S Z;

3 + 1 + 1 + 1

3 * 2 = 2

1 * 2 = 2

Γ |- T[l := _l] <: A
--------------------------
Γ |- (l : Level) ->

Unit : Type ω = (l : Level) -> (A : Type l) -> (x : A) -> A
  = _;

incr = x => (
  log("Hello");
  1 + x
);

incr 1 == (
  log("Hello");
  2
)

(x = 1; x + 1)
2 + x

Bool = <K>(x : K, y : K) -> K;


(x : #debug A) => x


Type : Type
Data : Type
Prop : Type
Line : Type

A -> B

type 'k key [@@linear]

Nat & Type : {
  zero : T;
} = {
  @ = Int;
  zero = 1
};
x : Nat = Nat.zero;

// nat.tei
zero = _;
succ = _;
{ zero; succ; }


_ : incr 1 == 2 = refl;



(∀, A, C)
(Σ, 1, 2)
()

id

(x = 1; 2 + x)

1; 2; VAR -1; ADD
1; 3


(x => x)

Type : Kind
(x : A) -> B : Type

M == (y : A) -> B

y => M y == x => N


<A>(x : )

f A B C

A; APPLY f; B; APPLY 1; C; APPLY 1

M; A; APPLY; B; APPLY; C; APPLY

N; APPLY n;

Value (V) ::=
  | _X
  | x ...V
  | x => V

_X |-> HOLE X
x ...V       |-> VAR x; ...(BEGIN; V); END
(x : A) -> B |-> FORALL; BEGIN; A; B
x => V       |-> LAMBDA; V

(x : Int) -> Int

CALL A |-> PUSH |PC + A|; A;

VAR x;

LAMBDA;

f ...V |-> x => (f ...V) x
M |-> LAMBDA; M; BEGIN; VAR x; END

VAR y; APPLY x;

VAR x; END == LAMBDA; VAR x

VAR x; END == LAMBDA; M

(f, sink)


sink = (ctx, x) => ((ctx, x), sink);

f x |-> ((f, x), sink)


f = ctx => x => ((ctx, x), f);


User_repr : Type;
User_repr = {
  id : Nat;
  name : String;
  partner : User_repr;
};

User = (user : User_repr) &
  user == user.partner.partner;

User : Type;
Partner : (user : User) -> Typer;

User_repr : Data;
User : Data;

User_repr = {
    id : String;
    name : String;
    partner : User_repr;
};
User = (self : User_repr) &
  self == self.partner.partner;







Partner = user => (self : User) & user == self.partner;

User =

L = b => b | true => L false | false => L true;
R = b => b | true => R false | false => R true;

A : Type;
A = A;

A :> L true
A :> L true

L true :> R false
L true :> L true & L false

L true & L false :> R true & R false
R true & R false :> R false

L true & L false :> R true & R false
R true & R false :> R true

R true == R true & R false



|- L true == R false
L true == R false |- L false == R true
L true == R false; L false == R true |- L true == R false

|- L true == R false

(x : Int & )

Unit = (u : Unit) & ()

Unit = [];
Named = [x : Nat];
User = [
  id : Nat;
  name : String;
];

witch : User = [
  id = 0;
  name = "Witch";
];
eduardo : User = [1, "Eduardo"];

0 |- (x/0 : _A/0) => (y/1 : _) => _

id = <A>(x : A) =>

id = (A : Type, x : A)


List = A => A[];




[x : A, y]
(x :)
T =

A : Type;
A = (x : A) -> ();

B : Type;
B = (x : B) -> ();

Unit = (
  u : Unit;
  (u : <P>(x : P(unit)) -> P(u)) -> _;
);

x : Nat;
x = f x;

[\!1][f \-1]M

x : Nat;
x = 1 + x;

Nat : Type;

Ω : Ω


Spec = {
  Context : _;

  read_name : (id : Nat) -[Context]> (name : String);
  create : (name : String) -[Context]> (id : Nat);l

  create_then_read : (s : String) -> s == read_name(create(s));
};
↑ : 1

x => y => x

Nat : Data;
Nat = (succ : (pred : Nat [a]) -> ()) -> ();

Stream : Data;
Stream = <K>(w : (tl : Stream(A)) -> K) -> K;

cons : Stream;
cons = <K>(w) => w(cons);

Stream : Data;
Stream = (x : Nat, tl : Stream);

cons : () -> Stream;
cons = (x = 1, tl = cons());


x : A;
y : B;

x = M; // depends on y

z : C; // depends on x == M
y = N;
z = K;

f_x = (x : A) => (y : B) => (M : T_x);

f_y = (z : C) =>

x = _;
y = _;

[2; A; B]

A : Type 1
eq : (P : (x : A) -> Type 0) -> P x -> P y


db : Database;

unique :

id = x => 1 + x;

(x : A) -> B

f = x => M;
g = f x;

C<f >[f := λx. M] |->

Stream : (A : Data) -> Data;
cons : <A>(hd : A, tl : Stream(A)) -> Stream(A);

Stream = A => (s : Stream(A)) &
  <P>(c : <hd, tl>(H : P(tl)) -> P(cons(hd, tl))) -> P(s);
cons = <A>(hd, tl) => <P>(c) => c(tl(c));


L = b =>
  b
  | true => (x : Nat, y : L false)
  | false => (x : String, y : L true);
R = b =>
  b
  | true => (x : String, y : R false)
  | false => (x : Nat, y : R true);

L true == R false
(x : Nat, y : L false) == (x : Nat, y : R true)
L false == R true

(x : String, y : L true) == (x : String, y : R false)

L true & == R true & R true
(x : Nat, y : L false) == (x : Nat, y : R true)
L false == R true

L true & (x : Nat, y : L false) == R true & (x : Nat, y : R true)
(x : Nat, y : L false) == (x : Nat, y : R true)

L true & (x : Nat, y : L false) == R false

\Left
x = A;
y = B;

Either = (A, B) =>
  | (tag == "a", payload : A)
  | (tag == "b", payload : B);

Either = (A, B) =>
  (
    tag : String,
    payload :
      tag <|
      | "a" => A
      | "b" => B
      | _ => Never
  );

x : Either(Int, String);

(tag : String, payload : tag <|
      | "a" => Int
      | "b" => String
      | _ => Never) = x;

Term (M, N)
Type (A, B)

Value (V) ::=
  | x ...V
  | x => M
  | (x : A) -> B
  | (x : A) & B

Fix : Type;
Fix = (x : Fix) -> ();

Unit : Type;
Unit = (u : Unit) & T;

Unit == (u : Unit) & T;

(==) : <A>(x : A, y : A) -> Type;
refl : <A>(x : A) -> x == x;
(==) = <A>(x, y) => <P>(h : P(x)) -> P(y) &
  (eq : x == x) & <P>(h : P(refl(x))) -> P(eq);
refl = h => h;


almost_k : <A, x : A>(eq : x == x) ->
  <P>(h : P(eq :> x == x)) -> P(refl(x));

(M : A) :> A == M

k
(u : X) & T == (u : X) & T;

f : (x : Int) -> Int;
(f 1)


T : Type = <A, B>(x : A, y : B) -> A;
U : Type = <A, B>(x : A, y : B) -> B;


uni : (h : Iso(A, B)) -> A == B;
uip : <A, x, y : A>(a : x == y, b : x == y) -> a == b;


Type : Type;

A == B

ind : <P>(b : Bool, t : P(true), f : P(false)) -> P(b);

Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool) & <P>(t : P(true), f : P(false)) -> P(b);
true = _;
false = _;

(true : (b : Bool) & <P>(t : P(true), f : P(false)) -> P(b)) :>
  <P>(t : P(true), f : P(false)) -> P(b :> Bool)


ind : <P>(b : Bool, t : P(true), f : P(false)) -> P(b :> Bool)


Z2 : Type;
zero : Z2;
succ : (pred : Z2) -> Type;
mod2 : (z : K, s : (acc : K) -> K, h : s(s(z)) == z) ->
  succ(succ(zero))(z, s, h) == zero(z, s, h);

Z2 = <K>(z : K, s : (acc : K) -> K, h : s(s(z)) == z) -> K;
zero = <K>(z, s, h) => z;
succ = pred => <K>(z, s, h) => s(pred(z, s, h));
mod2 =

data ∥_∥₂ {ℓ} (A : Type ℓ) : Type ℓ where
  ∣_∣₂ : A → ∥ A ∥₂
  squash₂ : ∀ (x y : ∥ A ∥₂) (p q : x ≡ y) → p ≡ q



Set : (A : Type) -> Type;
box : <A>(x : A) -> Set(A);
squash : <A, x, y>(p : x == y : A, q) -> p == q;

Set : (A : Type) -> Type;
box : <A>(x : A) -> Set(A);
squash : <A, x>(p : x == x : A) -> p == refl(x);


Set = A =>
  <K>(
    w : (x : A) -> K,
    h : <x>(p : x == x : A) -> p == refl(x)
  ) -> K


id : <A>(x : A) -> A;

id = <A>(x : A) => x;

((x : Int) -> Int) & ((x : String) -> String)

(id<Int> : (x : Int) -> Int, id<String> : (x : String) -> String)

A : Meta;
B : Type;

M : (x : A) -> B x
// then
(x : A) -> (y : A) -> M x =~= M y


Unit : Type;
unit : Unit;

Unit = (u : Unit) & <P>(v : P(unit)) -> P(u);
unit = <P>(v) => v;

Unit = Unit;

M;

x => f = M; f

Fix : Type;
Fix = (x : Fix) -> ();

expected : Fix
received : (x : Fix) -> ()


A = A;
B = B;

C[a := A] == A  C[a := B] == B
------------------------------
A == B

A = (x : A) -> ();
B = (x : B) -> ();


A == B

C : (T : Type) -> Type;
l : C(A) == A;
r : C(B) == B;

C =
x : P(A)
y : P(B)

y = x

M : (x : A) & B

Unit : Type;
unit : Unit;

Unit = (u : Unit) & (P : (u : Unit) -> Type) -> (v : P(unit)) -> P(u);


Γ |- M ⇐ A  Γ |- M ⇐ B
----------------------
Γ |- M ⇐ A & B



μ(x : A -> B). y => M
μ(x : A -> B). y => M


T = f T;

(x : A) -> B

M : (x : A, y : B)

M |-> x => M x
M |-> (x, y) = M; (x, y)

(a, b) = M;
(c, d) = M;

(fst M, snd M)

(x => Nat) :> (y => Int)
Nat :> Int

M : (x : A) -> Type;
N : (x : A) -> Type;

M | N
  |-> (x => M x) | (x => N x)
  |-> x => M x | N x

Γ |- M : (y : Type) -> Type
---------------------------
Γ |- (x : A) -(M)> B : Type


∀x. A -> B x


(1 & "a" : Nat & String)
(1 & "a" :> Nat) |-> 1

Γ |- M : A  Γ |- N : B
----------------------
Γ |- M & N : A & B

(x : A, y : B) :> (x : A) & B

((==) : <A : Data>(x : A, y : A) -> Prop) &
((==) : <A : Eq>(x : A, y : A) -> Bool & )

(1 & "a" : Nat & String)

Any

(x : Int & A) => _

expected : Int & _A
received : Nat | _B

(x : A, y : B, ...R) : Data

(x : A, y : B, ...R)

(x : A, y : B, ...(z : C, a : D))
(x : A, y : B, z : C, a : D)

(x : A, y : B, ...T)
(x = 1, ...(y = 2, z = 3))
(x, y, M)

Tuple : Type;
Tuple =
  | ()
  | <R : Row>(x : A, ...R);


(..., z : C) : Row

(..., a : B, ...R)

(x : A, y : B, ...(..., z : C))
(x : A, y : B, z : C)

(x : A, y : B) & R [A B]
(z : C) & R [C]

(x : A) & { x : A; y : B; }

...T : Flat
(x : A, ...y : B, ...C)

(x : A, ...T)

User = {
  id : Nat;
  name : String;
};

(M : (x : A, y : B, z : C)) :> (x : A, ...p : (y : B, z : C))

Tuple : (R : List(Data)) -> Flat(List.length(R));
Tuple = R =>
  R <|
  | [] => ()
  | [A, ...R] => (x : A, ...p : Tuple(A));


Type l : Type (1 + l);
Data l : Type (1 + l);
Prop l : Type (1 + l);
Line l : Type (1 + l);
Flat l : Type (1 + l);

()
(x : A)
(x : A, y : B)
(x : A, y : B, z : C)

(p : (x : Int, y : Int, ...R)) => _

Bool : Type & {
  true : Bool;
  false : Bool;
};

true : Bool = Bool.true;

Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool) & <P>(t : P(true), f : P(false)) -> P(b);
true = <P>(t, f) => t;
false = <P>(t, f) => f;


Bool : {
  true : Bool;
  false : Bool;

  ind : <P>(b : Bool, t : P(true), f : P(false)) -> P(b);
  ind_when_true : <P, t, f>() -> ind<P>(true, t, f) == t;
  ind_when_false : <P, t, f>() -> ind<P>(false, t, f) == f;
} = _;

M : A :: N : B

M = (Bool : Type & Bool == <A>(x : A, y : A) -> A) & M;

(Bool : Type) & S

cons : (Γ : Context) -> (x )

Value = (Γ : Context) -> Term(Γ);

infer : (Γ : Context, M : Term(Γ)) -> Term(Γ);
infer : (Γ : Context, M : Term(Γ)) -> Value;

check : (Γ, M : Term(Γ), T : Term(Γ)) -> ();
check : (Γ, M : Term(Γ), T : Value) -> ();

(λ, C)
(∀, A, C)


check :

x = M;
N
(x : A) => (M : B)

(x : A) -> B

Unit : Type;
Unit = (u : Unit, i : _, w : u == fst(u));

Self : (A : Type, B : (x : A) -> Type) -> Type;
Self = (A, B) => (x : Self(A, B), c : A, w : x =~= c);

Unit : Type;
Unit_I : (u : Unit) -> Type;
Unit_H : Unit == Self(Unit_I);
unit : Unit;

Unit = Self(Unit, u => <P>(x : P(unit)) -> P(u), )

Y : <A>(f : (x : A) -> A) -> A;
Y = <A>(f) => f(Y(f));

Y : <A, B>(f : (x : A) -> B(x)) -> B(x);
Y = <A>(f : ) => f()

Self_T : Type;
Self : (T : Self_T) -> Type
Self_W : (T : Self_T, x : Self(T)) -> Type;

Self = T => (x : Self(T), i : _, w : Self_W(T, x))
Self_W = (T, x) => x == fst(x);
Self_T = Self()

Self_T_W : Self_T;

Self_T = Self(Self_T_W);


Self : (T : (x : Self(T)) -> Type) -> Type
Self = T => (x : Self(T) $ 0, i : _, w : x == fst(x));

T =
  | (l : (tag == "left", x : Int))
  | (tag == "right", y : Nat, h : y >= snd l);

_ : (y : Nat, h : y >= 0);



Γ, x : S |- (M : A) :> T
---------------------------
Γ |- (M : A) :> (x : S) | T

Γ |- (M : S) :> A
---------------------------
Γ |- (M : (x : S) | T) :> A

Γ |- (M : S) :> A  Γ |- K : S  Γ |- (M : T[x := K]) :> A
--------------------------------------------------------
Γ |- (M : (x : S) | T) :> A

T = (A : Type) | A;



// dependent intersection
Γ |- (M : A) :> S  Γ |- (M : A) :> T[x := M]
--------------------------------------------
Γ |- (M : A) :> (x : S) & T

Γ |- (M : S) :> A
---------------------------
Γ |- (M : (x : S) & T) :> A

Γ |- (M : T[x := M]) :> A
---------------------------
Γ |- (M : (x : S) & T) :> A

Unit : Type;
unit : Unit;

Unit = (u : Unit) & <P>(v : P(unit)) -> P(u);
unit = <P>(v) => v;

ind : <P>(u : Unit, v : P(unit)) -> P(u)
ind = <P>(u : Unit, v) => (u<P>(v) : P(u));



T = (id : Type -> Type) | (x : id(String)) -> id(String);
M : T = x => x;



Unit : Type;
unit : Unit;

Unit = (u : Unit) & (P : Unit -> Type) -> P(unit) -> P(u);
unit = x => x;


(y : (x : A) & B) & C

(x : A) & B
(y : (x : A) & B) & C

Unit =
  (P : Unit -> Type) ->

Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool) &


T =
  (l : (tag == "left", A : Type)) |
  (r : (tag == "right", id : snd(A) -> snd(A)));

<A>(x : A) -> A;
<A>(x : A) => A

T =
  (
    tag : String,
    payload :
      tag <|
      | "left" => Type
      | "right" => <l : (tag == "left", A : Type)> =>
        (tag == "right", id : snd(A) -> snd(A))
      | _ => Never
  );
T = (u : Unit) & Unit_I(u);


id : T = ("right", x => x);

(x : A) &

Id = (A : Type) | (x : A) -> A;
id : Id = x => x;




Never & Type |-> Never

id :

id : String -> String


id = (tag == "left", A : Type) | (tag == "right", x : A)

id = (tag : String, payload : tag <| | "left" => A | "right")


mem : Memory;
data : Ptr;

mem.get(data) <|
| Left => _;
| Right => _;

(mem, data) <|
| Left => _
| Right => _;

Value =
  | (tag == "univ")
  | (tag == "arrow", param : Value, body : Value)
  | (tag == "thunk", env : Env, term : Term);

value : Value;

f = value =>
  value <|
  | ("univ") => _
  | ("arrow", param, body) => _
  | ("thunk", env, term) => f(eval(env, term));

f = value =>
  value <|
  | ("univ") => _
  | ("arrow", param, body) => _;

f = value =>
  value <|
  | ("univ") => _
  | ("arrow", param, body) => _;


--------------------------
Γ |- () : (() : Prop) & ()

Γ |- A : K  Γ |- T <: ()
-----------------------------------
Γ |- (x : A, ...T) : lower(K, Prop)

Γ |- A : K_A  Γ |- T <: (y : B, ...S) : K_T
-------------------------------------------
Γ |- (x : A, ...T) : lower(K_A, K_T)

() // unit
(x : A) == (x : A, ...()) // named value
(x : A, y : B) == (x : A, ...(y : B, ...())) // pair

Row : Type;
Row = () | <A, R>(x : A, ...R);

<R : Row>(x : A, ...R)

(_ <: _) : (A : Type, B : Type) -> Type;
coerce : <A, B>(H : A <: B, M : A) -> B;

refl : <A>() -> A <: A;
fun : <A, B_S, B_T>() -> ((x : A) -> B_S(x)) <: ((x : A) -> B_T(x))
noop : <A>(H : A <: A) -> M == coerce(H, M);

(_ <: _) = (A, B) => (M : A) -> B;



Either = (A, B) =>
  | (tag == true, payload : A)
  | (tag == false, payload : B);


Γ |- (M : A) :> S -| Δ  Γ |- (M : A) :> T -| Φ
----------------------------------------------
Γ |- (M : A) :> S & T -| Δ & Φ

Γ |- (M : S) :> A -| Δ  Γ |- (M : T) :> A -| Φ
----------------------------------------------
Γ |- (M : S & T) :> A -| Δ & Φ

Γ |- (M : S) :> A -| Δ  Γ |- (M : T) :> A -| Φ
----------------------------------------------
Γ |- (M : S | T) :> A -| Δ | Φ

(M : _A) :> Nat & String

(M : _A) :> Nat
(M : _A) :> _A & Nat

(M : _A) :> _A & Nat & String

_B <: (x : _A) -> _A
(M : _B) :>
  ((x : Int) -> Int) & ((x : String) -> String);

(M : Nat | String) :> _A | Nat | String



Bool : Data;
true : Bool;
false : Bool;

Bool = (b : Bool $ 0 , i : <P>(t : P(true), f : P(false)) -> P(b));
true = (b = true, i = <P>(t, f) => t);
false = (b = false, i = <P>(t, f) => f);

Bool : Data;
true : Bool;
false : Bool;

Bool = (b : Bool) & <P>(t : P(true), f : P(false)) -> P(b);
true = <P>(t, f) => t;
false = <P>(t, f) => f;

BoolW = (b : Bool, W : b == (b :> Bool));

Eq = <A>(x : A, y : A) => <P>(v : P(x)) -> (P(y) : Data);


(x : A : Type, y : B : Data, z : C : Data)

(⇑K) : <A : Type, x>(H : x == (x : A) : Type) -> H == refl : Type

(b : Bool : Data) => (⇑b : Bool)

(true :> <P>(t : P(true), f : P(false)) -> P(true :> Bool))

(b : Bool, w : b == true | b == false);

true
false = (u : Unit)


Γ |- A : Data
-----------------------
Γ |- (M : A) :> A |-> M


Γ |- A : Set(Data)
Γ |- M : A  Γ |- N : A
----------------------
Γ |- M == N : Prop

Γ |- A : Set(Type)
Γ |- M : A  Γ |- N : A
----------------------
Γ |- M == N : Prop


Eq : <A>(x : A, y : A) -> Data;
refl : <A, x> -> Eq<A>(x, x);

Eq = <A>(x, y) =>
  (l : <P>(v : P(x)) -> P(y)) &
  (r : Eq<A>(x, x)) &
  <P>(v : P(refl)) -> P(r);
refl = <A>(x : A) => x;

Eq<A>(x, x) ==
  (l : <P>(v : P(x)) -> P(y)) &
  (r : Eq<A>(x, x)) &
  <P>(v : P(refl)) -> P(r)

(H : Eq<A>(Bool, Bool)) :> <P>(v : P(refl)) -> P(H :> Eq<A>(Bool, Bool));
(H : Eq<A>(x, y)) :> <P>(v : P(refl)) -> P(H :> Eq<A>(x, x));

Bool : Data;
true : Bool;
false : Bool;

Bool = (b : Bool) & <P>(t : P(true), f : P(false)) -> P(b);

User = {
  Bool : Data;
  Bool : {
  };
};

Bool : Data;
true : Bool;
false : Bool;

Bool = (b : Bool) & <P>(t : P(true), f : P(false)) -> P(b);
true = <P>(t, f) => t;
false = <P>(t, f) => f;

{ Bool; true; false }

Bool = (
  Bool = (b : Bool) & <P>(t : P(true), f : P(false)) -> P(b);
  Bool = {
    true = <P>(t, f) => t;
    false = <P>(t, f) => f;
  }
);

T_False : Type;
TH_False : (False : T_False) -> Type;

T_False = (False : T_False) &
  (H : TH_False(False)) -> Type;
TH_False = False => (H : TH_False(False)) &
  False(H) == (f : False(H)) & <P> -> P(f);

False : Type;

f = A => (x : T) & S;
f = (x : A => T) &
  (A => S[x := x A]);

Self = (A, B)

Monad = (M : Data) & {
  bind : <A, B>(m : M(A), f : (x : A) -> M(B)) -> M(B);
};

L<x => M>
x => L[[↑L]\0]<M>

Infer : (env : Env, term : Term) -> Prop;
Infer = (env, term) =>
  term <|
  | ("let", arg, body) =>
      arg_type = Infer(env, arg);
      body_type = Infer(env, body_type);
      (type == ("let", arg_type, body_type))

(x => x(x))(x => x(x))

Fix : Type;
Fix : (x : Fix) -> False;

Type =
  | ("forall", A : Type, B : (x : A) -> Type)
  | ("pair", A : Type, B : (x : A) -> Type);

Unit : Type;
Unit = ("pair", Unit, x => T);

L<x => M>

(x => L<M>)

(M : A) :> (u : Unit) & Unit_I(u);
(|M| :> Unit) :> Unit_I(u);

T_unit : Type;
unit : T_unit;

T_unit = <P : (u : T_unit) -> Type>(x : P(unit)) -> P(unit);

Unit : Type;
unit : Unit;

Unit = (u : Unit) & <P>(x : P(unit)) -> P(u);
unit =

ind = <P>(b, t, f) => b<P>(t, f);


Bool : Data;

b : Bool;

ind<P>(• b)(t)(f) |-> b<P>(t)(f)  (Bool = _) || (b = _);



Bool : {
  • true : Bool;

  ind : <P>(• b : Bool, t : P(true), f : P(false)) -> P(b);
};

Bool = {
  Key : Nominal;
  T = <A>(t : A, f : A) -> A;
  true : T = <A>(t, f) => t;
};

Bool.true |-> <P>(t, f) => t;

Bool.ind<P>(• Bool.true)(t)(f) |-> t;

(M : (x : A) -> B) (N : Bool)

ind<P>(• false)(t)(f) |-> f;

f : (a : _) -> Bool = a =>
[@parallel]

Term ::=
  |

Bool = (
  key : Key;
  Bool = <A>(k : Key, H : k == key, t : A, f : A) -> A;
  true : Bool = <A>(k, H, t, f) => t;
  false : Bool = <A>(k, H, t, f) => f;
);

M = (
  key : Key;
  Bool = <A>(t : A, f : A) -> A;
  key
);

Bool : Type;
true : Bool;
false : Bool;

Bool = <A>(t : A, f : A) -> A;
true = <A>(t, f) => t;
false = <A>(t, f) => f;

print("hi")


Γ |- V == V


Data : Type;
Prop : Type;

(x : A) ->

x : Name;

Γ |- A : K  Γ |- E : (A : K) -> S
---------------------------------
Γ |- M : A % E

Γ |- M : A % E  Γ; x : A |- N : B
---------------------------------
Γ |- x = M; N : B % E

Γ |- A : K  Γ |- E : (A : K) -> S
---------------------------------
Γ |- (x : A) -> B % E : S

read : () -> A % E;

(x = read(); refl) : (x = read(); x == x)

f : () -> (x = read(); x == x) % E;
// dependent monads????
map : <A, B>(x : Maybe(A), f : (x : A) -> B) -> Maybe(B);
dep : <A>(m : Maybe(Type)) -> (A : Type, x : Maybe(A), x ==);
dep = <A>(m : Maybe(Type)) =>
  m <|
  | ("none") => Maybe(Never)
  | ("some", payload) => Maybe(payload);

map : <A, B>(x : Maybe(A), f : (x : A) -> B) -> Maybe(B);

dep_map : <A, B>(x : Maybe(A), f : (x : A) -> B(x)) -> dep(map(x, B));


f : () -> M(Type);
g =
  map<Type, ?>(f(), A => (M : A))

pure : <A>(x : A) -> M(A);
map : <A>(m : M(A), f : (x : A) -> B) -> M(B);
flat : <A>(m : M(M(A))) -> M(A);

is_pure : <A>(m : M(A)) -> Prop;
unwrap : <A>(m : M(A) & is_pure(m)) -> A;

dep : <A>(m : M(A), B : (x : A) -> Type) -> Maybe(?);

bind : <A>(m : M(A), f : (x : A) -> M(B(x))) -> M((x : A, B(x)));

m : Maybe(Type)

(A = unpack(m); A)

bind(pure(x), f) : M(B(x))

error : () -> Never % Error;
(x = error(); refl) : (x = error(); x == x);

Maybe(Maybe(Int)) -> Maybe(Int);

parse : () -> Option(Int);
map : <A, B>(x : Maybe(A), f : (x : A) -> B) -> Maybe(B);

map<Int, Type>(parse(), x => x == x)

dep_map : <A, B>(x : IO(A), f : (x : A) -> B(x)) -> IO(x);

dep_map<String, Type>(read(), x => x == x) : IO(Type);

Either = (A, B) =>
  | (tag : String, payload : A) & tag == "left"
  | (tag : String, payload : B) & tag == "right";

Log = A => (tag == "log") & A == Unit;

M : T % Log;

%(M) <|
| <A>(cont : (x : A) -> T, tag == "log") => cont()

Γ |- A is Erasable
-----------------------
Γ |- (M : A) :> A |-> M

(x => x(x))(x => x())

A & B

r |- _A `unify` Int & String

_c1 := Up(r)
_c1 |- _A `unify` Int
_A := _c1 |- Int

_c2 := Up(r)
_c1 := Link(r)
_c2 |- _A `unify` String
_A := _A[_c1] & _c2 |- String

_c1 := Right(_c2)
_c2 := Merge(r);

(x : _A) -> _A
S & T == (x : A) -> B

((x : Int) -> Int) &
((x : String) -> String) == <A>(x : A) -> A

Int & String == String & Int

_A == Int | String

<A <: Int | String>(x : A) -> A;

Int & String == Int | String | _A

Int & String == _A[_c0 & _c1]

Γ |- (M : A) :> T
----------------
(M : A & B) :> T

Γ |- M : A  Γ |- N : A
Γ |- A : Data
----------------------
Γ |- M == N : Prop

(id : M == (N : (x : A, y : B)))
(id : (x : fst(M) == fst(N), y : snd(M) == snd(N)))


(A : Type, refl : (x : A) -> x == x) =

(==) : _;
fun : <A, B>(f : (x : A) -> B) -> f == f;

(==) = <A>(x, y) =>
  <P>(v : P(x)) -> P(y) &
  (H : x == x) & <P>()

fun : (H : (x : A) -> f(x) == g(x)) == f == g;

(=~=) = <A, B>(x, y) =>
  <P>(v : P<A>(x)) -> P<B>(y);

Interval : Type;
left : Interval;
right : Interval;
segment : left == right;

Interval = (I : Interval) &
  <P>(l : P(left), r : P(right), s : l =~= r) -> P(I);
left = <P>(l, r, s) => l;
right = <P>(l, r, s) => r;

segment_f : <P>(l, r, s) -> left<P>(l, r, s) =~= right<P>(l, r, s);
segment_f = <P>(l, r, s) => s;

segment_f : <P>(l) -> (r) -> (s) -> left<P>(l)(r)(s) =~= right<P>(l)(r)(s);
segment_h : left =~= right;
segment_h = funext(segment_f);

segment = k(segment_h);

fun : (H : (x : A) -> f(x) == g(x)) == f == g;

Univ : Type;
Univ = {
  A : Type;
  Id : (x : A, y : A) -> Univ;
  refl : (x : A) -> Id(x, x);
};

(f : False) & <P>() ->
T_Unit : Type;
T_Unit = (Unit : T_Unit) & (
  T_unit : Type;
  T_unit = (unit : T_unit) & Unit(unit);
  (unit : T_unit) -> Type;
);

Unit = unit => (u : Unit(unit)) &
  <P>(v : P())

Id : <A : Type & Magic>(x : A, y : A)
Id = <A>(x, y) =>
  <P>(v : P(x)) -> P(y) &
  (H : )

x : A;
x = M;

[-1][M]

\lambda

L = b =>
  b
  | true => (x : Nat, y : L(false))
  | false => (x : String, y : L(true));
R = b =>
  b
  | true => (x : String, y : R(false))
  | false => (x : Nat, y : R(true));

pair : <A, L_B, R_B>(H : L_B == R_B) -> (x : A, y : L_B) == (x : A, y : R_B);

H_L : L(true) == R(false);
H_R : L(false) == R(true);

H_L = pair(H_R);
H_R = pair(H_L);

H_L = deg_ua(
  x =>  x
)
H_L = <P>(v : P(L(true))) => (
  v : P((x : Nat, y : L(false))) = v;
  H_R(v)
)
L(true) == (x : Nat, y : L(false));

f : (l : L(true)) -> (r : (x : Nat, y : L(false)), w : l =~= r);
f = l => l;

refl : <A, B>(
  f : (x : A) -> B,
  f_is_id : (x : A) -> x =~= f(x),
  g : (x : B) -> A,
  g_is_id : (x : A) -> x =~= g(x),
) -> A == B;



A : Type;
B : Type;
H : A == B;

T = (x : Nat, y : A);
U = (x : Nat, y : B);


Id = A => (f : (x : A) -> A, w : (x : A) -> x == f(x));

deg_ua : <A, B>(
  f : Id(A), g : (x : A) -> A,)

H_L : L true == R false;
H_R : L false == R true;

H_L = refl;

L true == R false
(x : Nat, y : L false) == (x : Nat, y : R true)

sig : <A, B>(l : (x : A, y : B x), r : (x : A, y : B x)) ->
  (H_L : fst l == fst r)
H : L false == R true;
L true == R false
(x : Nat, y : L false) == (x : Nat, y : R true)
L false == R true

fix : <T>(f : (x : T) -> T, g : (x : T) -> T) ->
  (H : (x : T) -> f(x) == g(x)) -> Y(f) == Y(g);

Unit : Type;
unit : Unit;

Unit = (u : Unit) & <P>(v : P(unit)) -> P(u);
unit = <A>(v : A) => v;

(u : Unit) & <P>(v : P(unit)) -> P(u :> Unit)
(u : Unit)
(<A>(v : A) => v : Unit)

(x : A, y : B) == (x : C, y : D) ≡ (x : A == B, y : C == D)
(x = A, y = B) == (x = C, y = D) ≡ (x : A == B, y : C == D)

(M : A & A) :> A
(true : Bool & Bool) :> Bool

T : (s : Size) -> Type;
T = s => (x : Nat, y : (i & i < s) -> T(i));

l : (s : Size) -> T(s);
l = s => (x = 1, y = l);

r : (s : Size) -> T(s);
r = s => (x = 1, y = r);

p : l == r;
p : (x : 1 == 1, y : l == r) = (refl, p);

try M with
| effect E => _
end

Γ |- M : A % E
-------------
Γ |- M : A % E

(x : A) -> B % E;

T % E : Type?

(M : A % E)
M % x = N

M <|
|
| %E => _
M %
| A => _
| B => _;
M <|
% E => _
| ;

transport<Bool, Bool>(not, not,
  T => (x : Nat, y : T), (x = 1, y = b)) == (x = 1, y = not(b));

Γ |- L : Type  Γ |- R : Type
Γ |- H : L == R  Γ |- P : (x : Type) -> Type
--------------------------------------------
Γ |- transport(H, P) : P(L) -> P(R)

Color = "red" | "green" | "blue";
f : (x : Color) -> Color =
  | "red" => "green"
  | "green" => "blue"
  | "blue" => "red";
g : (x : Color) -> Color =
  | "green" => "red"
  | "blue" => "green"
  | "red" => "blue";

L = (x : Nat, y : L);
R = (x : Nat, y : R);

w : <A, B>(x : A, y : B)
f_of_eq = (H : Color == Color) => transport(H, I);
g_of_eq = (H : Color == Color) => transport(symm(H), I);

transport_id_implies_k : <A, B>(
  p : A == B,
  H : transport(r, T => T) == id
) -> r == refl;

A : Type;
B : Type;
H : Equiv(A, B);

transport(H) : (x : A) -> B;


transport(ua(f, _)) == f;


transport(ua(t, _, _), A => (x : A, y : A)) ≡ p => (t(fst(p)), t(snd(p)));
transport(ua(t, _, _), A => (x : A) -> A) ≡ f => x => t(f(t(x)));

transport(ua(t, u, _), A => A == A) == (h : A == A) =>

T : Type;
T = (x : Nat, y : T);

U : Type;
U = (x : Nat, y : U);

f : (t : T) -> U;
f = (x, y) => (x, f(y));

H : f == id;


Γ |- M : A  Γ |- N : A
----------------------
Γ |- M == N : Type

T = <A, B>(x : A, y : B) -> A;
U = <A, B>(y : B, x : A) -> A;

in = (g) => (x, y) => g(y, x);
out = (f) => (y, x) => f(x, y);

A == B ≡ (in : (x : A) -> B, out : (y : B) -> A);

in = _ => true;
out = b => b;


H : Bool == Bool;
H = (not, not);

transport(H, T => T) == fst(H)


H = (not, id);

symm = <A, B>(H : A == B) =>
  transport(H, T => T == A)(refl);
symm = <A, B>(H : A == B) =>
  transport(
    H,
    T => (in : (x : T) -> A), out : (y : A) -> T
  )((id, id));
symm = <A, B>(H : A == B) =>
    (? ∘ id, ? ∘ id);
symm = <A, B>((in, out) : A == B) =>
  ((out, in) : B == A);

1 + 2 == 3

normal(1 + 2) == 3

symm(symm(H)) != H
symm(symm(H)) != H

f == g

transport(H, T => (I : Bool == T, b : T) ->
  b == transport(I, _)(true))
(refl, true);

false == true

Unit == Bool


1 == 2



symm
  : (H : A == A) -> B == A
  = transport(H, C => C == A);

true == false


(in, out) => (x => fst(H) <| in(x), out)

univalence : <A, B>(eq : Equiv(A, B)) -> A == B;
k : <A>(eq : A == A) -> eq == refl;

H : A == B;


mergesort == bubblesort

1 + n == n + 1


: M == N

random : () -> Bool;

refl : M == M

A : Type;
h : A == A;

x = random();

transport()

Block (B) ::=
  | x = C; B
  | C
Compute (C) ::=
  | V
  | C V
Value (V) ::=
  | x
  | x => B
  | (x : V) -> B

log : <A>(x : A) -> A % Log;
x <- random();
y <- random();
z = 1;

(x : Bool, y : Bool) -> _

random() == random();


random() == random();
(b = random(); A) == A
x = incr(1);
x == 2

random() == random()


A : Type;
x : A;

H : x == x ≡ (k : H == refl, e : Extensional(A))



(==) : <A>(x : A, y : A) -> Type;
transport : <A, x : A, y, P>(H : x == y, v : P(x)) -> P(y);

match : <P>(
  univ : (v : P(Type)) -> Type,
  pi : <A, B>(v : P((x : A) -> B(x))) -> Type,
  unknown : <A>(v : P(A)) -> Type,
) -> (A : Type, v : P(A)) -> Type;
(==) = <A>(x, y) =>
  match(
    ((S, T)) => Equiv(S, T),
    <A, B>((f, g)) => (x : A) -> f(x) == g(x),
    <A>(v) => error("not supported type")
  )(A, (x, y));

H : (x : Int) -> Int;

f = n => 1 + n;
g = n => n + 1;

H : A == B;
transport(H, T =>

)

H : ABool == BBool;
H = (in_b, out_b);

transport(H, T => (x : Bool) -> T)(id);

a_true => b_true
a_true => b_true

P(T) <|
| ["type"] => id
| ["pi", A, B] =>
| ["token"] => in

match

match : <P>(
  univ : P(Type),
  pi : (A, B) -> P((x : A) -> B(x)),
  trunct : (A, B) -> univ =~= pi(A, B),
) -> (A : Type) -> P(A);

map : (T : Type) -> (x : T) -> T;
map_is_id : <T>(x : T) -> map(x) == id(x);


map = match(
  univ = x => x,
  pi = (A, B) => x => x;
  trunct = (A, B) =>
    <P>(v : P(univ)) => (_ : P(pi))
);

  T <|
  | ["type"] => x => x
  | ["pi", A, B] => f => (x : A) => map(B)(f(x))
  | trunct = (A, B) =>
    hrefl;

match : <P>(
  univ : P(Type),
  pi : (A, B) -> P((x : A) -> B(x)),
  trunct : (A, B) -> univ =~= pi(A, B),
) -> (A : Type) -> P(A);

id = A => (x : A) =>
  match<A => A>(x, (A, B) => x, (A, B) => hrefl);


(H : P(f)) => P(r)

Extensional(A) ≡



A == B ≡ (
  in : (x : A) -> B,
  out : (y : B) -> A,
  rel_l : in =~= id,
  rel_r : out =~= id,
);


T = (
  A : Type;
  A -> A
);

U : Kind;
T : U -> Type;

univ : U;
pi : Π(A : U) -> (T(A) -> U) -> U;

U = Type -> (Π(A : U) -> (T(A) -> U) -> Type) -> Type;
T = k => k;

univ = _;
pi = A => B =>

U : Type;
T : U -> Type;

univ : U;
pi : Π(A : U) -> (T(A) -> U) -> U;

U = ⊥ -> (Π(A : U) -> (T(A) -> U) -> U) -> ⊥;
T =

Bool : U;
true : T(Bool);
false : T(Bool);
elim : T(Bool) -> Π(A : U). T(A) -> T(A) -> T(A);

Bool = Π(A : U). T(A) -> T(A) -> T(A);
true = A => x => y => x;
false = A => x => y => y;
elim = b => b;

Bool : U;
true : T(Bool);
false : T(Bool);
match_A : T(Bool) -> A -> A -> A;
match_B : T(Bool) -> B -> B -> B;

Bool = u => u;
true = A => x => x;
match_A = b => l => r => b(A)(l)(r)



T(forall(B)) == (x : U) ->

Bool : Type;
true : Bool;
false : Bool;

Type = (equal, elim);

id = <A>(x : fst(A)) => x;

match : <P>(
  univ : P(Type),
  pi : (A, B) -> P((x : A) -> B(x)),
) -> (A : Type) -> P(A);

match : <P>(
  univ : P(Type),
  pi : (A, B) -> P((x : A) -> B(x)),
) -> (A : Type) -> P(A);


U : Type;
T : (x : U) -> Type;

(==) : <A>(x : A, y : A) -> A;
transport : <A, x : A, y, P>(H : x == y, v : P(x)) -> P(y);
K : <A, x : A, P>(H : x == x) -> transport(H, v) == v;

(==) = <A>(x, y) =>
  A <|
  | ["type"] => (f : );
A : Type;
B : Type;

A == B ≡ (f : (x : A) -> B, )


r : (x) -> f(x) == g(x) |- f ≡ g
r : (x) -> f(x) == g(x) |- y => f(y) ≡ y => g(y)
r : (x) -> f(x) == g(x), y |- f(y) ≡ g(y)
r : (x) -> f(x) == g(x), y |- f(y) ≡ f(y)


Γ, z |- M[x := z] == N[y := z]
------------------------------
Γ |- x => M == y => N

Γ, z |- f(z) == g(z)
---------------------------
Γ |- x => f(x) == y => g(x)

Γ |- M[b := true] == N[b := true]
Γ |- M[b := false] == N[b := false]
-----------------------------------
Γ, b : Bool |- M == N


not = b => case(b, false, true);
not_rev_not = b => case(not(b), true, false);

|- not == not_rev_not
b |- not(b) == not_rev_not(b)
|- not(true) == not_rev_not(true)
|- not(false) == not_rev_not(false)

p : x == x |- p == refl(x)
p : x == x |- x == refl(x)

Γ, p : x == y |- cast(p, x) == N
--------------------------------
Γ, p : x == y |- x == N

Γ, p : Unit == Id |- P(transport(p,)) == N
------------------------------------
Γ, p : Unit == Id |- P(Unit) == P(Id)


f(x) == g(x) |- f == g

H : x == y;

H_H : x == x = H(H);

H : (x : A) -> f(x) == g(x);

H =

transport = (H, P) => (v : P(f)) => (
  P(f) <|
    | ["univ"] =>
);

P(f) = Type;
P(g) = Type;

Prop : Type;
Prop = (
  A : Type,
  w : (x : P)
);
transport(H, )
match<T => >


----------------- // refl
Γ |- (M : A) :> A

Γ |- p : A == B
----------------- // refl
Γ |- (M : A) :> B

(M : A) != M

T : (i : Size, A : Type) -> Type;
T = A => (x : A, y : (j : Size < i) -> T(j, A));

U : (i : Size, A : Type) -> Type;
U = A => (x : A, y : (j : Size < i) -> U(j, A));

f : (i : Size, A : Type) -> (p : T(i, A)) -> U(i, A);
f = (i, A) => (p) => (
  (x, y) = p;
  (x, j => f(j, A)(y))
);

f_id : (p : T) -> f(p) =~= id(p);
f_id = p => (
  (x, y) = p;
  (hrefl, f_id(y))
);

A == B ≡ (
  f : (x : A) -> B,
  w : (H : A == B) -> f == id;
);

match : <P>(
  univ : P(Type),
  pi : (A, B) -> P((x : A) -> B(x)),
) -> (A : Type) -> P(A);

match : <P>(
  univ : P(Type),
  pi : (A, B) -> P((x : A) -> B(x)),
) -> (A : Type) -> P(A);


H : Bool == Bool;
H = (not, not);

transport(H, T)

H : A == B;
transport(H, T => (x : T) -> T) == f => x => in(f(in(x)));

transport(H, T => (x : T) -> Bool) == f => x => f(not(x));


map =
  match(
    x => x,
    (A, B) => x => ,
  )(P(f));

(A : Type) =>
P = T => T;

f : (x : A $ 0) -> _;

is_prop = (A : Type) => (x : A, y : A) -> x == y;

A : Prop
M : A
N : A

f(M) == f(N)

Unit : Type(0);
unit : Unit;

Unit = (u : Unit, i : <P>(x : P(unit)) -> P(u), w : u == fst(u));

≡

Γ |- M : (x : A) -> B
---------------------
Γ |- M |-> x => M(x)


------------------------------------------
Γ |- %beta : P((x => N)(M)) == P(x = M; N)

-----------------------------------------
Γ |- %subst : P(x = M; N) == P(N{x := M})

-----------------------
(x => x)(N) : T $ 1 + N

(x => )

M(N) : T % 1 + |M| + |N|

beta : <M, N>() -> (x => M(x))(N) == x = N; M


Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool) &
  (P : (b : Bool) -> Type, then : P(true), else : P(false)) -> P(b);
true = (P, then, else) => then;
false = (P, then, else) => else;

T_Unit : Type;
T_unit : (Unit : T_Unit) -> Type;

T_Unit = (Unit : Type) & (unit : T_unit(Unit)) -> Type;
T_unit = Unit => (unit : T_unit(Unit)) & Unit(unit);

Unit : (s0 : Size < ∞) -> Type;
unit : (s1 : Size < s0) -> Unit(s1);

Unit = s0 =>
  (
    u : (s1 : Size < s0) -> Unit(s1),
    i : <P>(v : P(unit)) -> P(u),
  );
unit = s1 => (
  u = unit,
  i = <P>(v) => v
);


Unit0 : (s0 : Size < ∞) -> Type;
Unit0 =
a_b : (s0 : Size < ∞) ->
Both : (
  A : Type,
  B : (a : A) -> Type,
  m_a : (a : A, b : B(a)) -> A,
  m_b : (a : A, b : B(a), w_a : a == m_a(a, b)) -> B(a),
) -> (a : A, b : B, w_a : a == m_a(a, b), w_b : b == m_b(a, b));
Both = (A, B, m_a, m_b) => (
  (a : A, b : B, w_a : a == m_a(a, b), w_b : b == m_b(a, b));
  (a, b, w_a, w_b) = (
    i_a : A;
    i_a = m_a(a, b);

    i_b : B(a);
    i_b = m_b(a, b);

    (
      a = i_a;
      b = i_b;
      w_a = refl;
      w_b = refl;
    )
  )
)

Unit : Type
Unit : T_Unit;
Unit =

Both = (
  A : Type,
  B : (a : A) -> Type,
  m_a : (a : A, b : B(a)) -> A,
  m_b : (
    a : A;
    a = m_a(a);
    (b : B(a)) -> B(a)
  ))

U : Set;
T : (t : U) -> Set;

U = (t : U) & <P>(
  arrow : <A, B>() -> P((x : A) -> B)
) -> P(t);

A : Type;
B : Type;
C : Type;

H : A =t= B;
P : A =p= B;
P = transport(H, )

W : B =p= C;

G : A =p= C;
G = transport(W, T => A =p= T, x : A =p)

Bool = <A>(x : A, y : A) -> A;
true : Bool = <A>(x, y) => x;
false : Bool = <A>(x, y) => y;

Bool0 = (b : Bool, w : )

Unit0 : Type;
Unit0 = (u : Unit, w : Path(u :> Unit, u));

Unit0 : Type;
Unit0 = (u : Unit, w : Path(fst(u), u));

Unit = (u : Unit, i _,)

(x : A) & B(x);

p : (x : A, r : (y : A, h : x == y))
snd(p) == (fst(p), refl);



(x : Type, r : (y : Type, h : x == y))

(x = Bool, r = (y = Bool, h = h));

()
(M : Bool & Bool)
(M : (x : Bool, y : Bool, H : x =~= y));

Unit : Type;
Unit = (u : Unit, x : _, )

-------------------------
Γ |- (x : A, ...B) : Type


f = (x : { id : Int; name : String; }) => x;

map : <M : Meta>
map = <M : Meta>() =>
  match M {}
(x :)

I_Bool = c_b =>

  <P>(x : P(c_true), y : P(c_false)) -> P(c_b);

I_Bool(c_true)


<P>(x : P(c_true), y : P(c_false)) -> P(c_true)



(c_b : C_Bool, i_b : <P>(x : P(c_true), y : P(c_false)) -> P(c_b))
(c_b : C_Bool, i_b : c_b(Prop)(c_b == c_true)(c_b == c_false))
(c_b : C_Bool, i_b : c_b == c_true || c_b == c_false)

T =
  | ("a");

() // unit
(x : A) == (x : A, ...()) // named value
(x : A, y : B) == (x : A, ...(y : B, ...())) // pair

Row : Type;
Row =
  | ()
  | (<A : Type>, <R : Row>, x : A, ...R);

User = {
  id : Nat;
  name : String;
};

(x : Int, ...())

(x : Int, ...User);
(x, { id; name; })
(x, ())

<R>(x : Int, ...R) =>
(x : Int, ...());


<R : Row>(x : A, ...R)

()
(x : (y : Int)) == Int

T : Type;
T = (x : Nat, y : T);

U : Type;
U = (x : Nat, y : U);


f : (p : T) -> U;
f = (x, y) => (x, f(y));

g : (q : U) -> T;
g = (x, y) => (x, g(y));

strict_ua : <A, B>(
  f : (x : A) -> B;
  g : (x : B) -> A;
  w_f : (H : A == B) -> f(x) == id(x);
  w_g : (H : A == B) -> g(x) == id(x);
) -> A == B;

Is_set = (A : Type) => <x>(p : x == x) -> p == refl;

H : Bool == Bool;

UIP : <A>(x : A, y : A, p : x == y, q : x == y) -> p == q;
K : <A>(x : A, p : x == x) -> p == refl;




transport(H, T => T)(true) == true
ua(idEquiv)

strict_ua(f, g) == refl
strict_ua(f, g) : T == U

x : T;
(x : U)


f : (l : T) -> U;
f = (l) => (
  (x, y) = l;
  f_y = f(y);
  (x, f_y)
);

H : T == U;

w : (x : T) -> f(x) =~= id(x);
w = (x : T) => (refl, w);


measure : (A : Type) -> Size;
measure(Type) = 1;
measure((x : A $ n) -> B) = 1 + n + measure(B);
measure(<G> -> T) = ω + measure(T)

Γ |- <x> -> A  Γ |- G : Grade
-----------------------------
Γ |- M<G> : A[x := G]


U : Type;
nat : U;
arrow : (param : U, body : U) -> U;

T : (x : U) -> Type;

U =


(x : A $ 1) ->
(x : A) -> B;

<G> -> B $ G

ω + 1
n

<G> => M<G>

M

measure((x : A $ n) -> B) > measure(x = N; B)

-----------------------------
(x => M) N |-> M[x := N]
M N :

T : (x : A $ m) -> B $ n;

f = (G : Grade) =>

<m : Nat, n, x, y>(H : m > n) -> (m, x) > (n, y)
<m : Ord, n, x>(H : m > n) -> (x, m) > (x, n)

2 * ω + n

m * ω + n

f = λ. \1;
g = λ. _A;

f == g
f(x) == g(x)
x == _A[x]

_A[x] == x

x = y; L<M>

f \1 == g \1
\1 == _A[\1]
\1 == _A


(λ. M)(x)  (λ. N)(x)
-------------------
λ. M == λ. N

n : 1 = 1;
n : (x : Int, p : x == 1) = (1, refl);

(x : Int, p : x == 1) == (x = 1, p = refl);


f : (x : A) -> B(x);
g : (x : A) -> B(x);

(f == g) ≡ (x : A) -> f(x) == g(x)

p : (x : A, y : B);
q : (x : A, y : B);

(p == q) ≡ (x : fst(p) == fst(q), y : snd(p) == snd(q));


(Bool == Bool)

(==) : <A>(x : A, y : A) -> Type;
(==) = <A> =>
  A <|
  | ["fun", A, B] => (f, g) => (x : A) -> f(x) == g(x)
  | ["pair", A, B] => (p, q) => (x : fst(p) == fst(q), y : snd(p) == snd(q));

(P : (x : A) -> Type) -> (v : P(x)) -> P(y)

funext : (H : (x : A) -> f(x) == g(x)) -> f == g;
funext = H => H;

nat = Nat;
pair = (x : A, y : B);
arrow = (x : A) -> B;

() == ()
(x : A, ...()) == (x : A)
(x : A, ...(y : B, ...())) == (x : A, y : B)
(x : A, ...(y : B, ...(z : C, ...()))) == (x : A, y : B, z : C)

fst = <A, R>(p : (x : A, ...R)) => (
  (x, ..._) = p;
  x;
);
snd = <A, B, R>(p : (x : A, y : B, ...R)) => (
  (_, y, ..._) = p;
  y;
);

User = {
  id : Nat;
  name : String;
};

(...A, x : Int)

T = (x : A, ...User);
T = (x : A, id : Nat, name : String);
T = (x : A, user : (id : Nat, name : String));


(p : (x : A, y : B, ...R)) -> C
==
(x : A, y : B) -> C;

point : (x : Int, y : Int) = (1, 2);



p : T;

(x, user : User) = p;


T : Type;
x : T;

f = (x : A) ->

ω : Size
(x : A)
Value :

Y : (A : Type) -> Type $ 0;
Y =
Fix : (A : Type) -> Type $ 0;
Fix = A => <K>(
  i : Size,
  f : (self : (j : Size < i) -> Fix(A) $ ?) -> K $ ) -> K;

fix = f => f(f);

t : (s : Size) ->


Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool) & <P>(t : P(true), f : P(false)) -> P(b);
true = <P>(t, f) => t;
false = <P>(t, f) => f;
opaque_b : Bool;

Bool : {
  @lock Bool : Type;
  true : Bool;
  false : Bool;

  ind : <P>(b : Bool, t : P(true), f : P(false)) -> P(b);
} = {
  Bool = Bool;
  true = <P>(t, f) => opaque_b<P>(t, f);
  false = false;
  ind = <P>(b, t, f) => b<P>(t, f);
};

b : Bool.Bool;

x = opaque_b<P>(1, 2);

f : <A>(f : A) -> A;

Bool = {
  Bool = <A>(t : A) -> (f : A) -> A;
  true = <A>(t) => (f) => t;
  false = <A>(t) => f<A>;
  ind = <P>(b, t, f) => b<P>(t, f);
};


b : Bool.Bool;
x = Bool.ind(• Bool.false, 1, 2)

Nat =
  | Z
  | S (pred : Nat);

Z4 =
  | Z
  | S (pred : Nat)
  | mod4 : Z == S(S(S(S(Z))));

mod4 : Z == S(S(S(S(Z))))
Z4.fold : <A>(
  n : Z4,
  z : A,
  s : (acc : A) -> A,
  mod4 : z == s(s(s(s(z))))
) -> A;


5 + 1 == 6
1 + 1 == 2

Bool.case : <A>(
  b : Bool,
  t : A,
  f : A,
  rel : t == f
) -> A;

op(true) == op(false)

f : (i : Interval) -> Nat;

f = i =>
  i <|
  | "left" => 1
  | "right" => 1;

T = (A : Type) => A;

A = Nat;
f = (x : A) => x;

mod : f(left) == f(right)

Bool.case(
  true,
  1,
  1,
  refl
)
Bool.rel : true == false

Z4 =
  | Z
  | S (pred : Nat)
  | mod4 : Z == S(S(S(S(Z))));

op(S(S(S(S(S(Z)))))) == op(S(Z))
Bool : Exists(Bool => {
  true : Bool;
  false : Bool;

  ind : <P>(b : Bool, t : P(true), f : P(false)) -> P(b);
}) = {
  Bool : Type;
  true : Bool;
  false : Bool;

  Bool = (b : Bool) & <P>(t : P(true), f : P(false)) -> P(b);
  true = <P>(t, f) => t;
  false = <P>(t, f) => f;

  ind = <P>(b, t, f) => b<P>(t, f);
};

Exists : <A>(B : (x : A) -> Type) -> Type;
fst : <A, B>(s : Exists<A>(B)) -> A;
snd : <A, B>(s : Exists<A>(B)) -> B(fst(s));

Exists = <A>(B) => (s : Exists<A>(B)) &
  <P>(
    k : (x : A, y : B(x)) -> P(x),
    mod : (r : Exists<A>(B)) ->
      k(fst(s), snd(s)) == k(fst(r), snd(r));
  ) -> P(fst(s));

Exists : <A>(B : (x : A) -> Type) -> Type
Exists = <A>(B) =>
  <K>(
    k : (x : A, y : B(x)) -> K,
    mod : (x_r : A) -> k(x) == k(x_r)
  ) -> K;

// x are opaque bits, right are concrete bits
Spec : <A, B>(x : A, y : B(x)) -> Type;
Spec = <A, B>(x, y) => (s : Spec<A>(B)) &
  <P>(
    k : (x : A, y : B(x)) -> P(x),
    mod : (r) -> k(fst());
  ) -> P(fst(s));

mod : <A, B, x, y>(l : Spec<A, B>(x, y), r) -> l == r;


Prop : (A : Type) -> Type;
Prop = (A : Type) =>
  <K>(
    k : (x : A) -> K;
    mod : (x : A, y : A) -> k(x) == k(y);
  ) -> K;

S_Bool = Spec<Type, Bool => {
  true : Bool;
  false : Bool;
  case : <A>(b : Bool, t : A, f : A) -> A;
}>(<A>(t : A, f : A) -> A, {
  true = <A>(t, f) => t;
  false = <A>(t, f) => f;
  case = <A>(b, t, f) => b<A>(t, f);
});

Bool : S_Bool;

Bool(
  (x, y) => y.true
) : fst(Bool)

f(Bool =>)


read("a.txt") == read("a.txt")
f(x) == f(x)


random() == random() // is false

x == x

// assume that M == M is not true for Type
U : Type;
T : (x : U) -> Type;
mod_refl : <A : U, x : T(A)>() -> x == x;

Nat : U;
zero : T(Nat);
incr : (x : T(Nat)) -> T(Nat);

p : incr(zero) == incr(zero) = mod_refl();

f(x) == f(x)
x = random();
p : x == xl
 = refl();
M == M : A % ⊥
x == x : A % IO

CType : Type;
Type : Type;

A : CType;

Data : Type;
Line : Type;

()

Effect : Type;
IO : Effect;

(%) : (A : Type, E : Effect) -> Type;

x : Type % IO = T(Nat);
y : Type % IO : T(Nat);

x; y |- z => (x + z)

0 | x
1 | M

x == y

f = x => (
  y : Nat;
  y = 1 + y;
  y
);


f = λ. (
  y = \1 + 1;
  y
);

f = λ. (
  y = x + 1;
  y
);
x = f(1);
y = f(2);
f(1) |-> 2
f(2) |-> 3


(f : (x : A) -> B) => (
  f = x => f(x);
  f(x)
);

f x


Grade : Type;

f = (x : Nat $ 2) => x + x;

Nat : Type $ 0;
Nat = <A>(z : A $ 1, s : (pred : Nat) -> A $ 1) -> A;

size : (n : Nat) -> Size;

f : Nat;
f = 1 + f;

n = 1 + pred
fold : (s : Size) -> <A>(n : Nat, z : A, s : (pred : A) -> A $ size(n)) -> A;
fold = <A>(n, z, s) =>
  n(z, pred => s(fold(pred, z, s)));

(n : Nat) => fold(n, "a", x => M)

M : A

T = (x : Nat $ n) -> Nat;
measure : (A : Type) -> Size;

measure((x : A $ n) -> B) == 1 + (measure(A) * n) + measure(B)
measure(<G> -> T) == 1 + ω ^ ω + measure(T)

dup : (n : Nat $ 1) -> (n2 : Nat $ 2, h : n == n2);

(n : Nat : ∞) => _;

Type(n)

1 + ω + measure(T)
1 + n + measure(T)


M<n>

f : <G> -> (x : A $ G) -> B
f<ω>

6 + measure(T)

M : (x : A $ n) -> B
M(N) : x = N; B


f = (x : Nat $ 2) => x + x;
g = (x : Nat $ 4) => g(x) + g(x);


(n : Nat) :> Ord

ω : Ord

ω > n
ω + 1 > ω

2ω > ω + n
3ω > 2ω + n

measure(A : Type(0)) == 0
measure(A : Type(1)) == ω
measure(A : Type(2)) == 2ω
measure(A : Type(3)) == 3ω
measure(A : Type(ω)) == ω ^ 2

Type(l) : Type(1 + l);

measure((x => M)(N)) == 1 + n
measure(x = N; M) == n


Type : Type;

Set : Type;
T : (x : Set) -> Type;
mod_refl :  <A : Set, x : T(A)>() -> x == x;

V == V : Name

Nat : Type $ 0;
size : (n : Nat) -> Grade;

Nat = (n : Nat) &
  <A>(z : A $ 1, s : (pred : A) -> A $ size(n)) -> A;

<G> -> (x : A $ G) -> B

Bool : Type(0);
Bool.case : <l, P : (x : Bool) -> Type(l)>(
  b : Bool,
  t : P(true),
  f : P(false),
) -> P(b)

ω : Size;

A : U;
M : A

x = Nat;
x ==
Type(1) == ω

equal_value(V1, V2)

Pure
Pure % Effect

f : (M : (x : Type) -> Type) -> M(Nat) == M(Nat);
er

(==) : <A>(x : A, y : A) -> Type;
(==) = <A>(x, y) => <P>(v : P(x)) -> P(y);

Id = (A : Type) -> (x : A) -> A;

(+)
(*)
(%)

(f : (A : A) -> B(x)) ->


Bool : {
  Bool : Type;
  true : Bool;
  false : Bool;

  ind : <P>(b : Bool, t : P(true), f : P(false)) -> P(b);
} = {
  Bool : Type;
  true : Bool;
  false : Bool;

  Bool = (b : Bool) &
    <P>(b : Bool, t : P(true), f : P(false)) -> P(b);
  true = <P>(t, f) => t;
  false = <P>(t, f) => f;
  ind = <P>(b, t, f) => b<P>(t, f);
};


Bool.ind(Bool.true, t, f) == t

Bool.ind(Bool.true, t, f) == f

b => Bool.ind(b, t, f)

b => Bool.ind(Bool.true, t, f)
b => b<_>(t, f)

(x = M; (x, x)) == (y = N; (y, y))

x = M;
y = N;
(x, x) == (y, y)

x/1 = M;
z = M;
y/1 = N;
(x/1, z) == (y/1, y)

Γ |- A == C  Γ |- B == D
------------------------
Γ |- A & B == C & D

Nat & Nat |->

<A>(x : Nat & A) => _

Bool & Bool
(x : Bool, y : Bool, h : x == y) ==? Bool

(x : Bool, p : (y : Bool, h : x == y))
(x : Bool) // p is a singleton so proof irrelevant

(x : Bool, y : Bool, h : x =~= y) ==? Bool
(x : Bool, p : (y : Bool, h : x =~= y))
(x : Bool) // p is a singleton, because Bool is a set

(x : Nat) & Bool
(x : A, p : (y : B, h : A =~= B)) == A

(
  x : A,
  h_l : A == B,
  p : (y : B, h_r : h_l(x) == y)
)

Nat & Nat == Nat
(
  x : A,
  h_l : A == B,
  p == (h_l(x), refl)
)

H : Bool == Bool;
transport(H, T => (x : T) -> (y : T, p : x == y))
Nat & Bool

Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool, i : _, p : fst(b) == b)



T = (x : A, p : fst(x) == x);

p : (x : A, p : fst(x) == x)

p == (x = fst(fst(p)), p : fst(fst(p)) == fst(fst(p)) = refl)

Is_set = (A : Type) =>
  <x : K, y>(p : x == y, q) -> p == q;

Set = (A : Type) => <K>(
  k : (x : A) -> K,
  mod : Is_set(K),
) -> K
mod : <A, x : Set(A)>(p : x == x) -> p == refl;

snd(p) == refl


b : Bool
(b = fst(b))
left : <A, B>(x : A, y : B) -> A;
Bool = <A>(t : A, f : A) -> A;
Nat = <A>(z : A, s : (acc : A) -> A) -> A;

(b : Bool, n : Nat) -> b =~= n;


(b : Bool, n : Nat) ->
  <A>(x : A) ->
    b<A>(x, ?) ==
    n<A>(x, ?)

(
  x : T,
  p1 : (x : T) -> Bool,
  p2 : (x : T) -> Nat,

)
(x : Bool, y : Nat, p : Bool)
(x : A, p : (y : B, h : x =~= y))

(y : B, h : x =~= y)

(b : Bool, p : fst(b) == b, )

(x : A)
(Bool, p = (Bool, ua(not_equiv)));

x : (Bool, ua(not_equiv)) == y : (Bool, refl);

f : (H : fst(x) == fst(y)) -> transport(H, snd(x)) == snd(y)

Interval =
  | left
  | right
  | mod : left == right;

mod : left == right;

Nat : Type;
Nat =
  | Z
  | S (pred : Nat);

Z4 : Type;
Z4 =
  | A
  | B
  | C
  | D;
Z4 : Type;
Z4 =
  | Z
  | S (pred : Z4)
  | mod4 : Z == S(S(S(S(Z))));

Term : Type;
eval : Term -> Term;
Term =
  | Literal(n : Nat)
  | Add(a : Term, b : Term)
  | mod : (term : Term) -> term == eval(term);

Nat64 : Type;
Nat64 =
  | Z
  | S (pred : Nat64)
  | mod : 0 == 2 ** 64;


fold : <A>(
  n : Z4,
  z : A,
  s : (acc : A) -> A,
  mod4 : z == s(s(s(s(z)))),
) -> A;

case(
  left,
  x,
  x,
  refl
) == x

Bool : {
  T : Type;
  true : T;
  false : T;
} = {
  T = <A>(t : A, f : A) -> A;
  true = <A>(t, f) => t;
  false = <A>(t, f) => f;
};


p : (x : A, y : B(x));
q : (x : A, y : B(x));

// this
H_S : (H_F : B(fst(p)) == B(fst(q))) ->
  transport(H, snd(p)) == snd(q);

// not this
H_S : snd(p) == snd(q);

// or this
H_S : (H_F : fst(p) == fst(q)) ->
  transport(H, snd(p)) == snd(q);

H_S : (H_F : B(fst(p)) == B(fst(q))) ->
  transport(H, snd(p)) == snd(q);

f : (x : A) -> B;

strict :
(H : A == B) ->
  (x : A) -> transport(H, f(x)) == id(x)

(x = Bool, ua(not_equiv)) == (x = Bool, refl)
(x : Type, p : x == Bool)

Unit : Type;
unit : Unit;


Bool : Type;
Bool = (b : Bool) & <A>(
  t : A,
  f : A,
  mod : (p : b == b) -> p == refl;
) -> A;

(x : A) @-> B

M : A & B
fst(M) : A
snd(M) : B

Self : (A : Type, B : (x : A) -> Type) -> Type;
left : <A, B>(s : Self(A, B)) -> A;

a_is_rec : <A>(s : Self(A, B)) -> A == Self(A, B);
left_is_rec : <A>(s : Self(A, B)) -> s == left(s);

Self = (
  x : A,
  y : B,
  p_l : A == Self(A, B),
  p_r : x == left(x),
);


Unit0 : Type;
Unit0 = (u : Unit0, i : _);

Unit : Type;
Unit = (u : Unit0, p : fst(u) == u);

Interval : Type;
left : Type;
right : Type;
mod : left == right;

Interval = <A>(l : A, r : A, mod : l == r) -> A;
mod =


Box : Type;
self : (b : Box) -> Box;
mod : (b : Box) -> b == self(b);

Box = <K>(
  x : K,
  mod : (b : Box) -> b == self(b)
) -> K;

mod = b => <K>(
  x : K,
  mod : (b : Box) -> b == self(b)
) : b<K>(x, mod) == self(b)<K>(x, mod) =>
  transport<b => b<K>(x, mod)>(mod(b), refl);

Box : Type;
box : Box;
self : (b : Box) -> Box;
mod : (b : Box) -> b == self(b);

Box = (mod : (b : Box) -> b == self(b)) -> Box;
box = mod => box;
self = (b) => (mod) => b(mod);
mod = (b) => (mod) : b(mod) == self(b)(mod) =>
  transport<x => b(mod) == x(mod)>(mod(b), refl);


Self : (A : Type, B : (x : A) -> Type) -> Type;
left : <A, B>(s : Self(A, B)) -> S;

Self = (A, B) =>
  | fix : (f : (x : A) -> B) -> Self(A, B)
  | mod : (s) -> f(left(s)) == left(both);

Bool : Type;
self : (b : Bool) -> Bool;

Bool =
  (mod : (b : Bool) -> b == self(b)) ->
  (b : Bool, i : <K>(t : K, f : K) -> K);
self = (b) => fst(b);

true : Bool;
false : Bool;

true = mod => (b = false, i = <K>(t : K, f : K) => t);
false = mod => (b = false, i = <K>(t : K, f : K) => f);


true : Bool;
false : Bool;


P(self(box))

mod(box)(box) == transport<x => box(mod) == x(mod)>(mod(box), refl)


transport<x => (mod) -> box(mod)(mod) == x(mod)(mod)>(mod(box), refl)
(mod2) => transport<x => box(mod)(mod2) == x(mod)(mod2)>(mod(box), refl)

box(mod) == box




(u = u, p = transport(p, refl))
(u : Unit0, p : fst(u) == u)

(u : Unit, q : fst(fst(u)) == fst(u)) -> snd(u) == q;


A : Type;
A = (x : A, p : x = fst(x), y : B(x))

(x = fst(x))

mod : (b : Bool) -> (p : b == b) -> p == refl;
Bool(

)

(:>)


hId(A, B, H, f, id)

----------------
A & B :> _A


Box : Type;
box : Box;
self : (b : Box) -> Box;
mod : (b : Box) -> b == self(b);

Box = (m : (b : Box) -> b == self(b)) -> Box;
box = m => box;
// self = (b) => (mod) => b(mod);
mod = (b) => (m) : b(m) == self(b)(m) =>
  transport<x => b(m) == x(m)>(m, refl);

Unit : Type;
unit : Unit;

Unit = (u : Unit, i : <P>(v : P(unit)) -> P(u));
unit = (u = unit, i = <P>(v) => v);

S_Unit :

self : (u : Unit) -> Unit;
mod : (u : Unit) -> u == self(u);

Unit = (m : (u : Unit) -> u == self(u)) =>
  (u : Unit, i : <P>(v : P(unit)) -> P(u));
unit = (m) => (u = unit, i = _);
self = (u) => (m) => fst(u(m));
mod = (u) => (m) : u(m) == self(u)(m) =>
  (
    u : fst(u(m)) == fst(self(u)(m)) =
      transport<x => fst(u(m)) == fst(x)>(m(u), refl),
    i = _;
  );


unit = unit(mod);
ind : <P>(u : Unit, v : P(unit)) -> P(u);
ind = <P>(u, v) => (
  i : <P>(v : P(unit)) -> P(fst(u(mod))) = snd(u(mod));
  i : <P>(v : P(unit)) -> P(u(mod)) = _;
)


Interval : Type;
left : Interval;
right : Interval;

Interval = <K>(l : K, r : K, m : l == r) -> K;
left = <K>(l, r, m) => l;
right = <K>(l, r, m) => r;

mod : left == right = <K>(l, r, m) => m;

is_prop : <A>(p : left == left) -> p == refl;
is_prop = <A>(p) => p

Box : Type;
box : Box;
self : (b : Box) -> Box;

Box = (m : (x : Box) -> x == self(x)) ->
  T;

Interval : Type;
left : Interval;
right : Interval;

Interval = <K>(
  l : K,
  r : K,
  m : (p : Interval, q) -> p == q,
) -> K;
left = <K>(l, r, m) => l;
right = <K>(l, r, m) => r;

mod : (p : Interval, q) -> p == q;
mod = (p, q) : <K>(l, r, m) : p<K>(l, r, m) == q<K>(l, r, m) =>
  transport(m(p, q), refl)

i(1, 2, mod)
Bool : Type;
true : Bool;
false : Bool;

Bool = (m : <b : Bool>(p : b == b) -> p == refl) ->
  (b : Bool, i : <P>(t : P(true), f : P(false)) -> P(b));
true = m => (b = true, i = <P>(t, f) => t);
false = m => (b = false, i = <P>(t, f) => f);

mod : <b : Bool>(p : b == b) -> p == refl;
mod = <b>(p) => <i> : b == b => <j> : Bool =>
  (m) => m<b>(p)<i><j>(m);


p : x == y;
p 0 == x
p 1 == y

(<i> -> p i : x == y)

A : Type;
(A == B) ≡ (
  f : (x : A) -> B,
  g : (y : B) -> A,
);

f : (x : A) -> B;
(f == g) ≡ (x : A) -> f(x) == g(x);

p : (x : A, y : B);
(p == q) ≡ (x : fst(p) == fst(q), y : snd(p) == snd(q));


H0 : p == q;
(H0 == H1) ≡ (P, x) -> transport<P>(H0, x) == transport<P>(H1, x);

Box =

Self : (A : Type, B : (x : A) -> Type) -> Type;
self : <A, B>(s : Self(A, B)) -> Self(A, B);
left : <A, B>(s : Self(A, B)) -> A;
unfold : <A, B>(s : Self(A, B)) : B(left(s));

Self =
  | fix : (H : A == Self(A, B), f : (x : A) -> B) -> Self(A, B)
  | mod : (s : Self(A, B)) -> s == self(s);

Eq : <A>(x : A, y : A) -> Type;
refl : <A>(x : A) -> Eq(x, x);

Eq =
  <P>(v : P(x)) -> P(y) &
  (eq : Eq<A>(x, x)) &
  <P>(v : P(refl(x))) -> P(eq);
refl = <P>(v) => v;

(eq : Eq<Type>(Bool, Bool))
: <P>(v : P(refl(Bool))) -> P((eq :> Eq<Type>(Bool, Bool)))


fst(x) == fst(fst(x))

box = <K>(k, m) => k(box);
self = b => fst();
mod =


S : {
  User : Type;

  create_user : (user : User) -> () % IO;
  fetch_users : () -> List(user) % IO;

  test : (eduardo : User) -> (
    users =
      create_user(eduardo);
      fetch_users();
    users == [eduardo]
  );
};


Interval : Type;
left : Interval;
right : Interval;

Interval = <K>(
  l : K,
  r : K,
  m : (p : Interval, q) -> p == q,
) -> K;
left = <K>(l, r, m) => l;
right = <K>(l, r, m) => r;


x |- x

x => y => k =>
  (k y) x (y => x => )

x =>
x = \0; y => \0 + \0
x = \0; y => y + x
y => x + \1

x, y

------
x |- x

Γ |- M  Δ |- N
--------------
Γ, Δ |- M N

Γ |- M  Δ |- N
--------------
Γ, Δ |- M N

HEAD;
\0
Term =

λ. λ. \1

λ. \0
fix (s => self()

(x : Nat $ 2) -> x + x

(x_0 : Nat $ , x_1 : Nat) -> x + x

Term =
  | \0
  | λ. M
  | M N;

\0[K] == K
λ. M ==

(M N)[K] == M[K]

0 0

Interval : Type;
left : Interval;
right : Interval;
mod : left == right;

Interval = <K>(
  l : K,
  r : K,
  m : l == r,
) -> K;

Interval : Type;
left : Interval;
right : Interval;

Interval = <K>(
  l : K,
  r : K,
  m : (p : Interval, q) -> p == q,
) -> K;

left = <K>(l, r, m) => l;
right = <K>(l, r, m) => r;
mod = <K>(l, r, m) => m;

i
i(0, 1, ?)


Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool) & <P>(t : P(true), f : P(false)) -> P(b);
true = <P>(t, f) => t;
false = <P>(t, f) => f;

(b : Bool) =>
  (b : <P>(t : P(true), f : P(false)) -> P(b))
id = <A>(x : A) => x;

id = (A : (b : Bool) -> Type, x : A) => x;

id(String, 1)

b : Bool;

transport : <A, x : A, y, P>(H : x == y, v : P(x)) -> P(y);

A : Type;
x : A;
y : A;

B : Type;
f : (x : A) -> B;

Eq = <A>(x : A, y) => <P>(v : P(x)) -> P(y);
transport = <A, x, y, P>(H : Eq<A>(x, y), v : P(x)) => H<P>(v);


(H : x == y) : f(x) == f(y) => refl;

(H : x == y) : f(x) == f(y) =>
  transport<A, x, y, z => f(x) == f(z)>(H, refl);


ind : <P>(t : P(true), f : P(false)) -> P(b) = b;

b : (b : Bool) & <P>(t : P(true), f : P(false)) -> P(b);

b : _A | <P>(t : P(true), f : P(false)) -> P(b);

_A | <P>(t : P(true), f : P(false)) -> P(b) | Bool

b : _A | (<P>(t : P(true), f : P(false)) -> P(b) & Bool);

_A


Pure -> Impure by Monad
Impure -> Pure by Monad // simpler

Consistent -> Inconsistent by Monad // simpler
Inconsistent -> Consistent by Monad


Interval : Type;
left : Interval;
right : Interval;
mod : left == right;

Interval = A =>
  <K>(
    l : K,
    r : K,
    m : l == r,
  ) -> K;
left = <K>(l, r, m) => l;
right = <K>(l, r, m) => r;
mod = <K>(l, r, m) => m;



Interval : (A : Type) -> Type;
left : (A : Type) -> Interval(A);
right : (A : Type) -> Interval(A);
mod : (A : Type) -> left(A) == right(A);

Interval = A =>
  <K>(
    t : K,
    f : K,
    m : t == f,
  ) -> K;
left = A => <K>(t, f, m) => t;
right = A => <P>(t, f, m) => f;
mod = A => <P>(t, f, m) => m;


Box : Type;
box : Box;
self : (b : Box) -> Box;
mod : (b : Box) -> b == self(b);

Box = <K>(
  k : (b : Box) -> K,
  m : (b : Box) -> b<K>(k,)
) -> K;
box = <K>(k, m) => k(box);
// b<K>(k, m) == k(b)
// b<K>(k, m) == self(b)<K>(k, m)

self = (b) => <K>(k, m) : b<K>(k, m) == self(b)<K>(k, m) =>
  m(b)


Unit : Type;
unit : Type;
elim : <K>(u : Unit, v : K) -> K;

mod : (u : Unit) -> u == unit;

Unit = <K>(
  v : K,
  m : k == elim<K>(u, v),
) -> K;
unit = <K>(v, m) => v;
elim = <K>(u, v : K) : K => u<K>(v)

Bool : Type;
true : Bool;
false : Bool;

Bool = <K>(
  k : (x : Bool) -> K,
  m : <P>(b : Bool, t : P(true), f : P(false)) -> P(b)
) -> K;

(x : A) &

Bool = (b : Bool) | <P>(b : Bool, t : P(true), f : P(false)) -> P(b);

Unit : Type;
unit : Unit;
self : (u : Unit) -> Unit;

Unit = <P>(
  v : P(unit),
  u : Unit,

) -> P(u);
unit = <P>(v, u) => (unit, v);

ind = u => u(u)
self = (u) => fst(u<_>(_, u, m))

unit : Unit;

self : (u : Unit) -> Unit;
mod : (f : False) -> f == self(f);

Unit = <K>(
  s : Unit,
  m : s == self(s),
) -> P(s);
unit = <P>(s, u, m) =>

self = f => f<False>();
mod = f => <K>(s, m) : f<K>(s, m) == self(f)<K>(s, m) =>
  transport(m, refl);

ind = <K>(f : False) => f<K>(f, refl);

Bool : Type;
true : Bool;
false : Bool;
ind : <K>(b : Bool, t : K, f : K) -> K;

Bool = <P>(
  t : P(true),
  f : P(false),
  b : Bool,
  m : b == case<Bool>(b, true, false);
) -> P(b);
true = <P>(t, f, b, m) =>


False : Type;
self : (f : False) -> False;
mod : (f : False) -> f == self(f);

False = (
  s : False,
  i : <K>(m : s == self(s)) -> K
);
self = f => fst(f);
mod = f => (
  s = mod(f),
  i = <K>(m) => (_ : transport(s, snd(f))<K>(m) == snd(self(f))<K>(m));
);


Bool : Type;
true : Bool;
false : Bool;

self : (b : Bool) -> Bool;
mod : (b : Bool) -> b == self(b);

Bool = (
  s : Bool,
  i : <P>(t : P(true), f : P(false)) -> P(s),
);

b : Bool;
H : fst(b) == true;
H : fst(b) == fst(true);

i : <P>(t : P(true), f : P(false)) -> P(true);

i : <P>(t : P(true)) -> P(true);

Unit : Type;
unit : Unit;

Unit = (
  u : Unit,
  i : <P>(t : P(unit)) -> P(u);
)

(b : )

(u, p) : Unit;

(u, p) == (unit, refl);

<i>(
  p<s => s == unit>(refl)(i),
  <j>_
);
<i>
i : unit == unit;

p : i == <P>(t, f) => t;

h :



p = <P>(t, f) : i<P>(t, f) == t =>
  <i> => (_ : P(true));

p = <P>(t, f) : i<P>(t, f) == t =>
  <i> => (t : P(true));

n : Nat;
m : Nat;

p : n == m;
p = <i> => n
self = f => fst(f);
mod = f => (
  s = mod(f),
  i = <P>(m, t, f) =>
    (_ : transport(s, snd(f))<P>(m) == snd(self(f))<P>(m));
);

p : (x : A, y : B(x));
q : (x : A, y : B(x));

p == q ≡ (
  x : fst(p) == fst(q),
  y : transport<B>(x, snd(p)) == (snd(q) : B(fst(q)))
);

False : Type;
self : (f : False) -> False;
case : <K>(f : False) -> K;

False = (
  s : False,
  i : <K>(m : s == self(f)) -> K;
);
self = f => fst(f);
case = f => _;

mod

Unit : Type;
unit : Unit;
ind : <P>(u : Unit, v : P(unit)) -> P(u);


snd(u)<P>(u, _) : P(fst(u))

fst(u) == unit;
fst(u) == fst(unit);

Exists : <A>(B : (x : A) -> Type) -> Type;
exists : <A, B>(x : A, y : B(x)) -> Exists<A>(B);
fst : <A, B>(s : Exists<A>(B)) -> A;
snd : <A, B>(s : Exists<A>(B)) -> B(fst(s));

??
mod : <B>(l : Exists<A>(B), r, H : snd(l) == snd(r)) -> l == r;

Exists = <A>(B) =>
  <K>(
    k : (x : A, y : B(x)) -> K,
  ) -> K;

False : Type;
self : (f : False) -> False;
case : <K>(f : False) -> K;

False = (
  s : False,
  i : <K>(m : s == self(f)) -> K;
);
self = f => fst(f);

Unit : Type;
unit : Unit;

Unit = (
  s : Unit,
  c : <P>(v : P(unit)) -> P(s),
);
unit = (unit, <P>(v) => v);


u : Unit;
c : <P>(v : P(unit)) -> P(u);

p : (u, c) == (unit, <P>(t, f) => t)
p = i => (
  c<u => u == unit>(i),
  <j> c<u => u == unit>(i)
)

p : c == <P>(v) => v;
p = <P>(v) : c<P>(v) == v =>
  <i> c<u => u == unit>(refl) : u == unit

p : c) == (unit, <P>(v) => v)
p = <i>(true,
  <j> c<b == true || b == false>
  )

Bool = (
  s : Bool,
  c : <P>(t : P(true), f : P(false)) -> P(s),
);

s = true;
c : <P>(t : P(true), f : P(false)) -> P(true);

c : <P>(t : P(true), f : P(false)) -> P(true);

p : (s, c) == (true, <P>(t, f) => t)
p = <i>(
  true,
  <P>(t, f) : c<P>(t, f) == =>
)

False : Type;
self : (f : False) -> False;

False = (
  s : False,
  m : s == self(s);
  i : <K>(m : s == self(f)) -> K;
);
self = f => fst(f);


T = (x : A) & B(x);

b : Bool & Bool;


transport<T => Bool & T>(H, _)


Inter : (A : Type, B : (x : A) -> Type) -> Type;
Inter = (A, B) => (
  x : A,
  y : B,
  m : (H : A == B) -> transport(H, x) == y,
);

not : Bool == Bool;
b_true : Inter(Bool, Bool)
  = (x = true, y = true, m = H => refl);

T = (
  x = true,
  y = false,
  m = H =>
    transport(not, (refl : transport(H : Bool == Bool, true) == false)),
);

T.x == transport(T.m(refl), T.y)

f : (x : A) -> B;
g : (y : B) -> B;

p : (H : A == B) ->
  (x : A) -> f(x) == g(transport(H, x));

transport(H, )

snd(T) == false

Inter(Bool, Bool)

Inter = (A, B) => (
  x : A,
  y : B,
  m : (H : A == B) -> x == transport(H, y),
);

Inter = (A, B) => (
  T : Type,
  x : T,
  l : (x : T) -> A,
  r : (x : T) -> B,
  m : (H : A == B) -> x == transport(H, y),
);

((M : A) :> T) == ((N : A) :> T)
M == N

Inter((x : Int) -> Int, (x : String) -> String);

Unit : Type;
unit : Unit;

Unit = (u : Unit) & <P>(v : P(unit)) -> P(u);
unit = <P>(v) => v;

u : <P>(v : P(unit)) -> P(u) == Unit
u : <P>(v : P(unit)) -> P(u) == (u : Unit) & <P>(v : P(unit)) -> P(u)
u : <P>(v : P(unit)) -> P(u) == <P>(v : P(unit)) -> P(u)


t : (x : A) & B(x);

t == u ≡ (
  x : t.0 == u.0,
  y : transport<B>(x, t.1) == u.1;
);

u.0 == unit


Inter = (A, B) => (
  x : A,
  y : B(x),
  m : (H : A == B(x)) -> x == transport(H, y),
);

Self : (A : Type, B : (x : A) -> Type) -> Type;
Self = (A, B) => (
  x : A;
  y : B(x);
  h : A == Self(A, B);
)


(H, b_true)

A & B
transport<T => Inter(Bool, T)>(H, b_true)

f = ([A], x : A) => x


A & B
(x : A) & B(x)

f : (x : A) | B(x)

id : (A : Type) | A -> A
  = x => x;

Type :> Nat -> Nat
(id : (A : Type) | A -> A) :> Type | Nat -> Nat

(M : (A : Type) | (Nat & A) :> Nat)
M :

(x : Nat & x <= 5) => _

(x : A) => _

M : {
  T #: Type;
  of : (x : Nat) -> T;
  to : (x : T) -> Nat;
} = {
  T #= Nat;
  of = x => x;
  to = x => x;
};

m : M.to(M.of(1)) == 1 = refl;

Monad : (M : (A : Type) -> Type) -> {

};

(Monad M) => (x : M(A)) => _;

Γ |- A : Canonical
------------------
Γ |- M == N : A

rel : (x : A, y) -> x == y;


User = {
  id : Nat;
  name : String;
};


f : <A>(x : A & { name : String; }) -> A
  = (x : { name : String; }) => x;

f = [x, y] => x + y;

user.show : Show(User)


User #= {
  id : Nat;
  name : String;
};
t : User = {
  id = 0;
  name = "A";
}

Bool = {
  T : Type
}

Canonical = {};

Unit = u & <P>(v : P(unit)) -> P(u);

Unit = @u : Unit; <P>(v : P(unit)) -> P(u);

Unit = u : Unit = @; <P>(v : P(unit)) -> P(u);

A -> B |-> (x : A) -> B(x)
A * B |-> (x : A) * B(x)
A & B |-> (x : A) & B(x)
A | B |-> (x : A) | B(x)


x >= 5 <|
| true =>
  p : x >= 5;

  (x >= 5 == true -> x >= 5 : Prop)

T =
  | "a"
  | "b";

M : T;

M == "a"


Γ |- (M : A) :> C  Γ |- (M : B) :> C
------------------------------------
Γ |- (M : A | B) :> C


Γ |- p : ⊥
-----------------
Γ |- (M : A) :> B

Γ |- p : (M : A) :> B
---------------------
Γ |- (M : A) :> B


<P>(v) => v;
A == B ≡ (
  f : (x : A) -> B,
  g : (y : B) -> A,
  p : (x : A) -> f(x) == g;
);



union : (L : (M : A) :> C, R : (M : A) :> C) -> (M : A | B) :> C;

id :
  & (x : Nat) -> Nat
  & (x : String) -> String;

(x == M, ...L) | (x == N, ...R)

(x : A, ...L) | (x : A, ...R)

(x : A, ...(x <| M => L | N => R))


(x : Nat, p : x >= 5) => _;

(x : Nat & x >= 5) => _;

Pattern (P) ::=
  | P &

add = (
  x : Nat;
  y : Nat;
  x + y;
);

id = (A : Type, x : A) => x;
id = <A : Type>(x : A) => x;
id = [A : Type](x : A) => x;

(x : Nat, [x >= 5]) => _;

add : (x : Nat; y : Nat; Nat);


(x = 1; y = 2; ...add) : Nat
// sugar
add(1, 2) : Nat


Either = (A, B) =>
  | (left : )

(x : Nat & x >= 5) => _;

Bool & Bool |->

union : (L : (M : A) :> C, R : (M : A) :> C) -> (M : A | B) :> C;

(<A>(x : A) => x :> (x : Int) -> Int) == (x : Int) => x

(x : Int) => x == (x : String) => x;

(x : A) | B(x)


(x : A, y : B)

(A : Type, y : (x : A) -> A)


T = (x : Nat) & String;

Unit : Type;
unit : Unit;

Unit = (u : Unit) & <P>(v : P(unit)) -> P(u);
unit = <P>(v) => v;


H : Bool == Bool = ua(not);
transport<T => T>(H, true) == false
transport<T => Bool>(H, true) == true


Nat\+1
((x : Nat) -> Nat).level = 1

((A : Type) -> (A\-1)).level = 0


g = x => k(_A, y => _A);
h = x => k(\1, y => \2);

_A := x => \1;

g = x => k(_A \1, y => _A \2);
h = x => k(\1, y => \2);


g x == h x
k(_A x, y => _A x) == k(x, y => x)
_A x == x
x => x

x => _A == x => \-1


f = x => (
  g = y => x + y;
  h = z => g(z) + 1;
  g(1) + h(2);
);

g = (x, y) => x + y;
h = (x, z) => g(x, z) + 1;
f = x => g(x, 1) + h(x, 2);

f = (a, b) => (
  g = y => a + b + y;
  h = z => g(z) + 1;
  g(1) + h(2);
);

g = (_P, y) => a + b + y;
h = (_P, z) => g(_P, z) + 1;
f = (a, b) => (g(_P, 1) + h(_P, 2));

P = (a, b);
g = (...P, y) => a + b + y;
h = (...P, z) => g(...P, z) + 1;
f = (a, b) => (g(...P, 1) + h(...P, 2));

g = (a, b, y) => a + b + y;
h = (a, b, z) => g(a, b, z) + 1;
f = (a, b) => (g(a, b, 1) + h(a, b, 2));

l : List(Nat);
l = 1 :: 2 :: l;





x = 1




b = _;
c = _;
z = 3;
a = _;
// cache l1
x = 3;
y = 3;
3 + 3

[(10, 1), (20, 2), null]
[10, 2, 20, 4, null]

[(10, 1), (20, 2), null]

x0 = (10, x2);
x2 = (20, x4);
x4 = null;


x = 1;
f = (x) => x + 1;
p = (1, 2);
t = [1, 2];

Tag = ;

(k : (A : Tag) -> type(A)) =>
  k(Show)()

_A x == _B;

(x : A, ...R)
(...R, x : A)

Γ |- _ : Size(R)
----------------
Γ |- M.x : A

f = (x) =>
  x = 1;
  y = 2;
  z = x + 2;
  z + 3;

main = () =>
  () = print("a");
  x = read("file");
  () = print(x);
  ();

x = 1;
x + 2
((x = 1) (x = 2))

((x = 1) ; (x + 1))

omega = () => omega();
drop = (x, y) => y;

x = drop(1, omega());

f = <P>(f : P(x)) => _;


(x : A)


s : _;
s = x => M;

(u : Unit) & <P>(v : P(unit)) -> P(u)

k = (<P>(v) => v : (u : Unit) & <P>(v : P(unit)) -> P(u))

unit

fst : <A, B, s>(p : (x : A $ 1, y : B(x) $ s)) ->
  (p : (x : A $ 0, y : B(x) $ s), x : A $ 1);


(p : (x : A $ 1, y : B $ 1)) => (
  fst()
)


f = l | x => k(_A\l+1, y => _A\l+1);

f = x => k(_A\l+1, y => _B\l+2);

f = x => k(_A, y => _A);
x == _A[]

g = x => _A;
h = x => _B;

g = sink 0;
h = sink 1;

f = x => (
  k(g(x), y => g(x))
);



g = x => k(_A, y => _A\l+1);

_A\l+1 := _C\l[↑]
_B\l := _C\l
f = x => k(_A \1, y => _A \2);
g = x => k(_A \1, y => _A \2);
h = x => k(\1, y => \2);


_A \1 == _B[\1]

_A := λ. _B[\1]

_B[\1]

_A = p => x => _A (x, p);
_A p = x => _A (x, p)
_A p 1 = x => _A (x, (1, p))

_A (x, (1, p))

_A 1 = y => (x => y => _A x y) 1 y


g = f => f _A;
g (x => x 1);

_A[\1] == \1

_A := x => x
_A \1 == \1

x 1

sink = l => p => x => sink l (x, p);

f = sink 0;
g = sink 1;

x => (
  y = _;
  _;
);

_A\l
_B\l+1
_A := x => _B

(+) : (x : Nat, y : Nat) -> Nat;

incr = x => 1 + x;


x = (M : (x : A) & B(x)) : B(x)

x = (M : (x : A) & B(x)) : B(x)

x : B(x) |-> x : (x : A) & B(x)
(M : (x : A) & B(x)) : (x : A) &B(x)

Unit : Type;
Unit = (
  T = Unit;
  (x : T) & Nat
);

Unit

sum : (l : List(Nat)) -> Nat;
sum = (l) =>
  l <|
  | [] => []
  | [hd, ...tl] => hd + sum(tl)
  | [hd0, hd1, hd2, hd3, ...tl] =>
    hd0 + hd1 + hd2 + hd3 + sum(tl);

(x : (y : A) -> B) => _


f : (x : Nat) -> Nat;

f("a")
f(("a" : String) :> Nat)

(f : (x : String) -> Nat)
(f : (x : Nat) -> Nat) : (x : String) -> Nat



(M : (x : Nat) -> Nat) : (x : String) -> Nat
```

## This time is for real

```rust
f : (x : Nat) -> Nat;


f("a")
f(("a" : String) :> Nat)

(M : (x : _A) -> _A)
T = (x : A) -> (y : )


Unit : Type;
Unit = ()

t = (x, y, z) => ();

Γ, x == M |- M : T
-------------------
Γ |- %Y(x). M : T


lv = let var

M N |-> M; N; \lv(N) \0


map : <A, B>(
  l : List(A),
  f : (x : A) -> B,
  stack : Stack(length(l)),
) -> List(B);
map = <A, B>(l, f) =>
  l <|
  | [] => []
  | [hd, ...tl] =>
    map();

List : (A : Type) -> Type;
null : <A> -> List(A);
cons : <A>(el : A, tl : List(A)) -> List(A);
length : <A>(l : List(A)) -> Size;

List = A => (l : List(A)) &
  <P>(
    n : () -> P(null),
    c : (el : A, tl : List(A)) -> P(cons(el, tl)),
  ) -> P(l);

null = <A> => <P>(n, c) => n();
cons = <A>(el, tl) => <P>(n, c) => c(el, tl);


List : (A : Type) -> Type;
null : <A> -> List(A);
cons : <A>(el : A, tl : List(A)) -> List(A);
length : <A>(l : List(A)) -> Size;

List = A => (l : List(A)) &
  <P, n_s, c_s>(
    n : () -(n_s)> P(null),
    c : (el : A, tl : List(A)) -(c_s)> P(cons(el, tl)),
  ) -(max(n_s, c_s))> P(l);

null = <A> => <P>(n, c) => n();
cons = <A>(el, tl) => <P>(n, c) => c(el, tl);



map : <A, B>(l : List(A), f : (x : A) -> B) -> List(B);
map = <A, B>(l, f) =>
  l <|
  | [] => []
  | [hd, ...tl] => map();

map : <A, B>(
  k : Write(List(B)),
  l : List(A), f : (x : A) -> B
) -> ();
map = <A, B, k>(dst, l, f) =>
  l <|
  | [] => write(dst)
  | [hd, ...tl] => map()


f = (stack : , ) => _


Code :

eval : (e : Env, t : Term) -> Value;
reify : (v : Value) -> Code;



reify : (v : Value) -> Term;



Term :=
  | Type
  | x
  | x => M
  | M N
  | x : M; N
  | x = M; N;

Nat = (n : Nat) & <P>(z : P(zero), s : (p : Nat) -> P(succ(p))) -> P(n);

ind = <P>(n, z, s) =>
  n<P>(z, p => s(p, ind<P>(p, z, s)));

ind : <P : (b : Bool) -> Type>(b : Bool, t : P(true), f : P(false)) -> P(b);

C_Bool = <K>(t : K, f : K) -> K;

(b : C_Bool, w : b == true || b == false);

Bool : Type;
Bool = (
  true : Bool = <P>(t, f) => t;
  false : Bool = <P>(t, f) => f;
  (b : Bool) & <P>(t : P(true), f : P(false)) -> P(b)
);

T_Unit : Type;
T_unit : (Unit : T_Unit) -> Type;

T_Unit = (Unit : T_Unit) & (unit : T_unit(Unit)) -> Type;
T_unit =

T : (A : (A : Type) -> Type, B : (x : A) -> Type) -> Type;
unfold : T(A, B) == A(T(A, B));


Bool : Type;
Bool = (
  true : Bool = <P>(t, f) => t;
  false : Bool = <P>(t, f) => f;
  (b : Bool) & <P>(t : P(true), f : P(false)) -> P(b)
);

true = <P>(t, f) => t;
false = <P>(t, f) => f;

Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool) & <P>(t : P(true), f : P(false)) -> P(b);
true = <P>(t, f) => t;
false = <P>(t, f) => f;

ind = <P>(b, t, f) => b<P>(t, f);
case = <K>(b, t, f) => b<_ => K>(t, f);


T : Type;
T = (x : Nat) -> <P>(y : T) -> P(y(1));

Interval : Type;
left : Interval;
right : Interval;

Interval = (i : Interval) & <P>(
  l : P(left),
  r : P(right),
  H : P(left) == P(right),
  m : transport(H, l) == r;
) -> P(i);
left = <P>(l, r, H, m) => l;
right = <P>(l, r, H, m) => r;

(A == B) ≡ (
  f : (x : A) -> B;
  g : (y : B) -> A;
  h : (x : A) -> g(f(x)) === id(x);
);

H : A == A ;

transport<P>(H, t) == t;

T = () -> T;
U = () -> U;

f : (x : T) -> U;
f = (x) => () => f(x());

g : (y : U) -> T;
g = (y) => () => g(y());

H : T == U;
H = (f, g, h);

(refl : T == U)




mod : left == right;
mod =
i : Interval;

i<_ => Bool>(true, true, refl, refl)

i<_ => Bool>(true, false, ua(not), refl);

i<_ => Nat>(0, 1, H => _)

Bool : Type;
true : Bool;
false : Bool;

Bool = (b : Bool, i : <P>(t : P(true), f : P(false)) -> P(b));
true = (true, <P>(t, f) => t);
false = (false, <P>(t, f) => f);

Self : (A : Type, B : (x : A) -> Type) -> Type;
self : <A, B>(s : Self(A, B)) -> A;
mod : <A, B>(s : Self(A, B)) -> s == self(s);

Self = T => (x : A, i : B(x), H : A == Self(A));
self : <A, B>(s : Self(A, B)) => transport(s.H, s.x);

b == fst(b)

T : Type;
T = T_Type @ (x : T_Type) -> Type;

x => y => M

f = y => M;
x => f;



symm : <A, x : A, y : A>(H : x =<= y) -> x =>= y;


Data : Type;
Measure : Type;

beta : <A, B, M : (x : A) -> B, N>((x => M(x))(N)) == (x = N; M(x));

Cost : <A : Data>(x : A) -> Measure;
size : <A, x : A>(h : Cost(x)) -> Size;

c : Cost((x => x)(1));
c_let : Cost(x = 1; x)
  = transport(beta, c);

size(c) == 1 + n;
size(c_let) == n;


x == id(x)


f(...args);

f[0](f[1], ...args)

f[0](f + 1, ...args)

f = (x) => (
  g = y => 1 + x;
  h = y => 2 + x;
  (g, h)
);

Fun_ptr = 8;
Closure = {
  A : Type;
  B : (x : A) -> Type;
  C : Type;
  f : (c : C, x : A) -> B(x);
  ...(c : C);
};

(
  tag : Bool,
  payload :
    tag <|
    | true => _
)

Term (M, N) ::=
  | x
  | x => M
  | M(N)
  | x = N; M
Type (A, B) ::=
  | Type
  | Data
  | (x : A) -> B
  | (x : A, y : B);



(x : A -> y : B -> C)

size<_ : Type>(M) |-> 0
size<((x : A) -> B(x)) : Data>(M) |-> size(fun_ptr)
size<(x : A, y : B(x)) : Data>(M) |-> size(fst(M)) + size(snd(M))


A : Type;
(p : (x : A, y : B(x))) => (
  size(p)
)

f = (x, y) => (
  _
);

size()
(x : A ) ->

String_body : Data;
String_body = (hd : Char, ...(tl : String_body));

String : Data;

Array : (s : Size, A : Type) -> Type;
Array = (s, A) => (
  hd : A,
  tl :
    s <|
    | 0 => ()
    | 1 + s => Array(s, A);
);

snd(a : Array(s, A)) :

String : Type;
size : (s : String) -> Size_of(s);

String = (
  s : Size,
  a : Array(s, A),
);

f = (s : String) => (
  (
    1; size(1),
    s; size(s)
  )
)



f_code = (p : (x : Int, y : Int)) => p.x + p.y;
f = (p : Box(x : Int, y : Int)) => f_code(*p);


f_code((1, 2));

f(box(1, 2));


Memory = {
  Safe : (A : Type, src : A, off : Offset, B : Type) -> Type;
  get : <A, B>(src : A, off : Offset, w : Safe(src, off)) -> B;
};

String = (d : String) & {
  s : Size(snd(d));
  ...
}


Data : Type;

Size : <A : Data>(x : A) -> Data;
Size_of_size : <A>(x : A, s : Size(x)) -> Size(s);

Size(s : Size<A>(M)) ≡ Size_of_size(M, s);

Size(M : (x : A, y : B(x))) ≡ (x : Size(fst(M)), y : Size(snd(M)));

Box : (A : Data) -> Data;
box : <A>(x : A, s : Size(x)) -> Box(A);
unbox : <A>(x : Box(A), s : ?) -> A;
beta : <A>(x : A, s : Size(x)) -> x == unbox(box(x, s), s)



(p : (x : A, y : B(x))) => box(p)

Either = (
  tag : Bool,
  payload :
    tag <|
    | true => Nat32,
    | false => Bool,
);

x : Either;
(
  tag = Size_of_Bool(fst(x)),
  payload : Size(
    fst(tag) <|
    | true => Nat32,
    | false => Bool
  ) =
    fst(tag) <|
    | true => Size_of_Nat32(snd(x))
    | false => Size_of_Bool(snd(x))
) : Size(x)


// works because now you can compute


f = (A : Type $ 0, A_s : (x : A $ 0) -> Size, x : A) => x;


f = (A : Type, A_s : (x : A $ 0) -> Size, x : A) => x;


f = (A : Type, B : Type, p : (x : A, y : B)) => snd(p);


Nat32_or_bool =
  | When_true (payload : Nat32)
  | When_false (payload : Bool);

Either = (A, B) => (
  tag : Bool,
  payload :
    tag <|
    | true => A,
    | false => B
);

Nat32_or_bool = (
  tag : Bool,
  payload :
    tag <|
    | true => Nat32,
    | false => Bool,
);

x : Nat32_or_bool = (tag = false, payload = true);
s : Size(x) = 16;

box : <A>(x : A) -> Box(A);
box : <A>(x : A, s : Size(x)) -> Box(A);


Both = (
  tag_0 : Bool,
  tag_1 : Bool,
  x :
    tag_0 <|
    | true => Nat32,
    | false => Bool,
  y : (
    tag_1 <|
    | true => Nat32,
    | false => Bool,
  ),
);

x : Nat32_or_bool;

16 + tag * 32

T : Type;
T = () ->

Unit : Type;
unit : Unit;

Unit = (u : Unit) & <P>(v : P(unit)) -> P(u);
unit = <P>(v) => v;


x : Unit;
H : x == unit;
x = <P>(v : P(unit)) => transport(H, v);
H =

unit = [<P>(v) => v];


f : (x : Unit) -> ();

f(unit);
f(<P>(v : P(unit)) => v);
f(
  u : Unit;
  u = <P>(v : P(unit)) => (v : P(u))
  u
);

Unit : Type;
unit : Unit;

Unit = <P>(v : P(unit)) -> P(u);
T : Type;
T = () -> (x : A) & ;

(x : A, ...)


p : (x : Nat, y : Nat);
p = (1, 2);

[x, y] : [x : Nat, y : Nat];
x = 1;
y = 2;

f : (x : Int, y : Int) -> T;

f : (x : Int; y : Int) -> Int;
f : (p : ...(x : Int, y : Int)) -> Int

g : (p : (x : Int, y : Int)) -> Int;
(f : (p : (x : Int, y : Int)) -> Int)


f = (x) => (y) => _;


f = (p : (Int, Int)) => p;

f = (x : Int, y : Int) => x + y;
f = (p : (x : Int, y : Int)) => (
  (x, y) = p;
  x + y
);

p = (1, 2);
f p
f 1 2

// fails
(r) => (
  { x; y } = r;
  f({ x; y })
) : (r : { x : Int; y : Int; }) -> (a : Int, b : Int)

(p : (Int * Int))
(Int, Int) -> _;


f = r => (r, r);
f = ({ x; y }) => (
  r = { x; y };
  (r, r);
)

r = { x = 1; y = 2; };
f({ x = r.x; y = r.y; })
p : f.param = (1, 2);
t = f(p)

f = (x : Int) => (y : Int) => fst p + snd p;
f = (x) => (y) => _
f = (x, y) => _;
p = (1, 2);
x = f(p)

f = [x : Int, y : Int] => fst p + snd p;
f [1, 2]
f = (p : [x : Int, y : Int]) => fst p + snd p;
f([1, 2]) != f [1, 2]

Type : Erasable(Type);
Data : Type;

Erasable : (A : Univ) -> Univ;
Unboxed : (A : Univ) -> Univ;
Linear : (A : Univ) -> Univ;
Unboxed(Linear(A))
Linear(Unboxed(A))
()

(A : Type) => (x : A) => _;

U : Unboxed(Type) = ...A;
T = (
  x : U,
  y : B,
);
t : T;

(x, y, z) = (_ : (x : A, _ : ...(y : B, z : C)));




Size_of_data : <A : Data>(x : A) -> Size(x);

x : Nat

Γ, x : A |- r : Size(x)
-----------------------
Γ, x : A |- x : A



y = x;

Γ |- M : A  Γ |- M_S : Size(M)
Γ, x == N |- M : B  Γ, x == N |- N_S : Size(N)
--------------------------------------------
Γ |- (x = M, y = N) : (x : A, y : B)

Γ, x : A |- r : Size(x)
-----------------------
Γ, x : A |- x : A


Γ, x : A |- r : Size(x)  Γ |- Σ
-------------------------------
Γ, x : A |- Σ




// should fail, how do you drop y without knowing it's size?

(x : Nat32_or_bool) => (
  (x, x)
)


Array : (l : Length, A : Type) -> Type;
Array = (l : Length, A : Type) =>
  l <|
  | 0 => ()
  | 1 + n => (hd : A, tl : Array(l, A));

f = <A, B>(x : A, y : B) => x;

T_var : Nat8 = 0;
T_let : Nat8 = 1;
T_lambda : Nat8 = 2;
T_apply : Nat8 = 3;

Index = Nat32;




code = (T_lambda, (T_let, (T_var, 0), (T_apply, 1, 0)));

Term : Type;
Term =
  | (tag == T_var, var : Index)
  | (tag == T_let, arg : Term, body : Term);

size : (term : Term) -> Size(term);
size = term =>
  term <|
  | (T_var, var) => Nat8 + Index
  | (T_let, arg, body) =>
    s_arg = size(arg);
    Nat8 + s_arg + size(body);

copy : (term : Term) -> (s : Size(term), copy : Term, c_s : Size(copy));
copy = (term) =>
  term <|
  | (T_var, var) => (Nat8 + Index, (T_var, var), Nat8 + Index)
  | (T_let, arg, body) =>
    (s_arg, c_arg, s_c_arg) = copy(arg);
    (s_body, c_body, s_c_body) = copy(body);
    (T_let, c_arg, _body);


C_String : Type;
C_String = (
  hd : Char,
  tl :
    hd <|
    | '\0' => ()
    | _ => C_String
);

strlen : (str : C_String) -> Nat32;
strlen = (hd, tl) =>
  hd <|
  | '\0' => 0
  | _ => 1 + strlen(tl);

f = (str : C_String, len : Size(str)) => _

main : () -> Nat8 % IO;

x => (y = x; x(y));

Γ |- M : A % E
--------------
Γ |- %M :

(M; N); K
M; N; f↑ : 1; K

x = (y = M; N); K

←; y = M; x = N; K

x0 = (y1 = M; N); z2 = K; A

y1 = M; x0 = N; z2 = K; A

f =


tag : Nat8;
index : Nat8;

ptr : (tag == T_var, var : Index)

ptr : Term;
ptr.body


Term : Type;
Term = (t : Term) &
  | (tag == T_var, var : Index)
  | (tag == T_let, arg : Term, body : Term)
  | (tag == T_apply, arg : Term, body : Term)
  | (tag == T_lambda, s : Size(t), body : Term)
  | (tag == T_link, to_ : Box(Term));


Value : Type;
Value =
  | (tag == V_lambda, body : Box(Term));

ind : <P>(
  term : Term,
  v : (var : Index) -> P((T_var, var)),
  l : (
    arg : Term,
    H_arg : P(arg),
    body : Term,
    H_body : P(body)
  ) -> P((T_let, arg, body))
) -> P(term) = _;

#meta (term => (v, l) => ind(term, v, l) : (
  v : (var : Index) -> P((T_var, var)),
  l : (
    arg : Term,
    H_arg : P(arg),
    body : Term,
    H_body : P(body)
  ) -> P((T_let, arg, body))
) -> Term);

ptr.tag == ptr[0]

ptr.arg.tag
ptr[1][0]

(ptr + 1)[0]

code = (T_lambda, T_let, T_var, 0, T_apply, 1, 0);

SECD : Type;
SECD = (l : SECD) &
  | (instr == I_VAR, var : Index)
  | (arg : SECD, open == I_LET, body : SECD, close == I_ENDLET)
  | (instr == I_LAMBDA, s : Size(l), body : SECD, close == I_RET)
  | (funct : SECD, arg : SECD, op == I_APPLY);

Term = (code : SECD, halt : I_HALT);

Value : Type;
Head : Type;
Spine : Type;

Value = (v : Value) &
  | (instr == I_LAMBDA, s : Size(v), body : Head, close == I_RET)
  | (instr == I_VAR, var : Index);
Head =
  | (v : Value, next : Spine)
Spine =
  | (instr == I_HALT)
  | (instr == I_LET, body : Head, close == I_ENDLET, next : Spine)
  | (arg : Value, op : I_APPLY, next : Spine);

M :
  | (instr == I_VAR, var : Index)
  | (arg : SECD, open == I_LET, body : SECD, close == I_ENDLET)
  | (instr == I_LAMBDA, s : Size(M), body : SECD, close == I_RET)
  | (funct : SECD, arg : SECD, op == I_APPLY);

(M :
  (instr == I_VAR, var : Index)
  | (instr == I_LAMBDA, s : Size(M), body : SECD, close == I_RET)
  | (arg : SECD, open == I_LET, body : SECD, close == I_ENDLET)
  | (funct : SECD, arg : SECD, op == I_APPLY)
) :> (
  instr : Instr,
  rest : ...instr
    | I_VAR => (var : Index)
    | I_LAMBDA => (s : Size(M), body : SECD, close == I_RET)
)

M : (
  instr : Instr,
  rest : ...instr
    | I_VAR => (var : Index)
    | I_LAMBDA => (s : Size(M), body : SECD, close == I_RET)
);

Term : Type;
Term = (code : SECD; done == I_HALT);

eval : ()

(x : A, y : B, z : C)
(x : A, _ : (y : B, z : C))
(x : A, _ : ...(y : B, z : C))


f : () -> ();
f = () => f();


T : Type;
T = (x : (s : Size)T, y : A);


t : T;
t = (x = t, y = 1);


Bool : Type;
true : Bool;
false : Bool;
aq
Bool = (b : Bool) & <P>(t : P(true), f : P(false)) -> P(b);

H : Bool == Bool;
transport<T => (true : T, false : T) ->
  (b : T) & <P>(t : P(true), f : P(false)) -> P(b)
>(H, true)(true, false) == false

W_Eq : <A>(x : A) -> Type;
refl : <A>(x : A) -> W_Eq<A>(x);

W_Eq = <A>(x) =>
  (s : W_Eq<A>(x)) & <P>(v : P(refl(x), x)) -> P(s, x);
refl = <A>(x) => x;


make : <A>(length : Int & length >= 0, initial : A) -> Array(A);

id = <A>(x : A) => x;
x = id<String>("a");

id = (A : Type, x : A) => x;
x = id(String, "a");

Show = {
  T : Type;
  show : (x : T) -> String;
};

show = (S : Show, x : S.T) => S.show(x);

show({ T = Int; show = Int.to_string; }, 1)

_ : (n : Nat) -> 0 + n == n = n => refl(_);

User = {
  id : Nat;
  name : String;
};

Term : Type;
Term =
  | Var(x : Index)
  | Let(arg : ...Term, body : ...Term);

T = (x : Nat, user : ...User);

f : (x : A, y : B) -> C;
f : (p : ...(x : A, y : B)) -> C;
g : (p : (x : A, y : B)) -> C;

p = (1, 2)
f((1, 2))
g((1, 2))

Type : Type;
Data : Type;


f = (A : Data, x : A) => x

f = (x, k) => (
  l = k(x);
  [x, ...l]
);

const f = () => function* () {}

(x, y) = (p : (x : _A, y : _B));

(P : _T) = M; K
(P : _T) = (M : _T); K

T : (A : Type) -> Type;
T = A => (
  T_A : Type = T(A);
  T_B : (x : T_A) -> <P>() -> P(x);
  (x : T_A) & T_B(x)
);

// first-class singleton
x_m = (z == 2) => 1 + z;

f = (z == 2, x == 1 + z) => (y) => x + 1;
g = (z == 2, x == 1 + z) => (y) => x + 1;


x = (z === 2) => 1 + z;
// reduces to
x = (z === 2) => 3;



f = (x) => (y) => x + 1;
main = () => (
  x = 1 + 2;
  y = f(x, 1);
  ()
);
f : T =



x = (y = M; N); K

y_1 = M; x_0 = N; K
x = (y = M; N); K


f : (u : Unit) -> ();
y : Unit;
H : y == unit;

f = (x, y) => M;
f = (p) => (
  x = fst(p);
  y = snd(p);
)

f();

Term : Type;
Term =
  | (tag == "var", var : Index)
  | (tag == "let", arg : Term, body : Term)
  | (tag == "apply", funct : Term, arg : Term);

match : <P>(
  term : Term,
  !var : (var : Index) -> P("var", var),
  !let : (arg : Term, body : Term) -> P("let", arg, body),
  !apply : (funct : Term, arg : Term) -> P("apply", funct, arg),
) -> P(...term) = _

match(
  term,
  var = (var) => _,
  let = (arg, body) => _,
  apply = (funct, arg) => _,
);

Bool = <A>(t : A, f : A) -> A;
true : Bool = <A>(t, f) => t;
false : Bool = <A>(t, f) => f;

ind : <P>(
  u : Unit,
  v : P(unit)
) -> P(u);

ind<u => u == unit> : (u : Unit, v : unit == unit) -> u == unit

(u : <P> -> (u : Unit) & (v : P(unit<P>)) -> P(u)) ->

// from
Term (M, N) ::=
  | (M : A)
  | x
  | M N
  | x => M
  | (x : A) -> B

// to
Block (B) ::=
  | x : A = C; B
  | (C : A)
Compute (C) ::=
  | M N
  | A
Atom (A) ::=
  | x
  | x => B
  | (x : A) -> B;

[(M : A) : E] ≡ x : A = M; (x : E)
[(M N : E)] ≡
  f : (x : _A) -> _B = M;
  x : _A = N;
  (f(x) : E)
[x => M : (x : A) -> B] ≡ (x => M : (x : A) -> B)
[(x : A) -> B : Type] ≡
  t_A : Type = A;
  ((x : t_A) -> B(x) : Type)

[M N]

Unit : Type;
unit : Unit;

Unit = (u : Unit) & <P>(v : P(unit)) -> P(u);
unit = <P>(v : P(unit)) => v;

f : (u : Unit) -> _;

f()

z = x;
z == y
x == y

M N
x0 = M N
x0 = x1 = M; x2 = N; x1 x2

x1 = M;
x2 = N;
x0 = x1 x2;

#loc x : t = V;

x == y

x = id(1);

f = (loc) => (loc, 1, 2);
g = (loc) => (
  y = f(y_loc);
  (loc, y)
);
x = g(x_loc);


snd(x)

_A N |->
(λ. _X) N |->
_X[N]

(x : _A) -> _B[x]

x = 1; _A
_A[x := 1]


_M[1; 2] x

L<_M> := λ. _K
L<_M> := L<λ. _K>

ind : <P>(u : Unit, v : P(unit)) -> P(u);

ind : (u, _ : _P unit) -> _P u;

_P[u] == u == unit
_P := \0 == unit

L<_T> ==
_T == (x : _A) -> _B

ind(u, refl)

ind<P>(u, refl) : u == unit
M N :


_T x
_T := λ. _TB

_TB[x] :=



l | f = y => _A y
g = _;
_A := \(1 + l)
f

x => _A == y => y

_A[]

_T ...V == (x : _A) -> [_B]<\0>
_T ...V == (x : _A) -> [_B]<\0>
x = _A;

(x, y) => M
p => (
  x = fst(p);
  y = snd(p);
  drop(p);
)

False : Type;
False = (f : False) & <P>() -> P(f);

M N

f : (x : _A) -> _B = M;
x : _A = N;
f(x)

[x => M : _E] ≡
  T_A = _A;
  T_B = x => _B;
  T = (x : T_A) -> T_B(x);
  f : T = x => M;
  f

t = (x : A) -> B

t : Type;
a : Type;
b : (x : a) -> Type;


t = (x : a) -> b(x)
a = A;
b = B;

f : (u : Unit) -> ()


Unit = (x : Unit) & T

T = A => (x : T(A))

Γ, x : A |- B : Type
Γ |- A === (x : A) & B
----------------------
Γ |- (x : A) & B

A = (x : A) & B


x : _B = _A;

x = _;
(f : _, y : _) => _;

T = _A;
(f : T, y : T) -> Nat;

M N ≡
  p = _A;
  b = _B;
  f : (x : p) -> b(x) = M;
  x : p = N;
  f(x)

T = (x : A) -> Type;
B :  = _;
Unit = (x : A) -> B(x)
Unit = (u : Unit) & <P>
f : (x )

(x : A) -> (t : Y) &

f : (x : X, y : Y) -> (a : A, b : B);


T = (x : A) -> B;

0 | f = λ. (_A\1, λ. _A\1)
0 | g = λ. (\0, λ. \0)

f(x) == g(y)
(_A\1, λ. _A\1)[x] == (\0, λ. \0)[y]
_A\1[x] == y

L<_A> == (g ...R)

LX<_A> ==
L<C> == R<D>

l | f = x => _A x;

_A :=
f(1) == 1
main = () => (

)

M == N
_A == N
_A := N


x = (y = M; N); K
y1 = M; x0 = N; K

l | f = x => \(1 + l)
l | g = x => _A\

_A =
g_clos = (y) => (x) => y + x;

f = y =>
  g = g_clos(y);
  g(y);

(M : A) == x : A = M; x

(M N) ==
  f : (x : _A) -> _B = M;
  x : _A = N;
  f(x);


Γ |-

f = x => y => _;
f = (x, y) => _;

(x, y) = p;

// destruct
(x, p) = fst(p);
(y, p) = snd(p);
drop(p);

(x, y) = p;

x = fst(p);

open : (world : World, )


(x = 1, y = 2) : (x : Nat) : Nat

(x = 1, y = ()) : Nat

(x : Nat, _ : ...())

(x : A, y : ...())

x = 1;


M N = {
  f = M;
  x = N;
  f(x)
};

((x, y), (a, b)) = p;

l = fst(p);
r = snd(p);
x = fst(l);
y = snd(l);
a = fst(r);
b = snd(r);


(A : Type) -> (x : A) -> A;

()

(∀, A : Type, B : (→, A = A, B = Type))

(x : A) ->
(x : A) -> B


(/)

IO = {

};

t = f(1, 2)

1   | x = 1;
1.5 | z = 3;
2   | y = 2;

x = M;
x = N;

x\1 == M
x\0 == N

λ. M

x =

(f, x) => f(x)
(k, f, x) => f(k, x);

x : Nat;
y : Nat = 1;

Data : Type;



Array : (s : )

List : (A : Type) -> Type;

List = A =>
  | (tag == "null")
  | (tag == "cons", hd : A, tl : List(A))
  | (tag == "link", list : Box(List(A)));

link = (list) =>
  list <|
  |
  | ("link", _) => list

perform :

Either = (A, B) => (
| (tag == "left", payload : A)
| (tag == "right", payload : B)
);

not =
  | "true" => "false";
  | "false" => "true";


M match (
| ("continue", value) => _
| ("exception", exn) => _
);

M ?
| true => N
| false => K;

M ? (
| ("ok", value) => _
| ("error", error) => _
)

M ? (
| ("continue", value) => _
| ("exception", exn) => _
);


(x, y) =
  z = 1;
  x + y + z

(x, y) =
  z = 1;
  x + y + z

M : A % E

#try M :
  | ("result", x : A)
  | ("effect", x : E, k : Continue(A));



M.(
| ("continue", value) => _
| ("exception", exn) => _
);


M : A % E



M %
M % handler;

try : <A, E>(M : A % E, (eff : E) -> A) -> A;
match : <A, E, K>(
  M : A % E,
  return : (x : A) -> K,
  effects : (eff : E) -> K
) -> K;
%M :

x : () -> ();
y = 1;
x = () -> ()


M.(
|
)
user.is_valid


(:>) : (A : Type, B : (x : A) -> Type) -> Type;
coerce : <A, B>(M : A, H : (x : A) :> B(x))

False : Type;
False = (f : False) :> <P>() -> P(f);

x : A;
y : B;

(x, y) = (1, 2);

(x, y) : (x : A, y : B);
x

x : A;
x : A;

x : A : B

(x, y) : (x : A, y : B);
x = 1;
y = 2;


M = @{
  n = f(1);
  f = (x) => 1 + x;
};




rec {
  x = 1;
  y = 2;
}

Unit = (u : A) & <P>_;

Unit =
  (A == Unit);
  u & <P>_

[(M : A)] = x : A = M; x
[M N] = f = M; x = x
[(M : A) : E] = A == E; [M : A]

P : (f : (x : _A) -> Type) -> Type
_A := P(x => Nat)

app = (f : (x : A) -> B, x) => f(x)
f = x => g(x);

x => 1 : f

y : P(y) = (M : (x) & P(x))

unit : P(unit) = _;

Γ, x : A |- B : Type
Γ |- A ≡ (x : A) & B
-----------------------
Γ |- (x : A) & B : Type

f : (x : A, y : B) -> C;
g : (p : (x : A, y : B)) -> C

f }
```
