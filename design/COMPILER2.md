## Overview

Guarantees?

If you write code like C, it should give you C-like performance.

Sharing during equality? Maybe hypothesis?
Strong CBV seems to make sharing less efficient.

When two values pass strict equality, mutate to point to the other. This is dangerous.

Equality, has two options, either when machine halts or when it fails.

- Tail-call optimization
- Trivial / guaranteed inlining
- Trivial / guaranteed unboxing
- No closures for closed terms
- Dead code elimination

Moving data up or down through lambdas based on the demand.

Reduction strategy is Strong CBV, always left most.

Injectivity tracking

```rust
Erase Sugar

Flatten apply
Lift substitutions
Lift lambdas || Closure conversion

Closure-less call's

Linear inlining

Lazy name solving?

Elaboration as Compilation


```

## Front

```rust
Pattern (P) ::=
  | x
  | (P : A)
Sort (S) ::=
  | Type
Term (M, N)
Type (A, B) ::=
  | (M : A)
  | x
  | P => M
  | (P : A) -> B
  | M N
  | P = N; M

// After sugar is removed
// TODO: forall?
Term (M, N) ::=
  | x
  | x => M
  | M N
  | x = N; M

(M N) K |-> x = M N; x y

Value (V) ::= x

// lift lambda
(λ. M) N |-> [λ. M](\0 [↑]N) // closure-l
V (λ. N) |-> [λ. N]([↑]M \0) // closure-r

// flatten apply
(M N) K |-> [M N](\l [↑]K) // anf-l
V (N K) |-> [N K]([↑]V \l) // anf-r

// lift subst
λ. [λ. N]M |-> [λ. λ. N](λ. [\0 \1][↑ : 1]M) // out-lambda
[K]M N |-> [K](M [↑]N) // out-apply-l
V [K]N |-> [K]([↑]V N) // out-apply-r
[[K]N]C |-> [K][N][↑ : 1]C // out-substs

[[K]N]C |-> [K][N][↑ : 1]C


L<[K]M N> |-> L[K]<M N> // out-apply-l

Context (C<•>) :=
  | •
  | (x : C) -> B
  | (x : V) -> C
  | (x : C) => M
  | (x : V) => C
  | C N
  | V C
  | x = C; V
  | x = V; C
  | (M : C)
  | (C : V)
  ;

Value (V) ::=
  | x ...V
  | (x : A) -> B
  | x => M
x = N; C<x> |->


G |-

(λ. λ. _X) N

f == g
λ. M == _X

f == g
E<λ. M> == _X


Term (M, N) ::=
  | x
  | (x : A) & B
  | (x : A) -> B
  | x => M
  | M N
  | x : A; M
  | x = N; M

// TODO: if the
```

## IR

Heap currently cannot point to the stack

```rust

CCω
// TODO: no lambda in body because that would be code generation, maybe allow that?

Atom (A) :=
  | \+l // memory var
  | \-n // local var
Code (C) :=
  | [A¹ ...A²]C // call
  | [(A¹, ...&Aⁿ)]C // pair
  | [fst A] // fst
  | [snd A] // snd
  | A // return

Pointer (&V) ::=
  | \+l
Value (V) ::=
  | λ. C // lambda
  | (&V¹, &V²) // pair
Heap (H<•>) ::=
  | •
  | [V : &V | ]H
Program ::= H<B>

Type / Data = 0
Prop = 1

Heap (H<•>) ::=
  | •
  | [V : &V | ]H

Tag ::=
  | "code"
  | "forall"
  | "closure";

(x : A) -> B
(x : A) -> (x => B) x
(Π, &A, &B)
(Σ, &A, &B)
(&, &A, &B)

Context (C<•>) ::=
  |

Atom (A) :=
  | x // memory var
  | \-n // local var
Code (C) :=
  | [A¹ ...A²]C // call
  | [(A¹, ...&Aⁿ)]C // pair
  | [fst A] // fst
  | [snd A] // snd
  | A // return


(f x) // aka a closure


(x : A, y : B)


l | [λ. M]C<\l N> |-> [λ. M]C<[N]M>

(∀, &A, C)


H<[V : &Vᵃ]C> |-> H[V : &Vᵃ]<C>// allocate

[λ. [\-0[0]][\-1[1]][\-1 \-0]\-0]
[c c]

(x : A) -> B

(x : A) & B
()
(∀, A, x => B)

Type_tag =
  | Type
  | Prop
  | ∀
  | &
  ;
Type =
  | (tag == Type, ())
  | (tag == Prop, ())
  | (tag == ∀, (A : Type, B : (x : A) -> Type))
  | (tag == &, (A : Type, B : (x : A) -> Type))


Γ |-
----------------
Γ[] |- \-n

x : A

[∀:A. C : Type]
[λ. C : A]
[∀:A. C : Type]

y; x; @; let f; y; x; @; let g; f; g; @

f = x y;
g = x y;
f g

f = x y;
f g


normalize -> equal
more simd friendly
strong normalization is an optimization
two passes required

equal -> expand_head -> equal
single pass

(x, y) = M;

x : A;
y : B;


t = f x;

M : {
  log : (msg : String $ 1) -(Log)> ();
  log : (msg $ 1) -> ();
} = _;

G |- M | N : (x : A) -> B |-> x => (M x | N x)
G |- (x == L, y : B) | (x == R, y : C) |-> (x : A, y : x | L => B | R => C | _ => Never);


// TODO: study monomorphization of Environment
incr : <E, K>(n : Nat, e : E, k : (x : Nat, y : E) -> K) -> K;


f(n, x => x + y)

```

## Constraints

```rust
[(M : A) == E : T] =
  [A == T : Type] &&
  [M == E : A]
[(M : A) : T] =  ∃(_E : T). M == E

[(x : A) -> B == E : T] =
  ∃(_A : Type). ∃(_B : (x : A) -> Type).
  T == Type &&
  E == (x : _A) -> _B x &&
  [A == _A : Type] &&
  ∀(x : A). [B == _B : Type];
[(x : A) -> B : T] = ∃(_E : Type). [(x : A) -> B == _E : T]

[x => M == E : T] = [x => M : T] && x => M == E
[x => M == E : T] =
  ∃(_A : Type). ∃(_B : (x : A) -> Type).
  T == (x : _A) -> _B x &&
  ∀(x : _A). [M : _B x]

[M N == E : T] = [M N : T] && M N == E
[M N : T] =
  ∃(_A : Type). ∃(_B : (x : _A) -> Type).
  [M : (x : _A) -> _B x] &&
  [N : _A] &&
  T == _B N

// TODO: Type in both directions, accumulate holes and then solve them

[(M : A) : T] = [A == T : Type] && [M : A]
[(x : A) -> B : Type] = [A : Type] && ∀(x : A). [B : Type]
[x => M : (x : _A) -> _B x] = ∀(x : _A). [M : _B x]
[M N : T] =
  ∃(_A : Type). ∃(_B : (x : _A) -> Type).
  [M : (x : _A) -> _B x] && [N : _A] && _B N == T


[M == E : T] = [M : T]
[M : A] = [M ⇒ T] &&

P _x -> P _x
P 1 -> P 1

(x : P _x) =>

E ==

M == _x : Int

Γ |- A : Type
Γ, x : A |- B : Type
-----------------
Γ |- x => M :


x : A; x := M; C<x> |-> x : A; x := M; C<M>

Type : Type;

x : +


f = x => N; M |-> g = N; f = x => g; M{f x := g}   x ∉ fv(N)


f = x => g; M |-> M{f x := g};

(x => M) N
x = N;
M

Term (M, N) ::=
  | x
  | x => M
  | M N
  | x = M; N

(x => M) N |-> x = N; M // beta
x = N; M |-> M{x := N} // subst


Block (B) ::=
  | x = C; B
  | C
Code (C) ::=
  | x
  | x => M
  | f x

x = (y = N; M); K |-> y = N; x = M; K // flatten-let

(x => M) N |-> x = N; M // beta
x = N; M |-> M{x := N}  count(x, N) == 1 // linear subst
x = y; M |-> M{x := y} // alias subst
x = N; M |-> M  x ∉ fv(M)  // gc

M N |-> f = M; f N    M != x // lift-apply-l
M N |-> y = N; M y    N != x // lift-apply-r

f = x => x + 1; f N
(x => x + 1) N
x = N; x + 1
N + 1

x = (y = 1; y + 1); 2

(x => M) N |-> f = x => M; f N
f = x => M; f N |-> f = x => M; y = N; f y

(x => M) N |->
  f = x => M; a = N; f a

Term (M, N) ::=
  | x
  | x => M
  | M N
  | x = M; N

Core (M, N) ::=
  | x
  | x => M
  | x = M; N
  | x <- M; N
  | f x

M N |-> f <- M; f M       M != x // cps-lam-l
M N |-> y <- N; M y       N != x // cps-lam-r
x = M; N |-> x <- M; N    M != x // cps-let

x => (y = M; N) |-> y = M; x => N   x ∉ fv(M)


(N; M); K |-> N; M; (↑ : 1); K

(N; M); K |-> N; M; (↑ : 1); K



(x => y = 1 + 1; y + 1);
(y = 1 + 1; x => y + 1)

(x => y = print("hi"); y + 1)
(y = print("hi"); x => y + 1)

x = M; y = N; K ≡ y = N; x = M; K   x ∉ N && y ∉ M


x = (y = M; N); K
x = (y = M; N); K

f_code = (x, y) => x + y;
f = (x) => (y) => f_code(x, y);
g = x => (
  f(x)(1)
);

y = (x => M)(z); K

f = x => M; y = f(z); k

L<(M; N); K> |-> L<M; N>; L<K>
L<M(N)> |-> L<M>; \1 L<N>

(M; N); K |-> M; N; ↑ : 1; K

M(N) |-> M; \1 N

sum_or_mul : (is_mul : Bool, l : List(Nat)) -> List(Nat);
sum_or_mul = (is_mul, l) =>
  l <|
  | [] => []
  | [hd, ...tl] =>
    is_mul <|
    | true => hd * sum_or_mul(is_mul, tl)
    | false => hd + sum_or_mul(is_mul, tl);

sum_or_mul : (is_mul : Bool, l : List(Nat)) -> List(Nat);
sum_or_mul_is_mul_true : (l : List(Nat)) -> List(Nat);
sum_or_mul_is_mul_false : (l : List(Nat)) -> List(Nat);

sum_or_mul = (is_mul) =>
  is_mul <|
  | true => sum_or_mul_is_mul_true(tl);
  | false => sum_or_mul_is_mul_false(tl);
sum_or_mul_is_mul_true = (l) =>
  is_mul = true;
  l <|
  | [] => []
  | [hd, ...tl] =>
    hd * sum_or_mul_is_mul_true(tl);
sum_or_mul_is_mul_false = (l) =>
  l <|
  | [] => []
  | [hd, ...tl] =>
    hd + sum_or_mul_is_mul_false(tl);

// very important for literals
x == y |- f(x) |-> f(y)

f : (b : Bool, l : List(Nat)) -> List(Nat);
f_cons : (b : Bool, hd : Nat, tl : List(Nat)) -> List(Nat);

f = (b, l) =>
  l <|
  | [] => []
  | [hd, ...tl] => f_cons(b, hd, tl);
f_cons = (b, hd, tl) =>
  b <|
  | true => f(true, tl)
  | false => f(false, tl);

f : (b : Bool) -> Unit;
f = (b) =>
  b <|
  | true => f(b)
  | false => f(b);

f = (b) =>
  b <|
  | true => f_is_true()
  | false => f_is_false();

f : (b : Bool) -> Unit;
f_is_true : () -> Unit;
f_is_false : () -> Unit;
f = (b) =>
  b <|
  | true => f_is_true()
  | false => f_is_false();

H : f_is_true() == f(true);
f_is_true = () => f_is_true();
f_is_false = () => f(false);

f : (b : Bool) -> Unit;
f_is_true : () -> Unit;
f = (b) =>
  b <|
  | true => f_is_true()
  | false => f(false);

f_is_true = () =>
  b = true;
  b <|
  | true => f_is_true()
  | false => f(false);

cost_of_f : Cost(f);
cost_of_f = b =>
  b <|
  | true => 1 + cost_of_f(true)
  | false => 1 + cost_of_f(false);

cost_of_f_true = () =>
  cost_of_f(true);


f : (b : Bool) -> Unit;
f_is_true : () -> Unit;
f_is_false : () -> Unit;

f = (b) =>
  b <|
  | true => f_is_true()
  | false => f_is_false();
f_is_true = () => f(true);
f_is_false = () => f_is_false();

f = (b) =>
  b <|
  | true =>
    l : _;
    H : Cost(l()) < Cost(f(true));
    l = () => l();
    l();
  | false => f(true);



```

## Back to Constraints

```rust
[M == S : E] ≡
  E == A & [S : A; M]
[x == S : E] ≡
  E == lookup(x)
[(x = N; M) == S : E] ≡
  ∃A. [x : A; N] &
    x : A = N; [S : E; M]
[(x : S_A) -> S_B(x) == (x : A) -> B(x) : E] ≡
  E == Type &
  [S_A == A : Type] &
    x : A; [S_B(x) == B(x) : Type];
[S : E; (x : A) -> B(x)] ≡
  E == Type &
  (S_A, S_B) = eta_forall(S);
  [S_A : Type; A] &
    x : A; [S_B(x) : Type; B(x)];


-----------------------------
Γ |- t = (x : A) & B(x) : Type

x : <P>(v : P(unit)) -> P(u) = unit;


T : (A : Type) -> Type;
T := A => (x : T(A)) & _;

T = A => (x : T(A)) & _;


(x = N; M)

_A := •A;
x : •A = •B;

----------------------------
Γ |- •A == N -| Γ |- •A == N
M

id : <A>(x : A) -> A & (x : _A1) -> _A1
  = <A>(x : A) => x;

x = (id : (x : _A1) -> _A1)("a");

f = (x : A, x_S : x == M || x == N) => _;

(x, y) =
  z = 1;
  (z, 1);

x_y_z = 1;
(x, y) = f(x_y_z);
(x, y) =
  z = 1;
  f(z);


(x, y) : A;

----------------
Γ |- x -| Δ

(x, y) : (x : A, y : B);
x = 1;
y = 2;

M ? (
| ("continue", value) => _
| ("exception", exn) => _
);


Bool & Bool
Γ, x : A |- B : Type
Γ |- A ≡ (x : A) & B
-----------------------
Γ |- (x : A) & B : Type

Flat_array : (s : Size, A : Data) -> Unboxed(Data);
Flat_array = (s, A) =>
  s ? (
  | 0 => ()
  | 1 + s => (hd : A, tl : Flat_array(s, A))
  );

copy : <A>(s : Size, arr : Flat_array(s, A)) -> Flat_array(s, A);
copy = <A>(s, arr) =>
  s ? (
  | 0 => ()
  | 1 + s =>
    (hd, tl) = arr;
    (hd, copy(s, tl))
  );

Array = (A : Data) -> Data;
Array = A => (
  size : Size,
  mem : Flat_array(size, A),
);


copy : <A>(arr : Array(A)) -> Array(A);
copy = <A>(arr) => (
  loop :
)
set : <A>(
  arr : Array(A),
  el : A,
  i : Nat & i < length(arr)
) -> (arr : Array(A))



set_exn : <A>(
  arr : Array(A),
  el : A,
  i : Nat,
) -> (arr : Array(A)) % Invalid_index;

log : (msg : String) -> () % Log;

List : (A : Data) -> Data;
List = A =>
  | (tag == "null")
  | (tag == "cons", hd : A, tl : List(A));

List : (A : Data) -> Data;
List = A =>
  | (tag == "null")
  | (tag == "cons", hd : A, tl : ...List(A))
  | (tag == "link", list : List(A));



Nat : Data;

Unboxed_nat : Unboxed(Data) = ...Nat;

(x : Unboxed_nat, y : Unboxed_nat)

f1 : (x : A, y : B) -> C;
f2 : (p : ...(x : A, y : B)) -> C;

g : (p : (x : A, y : B)) -> C


```
