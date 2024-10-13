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
```
