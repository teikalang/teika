## Overview

Guarantees?

Reduction strategy is Open CBV, always left most.

```rust
Erase Sugar

Flatten apply
Lift substitutions
Lift lambdas || Closure conversion

Closure-less call's

Linear inlining
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


[N]


(λ. λ. _X) N

f == g
λ. M == _X

f == g
E<λ. M> == _X


Value (V) ::=
  |
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
```
