# Spefication

```rust
Term (M, N) ::=

(x, y) =>

f (x, 1)

f x 1

Point = { x : Int; y : Int; };
```

## Rules

```rust
// typing context
-----------
• : Context

Γ : Context  x : A ∉ Γ  Γ |- A : Type
--------------------------------- // term hypothesis
(Γ, x : A) : Context

Γ : Context  x == _ ∉ Γ  Γ |- M : A
----------------------------------- // term alias
(Γ, x == M) : Context

Γ : Context  _ :> x ∉ Γ  Γ |- M : x  Γ |- T : Type
-------------------------------------------------- // L-coercion hypothesis
(Γ, (M : x) :> T) : Context

Γ : Context  _ :> x ∉ Γ  Γ |- M : S  Γ |- x : Type
-------------------------------------------------- // E-coercion hypothesis
(Γ, (M : S) :> x) : Context

// TOOD: subtyping hypothesis
// TODO: cast coerce hypothesis to subtyping hypothesis?


// typing rules
Γ : Context  Γ |- A ⇐ Type
-------------------------- // Infer
Γ |- M ⇒ A

Γ : Context  Γ |- A ⇐ Type
-------------------------- // Check
Γ | E |- M ⇐ A

Γ : Context  Γ |- S ⇐ Type  Γ |- T ⇐ Type
----------------------------------------- // Coerce
Γ | R | E |- (M : S) :> T

Γ : Context  Γ |- S ⇐ Type  Γ |- T ⇐ Type
----------------------------------------- // Subtype
Γ | R | E |- S <: T


// TODO: better notation
// TODO: if R and E are missing then they're empty

Γ |- M ⇐ A ≝ Γ | E |- M ⇐ A

Γ |- (M : S) :> T ≝ Γ | R | E |- (M : S) :> T

Γ |- S <: T ≝ Γ | R | E |- S <: T

```

## Infer

```rust
Γ |- M ⇐ A
---------------- // Infer -> Check
Γ |- (M : A) ⇒ A
```

## Check

```rust
Γ |- M ⇒ S  Γ | • | E |- (M : S) :> T
------------------------------------- // Check -> Coerce
Γ | E |- M ⇐ T

Γ, x == A | E, x |- M ⇐ A
------------------------- // Check-eq-once
Γ, x == A | E |- M ⇐ x
```

## Coerce

```rust
// TODO: should expansions be preserved from coerce into subtype?
Γ | R | E |- S <: T
------------------------- // Coerce -> Subtype
Γ | R | E |- (M : S) :> T

--------------------------------------- // Coerce-S-assumption
Γ, (M : x) :> T | R | E |- (M : x) :> T

--------------------------------------- // Coerce-T-assumption
Γ, (M : S) :> x | R | E |- (M : S) :> T

Γ, x == S, (M : x) :> T | R, x | E |- (M : S) :> T
-------------------------------------------------- // Coerce-S-eq-once
Γ, x == S               | R    | E |- (M : x) :> T

Γ, x == S, (M : S) :> x | R | E, x |- (M : S) :> T
-------------------------------------------------- // Coerce-T-eq-once
Γ, x == S               | R | E    |- (M : S) :> x

Γ, x == M, (M : x) :> T | R, x | • |- (M : S) :> T
-------------------------------------------------- // Coerce-R-eq-annot
Γ, x == M               | R, x | • |- (M : x) :> T

Γ, x == N, (M : S) :> x | • | E, x |- (M : S) :> T
-------------------------------------------------- // Coerce-E-eq-annot
Γ, x == N               | • | E, x |- (M : S) :> x
```

## Subtyping

Γ == typing context
R == received context
E == expected context

S == received type
T == expected type

```rust
Γ | R | E |- S ≡ T
------------------- // Subtype -> Equality
Γ | R | E |- S <: T

Γ, x == S | R, x | E |- S <: T
------------------------------ // S-eq-once
Γ, x == S | R    | E |- x <: T

Γ, x == S | R | E, x |- S <: T
------------------------------ // T-eq-once
Γ, x == S | R | E    |- S <: x

Γ, x == M | R, x | • |- S <: T
------------------------------ // R-eq-annot
Γ, x == M | R, x | • |- x <: T

Γ, x == N | • | E, x |- S <: T
------------------------------ // E-eq-annot
Γ, x == N | • | E, x |- S <: x


Term :=
  | x | x => M | M N // lambda
  | x : A; K | x = M; K // hoist
  | Type | (x : A) -> B | (x : A) & B // types

```

## Equality

Γ == typing context
R == received context
E == expected context

M == received term
N == expected term

```rust

```

## Language

```rust
// variable
-----------------
Γ, x : A |- x ⇒ A

// functions
Γ, x : A |- B ⇐ Type
------------------------
Γ |- (x : A) -> B ⇒ Type

Γ |- T_A <: S_A  Γ |- S_B <: T_B
----------------------------------------- // Forall
Γ |- (x : S_A) -> S_B <: (x : T_A) -> T_B

Γ, x : A | • |- M ⇐ B
--------------------------
Γ |- x => M ⇐ (x : A) -> B

Γ |- T_A <: S_A  Γ | • | • |- (M : S_B) :> T_B
----------------------------------------------------
Γ |- (x => M : (x : S_A) -> S_B) :> (x : T_A) -> T_B

Γ |- M ⇒ (x : A) -> B  Γ |- N ⇐ A
---------------------------------
Γ |- M N ⇒ B[x := N]

// intersections
Γ, x : A |- B ⇐ Type
-----------------------
Γ |- (x : A) & B ⇒ Type

Γ |- (M : S) :> A  Γ |- (M : S) :> B[x := A]
--------------------------------------------
Γ |- (M : S) :> (x : A) & B

---------------------------
Γ |- (M : (x : A) & B) :> A

-----------------------------------
Γ |- (M : (x : A) & B) :> B[x := M]

Γ |- A <: T
--------------------- // R-A-Intersection
Γ |- (x : A) & B <: T

Γ |- B : Type  Γ |- B <: T
-------------------------- // R-B-Intersection
Γ |- (x : A) & B <: T

// TODO: subtyping for this but dependent
Γ |- B : Type  Γ |- S <: A  |- S <: B
------------------------------------- // E-Intersection
Γ |- S <: A & B
```
