# Teika Language

This document intends to describe the Teika Language. Which refers to all the features supported by Teika but not exactly how it will be presented to an user.

## Goals

Those are the current goals in order of importance and they may change.

1. Soundness, type preservation
2. Convenience, inference and effects
3. Uniformity, calculus of construction
4. Logical consistency, sizing
5. Decidability, subtyping

## Smol

This intends to be a core language, it shuold contain all the proof power of Teika, other features should be described in terms of Smol.

Additionally it should be possible to expand the features of Teika to Smol in a reasonably readable way for experienced humans.


## Warning Before the Warning

This document was written by me(@EduardoRFS) when I was way less proficient in type theory, so there were mistakes.

## Warning

This document was written by me(@EduardoRFS) and I'm not proficient in type theory, so mistakes and changes are very likely to happen here.

## Smol

This is the canonical language and should contain all the theoritical power on Teika, everything else should be describable in terms of the following features.

```rust
// Type in Type
(Type : Type);

// variable
(A);

// forall
((A : Type) -> (x : A) -> A);
// lambda
((A : Type) => (x : A) => x);
// apply
(id Nat 1);

// exists
(A : Type, A);
// pair
(A = Nat, 1 : A);
// split
((A, x) = p; x);

// equal
Equal : (A : Type) -> (x : A) -> (y : B) -> Type;
// refl
Refl : (A : Type) -> (x : A) -> Equal A x x;
// subst
Subst :
  (A : Type) ->
  (x : A) ->
  (y : A) ->
  (x_eq_y : Equal A x y) ->
  (P : (x_or_y : A) -> Type) ->
  (p_x : P x) ->
  P y;
```

### Forall, Lambda and Apply

This is abstraction and can be seen as universal quantification. Those are functions, they're your work horse, the majority of your code will likely be about declaring and calling those.

#### Forall

The forall is the description of a function, it describes the type of the paramater and the type of the return, the return may depend on the type such as in polymorphic functions and dependent functions. Those are values.

```rust
/* forall syntax */
((A : T) -> A);

/* rule */
A : Type  B : Type
------------------
  (x : A) -> B
```

#### Lambda

The lambda is the way to introduce a function, the type of a lambda will always be a forall. The body may dependend on the parameter. Those are values.

```rust
/* lambda syntax */
((A : Type) => A);

/* rule */
           b : B
---------------------------
(x : A) => b : (x : A) -> B
```

#### Apply

This is the source of computing, the left side is anything of an arrow type and the right side must have the same type of the paramater expected by the lambda.

```rust
/* apply syntax */
(lambda argument);

/* rule */
l : (x : A) -> B  a : A
-----------------------
      l a : B
```

### Exists, Pair and Split

This is allows pairing and can be seen as existential quantification. Those are pairs, they're the fundamental way to pack data together, all of your modules can be represented using those.

#### Exists

This is the description of pairs, it describes the type of the left value and the type of the right value, the type of the right value may depend on the left value. Those are values.

```rust
/* exists syntax */
(A : Type, A);

/* rule */
A : Type  B : Type
------------------
   (x : A, B)
```

#### Pair

This is how you introduce a pair, the type of a pair will always be an exists. The type of the right side may depend on the left side, but the value itself cannot depended on the left side. Those are values.

```rust
/* pair syntax */
(A = Nat, 1 : A)

/* rule */
      l : A   R : B
---------------------------
(x = l, r : B) : (x : A, B)
```

#### Split

This is how you destruct a pair, it is like a let, but it can extract pairs. The body may depend on the pair values. The type of the body may depend on the pair values.

```rust
/* split syntax */
((A, one) = p; A);

/* rule */
p : (x : A, B)  b : C
---------------------
((x, y) = p; b) : C
```

### Equal, Refl and Subst

This is leibniz equality, it can be used as a way to do some form of dependent elimination, they hopefully only matters as a stepping stone for building libraries.

#### Equal

This is the type of an equality, it states that the first and second values are literally the same, allowing to replace one for the other.

#### Refl

This is the introduction of an equality, it is the only way to construct an equality and any equality is equivalent to this.

#### Subst

This is how you can eliminate an equality, it is enough to derive all the other transformations that are desirable for equalities

```rust
/* sym */
Sym =
  (A : Type) =>
  (B : Type) =>
  (A_eq_B : Equal A B) =>
    subst
      ((B : Type) => Equal B A)
      A
      B
      A_eq_B
      (Refl A)
/* trans */
Trans =
  (A : Type) =>
  (B : Type) =>
  (C : Type) =>
  (A_eq_B : Equal A B) =>
  (B_eq_C : Equal B C) =>
    subst
      ((C : Type) => Equal A C)
      B
      C
      B_eq_C
      A_eq_B;
```
