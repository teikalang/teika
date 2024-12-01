## Let Function Syntax?

```rust
add (a, b) = a + b;
```

## Variant Syntax?

```rust
Either = (A, B) =>
  | ("left", content : A)
  | ("right", content : B);
```

## Arrow Syntax?

```rust
incr : Nat -> Nat = _;
```

## Type Variable Syntax?

```rust
map : (l : List 'A, f : (x : 'A) -> 'B) -> List 'B = _;
```

## Implicit Record Syntax?

```rust
bind : {M : Monad; A; B}. (m : M A, f : (x : A) -> M B) -> M B;

id : A. (x : A) -> A;

Never = A.(A;
x.(y)
m . x
```

## Option Syntax?

```rust

```

## Quantity Syntax

## Partial Application

```rust
incr : (n : Nat) -> Nat = ?(1 + _);
```

## Intersection Record

```rust
Nat : {
  @Nat : Type;
  zero : Nat;
} = {
  @Nat = Int;
  zero = 1;
};
zero : Nat = Nat.zero;
```

## Implicit Arguments

```rust
// solved through unification
id = {A : Type} => (x : A) => x;
// same as
id = {A} => (x : A) => x;
one = id(1);

// solved through magic
Monad = {
  pure : {A} -> (x : A) -> @M A;
  bind : {A} -> {B} -> (m : @M A, f : (x : A) -> @M B) -> @M B;
};
pure = {M : Monad} => {A} => (x : A) => M.pure x;
bind = {M : Monad} => {A} => (x : A) => M.pure x;
```

## Inference + Implicit Types + Instantiation + Generalization

```rust
// inference
incr = x => x + 1;
incr : (x : Nat) -> x = incr;

// implicit types
id = {A} => (x : A) => x;
id : {A} -> (x : A) -> A = id;

// instantiation
one : Nat = id(1);

// generalization
apply = (f, x) => f(x);

// instantiation
one = id 1;
```

## Implicit Arguments

```rust


```

## Overload Operator

```rust
// Equality Type
// Equality Check
( == ) : {A} -> (x : A) -> (y : A) -> Type;
Eq = { equal : (x : @A) -> (y : @A) -> Option (x == y); };
( == ) : {A : Eq} -> (x : A) -> (y : A) -> Option (x == y);

( == ) :
  ( == ) : {A} -> (x : A) -> (y : A) -> Type &
  ( == ) : {A : Eq} -> (x : A) -> (y : A) -> Option (x == y);

( == ) = Overload
{A : Eq}

[%define ( == ) {
  apply = args => (
    when_unify = typ => (
      typ ==
    );
    new_var ()
  )
}]
f = (x : Nat, x_eq_0 : x == 0) => ();
x = (x : Nat) =>
  x == 0
  | Some x_eq_0 => f (x, x_eq_0)
  | None => ();
```

###

```rust
Rust = 1;
add (a, b) = a + b;

```
