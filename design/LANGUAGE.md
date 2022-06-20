# Teika Language

This document intends to describe the Teika Language. Which refers to all the features supported by Teika but not exactly how it will be presented to an user.

## Goals

Those are the current goals and they should be treated as temporary solution to viabilize an initial implementation.

1. Soundness should provide confidence
2. Predictability is helpful to everyone
3. HM inference as most code is simple
4. System F-omega power means consistency
5. Decidability as no one likes typer stack overflow

## Warning

This document was written by me(@EduardoRFS) and I'm not proficient in type theory, so mistakes and changes are very likely to happen with time.

## Core Teika

This is the canonical language and should contain all the power on Teika after, everything else should be describable in terms of the following features.

```rust
// abstraction
((x: Nat) => x + 1);
(Nat -> Nat);

// polymorphism
((A: *) => (x: A) => x);
((A: *) -> A -> A);

// higher kinds
((T: * -> *) => (x: T Nat) => x);
((T: * -> *) -> T Nat -> T Nat);

// application
(id Nat 1);

// classical record
({ x = 1; y = "a"; });
(({ x; y; }) => x + y);

// existential record
({ A = Nat; x: A = 1; });
(({ A: *; x: A; }) => x);
```

### Abstraction

This is term abstraction, those are classical functions, functions where the expected parameter is a term, those are your work horse, the majority of your code should be about declaring and calling those.

```rust
add: Nat -> Nat -> Nat;
print: String -> ();
```

### Polymorphism

Abstract over types, they're also called generics, functions where the expected parameter is a type, those allows your code to be more general and work across a large number of types.

Extends the computational power of the pure language, with this you can already start describing things like numbers directly on the language without any extension.

```rust
id: (A: *) -> A -> A;
map: (A: *) -> (B: *) -> (A -> B) -> [A] -> [B];
```

### Higher Kinds

Allows your abstractions to hide type functions, while still exponsing the fact that it is a type function. This is mostly a feature for advanced use cases.

Extends the computational power of the pure language even further, likely as powerful computationally as the core calculus gets.

```rust
Monad = {
  Monad: * -> *;
  return: (A: *) -> A -> Monad A;
  bind: (A: *) -> (B: *) -> Monad A -> (A -> Monad B) -> Monad B;
};
```

### Application

This is how abstractions can be eliminated, aka how to call functions. It doesn't discriminate between parameters that are terms or types.

```rust
id Nat 1;
map Nat String Nat.to_string [1];
```

### Classical Records

Those are also called "structs", they're the canonical way to pack data together and are expected to be used on a daily basis.

In Core Teika they offer subtyping, they're technically an existential record where the quantifier is empty.

```rust
User = {
  id: Nat;
  name: String;
};
eduardo = { id = 0; name = "Eduardo"; };
```

### Existential Record

Those are also called "modules" they are records where some of the types were abstracted away, those should be your first tool to make implementation details go away.

Technically, they're weak existential records, in the future strong existential records may be desirable but they should not conflict with the weak version.

```rust
Name: {
  Name: *;
  make: String -> Name;
  show: Name -> String;
} = {
  Name = String;
  make x = x;
  show x = x;
};
```

#### Forget Rule

The above abstractin is only possible because of the forget rule, that allows to weaken the type, but as existential records are impredicative here, allowing this to apply in an unbounded manner makes the system undecidable.

So this should be limited in such a way that conserves the decidability of Core Teika, the main options here are:

- if the type being weakend is an existential record in a contravariant position it is rigid and should check only for equality, not for subtyping
  - Easy subset of this to implements, if it is below the existential weakend record it should always check for strict equality.
  - Another option instead of checking for strict equality is do subtyping except the FORGET rule
- disallow existential records to be weakend through FORGET and add specific
  - advantage of this is that it's a very principal rule, but requires more syntax.

## Teika

Everything here is highly WIP

```rust
// naturals
(1);
(Nat);
// strings
("a");
(String);

// tuples
(1, "a");
(Nat, String);

// let binding
(one = 1; one);

// implicit type arguments
({A} => (x: A) => x);
({A} -> A -> A);

// inference
((x => x + 1): Nat -> Nat);
// generalization
((x => x): {A} -> A -> A);

// named expressions
((1: (x: Nat)): Nat);
// strict named expressions
((x = 1): !(x: Nat));

// partial application
(?add 1 _);
(?(_ - 1));

// variants
(| Left Int | Right String);

// jsx
(<div> hello </div>);

// effectful
(String -[Read | Write]> ());
```

## Impredicative

Impredicative means everything can be treated in a homogenous and expected manner, alternatives are possible such as universe polymorphism, sadly impredicative cannot be universaly extended to all features, such as implicit module abstraction as that lead to undecidability
