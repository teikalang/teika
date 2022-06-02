# Inference

Teika intends to be an ML-like language, which means inference is a must.

## HM inference

The basic kind of inference present at Teika is Hindley-Milner inference, which essentially assume that all parameters are monomorphic(no quantification) and when a weak variable(aka a variable not constrained) escape it's scope, then an implicit forall is added.

It is quite limited but relatively simple to understand and good visualizations are possible to be developed.

### Higher Kinded Types

There is a couple decisions to be made on inference for higher kinded types.

```rust
// When infering the following two types are possible
f = T => (x: T Int) => x;
// the easy one, here we follow the fact that Int is a type
f = (T: _ -> _) => (x: T Int) => x; // T is an arrow
f = (T: #(Int) -> _) => (x: T Int) => x; // T param is the type Int
f = (T: #(Int) -> *) => (x: T Int) => x; // unify with x
// the logical one, consider it's kind
 f = T => (x: T (Int: *)) => x; // Int has kind *
 f = (T: _ -> _) => (x: T (Int: *)) => x; // T is an arrow
 f = (T: * -> _) => (x: T (Int: *)) => x; // T param is the kind *
 f = (T: * -> *) => (x: T (Int: *)) => x; // unify with x
```

I decided to go with the second one, because in the same way that when you call a function with a value it infers it's type, when you call a function with a type it infers it's kind. If you do `f => f 1` the type is not `{A} -> (f: 1 -> A) -> f 1`, but `Int -> Int`.
