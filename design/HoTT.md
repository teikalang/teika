```rust

K : <A, x : A>(eq : x == x) -> eq == refl(x);

Eq : <A>(x : A, y : A) -> Type;
refl : <A>(x : A) -> Eq(x, x);

Eq = <A>(x, y) =>
  (s : Eq(x, x)) & <P>(v : P(refl(x), x)) -> P(s, y);
refl = <A>(x) => <P>(v) => v;

K : <A, x>(eq : Eq<A>(x, x)) -> Eq(eq, refl(x))
  = <A, x>(eq) => eq<(eq, _) => Eq(eq, refl(x))>(refl(refl(x)));

eq : Eq(Bool, Bool);
eq = <P>(v) => v;

eq = transport<T => Eq(Bool, T)>(H, eq);
// same as
eq =
  eq = transport<T => Eq(Bool, T)>(H, eq);
  <P>(v : P(refl(Bool), Bool)) => transport<T => P(eq, T)>(H, v);



(x == y : (z : A) & B(z)) ≡

t : (z : A) & B(z) = transport(H, v);

t : (z : A) & B(z);
t = transport(H, v);



transport(H : )
transport<T => Eq(Bool, T)>(H, refl(Bool));

K : <A, x>(H : Eq<A>(x, x)) -> Eq(H, refl(x));
```
