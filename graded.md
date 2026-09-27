```rust
Term, M, N, K ::=
  | x | Type | Grade | 0 | 1 | M + N | M * N
  | Π(a : A $ G) -> B | λ(a : A $ G) => K | M(N)

∅ |- Type : Type

// type formation also counts usage, that's why I think it's terminating
// H is subject type usage, it's inferred
Γ |- A : Type  Δ |- G : Grade  Φ, a : A $ H |- B : Type
-------------------------------------------------------
Γ, Δ, Φ |- Π(a : A $ G) -> B : Type

// annotation doesn't count
Γ |- A : Type  Δ |- G : Grade  Φ, a : A $ G |- K : B
----------------------------------------------------
(Γ, Δ) * 0, |- λ(a : A $ G) => K : Π(a : A $ G) -> B

```
