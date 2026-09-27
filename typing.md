```rust
//
Type, A, B ::=
Term, M, N, K ::=
  | Data(l) | Type(l) | (M ⇒ A) | (M ⇐ A) | x
  | (Π(x : A) : B) | (λ(x : A) = K) | M(N)

Data(l) |-> Data(l) ⇒ Data(1 + l)
Type(l) |-> Type(l) ⇒ Type(1 + l)

// only allowed if not inferred yet
(Π(x : A) : C{x}) |-> (Π(x : A) : C{x ⇒ A})
(λ(x : A) = C{x}) |-> (λ(x : A) = C{x ⇒ A})

(Π(x : A ⇒ S_A) : (B ⇒ S_B)) |-> (Π(x : A) : B) ⇒ #pi_sort(S_A, S_B)
// TODO: check sort of B?
(λ(x : (A ⇒ S_A)) = (M ⇒ B)) |-> (λ(x : A) = M) ⇒ (Π(a : A) : B)
(M ⇒ (Π(a : A) : B))(N) |-> M(N ⇐ A) ⇒ B{x := (N ⇐ A)}

(M ⇒ A ⇐ A) |-> M

// only allowed for conversion checking
(λ(a : A) = K)(N) |-> K{a := N}

//
Type, A, B ::=
Term, M, N, K ::=
  | Data(l) | Type(l) | (M ⇒ A) | (M ⇐ A) | x
  | (Π(x : A) : B) | (λ(x : A) = K) | M(N)

Data(l) |-> Data(l) ⇒ Data(1 + l)
Type(l) |-> Type(l) ⇒ Type(1 + l)

// only allowed if not inferred yet
(Π(x : A) : C{x}) |-> (Π(x : A) : C{x ⇒ A})
(λ(x : A) = C{x}) |-> (λ(x : A) = C{x ⇒ A})
(Π(x : A ⇐ _S_A) : (B ⇐ _S_B)) |-> (Π(x : A) : B) ⇒ #pi_sort(_S_A, _S_B)
// TODO: check sort of B?
(λ(x : (A ⇒ S_A)) = (M ⇒ B)) |-> (λ(x : A) = M) ⇒ (Π(a : A) : B)
(M ⇒ (Π(a : A) : B))(N) |-> M(N ⇐ A) ⇒ B{x := (N ⇐ A)}

(M ⇒ A ⇐ A) |-> M

// only allowed for checked terms
(λ(a : A) = K)(N) |-> K{a := N}



```
