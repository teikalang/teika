```rust
Sort (S) ::= Type | Prop | Data | Line.
Term (M, N) ::=
  | S // sort
  | x // var
  | x => M // lambda
  | M N // apply
  | x : A; M // hoist
  | x = N; M // fix
  | (x : A) & B // intersection;

--------------------------
Γ |- Type l : Type (1 + l)

--------------------------
Γ |- Prop l : Type (1 + l)



--------------------------
Γ |- Data l : Type (1 + l)

--------------------------
Γ |- Line l : Type (1 + l)


--------------------------
Γ |- Size l : Type (1 + l)

```
