# Errors

## Mixes arrows

```rust
typed = (x : Int) -> x;
meant = (x : Int) => x;
```

How to detect this? The basic message will be, `expected a Type, but received a Int, maybe you meant => instead`.

- bidirectional checking helps when the type expected is a forall
- create an invalid forall where the right side is not a type and then detect it
- handle the error to get the context?

```ocaml
let return = infer_term return in
let return =
  try type_of_term return with
  | exception _ -> ...
```
