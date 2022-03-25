# Module

This intends to document behavior and features of modules.

## Implicit type

All structures contain an implicit type which by default is abstract both internally and externally, it can be assigned internally.

This behaves similarly to object-oriented languages like Java and but the goal is to achieve similar interface design to the `Module.t` convention in OCaml.

<!-- TODO: syntax for accessing internal type -->

### Implicit type alias

When assigning a structure to a value, the implicit type has an internal alias so that users can have consistent type naming internally and externally.

<!-- TODO: should this be available when doing (F { x }), no this makes changing the ident a breaking change -->

While this is not always available it will be used it can be used in most cases.

Example:

```rust
Amount = {
  // alias
  of_nat: Nat -> Amount;
  add: Amount -> Amount -> Amount;
};
```
