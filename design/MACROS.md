# Macros

## Clear

Clear the current scope, allows only the selected identifiers, necessity modality?

```rust
[%clear(+) 1 + 2]
[%clear(+) 1 * 2] // fails
```
