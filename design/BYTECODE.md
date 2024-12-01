## Design Philosophy

This initial prototype should be design in such a way that a future implementation could drop "all" of this instructions and implement them in a streamed pass, aka it should be able to run the instruction like it does today.

Two kinds of machine, one var is a jump and lambda takes arg from stack and jump if there is arg. Versus SECD where apply does the jump and lambda just pushes. First one is similar to ZINC.

```rust
allocation
call stack
data stack
effects
tail call
dynamic loading
type checked dynamic loading

// allocation
alloc

// call
call
ret
ret.jmp

// effects
raise
perform
barrier
```

## Optimizations

Stack depth analysis / inference
