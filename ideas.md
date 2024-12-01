# Ideas

## Bytecode

Shipping bytecode to JS could be faster and smaller than direct compilation, but it is likely to be slower than good direct compilation.

Additionally it could be used as a runtime way of switching between JS and WASM on demand. Plus it could imply in more predictable GC time when linearity is added.

## Elaborate then check

Similar to constraint based, allows to compile in paralell without typing for debugging.

Known as the french approach

## Substitution

TT_closed and level on term to know when to propagate substitutions vs discarding

## Tweets

- https://twitter.com/TheEduardoRFS/status/1704691533168153004
- https://twitter.com/TheEduardoRFS/status/1704371844407783779
- Interesting format M : A $ N vs M : A where A : (T = (T : Type, G : Grade), G =0)
- Type case can be allowed at compile time, but all final functions must have Type $ 0

## Locally Nameless vs Context

- both can do LSP hover
- locally nameless makes printing more direct, but the context makes it simpler
- both could allow partial recompile in theory
- locally nameless makes not possible to retype
- exposing the context to LSP / debuggers / REPL will be harder
- unification step-by-step tracking still needs a context

## GC root bit

Use a bit to enable GC root instead of registering, could work nicely on WASM and especially behaves super well with linearity.

## Linearity

- Interesting format M : A $ N vs M : A where A : (T = (T : Type, G : Grade), G =0)
- Type case can be allowed at compile time, but all final functions must have Type $ 0

What if every function only return 1 copy and there is a duplication + fusing of statement? This doesn't allow to have fractional resource requirement as in `(x : A $ 1) => B $ 2`

## Bootstrap

What is the base? Smol? IR? Bytecode? JavaScript? OCaml?
Use same parser or maybe Lisp like syntax?

## Multiple variables, single binder

Similar to holes

Isn't this just a pair?

## Named Tuple

Detect patological cases such as the following even without strict.

```rust
f = () : (x : Nat, y : Nat) = _;
(y, x) = f ();
```

## Types Context as a way of doing pairs

```rust
fst p == x;
snd p == x + 1;

λ. (\1 + 1)
```

## Syntax for potentially infinite terms

Is this just coinduction on a pair?

## Unification about callee properties

If a function is always only called with a pure function, some additional transformations can be done, this may make a function tail call recursive.

Also if List.map is always called with an associative function.

Finding those properties can be done with unification. A variable that is only generalized after the program finishest types but accumulate all the effects for example.
