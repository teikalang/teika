# Typer

## Normalization

Teika requires normalization for equality, terms such as `(x => x) A` should be equal to `A`, while this is trivial for closed terms, with open terms it gets trickier, especially under unification, trying to preserve error messages and being fast.

As such,

### Techniques

There is 3 major knobs, representation, laziness and optimizing for specific terms.

### Types of terms

It is useful to split terms in a couple categories:

**Simple terms**, terms that lack binder are considered simple terms, as such all variables are free variables, may have opaque functions, such as nominal type constructor, easy to detect on traverse, very common for types.

**Linear terms**, terms that all bound variables appears once, in general those are very friendly to mutation and behave quite close to simple terms when properly tagged, note that type-level occurrences still count as usage for typed normalization.

**Complex terms**, every other term

### Representation

This includes both, high-level representation, how to model the graph described by a term and low-level representation such as how the graph will actually be placed in memory.

The most common representations are:

**Traditional named variables**, lacks a canonical representation, requires α-renaming, can be seen as if every variable exists globally, it describes a heap and is very mutation friendly. It is especially efficient for simple terms and terms with few expansions.

**De-bruijn indices**, has a local canonical representation, requires shifting of free variables, quite fast to evaluate, models a traditional stack. Not ideal for simple as those have no bound variables.

**De-bruijn levels**, has a global canonical representation,

Note that α-renaming and shifting is only required when actually expanding substitutions.

### Tricks

Substituting a variable to another variable is the same

### Material

- https://hal.science/hal-00111285/document

, note that you can mix and match those, by having a specific
