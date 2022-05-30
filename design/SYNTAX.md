# Syntax

This should document the syntax of the project, they and the tradeoff's.

## Requirements

<!-- TODO: technically modules and types are fused  -->

The syntax needs to be able to describe four class of terms, modules, expressions, types and patterns.

The syntax should also be consistent and succint, this requires the syntax meaning to be context dependent.

The syntax should be simple so that it can easily be user manipulated, allowing tools like macros and ppx to be trivially implemented.

## Unified Representation

To avoid having too much notation, an unified representation was choosen, this means that all classes of terms have an identical syntax.

This is achieved by accepting a lot more code during parsing and rejecting this code later.

Another problem is the need for parens in many situations such as typing a simple binding. This is avoided by allowing a couple places to omit parens in an ad-hoc manner.

Pros:

- AST is really small, making macros easier
- Syntax error messages are much easier
- Error recovery is way easier
- Flexible, ppx can use of the invalid syntax

Cons:

- invalid code may be parsed
- not clear where parens are needed
- no syntatical indication of current context

```rust
Syntax =
  | Identifier // variable
  | Number // number
  | Syntax -> Syntax // arrow
  | Syntax => Syntax // lambda
  | Syntax Syntax // apply
  | Syntax = Syntax;
  | Syntax = Syntax; Syntax // binding
  | Syntax : Syntax;
  | Syntax : Syntax; Syntax // binding signature
  | Syntax Syntax[] = Syntax;
  | Syntax Syntax[] = Syntax; Syntax // binding + lambda
  | { Syntax } // structure
  | Syntax : Syntax // constraint
  | Syntax.Syntax // field
  | (Syntax) // parens
```

## Arrow and Lambda

The entire language can be described using lambda only, in fact arrow can only describe a subset of the language.

But adding arrow allows easier inference and a simpler syntax for describing common types.

## Implicit Argument

Initially `A. Syntax` was the thought way to do implicit parameters, but this lead to a couple weird syntax properties, such `A.Eq` and `A. Eq` being two different things.

Also that completely prevented binding + function syntax `id x = x`, which may be desirable in the future.

So for the syntax argument it is currently using `{A: M}` which was an already supported syntax.

- https://agda.readthedocs.io/en/v2.6.1/language/implicit-arguments.html

## Binding Lambda

A common feature in languages such OCaml and Haskell is to support a fusion syntax for lambdas and binding, in the form of `add a b = a + b`.

The advantages of this is that it's way more succinct for most functions and it's a common feature in other programming languages.

The disadvantage is that it's not straightforward to explain to users that `f = x -> x` is the same as `f x = x`, it also doesn't work with binding + constraint such as `x: Int -> Int = x -> x`.

This was decided to be a reasonable choice due to type constructors. And as constraints + lambda should not be common.

```rust
Packet {A} = {
  id: Nat;
  data: A;
};
```
