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
- no syntatical indication of current contex
- field and forall syntax kind of mixed

```rust
Syntax =
  | Identifier // variable
  | Syntax -> Syntax // lambda
  | Syntax Syntax // apply
  | Syntax. Syntax // forall
  | Syntax = Syntax; | Syntax = Syntax; Syntax // binding
  | { Syntax } // structure
  | Syntax : Syntax // constraint
  | Syntax.Syntax // field
  | (Syntax) // parens
```
