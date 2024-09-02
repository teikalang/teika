# Compiler

This is intended to be an overview of how the Teika compiler works.

## Smol

The `smol` library is intended to be an alternative frontend, a simpler version of Teika, that can be emitted from full fledge Teika.

The main advantages is that Smol is "smol", it is a much simpler language, with no subtyping, inference or implicits.

So compiling it can be done with a fraction of the code size. It is intended to act as a redundancy layer and be part of the bootstrapping process, anything that compiles on Teika should be compilable on Smol.

### Overview

Pipeline: `source -> Slexer -> Sparser -> Stree -> Lparser -> Ltree -> Typer -> Term`.

`Stree` is similar to a concrete syntax tree, it is intendend to represent the building blocks of the syntax and make parsing much easier. It accepts way more than just Smol.

`Ltree` is a traditional AST, it represents all the valid inputs for Smol. This is what actually defines the Smol language syntax.

`Term` is a typed AST, this is the output of `smol`, it is intended for code generation and verification. It is guaranteed to be semantically valid and it is the canonical definition of the Smol language semantics.

## Teika

The `teika` library here is the frontend for the full fledge Teika, this is the main step on running Teika.

The focus here is on having a frontend that is flexible, as it shall be used on the LSP and for the actual Teika compiler. Additionally this frontend is gonna be exposed to users for the development of user space features such as macros.

### Overview

Pipeline: `source -> Slexer -> Sparser -> Stree -> Lparser -> Ltree -> Typer -> Ttree`

`Stree` is similar to a concrete syntax tree, it is intendend to represent the building blocks of the syntax and make parsing much easier. It accepts way more than just Teika. Macros store the `Stree` allowing the macros to describe DSLs that contain more general syntax than plain old Teika.

`Ltree` is a traditional AST, it represents all the valid inputs for Teika. This is what actually defines the Teika language syntax. This should only describe valid states as it is intended to be the output of macros.

`Ttree` is a typed AST, this is the output of `teika`, it is intendend for code generation, instrumentantion and verification. It is guaranteed to be semantically valid and it is the canonical definition of the Teika language semantics. Macros may emit `Ttree` directly for development.

## Bootstrapping

Smol was originally also intended as a bootstrapping layer, inteded to be used when Teika moves to a self bootstrapping compiler. By compiling the Teika compiler to Smol and then using the Smol compiler that can be implemented in any other language.

The current plan is to have a very naive JS backend, focused on generating 1:1 code, both for development and bootstrapping, by doing `teika -> teika_to_js` and `teika -> teika_to_smol -> smol_in_ml` we can have some confidence that the final compiler is agnostic to the bootstrapping compiler.

While it may be overkill, the bulk of the cost is very likely in `teika_to_smol`, which is already a goal because of redundancy.
