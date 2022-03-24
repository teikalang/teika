# Hacking

## Terminology

To avoid noise, this code uses quite a bit of terminology, they're more or less standards.

### Identifier

Used to indicate any variable that is not part of the language, may be introduced by users through things like bind or lambda.

Examples:

- `ident = another_ident;`
- `ident -> ident`

https://en.wikipedia.org/wiki/Identifier_(computer_languages)

### Binding

The thing that **bind** an identifier to a term, it's how identifiers are introduced by users.

Essentially a binding says that inside some term the identifier exists.

Lambdas such as `(x: Int) -> body` saying that x inside of body will have the type Int.

Bind terms such as `x = y; body` saying that x inside of body will have the value o y.

https://en.wikipedia.org/wiki/Name_binding

### Lambda

TLDR: it's a function.

https://en.wikipedia.org/wiki/Lambda_calculus

### Application

It's the act of applying a function to an argument, also called `calling a function`.

https://en.wikipedia.org/wiki/Function_application

### Structure

I(@EduardoRFS) am not sure about the math origin, but here it will be used in the sense of one of the actual values of a module, it is essentially a record but it may also contain types.

The syntax for a structure has the format of `{ key = value; }`.

### Type constructor

Aka a lambda in the type system. Here it will likely be a lambda that introduces types on it's return.

Example:
`A -> { id: A } `

https://en.wikipedia.org/wiki/Type_constructor

### Expression

https://en.wikipedia.org/wiki/Expression_(computer_science)

### Annotation

In general it will be used in the direction of a type annotation,

The syntax for an annotation has the format of `received_value : expected_type`.

## Abbreviations

To avoid noise, this code uses quite a bit of abbreviations.

| abbrev | meaning     | uses               |
| ------ | ----------- | ------------------ |
| ident  | identifier  | `add` is an ident  |
| struct | structure   | `{}` is a struct   |
| annot  | annotation  | annot = `(x: int)` |
| expr   | expression  | expr = a + b       |
| param  | parameter   | param -> body      |
| arg    | argument    | f arg              |
| loc    | location    | `add`.s_loc = None |
| desc   | description | term_desc          |
| pat    | pattern     | x \| pat -> y      |
