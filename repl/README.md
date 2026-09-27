# Teika typer REPL

A standalone REPL for the core in the vendored `typer.ml`. It uses local copies
of `../typer/typer.ml` and its matching `../syntax/` files. The former demo
checker, evaluator, built-ins, and surface extensions have been removed.

From this folder:

```sh
dune exec --root . ./repl.exe
dune runtest --root .
```

From the repository root:

```sh
dune exec --root repl ./repl.exe
```

The REPL reads one term or let per line, elaborates it with `Typer.Elaborate`,
and prints its type and reduced core value. The initial context contains the
variable `Type`, defined as the core universe (`Type : Type`). Let bindings
persist until the REPL exits. Press Ctrl+D to exit.

For example, enter these lines in order:

```text
A = Type
id(x : A) = x
id(A)
```

The last line prints `Type` as both its type and value. A final semicolon is
optional. Failed commands leave the previous session intact.

## Syntax

These examples use the syntax that accompanies the upstream typer:

```text
Type
x : Type
f(x : Type) : Type
f(x : Type) = x
(f(x : Type) = x)(Type)
(id(A : Type)(x : A) = x)(Type)(Type)
A = Type; id(x : A) = x; id(Type)
id(A : Type)(x : A) : A = x; id(Type)(Type)
```

`x : A` forms a self type. `f(x : A) : B` forms a self type whose body is a
dependent function type. `(f(x : A) = M)` is a fix expression containing a
function. Applications use juxtaposition or parentheses; multiple arguments
use successive applications, such as `id(Type)(Type)`.

At the prompt, a bare `x = M` saves a sequential let binding for later lines.
`x = M; N` keeps its binding local to the expression `N`. In both cases, the
value and annotation are checked before `x` enters scope, so they can use
earlier bindings but cannot refer to `x` recursively or to later bindings.
Function patterns and inline annotations are supported.

Parenthesized fix expressions retain their recursive meaning. To save the
recursive `False` type, bind that expression and then use it on later lines:

```text
False = (False = (f(C(f : False) : Type) : C(f)))
False
absurd(f : False)(A : Type) : A = f(C(x : False) = A)
absurd
```

The outer `False = ...` saves the value; the inner `(False = ...)` introduces
its recursive name. Evaluating the inner expression alone does not save a
binding. Arguments inside a function parameter's type, such as the inner `f`
in `C(f : False)`, stay scoped to that parameter.

Standalone declarations such as `x : Type; ...` and declaration hoisting remain
unsupported. A line containing `x : Type` evaluates a self type and does not
declare `x` for later lines.

For example:

```text
(f(x : Type) = x)(Type)
type:  Type
value: Type
```

The printer displays core constructors such as `self`, `fix`, `Pi`, and
`lambda`; its output is diagnostic core notation, not additional input syntax.
Unsolved metavariables display as `?`.

## Scope

The existing variable, application, self/fix, sequential let, and
annotated/applied pattern paths are completed. Declaration blocks, boxes, pairs,
sigma types, unit, and their patterns remain explicitly unsupported. Literals,
unions, intersections, braces, and holes remain rejected by the vendored logical
parser. No demo language features or built-ins are supplied.

The solver supports pattern unification: flexible applications can be solved
when their arguments are distinct rigid variables. Some terms need explicit
annotations; for example, use `(f(x : Type) = x)(Type)` instead of
`(f(x) = x)(Type)`. Recursive definitions generally need result annotations.
Reduction is bounded so a divergent term reports an error and the REPL can
continue; this is not termination checking.

All generated terms carry source locations. New contexts, substitution state,
and reduction state are functional; the upstream term-link and level updates
remain inside `Typed_tree`. Each command checks a separate copy of the session's
term graph so partial unification cannot alter earlier session values.

See [TYPER_ISSUES.md](TYPER_ISSUES.md) for source provenance, fixes, limitations,
and regression coverage.
