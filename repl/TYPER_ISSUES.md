# Vendored typer issues

## Sources and boundary

The local files were copied from these repository sources, then patched only
inside `repl/`. The typer was brought into line with the source update that
removed per-term types; types now live in the elaboration context.

| Local file    | Source                  |
| ------------- | ----------------------- |
| `typer.ml`    | `../typer/typer.ml`     |
| `ctree.ml`    | `../syntax/ctree.ml`    |
| `ctree.mli`   | `../syntax/ctree.mli`   |
| `cparser.mly` | `../syntax/cparser.mly` |
| `lparser.ml`  | `../syntax/lparser.ml`  |

`lexer.ml`, `printer.ml`, `session.ml`, and `repl.ml` provide the standalone
input/output path. There is no dependency on a sibling syntax or typer library.
The old demo modules and their tests have been removed.

This integration completes the existing core and sequential let bindings.
Declaration hoisting, boxes, pairs, sigma types, unit, and their patterns remain
unsupported, as requested. The core retains the upstream nine term constructors.

## Fixed

1. **Incomplete typed core and elaborator.** The upstream source contains
   expression holes, unfinished context helpers, and constructor calls with
   missing arguments. The local implementations complete variables, dependent
   function families, lookup, annotated/applied patterns, and self/fix
   elaboration. Core terms do not store type annotations.
   `Type` is a variable with a definition in the initial context; it has no
   special case in term checking. An empty or shadowing context uses ordinary
   name lookup.
2. **Grouped expressions required EOF.** The concrete grammar reused the
   top-level `term_opt` inside delimiters. Group contents now end at their closing
   delimiter, allowing inputs such as `(f(x : Type) = x)(Type)`.
3. **Declaration annotations were discarded during lowering.** `parse_block`
   passed only the annotation's left side to `parse_pat`. The vendored parser
   now preserves the whole annotated pattern. This fixes the syntax tree;
   standalone declarations and declaration hoisting remain unsupported.
4. **Missing name-printer interface.** `Name.pp` existed in the syntax
   implementation but was absent from its interface. It is exposed locally
   for the REPL's variable printer and diagnostics.
5. **Substitution dropped shadowing lambdas.** Substitution for a lambda's own
   binder now preserves the lambda. Other binders are freshened to prevent
   capture. Substitution also traverses dependent function domains and bodies.
6. **Let substitution mutated a shared body.** The source linked a let's body
   to its substituted result. The local substitution builds a result without
   changing the original body.
7. **Nested substitutions could grow excessively.** The local implementation
   substitutes heads directly instead of repeatedly wrapping every child in
   another suspended let. This keeps ordinary dependent applications from
   exhausting reduction work or memory. A three-argument dependent function has
   an end-to-end regression.
8. **The escape check had the wrong direction.** A free rigid variable is
   allowed only if its level is visible to the metavariable being solved.
   Variables bound inside a proposed solution are allowed. Lowering happens
   after the solution passes validation; occurs checks reject cyclic solutions.
9. **Application comparison did not advance the expected function.** Both
   application spines are traversed correctly, with argument and arity checks.
   Solved flexible heads exposing beta-redexes are reduced before comparison.
10. **Flexible solutions reversed multiple parameters.** The solver abstracts
    arguments in application order, rejects duplicate/non-variable arguments,
    and links the flexible representative to its solution in either comparison
    direction. Contextual metavariables explicitly carry their rigid arguments
    so later dependent annotations can refer to earlier parameters.
11. **Lambda comparison was unfinished.** Lambda bodies are compared under a
    common fresh binder. Eta comparison handles a lambda versus a neutral
    function in both directions. Types stay in the elaboration context,
    following the updated upstream representation.
12. **Fix elaboration lost the fix constructor.** The source returned a lambda
    after checking a fix body. Elaboration now builds `T_fix` around the
    abstracted body, and application unfolds it. Nested self types are
    specialized at the checked value, including when a function is passed to
    dependent identity.
13. **Marks confused sharing with recursion.** The source marked both sides
    immediately before testing whether either was marked, so ordinary terms
    could be reported as cyclic. Reduction now carries its active path and
    remaining steps explicitly. Shared terms in separate branches are accepted;
    cyclic or excessively long reduction reports a located error.
14. **Missing locations in synthesized terms.** Constructors receive a source
    location or inherit one from their originating term/binder. There are no
    dummy locations. Lexer, logical-parser, and typer errors report the relevant
    input position. Rendering finishes before a result line is printed, so a
    rendering error does not leave a partial result.
15. **Sequential lets were not elaborated.** `B_fix` bindings now check their
    right-hand sides and inline annotations in the preceding context, then add
    the name for the remaining block. Applied patterns abstract their parameters
    into lambdas. Immutable variable definitions allow later bindings to use
    local type aliases, and the result uses the existing `T_let` constructor.
    Let names are not implicitly recursive; explicit fix values remain available
    on the right-hand side. `B_def` declarations are still rejected without
    hoisting names or annotations.
16. **Substitution missed variables captured by local aliases.** An immutable
    variable definition can contain the variable being substituted, even when
    the alias itself has a different identity. Substitution now follows that
    definition so dependent lets and returned closures receive the substituted
    value, while preserving the original alias.
17. **Higher-order parameters leaked their local arguments.** In
    `(False = (f(C(f : False) : Type) : C(f)))`, the inner `f` from the type
    of `C` was retained in the surrounding context. It shadowed the outer `f`
    and caused `unify: escape check for f` at column 36. Parameter checking now
    exports only the parameter's root binding, keeping its arguments scoped
    inside its type. This applies to both fix/self patterns and let parameters.
18. **Recursive type checking could not use the body being defined.** After
    fixing the scope leak, the same example failed with a type clash: `False`
    stayed opaque while checking `C(f)`. Self-type elaboration now exposes its
    shape before checking dependent annotations. Explicit fix bodies carry
    functional, scoped equations that conversion can unfold, and type checking
    also unfolds completed fix types. Recursive variables remain rigid in
    metavariable arguments; occurs and escape checks are unchanged. The exact
    example, an annotated version, a parameterized recursive type, and a saved
    `False` eliminator now check. `Type` is still rejected as a value of `False`.
19. **Every input line discarded its context.** `Session.submit` now returns
    the next session and an optional result. A bare `name = value` line saves
    a sequential let, with an optional final semicolon. Parenthesized fix
    expressions and local `x = M; N` blocks keep their existing meaning and do
    not export names. Definitions can be used and shadowed on later lines;
    standalone declarations and hoisting remain unsupported. For example:
    `False = (False = (f(C(f : False) : Type) : C(f)))` saves the recursive type.
20. **Persistent contexts could retain partial solutions from failed inputs.**
    The underlying solver updates term links and levels. Each command now
    copies the reachable graph using functional maps that preserve sharing,
    binder identity, variable definitions, and source locations. The CLI adopts
    the returned session after elaboration and rendering succeed. A rejected
    command cannot specialize a saved function's unresolved parameter type or
    replace a previous binding. Successful commands preserve sharing between
    aliases while leaving earlier session values intact.
21. **Binder freshening made substitution exponential.** Each lambda first
    traversed its body to rename the binder and then traversed it again for
    substitution. The two recursive walks doubled the work at every nested
    binder. Substitution now carries replacements and fresh names through a
    single traversal, including let bodies and captured aliases. In a local
    process-level check, defining `False` and its eliminator fell from about
    5.45 seconds to 0.006 seconds. The previously recorded composition-type
    timeout is also fixed and has a regression:
    `compose(A : Type)(B : Type)(C : Type)(f : (h(y : B) : C))(g : (k(x : A) : B))(x : A) : C`.

## Remaining limits

- The original pattern-unification restriction remains. For example,
  `(f(x) = x)(Type)` currently reports `unify: not a pattern`; its explicitly
  annotated version succeeds. Unannotated recursive result types can fail the
  escape check; `(f(x : Type) : Type = f(x))` supplies the needed result type.
- `Type : Type` follows the prototype's single-universe core. This REPL does not
  add a universe hierarchy or a consistency claim.
- Reduction has a 10,000-step budget per operation. A terminating but expensive
  term can reach that limit. Printing also limits nesting depth. Neither limit
  establishes termination of the language.
- Per-reduction budgets do not bound total elaboration time. Session graph
  copying also adds work proportional to the saved graph and its sharing-map
  lookups; this remains a prototype rather than a large-session performance
  guarantee.
- As in the upstream representation, unification updates term links and levels.
  There is no added mutable trail, mutable context, global counter, or rollback
  mechanism. A failed line is discarded with its copied context.
- Brackets, product nodes, and standalone declarations are parsed but rejected
  during elaboration.
  No interpretation or runtime behavior has been invented for those forms.

## Validation

`test_typer.ml` covers substitution, capture avoidance, dependent function types,
shared lets, substitution through captured aliases and 24 nested binders,
occurs/escape checks, level lowering, representative direction, alpha/eta
equality, application spines, higher-order pattern solutions, fix comparison,
scoped recursive conversion, bounded recursive reduction, and initial-context
lookup and shadowing.

`test_session.ml` covers parsing and elaborating the supported syntax,
dependent and higher-order applications, shadowing, recursive fix values,
source locations throughout typed graphs, unsupported-form rejection, and
isolation after a failed input. Dedicated polymorphic identity and constant
function cases check type formation, inferred types, evaluation with distinct
arguments, and rejection of bodies returning the wrong type.
Sequential let cases cover annotations, function patterns, dependent type
aliases, shadowing, captured values, nested scopes, explicit fix values, and
rejection of forward references, implicit recursion, invalid unused values, and
declarations.
Further cases cover the exact `False` reproducer, a parameterized recursive
type, the `False` eliminator, composition type formation, higher-order parameter
scope, bindings across input lines, optional semicolons, shadowing, comments,
session isolation, preservation of shared aliases, and failure after partial
unification. Location checks also traverse copied recursive definitions.

Run both suites with `dune runtest --root .` from `repl/`.
