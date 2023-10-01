```rust
1 :: id ::

t :: id |- \0 -> t

x[open := t]

_A[open x/+2] `unify` x\+2

_A := x/+2[close +2]
_A := x/-1

x/+2[close +2][open x/+2]

x/+2[+2 := -1][-1 := x/+2]

(_ => x/-2)[open x/+2]
x/-2[Lift][open x/+2]

[open x/+2; id] |- _A `unify` [Free; Free]

A/-1 -> A/-2
(A/+2 -> A/+2)[close +2]

A/+2[close +2] ~~> A/-1
A/+2[id][close +2] ~~> A/-2

A => B => _C `unify` A => B => A/+2[id][close +2]

{} [id; id;] |- _C `unify` [id; id; close +2; id;] |- A/+2


A => (B => A/+2)[close +2]
A => B => A/+2[id]
_C := A/+2[id][close +2][id][id][-id][-id]
{} - [id;] |- A/+2[id][close +2]
{ +2 = 0; } - [id;] |- A/+2[id]
{ +2 = 0; } - [id; id;] |- A/+2 ~~> A/-2

A => A/-1

A => A/+2[close +2]

{} - [] |- A/+2[close +2]
{ +2 = 0; } - [] |- A/+2 ~~> A/0
{ +2 = 0; } - [id] |- A/+2

A/-2[id][open x/+2] ~~> A/+2

A/+2[close +2][open +2] ~~> A/+2

A/+2[id][close +2] ~~> A/-2
A/+2[id][close +2][id][open +2] ~~> A/+2
A/+2 ~~> A/-2 ~~> A/
[Free, Free] |- A/+1

x @
| a
| b


Free = +
Bound = -

+A
A => -A

λ.λ.\1

(λ.λ.\1) "a" "b"

"b" :: "a" :: [] |- \1 ~> "a"

codemod that tags every occurencce of identifier so that the dev needs to go and check the code, removing the annotation

Type\1
String\2
A\3

Sorry\2
@fix(False). (f : @self(f -> @unroll False f)) =>
  (P : (f : @self(f -> @unroll False f)) -> Type) -> P f

False : @self. _A[close +3]
f : @self. _B[close +4]

@unroll \+3 : _A
_A := (f : @self. _B[close +4]) -> Type
@unroll \+3 \+4 : Type
_B := @unroll \+3 \+4

False : @self. (f : @self. @unroll \-1 \-0) -> Type
f : @self. _C[close +5]

@unroll \+3 : (f : @self. @unroll \+3 \-0) -> Type
@unroll \+3 \+5 : Type

expected : @self. @unroll \+3 \-0
received : @self. _C[close +5]

@self. @unroll \+3 \-0 `unify` @self. _C[close +5]
@self. @unroll \+3 \-0 `unify` @self. _C[close +5]

(False : @self. (f : @self. @unroll False f) -> Type) =>
  (P : (f : @self(f -> @unroll False f)) -> Type) -> P f

(False : @self(False -> (f : @self. @unroll False f) -> Type)) =>
  (P : (f : @self(f -> @unroll False f)) -> Type) -> P f

False : @self. (f : @self. @unroll \-1 \-0) -> Type
f : _A

@unroll \+3 : (f : @self. @unroll \+3 \-0) -> Type
@unroll \+3 \+5 : Type

@self(False). (f : @self(f). @unroll False f) -> Type

False : @self. _A[close +3]
f : @self. _B[close +4]

@unroll \+3 : _A
_A := (f : @self. _B[close +4]) -> Type
@unroll \+3 \+4 : Type

_B := @self. @unroll \+3 \-0

@self. (f : @self. @unroll \+3 \-0) -> Type[close +4]


False : @self. (f : @self. @unroll \-1 \-0) -> Type
f : @self. _B
@self((f : @self(f -> _B)) -> @unroll \+3 \-0)

f : @self. _A[close +5]

@unroll \+3 : (f : @self. @unroll \+3 \-0) -> Type
@unroll \+3 \+5 : Type

@self. @unroll \+3 \-0 `unify` @self. _A[close +5]
(@unroll \+3 \-0)[open \+6] `unify` (_A)[close +5][open \+6]
(@unroll \+3 \-0) `unify` (_A)[close +5]
(@unroll \+3 \-0) `unify` (_A)[open \+5]

@self(False -> (f : @self. @unroll False f) -> Type)

False : _A
f : _B

@unroll \+3


@self. _C[close +3] `unify` @self. _A

False : _A

(False : @self(False -> (f : @self. @unroll False f) -> Type)) =>
  (P : (f : @self(f -> @unroll False f)) -> Type) -> P f

False : @self. (f : @self. @unroll \-1 \-0) -> Type)
f : @self. _B[close +5]


T[subst] === U[subst]
T === U

_ -> _A -> _A === _ -> \-0 -> \-1
(_A -> _A)[subst] === (\-0 -> \-1)[subst]
_A\+3 -> _A\+3 === \-0 -> \-1

_A\+3 := \-0


```

## Unification

```rust
( + ) : Int -> Int -> Int;

x => x + 1

Γ, x : _A |- x + 1 : Int

_A := Int

incr : Int -> Int = x => x + 1;

map(x => x + 1)
(x : Int) => x + 1

(x => x) : 'a. 'a -> 'a
Γ, x : _A |- x : _A

id
  : (A : Type) -> (x : A) -> A
  = (A : Type) => (x : A) => x;

id(String) : ((x : A) -> A)[A := String]

f = (id : (A : Type) -> A -> A) => (id(1), id("a"))

3 [] {} |- \+3[close +3][open \+3]
4 [\+3 - 4] {} |- \+3[close +3]
5 [\+3 - 4] { +3 := -0 - 5 } |- \+3 ~> \-0
4 [\+3 - 4] { +3 := -0 - 5 } |- \+3 ~> \-0 ~> \+3

\+3[close +3][open \+3]
\+3[close +3][open \+3]

5 [\+3 - 4] { +3 := [\-0 - 3; -0 - 5] } |- \-0
4 [\+3 - 4] { +3 := [\-0 - 3; -0 - 5] } |- \-0 ~> \+3
3 [\+3 - 4] { +3 := [\-0 - 3; -0 - 5] } |- \-0 ~> \+3 ~> \-0

\+3[close +3][open \+3]
\-1[open \+3][close +3][open \+3]

Γ, open \+3, close +3, open \+3, close +4 |- \-0
Γ, open \+3, close +3, open \+3 |- \-0
Γ, open \+3, close +3 |- \+3
Γ, open \+3 |- \-0
Γ |- \+3

[] |- \+3[close +3]

[Type] |- (A : Type) -> ((x : A\+2) -> A\+2)[close]
[Type] |- (A : Type) -> ((x : A\+2) -> A\+2)[close]


\+3[close +3][open \+3]

expected : (A : Type) -> A -> A
received : (B : Type) -> B -> B

expected : (A : Type) -> \-1 -> \-2
received : (B : Type) -> \-1 -> \-2

(A : Type\+0) -> \+1 -> \+1
(A : Type) => (x : \-1 -> \-2) => (x : \-2 -> \-3)


(A : Type\+0) -> \-1
(A : Type) -> (\+1 -> \+1)[close]

(A : Type\+0) -> \-1 -> \-2
(B : Type\+0) -> _A -> _A

A := \-1

(A : Type\+0) -> \-1 -> \-2
(B : Type\+0) -> \-1 -> \-1


(\-1 -> \-2)[open +4]

\+4 -> \+4
_A -> _A

\+3[close +3][open \+3]

close +3 open \+3 |- \+3


\+3[close +3][open \+3]

[\+3] { +3 := -0 } |- \-0 ~> \+3 ~> -0

(\+3 -> \+3)[close +3][open \+3]
\+3 -> \+3[close +3][open \+3]

@self. @unroll \+3 \-0 `unify` @self. _A[close +5]
(@unroll \+3 \-0)[open \+6] `unify` _A[close +5][open \+6]
(@unroll \+3 \-0) `unify` _A[close +5]
(@unroll \+3 \-0)[open \+5] `unify` _A

|- @self. @unroll \+3 \-0 `unify`
|- @self. _A[close +5]

[] |- @self. @unroll \+3 \-0
[close +5; ] |- @self. _A

[open \+6; ] |- @unroll \+3 \-0
[close +5; open \+6; ] |- _A

_A := @unroll \+3 \-0[open \+5]

x = 1;
eq : x + 1 == 2 = refl;

x == 1;


f : (A : Type) -> (\+1 -> \+1)[close ] = _;
f

[Type] |- (A : Type) -> ((x : A\+1) -> A\+1)[close]

\+3[open \+3][close +3]

[] {} |- \+3[open \+3][close +3]
[] { +3 := -1 } |- \+3[open \+3]

{} |- \+3[close +3][open \+3]
{} |- \+3[\+3 := \-1][\-1 := \+3]
{ \-1 := \+3; } |- \+3[\+3 := \-1]
{ \-1 := \+3; \+3 := \-1; } |- \+3
{ \-1 := \+3; } |- \-1
{ } |- \+3

{} |- \+3[close +3][open \+3]
{} |- \+3[\+3 := \-1][\-1 := \+3]
{ \-1 := \+3; } |- \+3[\+3 := \-1]
{ \-1 := \+3; \+3 := \-1; } |- \+3
{ \-1 := \+3; \+3 := \+3; } |- \+3


{} |- \+3[close +3][open \+3 -> \+3]
{} |- \+3[\+3 := \-1][\-1 := \+3 -> \+3]
{ \-1 := \+3 -> \+3; } |- \+3[\+3 := \-1]
{ \-1 := \+3 -> \+3; \+3 := \-1; } |- \+3
{ \-1 := \+3 -> \+3; \+3 := \+3 -> \+3; } |- \+3

{} |- \+3[\+3 := \+3 -> \+3][\-1 := \+3 -> \+3]

{} |- \+3[\+3 := \-1][\-1 := \+3 -> \+3]

\+3[\+3 := \+3][\-1 := \+3]

\-1[open \+3][close +3][open \+3]

5 [\+3 - 4] { +3 := [\-0 - 3; -0 - 5] } |- \-1
4 [\+3 - 4] { +3 := [\-0 - 3; -0 - 5] } |- \-1 ~> \+3
3 [\+3 - 4] { +3 := [\-0 - 3; -0 - 5] } |- \-1 ~> \+3 ~> \-1

+1 |- (A : Type) -> \+1 -> \+1
+4 |- (A : Type) -> \+4 -> \+4

(A : Type) -> (\+1 -> \+1)[close +3]
(\-1 -> \-2)[open +3]


f : (Int\-1 -> Int\-2)[open Int\+3]
x : Int\-2[id][open Int\+3]

expected : Int\-1[open Int\+3]
received : Int\-1[id][open Int\+3]

2 [] |- (A : Type) -> A\-1 -> A\-2
2 [] |- (A : Type) -> _B -> _B

2 [] |- (A : Type) -> A\-1 -> A\-2
2 [] |- (A : Type) -> _B\+3 -> _B\+3

3 [open A\3; ] |- A\-1 -> A\-2
3 [open A\3; ] |- _B\+3 -> _B\+3

_B := A\-1
3 [A\3;] |- A\-1 -> A\-2
3 [A\3; A\3;] |- _B -> _B

3 [A\3;] |- A\-1
3 [A\3; A\3;] |- _B

_B := A\-1[close ]
2 [] |- (A : Type) -> _B[close] -> _B[shift][close]

3 [] |- (A\-1 -> A\-2)[open A\+3]
3 [] |- (_B[close] -> _B[shift][close])[open A\+3]

3 [open A\+3] |- A\-1 -> A\-2
3 [open A\+3] |- _B[close] -> _B[shift][close]

3 [open A\+3] |- A\-1
3 [open A\+3] |- _B[close]

3 [open A\+3] |- A\-1
3 [open A\+3; close] |- _B[]

3 [] |- (A\-1 -> A\-2)[open A\+3]
3 [] |- (_B -> _B)[close][open A\+3]

[] |- (A\-1 -> A\-2)[open A\+3]
[] |- (_B -> _B)[open A\+3]

[A\+3] |- A\-1 -> A\-2
[A\+3] |- _B -> _B

_B := A\-1[A\+3]![A\+3]

x => id(x)
x => x

[Int : Type] |- (x : Int\1 = _; x\1 : (Int\1)[lift])
[Int : Type; x : Int\1] |- (x\1 : (Int\2)[lift])

[] |- x\-1[y\-2][id][x\-1]
[x\-1] |- x\-1[y\-2][id]
[x\-1; id] |- x\-1[y\-2]
[x\-1; id; y\-2] |- x\-1
[x\-1; id] |- y\-2
[x\-1] |- y\-1
[] |- x\-1

1[2 . id][2 . id]
2[2 . id]
1

[] |- 1[2 . id][2 . id]
[2 . id] |- 1[2 . id]
[2 . id; 2 . id] |- 1
[2 . id] |- 2
[] |- 1

1[2[shift] . id]
2[shift]
3

[] |- 1[2[shift] . id]


[] |- 1[2 . id][1 . id]

[] |- 1[2[shift] . id]
[2[shift] . id] |- 1 ~> 2[shift]
[2[shift] . id] |- 2[shift]
[2[shift] . id] |- 2[2 . id]

[] |- 2[3 . id]
[2 . id] |- 2

[] |- -1[open +3]


\+3[close +3][open \+3]

4 [] {} |- \+3[close +3][open \+3]
5 [] { +4 : 4 } |- \+3[close +3]

4 [] |- \+3[close +3][open \+3]
4 [open \+3] |- \+3[close +3]
4 [close +3; open \+3] |- \+3
4 [open \+3] |- \-1
4 [] |- \+3

[] [] |- \+1[close +1][open \+1]
[open \+1] [] |- \+1[close +1]
[open \+1] [close +1 at 1] |- \+1
[open \+1] [] |- \-1
[] [] |- \+1

[] [] |- \+1[close +1][shift]

((A : Type) -> \+2 -> \+2)[shift]


\+1[close] = \-1
\+2[close] = \+1
\+3[close] = \+2

\-1[shift] = \-2
\-2[shift] = \-3

(\+2 -> \+2)[close +2]
\+2[close +2] -> \+2[close +2][shift]
\-1 -> \-2

(\+2 -> \+2)[close][close]
(\+2[close] -> \+2[close][shift])[close]
(\+2[close][close] -> \+2[close][shift][close][shift])


(A : Type) -> (\+2 -> \+2)[close +2]

\-1[open t] = t
\-(n + 1)[open t] = \-n

\+n[+n close -m] = \-m

l |- (A -> B)[s]
l |- A[s] -> B[open \+l][s][shift]

(\+2 -> \+2)[close +2]
(\+2[close +2] -> \+2[open \+l][close +2][shift])
(\+2[close +2] -> \+2[close +2][shift])

[] [] |- \+2[open \+l][close +2][shift]
[open \+l; noop; shift] [noop; close +2; noop] |- \+2
[open \+l; noop; shift] [noop; close +2; noop] |- \+2

[open \+l; shift] [close +2] |- \+2
[shift] [close +2] |- \-1

\+2[close][close] -> \+2[open \+l][close][open \+l][close]
\-2 -> \+1[close][open \+l][close]

(a -> b)[s]
(a[s] -> b[lift(s)])

[] |- (A : Type) -> A\-1
[] |- (A : Type) -> _A

[open \+2] |- (A : Type) -> A\-1
[open \+2] |- (A : Type) -> _A

(\+2 -> \+2)[close +2]
\+2[close +2] -> \+2[open \+l][close +2]

// TODO: exception?
\-1 == \-1
\-(n + 1)[lift(s)] == \-n[s][shift]

(\+2 -> \+2)[close +2]
(\+2[close +2] -> \+2[lift(close +2)])
(\+2[close +2] -> \+2[close +2][shift])

(\-1 -> \-2)[open A]
(\-1[open A] -> \-2[lift(open A)])
(\-1[open A] -> \-1[open A][shift])

(A : Type) -> (\+1 -> \+1)[close]
(A : Type) -> \+1[close] -> \+1[close][shift]
(A : Type) -> \-1 -> \-2

((A : Type) -> \-1 -> \-2)
(\-1 -> \-2)[1 . shift]

((A : Type) -> \+2 -> \+2)[shift]
((A : Type) -> \+3 -> \+3)

0 [] { 3 : 3 } |- \+3



[] |- (\+2 -> \+2)[close +2]
[close +2] |- \+2 -> \+2

[] |- (\+2 -> \+2)[close +2]
[close +2] |- \+2 -> \+2

\+2[close +2] -> \+2[open \+l][close +2]

[] |- (\+2 -> \+2)[close +2]
[close +2] |- \+2 -> \+2


[\+1] [+2] |- \-1


(\-1 -> \-2)[open ]

(A : Type) -> (\+2 -> \+2)[close +2]

[] |- (\+2 -> \+2)[close +2]
[close +2] |- \+2 -> \+2
[open +l; lift([close +2])] |- \+2


(λa)[s]
λ(a[+l . ])
[open +l; lift([close +2])] |- \+2

[open +l; lift([close +2])] |- \+2

[open \+l] [] |- \+2

[\+1] [+2] |- \-1 -> \-2

(\-1 -> \-2)[id]
\-1[id] -> \-2[open \+l][id][shift]

\-1 -> \-2


(λa)[s]
λ(a[+l . ])

[] |- \-1[open a[open b]]
[a[open b]] |- \-1 ~> a[open b]
[open b; _] |- (λ-2)

[] |- \-1[open a[open b]]

// fast / naive enough?
[open \+2; open \+1] {} |- \+1[close +1][open \+1]
[open \+1; open \+2; open \+1] {} |- \+1[close +1]
[open \+1; open \+2; open \+1] { 1 : [3] } |- \+1
[open \+1; open \+2; open \+1] [] |- \-1
[_; _; _] [] |- \+1


[open \+2; open \+1] {} |- \+1[open \+1][close +1][open \+1]
[open \+1; open \+2; open \+1] {} |- \+1[open \+1][close +1]
[open \+1; open \+2; open \+1] { 1 : [3] } |- \+1[open \+1]
[open \+1; open \+1; open \+2; open \+1] { 1 : [3] } |- \+1
[open \+1; open \+1; open \+2; open \+1] { 1 : [3] } |- \-2


[open \+2; open \+1] { 1 : [2] } |- \+1[open \+1]
[open \+1; open \+2; open \+1] { 1 : [2] } |- \+1
[open \+1; open \+2; open \+1] { 1 : [2] } |- \+1


[open \+2; open \+1] {} |- \+1[close +1][open \+1]
[open \+1; open \+2; open \+1] {} |- \+1[close +1]

[open \+1; open \+2; open \+1] { 1 : 3 } |- \+1
[open \+1; open \+2; open \+1] { 1 : 3 } |- \-1
[open \+1; open \+2; open \+1] { 1 : 3 } |- \+1

[open \+2; open \+1] [none; none] |- \+2[close +2][open \+2]
[open \+2; open \+2; open \+1] [] |- \+2[close +2]

[open \+2; open \+2; open \+1] { 2 : } |- \+2[close +2]

// capture entire stack
[open \+2; open \+1] {} |- \+1[open \+1][close +1][open \+1]
[open \+1; open \+2; open \+1] {} |- \+1[open \+1][close +1]
[open \+1; open \+2; open \+1]
{ 1 : ([open \+1; open \+2; open \+1], {}) } |- \+1[open \+1]
[open \+1; open \+1; open \+2; open \+1]
{ 1 : ([open \+1; open \+2; open \+1], {}) } |- \+1
[open \+1; open \+2; open \+1] {} |- \+1
[open \+1; open \+2; open \+1] {} |- \-1

// shift stack
[open \+2; open \+1] {} |- \+1[open \+1][close +1][open \+1]
[open \+1; open \+2; open \+1] {} |- \+1[open \+1][close +1]
[open \+1; open \+2; open \+1] { 1 : (3, {}) } |- \+1[open \+1]
[open \+1; open \+1; open \+2; open \+1] { 1 : (3, {}) } |- \+1
[open \+1; open \+2; open \+1] {} |- \+1
[open \+1; open \+2; open \+1] {} |- \-1

\+1[open \+1][close +1][open \+1]
\+1[close +1][open \+1]
\-1[open \+1]
\+1


\-1[shift][open \+2][open \+1]

[open \+2; open \+1] |- \-1[shift]
[open \+1] |- \+1


\-1[open a] ~> a
\-(n + 1)[open a] ~> \-n

(λa)[+1 close -1]
λ(a[+l][+1 close -2])


(λa)[+1 close -1]
λ(a[+1][+1 close -2])

s1 λe1 = s2 λe2
lift s1 e1 = lift s2 e2

(λe1)
open t (λλ-2) = λλ-2


[noop; open +2; open +1]
lift s (-1) = -1
lift s (-(n + m)) =

-2[open +1][open +2]
-2[-l open +1]

[+1 close -2]

t[s][]
(λa)[+1 close -2]
λ(a[+l][close +1])

(λa)[close +1]
λ(a[+l][close +1])

lift(, 1) = 1
lift(open +n, n + 1) = shift(s(n))

lift(open +, 1) = 1
lift(open +n, n + 1) = shift(s(n))

lift(s, 1) = 1
lift(s, n + 1) = shift(s(n))

\-1[shift][open \+2]


2 [\+2; \+1] {} |- \+1[open \+1][close +1][open \+1]
2 [\+1; \+2; \+1] {} |- \+1[open \+1][close +1]
2 [\+1; \+2; \+1] { 1 : (2, 3, {}) } |- \+1[open \+1]
2 [\+1; \+1; \+2; \+1] { 1 : (2, 3, {}) } |- \+1
2 [\+1; \+2; \+1] {} |- \-1

2 [\+2; \+1] {} |- (λ\+1)[close +1][open \+1]
2 [\+1; \+2; \+1] {} |- (λ\+1)[close +1]
2 [\+1; \+2; \+1] { 1 : (2, 3, {}) } |- λ\+1
3 [\+2; \+1; \+2; \+1] { 1 : (2, 3, {}) } |- \+1
2 [\+1; \+2; \+1] {} |- \-1
2 [\+2; \+1] {} |- \+1

2 [\+1; \+2; \+1] { 1 : (3, {}) } |- \+1[open \+1]
2 [\+1; \+1; \+2; \+1] { 1 : (2, 3, {}) } |- \+1
2 [\+1; \+2; \+1] {} |- \-1


1 |- λ((\-1 \+2)[close +2])

(λ\+1)[close +1][open \+1]

(λ-1)[close +1] = (λ-1)[close +1]
(-1[+l][close +1][shift]) = (-1[+l][close +1][shift])

(λ+1)[close +1] ~~> λ-2

// colision inside of lambda related to open
(λa)[s]
λ(a[+l][])
closed[s][shift]


\-1[open \+l][shift]


1 2 => x y

1 |- (x => (x\+2 y\-2)[close +2 to -1])[open -1 to y\+1]
// entering a binder requires to shift all free negative variables
// the right side on open is locally closed, so only shift the left
// the left side on close is locally closed, so only shift the right
1 |- x => (x\+2 y\-2)[close +2 to -1][open -2 to y\+1]
1 |- x => (x\+2[close +2 to -1] y\-2[close +2 to -1])[open -2 to y\+1]
1 |- x => (x\-1 y\-2)[open -2 to y\+1]
1 |- x => (x\-1[open -2 to y\+1] y\-2[open -2 to y\+1])
1 |- x => x\-1 y\+1

// should definitely be true
(A : Type) -> \-1 -> \-2 `unify` (A : Type) -> _A -> _A
// solution, open and invoke a level in the context
2 |- (A : Type) -> \-1 -> \-2 `unify` (A : Type) -> _A -> _A
3 |- (\-1 -> \-2)[open -1 to \+3] `unify` (_A -> _A)[open -1 to \+3]
3 |- \+3 -> \+3 `unify` _A -> _A


// should ALWAYS be (y => x => x\-1 y\-2)
l |- (y => (x => x\-1 y\+2)[close +2 to -1])
// follow the steps and you will get to this
l + 2 |- (x\+(l + 2) y\+2)[close +2 to -2][open -2 to \+(l + 1)]
// what happens if l is 0?
2 |- x\+1 y\+1
// what happens if l is 1?
3 |- x\+3 y\+3

2 |- (x\+2 \+2)[open -2 to \+1][close +2 to -2]
2 |- (x\-2 y\-2)


l |- (y => (x => x\-1 y\+2)[close +2 to -1 at l + 1])
l + 1 |- (x => x\-1 y\+2)[close +2 to -1 at l + 1][open -1 to \+(l + 1)]
l + 2 |- (x\-1 y\+2)[open -1 to \+(l + 2)][close +2 to -2 at l + 1][open -2 to \+(l + 1)]
l + 2 |- (\+(l + 2) y\+2)[close +2 to -2 at l + 1][open -2 to \+(l + 1)]
// what happens if l is 0?
2 |- (\+2 y\+2)[close +2 to -2 at l + 1][open -2 to \+(l + 1)]

// should ALWAYS be (y => x => x\-1 y\-2)
l |- (y => (x => x\-1 y\+2)[close +2 to -1])
l + 1 |- (x => x\-1 y\+2)[close +2 to -1][open -1 to \+(l + 1)]
l + 2 |- (x\-1 y\+2)[open -1 to \+(l + 2)][close +2 to -2][open -2 to \+(l + 1)]
l + 2 |- (x\-1 y\+2)[open -1 to \+(l + 2)][close +2 to -2][open -2 to \+(l + 1)]

l |- (y => (x => x\-1 y\+2)[close +2 to -1])

// should ALWAYS be (y => x => x\-1 y\-2)
l |- (y => (x => x\-1 y\+1)[close +1 to -1])
l + 1 |- (x => x\-1 y\+1)[close +1 to -1][open -1 to \+(l + 1)]
l + 2 |- (x\-1 y\+1)[open -1 to \+(l + 2)][close +1 to -2][open -2 to \+(l + 1)]
l + 2 |- (x\-1 y\+(l + 1))[open -1 to \+(l + 2)][close +1 to -2][open -2 to \+(l + 1)]

// l = 0
2 |- (x\-1 y\+1)[open -1 to \+2][close +1 to -2][open -2 to \+1]
2 |- (x\+2 y\+1)

// l = 1
3 |- (x\-1 y\+1)[open -1 to \+3][close +1 to -2][open -2 to \+2]
3 |- (x\+3 y\+2)

// should ALWAYS be (y => x => x\-1 y\-2)
l |- (y => (x => x\-1 y\+2)[close +2 to -1])
l + 1 |- (x => x\-1 y\+2)[close +2 to -1][open -1 to \+(l + 1)]
l + 2 |- (x\-1 y\+2)[open -1 to \+(l + 2)][close +2 to -2][open -2 to \+(l + 1)]

// l = 0, leads to [close +2] at level 1, which is weird
// reminds me of escaping the scope
1 |- (x => x\-1 y\+2)[close +2 to -1][open -1 to \+1]



1 |- (x => x\-1 y\+2)[close +2 to -1][open -1 to \+1]

3 |- x\+3 y\+2


// should ALWAYS be (y => x => x\-1 y\-2), when l > 1
l |- (y => (x => x\-1 y\+2)[close +2 to -1])

(y => (x => x\-1 y\+2)[close +2 to -1])


l + 2 |- (x\-1 y\+(l + 1))[open -1 to \+(l + 2)][close +1 to -2][open -2 to \+(l + 1)]

(x\-1 y\+(l + 1))[open -1 to \+(l + 2)][close +1 to -2][open -2 to \+(l + 1)]
(x\+(l + 2) y\+l)

(x\+(l + 2) y\+(l + 1))


(x\+(l + 2) y\+(l + 1))[close +(l + 2) to -1]
(x\+(l + 2) y\+(l + 1))


l |- (y => (x => x\-1 y\+2)[close +l to -1])
l + 1 |- (x => x\-1 y\+2)[close +l to -1]

l |- (y => (x => x\-1 y\+2)[close +2 to -1])
l + 2 |- (x\+(l + 2) y\+(l + 1))

l + 2 |- (x\-1 y\+(l + 1))[open -1 to \+(l + 2)][close +1 to -2][open -2 to \+(l + 1)]


(x\+(l + 2) y\+(l + 1))[close +(l + 2) to -1]
(x\-1 y\+(l + 1))[open -2 to +]

//
l |- (y => (x => x\-1 y\+2)[close +2 to -1])
l + 2 |- (x\+(l + 2) y\+(l + 1))


l |- (y => (x => x\-1 y\+(l + 2))[close +(l + 2) to -1])
l + 1 |- (y => (x => x\-1 y\+(l + 2))[close +(l + 2) to -1])

//
l + 2 |- (\-1 \+2)[open -1 to \+(l + 2)][close +2 to -2][open -2 to \+(l + 1)]

l + 2 |- (\-1 \+2)[open -1 to \+(l + 2)][close +2 to -2][open -2 to \+(l + 1)]


l + 2 |- \+(l + 2) \+2[open -1 to \+(l + 2)]

//
l |- (y => (x => x\-1 y\+(l + 2))[close +(l + 2) to -1])
l + 1 |- (x => x\-1 y\+(l + 2))[close +(l + 2) to -1][open -1 to +(l + 1)]
l + 2 |- (x\-1 y\+(l + 2))[open -1 to +(l + 2)][close +(l + 2) to -2][open -2 to +(l + 1)]


0 |- (y => (x => x\-1 y\+1)[close +1 to -1])


0 |- (y => (x => x\-1 y\+1)[close +1 to -1])
l |- (y => (x => x\-1 y\+(l + 1))[close +(l + 1) to -1])

l |- (z = _; y => (x => x\-1 y\+(l + 1))[close +(l + 1) to -1])

(z = _; y => (x => x\-1 y\+1)[close +1 to -1])

0 [] [] {} |- (y => (x => x\-1 y\+1)[close +1 to -1])
1 [back] [open \+1] {} |- (x => x\-1 y\+1)[close +1 to -1]
1 [back; back] [open \+1] { +1 := ({}, -1 ~> \+1) } |- x => x\-1 y\+1
2 [] [open \+2; open \+1] { +1 := ({}, -1 ~> \+1) } |- x\-1 y\+1
2 [] [open \+2; open \+1] { +1 := ({}, -1 ~> \+1) } |- x\+2 y\+1

0 [] [] |- (y => (x => x\-1 y\+1)[close +1 to -1])
0 [] [] |- (y => (x => x\-1 y\+1)[close +1 to -1])


0 [] [] {} |- (y => (x => x\-1 y\+1)[close +1 to -1])
1 [back] [open \+1] {} |- (x => x\-1 y\+1)[close +1 to -1]
1 [back; back] [open \+1] { +1 := ({}, -1 ~> \+1) } |- x => x\-1 y\+1

(y => (x => x\-1 y\+2)[close +2 to -1])


(y => (x => x\-1 y\+1)[close +1 to -1])
(y => (x => (x\+2[close +2 to -1]) y\+1)[close +1 to -1])

(y => (x => x\-1 y\l+2)[close l+2 to -1])
(y => (x => x\-1 y\l+2)[close l+2 to -1])

[] [] |- (y => (x => x\-1 y\+1)[close +1 to -1])
[] [\+1] |- (x => x\-1 y\+1)[close +1 to -1]
[] [\+1] |- x => x\-1 y\+1
[] [\+2; \+1] |- x\-1 y\+1
  [y\+1] [\+2; \+1] |- x\+2
  [] [\+2; \+1] |- y\+1

[] [] |- λ(λ(\-1 \+1))[+1 := -1]
[] [\+1] |- (λ\-1 \+1)[+1 := -1]
[] [\+1] |- λ\-1 \+1
[] [\+2; \+1] |- \-1 \+1
  [\+1] [\+2; \+1] |- \-1 ~> \+2
  [] [\+2; \+1] |- \+1

[] [\+1] |- (x => x\-1 y\+1)[close +1 to -1]
[] [\+1] |- x => x\-1 y\+1
[] [\+2; \+1] |- x\-1 y\+1
  [y\+1] [\+2; \+1] |- x\+2
  [] [\+2; \+1] |- y\+1



[] |- _A[-1 := \+2] === +2


[] |- _A === +2[+2 := -1]

[] |- _A === \+1[+1 := \-1]
[] |- _A === \-1

l |- λ(_A λ(\-1 \+1))[+1 := -1]

0 |- (x = _; y => (x => x\-1 y\+2)[)


0 |- ((z => y => (x => x\-1 y\+2)[close +2 to -1]) _)

((z => y => (x => x\-1 y\+2)[close +2 to -1]) _)
(z => y => (x => x\-1 y\+2)[close +2 to -1])[open -1 to _]
(z => y => (x => x\-1 y\-2))

y => _A[close +2 to -1] `unify` x => x\-1
0 |- y => _A[close +2 to -1] `unify` x => x\-1


1 |- x => \+1[close +2 to -1]

(((1 z) => (2 y) => ((3 x) => x\-1 y\+2)[close +2 to -1]) _)
((2 y) => ((3 x) => x\-1 y\+2)[close +2 to -1])[open -1 to _]
((1 y) => ((2 x) => x\-1 y\+2[close +2 to -2][open -3 to _]))
(1 y) => ((2 x) => x\-1 y\-2)


(((1 z) => (2 y) => ((3 x) => x\-1 y\+2)[close +2 to -1]) _)
((2 y) => ((3 x) => x\-1 y\+2)[close +2 to -1])[open -1 to _]

y => _A[close +2 to -1] `unify` x => x\-1


l |- (A : Type) -> (_A -> _A)[close +(l + 1) to -1] `unify` (A : Type) -> A\-1 -> A\-2
l + 1 |- (_A[close +(l + 1) to -1] -> _A[close +(l + 1) to -2]) `unify` A\-1 -> A\-2
  l + 1 |- (_A[close +(l + 1) to -1]) `unify` A\-1
  l + 1 |- _A `unify`

  l + 1 |- A\-1[open -1 to \+(l + 1)][close +(l + 1) to -2] `unify` A\-2
  l + 1 |- A\-2 `unify` A\-2

A\-2[open -2 to \+(l + 1)][open -1 to \+(l + 1)]

A\-2[open \+(l + 1)][to \+(l + 2)]

(A : Type) -> (A\+1 -> A\+1)[from +1]

((A : Type) -> A\-1)[to \+2]

(A : Type) -> A\+1[from +1] -> A\+1[lift(from +1)]

(A : Type) -> A\+1[from +1] -> A\+1[lift(from +1)]

A\+1[from] = A\-1
A\+2[from] = A\+1

lift s \-(n + 1) = s \-n
lift s +n = s +n | \-m => \-(m + 1)

((B : Type) -> (A\-2 B\-1))[to f]
((B : Type) -> (A\-2 B\-1)[lift(to f)])
((B : Type) -> A\-2[lift(to f)] B\-1[lift(to f)])
((B : Type) -> A\-1[to f] B\-1)
((B : Type) -> f B\-1)

[to \-1; to a; to b]
[from; from; none]

l |- \+1

(B : Type) -> (A\-2[open f] B\-1)

[close; close; close] |- (A : Type) -> A\+4[close]
[close; close; close; close] |- A\+4 ~> A\-1


lift s +(n + 1) = s +n | \-m => \-(m + 1)

0 |- λ(λ+1)[close] ~~> λλ-2
0 |- λ(λ+1)[close]
     λλ+1[lift close]
     λλ+1[close][shift]
     λλ-2

lift s l \-(n + 1) = s +l \-n
lift s l +n = s +(l + 1) +n | \-m => \-(m + 1)

close l +l = \-1

l |- λ(λ+(l + 1))[close] ~~> λλ-2
l |- λ(λ+(l + 1))[close]
     λλ+(l + 1)[lift close]
     λλ-2


0 |- (x = _; (λ(λ+2)[close])[close]) ~~> λλ-2

1 |- λ(λ+2)[close] ~~> λ(λ+2[lift(close)])
1 |- λ(λ+2)[close] ~~> λ(λ+1[close])

λ(_A _A)[close] `unify` λ(+1 +1)[close]
λ(_A _A)[close] `unify` λ(+1 +1)[close]

((A : Type) -> A\+4[close])

A\+1[close] = A\-1
A\+2[close] = A\+2

A\+2[lift(close)] = A\-2
A\+3[lift(lift(close))] = A\-3

((B : Type) -> (A\-2 B\-1))[to f]


(λ(λ+2)[close])[close] ~~> λλ-2
λ(λ+2)[close][lift close]
λλ+2[lift close][lift (lift close)]

0 |- λ(λ+1)[close]
     λλ+1[lift close]
     λλ+1[close][shift]
     λλ-2

0 |- (λ(λ+2)[close])[close]
1 [] [close +1] |- λ(λ+2)[close]
1 [open \-1] [close +1] |- (λ+2)[close]
1 [open \-1] [close +1] |- (λ+2)[close]

λ.(λ.+2[close])[close]
λ.(λ.+1[lift close])
λ.(λ.-2)



\-1[lift s] = \-1
\-(n + 1)[lift s] = \-n[s][shift]

a[lift s]
// relevant property is that x doesn't appear in s
a[open x][s][close x]

-1[open x][s][close x] = x[s][close x] = x[close x] = -1
-2[open x][s][close x] = -1[s][close x] =
                         -1[close y][close x] = -1[close x] = -2
                         -1[open a][close x] = a[close x] = a

+y[open x][s][close x] = +y[s][close x]
+y[open _][close x] = +y
+y[close y][close x] = -1[close x] = -2

\-1[incr][-2 open a][decr] = \-2[open a][decr] = \-2[decr] = \-1
\-2[incr][open a][decr]

λ.(λ.-2)

+2[close][close] = \-1

[] [close] |- λ(λ+2)[close]
[free] [close] |- (λ+2)[close]
[free] [close; close] |- λ+2
[free; free] [close; close] |- +2


open t \-1 = t
open _ \-(n + 1) = \-n

close \+1 = \-1
close \+(n + 1) = \+n

lift _ \-1 = \-1
lift s \-(n + 1) = s \-n
lift s \+n = s +n | \-m => \-(m + 1)


λ.(λ.+2[close])[close]
λ.λ.+1[lift close]
λ.λ.-2

(λ.(λ.(λ.+2[close])[close])[close]) _
(λ.(λ.+2[close])[close])[close][open _]
λ.(λ.+2[close])[close][lift close][lift (open _)]
λ.(λ.-2)[lift close][lift (open _)]

(λ.(λ.(λ.+3[close])[close])[close]) _
(λ.(λ.+3[close])[close])[close][open _]


λ.(λ.+2[close])[close]
λ.(λ.+2[close][lift close])
λ.(λ.-2[lift close])
λ.(λ.-1)

λ.λ.(+2 +1)[close]

(λ+1.λ+2.+3) a

(λ.+2[close +2])
```

## Teika

```rust
Button
  : () -[Database]> React.Component
  = () => {
  const x = get();
  return (<div> {x} </div>)
};
```

## Joshua's

```markdown
## Chapter {{pinned}}

### Abc

BBBBB

### Def

AAAAA

#### Def concepts

... ai puts here
```

## AI optimizer

### Valid Transformations

```rust
pred (succ x)

- max term size
- locality / proxy accumulate de-bruijin
- linearity / variable grading
- total number of reduction



(x => M) N
M[x := N]

(x => x x) N(10)
N N

```

## Autocomplete, Code quality suggestion

Github find all PR / patches, trim many features, rank developers,

```markdown

```

## De-bruijin needs data

```rust
(λ.(λ.(λ.+1))[close +1]) a
(λ.(λ.+1))[close +1][open a]
λ.(λ.+1)[lift (close +1)][lift (open a)]
λ.λ.+1[lift (lift (close +1))][lift (lift (open a))]
λ.λ.-3[lift (lift (open a))]
λ.λ.-1[open a]
λ.λ.a


(λ.(λ.(λ.+3[close +3]))) a
(λ.(λ.+3[close +3]))[open a]
λ.(λ.+3[close +3])[lift (open a)]
λ.λ.+3[close +3][lift (lift (open a))]
λ.λ.-1[lift (lift (open a))]
λ.λ.-1

0 |- (λ.(λ.(λ.+3[close +3]))) a
0 |- (λ.(λ.+3[close +3]))[open a]
[] [] |- (λ.(λ.+3[close +3]))[open a]

0 [] [] |- (λ.(λ.(λ.+3))) a
0 [] [] |- (λ.(λ.+3))[a]
1 [a] [1] |- λ.(λ.+3)
2 [a; +2] [1; 2] |- λ.+3
3 [a; +2; +3] [1; 2; 3] |- +3

λ(1).+1 +1
λ(1)._A _A


(x = _; y = λ(2).+2 +2; +3)

(λ(1).(λ(2).+2) (λ(2).+2 +2)) _
((λ(2).+3) (λ(2).+2 +2))[1 | _]
((λ(2).+3) (λ(2).+2 +2))[1 | _]
(+2[2 | λ(2).+2 +2])[1 | _]
+2[2 | λ(2).+2 +2][2 | _]
+2[2 | λ(2).+2 +2][2 | _]


(λ(1).λ(2).+2 +2) _
∀(2).+2 +2
[-1]  |- (λ.(λ.+3))[open a]

(λ(1).(λ(2).(λ(3).+3))) a

0 [] [] |- (λ(2).(λ(3).+3))[a]
1 [a] [a] |- λ(2).λ(3).+3
2 [a; +2] [1; 2] |- λ(3).+3
3 [a; +2; +3] [1; 2; 3] |- +3
(λ(2).(λ(3).+3))[open a]



v <= l
-------
l |- +v


v |- b
------------
l |- λ(v). b




[] |- (λ(3).(λ(4).+2))[2 | a]


[] |- (λ(3).(λ(4).+2))[2 | a]

(λ(1).+1 +1) (λ(2).+2)
(λ(2).+2 +2)
(+2 +2)[2 := λ(1).+1]
(λ(1).+1) (λ(1).+1)
+1[1 := λ(1).+1]
λ(1).+1

x = _;
(λ(1).+1 _A)


[] |- λ(1).+1 === λ(2).+2
[] |- λ(1).+1 === [] |- λ(2).+2
[-1] |- +1 === [] |- λ(2).+2


λx.a `unify` λy.b
a[x := z] `unify` b[y := z]
_A `unify` b[y := x]


[] |- λ.+1[close +2] === [] |- λ.+2[close +2]

[] |- λ.+1[close +1] === [] |- λ.+2[close +2]
[] |- +1[close +1] === [] |- +2[close +2]
[close +1] |- +1 === [close +2] |- +2
[close +1] |- -1 === [close +2] |- -2

[] |- λ.(+1 (λ.+1))[close +1] === [] |- λ.(+3 (λ.+3))[close +3]
[] |- λ.(_A (λ._A))[close +1] === [] |- λ.(+3 (λ.+3))[close +3]
[] |- λ.(_A (λ._A))[close +1] === [] |- λ.(+3 (λ.-2))[close +3]
[] |- (_A (λ._A))[close +1] === [] |- (+3 (λ.-2))[close +3]
[close +1] |- _A (λ._A) === [close +3] |- +3 (λ.-2)
  [close +1] |- _A === [close +3] |- +3
  _A := +3[close +3][open \+1]
[close +1] |- λ.+3[close +3][open \+1] === [close +3] |- λ.-2
[lift (close +1)] |- +3[close +3][open \+1] === [lift (close +3)] |- -2
[close +3; open \+1; lift (close +1)] |- +3 === [lift (close +3)] |- -2
[open \+1; lift (close +1)] |- -1 === [lift (close +3)] |- -2
[lift (close +1)] |- +1 === [lift (close +3)] |- -2
[] |- -2 === [lift (close +3)] |- -2

[lift [close +1]] |- +2[close +2][open \+1] === [lift [close +2]] |- +2
[close +2; open \+1; close +1] |- +2 === [close +2] |- +2


[] |- λ.(_A (λ._A))[close +1] === [] |- λ.(-1 (λ.+3))[close +3]
[] |- (_A (λ._A))[close +1] === [] |- (-1 (λ.+3))[close +3]
[close +1] |- _A (λ._A) === [close +3] |- -1 (λ.+3)
  [close +1] |- _A === [close +3] |- -1
  _A := -1[close +3][open \+1]

[close +1] |- λ.-1[close +3][open \+1] === [close +3] |- (λ.+3)
[lift (close +1)] |- -1[close +3][open \+1] === [lift (close +3)] |- +3
[close +3; open \+1; lift (close +1)] |- -1 === [lift (close +3)] |- +3
[] |- -2 === [] |- -2


line 15, col 2

> f expr expr2
    ^  ^

Message

expected : Option Int
received : Option String
```

## Dynamic

```rust
[@mode dynamic]

l => l.map(x => x + 1);
l => List.map(l, x => x + 1);

f = x => print(cast x);
```

## Working

```rust

Id = (A : Type) -> A;
x : Id Int = _;
(x : _A);

[] |- _A === Id Int

expected : \-1[(open \+2) :: lift (open \+1)]
received : \-2[(open \+3) :: lift ((open \+2) :: lift (open \+1))]

expected : \-0[(open \+4) :: lift ((open \+3) :: lift ((open \+2) :: lift (open \+1)))]
received : \-0[(open \+3) :: lift ((open \+2) :: lift (open \+1))]

expected : \-0[(open \+4) :: lift ((open \+3) :: lift ((open \+2) :: lift (open \+1)))]
received : \-0[(open \+3) :: lift ((open \+2) :: lift (open \+1))]

expected : ((x : ) -> \-1)[(open \+3) :: lift ((open \+2) :: lift (open \+1))]
received : ((x : ) -> \-1)[(open \+2) :: lift (open \+1)][(open \+3) :: lift ((open \+2) :: lift (open \+1))]

annot : ((A : \+1) -> (x : \-0[(open \+4) :: lift ((open \+3) :: lift ((open \+2) :: lift (open \+1)))]) -> \-1)[(open \+3) :: lift ((open \+2) :: lift (open \+1))]
```

## Effect

```rust
x : IO(Result(String, Error))
x : Result(IO(String), Error)

Eff l r = Raw_eff (sort_uniqe l) r;

x : Eff [IO; Result] String
x : Eff [Result; IO] String
x : Eff [Random] String;

() = format();

(x : Nat 1) => x + x;

Socket : {
  connect : () -> $Socket;
  close : $Socket -> ();
};
() => (
  x = Socket.connect();
  Socket.close(x);
);

x => y => x;
x => y => y;

r0 : Linear
r1 : Linear

(three, r1) = add(r0, r1);

Array : {
  $Array : (A : Type) -> Type;

  make : () -> $Array Int;
  get : {A} -> (i : Nat, arr : Array A) -> (Array A, Option A);

  Read_only_array : _;

  split : (i : Nat, arr : Array A) -> (List (Read_only_array A i arr))
  merge : (l : List (Read_only_array A i arr)) -> Array A;
} = _;

f = arr0 => (
  (arr1, x) = Array.get(0, arr0);
  (arr2, y) = Array.get(1, arr1);

)
find : () -> Eff [Database] (List User);

integration()

@debug(find())
```

## Unification

```rust
(A : _) -> _B[close +3] === (A : _) -> \-1
(A : _) -> _B[close +3] === (A : _) -> \-1
_B === \-1[open \+3]


(A : Type) -> A\+2 === (A : Type) -> A\+2


(A : _) -> (_C ((B : _) -> _C))[close +2] === (A : _) -> (_D ((B : _) -> \-2))
(_C ((B : _) -> _C))[close +2] === (_D ((B : _) -> \-2))
((B : _) -> _C)[close +2] === (B : _) -> \-2
_C[lift (close +2)] === \-2
_C === \-2[lift (open \+2)]
_C === \-1

_C[shift] === \-2
_C === \-2[lower]

_A[lift s] === \-2
_A === \-2[lift (not s)]

_A[lift (close +n)] === \-2
_A === \-2[lift (open \+n)]
\-2[lift (open \+n)][lift (close +n)] === \-2
\+n[lift (close +n)] === \-2
\-2 === \-2

_A[lift (open +n)] === \+n
_A === \+n[lift (close +n)]
\+n[lift (close +n)][lift (open +n)] === \+n[lift (close +n)]
\-2[lift (open +n)] === \+n[lift (close +n)]
\-2[lift (open +n)] === \+n[lift (close +n)]


_A[lift (close +n)] === \+n

_A[shift] === \-2
_A[shift] === \-2

_A === \-2[1 . id]


(A : Type) -> _A
l := t

expected : ((x : \+1[id]) -> \+1[close +3])[id]
received : ((_x0 : \+3[id]) -> \+3[id])[id][lift id][open \+3][+3 := \+1][id]

_A[close \+2][open \+3] === +2
_A === +2
_A[close \+2][open \+3] === +2

_A === -1

_A === +1[open +2]

_A[x := N] === x
x === x


_A[+2] ===

x[x := N] === x

(λλ_A[lift (1 1)] === λλ2)
(λ_A[lift (1 1)] === λ2)
(_A[lift (1 1)] === 2)

(x => _A) N === y


_A[x := N] === x

Γ,Δ1 |- A    Γ,Δ2 |- B
----------------------
    Γ |- A === B


_A[open +2][close +2] = -1
_A = -1[open +2][close +2]

Γ |- e : A  Γ |- r[x := e] : B
------------------------------
    Γ |- x = e; r : B

(Id = (A : Type) => A; )

m.n
_::n(m)

m.n a b
_::n
```

## Magic Dot

```rust
Nat = (A : Type) -> (z : A) -> (s : (x : A) -> A) -> A;
(zero : Nat) = A => z => s => z;
(succ : (n : Nat) -> Nat) = n => A => z => s => s (n A z s);
(add : (n : Nat) -> (m : Nat) -> Nat) = n => m => n Nat m succ;
(mul : (n : Nat, m : Nat) -> Nat) = n => m => n Nat zero (add m);
one = succ zero;
two = succ one;
four = mul two two;
eight = mul two four;
sixteen = mul two eight;
byte = mul sixteen sixteen;
byte String "zero" (_ => @native("debug")("hello"))


x.f === _::f(x)
x.f (1, 2) === _::f(x)(1, 2)


add = (!n : Nat, !m : Nat) => n + m;
id = {A} => (x : A) => x;

add = (!n : Nat, !m : Nat, log : Option Bool) => n + m;

x = add (n = 1, m = 2);

(1, 2) : (_A : Nat, _B : Nat);

(_A : Nat, _B : Nat) === (n : Nat, m : Nat)

(_A : Nat, _B : Nat) === (!n : Nat, !m : Nat)

(x = 1, y = 2) : (x : Nat, y : Nat);


Int : {
  @Int : Type;
  show : (i : @Int) -> () -> String;
};

ind_bool : (b : Bool) -> (P : Bool -> Type) -> P true -> P false -> P b;
ind_bool : (b : Bool (i + 1)) ->
  (P : Bool i -> Type) -> P true -> P false -> P (lower b);

Consistency = Soundness + Strong Normalization;

(Type 0 : Type 1)
(Type n : Type (n + 1))

id = (A : Type 1) => (x : A) => x;

id (Type 1)


ind_bool : (b : Bool) -> (P : Bool -> Type) -> P true -> P false -> P b = _;


Γ, x : @self(x). T |- T : Type
------------------------------
Γ |- @self(x). T : Type


Γ, x : @self(x). T |- M : T[x := @fix(x). M]
--------------------------------------------
Γ |- @fix(x). M : @self(x). T


Γ |- M : @self(x). T
--------------------------
Γ |- @unroll M : T[x := M]
```

## TCO Nat

```rust

two = z => s => s (s z);
two = z => s => s z (x => s x);
three = z => s => s z (x => s x s)
```

## Prevent Currying

```rust
id = (x : 'A) => x;

((x : 'A) -> 'A)

id : (x : 'A) -> 'A;
sum : (x : Int, y : Int) -> Int;

sum : (f : (x : Int, y : Int)) -> Int;

id (1);
sum 1 2;

sum (1, 2);
(x = (1, 2); sum(...x))
(sum((1, 2)))

(x, y) = id(_ = (1, 2));

(x, y) =>

ind : (b : Bool, P : Nat -> Type) -> (x : P true, y : P false) -> P b;
sum : (x : Int) -> (y : Int) -> Int;

sum 1 _


?sum 1 _
y => ?sum 1 _

id(1, 2) : (1, 2)


add a b = c;

// named return
(x : Int) -> (x : Int)
```

## Nominal

```rust

(Bool : Type) -> (true : Bool) -> (false : Bool) ->w

(Bool : Type) & {
  true : Bool;
  false : Bool;
  case : {A} -> (pred : Bool, then : A, else : A) -> A;

} = _;

add 1 ~> add 1
add 1 2 ~> 3

Γ |- A : Type
--------------------------
Γ |- @frozen(A) : Type

Γ |- M : A
--------------------------
Γ |- @freeze M : @frozen A

Γ |- M : @frozen A
--------------------
Γ |- @unfreeze M : A

---------------------------
@unfreeze (@frozen M) === M

T =
  (R : Type) ->
  ((A : Type) ->
    (x : A) ->
    (to_string : A -> String) ->
  R) -> R;

M : T = _;

to_string : ((A : Type) -> M ) -> String;

M : {
  Nat : Type;
  one : Nat;
  to_int : Nat -> Int
} = @nominal(key => (
  Nat = @frozen(key, Int);
  one = @freeze(key, 1);
  to_int = x => @unfreeze(key, x);
));

T = @nominal(key => (
  Nat = @frozen(key, Int);
  one = @freeze(key, 1);
)).Nat;

M.one
```

## Primitives

```rust

```

## External

```rust

```

## Linear Machine

```rust

(2 x : Int) => x + x
```

## Escape

```rust
call v f = f v
(A : Type) -> A


mono = w => x => y => (id => w (k x) (k y)) (x => x);
(a : _A\+1) => (A : Type) => (b : _B\+2) =>




(x => x) 1

((2 x : _A & _B) => (x : _A) + (x : _B))

(x0 : Int) => (x1 : Int) => x0 + x1


Nat (G : Grade) = (A : Type) -> (1 z : A) -> (G s : (1 A) -> A) -> A;
(x : Nat) => x Nat zero (acc => acc);

|- Type : Type


Γ |- e : A : Type

(('G + 1) x : @rec T. ('G x: T) -> _) => x x
(x : @rec T. (x : T) -> _) => x x

((x : A) => x) : ((x : A) -> A) : Type



Γ |- e : A : Type

x = Type;



-----------------
ctx, x : A |- x : A


ctx |- func : A -> B    ctx |- arg : A
----------------------------------
ctx |- func arg : B

-----------------------------------------
ctx |- (x => body) arg === body[x := arg]


----------------------------------------
ctx | setState |- setState(false) === ()


My_button = (label : String) => <Button>{label}</Button>;


My_button label

My_button =
  ctx |- label : String
  --------------------------------
  ctx |- <Button>{label}</Button>


ctx |- My_button : String -> DOM    ctx |- label : String
---------------------------------------------------------
ctx |- My_button label === <Button>{label}</Button>: DOM


ctx |- label : String
-------------------------------
ctx |- <Button>{label}</Button>


My_button = (label : String) => <Button>{label}</Button>;

(A : Type) -> _A -> _B === (A : Type) -> _A -> _B

(A : Type) -> _A[close +2] === (A : Type) -> A\-1


_A[open \+2 \+2][close +2] === -1



_A[close +2] === +2


+2 |- (λ_A[close +1]) +2 === _B
+2 |- _A[close +1][open +2] === _B
+2 |- _A[close +1] === _B[close +2]
+2 |- _A === _B[close +2][open +1]

+2 |- _A === _B[close +2][open +1]


+2 |- (λ_A) +2 === _B
+2 |- _A[open +2] === _B
+2 |- _A === _B[close +2]



_B[close +2][open +1] === +2
_B === +2[open +2]

_B[close +1] === +2[open +2]

_B[close +2] === +2


_B[close +2] === +2
_B[open +2] === +2

[y] |- (x => _A) y === _B
[y] |- _A[x := y] === _B
[y] |- _A === _B[y := x]



unify = a => b => (k => (_ = k a); k b) (x => x);

P => y => z => (f : (x : _) -> P _A) => (g : P _B) =>
  unify (f y : P (_A[x := y])) g

(P (_A[x := y]) : P _B)



a : P (_B[y := x])
a : P y

_B[y := x] == y

_A[x := y] == _B

_A[x := y] == _B[y := x]


P => y => z => (f : (x : _) -> P _A) => (
  _ : P _B = (f y : P (_A[x := y]));
  // _A === _B[y := x]
  (f z : P y)
);

P => y => z => (f : (x : _) -> P _A) => (
  _ : P _B = (f y : (P _A)[x := y]);
  // _A === _B[y := x]
  (f z : P y)
);

(P _A)[x := y] === P y

P (_B[y := x][x := z]) === P y

_B[y := x][x := z] === y

// I think this cannot happen
P => y => z => (f : (x : _) -> P _A) => (
  _ : P _B = (f y : P (_A[x := y]));
  // _A === _B[y := x]
  (f z : P y)
);
_B[y := x] == y


_B[y := x] == y


_B[close +2] === +2


_A[x := y] == y
_A[x := y][y := x] == y[y := x]
_A == x


l |- _A == r |- _B


_A := _C[!r]
_B := _C[!l]


l |- _C[!r] == r |- _C[!l]
l . !r |- _C == r . !l |- _C

_C == _C

_C[l][!r] == _C[r][!l]

_C[l][l] == _C[r][r]
_C[l][!r][l] == _C[r]

_C

x[close +3][open +3][close +3]

Γ, x : Int |- xy : Int

(f => y => ) (x => x)

((x => x) y)[y := z]
(x => x[y := z]) y[y := z]
(x => x) z

(x = y; x)[y := z]
(x = y[y := z]; x[y := z])
(x = z; x)


(x = y; x[y := z])
(x = y; x[y := z])
x[y := z][x := y]

(x = y; equal x y)
(equal x y)[x := y]

x === y

one = s => z => s z;
two = s2 => s1 => z => s2 (s1 z);

Nat 'n = (A : Type) -> ('n f : A -> A) -> A -> A;

Bool (n : Grade) =
  (A : Type) -> (B : Type) -> (A -> B) -> A -> B

true = x => y => x y;
false = x => y => y;

(A : Type)

Type n


(x : Int 'n) -> (y : Int 'n) -> Int 'n

Nat =
  union
    (case "z" end)
    (case "s" (cell self) end)
  close;

Bool =
  | (tag : #true)
  | (tag : #false);

Either A B =
  | (tag : "left", payload : A)
  | (tag : "right", payload : B);

| ("left", a) => _
| ("right", b) => _

Either A B = (tag : Bool, payload : tag Type A B);

| (true, a) => _
| (false, b) => _

Either A B =
  | True(payload : A)
  | False(payload : B);

| True(a) =>
| False(b) => _

data%Bool =
  | true
  | false;

x = #true;

Nat =
  | (tag : "z")
  | (tag : "s", pred : Nat);

"z" : Nat
("s", ("z", ()))
Nat =
  | Z | S (n : Nat);

1 + 2 : Int _a
1 : Int _n
((A : Type 0) => (x : A 1) => x) : Type 15;

```

## Teika Schema

```rust
NONCE, auto retry
VERSION
TRANSACTION
LOGIN
PERMISSION
SECURITY


WEBSOCKET
STATEFUL APIs
```

## Teika Database

```rust
User and Post
Both indexed by User.id
```

## Teika Workers

```rust
{

}
```

## Teika System

Workers + Database

```rust
storage@User = {
  @index id : @increment Nat;
  name : String;
};

@Routes = [
  POST "/users" =
    { name : String; } => User.insert { name = name; };
  GET "/users/:id" =
    { id : Nat; } => User.find_by_id id;
  GET "/users" =
    () => User.list ();
];
```

## First-class Grades

```rust

Fix n = (Fix 1, (f : Fix 1) -[1]> Unit);
(f : Fix 1) => (
  (x, k) = f;
  k x;
)


Fix (G + 1) = (x : Fix G) -[1]> Unit;
(x : Fix 1) => (
  x x
)

Nat = (G : Grade, (A : Type) -> (1 z : A) -> (G s : A -> A) -> A);

f = (G : Grade) => Int G

T = Int 1;

((A : Type) -> A);

Type 0 : (G : Grade) -> Type 1 G




Γ |- n : Grade  Γ |- A : Type
-----------------------------
Γ, x $ n : A

Type : Type $ 0;

Γ |- A : Type  Γ |- n : Grade
-----------------------------
Γ |- M : A $ n

Γ, x : A $ n |- B : Type $ 0
------------------------------------
Γ |- ((x : A $ n) -> B $ m) : Type $ 0

Γ, x : A $ n |- M : B $ m
--------------------------------------------------
Γ |- (x : A $ n) => M : ((x : A $ n) -> B $ m) $ k

Γ |- M : ((x : A $ m) -> B $ n) $ 1  Γ |- N : $ n
--------------------------------------------------
Γ |- M N : ((x : A $ n) -> B $ m) $ k

(x : A $ 1) => x : A
Type $ 0 : Type

(x : Socket$) => _;


Id
  : (A : Type $ 0) => Type $ 0;
  = (A : Type $ 0) => A;
Never = (A : Type) ->
id = (A $ 0 : Type) => (x $ G : A) => x
id = ({A} => (x $ 1 : A) => x) $ 15;

A $ G -> A $ G
id
  : (A : Type $ 0) -> (x : A $ 'X) -> (A $ 'X) $ _G
  = (A : Type) => (x : A) => x;

(x : Int $ 5) => (
  incr () = x + 1;

  incr : () -> (Int $ _X) $ _Y where _X * _Y = 5

  // but, this seems as powerful
  incr : () -> (Int $ 1) $ 5
  // and more general than, but maybe this could put it back
  incr : () -> (Int $ 5) $ 1
);
Nat = (G : Grade, (A $ 0 : Type) -> ($ 1 : A) -> ($ G : ($ 1 : A) -> A) -> A);


```
