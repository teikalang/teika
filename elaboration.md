```rust
Grade, G, H ::=
  | 0 | 1 | G + H | G * H
Type, A, B, C ::=
  | Type | Grade | Π(a : A) -> B
Term, M, N, K ::=
  |

Γ |- M : Π(A, B)  Γ |- N : A
----------------------------
Γ |- M(N) : B(N)

B(_A) ⇐ T

Γ | x.0 |- M : A
Γ, x.0 == M | x.1 |- N ⇐ B(M)
-------------------------------
Γ | x |- [M, N] ⇐ [a : A, B(a)]

Γ | p.0 |- M : A
Γ, p.0 == (fix(x) => M{p := [x, p.1]}).@ | p.1 |- N ⇐ B(M)
--------------------------------------------------------
Γ | x |- [M, N] ⇐ [a : A, B(a)]



T = fix(T) : Type => () -> T.@;
U = fix(U) : Type => () -> U.@;

() -> T == () -> U
T.@ == U.@
Γ |-



P : [Unit : Type, Unit];
P = [
  self(u) -> (C : (u : Unit) -> Type, r : C(P.@.1)) -> C(u),
  fix(u) => (C, r) => r,
];

Γ | [S, _] |- M : [a : A, _B(a)]
---------------------------------
Γ | S |- M.0 : A

Γ | [_, S] |- M : [a == M.0, T]
-------------------------------
Γ | S |- M.1 ⇐ T

Γ, a : A | S(a) |- K ⇐ B(a)
-----------------------------------
Γ | S |- (a) => K ⇐ (a : A) -> B(a)

Γ | (a == N) => S |- M ⇐ (a == N) -> T
--------------------------------------
Γ | S |- M(N) ⇐ T



(M : S) :> T -| coerce(M)


((f : (a : A) -> S)(N) : T)
// normally
coerce(f(N))
// but with that propagation
((a) => coerce(f(a)))(N)


P : [a : A, B(a)];
P = [M, N];
x = P.0;
// same as
x : A;
x = [M, N].0;

//
P : [Unit : Type, Unit];
P = [
  self(u) -> (C, r : C(P.1)) -> C(u),
  fix(u) => (C, r) => r;
];
//
Unit : Type;
Unit =
  unit : Unit = fix(u) => (C, r) => r;
  self(u) -> (C, r : C(unit)) -> C(u);



C_Pair(A, B) : Type;
C_Pair(A, B) =
  c_exist(a : A, b : B(a)) : C_Pair(A, B) =
    (C, c) => c(a, b);
  self(s) -> (
    C : (c : C_Pair(A, B)) -> Type,
    c : (a : A, b : B(a)) -> C(c_exist(a, b))
  ) -> C(s)




T(A) = [A == A, f : (a : A) -> []];
U = [A : Type, f : (x : T(A)) -> []];
V = T(U);

// is V <: U
U = [A : Type, f : (x : [A == A, f : (a : A) -> []]) -> []];
V = [A : Type == U, f : (a : A) -> []];
V <: U

[A == U, f : (a : A) -> []] <: U
[A == U, f : (a : A) -> []] <: [A : Type, f : (x : [A == A, f : (a : A) -> []]) -> []]
(a : U) -> [] <: (x : [A == U, f : (a : A) -> []]) -> []
[A == U, f : (a : A) -> []] <: U


T(A @ 1) = [A @ 1 == A, f : (a : A) -> []];
U @ 2 = [A : Type @ 1, f : (x : T(A)) -> []];
V = T(U);

V <: U
[A : Type @ 1, f : (x : [A @ 1 == A, f : (a : A) -> []]) -> []]
<: [A @ 1 == U, f : (a : A) -> []]

(x : [A @ 1 == U, f : (a : A) -> []]) -> []
<: (a : A) -> []

V <: U
[A == U, f : (a : A) -> []] <: [A : Type, f : (x : T(A)) -> []]
(a : U) -> [] <: (x : T(U)) -> []
T(U) <: U

type T = {
  type A;
  f : A -> unit
}

type U = {
  type A;
  f : (T where type A = A) -> unit
}

type V = T where type A = U

[T : Type, f : (a : A) -> T]

[ι = M] == f(x) : [ι : T]
M == ()


[ι = M] == VO
M(VO) == VO.@

A = [ι = (A) => () -> A];

A == () -> A
[ι = (A) => () -> A] == () -> A
() -> A == A



_A + 1

_A(x) == M
_A == (x) => M

M([ι = M])


Γ |- M : A
------------------
Γ * G |- M : A $ G


x : T ! G |- x ⇐ _A


((x : _B) : _A)

x : T ! |- x : T ! #grade(_A) * #grade(_B)

(a : A $ G) $ G |- a : A

Γ $ G * H |- M ⇐ A
------------------
Γ $ H |- M ⇐ A $ G

_A $ _T

// builtin
(A : Type) $ (G : Grade) // many
// other formers are ungraded
Π(a : A) -> B(a)
Σ[a : A, B]

// alternatively just graded formers
Π(a : A $ G) -> B(a)
Σ[a : A $ G, B]

// trivially, if you have sigma
Many(A : Type, G : Grade) = [a : A $ G, []];

// lambda encodings
Many(A : Type, G : Grade) : Type;
many[A, G](a : A $ G) : Many(A, G);

Many(A, G) =
  self(m) -> (
    C : (m : Many(A, G)) -> Type,
    c : (a : A $ G) -> C(m)
  ) -> C(m);
many[A, G](a : A $ G) : Many(A, G) = (C, c) => c(a);


Γ / G |- M ⇐ A
--------------
Γ |- M ⇐ A $ G

_A[l]
_A := λ. _B[1 + l]

_A
_A == (x) => _B[x];

(a := f(x)) == (b := f(x))


f(x)(y) == f(x)(y)
f(x) == f(x)

f[_(x), _(y)] == f[_(x), _(y)]
f == f
x == x
y == y
_(x) == _(x)


_A(N) == _B[N]

_A(y) == N{y := x}{x := y}

_B[x := y] == N
_B == N[y := x]

_A[]

_A + L¹ == R¹
_A + L² == R²

_A + L¹ == R¹
L¹ + R² == R¹ + L²

_A * G
_A[G > 0 && ]

_A[>= M] == G
_A[>= g + _B] == _C + h

_A[>= M] + K == _B[>= M + K]
_A[>= M] == _B[>= M + K] - K



_A[>= M] * N == _B[>= M * N]
_A[>= M] == _B[>= M * N] / N

(_A + N) as _B

(Aˡ <= _A <= Aʳ) + N |->

_A := _B - N

(Bˡ <= _B <= Bʳ)

(Aˡ <= _A <= Aʳ) == (Bˡ <= _B <= Bʳ)

_A := _C
_B := _C
(max(Aˡ, Bˡ) >= _C <= min(Aʳ, Bʳ))

(Aˡ >= _A <= Aʳ) + (Bˡ >= _B <= Bʳ) == 1
_A == 1 - _B[<= 1]


1 - _B[<= 1] == 1
0 == _B[<= 1]

// _C is fresh
_A == 1 - _C[<= 1]
_B == 1 - _C[<= 1]

_A == _C
_B ==

_A[>= M] * _B[>= N] ==

_A[>= M] == _B[>= M * N] / N

_A[>= M] * 0 == 0

2 * _A == x
2 * _A == x / 2

_A[>= M] * 0 == 0

_B[>= M * N] * N / N

K + (M * N) == _

_A

_B == K + (M * N)

_A[>= M] == _B[>= M * N] / N

_B[>= M * N] - M * N


_A[>= M] == _B[>= M + K] - K

_B == A + M + K

_A[>= M] + K |-> _B[>= M]
_A == _B[>= M] - M

_A + N == 0
_B[>= M] - M + N == 0
_B[>= M] + N == M
_B[>= M] == M - N

Γ |- M : Grade  Γ |- N : Grade  Γ |- M >= N
-------------------------------------------
Γ |- M - N : Grade


(A : Type $ _A) =>
  T $ 1 = (a : A) -> A;
  U $ 1 = _;
  [refl() : T == U, T, U]


_A[l](x)
_A == (y) => L_B[1 + l](y)

self(x) -> T == (n : Nat) -> T
fix(x) => K == (n : Nat) => K
(M : self(x) -> T(x)).@ : T(M) == ??


Fix(α, A, f, f_I, f_J) : Type;
Fix(α, A, f, f_I, f_J) = [
  a : A,
  p : (β < α) -> Fix(β, f(a) == a, c_f(f), _, _)
];
mod(α)



I_Unit(A, u, s) : Type =
  (C : (a : A) -> Type, u : C(u)) -> C(s);

K_Unit(α, A, u, s) : Type;
K_Unit(α, A, u, s) =
  self(k) -> [u, k_unit] == [s, k];

K_Unit(α, A, u, s) : Type;
K_Unit(α, A, u, s) = [
  i : u == s,
  p : (β < α) -> K_Unit(β)(
    [s : A, i : u == s],
    [u, refl()],
    [s, i]
  )
];
k_unit(α, A, u) : K_Unit(α, A, u, u);
k_unit(α, A, u) = [refl(), (β < α) => k_unit(β, _, _)];

k_unit_mod(α, A, u, k : (α) -> K_Unit(α, A, u, u)) :
  k_unit(α, A, u) == k(α);
k_unit_mod(α, A, u, k) = [
  k(α).i,
  (β < α) =>
    K_Unit(β, )
  _ :
];
  k == k_unit(α, A, u);

I_Bool(A, t, f, b) : Type =
  (C : (a : A) -> Type, t : C(t), f : C(f)) -> C(b);

K_Bool(α, A, t, f, b) : Type;
K_Bool(α, A, t, f, b) = [
  i : I_Bool(A, t, f, b),
  p : (β < α) -> K_Bool(β)(
    I_Bool(A, t, f, b),
    i(I_Bool(A, t, f), i_true, _),
    i(I_Bool(A, t, f), i_true, _),
    i
  )
]
k_true(α, A, t, f) : K_Bool(α, A, t, f, t);
k_true(α, A, t, f) = [(C, t, f) => t, (β < α) => k_true(β, _, _, _)];

k_true_mod(α, A, t, f, k : K_Bool(α, A, t, f, t));

Bool = [c : C_Bool, i : (α) -> K_Bool(α, C_Bool, c_true, c_false, c)];



P : [Unit : Type, unit : Unit];
P = [
  (C, r : C(P.unit)) -> C(P.unit),
  (C, r) => r,
];

P : [Unit(α) : Type, unit(α) : Unit(α)];
P = [
  (α) => self(u) -> (
    C : (u : (β < α) -> P.Unit(α)) -> Type,
    r : C(P.unit)
  ) -> C(u),
  (α) => fix(u) => (C, r) => r,
];

Unit : Type = (α) -> P(1 + α).Unit;
unit : Unit = (α) => P(1 + α).unit;

unit(1)((u : (β < 1) -> P(β).Unit) =>
  u(0) : P()
  ) : (C : r)

Unit : Type = [α, P(α).Unit];
unit : Unit = [0, P(0).unit];


unit : (α) -> P(1 + α).Unit

Γ, x : A |-
------------------------------
Γ | A |- self(x, α) -> B(x, α)

fix(P :) =>

Γ, x |-
-------------------------------------------
Γ |- fix(x, α) => K ⇐ self(x, α) -> T(x, α)

∞
self(x) -> T == (n : Nat) -> T
fix(x) => K == (n : Nat) => K
(M : self(x) -> T(x)).@ : T(M) == ??

fix[T](f : (n, k : (m, p : n == 1 + m) -> T(m)) -> T(n), n) : T(n) =
  ind(
    n,
    T,
    f(0, (m, p : 0 == 1 + m) => succ_neq_zero(p)),
    (n, IH) =>
      f(1 + n, (m, p : 1 + n == 1 + m) => coe(succ_inj(p), T, IH))
  )(f);


_A == _B $ _G

(a : _A) => (a : T $ 5)
_A == _B $ _G

_B $ _G == T $ 5

#match_pi(_A)
#match_pi(_T $ _G)

#match_pi((a : A) -> B)

x : _T $ _G + H |- x : _T $ H -| x : _T $ _G

_A $ G $ H

(a : (A $ G) $ H) -> B

T(α) : Type;
T(α) = () -> (β < α) -> T(β);

U(α) : Type;
U(α) = () -> (β < α) -> () -> (δ < β) -> U(δ);

T(α) == U(α)
T(β) == () -> (δ < β) -> U(δ)
() -> (δ < β) -> T(δ) == () -> (δ < β) -> U(δ)
T(δ) == U(δ)
T(β) == () -> (δ < β) -> U(δ)

_A(y)
_A == (((x) => _) as _B)
(((x) => _) as _B)(y) |-> _B[x := y]

_A(y) == _B[x := y]
_A == (x) => _B

_A.@ == _B[x := _A]
_A == [x = @, _B]


T = [x = @, () -> x.@];

_A.@ == () -> T.@
_A == [x = @, () -> T.@]


[x = @, () -> T.@] == T

_A.@ == ((C, r) => r) as unit.@

_A.@ == (C, r) => r

_B[x := _A] == ((C, r) => r) as unit.@

_A.@ == N
 == _B[x := [x = @, _B]]

T : Type;
T = [G](f : T $ 1 + G)
f(f : _A $ 1 + _G) == f()

_A(y) == _B[x := y]


T = (x) => _B

T(y) == _B[x := y]

#match_pi(_B[x := y])


_A[x := y] == _B
_A == _B[y := x]

_B[x := y] == Π(_C, _D)
_B == Π(_C, _D)[y := x]

_B == Π(_C, _D)
_B[x := y] == Π(_C[x := y], _D[x := y])


_A * 2

Γ, x : A |-
----------------------------
Γ | A |- self(x) -> B : Type


_A[0 .. ∞]


(x * y * z)
(x * z) + (x * y)

(LM + LN) == (RM + RN)




A $ (_G * R) == A $ _H

_G * (1 + R) == _H

_G * 2 == _H
_G == _H / 2

_G == _H / 2

_H[]

_G == _H / 1 + R

_G == _H / 1 + R

_G * (1 + R) == _H

_A[0 .. ∞] + _B[0 .. ∞] == _C[0 .. ∞] + _D[0 .. ∞]
_A[0 .. ∞] == _C[0 .. ∞] + _D[0 .. ∞] - _B[0 .. ∞]

(_C[0 .. ∞] + _D[0 .. ∞])[>= _B[0 .. ∞]] - _B[0 .. ∞]
_A * _B == _C

GC + GA

(x * y) + (z * w)

_A(N) == [_B(N), _C(N)]

_A(x) == [_B(x), _C(x)]
_A == (y) => [_B(y), _C(y)]


_A == (x) => (y) => _C(x)(y)

_A(x) == x
_A(y)


_A(L)(R) == [_B(L)(R), _C(L)(R)]
_A == (x) => (y) => [_B(x)(y), _C(x)(y)]
_A == (x) => (y) => [_B(y)(x), _C(y)(x)]

(x) => (y) =>  [_(x), _(x)]
[_(y), _(y)]

_A(x)(y)(z) == [_B, _C]
x :: y :: z :: _

(x) => (y) => (z) => _

f(x)(y)(z)


_A(x) == Π(_B(x), _C(x))
_A == (y) => Π(_B(y), _C(y))

== [_B, _C]


_A(x).0


_A(x) == _B[y := x]
_A == (y) => _B

_A(x) == _B[y := x]

_A(x) == _B[y := x]

_A(x) == _B[y := x]

f(x) : B

f([x, y]) : B(x, y)


Γ | S.0 |- M ⇐ A
Γ | S == [M, S.1] | S.1 |- N ⇐ B(M)
-----------------------------------
Γ | S |- [M, N] ⇐ [a : A, B(a)]

Γ, x == (fix(x) => y).@ | S.@ |- K ⇐ T
-----------------------------------
Γ | S |- fix(x) => K ⇐ self(a) -> T

fix(P) : [Unit : Type, Unit] => [
  self(u) -> (C : (u : P.@.Unit) -> Type, r : C(P.@.unit)) -> C(u),
  fix(u) => (C, r) => r
]

Γ, x == S | S.@ |- K ⇐ T
-----------------------------------
Γ | S |- fix(x) => K ⇐ self(a) -> T

Γ | S.0 |- M ⇐ A
Γ | S == [M, S.1] | S.1 |- N ⇐ B(M)
-------------------------------
Γ | S |- [M, N] ⇐ [a : A, B(a)]

Unit : Type;
unit : Unit;

Unit = [u : @, ind(C : (u : Unit) -> Type, r : C(unit)) : C(u)];
unit = [u = @, ind(C, r) = r];

P : [Unit : Type, Unit];
P =
  Unit : Type;
  unit : Unit;

  Unit = _;
  unit = _;

  Unit
  [
  (
    Unit : Type;
    Unit = [u : @, ind(C : (u : Unit) -> Type, r : C(P.unit)) : C(u)];
    Unit
  ),
  [u = @, ind(C, r) = r];
  _ : ([u : @, ind(C : (u : P.Unit) -> Type, r : C(P.unit)) : C(u)] as P.Unit)
  _ : P.Unit
]


T : Type = (f(a : A) : B);



a : A;
b : B[a];

a = M[a, b];
b = N[a, b];
K

f(_A : )

(fix(P) : [a : A, b : B[a]] => [
  M[P.@.a, P.@.b],
  N[P.@.a, P.@.b]
]).@;

(fix(P) => f(P)).@
f(fix(P) => f(P))

a : A;
a =
  b : B[a];
  a = M[a, b];
  b = N[a, b];
  a;

b : B[a];
b = N[a, b];

P : [a : A, b : B(a)];


x.@(a).0 == M
x.@(a) == [M, r]
x.@ == (a) => [M, r]
x == [x : @, (a) => [M, r]]

//
a : A;
a =
  b : B[a];
  a = M[a, b];
  b = N[a, b];
  a;

a : A;
b : B[a];
c : C[a, b];

a = M[a, b, c];
b = N[a, b, c];
c = K[a, b, c];

//
T_a : Type;
T_b : Type;
T_c : Type;
a : T_a;
b : T_b;
c : T_c;

T_a = A;
T_b = B[a];
T_c = C[a, b];
a = M[a, b, c];
b = N[a, b, c];
c = K[a, b, c];

//
a : A;
b : B{a};

a = M{a, b};
b = N{a, b};

//
(@P : [a : A, b : B(a)] =
  b = N[P.a, P.b];
  [M[P.a, P.b], b]);


//
m_b(a : A) : B{a}
m_b(a) = N{a, m_b(a)};

a : A;
a = M{a, m_b(a)};

b = m_b(a);



Stream(A : Type) : Type;
Stream(A : Type) = [
  hd : A,
  tl : Stream(A)
];

ouroboros{A}(hd : A) : Stream(A);
ouroboros{A}(hd : A) = [hd, ouroboros{A}(hd)];

//
[a, b] =
  (@P : [a : A, b : B(a)] = [M[P.a, P.b], N[P.a, P.b]]).@;
//
[a, b] =
  (@P : [a : A, b : B(a)] =
    [
      (@a = M[P.a, P.b]),
      (@b = N[P.a, P.b])
    ]).@;

Pair(A : Type, B : Type) : Type;
fst(p : Pair(A, B)) : A;

Pair(A, B) = [@p(b : Bool) : b ? A : B(fst(p))];
fst(p) = p(true);

_P : Pair(A, B);
_P[b := true] == M
_P == (p(b) = b ? M : _C);
p(C : Type, c(a : A, b : B(a)) : C) : C;

_P(A, c(a, b) = a);
_P == M{(c(a, b) = a) := c}{A := C}


Bool = [b(C : Type, t : C, f : C) : C];
true : Bool = [b(C, t, f) = t];
false : Bool = [b(C, t, f) = f];

_B : Bool;
_B([a : A, b : B], [M, N], [L, R]) == [X, Y]
// by naturality
[_B(A, M, L), _B(B, N, R)] == [X, Y]
_B(A, M, L) == X
_B(B, N, R) == Y
// eventually it will get in a neutral A
// if A is neutral so is M and L
// which means this is an FCU case
_B(A, M, L) == X



b(Π(a : A) -> B(a), M, N) |-> λ(a : A) => b(B(a), M(a), N(a))

true(A, M, L) == X
false(A, M, L) == X


T = fix(T) : Type => () -> T.@;
T.@
(() -> T){T := frozen(T) : Type => () -> T}

() -> (frozen(T) : Type => () -> T).@

Γ |- M : (frozen(x) => K).@
-------------------------------
Γ |- M : K{x := frozen(x) => K}
T.@
M.@

Γ, x : |(@x : T)| |- K ⇐ T
--------------------------
Γ |- (@x = K) ⇐ (@x : T)

Γ |- M : |A|
------------
Γ |- M : A

Γ |- M : A
--------------
Γ |- [M] : |A|

(@x = K).@
x = [@x = K]; K

(@x = K).@


() -> (frozen(T) : Type => () -> T).@

_A == _A[x := y][y := x]



List(A : Type) : Type;
List(A) = [
  b : Bool,
  p : b ? [] : [hd : A, tl : List(A)]
];

List(A : Type) : Type;
List(A) = [
  n : Nat $ 0;
  p : n.fold(
    [b == false],
    s(T) = [b == true, hd : A, tl : T]
  );
];
// works because by naturality


a : A;
b : B{a};

a = M{a, b};
b = N{a, b};

P : [a : A, b : B(a)];
P = [M{P.a, P.b}, N{P.a, P.b}];

//
a : A;
b : B{a};

b = N{a, b};
a = M{a, b};

// to
P_T : Type;
out(P : P_T) : A;

P_T = (@P : [b : B(out(P)), a : A]);
out(P) = P.@.a;

P : P_T;
P = [N{P.@.a, P.@.b}, M{P.@.a, P.@.b}];

a : A = P.@.a;
b : B(a) = P.@.b;

// to
P_T : Type;
out(P : P_T) : A;

P_T = (@P : [b : B(out(P)), a : A]);
out(P) = P.@.a;

[a, b] = (@(P : P_T) = [N{P.@.a, P.@.b}, M{P.@.a, P.@.b}]);

a : A = P.@.a;
b : B(a) = P.@.b;

//
a : A;
b : B{a};

b = N{a, b};
a = M{a, b};
// to
a : A;
a =
  b : B{a};
  b = N{a, b};
  M{a, b};

b : B{a};
b = N{a, b};

//

a : A;
b : B{a};

a = M{a, b};
c : C{a, b};
b = N{a, b, c};
c = K{a, b, c};

//
a : A;
T_c : Type;
b : B{a};
c : T_c;

a = M{a, b};
T_c = C{a, b};
b = N{a, b, c};
c = K{a, b, c};

//
T_a : Type;
T_b : Type;
T_c : Type;
a : T_a;
b : T_b;
c : T_c;

T_a = A;
T_b = B{a};
a = M{a, b};
T_c = C;
//
T_a : Type;
T_b : Type;
b : T_b;
a : T_a;

T_a = A;
T_b = B{a};
b = N{a, b};
a = M{a, b};


a : A;
b : B{a};
b = N{a, b};
c : C{a, b};
a = M{a, b, c};
c = K{a, b, c};

// to
T_a : Type;
T_b : Type;
b : T_b;
T_c : Type;
a : T_a;
c : T_c;

T_a = A;
T_b = B{a};
b = N{a, b};
T_c = C{a, b};
a = M{a, b, c};
c = K{a, b, c};

// then when it's ordered
a : A;
b : B{a};
c : C{a, b};

a = M{a, b, c};
b = N{a, b, c};
c = K{a, b, c};

// to
P : [a : A, b : B(a), c : C(a, b)];
P =
  a = P.a;
  b = P.b;
  c = P.b;
  [
    M{P.a, P.b, P.c},
    N{P.a, P.b, P.c},
    K{P.a, P.b, P.c}
  ];


P => K

[] => K


P(Q) = x; K |-> P = x = x;

P(Q) = N; K |-> P = (_(Q) = N); K


_([]) = K |-> _(x) = drop(x, K)
_([P, Q]) = K |-> _(x) = P = x.0; Q = x.1; K
_(P(Q)) = K |-> _(P) = K

[P, Q] = N; K |-> P = N.0; Q = N.1; K

_([P, Q]) = K |-> _(x) = x(_(P)(Q) = K);


([f(x), g(y)]) = K




f(g)

_(P(Q)) = K
_(f) = P = f; K

_(x) = drop(x, K)

_(x) => [P, Q] = x; K



a : A;
b : B{a};

a = M{a, b};
b = N{a, b};

Bool_P(
  C(Bool : Type) : Type,
  c(Bool : Type, true : Bool, false : Bool) : C(Bool)
) : C(Bool_P.@(C(_) = Type, c(Bool, true, false) = Bool));


#x = a@0 | b@1 | c@2;
1 | x = d; K

//
#x = b@1 | c@2 | a@0 | d@1;

2 | K

// from
z = ((x) => K)(y);
// to
x = y;
z = K;


z = ((x) => M)(N); K |-> x = N; z = M; K  (x ∉ K)



f(x) = 1;
y = f(N);



((x) => (x) => K)(M)(N)
(x = M; (x) => K)(N)
x = N; K


M(N{x, y, z})

a = (#x; #y; #z; N); M(a)



[P, Q] = N; K |-> P = N.0; Q = N.0; K

[P, Q] = N; K |-> P = #after(Q , N); Q = N.1; K


a : A;
b : B{a};

a = M{a, b};
b = N{a, b};

a : A := M{a, b};
b : B{a} := N{a, b};

//
a : A := b; M{a, b};
b : B{a} := N{a, b};


(l := M) |-> (l := N)

Γ |- N ⇒ A
------------------------
Γ |- [x : A == N] ⇒ Type

Γ |- M : [x == N ⇐ A] -> T
---------------------------
Γ |- M(N) ⇐ T


Γ |- B : (A ⇐ Type) -> Type
---------------------------
Γ |- Π(A, B) ⇐ Type


T = forall X. forall Z. ((Z -> Z) -> X) -> X;
// same as, only negative strictly positive ocurrences
T = forall X. ((forall Z. Z -> Z) -> X) -> X;


// the naturals
T = forall X. X -> (X -> X) -> X;
// of course
T = forall X. X -> (T -> X) -> X;

// HOAS
T = forall X. ((X -> X) -> X) -> (X -> X -> X) -> X
// that can be transformed because it's a functor
T = forall X. (Functor(T, X) -> X) -> (T -> T -> X) -> X


P(forall A. B) = forall A. P(B)
P(X) = S;
P(Y) = Y;
P(A -> B) = N(A) -> P(B)

N(forall A. B) = forall A. N(B)
N(X) = X
N(Y) = Y
N(A -> B) = P(A) -> N(B)



(M(N) ⇐ T + S)

(M ⇐ ((a == (N ⇐ A)) -> T) + M(N \Left))(N ⇐ A + [N])

Γ |- M : Π([x == fix(M)], T)
----------------------------
Γ |- fix(M) ⇐ Self(T)


Γ |- M ⇐ Self(λ(_) = T)
-----------------------
Γ |- M.@ ⇐ T

Γ |- M ⇐ Σ([x == M.0], λ(_) = T)
--------------------------------
Γ |- M.1 ⇐ T

Γ |- _F(M) == T  Γ |- M ⇐ Self(_F)
----------------------------------
Γ |- M.@ ⇐ T

Γ |- M : A  Γ |- (M : A) :> B
-----------------------------
Γ |- M : B

(x : Self(λ(_) = T)).@

(x.@ : F(x) :> T)
(x : Self(λ(x) = F(x)) :> Self(λ(_) = T))


(b = (_(C, t, f) = _ : C)).@ : Π(C, t : C(true), f : C(false)) -> C(true)
((x = M).@ ⇐ T)
(x = (M ⇐ T)).@

((x = M).@ : T)

(x.@ : F(x) :> T)
(x : Self(λ(x) = F(x)) :> Self(λ(_) = T))

_F(M) == T

_F(x) == T

Γ |- M ⇐ Self(T)
----------------
Γ |- M.@ ⇐ T(M)

_A.@ == N

_A.@ == M

Γ |- M.@ == N.@
-----------------------
Γ |- (M : Self(T)) == N

_A == M

_A == (@x = N)

(@x = N).@ |-> N{x := N}

Γ, x : #[self(x) -> T] |- T : Type
----------------------------------
Γ |- self(x) -> T : Type

Γ, x == #[fix(x) => K] |- K : T
----------------------------------
Γ |- fix(x) => K : self(x) -> T

a : A;
b : B{a};

a = M{a, b};
b = N{a, b};

[a, b] = fix([a, b]) =>
  (([a, b]) => [a, N{a, b}])([M{a, b}, b])
  x = M{a, b};

  [
    block(l, M{a, b}),
    unblock(l); N{a, b}
  ];

fix(x) => [M, N]
fix(x) =>
  [block(f, M), unblock(f, N)]



λ([]) => K
λ([]) => K



----------------
Self(M) == Self(N)

(x.@ : T(x)) :> F{x}

(x : Self(T)) :> Self(λ(_) = F{x})


(M : Self(A)) :> Self(B)

(M.@ : A(M)) :> B(M)

Γ |- M ⇐ [x : A == N] -> T
--------------------------
Γ |- M(N) ⇐ T

Γ |- M ⇐ T(M)
----------------------
Γ |- (ι = M) ⇐ (x : T)

Γ |- M : A  Γ |- M == N
---------------------
Γ |- M ⇐ [x : A == N]

Γ |- M ⇐ Π([x : T == (ι = M)], T)
---------------------------------
Γ |- (ι = M) ⇐ (x : T)

Γ |- M ⇐ T(M)
----------------------
Γ |- (ι = M) ⇐ (x : T)

Γ, x : T |- T : Type
--------------------
Γ |- (x : T) : Type

Γ |- M : T{x := (@ = M)}
------------------------
Γ |- (@ = M) : (x : T)

//
P = N; K |-> #check(P); #check(N); (λ(P) => K)(N)


// bad
[f(x), g(y)] = N; K

(λ([f(x), g(y)]) => K)

(λ([f, g]) => K)


[] = x; K |-> drop(x, K)
[P, Q] = x; K |->
  #check(x : [a : _A, b : _B]);
  (P : _A) = x.0; (Q : _B) = x.1; K

λ([P, Q]) => K |-> λ(x) => P = x;

λ(P : A) => K


(P : A : B) = x; K
P = ((x : B) : A); K


(P : A) = x; K |-> #check(x : A); P = x; K
(P : A) = x; K |-> #check(x : A); P = x; K


λ(x) => #check(x : A); P = x; K

[P, Q] = x; K |->
  #check(x : [a : _A, b : _B]);
  (P : _A) = x.0; (Q : _B) = x.1; K

Pair{A}(B(a : A) : Type) : Type =
  (p : Π(C, c(a : A, b : B(a))) ->
    C(p(C(_) = A, c(a, b) = a)));
pair(a : A, b : B(a)) : Pair(B) =
  λ(C, c) => c(a, b);
// goes to, [] to indicate that it's not typed yet
p := [λ(C, c) => c(a, b)] ⇐
  Π(C, c(a : A, b : B(a))) -> C(p(C(_) = A, c(a, b) = a));
// pushing it down and breaking it a part a bit more
r{C, c, p} := c(a, b) ⇐ C(p(C(_) = A, c(a, b) = a));
p := λ(C, c) => [r{C, c, p}];
// r normalizes to
r{C, c, p} := c(a, b) ⇐ C(C(_) = A; c(a, b) = a; [r{C, c, p}]);
// but if you then expand r once on itself
r{C, c, p} :=
  c(a, b) ⇐ C(
    C(_) = A;
    c(a, b) = a;
    [c(a, b) ⇐ C(C(_) = A; c(a, b) = a; [r{C, c, p}])]
  );
// but what happens if you now reduce the C
r{C, c, p} := c(a, b) ⇐ C(c(a, b) = a; [c(a, b) ⇐ A]);
// which now type checks
r{C, c, p} := c(a, b) ⇐ C(c(a, b) = a; [c(a, b) ⇒ A ⇐ A]);
// and can then be reduced
r{C, c, p} := c(a, b) ⇐ C(a);
// which actually also type checks
r{C, c, p} := c(a, b) ⇒ C(a) ⇐ C(a);

//
Pair{A}(B(a : A) : Type) : Type =
  (p : Π(C, c(a : A, b : B(a))) ->
    C(p(C(_) = A, c(a, b) = a)));
pair(a : A, b : B(a)) : Pair(B) =
  λ(C, c) => c(a, b);
// let's start by introducing p as just a hole
p := _X(a, b)
λ(C, c) => c(a, b) ⇐
  Π(C, c(a : A, b : B(a))) -> C(p(C(_) = A, c(a, b) = a));
// as we go typing it gets updated
_X(a, b) := λ(C, c) => _Y(a, b, C, c)
λ(C, c) => c(a, b) ⇐ C(p(C(_) = A, c(a, b) = a));
// which by expanding is the same as
λ(C, c) => c(a, b) ⇐ C(C(_) = A, c(a, b) = a; _Y(a, b, C, c));
// which by infer leads to a stuck constraint
C(a) == C(C(_) = A, c(a, b) = a; _Y(a, b, C, c))
// but we also still have the original one
_X(a, b) == λ(C, c) => [c(a, b)]
// which when expanding and simplifying
_Y(a, b, C, c) == [c(a, b)]
// now back, note that c(a, b) is still untyped
C(a) == C(C(_) = A, c(a, b) = a; [c(a, b)])


λ(C, c) => c(a, b) ⇐ C(_Z(a, b));
// doing the infer
c(a, b) ⇒ C(a) ⇐ C(_Z(a, b));
// well pattern unification
_Z(a, b) := a;
// now we need to check
(λ(C, c) => c(a, b)) == _X(a, b)
// expanding
(λ(C, c) => c(a, b)) == λ(C, c) => _Z(a, b)


(@ = λ(C, c) => c(a, b) ⇐ C(p(C(_) = A, c(a, b) = a)))
(@ = λ(C, c) => c(a, b) ⇐ C(_R{a, b}))

p := λ(C, c) => c(a, b)

r{p} := p(C(_) = A, c(a, b) = a);
p := λ(C, c) => c(a, b) ⇐ C(r{p})

r{p} := a;

c(a, b) ⇐ C(C(_) = A; c(a, b) = a; c(a, b) ⇐ C(p(C(_) = A, c(a, b) = a)))


(λ(C, c) => c(a, b) : C(a))

C(a)
C((@ = λ(C, c) = [c(a, b)])(C(_) = A, c(a, b) = a))
C(c(a, b) = a; [c(a, b) ⇒ A] ⇐ A)

C(a)

C((@ = λ(C, c) = c(a, b))(C(_) = A, c(a, b) = a))
C(c(a, b) = a; [c(a, b)])

[C(a)]

Γ |- M(N) == N.@
-----------------------
Γ |- (@ = M) == (@ = N)

x : (@ : T);

x.@ : T(x)

x
(@ = λ(_) => λ(C, t, f) => t);


x == (@ = λ(x) => x.@)

x.@ == (λ(_) => x.@)(x)

x == (@ = x.@)

x == (@ = λ(_) => x.@)
x == (@ = λ(_) => M)

x.@ : T(x)




Γ | N, C |- M ⇐ T
----------------- // apply
Γ | C |- M(N) ⇐ T

Γ | @, C |- M ⇐ T
----------------- // unfold
Γ | C |- M.@ ⇐ T

Γ |- N ⇒ A  Γ, x == N | C |- K ⇐ T
---------------------------------- // let
Γ | N, C |- λ(x) => K ⇐ T

Γ | (@ = M), C |- M ⇐ T
----------------------- // fix
Γ | @, C |- (@ = M) ⇐ T

Γ | (@ = M), C |- M ⇐ T
----------------------- // fix
Γ |- (@ = M) ⇐ T

Γ |- A ⇐ Type  Γ, a : A | a |- B ⇐ Type
---------------------------------------
Γ |- (Π, A, B) ⇐ Type

Γ |- A ⇐ Type  Γ |- B ⇐ A -> Type
---------------------------------
Γ |- (Π, A, B) ⇐ Type

//
Γ |- A ⇐ Type  Γ, a : A |- B(a) ⇐ Type
--------------------------------------
Γ |- (Π, A, B) ⇐ Type

Γ | N, C |- M ⇐ T
----------------- // apply
Γ | C |- M(N) ⇐ T

Γ | @, C |- M ⇐ T
----------------- // unfold
Γ | C |- M.@ ⇐ T


//

// from
a : A;
b : B{a};

a = M{a, b};
c : _X;
c = K;
b = N{a, b};

// to
T_a : Type;
T_b : Type;
a : T_a;
b : T_B;

T_a = A;
T_b = B{a};
a = M{a, b};
b = N{a, b};

(P : [
  T_a : Type;
  T_b : Type;
  a : T_a;
  T_c : Type;
  c : T_c;
  b : T_B;
] =
  [T_a, T_b, a, T_c, c, b] = P;
  [A, B, M, _X, K, N]);
[T_a, T_b, a, b] = x;
a = P.a;
b = P.b;


//
(x : [
  T_a : Type;
  T_b : Type;
  a : T_a;
  b : T_B;
  c : _;
] =
  T_a = A;
  T_b = B{a};
  a = M{a, b};
  b = N{a, b};
  [A, B{a}, M{a, b}, N{a, b}, K]);

K{x.0, x.1}

f(x) = N; K

x = N; K |-> (λ(x) => K)(x = N)
[P, Q] = N; K |-> (λ([P, Q]) => K)([P, Q] = N)

P(Q) = N; K |-> (λ(P) => K)(P = λ(Q) => N)

[P, Q] = N; K |-> (λ([P, Q]) => K)(x = (P = x.0; Q = x.1; N))

[x, y] = N;

([f(x), g(y)] = [x, x])

(λ([P, Q]) => K)([P, Q] = N)

[f(x), g(y)] = N; K |-> (λ(x) => K)(x = N)

[P, Q] = N; K

P(Q) = N; K
P = λ(Q) => N; K

P(x)(y) = N; K

P = λ(x) => λ(y) => N; K

f(a : A) : B(a) = N; K
f(a : A) = (N : B(a)); K
f = λ(a : A) => (N : B(a)); K

λ(f(a : A) : B(a)) => K


λ(f(a : A) : B(a)) => K
λ(f : Π(a : A) -> B(a)) => K

λ([]) => K |-> λ(x) => drop(x, K)

λ([P, Q] : A) =>

λ(f(a : A) : B(a)) => K
λ(f : Π(a : A) -> _B) => K
λ(f : Π(a : A) -> B(a)) => K


λ(f : Π(a : A) -> B(a)) => K

λ(x) => f(a : A) : B(a) = x(a); K

λ(x) => f(a : A) : B(a) = x; K

M(N)

λ([P, Q] : A) => _
λ(x : A) => [P, Q] = x;

λ([P, Q] : A) => K
λ(y) => [P, Q] = y; (x : A) = x; K

// TODO:
[Σ, L, R] = A; λ([P : L, Q : R]) => _

(P : A : B)

P(x : A) : B = N; K
P = λ(x : A) => (N : B); K


[P, Q] : T = N; K
[P, Q] : T = N; K

_A(x)(y) == M
_A(x) == λ(y) => M
_A == λ(x) => λ(y) => M


x = N; K |-> (λ(x) => K)(
  (y = (λ(x) => N)(y.@)).@)

P+ = N; K |-> (λ(P+) => K)((y = (P+ = y.@; N)).@)

([P, Q] : T) |-> (y : ([P, Q] = y.@; T))
(P(Q) : T) |-> (P : Π(_A, λ(Q : _A) => T))

P(Q) : T = N; K |->
  (P : Π(_A, λ(Q : _A) => T)) = λ(Q) => N; K


P(Q) : T = N; K |->
  (P : Π(_A, λ(Q : _A) => T)) = λ(Q) => N; K

[x, y] : T
[P, Q] : T |-> T == Π(_A, _B);

P(Q) = N; K |-> (λ(P(Q)) => K)((y = (P(Q) = y.@; N)).@)

P = N; K |-> (λ(P) => K)(
  (y = (λ(P) => N)(y.@)).@)

P = N; K |-> (λ(P) => K)(P = N)


//
λ(x) =>
P = N; K |-> (λ(P) => K)((P = N).@)


P+ = N; K |-> snd([P, r] = [N; K])
P+ = N; K |-> snd([P, r] = [N; K])

x = N; K |-> (y = (a = y.@; x = a.0; r = a.1; [N; K])).1

P(Q) : T = N; K |->

P = (λ(Q) => (N : T)); K

[P, r] = [N; K]
y = ([P, r] = y.@; [N; K])

(P : T) = N; K |-> P = (N : T); K
P(Q) = N; K |-> P = λ(N : T); K


((x as []) = N) |-> (x = drop(x, N))
((x as [P, Q]) = N) |-> (x =

  drop(x, N))



P : A = N; K |-> (λ(P) => K)(_)


λ(P(Q)) => K
λ([P, Q]) => K


λ(x as [P, Q]) => K |-> λ(x) => [P, Q] = x; K
λ(x) => [P, Q] = x; K

λ(x as []) => K |-> λ(x) => drop(x, K)
λ(x as [P, Q]) => K |-> λ(x) =>
  unpair(λ(P) => λ(Q) => K)

λ(x as [P, Q]) => K |->
  λ(x) =>


((P : T) = N) |-> (P = (N : T))
(x = N)
(P(Q) = N) |-> (P = λ(Q) => N)
([] = N) |-> (x = ([] = x.@; N))
([P, Q] = N) |-> (x = ([P, Q] = x.@; N))

P = N; K

(λ(P) => K)(P = N)
|P| = (P = N); K

P(Q) = N; K



[f(a : A) : B(a, f), g(l : L) : R(l, g)]

([] : W[T]) |->


(P(Q) : B) |-> (P : Π(Q) -> B)




//
((P : A) : B) |-> (P : (unify(A, B)))
(P(Q) : B) |-> (P : Π(Q) -> B)
(x : T)
([] : T) |-> (x : ([] = x.@; T))
([P, Q] : T) |-> (x : ([P, Q] = x.@; T))

([P : _A, Q] : T) |-> (x : ([P, Q] = x.@; T))

//
((P : A) = N) |-> (P = (N : A))
(P(Q) = N) |-> (P = λ(Q) => N)
(x = N)
([] = N) |-> (x = ([] = x.@; N))
([P, Q] = N) |-> (x = ([P, Q] = x.@; N))


λ([])

[x : A, y : B] : C

P(Q(a : A) : B) = _;
(P(Q) : T)
((P : Π(Q) -> _X) : T)

(f(Q : A) : _B)

(P(Q : A) : B) = N; K
(P(Q : A) : B) = N; K

//
P = N; K

(P(Q) = N)
(P = λ(Q) => N)

(x = N)

((P : B) : A)

((x : B) : A)
(y : ((x : B) = y; A))

(x : A)
([] : T)
([P, Q] : T)
(P(Q) : T)

([] = N)
([P, Q] = N)
(x = ([P, Q] = x.@; N))
(x = (λ([P, Q]) => N)(x.@))

(P(Q) = N)
([P, Q] = N)


[P : A, B] |-> Σ(A)

(x = (λ(P(Q)) => N)(x.@))


a : A{a};
b : B{a, b};

a = M{a, b};
b = N{a, b};
K

//
a : A;
c : C;
b : B;

a = M;
b = N;
c = K;
K

//
a : A;
c : C;
b : B;

a = M;
b = N;
c = K;


f(g(a)) = _

([P : A, Q : B]) : T;

[x : A, ]

(P(Q) : T) |-> (P : Π(Q) -> T)

(P(Q) : T) |-> ((P : _X) : Π(Q) -> T)


((P : A)(Q) : _)
((P : A)(Q) : _)
((P : A)(Q) : _)

(P(Q : A) : B) |->
  (P : Π((Q : A), λ(Q) => B))
(P : Π(_, λ(Q) => T))

(ι : λ(x) => _A{x})
(x : _A)

(x : _A)

(P(Q) = N) |-> (P = λ(Q) => N)



(x(y) : F(x))

(a : A : B)

[x, y] = [M, N];


(x : T{x}) |-> (x ⇒ (ιx : _A) : T)

(x : T{x}) |-> (x ⇒ _A{x} : T)
(x : T{x}) |-> (x ⇒ unify(_A{x}, T))

(P : T)

(x = N) |-> (x : (x : _T) = N)

(P = N) |-> (x : (P : _T) = (λ(P) => N)(x.@))



//

(P(Q : A) : B) |->
  (P : Π((Q : A), λ(Q) => B))

Γ |- M ⇐ A{x := M}
------------------
Γ |- M ⇐ (x : A)



λ(f, a) => [A, B]


Γ |- M : (f(x : A) : B)  Γ |- N ⇐ A{M, N}
------------------------------------------
Γ |- M(N) : B{M, N}


//
Γ, f(a : A) : B, a : A |- A : Type
Γ, f(a : A) : B, a : A |- B : Type
----------------------------------
Γ |- (f(a : A) : B) : Type

Γ, f(a : A) : B, a : A |- N : B
--------------------------------
Γ |- (f(a) = N) : (f(a : A) : B)

Type |-> (T(a : f(a).0) : Type × Type)
(f(a : A) : B) |-> (T(f : T, a : f(a).0) = [A, B])

(T(f : T, a : f(a).0) : Type × Type) |->
  (T(f : U, a : f(a).0) = [T(f, a).0, ])

Type |-> (T(f : T, a : f(a).0) : Type × Type)



Id : Type = (T(f : T, a : f(a).0) = [A, B])

Type = (Π(f : T, a : A(f)) : Type × Type)

Γ, x : (x : T) |- T : Type
--------------------------
Γ |- (P : T) : Type

Γ, x : A |- A : Type
Γ, x : A |- B : Type
--------------------------
Γ |- (Π(x : A) : B) : Type

Γ, x : A |- A : Type
Γ, x : A |- B : Type
--------------------------
Γ |- (Σ(x : A) : B) : Type



x : _A |- T : Type

(x : El(A, len(x)))

T = [f(a : A) : B, g(l : L) : R];

// same as
T = [(
  f(self : T, a : A) : B,
  g(self : T, l : L) : R
  Data : Type,
  data : Data,
)];


(x(Q) : T) |-> (x : Π(Q) -> T)

(P(Q) : T) |-> (P : Π(Q) -> T)

(P(Q) : T = N) |-> (P = λ(Q) => (N : T))




(x : L{x}) ⇐ (x : R{x})

(x : L{x} == R{x})

(P : A) ⇐ (P : A)

Γ |- P ⇐ (P : A) -| Δ
---------------------
Γ |- P ⇒ A -| Δ

Γ, x : (x : T) |-
--------------------
Γ |- (x : T) : Type


((P : A)(Q : B) : T) |->


((x ⇒ (x : _A)) : T)

(x : T)


(P : T)

(P(Q) : T)

(P : (P(Q) : T))

(x : T)
(P(Q) : T)

[f(x), g(y)] : T

P([L, R] : T)
P([f(x), g(y)] : T)

[f(x), g(x)] = N; _


(P(Q) ⇐ T)
(x ⇐ (x : (T ⇐ Type)))


(P(Q) ⇐ T)
(P(Q) ⇐ T)

(P(Q) ⇐ (P(Q) : T))


(x ⇐ T)

(b : Bool)


((l := P) : T) |-> ((l := P) ⇐ (l : T))

(P = N) |-> (P = (N ⇐ (P : A)))

(P : T) |-> (P ⇐ (P : T))
((P ⇐ T)(Q) ⇐ T) |-> #check(P ⇐ T, Q ⇐ #param(T))

x |-> (x ⇒ (x : _A{x}))

(P ⇒ A) : B |-> P ⇒ A ⇐ (P : B)

(x : T) = N; K

(P ⇒ Π(a : A) -> B)(Q) |-> P(a as (Q ⇐ A)) ⇒ B(a)

[] |-> ([] ⇒ Unit)
[P ⇒ A, Q ⇒ B] |-> [x as P, Q] ⇒ A
(P ⇒ A ⇐ A) |-> P

(x : T)

(x ⇒ L : R)

Γ |- (P : T) -| Δ
-------------------
Γ |- (P : T) : Type

((P ⇒ L) : R)

(P = N) |-> (P = (N ⇐ (P ⇐ _A)));

-------------------
Γ |- P : A

Γ |- A : Type  Γ |- A == [B]
----------------------------
Γ |- ...A : Type

f(x, g : (g(y : A) : B)) =

f(x, g(y : A) : B) = x + y
f(x, g(y) = )

map = _;
@map = _;

ind(
  C(a) = C(1 + x),

)
P(Q)

[f(a : A) : B, g(b)] = N; K

[f(a), g(b)] |-> N

[
  f : (f(a : A) : B),
  g : (g(b : L) : R)
] = _


f(g(a : A) : B,)

[f(a : A) : B, g(b : L) : R] = _

[f(a : A) : B, g(b : L) : R] = _


(P(Q) : A)

(x ⇒ (x : T))

(x ⇒ (x : _A{a}))(Q)

(x ⇒ (x : _A{a}))(Q)

(x ⇒ _A{x} | (x : _A{x}))

(x ⇒ _A{x} | (x : _A{x}))(Q)
(x ⇒ Π(_L{x}, _R{x}) |
  (x : Π(_L{x}, _R{x})))(Q)
x(Q ⇐ _L{x}) ⇒ _R{x} | (x : Π(_L{x}, _R{x}))

P(Q)(R) = N

P(x)(y) = N


(P(Q) : A)

(x : _)
(x ⇒ (ι : _A) : _A(x))

f(x) == λ(y) => f(x)(y)
f(x) == f(x)





_A(x : A)(y : B) == (M : C{x, y})
_A(x : A) == λ(y : B) => (M : C{x, y})
_A == λ(x : A) => λ(y : B) => (M : C{x, y})



A : Data;
len(a : A # len(a)) : A;

(len : (ι : _A))(a : A # len(a)) : A;
_A == λ(len) => Π(_B(len), _C(len))

(len : (ι : λ(len) => Π(_B(len), _C(len))))(a : A # len(a)) : A;
(len.@ : Π(_B(len), _C(len)))(a : A # len(a)) : A;

len((a : A # len(a)) ⇐ _B(len)) : _C(len)(a) : A;
_B(len) == λ(a) => A # len(a)
len((a : A # len(a))) : _C(len)(a) : A;
_C == λ(len) => λ(a) => A

(x : (ι : _A))(Q)
(x.@ : _A(x))(Q)
(x.@ : Π(_A(x), _B(x)))(Q)
(x.@)(Q ⇐ _A(x)) ⇒ _B(x)
(x : (ι : _A))(Q)

(x : T)

(a : (ι : _X) | _X(a) : A # len(a))
(a : A # len(a) ⇒ A # len(a) ⇐ _B(len))

A # len(a) ⇐ _B(len)
len(a) ⇒ A # len(a) ⇐ _B

_X == λ(a) => A # len(a)

id(
  A : Data,
  len(a : A # len(a)) : A,
)

(P(Q)(U))

P(x)(y) = N;
P(x)(y) = λ(x) => N;

(C(f : False) : Type) :

(P(Q) : A = N)
(P = λ(Q) => (N : A))

(P : A : B) |->

(P(Q : A) : B)
(P : Π(A, λ(Q) => B))

((P : A) : B) |-> (P : unify(A, B))

(P = λ(Q) => (N : A))

(P(Q) : A)
(ι : λ(P(Q)) => A)

λ(P(Q)) => K

(x(Q))


(P(Q) = N) ⇐ Π(A, B)
(P(Q) = N).@ ⇐ Π(A, B)
((P ⇐ Π(A, B))(Q) = N).@ ⇐

(P(Q) ⇐ (x : T))
((P ⇐ (x : T))(Q) ⇐ (x : T))
(P(Q) ⇐ Π(A, B))
(P ⇐ Π(A, B)(Q ⇐ A))


([x, y] : T{x, y})
(a : [x, y] = a; T{x, y})


-----------------------------
Γ |- (Π(a : A) : B) ⇒ Type(l)

(Π(a : (A ⇐ _S_A)) : (B ⇐ _S_B(a))) ⇒ #pi_sort(_S_A, _S_B)

(Π(a : A) : B) ⇐ Type(l)

(Π(a : A) : B) ⇐ Type(l)

--------------
Γ, A |- \0 : A

Γ, T |- T : Type
----------------
Γ |- ι(T) : Type

Γ, T |- K : T
----------------
Γ |- μ(K) : ι(T)


Γ |- A : Type  Γ, A |- B : Type
--------------------------------
Γ |- Π(A, B) : Type

Γ, A |- K : B
-------------------
Γ |- λ. K : Π(A, B)

(P(Q : A) : B)
(P : Π((Q : A), λ(Q : A) = B))

(f(x : A)(y : B) : C)
(f(x : A)(y : B) : C)

Γ | N |- P : T -| Δ  Γ, Δ |- N : T
----------------------------------
Γ | S |- (P : A)

Γ | S |- P : T -| Δ
Γ, Δ, S == (P = ) |  |- N : T
------------------------------------------------
Γ | S |- (P = N)


(P(Q) : T)



f(x : Unit(x))

(f(x : A) : B)
(ι : λ(f) => Π(_A, λ(x : A) => B))
(ι : λ(f) => Π(_A, λ(x : A) => B))

(a = (b = N));

(a : A)
(a : (b : A))

T = (f(x : A) : B);

((f : T)(x : A) : B)

(ι : λ(f) => _A{f})
(ι : λ(f) => T)

(f(x : A) : B) |-> (ι : λ)

(P(Q : A) : B) |-> (ι : λ())

(ι : λ(f) => Π(_A, λ(x : A) = B))

(ι : λ(f) => _A(f))
(ι : λ(f) => Π(_A, λ(x) => _B(f)(x)))

(x : T)


f(g(a : A) : B) : C;

x : (g(a : A) : B);

f(x)

f : Π(g : Π(a : A) -> B) -> C

(f(x : A) : B = N)

(ι = λ(f) => λ(x) => f(x : A) : B = N)
(ι = λ(f) => _A(f))
(ι = λ(f) => λ(x) => _B(f)(x))
(ι = λ(f) => λ(x) => (_C{f}))
(ι = λ(f) => λ(x) => (N : B))

(ι : λ(x) => _A{x})

Γ, a : (a : A) |- A : Type
--------------------------
Γ |- (a : A) : Type


(P : A)

(P : A) as S |->
  (ι : λ(a) => _B(a)) == S;
  #elab_self(P : A, _B(a))

Γ, S == x | S |- N : A
--------------------------
Γ | S |- (x = N) : (x : A)

Γ | S |- P : A -| Δ
Γ, S == x | S |- N : A
--------------------------
Γ | S |- (P = N) : (x : A)

Γ, S == f, y : A | S(y) |- N : A
------------------------------------
Γ | S |- (f(y) = N) : (f(a : A) : B)


Γ,
------------------------------------
Γ | S |- (P = N) : (a : A)

Γ |- (f(x : A) : B = _A)
--------------------------
Γ |- (f(x : A) : B) : Type

Γ, a : (ι : T) |- T(a) :
-------------------------------
Γ |- (ι : T)

T_Unit = (Unit(unit : Unit(unit)) : Type);

(Unit(unit : Unit(unit)) : Type)

((Unit ⇒ (a : _A(a)))(unit : Unit(unit)) : Type)

_A(Unit) == Π(_X, _Y)
_A == λ(a) => Π(_X, _Y){Unit := a}

Unit(unit : Unit(unit)) : Type;
Unit(unit) = (u : unit == u);

U(l, El(r <= l, a : U(l, El)) : Set) : Set;
U(l, El) =
  | univ(l) : U(1 + l)
  | pi(A : U(l), B(a : El(A)) : U(l)) : U(l);

El(l, T : U(l, El)) : Set;
El(l, T) =
  l.is(
  // l == 1 + r, so the El is safe
  | univ(r) = U(r, El)
  | pi(A, B) = Π(a : El(l, A)) -> El(l, B(a))
  );

------------------------------
Γ | S |- x : A -| (x : A) == S

Γ | S |- f : Π(A, B) -| f == S
Γ, S == f(x), x : A | x |- Q : A -| Φ
------------------------------
Γ | S |- f(Q) : T -| (x : A) == S

Γ | S |- P : A -let| Δ
Γ, Δ |- N : A
Γ | N |- P :
Γ |- N : A -| Δ
-------------
Γ |- P = N; K

f(g(a)) = _;

f([x, y]) = _;

Γ | S |- P : Π(A, B) -| Δ
Γ, Δ |- Q : A -| Φ
------------------------------
Γ | S |- P(Q) : T -| (x : A) == S




Pattern, P, Q ::=
  | (P : A) | x | P(Q)

((P : A) : B) |-> (P : A as B)
// in self
(P(Q) : T) |-> (P : Π(Q) -> T)


//
(P(Q) = N) |-> (P = λ(Q) => N)
(P(Q) : T = N) |-> (P = λ(Q) : T => N)

(P(Q) = N)

(P : T)

(P : A)
(P = N)
(P = N; K)


Σ, Id

M as N == S

f(a : A, b(x : X) : B) = _;




a : A;
b : B;

a = M;
b = N;
// to
T_a : Type;
T_b : Type;
a : T_a;
b : T_b;

T_a = A;
T_b = B;
a = M;
b = N;

// alternative
a : A;
b : B{a};

b = N{a, b};
a = M{a, b};
//

P : (T_a : Type) × (T_b : Type) × (a : T_a) × _S2;
P = [A, B, M, _S3]

a : A;
b : B{a};

b = N{a, b};
a = M{a, b};

// to
m_a(b : B{m_a(b)}) : A;
m_a(b) = M{m_a(b), b};

b : B{m_a(b)};
b = N{m_a(b), b};


//

T_Unit = (Unit(unit : Unit(unit)) -> ((x) => x(x))((x) => x(x)));

(Unit(unit) : ((x) => x(x))((x) => x(x))) : Type
((x) => x(x))((x) => x(x)) == Type

T_Unit = (Unit(unit : Unit(unit)) -> Type);


T_Unit = (Unit : Π(unit : (unit : Unit(unit))) -> Type);


//
(Unit(unit) : id(Type)) : Type
T_Unit = (Unit(unit : Unit(unit)) -> Type);
//
T_Unit : Type;
T_unit(Unit : T_Unit) : Type;

T_Unit = (Unit(unit : T_unit(Unit)) : Type);
T_unit(Unit) = (unit : Unit(unit));

f(b : Bool, x : b ? Nat : String) = _;


Nat == true ? Nat : String

f(true, (1 : Nat) : true ? Nat : String)

f(true, _ : true ? Nat : String)

f(true, _ : Nat)
f(false, _ : String)


Either(A : Type, B : Type) : Type =
  [b : Bool, x : b ? A : B];

(M(N) : T) == S |->
  A = _; B = _; L = _; R = _;
  (M : Π(A, B)) == L;
  (N : A) == R;
  B(R) == T; L(R) == S
// or if delayed unification is allowed
(M(N) : T) == S |->
  A = _; B = _; L = _; R = _;
  B(R) == T; L(R) == S;
  (M : Π(A, B)) == L;
  (N : A) == R;

(M(N) : T) |->
  A = _; B = _;
  B(N) == T;
  (M : Π(A, B));
  (N : A);

(M(N) == S) |->
  L = _; R = _;
  L(R) == S;
  M == L;
  N == R;

//
(M(N) : T) == S |->
  A = _; B = _;
  (M : Π(A, B));
  (N : A);
  B(N) == T;
  M(N) == S




f(a) = 1 + a;
//
A = _:
f(a : A) = 1 + a;
//
A = _:
f(a : A) = 1 + ((a : A) : Nat);
//
f(a : Nat) = 1 + a;


//
A = _;
B = _;

B(A) == Nat;
//
A == a;

//
A = a;
B = (a) => Nat;


A : Type;
a : A;

X : A = _;
B : A -> Type = _;

B(X) == Nat;
X == a;

//

x : A;
X : A = _;
C = _;

B(a : A) : Type = Nat{X := a};
B(X) == Nat;

B == (a) => Nat

(a) => Nat{X := a} == (a) => Nat


Nat{X := a} == Nat;
X == x;


(M(N) : T) == S |->
  A : Type = _; B : A -> Type = _;
  L : Π(A, B) = _; R : A = _;
  B(R) == T; L(R) == S;
  (M : Π(A, B)) == L;
  (N : A) == R;


M(N) == S |->
  [L, R] = #match_apply(S);
  M == L;
  N == R;


M(N) ⇐ T |->
  A = _;
  B = _;
  B(N) == T;
  M ⇐ Π(A, B);
  N ⇐ A;

M(N) == S |->
  [L, R] = #match_apply(S);
  M == L;
  N == R;


(M(N) : T) == S |->
  A : Type = _; B : A -> Type = _;
  L : Π(A, B) = _; R : A = _;
  B(R) == T; L(R) == S;
  (M : Π(A, B)) == L;
  (N : A) == R;



Term, M, N, K ::=
  | x | Π(A, B) | λ(a) => M | M(N)

(Π(A, B)) == S |->
  [Sᵃ, Sᵇ] = #match_pi(S);
  A == Sᵃ;
  B == Sᵇ;

(λ(a) => K) == S |->
  [A, Sᵏ] = #match_lambda(S);
  def(a : A); K == Sz^(a)

M(N) == S |->
  [L, R] = #match_apply(S);
  M == L;
  N == R;

(ι : T) == S |->
  [Sᵗ] = #match_self(S);
  T == Sᵗ



M(N) == S |->
  [L, R] = #match_apply(S);
  M == L;
  N == R;


Γ |- A : Type  Γ |- B : A -> Type
---------------------------------
Γ |- Π(A, B) : Type

Γ |- T : (ι : T) -> Type
------------------------
Γ |- (ι : T) : Type

Γ |- M : ((ι : T) == S) -> Type
-------------------------------
Γ | S |- (ι = M) : Type

Γ |- M : Π(A, B)  Γ, S == M(a) | a |- N : A
-------------------------------------------
Γ | S |- M(N)


(P = N) |-> (P == N)

(P : T) |-> (P : T == _A);


(f(x : A) : B)

(f(x : A) : B) == S

(f(x : A) : B) == S
(f : λ(x : A) => (S : B))
(let f = λ(x : A) => (S : B))

f : Π(x : A) -> B

(f = λ(x : A) => (N : B))

f == λ(x : A) => (S : B)

(x == S) |-> let x = S;
(P : T == S) |-> P == (S : T);
(P(Q) == S) |-> P == λ(Q) => S

(P(Q) == S) |-> P == λ(Q) => S(Q)

(P : T) |->

(P(Q) : T)

(P(Q) == S) |->
  P == S;

  Q == a;


(P(Q) == S) |->
  P(a) == S;
  ()

((x : A) : B) |->
  A == B

(P(Q) : T) |->
  (P : Π(Q) -> T)


(P : A = K) |->


(P(Q) = K) |->
  (P = λ(Q) => K)



(f(x : A) : B) |->
  (f(x : A) : B ⇐ _T)

((f ⇒ (ι : _T))(x : A) : B)


(x ⇒ Π(A, B))(Q) |->
  Q ⇐ A; x ⇒ B;
(x ⇒ A) : B |->
  A == #inst(x, B);
  x ⇒ B;

(P(Q) : T)

P(Q) == S |->
  P == λ(Q) => S

P(Q) == S |->
  P == S

x == (S : T) |->
  let x : (ι : λ(x) => T) = S

P(Q) == S |->
  P == λ(Q) => S;

(f(x : A) : B)
(f : Π(x : A) -> B)

(f(x : A) : B)

(f(x : A) == (f(x) : B))

(x == (S : T))

(P(Q) == (S : B))
(P == (λ(Q) => S) : (Π(Q : A) -> B))

(f(x) : T = f(x))
(f(x) : T = f(x))

(P : T)

(f(g(x : A) : B) : C)

(f : Π(g : Π(x : A) -> B) -> C)
(f : Π(g : (g : Π(x : A) -> B)) -> C)
(P : Π(Q : A) -> B)

Γ, a : A(a) |- M(a) == N(a)
------------------------------
Γ |- M == N : (a : A(a))

M.@

M.@ : A(M)
N.@ : A(N)
M.@ == N

Γ, a : A(a) |- M(a) == N(a)
------------------------------
Γ |- M == N : Π(a : A(a)) -> B

Γ |- A : A -> Type
Γ |- B : A -> Type
-------------------------
Γ |- Π(a : A) -> B : Type




x : A(x);
y : B(x, y);

y = N(x, y);
x = M(x, y);


//
P : [x : A, y : B(x), _];
P = [_X, _Y, _];

//
P : [x : A, y : B(x)];
P = [x, y] = P; [M(x, y), N(x, y)];

//
P : [x : A, y : B(x)];
P = [M(x, y), N(x, y)];

//
P : [x : A, y : B(x)];
P = [A, B(P.x), M(P.x, P.y), N(P.x, P.y)];

//
P : [T_a : Type, T_b : Type, x : T_a, y : T_b];
P = [A, B(P.x), M(x, y), N(x, y)];


(x : (y : A(x, y)))

p : (y : A(x, y)) ==

(x : A) |-> (ι : λ(x) => A)

((P(Q) : A) : B) |-> (ι : λ(x) => A)

//
(x : A) : L;
(y : B(x)) : R(x);

x = M(x, y);
y = N(x, y);


//
x : A(x);
y : B(x, y);

x = M(x, y);
y = N(x, y);
K
//


T = (x : (y : y == x.@))
U = (x : x == x);


f(t : T) : U = (x = t.@.@);
g(u : U) : T = (x = (y = u.@));



id(
  A : Data,
  len(a : A # len(a)) : Length;
  a : A # len(a);
) : (r : A # len(r))

id(
  A : Data,
  len(a : A # len(a)) : Length;
  a : A # len(a);
) : (r := A # len(#self(r)))

id(
  A : Data,
  len(a : A # len(a)) : Length;
  a : A # len(a);
) : A # len(id(A, len, a))

False = (f(C(f : False) : Type) : C(f));
False = (C(f : False) : Type) -> C(#self(False));

(x : (y : A(x, y)));
(x : A(x, x.@));

f(Bool : Type, b : Bool) = _;

true : Bool(true);


f(Bool, _ : Bool)


(f(Q) : B)
(f : Π(_A, λ(y) => Q = y; B))
(f : Π(_A, λ(x : (S[x] : A)) => B))


(f(x : A) : B = N)

(f(x : A) = (N : B))

(f : Π(Q) -> B)

λ(f(Q) : T) => _
λ(f : Π(Q) -> T) => _


(f(Q) : B)
(P : Π(_A, λ(Q) => B))

(f : Π(A, λ(x) => B))


Γ |- A : Self
Γ |- B : A -> Self
-------------------
Γ |- Π(A, B) : Type

Γ, x : T |- T : Type
--------------------
Γ |- (x : T) : Self

Bool = (b(
  C(c : C(c)) : Type,
  t : C(t),
  f : C(f),
) : C(b(C, t, f)));

b : Bool


x : A(x);
y : B(x, y);

x = M(x, y);
y = N(x, y);
//
P : [T_a : ]


(P == (S : Π(A, B)))(Q) |->

(P == S)(Q) |-> P(Q) == S(Q)

P(Q ⇐ A) == (S(Q) : B(Q))

Γ, x == S |- P : A
--------------------
Γ | S |- (P = N) : A

Γ, x == S |- T : Type
--------------------------------
Γ |- (x : T = N) ⇐ (S : (x : T))

Γ, x == S |- T : Type
------------------------------
Γ | S |- (x : T = N) ⇐ (x : T)

(S : T{x := S})


(P ⇒ Π)


λ(x ⇒ (l := A)) => C[x] |-> λ(x ⇒ (l := A)) => C[x ⇒ l]

(λ(x) => K) ⇐ Π(A, B) |-> λ(x ⇒ A) => (K ⇐ B)
(M ⇒ Π(A, B))(N) |-> M(l := (N ⇐ A)) ⇒ B(l)


(l := _A(a))
_A(a)

a = M; (l := (λ(x) => K)(N))
a = M; (l := (x = N; K))




Term, M, N, K ::=
  | (l := D)
Term Data, D ::=
  | S[x] | λ(x) => K | M(N)

(l := M(N)) ==


λ(x) => (x @ y)

(λ(x) => (x @ y)) @ _A

(_A @ L) @ R
_A @ (L @@ R)


λ(a : A) => N |-> (_ : Π(A, _) = λ(a) => N)


((λ(a) => K) == S) ≡ K == S(a)
// only way for this to work is:
((λ(a) => K) == (S : Π(A, B))) ≡ K{a : A} == (S(a) : B(a))

[M, N] == S |-> M == S.0 && N == S.0





(f(x)(y) = N)

(f(x)(y) = N)


((x : A) : B)


_A{x := a}{x := b} == N


_A{x := f(y)} == f(y)

_A{x := f(y)} == C[f(y)]
_A{x := f(y)} == C[x]

_A{x := f(y)} == f(y)
_A == f(y){f(y) := x}

_A == λ(y) => _B{x := y}

_A{x := a}(y) |-> _B

_A{x := a}(y)

_A(a) |-> _A{a}
_A(a) |-> _B
_A == λ(x) => _B{x}

(λ(x) => _B{x := a})(b)
_B{b := a}


(l := succ(l))(x) == (r := succ(r))(y)

l := r
succ(r) == succ(r)

Term, M, N, K ::=
  | _A | Type | Π(a : A) -> B
  | x | λ(x) => K | M(N)

M(N) ⇐ (S : T) |->
  [L, R] = #match_apply(S : T);
  (M ⇐ L)(N ⇐ R)

#match_apply(M : T) ==
  _A, _B, _L, _R; // fresh
  unify([T, M], [_B(_R), _L(_R)]);
  [(_L : Π(_A, _B)), (_R : _A)]




(l := M(N)) == (r := X(Y))
M == X && N == Y


_A(x) |-> _B{a := x}
_A == λ(a) => _B{a}
```
