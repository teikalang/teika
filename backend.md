# Optimizations

```rust
Term, M, N, K ::=
  | x | x = N; K
  | (x) => K | M(N)
  | [M, N] | M.0 | M.1
  | true | false | M ? N : K
  | n | M + N | M * N | for(M, N, K);

(b : Bool) => M[b] ==
  (b : Bool) => b ? M[true] : M[false];

(b : Bool) => b ==
  (b : Bool) => b ? true : false

// bad code
(b : Bool) => (b ? (b ? false : true) : true) ==
  (b : Bool) => (b ? false : true)

// comm
M + N == N + M
M * N == N * M

// assoc
(M + N) + K == M + (N + K)
(M * N) * K == M * (N * K)

// inlining
f = (x) => M; f(N) == ((x) => M)(N)

// it's equality so also reverse
[M, M] == x = M; [x, x]


// parametricity
(b : Bool) => b(Bool, b = (C, t, f) => t; b(Bool, false, true), true) ==
  (b : Bool) => b(Bool, false, true)

// lambda set
```

## Parametricity

```rust
Bool = (C : Type, t : C, f : C) -> C;
*b : (
  C : Type, I : (c : C) -> Type,
  t : C, i_t : I(t),
  f : C, i_f : I(f)
) -> C(b(C, t, f))
// another instance of parametricity
(b : Bool) => (C, t, f) => b(C, b(Bool, false, true)(C, t, f), t)
(b : Bool) => (C, t, f) => b(C, f, t)

// why?
Bool = (C : Type, t : C, f : C) -> C;
b : Bool;
// this should be true externally
b((a : A) -> B(a), f, g)(x) == b(B(x), f(x), g(x))

// even if CPS

// should also be equal
b((a : A) -> B(a), f, g)(K, (c) => M[c(x)]) ==
  b(B(x), f(x), g(x))(K, (c) => M[c])

//
Unit = (C : Type) -> (r : C) -> C;
Bool = (C : Type) -> (t : C, f : C) -> C;
Nat = (C : Type) -> (z : C, s : (acc : C) -> C) -> C;

Third = (C : Type) -> (t : C, f : C, m : C) -> C;

(b : Bool) => b(b(false, true), true) ==
  (b : Bool) => b(false, true)

T = (C : Type) -> (H : C == Bool) -> C;
x : T;

t((C : Type) -> ())
(H) => t((a : A) -> B(a))(H)


b((a : A) -> B(a))
(t, f) => (x) => b((a : A) -> B(a), t(x), f(x))
(t : B(x), f : B(x)) => (x) => b(B(x))(t, f)
(t, f) => b((a : A) -> B(a))(t, f)

(x) => (t, f) => b(B(x))(t(x), f(x))



//
Bool = (C : Type, t : C, f : C) ->
  (K : Type, k : (c : C) -> K) -> K;
b : Bool = (C, t, f) => (K, k) => k(_A);

//
Bool = self(b) -> (
  C : (b : Bool) -> Type
) -> (
  t : C(true),
  f : C(false)
) -> C(b);

b.@ : (
  C : (b : Bool) -> Type
) -> (
  t : C(true),
  f : C(false)
) -> C(b);

(C) => (t, f) => (K, k) =>
  c = b.@(C)(t, f);
  // at best I can conclude the following
  // because otherwise types don't match
  p : [b, c] == [true, t] || [false, f];
  c

eq : (C : (z : A) -> Type) -> (r : C(x)) -> C(y);
(r) =>
  c = eq(C)(r);
  // same, c and r are not guaranteed to have the same type
  // sometimes they diverge and the solution must be uniform
  p : [x, r] == [y, c];
  c


//
Bool = (C : Type, t : C, f : C) ->
  (K : Type, k : (c : C) -> K) -> K;

Nat = (C : Type, z : C, s : (acc : C) -> C) -> C;
(n : Nat) == (C, z, s) => ((z | s(A)) as A)

(t, f) => (K, k) => b(T)(t, f)(K, (c) => k(c))
b(T) : (t : C, f : C) ->
  (K : Type, k : (c : C) -> K) -> K
b((a : A) -> B(a), f, g)(K, (c) => M[c(x)])

M[f(x)]
M[g(x)]


//
Bool = (C : Type) -> (t : C, f : C) -> C;
b : Bool;



p : (b == (C) => (t, f) => t) | (b == (C) => (t, f) => f);
p : b == (C) => (t, f) => (t | f) as _A;

//
b(Bool)(b, true) ==
  t = b; f = true; (t | f) as _A

p : b == (C) => (t, f) => (t | f) as _A;
(b : Bool) => (H : b == true) =>
  ((b | true) as _A)

H : (C : (b : Bool) -> Type) -> (r : C(b)) -> C(true);
H == (C) => (r) => H(C)(r);

p : [b, (C) => (r) => r] == [true, H]

(b : Bool) => (H : b == true) =>
  ((b | true) as _A)

(b : Bool) => (H : b == true) => true

//
(b : Bool) => (H : b == true) =>
  b(A, t, f)


(p(C)(t, f) == t)

(b == true) | (b == false)

b(Bool)(b(Bool)(false, true), true) ==
  b(Bool)(false, true) | true

true(Bool)(b(Bool)(false, true), true)

b(Bool)(b(Bool)(false, true), true) == b(Bool)(false, true)

//
Nat : Type;
Nat = [
  b : Bool,
  p : b ? Unit : Nat,
];
//
Nat = [
  b : (C : Type) -> (t : C, f : C) -> C,
  p : b(Type)((C : Type) -> (c : C) -> C, Nat),
];
//
Nat = (K : Type) ->
  (c : (
    b : (C : Type) -> (t : C, f : C) -> C,
    p : b(Type)((C : Type) -> (c : C) -> C, Nat)) -> K) -> K

//


Nat : Type;
Nat = (C : Type) -> (z : C, s : (p : Nat) -> C) -> C;
// because Unit is irrelevant
Nat = (C : Type) -> (z : (u : Unit) -> C, s : (p : Nat) -> C) -> C;
// this is a C_Either
Nat = C_Either(Unit, Nat);
// which traditionally is the same as
Nat = C_Pair(Bool, (b) => b ? Unit : Nat);


//
C_Either(A, B) = (C : Type) ->
  (l : (a : A) -> C, r : (b : B) -> C) -> C;
e : C_Either(A, B);
// the purely parametric structure is
p_e : C_Either(Unit, Unit);
// aka
p_e : Bool;

// works because



// both paths have the same structure
Nat = (C : Type) -> (c : (b : Bool, p : b ? Unit : Nat) -> C) -> C;
// well this is just C_Pair
Nat = C_Pair(Bool, (b) => b ? Unit : Nat);


//
Nat : Type;
P_Nat(C : Type) : Type;

Nat = (C : Type) -> P_Nat(C);
P_Nat(C) = (z : C, s : (p : Nat) -> C) -> C;


Nat = (C : Type) -> (p : [z : C, s : (p : Nat) -> C]) -> C;
//
Nat = (C : Type) -> (z : C, s : (p : Nat) -> C) ->
  (K : Type) -> (k : (c : C) -> K) -> K;

//


//
Nat : Type;
Nat = (C : Type, z : C, s : (p : Nat) -> C) -> C;

add(n : Nat, m : Nat) : Nat;
add(n, m) = n(Nat, m, (p) => succ(add(p, m)));

// which by parametricity is equal to
Nat : Type;
Nat = [
  b : Bool,
  p : b ? Unit : Nat,
];
// and as such
add(n, m) : Nat;
add(n, m) =
  n.is(
  | [false, []] => m
  // but now you can see the TRMC
  | [true, p] => [true, add(p, m)]
  );
// if you look enough, you can see assoc and comm


```

# JSEnd

Uncurrying

```rust
Term, M, N, K, A, B, G, H ::=
  | Type | Data | Prop | (M : A)
  | Grade | A $ G | 0 | 1 | G + H | G * H
  | x | P = N; M
  | (P : A) -> B | (P : A) => K | M(N)
  | [P : A, B] | [M, N] | M.0 | M.1
  | [P : @, B] | [P = @, M] | M.@

T(α) : Type;
T(α) = [β < α] -> T(β);

T(α) : Type;
T(α) = [a : [β < α] -> T(β), B];

x(α) : T(α);
x(α)


P = N; K == const P = N; return K;
(P : A) => K == function* (x) { const P = x; return K; }
M(N) == yield M(N)
[M, N] == { l : M, r : N }
M.0 == M.l
M.1 == M.r
// works because of eta, being pure
[P = @, (M : (a : A) -> B)] ==
  function f(a) { const P = f; return M; }
[P = @, (M : [a : A, B])] == _
M.@ ==


P : [a : Box(A), B];
P = [box(P.a)]
```

## Stack Management

```rust
map(f, l) : _;
map(f, l) =
  l.is(
  | [] => []
  | [hd, ...tl] =>
    tl = map(f, tl);
    [f(hd), ...tl]
  );
//
map_down(f, l) : _;
map_up(f, l) : _;

map_down(f, l) =
  l.is(
  | [] => map_up([])
  | [hd, ...tl] =>
    tl = map_down(f, tl);
    map_up([f(hd), ...tl])
  );
map_up(f, l) = _
// naive improvement
kmap(f, l, k) : _;
kmap(f, l, k) =
  l.is(
  | [] => k([])
  | [hd, ...tl] => kmap(f, tl, (tl) => k([f(hd), ...tl]));
  );
map(n, f, l) : _;

map(n, f, l) =
  // depends on depth of f
  (n >= 4096).is(
  | true => kmap(f, l, id)
  | false =>
    l.is(
    | [] => []
    | [hd, ...tl] => [f(hd), ...map(1 + n, f, tl)]
    )
  );


//

kmap(f, l, k) : _;
kmap(f, l, k) =
  l.is(
  | [] => k([])
  | [hd, ...tl] => kmap(f, tl, (tl) => k([f(hd), ...tl]));
  );

trm_map(f, l, k) =
  ret = [];
  loop {
    !l.is(
    | [] => !ret := [];
    | [hd, ...tl] =>
      hole = •;
      !ret := [f(hd), hole];
      ret := hole;
      l := tl;
    )
  };
  ret

kmap(f, l, k) =
  loop {
    !l.is(
    | [] => return !k([]);
    | [hd, ...tl] =>
      k := [[hd, !k], ([hd, k], tl) => k([f(hd), ...tl])];
      l := tl;
    )
  };


f(x) : _;
f(x) = h(x, f(g(x)));
//
f(x) : _;
f(x) = y = f(g(x)); h(x, y);
//
f(x, k) : _;
f(x, k) = f(g(x), (y) => k(h(x, y)))
//
f(x, k) : _;
f(x, k) = f(g(x), [[x, k], ([x, k], y) => k(h(x, y))]);
```

# Optimizations

## Graph Rewriting

```rust
Term, M, N, K ::=
  | x | x = N; K
  | (x) => K | M(N)
  | true | false | M ? N : K

L<(x) => K>(N) |-> L<x = N; K> // distant beta
x = V; K |-> K{x := V} // value subst

true ? N : K |-> N // when true
false ? N : K |-> K // when false

((x) => (y) => K)(M)(N)
(x = M; (y) => K)(N)
x = M; y = N; K


x = N; (x = M; (y) => K)(N)
x = N; x = M; y = N; K


(λ. λ. K)(M)(N)
(M; λ. K)(N)
M; N{↑}; K


(let x = 1 + 1 in x + x)
(let x = 2 in x + x)


T(M : Term) : Nat;
C(M : Term) : Term;
G : T(C(M)) <= T(M);

size(x) = 1;
size((x) => K) = size(K);
size(M(N)) = size(M) + size(N);

max_size(path) = path.max(size);

cost(M, []) = 0
cost(M, N :: path) = max(1, size(N) - size(M)) + cost(N, path);
// TODO: cost up to limit / some bound
cost(M) = cost(M, ...norm(M));

f = M; f(N)
M(N)


cost(((x) => x)(N), [N]) = 1
cost(N, []) = 0

cost(((x) => f(x)(x))(N), [f(N)(N)]) =
  (2 * size(N)) - 2
cost(N, []) = 0


y = V; ((x) => f(x)(x))(y) |-> f(y)(y)

b[x] = f(x)(x); y = V; ((x) => b[x])(y)
b[x] = f(x)(x); y = V; b[y]
f(x) = x(s[x]); b[x] = f(x)(x); y = V; b[y]

f(x) = x(s[x]); b[x] = f(x)(x); y = I; b[y]
y(s[y])

((x) => K)(I)
// same as
k[x] = K; g(x) = k[x]; g(I)

k[x] = K; g(x) = k[x]; x = I; k[x];

k[x] = y(x)(x); g(x) = k[x]; g(I)
k[x] = y(x)(x); g(x) = k[x]; y(I)(I)
k[x] = y(x)(x); g(x) = k[x]; y(I)(I)

k[x] = y(x)(x); g(x) = x(k[x]); g(I)


k[x] = y(x)(x); g(x) = x(k[x]); g(I)

k[x] = y(x)(x); g(x) = x(k[x]); k[I];

k(x) = y(x)(x); g(x) = x(k(x)); k(I);

g(x) = x(K); g(I)
k(x) = K; g(x) = x(k(x)); k(I)

g(x) = y(x)(x); g(I);
k[x] = y(x)(x); g(x) = x(k[x]); k[x];


r[x] = y(x); k[x] = r[x](x); g(x) = k[x]; g(I)
r[x] = y(x); k[x] = r[x](x); g(x) = k[x]; r[x](I)

((x) => x(K{x}))(I) |-> K{I}

//
k[x] = y(x)(x); g(x) = x(k[x]); g(I);
k[x] = (DUP; VAR(y); APPLY); g(x) = (k; VAR(x); APPLY); g(I)

k[x] = y(x)(x); g(x) = x(k[x]); g(I)
g(x) = (DUP; VAR(y); APPLY; APPLY); g(I)

#r = N; #f = (x) => x(#r); x = K[#f]; #f(y)
#r = N; #f = (x) => x(#r); x = K[#f]; y(#r)


(l := (x) => K)(N) |-> K{x := N}  is_affine_var(l)
(l := (x) => K)(N) |-> K{x := N}  is_affine((x) => K)
(l := (x) => K)(N) |-> K{x := N}  is_small(N)

f = (x) => g(x);

f = (x) => k[x]; [f(M), f(N)]
[x = M; k[x], x = N; k[x]]

(b) => b(b(false, true), true)
(b) => [b(false, true), b(true, false)]
(b) => b([false, true], [true, false]);

((x) => x(x))((x) => x(x))

f(x) = l[x](r[x]); f(y)
f(x) = x(r[x]); f(y)
f(x) = x(r[x]); y(r[y])

f(x) = x(x); f(y)

f(x) = x(x(x)); I
```

## Low-level Optimizations

```rust
Term, M, N, K ::=
  | x
  | Data | Comp | El(A, l) | Code(A)
  | Layout

  | _(a : A) : B | _(a) = K | M(N)
  | (a : A, B) | (M, N) | M.0 | M.1
  | @(x) : T | (@x = M) | M.@
  | Box(A) | [M] | *M

Γ |- Data is Data

Γ |- A is Data  Γ, a : A |- B is Data
-------------------------------------
Γ |- (_(a : A) : B) is Comp

Γ |- A : Data  Γ |- M is Layout
-------------------------------
Γ |- El(A, M) is Data

Γ, x : (@(x) : T) |- T is Data
------------------------------
Γ |- (@(x) : T) is Data

Γ |- A is Data
-------------------
Γ |- Code(A) : Data

|(a : A) -> B(a)| == [
  l : Length,
  Env : Data $ 0,
  e : El(Env, l),
  f(e : El(Env, l) == e, a : A) : B(a),
];

Bool = Π(
  A : Data,
  @len(@a : El(A, len(a))) : Length,
  @x : El(A, len(x)),
  // x and y have the same type
  y : self(y) -> El(A, len(y));
) -> self(z) -> El(A, len(z));

//



Γ |- Data(len : Length) is Data

Γ |- A : Data(l)
------------------
Γ |- El(A) is Data

Γ, x : self(x) -> T |- T is Data
--------------------------------
Γ |- self(x) -> T is Data

Bool = Π(
  A(l : Length) : Data(l),
  @len(@a : El(A(len(a)))) : Length,
  @x : El(A(len(x))),
  y : self(y) -> El(A(len(y)));
) -> self(z) -> El(A(len(z)));








//

Γ, x : self(x) -> T |- l : Length
Γ, x : self(x) -> T |- T : Data(l)
--------------------------------------
Γ |- self(x) -> T : Data(l{x := ???})



//
Eq(
  A(l : Length) : Data(l),
  @len(@a : A(len(a))) : Length,
  @x : A(len(x)),
  @y : A(len(y))
)



RΠ =
  | [Data, Data, Comp]
RΣ =
  | [Data, Data, Data]

(M + L1, N + L2)

Σ(a : A, B(a)) is Type
code(Type) : Type(0)
code(Σ(a : A + l, B(a) + r)) : Type(l + r)

self(a) ->

U : Data(fun_ptr);
El(T : U) : Type;
len{T}(x : El(T)) : Length;

U = self(T) -> [
  len(a : El(T)) : Length;
  A(l : Length $ 0) : Data(l);
];
El(T) = self(a) -> T.@.A(T.@.len(a));
len{T}(x : El(T)) = T.@.len(x);


(M : (A : Data(l)), N : (B : Data(r))) : Data(l + r)

(M : (A : Type), N : (B : Type))

(self(a) -> T : Data(l)) : Type

Data
El(A : Data, len : Length)

Buffer(n : Nat) : Data;
Buffer(n) =
  n.is(
  | 0 => []
  | 1 + n => [hd : Char, tl : Buffer(n)]
  );

Array : Data;
Array = [n : Nat, Buffer(n)];

U : Data(fun_ptr);
El(T : U) : Type;
len{T}(x : El(T)) : Length;

U = self(T) -> [
  len(a : El(T)) : Length;
  A(l : Length $ 0) : Data(l);
];
El(T) = self(a) -> T.@.A(T.@.len(a));
len{T}(x : El(T)) = T.@.len(x);

dup{A}(x : El(A)) =
  [x $ len(x), x $ len(x)];
// then an optimization would be
dup{A}(x : El(A)) =
  z = len(x);
  [x $ z, x $ z];




//
xchg{A : Type}(x : El(T), y : El(T)) =
  [y $ y.length(), x $ x.length()];

U = [l : Length $ l, A : Data $ l];
xchg{A : U}(x : , y : A) = [y, x];

// source
xchg{A : Type}(x : A, y : A) = [y, x];
// target
xchg{A : Data}(
  len,
  x : self(z) -> A + len(z),
  y : self(z) -> A + len(z)
) = [y, x];



xchg{A, B}(a : A, b : B) = [b, a];
p : [a : A, B];


M.1

[A : Type, B]



Type
Data
Comp

Π(A, B : ...Closure(A, B))

|(a : A) -> B(a)| == [
  Env : Type $ 0,
  e : Env,
  f(e : Env == e, a : A) : B(a),
];

Γ |- ptr
----------------------------------------
Γ |- Ptr(A : Type, B : Closure(A, Type))


-------------------
∅ is Static Context

Γ is Static Context
-------------------------------
Γ, a : A == M is Static Context


Γ is Static Context  Γ |- M : (Π(a : A) : B)
--------------------------------------------
Γ |- ptr(M) : Ptr(A, B)


x = 1;
f(y) = x + y;

f(a : A_f) : B_f;
g(a : A_g) : B_g;

Ptr(A : Type, B : Type) =
  | ["f", A == A_f, B == B_f]
  | ["g", A == A_g, B == B_g];
call{A, B}(f : Ptr(A, B), a : A) : B =
  f.is(
  | ["f", refl(), refl()] => f(a)
  | ["g", refl(), refl()] => g(a)
  );

f(x) =
  g(y) = K;
  M[g];

g(y) = K;
f(x) =

  M[g];

|(a : A) -> B(a)| == [
  Env : Type $ 0,
  captured : Env,
  f : Ptr([e : Env == captured, a : A], B),
];


// internal universe



// simple self type
U : Data(length_size) = Length;
El(A : U, a : El(A, a).T) : [
  len(a : El(A, a).T) : Length;
  T : Data(len(a));
];
El(A, a) =
  len(a : El(A, a).T) : Length = length_size + a.l;
  T : Data(len(a)) = [l : Length <= A, p : Buffer(l)];
  [len, T];

U : Data(length_size) = Length;

U : Data(_);
// this is the type needed to implement such functions

T_len : Data(fun_ptr);
T_El(len : T_len) : Data(fun_ptr);


T_El : ? = self(El) ->
  (A : Data(l), a : El(A, a)) -> [
    len(a : El(A, a).T)
  ]
S(A : U) = self(S) ->
  (T : )
  T : ??? = self(a) -> S(A).El(a);
  [
    len(a : T) : Length,
    El(a : El(a)) : Data(len(a))
  ];

Kind : Kind
Type : Kind


[
  len : Length;
  T : Data(len);
] : Type

T_El(A : U) : ?

@El(A : U, @a : El(A, a).T) : [
  len : Length;
  T : Data(len);
];


// usage
id(A : U, @a : El(A, a)) : self(r) -> El(A, r);


Γ |-

code(self(r) -> )

//
U : Data(_);
T(A : U) : Data(0);

U =
  len(A : U, @a : El(A, a)) : Length;
El(A : U, @a : El(A, a)) : Data(len(A, a));



U : Data(fun_ptr);
Len(T : U) : Length;
El(T : U) : Type;

coe{A, x : A, y}(p : x == y, C(a : A) : Type, c : C(x)) : C(y);
ap{A, B, x, y}(f(a : A) : B, p : x == y) : f(x) == f(y);

ap(f, p) = coe(p, C(z) = f(x) == f(z), refl());

//

id(
  A : Data,
  len(a : El(A, len(a))) : Length,
  x : El(A, len(x))
) : (y : El(A, len(y)));
id(
  A(l : Length) : Data(l),
  len(a : A(len(a))) : Length,
  x : A(len(a)),
) : (y : A(len(a)))

Buffer(l : Length) : Data(l);


Array : Data = [
  len : Length,
  buf : Buffer(len)
];

//
id(
  A(len(a : A(len, a)) : Length, a : A(len, a)) : Data(len(a)),
  len(a : A(len, a)) : Length,
  x : A(len, x),
) : (y : A(len, y))
//
id(
  A(l : Length) : Data(l),
  len(a : A(len(a))) : Length,
  x : A(len(a)),
) : (y : A(len(a)))

//
```

## Low-level Optimizations

```rust
Term, M, N, K ::=
  | x
  | Data | El(A, l) | Code(A)
  | Layout | ptr | idx
  | _(a : A) : B | _(a) = K | M(N)
  | (a : A, B) | (M, N) | M.0 | M.1
  | @(x) : T | (@x = M) | M.@
  | Box(A) | [M] | *M

Γ |- Data is Data

Γ |- A is Data  Γ, a : A |- B is Data
-------------------------------------
Γ |- (a : A, B) is Data

Γ |- A is Data  Γ, a : A |- B is Data
-------------------------------------
Γ |- (_(a : A) : B) is Comp

Γ |- A : Data  Γ |- M is Layout
-------------------------------
Γ |- El(A, M) is Data

Γ |- A is Data
-------------------
Γ |- Code(A) : Data

Γ, x : (@(x) : T) |- T is Data
------------------------------
Γ |- (@(x) : T) is Data

∅, a : A |- K : B(a)
--------------------------------
∅ |- (_(a) = K) : (_(a : A) : B)

Bool : Data = (Π(l, A : Data, t : El(A, l), f : El(A, l)) : A);


|(a : A) -> B(a)| == [
  l : Length,
  Env : Data $ 0,
  e : El(Env, l),
  f(e : El(Env, l) == e, a : A) : B(a),
];


Bool : Type;
true : Bool;
false : Bool;

Bool = Π(
  A : Data,
  len(a : El(A, len(a))) : Length,
  t : El(A, len(t)),
  f : El(A, len(f))
) : (r : El(A, len(r)));
// make the caller deal with it
Bool = (
  b : Π(
    A : Data,
    lt : Length,
    lf : Length,
    t : El(A, lt),
    f : El(A, lf),
  ) : El(A, b(Length, _, _, lt, lf))
);

// this works for any named nodes
Bool = Π(
  l,
  C(b : Bool) : Data,
  t : El(C(true), l),
  f : El(C(false), l)
) : El(C(Bool.self), l);

true = λ(C, t, f) = t;
// same as
Bool = ι(b) : Π(
  C(b : Bool) : Type,
  t : C(true),
  f : C(false)
) : C(b);

size{A : Data}(a : A) : Layout;

El(Code([a : A, b : B]), l)
El(Code([a : A, b : B]), l)
El(Code(T), l)

Γ |- M : (Π(a : A) : B)  Γ |- N : A
-----------------------------------
Γ |- M(N) : B{a := N}


id = λ(A : Data, )

Size(M : [
  len : Nat,
  buf : Buffer(len)
]) == Size(M.len : Nat) + Size(M.buf : Buffer(M.len))


//

Γ |- A is Type
-------------------
Γ |- Code(A) : Type

Γ |- A : Type  Γ |- L is Length
-------------------------------
Γ |- El(A, L) is Type

Γ |- M : A
---------------------
Γ |- size(M) is Length

Γ |- A : Data  Γ, a : A |- L is Length
--------------------------------------
Γ |- [a : A, size(a) == L] is Data

Γ |- M : A  Γ |- size(M) == L
------------------------------
Γ |- M : [a : A, size(a) == L]


El(Code(T), L) |-> [a : A, size(a) == L]


// Lean 4 source
inductive List (α : Type u) where
  | nil : List α
  | cons (head : α) (tail : List α) : List α

// lowered to Raphael
List(α : Type(u)) =
  | (nil : List(α))
  | (cons(head : α, tail : Box(List(α))) : List(α))
// additional {human,LLM} written definition
List_Repr(α : Type(u)) =
  | nil
  | cons(head : α, tail : List_Repr(α))
  | link(to_ : Box(List_Repr(α)))
  % ∀(l) -> link(l) == l
// then you can prove a theorem like this
list_eq_list_repr : Equiv(List, List_Repr);
// and tell the compiler to use the fast one
// use List_repr for List by list_eq_list_repr


id(
  A : Data,
  len(a : El(A, len(a))) : Length,
  x : El(A, len(x))
) : (y : El(A, len(y)))

Buffer(len : Nat) : Data;
Buffer(len) =
  len.is(
  | 0 => Code([])
  | 1 + len => Code([hd : Char, tl : El(Buffer(len), len)])
  );

Array = Code([
  len : Nat,
  buf : El(Buffer(len), len)
]);
len(a : El(Array, len(a))) = a.len;
id(Array, len, x : El(Array, len(x)) = _);

Value : Type;
Block : Type;

Value =
  | [n : Nat63, b : Bool == true]
  | [p : Aligned_Box(2, Block), b : Bool == false];
Block = [
  tag : Bool,
  len : Nat63,
  buf : Vec(len, Value)
];


//

id(
  A : Data,
  len(a : El(A, len(a))) : Length,
  x : El(A, len(x))
) : (y : El(A, len(y)))

id(A : Data, x : A) : A;


Γ, x : A |- T : Type
Γ |- A == self(x) -> T
------------------------
Γ |- self(x) -> T : Type

Γ, x == M |- K : T
Γ |- M == fix(x) => K
-------------------------------
Γ |- fix(x) => K : self(x) -> T


// from
Γ, x : T |- T : Type
--------------------
Γ |- (x : T) : Type

Γ |- A : Type  Γ, a : A |- B : Type
-----------------------------------
Γ |- (Π(a : A) : B) : Type

// to
Γ, f(a : A) : B, a : A |- A : Type
Γ, f(a : A) : B, a : A |- B : Type
----------------------------------
Γ |- (f(a : A) : B) : Type

Γ, p : A × B |- A : Type
Γ, p : A × B |- B : Type
------------------------
Γ |- (p : A × B) : Type

//
T = (a : C(a)); K



B : Type
T(M : A) |-> B{a := M}

A : Type
B : Type

A() : A()

(p : A == B)
// by funext
p() -> A() == coe(p, C(T) = T(), B())

p() -> A() == B()

Π(C(T : Type) : Type()) -> C(A) -> C(B)

A == B


A(l) : f(1 + l)




A : Data;
T = (a : El(A, len(a)));
// to
T = [a : El(A, len(a)), ()];

(λ(x) => K)(N) |-> K{x := N}



// but the following doesn't work
id(
  A : Data,
  len(a : El(A, len(a))) : Length,
  x : El(A, len(x))
) : (y : El(A, len(y)))
// it needs to be
id(
  A : Data + 0,
  len(a : A + len(x)) : Length,
  x : A + len(x)
) : A + len(id(A, len, x))

id(
  A : Data,
  len(a : El(A, len(a))) : Length,
  x : El(A, len(x))
) : (y : El(A, len(y)), ())


L(A : Type) : Type;
L(A) = Σ(X : Type, λ(X) => Σ(A, λ(a) => L(X)));

R(A : Type) : Type;
R(A) = Σ(X : Type, λ(X) => Σ(A, λ(a) => R(X)));

μ(F) -> λ(T) => [X : Type, a : A, b : F(X)]

L(A) == R(A)
λ(X) => Σ(A, λ(a) => L(X)) == λ(X) => Σ(A, λ(a) => R(X))
L(X) == R(X)


//
F(T : Type, g : Type -> Type) : Type;
F(T, g) => [a : T, b : F(g(T))(g)];


L(A : Type) : Type;
L(A) = Σ(X : Type, λ(X) => Σ(A, λ(a) => L(X)));

R(A : Type) : Type;
R(A) = Σ(X : Type, λ(X) => Σ(A, λ(a) => R(X)));



Type, A, B ::=
  | Type
  | Π(a : A) -> B
  | Σ(a : A) -> B
Term, M, N, K ::=
  | x | code(A)
  | λ(x) => K | M(N)
  | [M, N] | M.0 | M.1

id(A : Data, x : A) : A;

(a : A) -> B
[a : El(A, len(a)), n : Nat64]

[a : A + len(a), Nat64]

Π(A : Data, l : Length, x : El(A, l)) : El(A, l)
Π(
  A : Data,
  len(a : A) : Length,
  l : Length,
  x : A # len(x)
) : El(A, l)


λ(
  A : Data,
  len(a : A # len(a)) : Length,
  x : (a : A # len(a)),
  y : (a : A # len(a))
) => x == y

Eq(A : Data, x : A, y : A) =
  Π(C(a : A) : Data) -> C(x) -> C(y);



Mem(n : Nat64, A : Type) : Type;
Mem(n, A) =
  n.is(
  | 0 => ()
  | 1 + n => (hd : A, tl : Mem(n, A))
  );

(
  A : Data,
  len : (len(a : A # len(a)) : Length),
  x : (a : A # len(a)),
  y : (a : A # len(a)),
  p : x == y,
) -> len(x) == len(y)

len(b : Bool # len(b)) : Length;
len(true) = 0;
len(false) = 1;

true : (b : Bool # len(b));
false : (b : Bool # len(b));

(
  A : Data,
  len(a : A # len(a)) : Length,
  x : (a : A # len(a)),
  y : (a : A # len(a)),
  p : x == y,
) -> len(x) == len(y)

List(A : Type) : Type;
List(A) = [
  tag : Bool,
  payload : tag ? Unit : A
];


Π(a : ι(a) -> A) -> B
Σ(a : ι(a) -> A) -> B

id(A : Data, x : A # word) : A # word;
// just the size given that it's empty
x = id(String, "");
// also same size
x = id(Box(Nat), 0);

id(A : Data $ 0, x : A # word) : A # word;
// same as
id(
  A : Data $ 0,
  x : (a : A | len(A, a) == word)
) : (a : A | len(A, a) == word);


// just the size given that it's empty
x = id(String, "");
// also same size
x = id(Box(Nat), 0);


//
id(A : Data $ 0, x : A # word) : A # word;
// same as
id(
  A : Data $ 0,
  x : (a : A | len(A, a) == word)
) : (a : A | len(A, a) == word);


// A # L, same as El(A, L)
// in this design A is always erasable, size 0
id(A : Data, x : A # ptr) : A # ptr;

// but you need to be explicit when it's polymorphic
id(
  A : Data,
  len(a : A # len(a)) : Length,
  x : A # len(x)
) : (y : A # len(y));
// vs, len is part of the Data
id(A : Data, x : A) : A;
// but then Data is not erasable
// and to be specific you need to constraint further
id(
  A : Data $ 0, // can be erasable because len is static
  x : A & A.len(x) == ptr
) : (y : A & A.len(y) == ptr);


(A : Data).len(a : A)

(Data).len |-> λ(T) => ptr
(Bool).len |-> λ(b) => 1
(Π(a : A(a)) -> B(a)).len |-> λ(f) => ptr
(Σ(a : A(a)) -> B(a)).len |-> λ(p) =>
  [x, y] = p;
  A(x).len(x) + B(x).len(y)

μ(Nat) => Unit + Nat

(A + B).len |-> λ(x) =>
  1 + x.is(
  | [true, a] => A.len(a)
  | [false, b] => B.len(b)
  );

(μ(Nat) => Unit + Nat).len |->
  Nat.len(n : Nat) : Layout;
  Nat.len(n) =
    n.is(
    | [true, []] => 0
    | [false, n] => 1 + Nat.len(n)
    )



(Data).len |-> λ(T) => ptr
(Π(a : A) -> B(a)).len |-> λ(f) => ptr
(Σ(a : A) -> B(a)).len |-> λ(p) =>
  [x, y] = p;
  A.len(x) + B(x).len(y)



(Data).len(M) |-> ptr
(ι : T)
(Π(a : A) -> B(a)).len(M) |-> ptr
(Σ(a : A) -> B(a)).len(M) |->
  A.len(M.0) + B(M.0).len(M.1)

// Data is effectively
Data == [A : Data $ 0, len(a : A) : Length];


Buffer(n : Nat, A : Type) : Type;
Buffer(0, A) = ();
Buffer(1 + n, A) = (hd : A, tl : Buffer(n, A));

len{n, A}(buf : Buffer(n, A)) : Length;
len{0, A}(()) = 0
len(1 + n, A)((hd, tl)) = A.len(hd) + len{n, A}(tl)


(A : Data).len(a : A) : Length;

reuse{A : Type, B : Type}(
  x : A,
  y : B,
  p : A.len(x) == B.len(y)
) : B;


// unboxed unit, 0 size
()
().len(()) = 0
// unboxed sigma
(a : A, B)
(a : A, B).len((a, b)) = A.len(a) + B(a).len(b)
// boxed unit, word size
[]
[].len(_) = word
// boxed sigma
[a : A, B]
[a : A, B].len(_) = word


//
id(
  A : Data,
  len(a : A # len(a)) : Length,
  x : A # len(x)
) : A;


C_Bool = Π(C : Data, t : C, f : C) -> C;

c : C_Bool;
c == λ(C, t, f) => (t | f);

f(c : C_Bool) = c(c_true, c_true);

f = (λ(c : C_Bool) => c(c_true, c_true));

f = (λ(c : C_Bool == (λ(C, t, f) => (t | f))) => c(c_true, c_true))

f = (λ(c : C_Bool == (λ(C, t, f) => (t | f))) => c(c_true, c_false))


id(A : Type, a : A) = a;

id(String, "a")
id(Nat, 1)

id(
  A : Type == (String | Nat) as K,
  a : A == ("a" | 1) as K
) = a;


f = (λ(x == (M | N) as K) => x(y));

a = f(M);
b = f(N);


f = (λ(c : C_Bool == (λ(C, t, f) => (t | f) as c(C, t, f))) =>
  ((c_true | c_false) as K))

λ(c : C_Bool) => (f(c_true)) | (f(c_false));


f(x, y) = x(y, y);

f(M)


f(x == a | b) = _;

λ(a, b) =>
  x = f(a);
  z = f(a);
  _

λ(x) => λ(y) => K

λ(x) => [x, λ(x, y) => K]

λ(x : A, y : A == x) =>
  _

I_Bool = true | false;

```
