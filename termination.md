```rust
// TODO: make this tarski

Term, M, N, K ::=
Type, A, B ::=
  | Type(l) | Lift(A) | lift(M) | lower(M)
  | x | (x : A) -> B | (x : A) => K | M(N)

Type(l) : Type(1 + l);
Lift(A : Type(l)) : Type(1 + l);
lift(M : A) : Lift(A);
lower(M : Lift(A)) : A;

((x : A : Type(l)) -> (B : Type(l))) : Type(l);
((x : A) => K)(N) |-> K{x := N}


id : (A : Type(1), x : Lift(A)) -> Lift(A);


El(Lift(A)) is Type(0)

id(Type(0)) : (x : El(Lift(Type(0)))) -> El(Lift(Type(0)))


Term, M, N, K ::=
Type, A, B ::=
  | Type(l) | Lift(A)
  | x | (x : A) -> B | (x : A) => K | M(N)

Type(l) : Type(1 + l);
Lift(A : Type(l)) : Type(1 + l);
Lift(Type(l)) |-> Type(1 + l);
Lift((x : A) -> B) |-> (x : Lift(A)) -> Lift(B);

((x : A : Type(l)) -> (B : Type(l))) : Type(l);
((x : A) => K)(N) |-> K{x := N}

// erasure


C : (b1 : Bool@1) -> Type@1 : Type@2;
C(b1) = b1(Bool@0, b1, true)(Type@0, A : Type(1), B : Type(1));


M ∈ T

*M : A ∈ T

*id : (A : Type(0), A_i : (x : A) -> Type(0)) ->
  (x : A, x_i : A_i(x)) -> A_i(x);

(A : Type(0), x : Lift(A)) -> Lift(A)
id ∈ Id@1

C : (id : Id@1) -> (A : Type(1)) -> Type(1);
C(id, id_i, A) = id_i();

id1(Type(0), T : Type(1))

id1 : (A : Type(1), x : Lift(A)) -> Lift(A);
id2 : (A : Type(2), x : Lift(A)) -> Lift(A);

(x : A : Type(1 + l)) -> (B : Type(l)) == (x : A $ 0) -> B

M : A : Type(l)
M : A : Type(1 + l)


C : (id : Id@1) -> Type(1) : Type(2);
C(id) = id1(Type(0), T : Type(1));

C : (id : Id@2) -> Type(2) : Type(3);
C(id) = lift(id(Type(2), T : Type(2)));



R(x) == 0
R(Type(l)) == l * ω
R((x : A) -> B) == max(R(A), R(B)) + 1

M : (x : A) -> B
N : A

M(N) : N{x := N}

Type(0) == 0q
Type(0) -> Type(0) == 1

(A : Type(0)) -> (x : A) -> A

ω + 2


f : (C : (id : Id@1) -> Type(1) : Type(2)) -> (_ : Type(2)) = _;

T(Type(0)) |-> Type(0)
T((x : A) -> B)
T(A : Type(l)) |->


C(n : Nat@1 : Type(2)) : Type(1);

// to
C : (n : Nat@2 : Type(3)) -> Type(1);


f : (C : (id : Id@1) -> Type(1) : Type(2)) -> (_ : Type(2)) = _;
f((id : Id@2) => id(Type(1), T))

f : (C : (id : Id@2) -> Type(1) : Type(3)) -> (_ : Type(?)) = _


f((id : Id@2) => id(Type(1), T))



(id : Id@1) => id(Type(0), T) : Type(1);

(id : Id@2) => id(Type(1), T) : Type(1)

C :



(id1 : Id@1 : Type(2)) -> Type(1)


Id@l : Type(1 + l) = (A : Type(l), x : Lift(A)) -> Lift(A);


(id1 : Id@1 : Type(2)) -> (Type(1) : Type(2))



M : A : Type(2)



(id1 : Id@1 : Type(2)) -> Type(1) : Type(2)

(A : Type(1), x : Lift(A)) -> Lift(A)

(id2 : Id@2 : Type(3)) -> Type(1) : Type(3)

(id1 : Id@1 : Type(2)) -> Type(1) : Type(2)

(id1) => id1(Type(0), _) : Type(1);

(id1 : Id@1 : Type(2)) -> Type(1) : Type(2)

f = (id1 : Id@1) => id1(Id@0, x : Id@1);

f = (id1 : Id@1, id2: Id@2) => id2(Id@1, )

id1 : Lift(Id0) = _;
Lift(Id0) == Id1

id1 : Id1 = (A : Type(1), x : Lift(A)) => x;



 : Id1

C{x}  (lift(x) ∉ C) (fv(C) == ∅)


x : A; (M : B)  (x ∉ M)

x : A; C{x}

x : A; (C{x} : B)  (x ∉ B)

x : A; (C{x} : B)  (x ∉ B)


((A : Type) => (x : A) => x)(Type)(Type)

(
  (A : Type) = Type;
  ((x : A) => x)
)(Type)

U(l : Level) : Type;
El[l](T : U(l)) : Type;
Lift[l](A : U(l)) : U(1 + l);
down[l, T : U(l)](x : El(Lift(T))) : El(T);

univ(l : Level) : U(1 + l);
lift[l](T : U(l)) : U(1 + l);
pi[l](A : U(l), B : (a : El(A)) -> U(l)) : U(l);

U(l) = self(T) -> (
  C,
  u : C(1 + l, univ(l)),
  l : (T : U(l)) -> C(1 + l, lift(T))
  p : (A : U(l), B : (a : El(A)) -> U(l)) -> C(l, pi(A, B))
) -> C(l, T);
El_X[l](T) : Type =
  T.@(
    (_) => Type,
    l.is(
    | 0 => False,
    | 1 + l => U(l)
    ),
    (A, B) => (a : El(A)) -> El(B(a)),
  );
El[l](T) =
  [
    x : El_X(T),
    d :
      T.is(

      )
    ];
Lift[l](T) =
  T.@(
    (l, _) => U(1 + l),
    univ(1 + l),
    _,
    (A, B) =>
      pi(Lift(A), (a : El(Lift(A))) => _)
      _ : U(1 + l)
    _ : U(1 + 1 + l)
  );

//
U(l : Level) : Type;
El[l](T : U(l)) : Type;

univ(l : Level) : U(1 + l);
pi[l](A : U(l), B : (a : El(A)) -> U(l)) : U(l);

El_Raw[l](T : U(1 + l)) : Type;
Lift[l](T : U(l)) : U(1 + l);
down[l, T : U(l)](x : El(Lift(T))) : El(T);

U(l) = self(T) -> (
  C,
  u : C(1 + l, univ(l)),
  p : (A : U(l), B : (a : El(A)) -> U(l)) -> C(l, pi(A, B))
) -> C(l, T);
El[l](T) =
  l.is(
  | 0 => False
  | 1 + l =>
    [
      r : El_Raw[l](T),
      d : (A : U(l), p : T == Lift[l](A)) -> El[l](A)
    ]
  );

univ(l) = (C, u, p) => u;
pi[l](A, B) = (C, u, p) => p(A, B);

El_Raw[l](T) =
  T.@(
    (_) => Type,
    U(l),
    (A, B) => (a : El(A)) -> El(B(a)),
  );
Lift[l](T) =
  T.@(
    (l, _) => U(1 + l),
    univ(1 + l),
    (A, B) => pi(Lift(A), (a : El(Lift(A))) => Lift(B(down(a))))
  );
down[l, T](x) = x.d(T, refl());


Lift((A : U(0)) -> (a : El(Lift(A))) -> El(Lift(A)))

down[l, T](x : El(Lift(T))) =
  T.@(
    (l, T) => (x : El(Lift(T))) -> El(T),
    _,
    (A, B) => (x : (a : El(Lift(A))) -> El(Lift(B(down(a))))) =>
      x(lift(a)) : El(Lift(B(down(lift(a)))))
      _ : (a : El(A)) -> El(B(a))
  )(x)


(A : U(1)) ->
  A = down(A);
  (a : Lift(El(Lift(down(A)))) ->
  a = down(a);
  Lift(El(Lift(A))))


//

U(l : Level) : Type;
El[l](T : U(l)) : Type;

univ(l : Level) : U(1 + l);
pi[l](A : U(l), B : (a : El(A)) -> U(l)) : U(l);



U(l) = self(T) -> (
  C : (l : Level, T : U(l)) -> Type,
  u : C(1 + l, univ(l)),
  p : (A : U(l), B : (a : El(A)) -> U(l)) -> C(l, pi(A, B))
) -> C(l, T);
El[l](T) =
  T.@(
    (_) => Type,
    l.is(
    | 0 => False
    | 1 + j => U(j)
    );,
    (A, B) => (a : El(A)) -> El(B(a)),
  );

univ(l) = (C, u, p) => u;
pi[l](A, B) = (C, u, p) => p(A, B);

Lift[l](T : U(l)) : U(1 + l);
Down[l](T : U(1 + l)) : U(l);
lift[l, T : U(l)](x : El(T)) : El(Lift(T));
down[l, T : U(l)](x : El(Lift(T))) : El(T);

down[1 + l, univ(l)] : (x : El(Lift(univ(l)))) -> El(univ(l))
// same as
down[1 + l, univ(l)] : (x : El(univ(1 + l))) -> El(univ(l))
// same as


Lift[l](T) =
  T.@(
    (l, _) => U(1 + l),
    univ(1 + l),
    (A, B) => pi(
      Lift(A),
      (a : El(Lift(A))) => Lift(B(down(a)))
    )
  );
Down[l](T) =
  T.@(
    (l, _) => U(l),
    univ(l),
    (A, B) =>
      pi(
        Down(A),
        (a : El(Down(A))) => Down(B(lift(a)))
      )
  );
lift[l, T](x) =
  T.@(
    (l, T) => (x : El(T)) -> El(Lift(T)),
    (x : El(univ(l))) => Lift(x),
    (A, B) => (x : El(pi(A, B))) =>
      (a : El(Lift(A))) => lift(x(down(a)))
  )(x);
down[l, T](x) =
  T.@(
    (l, T) => (x : El(Lift(T))) -> El(T),
    (x : El(univ(1 + l))) => down(x),
    _
  )


U(l : Level) : Type;
El[l](T : U(l)) : Type;

univ(l) : U(1 + l);
pi[l](A : U(l), B : (a : El(A)) -> U(l)) : U(l);

// Hurken's
Not : Type -> Type
Not(A) = A -> ⊥

P : Type -> Type
P(A) = A -> Type

U : Type
U = (X : Type) -> (P(P(X)) -> X) -> P(P(X))

tau : P(P(U)) -> U
tau(t) = X => f => p -> t(x => p(f(x(X)(f))))

sigma : U -> P(P(U))
sigma(s) = s(U)(tau)

Delta : P(U)
Delta(y) = Not((p : P(U)) -> sigma(y)(p) -> p(tau(sigma(y))))

Omega : U
Omega = tau(p => (x : U) -> sigma(x)(p) -> p(x))

D : Type
D = (p : P(U)) -> sigma(Omega)(p) -> p(tau(sigma(Omega)))

lem1 : (p : P(U)) -> ((x : U) -> sigma(x)(p) -> p(x)) -> p(Omega)
lem1(p)(h) = h(Omega)(x => h(tau(sigma(x))))

lem2 : Not(D)
lem2 = lem1(Delta)(x => h2 => h3 =>
  h3(Delta)(h2)(p => h3(y => p(tau(sigma(y))))))

lem3 : D
lem3(p) = lem1(y => p(tau(sigma(y))))

boom : ⊥
boom = lem2(lem3)

//
Not(A : Type) : Type = A -> ⊥;
P(A : Type) : Type = A -> Type;
U : Type = (X : Type) -> (P(P(X)) -> X) -> P(P(X));
tau(t : P(P(U))) : U = X => f => p =>
  t(x => p(f(x(X)(f))));
sigma(s : U) : P(P(U)) = s(U)(tau);
Delta : P(U) = (y) =>
  Not((p : P(U)) -> sigma(y)(p) -> p(tau(sigma(y))));
Omega : U = tau(p => (x : U) -> sigma(x)(p) -> p(x));

D : Type = (p : P(U)) -> sigma(Omega)(p) -> p(tau(sigma(Omega)));
lem1(p : P(U))(h : (x : U) -> sigma(x)(p) -> p(x)) =
  h(Omega)(x => h(tau(sigma(x))));
lem2 : Not(D) = lem1(Delta)(x => h2 => h3 =>
  h3(Delta)(h2)(p => h3(y => p(tau(sigma(y))))));
lem3 : D = (p) => lem1(y => p(tau(sigma(y))));
boom : ⊥ = lem2(lem3);

//
P1(A : Type(1)) : Type(1) = A -> Type(0);
U1 : Type(1) = (X : Type(0)) ->
  (P(P(⇑X)) -> ⇑X) -> P(P(⇑X));

Not(A : Type(2)) : Type(2) = A -> ⊥;
P(A : Type(2)) : Type(2) = A -> Type(1);
U : Type(2) = (X : Type(1)) ->
  (P(P(⇑X)) -> ⇑X) -> P(P(⇑X));

tau(t : P(P(U))) : U = X => f => p =>
  t((x : U) => p(f(x(X)(f))));
sigma(s : U) : P(P(U)) = s(U1)(tau);

D : Type(2) = (p : P(U)) -> ⇑sigma(Omega)(p) -> ⇑p(tau(sigma(Omega)));
lem1(p : P(U))(h : (x : U) -> ⇑sigma(x)(p) -> ⇑p(x)) =
  h(Omega)(x => h(tau(sigma(x))));
lem2 : Not(D) = lem1(Delta)(x => h2 => h3 =>
  h3(Delta)(h2)(p => h3(y => p(tau(sigma(y))))));
lem3 : D = (p) => lem1(y => p(tau(sigma(y))));
boom : ⊥ = lem2(lem3);

//
Not(A : Type(2)) : Type(2) = A -> ⊥;
P(A : Type(2)) : Type(2) = A -> Type(1);
U : Type(2) = (X : Type(1)) ->
  (P(P(⇑X)) -> ⇑X) -> P(P(⇑X));

tau(t : P(P(U))) : U = X => f => p =>
  t((x : U) => p(f(x(X)(f))));
sigma(s : U) : P(P(U)) = s(U1)(tau);

Omega : U = tau(p => (x : U) -> sigma(x)(p) -> p(x));

sigma(Omega) : P(P(U))

s = Omega;
s(U1)(tau)

//
U(l : Level) : Type;
El[l](T : U(l)) : Type;
El_T[l](T : U(l)) : Type;
Lift[l](A : U(l)) : U(1 + l);

univ(l : Level) : U(1 + l);
arrow[l](A : U(l), B : U(l)) : U(l);
pi[l](A : U(1 + l), B : El(arrow(A, univ(l)))) : U(1 + l);

U(l) = self(T) -> (
  C : (l : Level, T : U(l)) -> Type,
  u : C(1 + l, univ(l)),
  a : (A, B) -> C(l, arrow(A, B)),
  p : (A, B) -> C(1 + l, pi(A, B))
) -> C(l, T);
El[l](T) = [
  x : El_T(T),
  n : El(Lift(T))
];
El_T[l](T) =
  T.@(
    (_) => Type,
    l.is(
    | 0 => False
    | 1 + j => U(j)
    ),
    (A, B) => (a : El(A)) -> El(B),
    (A, B) => (a : El(A)) -> El(B(a)),
  );
Lift[l](T) =
  T.@(
    (l, _) => U(1 + l),
    univ(1 + l),
    (A, B) => arrow(Lift(A), Lift(B)),
    (A, B) =>
      _ :
      pi(Lift(A), (a : El(Lift(A))) => _)
  )
El[l](T) =
  T.@(
    (_) => Type,
    l.is(
    | 0 => False
    | 1 + j => U(j)
    ),
    (A, B) => (a : El(A)) -> El(B(a)),
  );

univ(l) = (C, u, p) => u;
pi[l](A, B) = (C, u, p) => p(A, B);

Id0 = (A : Type(0), x : Lift(A)) -> Lift(A);

//
U(l : Level) : Type;
El_U[l](T : U(l)) : Type;

S(l : Level) : Type;
El_S[l](T : S(l)) : Type;
Lift_S[l](T : S(l)) : S(1 + l);

U_univ(l : Level) : U(1 + l);
U_pi(A : U(l), B : (a : El_U(A)) -> U(l)) : U(l);
U_simple(T : S(l)) : U(l);

S_arrow(A : S(l), B : S(l)) : S(l);
S_wrap(T : U(l)) : S(l);

U(l) = self(T) -> (
  C : (l : Level, T : U(l)) -> Type,
  u : C(1 + l, U_univ(l)),
  p : (A : U(l), B : (a : El(A)) -> U(l)) -> C(l, U_pi(A, B))
) -> C(l, T);
El_U[l](T) =
  T.@(
    (_) => Type,
    l.is(
    | 0 => False
    | 1 + j => U(j)
    ),
    (A, B) => (a : El(A)) -> El(B(a)),
  );
S(l) = self(T) -> (
  C : (T : S(l)) -> Type,
  a : (A : S(l), B : S(l)) -> C(S_arrow(A, B))
  w : (j : Level, T : U(j + l)) -> C(S_wrap(T)),
) -> C(T);

//
U : Type;
El(T : U) : Type;

nat : U;
arrow(A : U, B : U) : U;

U = self(T) -> (
  C : (T : U) -> Type,
  n : C(nat),
  a : (A, B) -> C(arrow(A, B))
) -> C(T);
El(T) =
  T.@(
    (_) => Type,
    Nat,
    (A, B) => (a : El(A)) -> El(B),
  );

nat = (C, n, a) => n;
arrow(A, B) = (C, n, a) => a(A, B);

//

id = (a) => a;
true = (t, f) => t;
zero = (z, s) => z;
one = (z, s) => s(zero);
two = (z, s) => s(one);

Bool : Type;
true : Bool;
false : Bool;
G_true(b : Bool) : Grade;
G_false(b : Bool) : Grade;

Bool = self(b) -> (
  C : (b : Bool) -> Type $ (0, 3),
  t : C(true) $ (_, ),
  f : C(false)
) -> C(b);

// strong fix
T : Type;
T = (x : T $ 1 + x(x)) -> Grade;
// fails
x : T = (x) => x(x : 1 + x(x));



(M : (A : Type(1)) -> (a : Lift(A)) -> Lift(A))(Type(0))

//

Bool = (A : Type(1)) -> (t : A) -> (f : A) -> A;
// falils because Bool : Type(2) and it was expecting Type(0)
f = (b : Bool) => (x : b(Type(0))(Bool)(Nat)) => _;


Bool = self(b) -> (A : Type) ->
  (t : A $ b(Grade)(1, 0)) ->
  (f : A $ b(Grade)(0, 1)) -> A;


true : Bool = (A) => (t) => (f) => t;
false : Bool = (A) => (t) => (f) => f;

b : Bool;
b(Grade)(1)(0) : Grade
fix(fix)
((x $ 2 + fix(fix)) => 2 + x(x))(fix)
x $ 2 + fix(fix) = fix; 2 + x(x)

fix $ fix(fix) = (x $ 2 + fix(fix)) => 2 + x(x);
x $ 1 + fix(fix) = fix; 2 + fix(x)




// cannot do this because of the context
x $ 2 = fix;
x(x)



fix(fix, fix)

fix $ 1 + (x $ 2 + x(x) = fix; 2 + fix(fix)) =
  (x $ 2 + x(x)) => 2 + x(x);

fix $ 1 + fix(fix) = (x $ 1 + x(x)) => 2 + x(x); fix(fix)

// if the Grade depends on the let
fix(fix)
x $ 1 + x(x) = fix; 1 + x(x)
x $ x(x) = fix; 1 + fix(x)
x $ fix(fix) = fix; 1 + fix(x)
x $ 1 + stuff = fix; 1 + fix(fix)



Grade, G ::=
  | 0 | 1 | G + G


//

Term, M, N, K, G ::=
  | 0 | 1 + G
  | x | x $ G = N; K
  | (x $ G) => K | M(N)


S(0) |-> 0
S(1 + G) |-> S(G)
S(x) |-> 0
S(x $ G = N; K) |-> S(G) + (S(N) * T(G)) + S(K)
S((x $ G) => K) |-> S(G) + S(K)
S(M(N)) |-> S(M) + (S(N) * ω²)

T(0) |-> 0
T(1 + G) |-> 1 + T(G)
T(N) |-> ω

x $ G¹ + G² = N; C[x] |-> x $ G² = N; C[N]  (C.G == G¹)
x $ 0 = N; K |-> K{x := N}  (x ∉ K)
L[(x $ G) => K](N) |-> L[x $ G = N; K]

//

x $ G¹ + G² = N; C[x] |-> x $ G² = N; C[N]  (C.G == G¹)

S(G¹ + G²) + (S(N) * T(G¹ + G²)); S(C[x])
S(G²) + (S(N) * T(G²)); S(C[N])


x $ 0 = N; y $ G = N; K
(S(N) * T(G)) + S(G) + S(K)

// beta
((x $ G) => K)(N) |-> x $ G = N; K

S((x $ G) => K) + (S(N) * ω²)
S(G) + (S(N) * ω²) + S(K)
//
S(G) + (S(N) * T(G)) + S(K)



Context, C ::=
  | _ (1)
  | 1 + C  (_.G == C.G)
  | x $ C = N; K (_.G == C.G)
  | x $ G = C; K (_.G == C.G * G)
  | x $ G = N; C (_.G == C.G)
  | (x $ C) => K (_.G == C.G)
  | (x $ G) => C (_.G == C.G)
  | C(N) (_.G == C.G)
  | M(C) (_.G == undefined)


//
Term, M, N, K ::=
  | x | x = N; K
  | (x) => K | M(N)

x = N; C[x] |-> C[N]  (count(x) ∈ C[x] == 1)
x = N; K |-> K  (x ∉ fv(K))
L[(x) => K](N) |-> L[x = N; K]

//
Term, M, N, K, G ::=
  | 0 | 1 + G
  | x | x $ G = N; K
  | (x $ G) => K | M(N)


Term : Type;
zero : Term;
succ(g : Term) : Term;
var(x : Var) : Term;
let(x : Var, g : Term, n : Term, k : Term) : Term;
lam(x : Var, g : Term, k : Term) : Term;
app(m : Term, n : Term) : Term;

S(t : Term) : Ordinal;
wnhf(t : Term) : Ordinal;

S(t) =
  t.is(
  | zero => 0
  | succ(g) => S(g)
  | var(x) => 0
  | let(x, g, n, k) => S(g) + (S(n) * wnhf(g)) + S(k)
  | lam(x, g, k) => S(g) + S(k)
  | app(m, n) => S(m) + (ω * S(n))
  );
wnhf(t) =

//

Term, M, N, K, G ::=
  | 0 | 1 + G
  | x | x $ G = N; K
  | (x $ G) => K | M(N $ G)

S(0) |-> 0
S(1 + G) |-> S(G)
S(x) |-> 0
S(x $ G = N; K) |-> S(G) + (S(N) * T(G)) + S(K)
S((x $ G) => K) |-> 1 + S(G) + S(K)
S(M(N) $ G) |-> S(M) + (S(N) * T(G)) + S(G)

T(0) |-> 0
T(1 + G) |-> 1 + T(G)
T(N) |-> ω + S(N)


x $ C.G + G = N; C[x] |-> x $ G = N; C[N]
x $ 0 = N; K |-> K{x := N}  (x ∉ K)
L[(x $ G) => K](N $ G) |-> L[x $ G = N; K]


x $ C.G + G = N; C[x] |-> x $ G = N; C[N]

S(C.G + G) + (S(N) * T(C.G + G)) + S(C[x]) |->
S(G) + (S(N) * T(G)) + S(C[N])



S(C.G)


lem1? : S(C.G + G) == S(C.G) + S(G)
lem2? : T(C.G + G) == T(C.G) + T(G)
lem3? : S(C[x]) + (S(N) * T(C.G)) == S(C[N])


Context, C ::=
  | _ (_.G == 1)
  | 1 + C  (_.G == C.G)
  | x $ C = N; K (_.G == C.G)
  | x $ G = C; K (_.G == C.G * G)
  | x $ G = N; C (_.G == C.G)
  | (x $ C) => K (_.G == C.G)
  | (x $ G) => C (_.G == C.G)
  | C(N $ G) (_.G == C.G)
  | M(C $ G) (_.G == C.G * G)

//
Term, M, N, K, G, R ::=
  | 0 | 1 | G + R
  | x | x $ G = N; K
  | (x $ G) => K | M(N $ G)

S(0) |-> 0
S(1) |-> 0
S(G + R) |-> S(G) + S(R)
S(x) |-> 0
S(x $ G = N; K) |-> S(G) + (S(N) * T(G)) + S(K)
S((x $ G) => K) |-> 1 + S(G) + S(K)
S(M(N) $ G) |-> S(M) + (S(N) * T(G)) + S(G)

T(0) |-> 0
T(1) |-> 1
T(G + R) |-> T(G) + T(R)
T(N) |-> ω + S(N)

x $ C.G + G = N; C[x] |-> x $ G = N; C[N]
x $ 0 = N; K |-> K{x := N}  (x ∉ K)
L[(x $ G) => K](N $ G) |-> L[x $ G = N; K]

Context, C ::=
  | _ (_.G == 1)
  | C + G  (_.G == C.G)
  | G + C  (_.G == C.G)
  | x $ C = N; K (_.G == C.G)
  | x $ G = C; K (_.G == C.G * G)
  | x $ G = N; C (_.G == C.G)
  | (x $ C) => K (_.G == C.G)
  | (x $ G) => C (_.G == C.G)
  | C(N $ G) (_.G == C.G)
  | M(C $ G) (_.G == C.G * G)


x $ C.G + G = N; C[x] |-> x $ G = N; C[N]

S(C.G + G) + (S(N) * T(C.G + G)) + S(C[x]) |->
S(G) + (S(N) * T(G)) + S(C[N])



S(C.G)


lem1? : S(C.G + G) == S(C.G) + S(G)
lem2? : T(C.G + G) == T(C.G) + T(G)
lem3? : S(C[x]) + (S(N) * T(C.G)) == S(C[N])




//
Term, M, N, K, G, R ::=
  | 0 | 1 + G
  | x | (x) => K | M(N $ G)

S(M(N $ G)) == S(M) + (T(G) * S(N)) + S(G)
((x) => K)(N $ G) |-> K{x := N}  (count(x) ∈ K == G)

(count(x) ∈ K == G)

Term, M, N, K, G ::=
  | z | s(G) | G + R | G * R
  | x | (x) => K | M(N $ G)

x =
L[(x) => K](N $ G) |-> L[x = (N $ G); K]
g + r |-> g{+ r}
g * r |-> g{* r}

T(zero) = 0
T(succ(G)) = 1 + T(G)
T(N) = ω

S(zero) = 0
S(succ(G)) = S(G)
S(G + R) = 1 + S(G) + S(R)
S(G * R) = 1 + S(G) + S(R)
S(x) = 0
S((x) => K) = 1 + S(K)
S(M(N) $ G) = S(M) + (T(G) * S(N)) + S(G)
S(G + R)

ω ^ ω
n * ((ω * x) + y)
(ω² * 2) + ω * 3

(ω * 2) + (ω * 3) = ω * 5

(a + (ω * b) + (ω² * c))
(a + (ω * b) + (ω² * c))
[a, b, c]

((ω * a) + (ω¹ * b) + (ω² * c))
[0, a, b, c]

ω * (a + (ω * b) + (ω² * c))

ω * (a + (ω * b) + (ω² * c))


(ω * b + a) * ω == (ω * b) + (a * ω)




L(n) = 0
L((ω ^ n) * x + y) = n

L(M) = l
L(M * ω ^ n) = 1 + l

(ω ^ (1 + n))

L(0) = 0
L(1 + G) = L(G)
L(x) = 0
L((x) => K) = L(K)
L(M(N) $ g) = 1 + L(M) + (L(N) * g)
L(M(N) $ G) =

ω * ω

H(0) = 0
H(1 + G) = H(G)
H(x) = 0
H((x) => K) = H(K)
H(M(N) $ G) = H(M) + H(N) + S(N)

H(N) = ω






(aω + b) + (bω + c)

//
((x) => K)(N $ G) |-> K{x := N}  (count(x) ∈ K == G)


((x) => )(N)
// from
1 + S(K) + (S(N) * T(G)) + S(G)
// to
S(K) + (S(N) * count(x) ∈ K)

//

Term, M, N, K ::=
  | z | s(M) | M + N | M * N
  | x | (x) => K | M(N $ G)

((x) => K)(N $ G) |-> K{x := N}  (count(x, K) == G)
z + N |-> N
s(M) + N |-> s(M + N)
z * N |-> z
s(M) * N |-> N + (M * N)


Grade, g ::=
  | 0 | s(g) | g + g | g * g

T(g) = 0
T(s(G)) = 1 + T(G)
T(M + N) = T(M) + T(N)
T(M * N) = T(M) * T(N)
T(x | (x) => K | M(N $ G)) = ω

T(x) |->

x = (N $ L + R); C[x] |-> x = (N $ R);  (C.G == L)

T(M * (N + K)) == (M * N) + (M * K)

T(L + R) * S(N) == (T(L) * S(N)) + (T(R) * S(N))
T(L * R) * S(N) == T(L) * T(R) * S(N)


S(zero) = 0
S(succ(G)) = S(G)
S(G + R) = 1 + S(G) + S(R)
S(G * R) = 1 + S(G) + S(R)
S(x) = 0
S((x) => K) = 1 + S(K)
S(M(N) $ G) = S(M) + (T(G) * S(N)) + S(G)
S(G + R)

((x) => f(x $ g)(x $ R))(N $ g + R)
f(N $ g)(N $ R)


((x) => f(x $ L)(x $ R))(N $ L + R)

x <= y
x < 1 + y


//


Term, M, N, K ::=
  | z | s(M)
  | x | (x) => K | M(N $ G)

T(g) = 0
T(s(G)) = 1 + T(G)
T(x | (x) => K | M(N $ G)) = ω

H(zero | x) = 0
H(succ(G)) = H(G)
H((x) => K) = H(K)
H(M(N) $ G) = 1 + H(M) + (T(G) * H(N)) + H(G)

// problem
((x) => f(x $ 1)(x $ y))(N $ 1 + y) |-> f(N $ 1)(N $ y)
// which is still safe because
S(N) + (ω * S(N)) == (ω + S(N))
// but what if we reduce one of the N?
f(N2 $ 1)(N $ y)
// then
S(N2) + (ω * S(N))
// which may be higher than before

// affine + fix


Term, M, N, K ::=
  | x | (x) => K | M(N)
  | 0 | 1 + M | rec(K, M, N)

((x) => K)(N) |-> K{x := N}  (count(x, K) <= 1)
rec(0, M, N) |-> M
rec(1 + K, M, N) |-> N(rec(K, M, N))  (fv(N) == ∅)

H(x) == 1
H((x) => K) == 1 + H(K)
H(M(N)) == H(M) + H(N)
H(0) == 0
H(1 + K) == 1 + H(K)
H(rec(K, M, N)) == H(K) + H(M) + (T(K) * H(N))

rec(x, M, N)

rec(((x) => x)(1), M, N)

y @ 0
x @ 1

ω
ω * 2

ω * lvl(x)


((x) => rec(x, M, N))(1 + y) |-> rec(1 + y, M, N)

H(M) + H(N) + H(N)

rec(1 + y, M, N)

//
y @ (ω * 1)
x @ (ω * 2)

y = _; x = 1 + y; rec(x, M, N)
y = _; x = 1 + y; rec(1 + y, M, N)
//
y = _; x = 1 + y; N(rec(y, M, N))

//
Term, M, N, K, G ::=
  | 0 | 1 | M + N | M * N
  | x | (x) => K | M(N $ G)

((x) => K)(N $ G) |-> x = N; K  (count(x, K) == G)


A(B $ C(D $ E(F $ G)))

L(t : Term) : Term
L(0) == 0
L(1) == 1
L(M + N) == L(M) + L(N)
L(M * N) == L(M) * L(N)
L(x) == x
L((x) => K) == 1 + (x) => L(K)
L(M(N $ g)) == L(M) + (G * L(N)) + L(G)

L((f) => (x) => k(a $ f(x $ 1)))
1 + (f) => 1 + (x) => L(k) + (f(x $ 1) * L(a)) + L(f(x $ 1))


((f) => (x) => k(a $ f(x)))

//
rank(t : Term) : Nat;

H((x) => f(N $ x))

(x) => 1 + (x * H(N))

l | ((x) => f(N $ x))(M $ 1)
2 + (x * H(N)) + H(M) > 1 + (M * H(N)) + H(M)


l | ((x) => x)(M $ 1)
2 + (x * H(N)) + H(M) > 1 + (M * H(N)) + H(M)




1 + H(1 + y)


x = 1 + y; f(N $ x)

x = 1 + y; x * H(N)



// 3 levels of grading, so H(H(H(M)))
H(A1(A2 $ B1(B2 $ C1(C2 $ 2))))
// same as, but now it has one less level of nesting
H(A1) + (B1(B2 $ C1(C2 $ 2)) * A2) + H(B1(B2 $ C1(C2 $ 2)))
// then I can do
H(H(A1) + (B1(B2 $ C1(C2 $ 2)) * A2) + H(B1(B2 $ C1(C2 $ 2))))
// same as
H(H(A1)) + (H(B1(B2 $ C1(C2 $ 2))) * H(A2)) + H(H(B1(B2 $ C1(C2 $ 2))))
// let's focus on one component
H(B1(B2 $ C1(C2 $ 2)))
// same as, now it has one less level, eventually it stops
H(B1) + (C1(C2 $ 2) * H(B2)) + H(C1(C2 $ 2))


H(M) + (x * H(N)) + (y * H(N))


((x) => f(x $ g)(x $ G))(N $ g + G) |-> f(N $ g)(N $ G)

x = M; f(N $ x) > x = M; f(N $ M)

((x) => f(N $ x))(M) > f(N $ M)



l | x = 1 + y; f(N $ 1 + y)
ω * l

f(N $ 1 + y)



f(x $ g(y $ ))

count(x, C[x]) == C.G + count(x, C[_])
H(C[x]) == H(C[_])

count(x, C[N]) == count(x, C[_])
H(C[N]) == (C.G * H(N)) + H(C[_])

C.G + count(x, C[_]) + (C.G * H(N)) + (count(x, C[_]) * H(N)) + H(C[_])
count(x, C[_]) + (count(x, C[N]) * H(N)) + (C.G * H(N)) + H(C[_])

C.G + count(x, C[_])
count(x, C[_])

((C.G + count(x, C[_])) * H(N)) + H(C[_])






l | ((x) => x(x))((x) => x(x))

ω * lvl(1 + l)
ω * lvl(l)
l | x = N; K

((x) => K)(N $ G) |-> K{x := N}  (count(x, K) == G)
K{x := N}

x = 1 + y; f(N $ x)
f(N $ 1 + y)

l | ((x) => K)(N $ G)

((x) => K)(N $ G) |-> K{x := N}  (count(x, K) == G)

H(f(N $ 1 + y))

f(N $ 1 + y)

x = N; K |-> K{x := N}

(count(x, K) * H(N)) + (ω * H(K))

(count(x, K) * H(N)) + (ω * H(K))
(count(x, C[N]) * H(N)) + (ω * H(C[N]))

H(x = 1 + y; f(N $ x)) == H(1 + y) * (ω * H(K))

x = 1 + y; f(N $ x)
f(N $ 1 + y)



H(x) > H(N)


l | ((x) => f(N $ x\(1+l)))(1 + \l)
l | f(N $ (1 + \l))

l | x = 1 + y; f(N $ x)
l | f(N $ 1 + y)

l | S(\l); f(N $ \(1 + l))
l | f(N $ S(\l))


((x) => K)(N $ G) |-> K{x := N}  (count(x, K) == G)

l | x = 1 + y; f(N $ x)
l | f(N $ 1 + y)

T(l, 0) == 0
T(l, 1 + G) == T(G) + 1
T(l, x | (x) => K | M(N $ G) as M) == (ω * H(M))


H(M(N $ G)) == (ω * H(M)) + (T(G) * H(N)) + H(G)

((x) => K)(N $ G)

((f) => f(N $ G))(M $ 1)
M(N $ G)

((x) => f(N $ x))(1 + y $ 1) |-> f(N $ 1 + y)

S(l, x = N; K) = S(N) + S(1 + l, K)

l | x = 1 + y; f(N $ x)
l | f(N $ 1 + y)

l | S(\l); f(N $ \(1 + l))
l | f(N $ S(\l))

l | N; C[\bv(C)]

l | (λ. f(N $ \(1 + l)))(2 + \l)
l | f(N $ 2 + \l)

(1 + \0); f(N $ \0)
(1 + \0); f(N $ 1 + \1)

T(1 + y) > T(x)

H(x) == 1
H((x) => K) == 1 + H(G) + H(K)
H(M(N $ G)) == H(M) + H(N) + H(G)

//
Term, M, N, K, G ::=
  | z | s(M) | M + N | M * N
  | x | (x) => K | M(N $ G)

((x) => K)(N $ G) |-> x = N; K  (count(x, K) <= G)
z + N |-> N
s(M) + N |-> s(M + N)
z * N |-> z
s(M) * N |-> N + (M * N)

//
count(x, C[x]) == C.G + count(x, C[_])
F(C[x]) == F(C[_])
F(C[N]) == (C.G * F(N)) + F(C[_])
//


F(0) == 0
F(s(G)) == 1 + F(G)
F(M + N) == F(M) + F(N)
F(M * N) == F(M) * F(N)
F(x) == [0, x]
F((x) => K) ==
  [T_K, D_K] = F(K);
  [1 + T_K, [count(x, D_K), (x) => D_K]];
F(M(N $ g)) ==
  [T_M, [G_M, D_M]] = F(M);
  [T_N, D_N] = F(N);
F(M(N $ G)) ==
  [T_M, [G_M, D_M]] = F(M);
  [T_N, D_N] = F(N);
  [T_M_N, D_M_N] = D_M(D_N $ G_M);
  [
    T_M + T_M_N,
    D_M_N
  ]

(x) => f(N $ x)
[
  1,
  (x) => [
    1,

    x * F(N)
  ]
]

//
(f) => (x) => f(N $ x)
[
  1,
  (f) => [
    1,
    (x) =>
      [T_N, D_N] = F(N);
      [T_x, D_x] = F(x);
      [
        T_N * x,
        _
      ]
  ]
]

((f) => (x) => f(y $ x))((y) => k(z $ y) $ 1)


F(M(N $ g)) = T(M) + (T(N) * g)
(M(N))
(x) => f(x $ 1)
[
  1,
  (x) => [0, D_f(D_x $ G_f).1]
]


(x) => f(N $ 2)
[
  1,
  (x) =>
    [T_f, [G_f, D_f]] = [0, f];
    [T_N, D_N] = F(N);
    [T_f_N, D_f_N] = D_f(D_N $ G_f);
    [
      T_f + T_f_N,
      D_f_N
    ]
]

(x) => f(N $ x)
[
  1,
  (x) =>
    [T_f, [G_f, D_f]] = [0, f];
    [T_N, D_N] = F(N);
    [T_f_N, D_f_N] = D_f(D_N $ G_f);
    [
      0 + T_f_N,
      D_f_N
    ]
]

((x) => f(N $ x))(1 + y $ 1)



((x) => [x, x])(y $ 2)

(f) => (x) => f(x $ 2)

(x) => 0


T(f) + (x * T(N))
(x) => f(N $ x)

//
Term, M, N, K ::=
  | z | s(M) | M + N | M * N
  | x | (x) => K | M(N $ G)

// it doesn't count the the stuff in G
((x) => K)(N $ G) |-> K{x := N}  (count(x, K) == G)

// linear LC + natural numbers


// first-class "graded" LC
Term, M, N, K ::=
  | z | s(M) | M + N | M * N
  | x | (x) => K | M(N $ G)

// beta is only valid if the G is correct
((x) => K)(N $ G) |-> K{x := N}  (count(x, K) == G)


//
Term, M, N, K ::=
  | x | (x) => K | M(N)
  | 0 | 1 + M | rec(K, M, N)

F(x) == 0
F((x) => K) == 1 + F(K)
F(M(N)) == F(M) + F(N)
F(0) == 0
F(1 + M) == 1 + F(M)
F(rec(K, M, N)) == F(K) + F(M) + (T(K) * F(N))

T(0) == 0
T(1 + M) == 1 + T(M)
T(rec(K, M, N)) == ω

// beta is only valid if it's linear
((x) => K)(N) |-> K{x := N}  (count(x, K) == 1)
rec(0, M, N) |-> M
rec(1 + K, M, N) |-> N(rec(K, M, N))  (fv(N) == ∅)


T(x) == 0
T((x) => K) == 1 + T(K)
T(M(N)) == T(M) + T(N)
T(0) == 0
T(1 + M) == 1 + T(M)
T(rec(K, M, N)) == T(K)

//
Term, M, N, N, G ::=
  | x | (x) => K | M(N)
  | 0 | 1 + M | rec(K, M, N)

((x) => K)(N) |-> K{x := N}  (count(x, K) <= 1)
rec(0, M, N) |-> M
rec(1 + K, M, N) |-> N(rec(K, M, N))  (fv(N) == ∅)

F(t : Term) : Term
F(x) == 0
F((x) => K) == 1 + F(K)
F(M(N)) == F(M) + F(N)
F(0) == 0
F(1 + M) == 1 + F(M)
F(rec(k, M, N)) == F(M) + (k * F(N))

Bool : Type;
grade_t(b : Bool) : Grade;
grade_f(b : Bool) : Grade;

Bool = self(b) -> (
  A : Type $ 0
  t : A $ grade_t(b),
  f : A $ grade_f(b)
) -> A;
grade_t(b) = b.@(Grade, 1, 0);
grade_f(b) = b.@(Grade, 0, 1);

Term, M, N, K, G ::=
  | Type | A $ G
  | x | x = N; K
  | (x : A) -> B | (x) => K | M(N)
  | [a : A, B] | [M, N] | M.0 | M.1

[a : A, B] $ G = [a : A $ G, B $ G]

Γ |- M ⇐ A
------------------
Γ * G |- M ⇐ A $ G

Γ |- M ⇒ (x : A) -> B  Δ |- N ⇐ A
---------------------------------
Γ + Δ |- M(N) : B{x := A}

----------------------------
Γ * 0 |- (x : A) -> B : Type

Γ |- M : [a : A, B]
-------------------
Γ |- M.0 : A

Γ |- M : [a : A, B]
-------------------
Γ |- M.1 : B

(T : Type) $ n =
  n.is(
  | 0 => []
  | 1 + n => [x : T, T $ n]
  );

T : Type;
T = (x : T $ 0) -> False $ 0;
x : T = (x) => x(x);
// same as
x : T = (x) => [];

f : (x : Type $ 0) -> Type;

x(x)

()

Bool : Type;
grade_t(b : Bool $ 1) : Grade;
grade_f(b : Bool $ 1) : Grade;

Bool = self(b) -> (
  A : Type $ 0,
  t : A $ grade_t(b),
  f : A $ grade_f(b)
) -> A;
grade_t(b) = b.@(Grade, 1, 0);
grade_f(b) = b.@(Grade, 0, 1);

(b : Bool $ 1) => b.@(
  _,
  _ : b.@(Grade, 1, 0),
  _ : b.@(Grade, 0, 1)
)
T : Type;
T = (x : T $ 1) -> Type $ 0;

x : T = (x) => x(x);


// terminates because the nesting in G goes down
Term, M, N, K ::=
  | Type | x
  | (x : A) -> B | (x) => K | M(N)
  | 0 | 1 + M | ind(M, C, N, K)


// count produces a term
((x) => K)(N $ G) |-> K{x := N}  (count(x, K) == G)
z + N |-> N
s(M) + N |-> s(M + N)
z * N |-> z
s(M) * N |-> N + (M * N)



// linear
Type : Type
Grade : Type
// intuitionistic
Data : Type


(x : A : Data) -> (B : K)
// same as
(x : A $ ∞) -> (B : K)

// because all data are duppable
dup(b : Bool $ 1) = b ? (true $ 2) : (false $ 2);



//
Term, M, N, K ::=
  | 0 | 1 + N | rec(M, K, N)
  | x | (x) => M | M(N)
  | #many(M, G) | #split(M, G)

x = (x) => #many(x(x), 0);
x(x)

T : Type;
T = (x : T $ 0) -> False $ 0

x : T = (x : T $ 0) =>
  #many((x : T $ 1) => x(x), 0) : ((x : T $ 1) -> False $ 0) $ 0;


f = (x : A $ L $ R) : A $ (L * R) =>
  (y : A $ L) $ R = x;
  z : A $ (L * R) = y;
  z


((x : A) -> B $ G)
// same as
(x : A $ G) -> (B $ G)


// not the same as
(x : A $ G) -> (B $ G)

(A : Type $ 0) -> (x : )
// fails because x(x) is not typeable anywhere


|- M : A
----------------------
|- #many(M, G) : A $ G

T : Type;
T = (x : T $ 0) -> False $ 0

x = (x) => #many(x(), 0)
x : T = (x : T $ 0) =>
  #many((x : T $ 1) => x(x), G) : ((x : T $ 1) -> False $ 0) $ G;

((x : A) -> B $ G)
// not the same as
(x : A $ G) -> (B $ G)



Γ |- A : Type  Γ, a : A |- B : Type
-----------------------------------
(Γ + G) * 0 |- (a : A) -> B : Type


(
  C : (a : A) -> Type $ 0
) -> (

  (r : C(x) $ 1) -> C(y)
)



f = (A : Type, x : A $ 0, p : A == Type, x : p.cast(x)) => x;

f(Type, _ : Type $ 0)


//
Term, M, N, K ::=
  | Type | (M : A)
  | x | x = N; K |
  | (x : A) -> B | (x) => K | M(N)
  | [a : A, B] | [M, N] | M.0 | M.1
  | T $ G | #zero(M) | #one(M) | #merge(M, N) | $x = N; K

// this is bad
f = (x : String $ 2) : [l : String, r : String] =>
  $y = x;
  [y, y];

T : Type;
T = (x : T) -> False $ 0;

x : T = (x : T) => lift(x)(0, x)
f(a : A $ 2) : B;
g(a : A $ 6) : B $ 3 =
  lift(f);

Closure = [E : Env $ 1, f : (e : Env $ 1, a : A) -> B $ 1];

// 4 copies of a closure
[E : Env $ 1, f : (e : Env $ 1, a : A) -> B $ 1] $ 4
[E : Env $ 4, f : (e : Env $ 1, a : A) -> B $ 4]
[E : Env $ 4, f : (e : Env $ 4, a : A $ 4) -> (B $ 4) $ 1]

// 0 copies of a closure
[E : Env $ 1, f : (e : Env $ 1, a : A) -> B $ 1] $ 0
[E : Env $ 0, f : (e : Env $ 1, a : A) -> B $ 0]
// cannot push further

// same for dividing a closure
[E : Env $ 1, f : (e : Env $ 1, a : A $ 4) -> (B $ 4) $ 1]
// the env would need to be duplicated 4 times

// works
[E : Unit $ 1, f : (e : Unit $ 1, a : A $ 4) -> (B $ 4) $ 1]
[E : Unit $ 4, f : (e : Unit $ 4, a : A) -> B $ 4]

// doesn't work again
[E : Unit $ 1, f : (e : Unit $ 1, a : A $ 0) -> (B $ 0) $ 1]
// up seems to work
[E : Unit $ 0, f : (e : Unit $ 0, a : A) -> B $ 0]

//
[E : Unit $ 1, f : (e : Unit $ 1, a : A $ 0) -> (B $ 0) $ 1]
// up seems to work
[E : Unit $ 0, f : (e : Unit $ 0, a : A) -> B $ 0]

//
[E : Env $ 1, f : (e : Env $ 1, a : A) -> B $ 1] $ 0
[E : Env $ 0, f : (e : Env $ 1, a : A) -> B $ 0]
[E : Env $ 0, f : (e : Env $ 0, a : (A $ 0)) -> (B $ 0)]

T : Type;
T = (x : T $ 0) -> False $ 0;

x = (x : T $ 0) =>


|- M : A
------------
|- M ⇐ A $ G

Γ |- M ⇐ A  Γ |- G ⇐ Grade
----------------------------
Γ * (1 + G) |- M ⇐ A $ 1 + G

T = μ. (x : T $ 0) -> Type;

x : T = (x) => (u : Unit) -> unfold(x)(x)

dup = (x : String) => #many(x, 2);

id = (A : Type $ 0, x : A) => x;
refl[A, x, y](C : (a : A) -> Type $ 0, r : C(x)) : C(x) =>
  #many((C : (a : A) -> Type) => C())
  id(() -> C(x), r);


x : Nat $ 0 =
f(#many("a", 2))


// take functions to be closures
[E : Env, (e : E, a : A) -> B]
// in an explicitly graded setting something like
[E : Env $ 1, (e : E $ 1, a : A $ 1) -> (B $ 1) $ 1]

// "pushing down" in a graded setting:
f : ((a : A) -> B) $ G;
// which when G is not 0, can become
f : ((a : A $ G) -> (B $ G)) $ 1;

// but if G is 0
f : ((a : A) -> B) $ 0;
// which would become this
f : ((a : A $ 0) -> (B $ 0)) $ 1;
// but that is making something($ 1) out of nothing ($ 0)

// "pushing up" would be the opposite
f : ((a : A $ G) -> (B $ G)) $ 1;
// then
f : ((a : A) -> B) $ G;
// but that's bad, because of closures

// if you take functions to be closures it is the same as
f : [Env : Type, e : Env $ 1, (e : Env $ 1, a : A $ G) -> (B $ G)] $ 1;
// becoming
f : [Env : Type, e : Env $ 1, (e : Env $ 1, a : A) -> B] $ G;
// but now you have G copies instead of 1, which is bad




r_not_in_f : (n : Nat) -> f(n) != r;
r_not_in_f = (n) =>
  ind(
    n,
    (n) => f(n) == r,
    _ : f(0) == (n : Nat $ 2) => not(f(n)(n))
  );

//
Bool : Type;
Bool.dup(b : Bool) : Bool $ 2 =
  b ? (true $ 2) : (false $ 2);

Nat : Type;
Nat.dup(n : Nat) : Nat $ 2;

Real = (n : Nat) -> Bool;
// this one is affine, only uses one n
Real.dup(r : Real $ 1) : Real $ 2 =
  (n) => Bool.dup(r(n));

Real = (n : Nat) -> Bool;
f(n : Nat $ 1) : Real;

r : Real = (n : Nat $ 2) = not(f(n)(n));
r_not_in_f(n : Nat) : f(n) != r;
r_not_in_f(n) = (p : f(n) == r) =>
  Bool.b_neq_not_b(coe(p, (g) => f(n)(n) == g(n), refl()));


zero = (z, s) => z;
succ(n) = (z, s) => s(n);

//
T = (a : A $ 2) -> Bool;
f(a : A $ 1) : T;

r : T = (a : A $ 2) => not(f(a)(a));
r_not_in_f(a : A $ 2) : f(a) != r = (p : f(a) == r $ 1) =>
  Bool.ind(
    f(a)(a)
    (b) => (p : b == not(b)) -> False,
    (p) => _,
    (p) => _,
  )(coe(p, (g) => f(a)(a) == g(a), []))

f(a)(a) == not(f(a)(a))

(==) : [A, x, y](
  C : (a : A ) -> Type $ (0, 2),
  r : C(x) $ (1, 0)
) -> C(y);
r_not_in_f(a : A) : (p : f(a) == r $ 1) -> False =
  Bool.b_neq_not_b(
    coe(p, (g) => f(a)(a) == g(a), refl())
  );

//
Real = (n : Nat $ 2) -> Bool;
f(n : Nat $ 1) : Real;

r : T = (a : A $ 2) => not(f(a)(a));
r_not_in_f(a : A $ 2) : f(a) != r = (p : f(a) == r $ 1) =>



// goal
Real : Type;
Real = (n : Nat) -> Bool;
// same as
Real : Type;
Real = [l : Bool, r : Real];

// sugar, (n : Nat) -> T
F(T : Type) : Type;
F(T) = [l : T, r : F(T)];

app[T](f : F(T), n : Nat) : T;
app[T](f, n) =
  n.is(
  | 0 => f.0
  | 1 + n => app(f.1, n)
  );

Real = F(Bool);
f : F(Real);

incr = (k, n) => k(1 + n);
// same as
incr(k) = k.1;


n = (k : F(A)) => k.1.1.1.0

id = (k) => (x) => k(x);

id : F(Nat);
id = [
  [0, ...],
  id.1
]

add : F(Nat);
add = [
  [
    0,
    1,
    2,
    3,
    _
  ],
  [
    1,
    2,
    3,
    _
  ],
  [
    2,
    3,
    4,
    _
  ],
  _
]
x : Real;
x = f.1;
r : Real;
r = fix(n) => not(app(app(f, n), n));

app()

r = r(0);




// potentially not linear
Type, A, B ::=
  | Nat | A -> B | [A * B]

Term, M, N, K ::=
  | x | x = N; K
  | (x : A) => K | M(N)
  | [M, N] | [x, y] = N; K
  | 0 | 1 + N | rec(M, N, K)

(x : A) => C[x] |-> (x : A) => [x, y] = DUP(A, x); C[y] // dereliction

// to a linear term
F(0) == 0
F(1 + N) == 1 + F(N)
F(rec(M, N, K)) == _
F(x) == x
F((x : A) => K) == (x : A) =>

  _; F(K)
F(M(N)) == F(M)(F(N))


f : (a : A) -> Bool;

g : (b : Bool) -> A;


k = (b) => g(b)

 : (n : Nat) -> A;



Dup(A) = Closed((a : A) -> [A, A]);


triple = (
  A : Type,
  dup : (G : Grade) -> (a : A) -> A $ G,
  a : A
) =>
  [a, b] = dup(x);
  [a, dup(b)];


U(l : Level) : Type;
U(l) =
  (
    C,
    // univ
    u : C(1 + l),
    // pi
    p : (A : U(l), B : (x : _) -> U(l)) -> C(l),
  ) -> C;

//
U : Type;
U = (
  C : Type,
  // ∀A -> T
  all : (T : (A : U) -> U) -> C,
  arr : (A : U, B : U) -> C,
) -> C;

forall(T : (A : U) -> U) : U = (C, all, arr) => all(T);
arr(A : U, B : U) : U = (C, all, arr) => arr(A, B);

El(x : U) : Type;
El(x) =
  x(
    Type,
    (T : (A : U) -> U) => (A : U) -> El(T(A)),
    (A : U, B : U) => (a : El(A)) -> El(B)
  );

Nat : Type;
Nat = [
  G : Grade,
  (
    C : Type,
    z : (p : G == 0) -> C,
    s : ((G_n, p : G == 1 + G_n, n : C) -> C) $ G
  ) -> C
];

zero : Nat = [0, (C, z, s) => z];
succ(n : Nat) : Nat =
  [G_n, n_f] = n;
  [1 + G_n, (C, z, s) => s(G_n, refl(), n_f(C, z, s))];


Term, M, N, K, A, B ::=
  | x | x = N; K
  | (a : A) -> B | (x) => K | M(N)
  | [a : A, B] | [M, N] | M.0 | M.1
  | 0 | 1 | M + N | M * N

0 + N |-> N
(1 + M) + N |-> M + (1 + N)

0 * N |-> 0
(M + N) * K |-> (M * K) + (N * K) // distr
5 * _A

Grade Value, G0 ::=
  | 0 | 1 | x | 1 + G0
  | G0 + G1
Grade Addition, G1 ::=
  | 1 + G1
  | V + G
Grade Multiplication, G2 ::=
  | x * G2
list(G)

-------------------------
Γ, x : A |-> Γ, x : A $ 1


Bool : Type;
Bool = [
  T : Grade < 2,
  F : Grade < 2,
  (C : Type, t : C $ T, f : C $ F) -> C
];
true : Bool = [1, 0, (C, t, f) => t];
false : Bool = [0, 1, (C, t, f) => f];


Type(0) : Type(1) : Type(2)



Id : Type = (A : Type) -> (x : A) -> A;
id : Id = (A : Type) => (x : A) => x;

map : (
  A : Type(0),
  B : Type(0),
  f : (a : A) -> B,
  l : List(A)
) : List(B);


dup(n : Nat) : Nat $ 2;
dup(n) =
  n.is(
  | 0 => 0 $ 2
  | 1 + n => (1 + dup(n)) $ 2
  );
Socket : Type;
connect(ip : IPv4) : Socket;
close(sock : Socket) : Unit;


Type(l) : Type(1 + l)

T : Type;
T = (x : T) -> False;

x : T = (x) => x(x);

f : False = x(x);


// Nat and Bool can be duppable
Real = (n : Nat) -> Bool;
// functions like Real and f cannot be easily duppable
f(n : Nat) : Real;
// usages on the type level doesn't count
diag(n : Nat) : f(n) != ((n) => not(f(n)(n)));
diag(n) = (p : f(n) == ((n) => not(f(n)(n)))) =>
  l : f(n)(n) == not(f(n)(n)) =
    coe(p, (g) => f(n)(n) == g(n), refl());
  // consuming f here
  r : f(n)(n) != not(f(n)(n)) =
    Bool.true_neq_false(f(n)(n));
  r(l);

// then let's assume that f is magic
// but this function also consumes f
f_magic(r : Real) : [n : Nat, p : f(n) == r];
// if you try to make a contradiction
[n, p : f(n) == ((n) => not(f(n)(n)))] =
  f_magic((n) => not(f(n)(n)));
// diag uses f, f_magic uses f, you cannot call both
q : f(n) != ((n) => not(f(n)(n))) = diag(n);

// the catch, eval is linear
// you get to use it once GLOBALLY
eval(n : Nat) : Real;
godel(r : Real) : Nat;
eval_godel_id[A](r) : eval(godel(r)) == r;
godel_eval_id[A](n) : godel(eval(n)) == n;

// useful for programming
// aka for all the cases that are duppable
eval[A : Data](s : String) : Option(A);






f = (n) => eval(check(Real, n.godel()));
g = (n) => eval(check(Real, n.godel()));

eval[A](p : String) : A;

f(n : Nat) : Real = eval[Real](n.godel());

// Gödel Number -> Real
f(n : Nat) : Real;
// Real -> Gödel Number
g(r : Real) : Nat;
g_inj(r1, r2 : Real, p : g(r1) == g(r2)) : r1 == r2;

//
// Nat and Bool can be duppable
Real = (n : Nat) -> Bool;
// functions like Real and f cannot be easily duppable
f(n : Nat) : Real;
// usages on the type level doesn't count
diag(n : Nat) : f(n) != ((n) => not(f(n)(n)));
diag(n) = (p : f(n) == ((n) => not(f(n)(n)))) =>
  l : f(n)(n) == not(f(n)(n)) =
    coe(p, (g) => f(n)(n) == g(n), refl());
  // consuming f here
  r : f(n)(n) != not(f(n)(n)) =
    Bool.true_neq_false(f(n)(n));
  r(l);

g(r : Real) : Nat;
f_g_id(r : Real) : f(g(r)) == r;

p : f(g((n) => not(f(n)(n)))) != ((n) => not(f(n)(n))) =
  diag(g((n) => not(f(n)(n))));
q : (n) => not(f(n)(n)) != ((n) => not(f(n)(n))) =
  coe(
    f_g_id((n) => not(f(n)(n))),
    (r) => r == ((n) => not(f(n)(n))), refl());
f : False = p(q);

//
f(n : Nat)

dup(n : Nat) : [Nat, Nat];
dup(n) =
  n.is(
  | 0 => [0, 0]
  | 1 + n => [1 + dup(n).0, 1 + dup(n).1]
  );
Term, M, N, K ::=
  | x | (x) => K | M(N)
  | 0 | 1 + N | rec(M, K, N)

((x) => K)(N) |-> K{x := N}  (count(x, K) == 1)
rec(0, M, N) |-> M
rec(1 + K, M, N) |-> N(rec(K, M, N))  (fv(N) == ∅)



El_P(T : U, A : Type) : Type;
El_P(T, A) =
  T(
    Type,
    A,
    (L, R) => [l : El_P(L, A), r : El_P(R, A)],
    (L, R) => (l : El_P(L, A)) -> El_P(R, A)
  );
El(T) = (A : Type) -> (p : El_P(T, A)) -> A;



n(Nat, zero, succ)

dup[A : U](a : El(A)) : El(A) $ 2 =
  a(El(A))(

    _ : El_P(T, El(A))
  );

Bool = either(self, self);
El(Bool) : (A : Type) -> (p : [l : A, r : A]) -> A;

Nat = either(self, arrow(self, self));
El(Nat) : (A : Type) -> (p : [l : A, r : (l : A) -> A]) -> A;



//
Term, M, N, K ::=
  | Type | x
  | (a : A $ G) -> B | (x) => K | M(N)

Grade, G ::= 1 + n

|- Type : Type

|- A : Type  a : A |- B : Type
------------------------------
|- (a : A) -> B : Type


Γ, a : A $ G |- M : B
----------------------------
Γ |- (a) => M : (a : A $ G) -> B

-------------------
a : A $ 1 |- a :ⁿ A

Γ |- M :ⁿ (a : A $ G) -> T{N := a}  Δ |- N : A
----------------------------------------------
Γ + (Δ * G) |- M(N) :ⁿ T



// this makes the system not linear
// because you can dup or weaken the nat

Eq[A](x, y) =
  (C : (z : A) -> Type $ 0, r : C(x)) -> C(y);
refl[A](x) : Eq[A](x, x) = (C, r) => r;

Eq.drop[A, x : A, y](p : Eq(x, y)) : [] =
  p((z) => Unit, []);
Eq.dup[A, x : A, y](p : Eq(x, y)) : Eq(x, y) $ 2 =
  p((z) => Eq(x, z), refl(x));

eval(n : Nat) : [A : Type, a : A]
Data(A) = [
  a : A $ 0,
  n : Nat,
  p : Eq(eval(n), [A, a])
];
drop = [A](x : Data(A)) : [] =
  [a, n, p] = a;
  [] = Nat.drop(n);
  Eq.drop(p);
dup = [A](x : Data(A)) : Data(A) $ 2 =
  [a, n, p] = x;
  n = Nat.dup(n);
  p = Eq.dup(p);
  [a, n, p];

T : Type;
T = (x : Data(T)) -> False;

t : T = (x) =>
  x $ 2 = dup(x);
  [a, n, p] = x;
  [A : Type, a : A] = eval(n);
  t : T = p(([S, _]) => S, a);
  t(x);



Real : Type;
eval(n : Nat) : Real;

godel[A : Type](a : A) : Nat;
eval[A : Type, a : A](n : Nat, p : godel(a) == n) : A;

Nat.dup(n : Nat) : [m : Nat, p : n == m] $ 2;
Nat.drop(n : Nat) : [];

drop = [A](a : A) : [] = Nat.drop(godel(a));
dup = [A](a : A) : [A, A] =
  [m, p] = Nat.dup(godel(a));
  [eval(m, p), eval(m, p)];



eval_godel_id[A](a) : eval(a, godel(a), refl()) == r;



f : (A : Type) -> A;
(A) => _M : (A : Type) -> A;

f : (a : A) -> C(a);

(_M : (a : _A) -> C(x){_N := a})(_N) : C(x)

(a : A) -> C(a) == (a : _A) -> C(x){_N := a}
_A := A
C(a) == C(x){_N := a}
C(a){a := _N} == C(x)
C(_N) == C(x)
_N == x

f(V)


C(...)


(C : (T : Type) -> Type, p : C((A : Type) -> A)) -> C(Type)

((A) => (_ : A) : (A : Type) -> A)

_A : (a : A) -> B |-> ((a) => _M[a : A] : (a : A) -> B)

_A : x(...V)

Value, V ::=
  | Type | (a : VA) -> VB
  | (x) => V | VS[x]
Value Spine, VS ::=
  | _ | VS(V)

C : (a : A) -> Type;
f : (a) -> C(a);

f(V) : C(V)

//

Term, M, N, K ::=
  | x | (x) => K | M(N)
  | z | s(N) | rec(M, K, N)

((x) => K)(N) |-> K{x := N}  (count(x, K) <= 1)
rec(z, M, N) |-> M
rec(s(K), M, N) |-> N(rec(K, M, N))  (fv(N) == ∅)


// let's assume stratified eval
// at 0, there is no eval, at 1 there is eval 0 inside
t = (n) =>
  [x, n] = dup(n);
  t = eval(x);
  t(n);

// #godel is just sugar
t(#godel(t))

eval(n : Nat) : [A : Type, a : A]
Data(A) = [
  n : Nat,
  p : Eq(eval(n).A, A)
];


// affine but you can recurse on any external data
Term, M, N, K ::=
  | x | x = N; K
  | (x) => K | M(N)
  | fix(f) => K

id = (A : Type) => (a : A) => a;
true = (A : Type) => (t : A) => (f : A) => t;
false = (A : Type) => (t : A) => (f : A) => f;

// church encoding of nat
zero = (z) => (s) => z;
succ(n) = (z) => (s) => s(n);
one = (z) => (s) => s(zero);
two = (z) => (s) => s(one);

double : _;
double = (n) =>
  n(zero, (p) =>
    // p is produced externally
    x = double(p);
    succ(succ(x)));

//
Term, M, N, K, G ::=
  | x | (x) => K | M(N)
  | M $ G |
  | 0 | 1 | M + N | M * N | eval(M)

zero = [0, 0, (z, s) => z];
succ(n) =
  [G_n, x] = n;
  [1 + G_n, (z, s) => s(x(z $ 1, s $ G_n) $ 1)];

dup(n) =
Type : Data
Data : Type



l : (a : A) -> B;
r : (a : A) -> Not(B);



Term : Type;
fun(body : (arg : Term) -> Term) : Term;
app(funct : Term, arg : Term) : Term;

Term = self(t) -> (
  C,
  f : (body) -> C(fun(body)),
  a : (funct, arg) -> C(app(funct, arg)),
) -> C(t);
fun(body) = (C, f, a) => f(body);
app(funct, arg) = (C, f, a) => a(funct, arg);

wnhf(term : Term, args : List(Term)) : Term;
wnhf(term, args) = t(
  _ => Term,
  (body) =>
    a.is(
    | [] => fun(body),
    | arg :: args => wnhf(body(arg), args)
    ),
  (funct, arg) => wnhf(funct, arg :: args),
);




Term : Type;
fun(body : (arg : Term) -> Term) : Term;
app(funct : Term, arg : Term) : Term;
lit(body : (base : Term, step : (G : Grade) ->
  ((acc : Term) -> Term) $ G) -> Term) -> Term;
rec(n : Term, base : Term, step : (G : Grade) ->
  ((acc : Term) -> Term) $ G) : Term;

Term = self(t) -> (
  C,
  f : (body) -> C(fun(body)),
  a : (funct, arg) -> C(app(funct, arg)),
  l : (body) => C(n(body)),
  r : (n, base, step) -> C(rec(n, base, step))
) -> C(t);
fun(body) = (C, f, a, l, r) => f(body);
app(funct, arg) = (C, f, a, l, r) => a(funct, arg);
lit(body) = (C, f, a, l, r) => l(body);
rec(n, base, step) = (C, f, a, l, r) => r(n, base, step);

Spine =
  | ["_"]
  | ["app", arg : Term, spine : Spine]
  | ["rec", base : Term, step, spine : Spine];

wnhf(t : Term, spine : Spine) : Term;
wnhf(t) = t(
  _ => Term,
  (body) =>
    spine.is(
    | ["_"] => fun(body)
    | ["app", arg, spine] => wnhf(body(arg), spine)
    | ["rec", base, step, spine] => _
    ),
  (funct, arg) => wnhf(funct, ["app", arg, spine])
  (body) =>
    spine.is(
    | ["_"] => lit(body)
    | ["app", arg, spine] => _
    | ["rec", base, step, spine] =>
      wnhf(body(base, step), spine)
    )
    _,
  (n, base, step) => wnhf(n, ["rec", base, step, spine])
);

wnhf(t : Term) : Term;
wnhf(t) = t(
  _ => Term,
  (body) => lam(body),
  (funct, arg) =>
    // burning funct
    wnhf(funct)(
      _ => Term,
      // burning arg
      (body) => wnhf(body(arg)),
      _, _, _
    ),
  (body) => lit(body),
  (n, base, step) =>
    wnhf(n)(
      _ => Term, _, _,
      (body) =>
        // burning base
        wnhf(body(base, step)),
      _,
    )
);


eval(t : Term) : Term;
eval(t) = t(
  _ => Term,
  (funct, arg) =>
    eval(funct)(
      _ => (arg : Term) -> Term,
      (arg) => (funct_funct, funct_arg) =>
        app(app(funct_funct, funct_arg), eval(arg)),
      (arg) => (body) => body(arg)
    )(eval(arg)),
  (body) => lam((arg) => eval(body))
)


dup(t : Term) : Term $ 2;
dup(t) = t(
  _ => Term $ 2,
  (funct, arg) => app(dup(funct), dup(arg)),
  (body) => lam((arg) => dup(body(dup(arg))))
);



Term : Type;
fun(body : (arg : Term) -> Term) : Term;
app(funct : Term, arg : Term) : Term;

Term = self(t) -> (
  C,
  f : (body) -> C(fun(body)),
  a : (funct, arg) -> C(app(funct, arg)),
) -> C(t);
fun(body) = (C, f, a) => f(body);
app(funct, arg) = (C, f, a) => a(funct, arg);


dup(t : Term) : Term $ 2;
dup(t) = t(
  _ => Term $ 2,
  (body) => lam((arg) => dup(body(arg))),
  (funct, arg) => app(dup(funct), dup(arg))
);

Term, M, N, K, G ::=
  | 0 | 1 | M + N | M * N
  | x | (x) => K | M(N $ G)

((x) => K)(N $ G) = K{x := N}  (count(x, K) == G)

//


Term, M, N, K, G ::=
  | x | (x) => K | M(N)
  | 0 | 1 + G
  | #many(M, G) | #x = N; K


((x) => K)(N) |-> K{x := N}  (count(x, K) == 1)
//
Ωᵍ(f) =
  x(x) = f(1 + x(x $ 0) $ 0);
  x(x $ 0);
Ωᵗ(G) =
  y(y) = 1 + y(y $ G);
  y(y $ 1 + G);
Ω = Ωᵍ(Ωᵗ $ 1);
//
x(x) = Ωᵗ(1 + x(x $ 0) $ 0);
Ωᵗ(1 + x(x $ 0) $ 0);
//
x(x) = Ωᵗ(1 + x(x $ 0) $ 0);
y(y) = 1 + y(y $ 1 + x(x $ 0));
y(y $ 1 + 1 + x(x $ 0));
//
x(x) = Ωᵗ(1 + x(x $ 0) $ 0);
y(y) = 1 + y(y $ 1 + x(x $ 0));
1 + y(y $ 1 + x(x $ 0))
y = (y) => y(y $ Ωᵍ);
Ω = y(y $ 1 + G);
Ω = y(y $ 1 + G);

//
Ωᵍ(f : (G : Grade $ 0) -> ⊥ $ 1) =
  x(x $ 0) = f(1 + x(x $ 0) $ 0);
  x(x $ 0);
Ωᵗ(G $ 0) =
  y(y $ 1 + G) = 1 + y(y $ G);
  y(y $ 1 + G);
Ω = Ωᵍ(Ωᵗ $ 1);

y = (y) => y(y $ G);
y(y $ G)

x = (x) => Ωᵗ((1 + x(x $ 0)) $ 0);
x(x $ 0);

f(1 + x(x $ 0) $ 0)

// Hurken's
Not : Type -> Type
Not(A) = A -> ⊥

P : Type -> Type
P(A) = A -> Type

U : Type
U = (X : Type) -> (P(P(X)) -> X) -> P(P(X))

tau : P(P(U)) -> U
tau(t) = X => f => p => t(x => p(f(x(X)(f))))

sigma : U -> P(P(U))
sigma(s) = s(U)(tau)

Delta : P(U)
Delta(y) = Not((p : P(U)) -> sigma(y)(p) -> p(tau(sigma(y))))

Omega : U
Omega = tau(p => (x : U) -> sigma(x)(p) -> p(x))

D : Type
D = (p : P(U)) -> sigma(Omega)(p) -> p(tau(sigma(Omega)))

lem1 : (p : P(U)) -> ((x : U) -> sigma(x)(p) -> p(x)) -> p(Omega)
lem1(p)(h) = h(Omega)(x => h(tau(sigma(x))))

lem2 : Not(D)
lem2 = lem1(Delta)(x => h2 => h3 =>
  h3(Delta)(h2)(p => h3(y => p(tau(sigma(y))))))

lem3 : D
lem3(p) = lem1(y => p(tau(sigma(y))))

boom : ⊥
boom = lem2(lem3)


//
T : Type;
T = (x : T $ 0) -> Grade;

U : Type;
U = (G : Grade $ 0) -> (x : T $ 1 + G) -> Grade;

Ωᵍ(f : (G : Grade $ 0) -> Grade $ 1) =
  x : T = (x $ 0) => f(1 + x(x $ 0) $ 0);
  x(x $ 0);
y : U = (G) => (y) => 1 + y(G)(y $ G)

Ωᵗ(G $ 0) =
  y : U(G) = (y) => 1 + y(y $ G);
  y(y $ 1 + G);
Ω = Ωᵍ(Ωᵗ $ 1);

U(G : Grade $ 0) : Type;
U(G : Grade $ 0) = (x : T $ 1 + G) -> Grade;

Ωᵍ(f : (G : Grade $ 0) -> Grade $ 1) =
  x : T = (x $ 0) => f(1 + x(x $ 0) $ 0);
  x(x $ 0);
Ωᵗ(G $ 0) =
  y : U(G) = (y) => 1 + y(y $ G);
  y(y $ 1 + G);
Ω = Ωᵍ(Ωᵗ $ 1);
//
Ωᵍ(f $ 1) =
  x : T = (x $ 0) => f(1 + x(x $ 0) $ 0);
  x(x $ 0);
Ωᵗ(G $ 0) =
  y : U(G) = (y) => 1 + y(y $ G);
  y(y $ 1 + G);
Ω = Ωᵍ(Ωᵗ $ 1);
//
Ωᵍ(f) =
  x(x) = f(1 + x(x $ 0) $ 0);
  x(x $ 0);
Ωᵗ(G) =
  y(y) = 1 + y(y $ G);
  y(y $ 1 + G);
Ω = Ωᵍ(Ωᵗ $ 1);

//
T : Type;
T = (x : T $ 0) -> Grade;

f : (G : Grade) -> Grade;
x : T = (x) => f(1 + x(x $ 0) $ 0);
x(x $ 0);


Ω = Ωᵍ(Ωᵗ $ 1);
// which if you normalize
Ωᵗ(G) =
  y(y) = 1 + y(y $ G);
  1 + y(y $ G)
// but that's a stuck term

Term, M, N, K ::=
  | Type | (x : A) -> B | Grade | A $ G
  | x
  | (x) => K | M(N)
  | 0 | 1 | M + N | M * N
  | #(M, G) | #x = N; K

((x) => K)(N) |-> K{x := N}
#x = #(M, G); K |-> K{x := M}

Value, V ::=
  | Type | (x : Vᵃ) -> Vᵇ | Grade | Vᵃ $ VG
  | VS[x] | (x) => V | VG | #(V, VG)
Value Grade, VG :=
  | VS[x] | 0 | 1 | VGˡ + VGʳ | VGˡ * VGʳ
Value Spine, VS ::=
  | _ | VS(V) | #x = VS; VS[x]

//
M(N)
#(M, G)

// types
F(Type) == 0
F((x : A) -> B) == F(A) + F(B)
F(Grade) == 0
F(A $ G) == F(A) + F(G)
// grades are inert
F(0) == 0
F(1) == 0
F(M + N) == F(M) + F(N)
F(M * N) == F(M) + F(N)

// compute
F(x) == 0
F((x : A) => K) == F(K)
F(M(N)) == 1 + F(M) + F(N)
F(#(M, G)) == F(M) * T(G)
F(#x = N; K) == 1 + F(N) + F(K)

T(0) == 1
T(1) == 1
T(M + N) == T(M) + T(N)
T(M * N) == T(M) * T(N)
T(x) == 0 // free
T(x) == ω // bound


((x) => #(K, x))(1 + a)
((f) => #(K, f(N)))(M)
#(K, M(N))

((x) => x(Vˡ))((y) => Vʳ)

((x) => VSˡ[x])(#(M, G))

VSˡ[((y) => VSʳ[y])]

((x) => VS[x])

((x) => #(K, f(x)))(1 + a)

l | ((x) => #(K, G[x]))(1 + a)


((x) => #(K, G[x]))(N)
((x) => #y = #(M, G[x]); K)(N)

ω * F(M) * T(G[N])

F(M) * T(G[N])

#y = #(M, G[N]); K

#y = #(M, G[N]); y(A)

((x) => #(M, x))(a * b)


//
Term, M, N, K, G ::=
  | 0 | s(G) | M + N | M * N
  | x | (x) => K | M(N $ G)

Value, V ::=
  | 0 | s(V) | (x) => V | VN
Value Neutral, VN ::=
  | x | VN + V | VN * V | VN(V)
((x) => K)(V $ G) |-> K{x := V}  (count(x, K, V) == G)

F(0) == 1
F(s(G)) == 1 + F(G)
F(M + N) == F(M) + F(N)
// TODO: add or mul?
F(M * N) == F(M) * F(N)
F(x) == 0
F((x) => K) == 1 + F(K)
F(M(N $ G)) == F(M) + (T(G) * F(N)) + F(G)

((x) => (f) => f(x $ 1)(x $ 1))(V $ 2)
(f) => f(V)(V)

((x) => K)(V $ G) |-> K{x := V}

((x) => x + x)(2 $ 2)
2 + 2



F(K{x := V}) == max(F(K<_>), F(V))


((x) => [x, x])(a(b) $ 1)
1 + max(F(f(x)), F(I), 1)

I(x)
F(I(x))
1 + max(F(f(x)), 0, 1)

((x) => M(N $ x))(K $ 0)
max(1 + max(F(M), F(N), F(x)), F(K), 1)

M(N $ K)
max(F(M), F(N), F(K))

T(0) == 0
T(s(G)) == 1 + T(G)
T(M + N) == T(M) + T(N)
T(M * N) == T(M) * T(N)
T(x) == 0  // free
T(x) == ω  // bound
T(x(N)) ==
T(x())

//
T : Type;
T = (x : T $ 0) -> Grade;

f : (G : Grade) -> Grade;
x : T = (x) => f(1 + x(x $ 0) $ 0);
x(x $ 0);
//
f(1 + x(x $ 0) $ 0)

(λ. M(N $ \1))

((x) => K(M(N $ x) $ x))

(F(N) * ω)

F()

((x) => x)(y) == y

((x) => x)(y) == N


y == ((x) => x)(y)
((x) => x)(y) == y
------------------
y == ((x) => x)(y)

y == x
x == y
----------
y == x


M == N
N == R
------
M == R


Term, M, N, K, G ::=
  | 0 | 1 | M + N | M * N
  | x | (x) => K | M(N $ G)

((x) => K)(N $ G) |-> K{x := N}  (count(x, K{x := N}) == G)

// bad
x(x) = f(1 + x(x $ 0) $ 0);
Ω¹ = x(x $ 0);
Ω² = f(1 + Ω¹ $ 0);

// likely good
Term, M, N, K, G ::=
  | #many(M, G) | #x = N; K
  | x | (x) => K | M(N)

((x) => K)(N $ G) |-> K{x := N}  (count(x, K) == 1)
// subst doesn't go through M in #many
#x = #many(M, G); K |-> K{x := M}  (fv(M) == ∅)



//
Ω = Ωᵗ(1 + x(x $ 0) $ 0);
Ω = Ωᵗ(1 + Ωᵗ(1 + x(x $ 0) $ 0) $ 0);

//
Ωᵗ(G) =
  y(y) = 1 + y(y $ G);
  y(y $ 1 + G);
x(x) = Ωᵗ(1 + x(x $ 0) $ 0);
Ωᵗ(1 + x(x $ 0) $ 0);

//
Term, M, N, K ::=
  | x | (x) => K | M(N)
  | Type | (a : A) -> B

// Hurken's
Not : Type -> Type
Not(A) = A -> ⊥

P : Type -> Type
P(A) = A -> Type

U : Type
U = (X : Type) -> (P(P(X)) -> X) -> P(P(X))

tau : P(P(U)) -> U
tau(t) = X => f => (p : P(X)) : Type =>
  t((x : P(U)) : Type => p(f(x(X)(f))));

sigma : U -> P(P(U))
sigma(s) = (p : P(U)) : Type => s(U)(tau)(p)

Delta : P(U)
Delta(y) : Type = Not((p : P(U)) -> sigma(y)(p) -> p(tau(sigma(y))))

Omega : U
Omega = X => f => p => tau(p => (x : U) -> sigma(x)(p) -> p(x))(X)(f)(p)


D : Type
D = (p : P(U)) -> sigma(Omega)(p) -> p(tau(sigma(Omega)))

lem1 : (p : P(U)) -> ((x : U) -> sigma(x)(p) -> p(x)) -> p(Omega)
lem1(p)(h) = h(Omega)(x => h(tau(sigma(x))))

lem1_Delta : ((x : U) -> sigma(x)(Delta) -> Delta(x)) -> Delta(Omega)
lem1_Delta(h) = (a) : ⊥ => h(Omega)(x => h(tau(sigma(x))))(a);

lem2 : Not(D)
lem2 = (d : D) : ⊥ => lem1_Delta(x => h2 => h3 : Delta(x) =>
  (a) : ⊥ => h3(Delta)(h2)(p => h3(y => p(tau(sigma(y)))))(a))

lem3 : D
lem3(p) = lem1(y => p(tau(sigma(y))))

boom : ⊥
boom = lem2(lem3)

//
// Hurken's
Not : Type -> Type
Not(A) = A -> ⊥

P : Type -> Type
P(A) = A -> Type

U : Type
U = (X : Type) -> (P(P(X)) -> X) -> P(P(X))

tau : P(P(U)) -> U
tau(t) = X => f => (p : P(X)) : Type =>
  t((x : P(U)) : Type => p(f(x(X)(f))));

sigma : U -> P(P(U))
sigma(s) = (p : P(U)) : Type => s(U)(tau)(p)

Delta : P(U)
Delta(y) : Type = Not((p : P(U)) -> sigma(y)(p) -> p(tau(sigma(y))))

Omega : U
Omega = X => f => p =>
  q = (x : P(U)) : Type => p(f(x(X)(f)));
  (x : U) ->
    x(U)(tau)((p : P(U)) : Type => s(U)(tau)(p)) ->
    p(f(x(X)(f)));

id = (A : Type $ 0) => (a : A) => a;

Eq(A, x, y) = (C : (a : A) -> Type $ 0, r : C(x)) -> C(y);
//
x(x) = f(x(x $ 0), 0);
Ω = x(x $ 0);
//
id = (G : Grade $ 0) => (A : Type $ 0) => (a : A $ G) => a;

//

Term, M, N, K ::=
  | x | (x) => K | M(N)
  | 0 | 1 + N | rec(M, N, K)

((x) => K)(N) |-> K{x := N}  (count(x, K) <= 1)
// subst doesn't go through N in rec
rec(0, N, K) |-> K
rec(1 + M, N, K) |-> N(rec(M, N, K))  (fv(N) == ∅)

//
Term, M, N, K ::=
  | x | (a : A) -> B | (x) => K | M(N)
  | Type(l) | Box_T(A) | box_T(M) | out_T(M)
  | Line(l) | Box_L(A) | box_L(M) | out_L(M)


((x) => K)(N) : _ : Line(l) |-> K{x := N}
((x) => K)(N) : _ : Type(l) |-> K{x := N}


// aka M is closed over linear values
Γ, • |- M : A : Line(l)
---------------------------
Γ, • |- box_T(M) : Box_T(A)

Bool = Box_T(L_Bool);
b : Bool;
// inside linear
out_L(out_T(b)(Box_L(A))(box_L(t))(box_L(f)))
// outside it's type based?
// went from Box_L(A) to A


// something about the way they interact


Type(l) : Type(1 + l)
Line(l) : Line(1 + l)
(a : A : Sˡ) -> (B : Sʳ) : Type(max(Sˡ, Sʳ))

Box(A : Type(l)) : Line(l)
box(M : A) : Box(A)
out(M : Box(A)) : A

Unit = (A : Line(0)) -> (a : A) -> A;
Bool : Line(1) = (A : Line(0)) -> (t : A) -> (f : A) -> A;
Either(A : Type(0), B : Type(0)) : Line(0) = [
  b : Bool,
  p : ⇑b(Line(0))(Box(A))(Box(B))
]


//
Term, M, N, K ::=
  | x | (x) => K | M(N)
  | (a : A) -> B | (x : *) -> T | Type(l)

// TODO: like a PTS
Γ, x : * |- T : *
---------------------------
Γ |- (x : *) -> T : Type(0)

----------------------------
Γ |- (x) => K : (x : *) -> T

((x : *) => K)(N : Type(l)) |-> K{x := N}


((x : *) : T => K)(N : Type(l)) : (T{x := N} : Type(l))


((x) => K)(N) : _ : Line(l) |-> K{x := N}
((x) => K)(N) : _ : Type(l) |-> K{x := N}


Term, M, N, K ::=
  | x | (x) => K | M(N)
  | 0 | 1 + M | rec(M, N, K)

((x) => K)(N) |-> K{x := N}  (count(x, K) <= 1)
// subst doesn't go through N in rec
rec(0, N, K) |-> K
rec(1 + M, N, K) |-> N(rec(M, N, K))  (fv(N) == ∅)

F(x) == 0
F((x) => K) == 1 + F(K)
F(M(N)) == 1 + F(M) + F(N)
F(0) == 1
F(1 + M) == 2 + F(M)
F(rec(M, (l := N), K)) == F(M) + F(K)

rec(2 + M, (l := N), K)
l(l(rec(M, (l := N), K)))

l = (x) => _;

rec(2, (l := N), K)

l = N;

((x) => f(l(l(rec(M[x], l, K)))))


F(x = N; K) == F(N) + (ω * F(K))

l = N; l(l(K))


T(0) == 0
T(1 + M) == 1 + T(M)
T(V) == ω  (some bv in M)
T(V) == 0  (no bv in V)
T(M) == ω

((x) => C[x])(N) |-> C[N]

l | ((x) => rec(x, N, K))(M)

F(((x) => Vᵏ)(Vⁿ)) > F(Vᵏ{x := Vⁿ})

((x) => x(Vˡ))(Vʳ) |-> Vⁿ(Vˡ)
((x) => rec(x, Vⁿ, Vᵏ))(Vʳ) |-> rec(Vʳ, Vⁿ, Vᵏ)

((x) => rec(x, Vⁿ, Vᵏ))(Vʳ)

((x) => rec(x + y, N, K))(1)
rec(1 + y, N, K)


//
Term, M, N, K ::=
  | x | (x) => K | M(N)
  | 0 | 1 + M | rec(M, N, K)

// consumes the lambda, but may increment the recursor
((x) => K)(N) |-> K{x := N}  (count(x, K) == 1)
// consumes the 0, strictly decreasing
rec(0, N, K) |-> K
// consumes the 1, may create new lambda, but lower rank
rec(1 + M, N, K) |-> N(rec(M, N, K))  (fv(N) == ∅)

Context, C ::=
  | _ | (x) => _ | C(N) | M(C)
  | 1 + C | rec(C, N, K) | rec(M, C, K) | rec(M, N, C)


//
F(x) == 0
F((x) => K) == (ω * degree(K)) + F(K)
F(M(N)) == F(M) + F(N)
F(0) == 1
F(1 + M) == 1 + F(M)
F(rec(M, N, K)) == F(M) + F(N) + (T(M) * F(N)) + F(K)

T(0) == 0
T(1 + M) == 1 + T(M)
T(M) == 0


((x) => K)(N) |-> K{x := N}
((x) => (y) => rec(x, N, K))(M)

C[_] +


((k) => k(1 + M))((x) => rec(x, N, K))
((x) => rec(x, N, K))(1 + M)

F(((x) => rec(x, N, K))(1 + M))
F((x) => rec(x, N, K)) + F(1 + M)
(ω * (1 + degree(N))) + F(N) + F(K) + 1 + F(M)


F(rec(1 + M, N, K))
1 + F(M) + F(N) + (T(1 + M) * F(N)) + F(K)

F(N(rec(M, N, K)))
F(N) + F(rec(M, N, K))
F(N) + F(M) + F(N) + (T(M) * F(N)) + F(K)

(ω * (1 + degree(N))) + F(N) + F(K) + 1 + F(M)


((x) => rec(x, N, K))(M)
rec(M, N, K)

((x) => K)(N) |-> K{x := N}  (count(x, K) == 1)

//
((x) => K)(N) |-> K{x := N}  (count(x, K) == 1)
// subst doesn't go through N in rec
rec(0, N, K) |-> K
rec(1 + M, N, K) |-> N(rec(M, N, K))  (fv(N) == ∅)

((x) => rec(x, N, K))(M)

((x) => rec(x, N, K))(1 + M)


rec(M, N, K)

F(rec(1 + M, N, K)) |-> F(N(rec(M, N, K)))  (fv(N) == ∅)

rec_1(2 + M, (x) => rec_0(x, _, _), K)

rec_0(rec_0(rec_1(M, (x) => rec_0(x, _, _), K), _, _), _, _)

N{x := rec(M, N, K)}
N(rec(M, N, K))


//

//
P : Type -> Type
P(A) = A -> Type

U : Type
U = (X : Type) -> (P(P(X)) -> X) -> P(P(X))

P(A)

Phi(A) = (A -> Prop) -> Prop;


P(A : Type) = A -> Type;

// scott encoding
A : Type;
A = (X : Type) -> (P(P(A)) -> X) -> X;
introA(p : P(P(A))) : A = (X) => (f) => f(p);
matchA(a : A) : P(P(A)) = a(P(P(A)), (p) => p);
beta(p) : matchA(introA(p)) == p = refl();

// you can also just notice that this is just a box
A : Type;
A = [p : P(P(A))];
introA(p : P(P(A))) : A = [p];
matchA(a : A) : P(P(A)) = a.p;

// which is equivalent to
A : Type;
A = P(P(A));
// but that's equivalent to
A : Type;
A = P(A);
// which to remember
A : Type;
A = A -> Type;
// WHICH IS REALLY BAD


introA(p : P(P(A))) : A = p;
matchA(a : A) : P(P(A)) = a;
beta(p) : matchA(introA(p)) == p = refl();

// but that

// well that's negative recursion
A : Type;
A = P(A);
introA(p : P(P(A))) : A = p;
matchA(a : A) : P(P(A)) = a;
beta(p) : matchA(introA(p)) == p = refl();
// but we know that they're equivalent
L : Type;
L = P(P(L));

R : Type;
R = P(R);
// then L == R

// scott encoding
A : Type;
A = (X : Type) -> (P(P(A)) -> X) -> X;
introA(p : P(P(A))) : A = (X) => (f) => f(p);
matchA(a : A) : P(P(A)) = a(P(P(A)), (p) => p);
beta(p) : matchA(introA(p)) == p = refl();

// with Type : Type you can try just church encoding
A = (X : Type) -> (P(P(X)) -> X) -> X;
// TODO: this fails without Type : Type???
introA(p : P(P(A))) : A = (X) => (f) =>
  f((px : P(X)) => p((a) => px(a(X)(f))));
lowerA(p : P(P(P(P(A))))) : P(P(A)) =
  (r : P(A)) => p((x : P(P(A))) => x(r));
matchA(a : A) : P(P(A)) = a(P(P(A)), lowerA);


// ????
P(A : Type(1)) = A -> Type(0);
// with Type : Type you can try just church encoding
A : Type(1) = (X : Type(1)) -> (P(P(X)) -> X) -> X;
// this fails without Type : Type
introA(p : P(P(A))) : A = (X) => (f) =>
  f((px : P(X)) => p((a : A) => px(a(X)(f))));
matchA(a : A) : P(P(A)) =
  (r) => a(Type, )
    r((X) => (f) => _);
beta(p) : matchA(introA(p)) == p = refl();

matchA(introA(p))
introA(p)(A, introA)
introA((px : P(A)) => p((a : A) => px(a(A)(introA))))

f = (px : P(A)) => p((a : A) => px(a(A)(introA)));
f((px : P(X)) => p((a : A) => px(a(X)(f))))
((px : P(X)) => p((a : A) => px(a(X)(f))))

f((px : P(X)) => p((a : A) => px(a(X)(f))))



//
P(A : Type) = A -> Type;
// the bad one
A : Type;
A = P(A);

in : P(A) -> A;
out : A -> P(A);
beta(p) : out(in(p)) == p = refl();


// church encoded
A : Type;
A = (X : Type) -> (P(A) -> X) -> X;

//

// this can be constructed properly
A : Type = (X : Type) -> (P(X) -> X) -> X;
// but what if I specialize it?
A : Type = (X : Type) -> (P(P(X)) -> P(X)) -> P(X);
// same as before, P(A) == A and we expect X to be A
A : Type = (X : Type) -> (P(X) -> X) -> P(X);

//
A : Type = (X : Type) -> (P(X) -> X) -> X;

introA(p : P(P(A))) : A =
  (X) => (f) => f((x) => p((a) => a(X, f)(x)));

(f) => f((x) => p((a) => a(A, f)(x)))
matchA(a : A) : P(P(A)) =
  (p : P(A)) => a(Type, _ : P(Type) -> Type)
lowerA(p : P(P(P(A)))) : P(P(A)) =
;
matchA(a : A) : P(P(A)) =
  a(P(P(A)), (p : P(P(P))))
  a(A, introA);
introA(p : P(P(A))) : A =
  (X) => (f) => (x) => p((a) => a(X, f)(x));
liftA(p : P(A)) : P(P(A)) =
  p(introA());
lowerA(p : P(P(A))) : P(A) = (a) =>
  a(A, )(a);
matchA(a : A) : P(A) = lowerA(a(P(A), lowerA));
matchA(a : A) : P(P(A)) =
  (r : P(A)) => r((X) => (f) => _ : P(X));

(x) => p((a) => a(A, f)(x))

(X) => (f) => (x) => p((a) => a(X, f)(x))

lowerA(p : P(P(A))) : P(A) = (a) => p(a(A, introA));
matchA(a : A) : P(A) = lowerA(a(P(A), lowerA));
//
A : Type = (X : Type) -> (P(X) -> X) -> P(X);
U : Type = (X : Type) -> (P(P(X)) -> X) -> P(P(X));


in(p : A) : U =
  a(P(U), (ppu : PPU(U)) =>
    u = tau(ppu);
    _ : P(u)
    a(U, (pu : P(U)) => U)
    _ P(U))


introA(p : P(A)) : A =
  (X) => (f) =>

matchA(a : A) : P(A) =

introA(p : P(A)) : A =
  (X) => (f) => _
lowerA(p : P(P(A))) : P(A) = (a) => p(a(A, introA));
matchA(a : A) : P(A) = lowerA(a(P(A), lowerA));

in{A}(p : P(P(A))) : P(A) =
  (a : A) => p((b) => _ : P(A))
introA(p : P(P(A))) : A = (X) => (f) =>
  f((x : X) =>
    p((a) => a(X)(f) : X)
  )
  f((x : X) => p(_));
matchA(introA(p))
x : A = (X) => (f) => f((x : X) => _ : P(X))
// but what if I specialize it

P(A : Type) = A -> Type;
// scott encoding
A : Type;
A = (X : Type) -> (P(P(A)) -> X) -> X;
introA(p : P(P(A))) : A = (X) => (f) => f(p);
matchA(a : A) : P(P(A)) = a(P(P(A)), (p) => p);
beta(p) : matchA(introA(p)) == p = refl();

// but you don't need all the types, just enough for beta
// so let's specialize it
A = (X : Type) -> (P(P(P(P(X)))) -> P(P(X))) -> P(P(X));
// but we know we only care about instantiating with A
// and we know that P(P(A)) == A, so let's just compact
A = (X : Type) -> (P(P(X)) -> X) -> P(P(X)); // that's U!!!
introA(p : P(P(A))) : A = (X) => (f) =>
  // the goal is to return something equivalent to p
  // when X is A, this is just eta
  (px : P(X)) => p((a) => px(a(X)(f)));
matchA(a : A) : P(P(A)) = a(A, introA);
beta(p) : matchA(introA(p)) == p = refl();
// because
matchA(introA(p)) == introA(A, introA)
matchA(introA(p)) == (px : P(A)) => p((a) => px(a(A)(introA)))
// but hopefully a(A)(introA) == a, so:
matchA(introA(p)) == (px : P(A)) => p((a) => px(a))
matchA(introA(p)) == (px) => p(px)
matchA(introA(p)) == p




// but we kno
//


matchA(introA(p)) == p
beta(p)

// well just the pair
A = [p : P(P(A))];
// which means
τ
iso : A == P(P(A)) = ua(τ, σ, _);
// but then


// scott encoding
A : Type;
A = (X : Type) -> (P(P(A)) -> X) -> X;
introA(p : P(P(A))) : A = (X) => (f) => f(p);
matchA(a : A) : P(P(A)) = a(P(P(A)), (p) => p);
beta(p) : matchA(introA(p)) == p = refl();


// the bad one
A : Type;
A = P(A);

in : P(A) -> A;
out : A -> P(A);
beta(p) : out(in(p)) == p = refl();

//
P : Type → Type
P A = A → Type

And : Type → Type → Type
And A B = (R : Type) → (A → B → R) → R

Exists : (A : Type) → P A → Type
Exists A p = (R : Type) → ((x : A) → p x → R))

Id : (A : Type) → A → A → Type
Id A x y = (p : P A) → p x → p y

map : (A : Type) → (B : Type) → (A -> B) → P A → P B
map A B f = λ A → λ b → Exists A (λ a → And (A a) (Id B b (f a)))

μP : Type
μP = (α : Type) → (P α → α) → α

fold : (α : Type) → (P α → α) → μP → α
fold α f A = A α f

roll : P μP → μP
roll A = λ α → λ f → f (map μP α (fold α f) A)

unroll : μP → P μP
unroll = fold (P μP) (map (P μP) μP roll)
//
A : Type = (X : Type) -> (P(X) -> X) -> P(X);

Exists(A : Type, p_a : P(A)) : TYpe
Exists(A, p) = (R : Type) -> (x : A) -> p x -> R;

Id : (A : Type) → A → A → Type
Id A x y = (p : P A) → p x → p y

map(A, B, f(a : A) : B, p_a : P(A)) : P(B);
map(A, B, f, p_a) = (b) =>
  Exists(A, (a) => )

//

U : Type = (X : Type) -> (P(P(X)) -> X) -> P(P(X));
tau : P(P(U)) -> U
tau(t) = X => f => p => t(x => p(f(x(X)(f))))

sigma : U -> P(P(U))
sigma(s) = s(U)(tau)

Omega : U
Omega = tau(p => (x : U) -> sigma(x)(p) -> p(x))

weird(x : P(U)) : U =
  tau((y) => x == y);

A : Type = (X : Type) -> (P(X) -> X) -> P(X);
introA(p : P(P(A))) : A = (X) => (f) =>
  (x) => p((a) => a(X, f)(x));

f(a : A) : U = weird(a(U, weird));
g(u : U) : A = introA(u(A, introA));

x : P(A);
weird((u) => x(g(u)))
tau((y) => ((u) => x(g(u))) == y)
inner =
  X => f => p => ((u) => x(g(u))) == ((u) => p(f(u(X)(f))));

introA(inner(A, introA))

r = (p : P(A)) =>
  ((u) => x(g(u))) == ((u) => p(introA(u(A)(introA))));
introA(r)


(X) => (f) => (x2) => r((a) => a(X, f)(x2))

(X) => (f) => (x2) =>
  ((u) => x(g(u))) == ((u) => g(u))(X, f)(x2)




introA(r)


introA(p : P(P(A))) : A = (X) => (f) =>
  (x) => p((a) => a(X, f)(x));
weirdA(x : P(A)) : A =
  g(weird((u) => x(g(u))));

OmegaA = (X) => (f) => (x) =>
  p = u => g(u)(X, f)(x);
  (x : U) -> sigma(x)(p) -> p(x);
OmegaA = (X) => (f) => (x) =>
  p = u => g(u)(X, f)(x);
  (x : U) -> x(U)(tau)(p) -> p(x);
OmegaA = (X) => (f) => (x) =>
  p = a => a(X, f)(x);
  (x : A) -> weird(a(U, weird))(a)(A)(introA)(p) -> p(a);
OmegaA = (X) => (f) => (x) =>
  p = a => a(X, f)(x);
  (x : A) -> tau((y) => a(U, weird) == y)(a)(A)(introA)(p) -> p(a);

OmegaA = (X) => (f) => (x) =>
  Omega(A, introA)((a) => a(X, f)(x));
OmegaA = (X) => (f) => (x) =>
  t = p => (x : U) -> sigma(x)(p) -> p(x);
  tau(t)(A, introA)((a) => a(X, f)(x));
OmegaA = (X) => (f) => (x) =>
  t = p => (x : U) -> sigma(x)(p) -> p(x);
  p = (a) => a(X, f)(x);
  t(u => p(introA(u(A)(introA))));
OmegaA = (X) => (f) => (x) =>
  t = p => (x : U) -> sigma(x)(p) -> p(x);
  t(u => introA(u(A)(introA))(X, f)(x));
OmegaA = (X) => (f) => (x) =>
  p = u => introA(u(A)(introA))(X, f)(x);
  (x : U) -> sigma(x)(p) -> p(x);
OmegaA = (X) => (f) => (x) =>
  t = p => (x : U) -> sigma(x)(p) -> p(x);
  p = (a) => a(X, f)(x);
  t(u => p(introA(u(A)(introA))));
OmegaA = (X) => (f) => (x) =>
  tau(p => (x : U) -> sigma(x)(p) -> p(x))(A, introA)((a) => a(X, f)(x))


  a(P(U), (ppu) =>

    _ : P(U)
    )
introA()

introA(p : P(A)) : A =
  (X) => (f) => (x) =>

    map(A, X, p);
introA(p)(A, introA) == p

introA(p : P(A)) : A =
  (X) => (f) => (x) =>
    p((Y) => (g) => (y) => _)

matchA(a : A) : P(A) = a(A, introA);


P(A) = A -> Type;

data A : Type =
  | introA(p : P(P(A)));
// same as
A : Type;
A = (X : Type) -> (P(P(A)) -> X) -> X;
// same as
A : Type;
A = P(P(A));
introA(p : P(P(A))) : A = p;
matchA(a : A) : P(P(A)) = a;
// which if equirecursive is the same as
A : Type;
A = P(A);
// if you try to encode using nu, it doesn't work
A : Type;
A = (X : Type) -> (P(X) -> X) -> X;
// but you only need a specific instance, P(A)
A = (X : Type) -> (P(P(X)) -> P(X)) -> P(X)
// but we know that P(A) will be equivalent to A
A = (X : Type) -> (P(X) -> X) -> P(X);

// only goal is to have an injective f
A : Type;
f(a : P(A)) : A;

//
A = (X : Type) -> (P(X) -> X) -> P(X);
f(x : P(A)) : A = tauA((y) => x == y);

tauA(p : P(P(A))) : A = (X) => (f) => (x) =>
  p((a) => a(X)(f)(x));
introA(p : P(A)) =
  introA((q) => )
  (X) => (f) => (x) =>
  x == f(_ : P(X))

//
Term, M, N, K ::=
  | x
  | Π(a : A) -> B | (a : A) => K | M(N)
  | Σ(a : A) -> B | [M, N] | M.0 | M.1
  | Type(l)

Γ, A : * |- B : *
----------------------------
Γ |- Π(A : *) -> B : Type(0)

Γ |- M : Π(A : *) -> B  Γ |- N : Type(l)
----------------------------------------
Γ |- M(N) : B{A := N}

Unit : Type(0) = Π(A : *, r : A) -> A;
Bool : Type(0) = Π(A : *, t : A, f : A) -> A;

Unit : Type(0);

Bool : Type(0);
case{l}(b : Bool, A : Type(l), t : A, f : A) : A

Nat : Type(0);
Nat = [
  b : Bool,
  n : case(b, Type(0), Unit, Nat),
];
fold{l}(n : Nat, A : Type(l), z : A, s(a : A) : A) : A;
fold{l}(n, A, z, s) =
  [b, m] = n;
  ind{l}(
    b,
    C(b) = (m : case(b, Type(0), Unit, Nat)) -> A,
    (m) => z,
    (m) => s(fold(m, A, z, s))
  )(m);

T(b : Bool) : Type(0) = b(Type(0), Unit, Bool);

(⊥ -> ⊥ -> ⊥) : Type(0)
(A -> A -> A)⇑{A : Type(l)} : Type(l)


D : Type;
D = [
  p : P(P(D))
]

Nat = ∀A.

Term : Type;
Term = [
  tag : String,
  p :
    tag.is(
    | "var"
    )
];

List(A) : Type;
List(A) = [
  b : Bool,
  x : case(b, Type(0), Unit, List(A))
];

Nat = ∀A. A -> (A -> A) -> A;
List(A) : Type = [
  n : Nat,
  l : n(Type(0), Unit, (T) => [hd : A, tl : T])
];

Type, A, B ::=
  | Type(ω) | Type(l) | Π(a : A) -> B | El(M)
Term, M, N, K ::=
  | code(A) | x | (a : A) => K | M(N)

Sort, S ::=
  | Type(ω) | Type(l) | Kind(l)

R =
  | [Type(l), Type(r), Type(max(l, r))]
  | [Type(l), Kind(r), Kind(max(l, r))]
  | [Kind(l), Type(ω), Type(l)]

Γ |- A : Type(l)  Γ, a : A |- B : Kind(r)
-----------------------------------------
Γ |- Π(a : A) -> B : Kind(max(l, r))

Γ |- A : K_A  Γ, a : A |- B : K_B  [K_A, K_B, K] ∈ R
----------------------------------------------------
Γ |- Π(a : A) -> B : K

-----------------------
Γ |- Type(ω) : Kind(0)

Γ |- M : Type(l)
---------------- // cumulativity?
Γ |- M : Type(ω)

Id : Type(0) = Π(A : Type(ω)) -> A -> A;
id : Id;
x = id(Id); // works by cumulativity

//
Type, A, B ::=
  | Set | Π(a : A) -> B | El(M)
Term, M, N, K ::=
  | code(A) | x | (a : A) => K | M(N)

Sort, S ::=
  | Set | Type | Kind

R =
  | [Type(l), Type(r), Type(max(l, r))]
  | [Type(l), Kind(r), Kind(max(l, r))]
  | [Kind(l), Type(ω), Type(l)]


Term, M, N, K ::=
  | Set | * | Π(a : A) -> B
  | x | (a : A) => K | M(N)
Sort, S ::=
  | Set | Type | * | Kind

---------------
Γ |- Set : Type

Γ |- A : {Set,Type}  Γ, a : A |- B : Set
---------------------------------------------
Γ |- Π(a : A) -> B : Set

Γ |- A : {Set,Type}  Γ, a : A |- B : Type
-----------------------------------------
Γ |- Π(a : A) -> B : Type

// new stuff

// * is empty

-------------
Γ |- * : Kind

// no Kind parameter
Γ |- A : {Set,Type}  Γ, a : A |- B : Kind
-----------------------------------------
Γ |- Π(a : A) -> B : Kind

Γ |- A : Kind  Γ, a : A |- B : *
--------------------------------
Γ |- Π(a : A) -> B : Set

Γ |- A : Kind
Γ |- M : Π(a : A) -> B  Γ |- N : A{* := Set}
--------------------------------------------
Γ |- M(N) : B{A := N}

Γ |- A : Kind
Γ |- M : Π(a : A) -> B  Γ |- N : A{* := Type}
---------------------------------------------
Γ |- M(N) : B{A := N}

Bool : Set = Π(A : *) -> A -> A -> A;
true : Bool = (A) => (t) => (f) => t;
f = (b : Bool) => (x : b(Set, Bool, Nat)) => x;

∀A -> A -> A


Bool : Set = Π(A : Type) -> A -> A -> A;
true : Bool = (A) => ((t) => (f) => t : A -> A -> A);
Nat : Set = Π(A : Type) -> A -> (A -> A) -> A;
f = (b : Bool) => (x : b(Set, Bool, Nat)) => x;

id : Id = (A) => (a) => a;
id : Id = id((Id : Set) : Type);



Γ |- A : K_A  Γ, a : A |- B : K_B  [K_A, K_B, K] ∈ R
----------------------------------------------------
Γ |- Π(a : A) -> B : K

-----------------------
Γ |- Type(ω) : Kind(0)

Γ |- M : Type(l)
---------------- // cumulativity?
Γ |- M : Type(ω)

Id : Type(0) = Π(A : Type(ω)) -> A -> A;
id : Id;
x = id(Id); // works by cumulativity







Nat = ∀(C : Unit -> *) -> C() -> (C() -> C()) -> C();
Eq(A, x : A, y : A) = ∀(C : A -> *). C(x) -> C(y);

List(A) : Type;
List(A) = [
  b : Bool,
  n : case(b, Type(0), Unit, Nat)
];

F(A) == G(+A, -A)

∀A. F(A)

∀A. N(A) -> A
∀A. Unit -> A
∀A. A -> A
∀A. [l : L(A), r : R(A)] -> A
∀A. (P(A) -> A) -> A

//

∀A. [l : L(A), r : R(A)] -> A

∀A. [l : L(A), r : R(A)] -> A
∀A. [l : Unit -> A, r : Unit -> A] -> A
∀A. [l : A -> A, r : A -> A] -> A
∀A. [l : Unit -> A, r : Unit -> A] -> A

μT. ∀A. G(T, -A)


//

Sort, S ::=
  | Type(ω) | Type(l) | Kind(l)

R =
  | [Type(l), Type(r), Type(max(l, r))]
  | [Type(l), Kind(r), Kind(max(l, r))]
  | [Kind(l), Type(ω), Type(l)]

----------------------
Γ |- Type(ω) : Kind(0)

Γ |- A : Type(l)  Γ, a : A |- B : Kind(r)
-----------------------------------------
Γ |- Π(a : A) -> B : Kind(max(l, r))

Γ |- A : Kind(l)  Γ, a : A |- B : Type(r)
-----------------------------------------
Γ |- Π(a : A) -> B : Type(max(l, r))

Γ |- A : Kind  Γ |- M : Π(a : A) -> B  Γ |- N : A{ω := l}
---------------------------------------------------------
Γ |- M(N) : B{a := N}

//
//


Γ, l : Level |- A : Kind(r)
Γ, l : Level, a : A |- B : Type(l)
-----------------------------------------------
Γ |- Π(l : Level, a : A) -> B : Type(r{l := 0})

// it must be this form, because it must return l
Π(l, )


Γ |- A : K_A  Γ, a : A |- B : K_B  [K_A, K_B, K] ∈ R
----------------------------------------------------
Γ |- Π(a : A) -> B : K

-----------------------
Γ |- Type(ω) : Kind(0)

Γ |- M : Type(l)
---------------- // cumulativity?
Γ |- M : Type(ω)

Id : Type(0) = Π(A : Type(ω)) -> A -> A;
id : Id;
x = id(Id); // works by cumulativity


//
Type, A, B ::=
  | Type(ω) | Type(l) | Π(a : A) -> B | El(M)
Term, M, N, K ::=
  | code(A) | x | (a : A) => K | M(N)

------------------------------------------
Γ |- Σ(l : Level $ 0) -> Type(l) : Kind(0)

Γ |- A : Type(l)  Γ, a : A |- B : Kind(r)
-----------------------------------------
Γ |- Π(a : A) -> B : Kind(max(l, r))

Γ |- A : Kind(l)  Γ, a : A |- B : Type(r)
-----------------------------------------
Γ |- Π(a : A) -> B : Type(max(l, r))

Γ |- M ⇐ Σ(l : Level) -> Type()


Id : Type(0) = Π(A : Σ(l : Level $ 0) -> Type(l)) -> A.1 -> A.1;
id : Id;
x = id([0, Id])(id);



Γ |- A : Kind(l)
Γ |- M : Π(a : A) -> B  Γ |- N : A{ω := r}
------------------------------------------
Γ |- M(N) : B{a := N}


//
Type, A, B ::=
  | Type(ω) | Type(l) | Π(a : A) -> B | El(M)
Term, M, N, K ::=
  | code(A) | x | (a : A) => K | M(N)

----------------------
Γ |- Type(l) : Kind(0)

Γ, l : Level |- T : Kind(r + l)
--------------------------------
Γ |- Σ(l : Level) -> T : Kind(r)

Γ |- A : Type(l)
----------------------------------
Γ |- Π(a : A) -> Type(r) : Kind(l)

Γ |- A : Kind(l)  Γ, a : A |- B : *
------------------------------------
Γ |- Π(a : A) -> Π(b : B) -> a() : Type(max(l, r))



Π(l : Level) -> Π(A : Type(l)) ->

Π(a : Σ(l : Level) -> Π(z : A) -> Type(l)) ->


Eq(A, x : A, y : A) =
  Π(C(z : A) : *, r : C(x)) -> C(y);

Eq(A, x : A, y : A) =
  Π(l : Level, C : Π(z : A) -> Type(l)) ->
    Π(r : C(x)) -> C(y);

Π(A : *) -> N(A) -> A
Π(A : *) -> A -> A
Π(A : *) -> [l : L(A), r : R(A)] -> A
Π(A : *) -> (P(A) -> A) -> A

Π(A : *) -> [l : A, r : P(A) -> A] -> A


Π(A : Type(ω)) -> A -> A


Id : Type(0) = Π(A : Type(ω)) -> A -> A;

Id : Type(0) = Π(A : Σ(l : Level $ 0) -> Type(l)) -> A.1 -> A.1;
id : Id;
x = id([0, Id])(id);


Term, M, N, K ::=
  | Set(l) | * | Π(a : A) -> B
  | x | (x : A) => K | M(N)
Sort, S ::=
  | Set(l) | * | □

R =
  | [*, *, *]
  | [□, *, Set(0)] // impredicativity
  | [Set(l), □, □] // indexing
  | [Set(l), *, *] // carrying data

// the last rule allows for smuggling, aka SELIT required
Lower{l}(A : Set(l)) : Set(0) =
  Π(C : *) -> (A -> C) -> C;

// but you can add sigma and remove the last rule
R =
  | [*, *, *]
  | [□, *, Set(0)] // impredicativity
  | [Set(l), □, □] // indexing

// this still allows impredicative indexing
Eq{l}(A : Set(l), x : A, y : A) : Set(0) =
  Π(C : A -> *) -> C(x) -> C(y);

// this one is equivalent to adding Sigma and removing the last rule
Sort, S ::=
  | Set(l) | Type(ω) | □(l)
R =
  | [*, *, *]
  | [□, *, Set(l)] // impredicativity
  | [Set(l), □, □] // indexing
  | [Set(l), *, *] // carrying data

Γ |-
--------------
Γ |- M(N)

// this is bad
Lower{l}(A : Set(l)) : Set(0) =
  Π(C : *) -> (A -> C) -> C;

Pair{l}(A : Set(l), B : Set(l)) : Set(0) =
  Π(C : *) -> (A -> B -> C) -> C;

Eq{l}(A : Set(l), x : A, y : A) =
  Π(C : A -> *) -> C(x) -> C(y);





Id = (Π(A : *) -> A -> A) : Type(0);

Π(A : *) -> (T -> A) -> A;

Id = (Π(A : Type(1)) -> A -> A) : Type(0);

Eq(A, x, y) : Type(0) = Π(C : A -> *) -> C(x) -> C(y);


Π(A : *) -> A -> A

Π
* -> *
Π(A : *) -> (A : *) -> (A : *)
* -> * : *

(Π(A : *) -> (T : *) : Type(0))

Π(A : T ->*) -> B(A) -> A

T : Type(r) = Π(A : Type(l)) -> (B : Type(r)) -> A;

T : Kind(1) = Id -> Id;


T : Type(1) = Id -> Id;

Set : Type

// this is ok
Γ |- Set : Type

Γ |- M : Π(A : Kind) -> T  Γ |- N : {Set, Type}
-----------------------------------------------
Γ |- M(N) : T{A := N}

// this is not ok
Set : Kind
Type : Kind
id : Π(A : Kind) -> A -> A;

id = id(Set);



Γ |-
--------------------
Γ |- (a : A) -> B :

id : (A : *) -> A -> A;

Term, M, N, K ::=
  | Set | * | Π(a : A) -> B
  | x | (x : A) => K | M(N)
Sort, S ::=
  | Set | Type | * | □

// add the usual CoC here, Set : Type
R =
  | [*, *, *]
  | [□, *, Set] // impredicativity 2
  | [Set, □, □] // indexing
  | [Set, *, *] // carrying data


//
----------
Γ |- * : □

---------------
Γ |- Set : Type

Γ |- A : □  Γ |- M : Π(a : A) -> B
Γ |- N : A{* := {Set, Type}}
---------------------------------- // large elimination
Γ |- M(N) : B{a := N}


//

Term, M, N, K ::=
  | Set | * | Π(a : A) -> B
  | x | (x : A) => K | M(N)
Sort, S ::=
  | Set | Type | * | □

// add the usual CoC here, Set : Type
R =
  | [*, *, *]
  | [□, *, Set] // impredicativity 2
  | [Set, □, □] // indexing
  | [Set, *, *] // carrying data

// but given this you get some interesting ones
Box(T : Set) = Π(A : *) -> (T -> A) -> A;
// which is the same as just T, meaning this is bigger

// which means I could allow for
Box(T : Set) : Set = Π(A : *) -> T;

// so, this?
U : Set = Π(A : *) -> (P(P(A)) -> A) -> A;

//
Term, M, N, K ::=
  | Set(l) | All(l) | Π(a : A) -> B
  | x | (x : A) => K | M(N)
Sort, S ::=
  | Set(l) | All(l) | Kind(l)

|- Set(l) : Set(1 + l)
|- All(l) : Kind(l)
// no type for Kind(l)

R =
  | [Set(l), Set(r), Set(max(l, r))] // set tower
  | [All(l), All(r), All(max(l, r))] // all tower
  | [Kind(l), All(r), Set(max(l, r))] // inductive type
  | [Set(l), Kind(r), Kind(max(l, r))] // predicative indexing
  | [Set(l), All(r), Set(max(l, r))] // carrying data

Bool0 : Set(0) = Π(A : All(0)) -> A -> A -> A;
Bool1 : Set(1) = Π(A : All(1)) -> A -> A -> A;
// large elimination by lifting
lift(b : Bool0) : Bool1 = ⇑b;
// other universes are preserved
Box : Set(1) = Π(A : All(0)) -> (Set(0) -> A) -> A;
b : Box; (⇑b : Π(A : All(1)) -> (Set(0) -> A) -> A)

// extending it a bit again
Term, M, N, K ::=
  | Set(l) | All(l) | Π(a : A) -> B
  | x | (x : A) => K | M(N)
Sort, S ::=
  | Set(l) | All(l) | Kind(l)

|- Set(l) : Set(1 + l)
|- All(l) : Kind(l)
// no type for Kind(l)

R =
  | [Set(l), Set(r), Set(max(l, r))] // set tower
  | [All(l), All(r), All(max(l, r))] // all tower
  | [Kind(l), All(r), Set(max(l, r))] // inductive type
  | [Set(l), Kind(r), Kind(max(l, r))] // predicative indexing
  | [Set(l), All(r), Set(max(l, r))] // carrying data

// if I kind do
T : Set(1) = Π(A : All(0)) -> (Set(0) -> A) -> A;
// that is equivalent to
T : Set(1) = Π(A : All(0)) -> Set(0);

// then this
T : Set(0) = Π(A : All(0)) ->
  ((Π(B : All(0)) -> B -> B) -> A) -> A;
// which is equivalent to
T : Set(0) = Π(A : All(0)) -> Π(B : All(0)) -> B -> B;
// those levels are coupled, so in Monnier's style
T : Set(0) = Π(l : Level) ->
  Π(A : All(l)) -> Π(B : All(l)) -> B -> B;
// but by using the instantiation approach they can be different
T : Set(0) = Π(l : Level) -> Π(A : All(l)) ->
  Π(r : Level) -> Π(B : All(r)) -> B -> B;
// and I know the instantiation approach is sane

// lifting doesn't cross into Set
T0 : Set(0) = Π(A : All(0)) ->
  ((Π(B : All(0)) -> B -> B) -> A) -> A;
// lifts to
T1 : Set(1) = Π(A : All(1)) ->
  ((Π(B : All(0)) -> B -> B) -> A) -> A

// erasability may actually save it
f : Π(l : Level $ 0) -> Set(1 + l);
f = λ(l) => Set(l); // this fails

// this lifts B
Π(A : All(0)) -> ((Π(B : All(0)) -> F(A, B)) -> A) -> A
// this doesn't lift B, A is not mentioned internally
Π(A : All(0)) -> ((Π(B : All(0)) -> F(B)) -> A) -> A

// hmm I think the following is true then:
T = Π(l : Level $ 0) -> F(l);
// is always equivalent to
T = Π(l : Level $ 0) -> Π(A : Set(l)) -> (F(l) -> A) -> A;
// which may be enough to reduce it to my system

∀(l) -> Π(A : Set(l)) -> Π(B : Set(l)) -> A -> B -> B
// this is more general
∀(l) -> Π(A : Set(l)) -> ∀(r) -> Π(B : Set(r)) -> A -> B -> B
// and it's equivalent to
∀(l) -> Π(A : Set(l)) -> A -> ∀(r) -> Π(B : Set(r)) -> B -> B
// which then can be described by All
Π(A : All(0)) -> A -> Π(B : All(0)) -> B -> B

// instantiation + cumulativity may be enough
f : Π(A : All(0)) -> Π(B : All(0)) -> A == B -> A -> B;
f(Set(0)) : Π(B : All(0)) -> Set(0) == B -> Set(0) -> B
// but that requires (B : All(0)) :> All(1)
// and (Set(0) : Set(1)) :> All(1)

f : ∀(l) -> Π(A : Set(l)) ->
  ∀(r) -> Π(B : Set(r)) -> Lift(r, A) == Lift(l, B) ->
    Lift(l, A) -> Lift(r, B);

//
f : Π(A : All(0)) -> Π(B : All(0)) -> A == B -> A -> B;
//
f : Π(A : All(1)) -> Π(B : All(1)) -> A == B -> A -> B;
// same as which is allowed anyway
f : Π(A : All(1)) ->
  (((Π(B : All(1)) -> A == B -> A -> B) : Set(1)) -> A) -> A;

f(Set(1)) : Π(B : All(2)) -> Set(1) == B -> Set(1) -> B

f(Set(1))(Set(0)) : Π(B : All(1)) -> Set(1) == B -> Set(1) -> B

(M : Set(l)) :> All(l)


//
T = ∀(l) -> F(l)

//
T = ∀(l) -> SP(l);

SP(l) =| Π(a : N(l)) -> SP(l)

P(l) =| A : Set(r + l)
P(l) =| Set(r + l)
P(l) =| Π(a : N(l)) -> P(l)

N(l) =| Set(r + l)

T = ∀(l) -> Π(a : N(l)) -> P(l);




Γ |- T : Type(r + l)
-----------------------------------
Γ |- (l : Level $ 0) -> T : Type(r)


∀(l) -> Π(A : Set(l)) -> Π(B : Set(l)) -> A -> B -> B

∀(l) -> T(l)
// same as
∀(l) -> Π(A : Set(l)) -> (T(A, l) -> A) -> A
// same as
Π(A : All(0)) -> (T(A, 0) -> A) -> A

∀(l) -> Π(A : Set(l)) -> Set(l)

∀(l) -> C(l)
M(l) : Set(0) -> Set(0)


Π(A : All(0)) -> A -> A
M(Set(0)) : Set(0) -> Set(0)

∀(l) -> Π(A : Set(l)) -> Set(l)

//
∀(l) -> P(l) == Π(A : All(0)) -> |P|(A)
∀(l) -> Set(r + l) == Π(A : All(0)) -> A
∀(l) -> N(l) -> P(l) == Π(A : All(0)) -> A

∀(l) -> Set(r + l) $ 0 -> Set(r + l) $ 0 ==
  Π(A : All(ω)) -> A -> A

Π(A : All(0)) ->

Π(A : All(0)) -> Set(l)

f : ∀(l) -> Set(r + l) -> Set(r + l);
f = ∀(l) => (A : Set(r + l)) => A -> A;

∀(l) -> Set(r + l) -> Set(r + l) == Π(A : All(0)) -> A


// the main issue is around type formation
f : ∀(l) -> Set(r + l) -> Set(r + l);
f = (l) => (A) => A -> A; // this is allowed

// restrict it further, level only in erasable
f : ∀(l) -> Set(r + l) $ 0 -> Set(r + l) $ 0;
// same as
f : Π(A : All(0)) -> Lift(r, A) -> Lift(r, A);

//
T : Set(1) = ∀(l) -> Set(l) -> Set(l);
f : T = (l) => (A) => A -> A;
f(l) : Set(l) -> Set(l) = (A) => A -> A;

// this is general enough
T : Set(r) = ∀(A : All(0)) ->
  (A -> A -> A) -> A -> A
f : T = (l) => (A) => (arr) => arr(A);
f(Set(l), (A, B) => A -> B) : Set(l) -> Set(l) = (A) => A -> A;



T : Set(1) = ∀(l) -> Set(l) -> [T : Set(l), x : T];
f : T = (l) => (A) => [A -> A, (x) => x];

// this is general enough
T : Set(1) = ∀(U : All(0)) -> ∀(El : U -> All(0)) ->
  Π(arr : U -> U -> U) ->
  Π(fun : Π(A : U, B : U) -> (El(A) -> El(B)) -> El(arr(A, B))) ->
  U -> [T : U, x : El(T)];

f : T = (l) => (U) => (El) => (arr) => (fun) => [
  arr(U, U),
  fun(U, U)((x) => x)
];

f(Set(l)) : (Set(l) -> Set(0)) -> Set(l) -> Set(l)

f(Set(l), (A, B) => A -> B) : Set(l) -> Set(l) = (A) => A -> A;


f(l) : Set(l) -> Set(l) = (A) => A -> A;

T -> (K : Set(2 + r))
(x : T) =>

f : ∀(l) -> Set(r + l) -> Set(r + l);
f = (l) => (A) => A -> A; // this is allowed




id : ∀(l) -> ∀(A : Set(l)) -> A -> A;
id(l)(M : Set(l)) : ∀(A : Set(l)) -> A -> A;

id : ∀(u : Unit) -> ∀(A : All(0)) -> A -> A;
id(unit)(M : Set(l)) : ∀(A : All(l)) -> A -> A;



//

Box(A : Set(1)) : Set(1)
Box(A) = (X : All(0)) -> (A -> X) -> X;

P(A : Set(1)) : Set(1) = A -> Set(0);

U : Set(1) = (X : All(0)) -> (P(P(X)) -> X) -> P(P(X));
tau : P(P(U)) -> U
tau(t) = X => f => (p) => t((u) => p(f(u(X)(f))))

sigma : U -> P(P(U))
sigma(s) = ⇑s(U)(tau);

//
U : Type = (X : Type) -> (P(P(X)) -> X) -> X;

mapPP{A, B}(f : A -> B, p : P(P(A))) : P(P(B)) =
  (q : P(B)) => p((a) => q(f(a)));

tau : P(P(U)) -> U;
tau(t) = (X) => (f) => f((p) => mapPP((u) => u(X)(f), p));

sigma(s : U) : P(P(U)) =
  s(P(P(U)))((p) => mapPP(tau, p));

//
U : Type(1) = (X : Type(1)) -> (P(X) -> X) -> X;

mapPP{A, B}(f : A -> B, p : P(P(A))) : P(P(B)) =
  (q : P(B)) => p((a) => q(f(a)));

tau : P(P(U)) -> U;
tau(t) = (X) => (f) =>
  f((x) => )
  f((x) => )
  f((p) => mapPP((u) => u(X)(f), p));

sigma(s : U) : P(P(U)) =
  s(P(P(U)))((p) => mapPP(tau, p));
U : Type = (X : Type) -> (P(P(X)) -> X) -> P(P(X));

U : Set(1) = (X : All(0)) -> (P(P(X)) -> X) ->
  (x : (r : P(P(X))) -> X) -> X;

U : Set(1) = (X : All(0)) ->
  (((X -> Set(0)) -> Set(0)) -> X) -> X;



tau : P(P(U)) -> U
tau(t) = X => (f) => (x) =>
  x((p) => t(u) => p(u(X)(f)(f)));

sigma : U -> P(P(U))
sigma(s) =
  s(U) : (P(P(U)) -> U) ->
  (x : (r : P(P(U))) -> U) -> U;


U : Set(1) = (X : All(0)) ->
  (x : (f : (P(P(X)) -> X) -> P(P(X))) -> X) -> X;

f{A}(b : Box(A)) = b(A)


P(A : Type(1)) : Type(1) = A -> Type(0);

U : Type(1)
U = (X : All(0)) -> (P(P(X)) -> X) -> P(P(X));

tau : P(P(U)) -> U
tau(t) = X => x =>
  x((f) => (p) => t((u) =>
    ppx = u(P(P(X))) :
      (x : ((P(P(P(P(X)))) -> P(P(X))) -> P(P(P(P(X))))) -> P(P(X))) -> P(P(X))
    ;
    p(f(_ : P(P(X))))))

tau : P(P(U)) -> U
tau(t) = X => f => p -> t(x => p(f(x(X)(f))))

sigma : U -> P(P(U))
sigma(s) = s(U)(tau)


//

CPS(A : Set(l)) : Set(l) =
  Π(X : All(0)) -> (A -> X) -> X;

// especially with self types
CPS(A : Set(l)) : Set(l);
ret{A}(a : A) : CPS(A);

CPS(A) = ι(s) -> Π(X : CPS(A) -> All(0)) ->
  (Π(a : A) -> X(ret(a))) -> X(s);
ret{A}(a) = (X) => (k) => k(a);




I_Stream(n, A : Type) : Type;
I_Stream(n, A : Type) = [
  hd : A,
  tl : (m, p : n == 1 + m) -> I_Stream(m, A)
];

i_ouroboros{A}(n, hd : A $ n) : I_Stream(n, A);
i_ouroboros{A}(n, hd) = [
  hd,
  (m, p : n == 1 + m) =>
    n.is(
    | 0 => z_neq_s(p)
    | 1 + r => ouroboros{A}(m, coe(s_inj(p), (G) => A $ G, hd))
    );
];

Stream(A : Type) = (n : Nat) -> I_Stream(n, A);
ouroboros{A}(hd : (G : Grade) -> A $ G) : Stream(A) =
  (n) => i_ouroboros{A}(n, hd(n));

//


Stream(A : Type) : Type;
Stream(A : Type) = [hd : A, tl : Stream(A)];

// hd will be implicitly instantiated
ouroboros{A}(hd : (G : Grade) -> A $ G) : Stream(A);
ouroboros{A}(hd) = [hd, ouroboros{A}(hd)];

// same as
ouroboros{A}(hd : (G : Grade) -> A $ G) =
  tl : Stream(A)
  tl = [hd, tl];
  tl


//
Stream(A : Type) : Type;
Stream(A : Type) = [hd : A, tl : Stream(A)];

// hd will be implicitly instantiated
ouroboros{A}(hd : A $ ω) : Stream(A);
ouroboros{A}(hd) = [hd, ouroboros{A}(hd)];


f(a : A $ ∞) : B;
// same as
f(a(G : Grade) : A $ G) : B;

f(a : A $ ∞) : B;
// ok, you can always pass around A $ ∞
g(a : A $ ∞) : B = f(a);
// bad, you cannot split A $ ∞, A $ (1 + ∞)
h(a : A $ ∞) : B = [a $ 1, f(a)];

// very useful for cycles
Stream(A : Type) : Type;
Stream(A : Type) = [hd : A, tl : Stream(A)];

// now this makes a lot more sense
ouroboros{A}(hd : A $ ∞) =
  tl : Stream(A);
  tl = [hd, tl];
  tl;


// I cannot get A without instantiating H

Nat = [
  c(C, z : C, (s $ ω)(acc : C) : C) : C;
];

Nat = [
  G : Grade,
  c(C, z : C, (s $ G)(acc : C) : C) : C;
];

f(x : A $ ω) +
g(x : A $ ω)

T =
  | [true, x : A $ ω]
  | [false, x : A $ 0];
// same as
T = [b : Bool, x : A $ b ? ω : 0];

in_(a(H) : A $ H) : A $ ω = a(ω);
out(a : A $ ω)(H) : A $ H = (a $ H);

out(a(ω))

out(in_(a))

a(G, H) : A $ (G + H);
x : A $ ω + ω = a(ω, ω);
y(G) : A $ G = x;
z(H) : A $ H = x;

// trying to split further will fail
z : A $ 5 = x;

ω + ω


T(G) = [a(H) : A $ (G * H)];
U(G) = [l : A $ G, n(H) : [r : A $ H, p : l == r]];

f(a : T) : U =


a(G, H) : A $ (G * H);
x : A $ 5 = a(5, ?);

// with this I can, but note the explicit equality
a(G) : [l : A $ G, n(H) : [r : A $ H, p : l == r]];
x : A $ 5 = a(5).l;
y(H) : A $ H : a(5).n(H).r;

// proposal, ω acts like the latter
a : A $ ω + ω;
x(G) : A $ G = a;
y(H) : A $ H = a;
z : A $ 5 = a; // this will fail




a(G, H) : A $ (G * H)

x : A $ G;
y : A $ H


f(a : A $ ∞) : B;

// Russel's
// the set of all sets that doesn't include themselves

// U : Set = ∀(X : Set), X ∉ X
// does U includes itself?

Prop = Set;
U : Set = [X : Set, P(S : X) : Prop];
El(S : U) : Set = [x : S.X, p : S.P(x)];

(X : U) ∈ X == [x : El(X), p : [X, x] =~= [U, X]];

(X : U) ∈ X == [x : El(X), p : [X : U, x] =~= [U, X]];

R : U = [U, P(S : U) = ~(S ∈ S)];

l : ~(R ∈ R) = ([x : El(R), p : [R, x] =~= [U, R]]) : False =>
  p.0 : R == U;
  p.1 : p.0.coe(El, x) == R;
  p.0.coe(El, x) : El(U)

  x.x : U == R
  f(X : U, x : El(X)) ->

  q : x.x ∈ x.x = [
    [
      _ : x.x.X,
      _
    ]
    _ : El(x.x),
    _,
  ];
  x.p(q)


(X ∈ X) : Prop = [x : X, p : [X, x] =~= [Set, X]];
(X ∈ X) : Prop = [x : X, p : [X, x] =~= [Set, X]];

R : Set = [X : Set, p : ~(X ∈ X)];

l : ~(R ∈ R) = ([x : R, p : [R, x] =~= [Set, R]]) =>
  p.0 : R == Set
  p.1 : p.0.cast(x) == R
  x.p(
    [
      _ : X,
      _
    ]
    _ : x.X ∈ x.X) : ~(x.X ∈ x.X);
l : R = [R, _ : ~(R ∈ R)];

V : Set = [A : Set, P : (a : A) -> Set];

R : Set = (X : Set) -> ~(X ∈ X);

l : ~(R ∈ R) = ([x, p] : R ∈ R) : False => x(R)([x, p]);
l : ~(R ∈ R) = ([x, p] : R ∈ R) : False => x(R)([x, p]);
r : R = (X) => ([x, p] : X ∈ X) : False =>


r : R ∈ R = [_]

? : [u : U, p : [U, u] =~= [Set, U]]

(omega : U)(X)(x, p) =


u : U;
u(U)()

  X ∉ X
//

T = {X}(x(a : A) : X) -> X;


//

// extending it a bit again
Term, M, N, K ::=
  | Set(l) | All(l) | Π(a : A) -> B
  | x | (x : A) => K | M(N)
Sort, S ::=
  | Set(l) | All(l) | Kind(l)

|- Set(l) : Set(1 + l)
|- All(l) : Kind(l)
// no type for Kind(l)

R =
  | [Set(l), Set(r), Set(max(l, r))] // set tower
  | [All(l), All(r), All(max(l, r))] // all tower
  | [Kind(l), All(r), Set(max(l, r))] // inductive type
  | [Set(l), Kind(r), Kind(max(l, r))] // predicative indexing
  | [Set(l), All(r), Set(max(l, r))] // carrying data

CPS(A : Set(l)) : Set(l);
ret{A}(a : A) : CPS(A);

// still, no All(l), Set(r)
// but you can still describe dependency
CPS(A) = self(s) -> ∀(X : CPS(A) -> All(0)) ->
  Π(x : Π(a : A) -> X(ret(a))) -> X(s);
ret{A}(a) = (X) => (k) => k(a);


//
Prop : Type(0);
Type(l) : Type(l + 1);

//
Set : Set;

U(l : Level) : Set;
El_U{l}(T : U(l)) : Set;
prop : U(0);
univ(l) : U(1 + l);
pi{l}(A : U(l), B(a : El_U(A)) : U(l)) : U(l);

Prop : Set;
El_P(T : Prop) : Set;
pi_u{l}(A : U(l), B(a : El_U(A)) : Prop) : Prop;
pi_p(A : Prop, B(a : El_P(A)) : Prop) : Prop;

U(l) = self(T) -> (
  C{l}(T : U(l)) : Set,
  p
  u(l) : C(univ(l)),
  f{l}(A, B) : C(pi{l}(A, B)),
) -> C(T);
El_U{l}(T) =
  T.@(
    C{r}(T) = Π(p : l == r) -> Set,
    p = Prop,
    u(r) = λ(p : l == 1 + r) => U(r),
    // is El_U(B(a)) going down
    f{l}(A, B) = λ(p) => Π(a : El_U(A)) -> El_U(B(a)),
  )(refl());
prop(C, p, u, f) = p;
univ(l)(C, p, u, f) = u(l);
pi{l}(A, B)(C, p, u, f) = a(A, B);

Prop = self(T) -> (
  C(T : Prop) : Set,
  u{l}(A, B) : C(pi_u(A, B)),
  p(A, B) : C(pi_p(A, B)),
) -> C(T);
El_P(T) =
  T.@(
    C(T) = Set,
    // is El_P(B(a)) going down
    u{l}(A, B) = Π(a : El_U(A)) -> El_P(B(a)),
    // is El_P(B(a)) going down
    p(A, B) = Π(a : El_P(A)) -> El_P(B(a)),
  )(refl());

//

U(l : Nat, s : Nat) : Set;
El{l, s}(T : U(l, s)) : Set;

U(l, s) = (
  C(l : Nat, s : Nat) : Set,
  u(l : Nat) : C(1 + l, 0),
  f(
    s : Nat,
    A : U(l, s) $ 0,
    AH : C(l, s),
    B(a : El(A)) : C
  ) : C(l, 1 + s),
) -> C(l, s);
El{l}(T) =
  T(
    Set,
    u(r, p) = U(r),
    f(A, AH, B) = Π(a : El(A)) -> B(a),
  );



El{l}(T : U(l)) : Set;
univ(l) : U(1 + l);
pi{l}(A : U(l), B(a : El(A)) : U(l)) : U(l);

U(l) = self(T) -> (
  C{l}(T : U(l)) : Set,
  u(l) : C(univ(l)),
  f{l}(A, B) : C(pi{l}(A, B)),
) -> C(T);
El{l}(T) =
  T.@(
    C{r}(T) = Set,
    u(r) = (r),
    // is El(B(a)) going down
    f{l}(A, B) = Π(a : El(A)) -> El(B(a)),
  );
univ(l)(C, p, u, f) = u(l);
pi{l}(A, B)(C, p, u, f) = a(A, B);


//

f : Π(a : A) -> B(a);

f : ∀a -> A(a) -> B(a);


U(s : Nat) : Set;
El{s}(T : U(s)) : Set;
nat : U(0);
pi(
  sa : Nat,
  A : U(sa),
  sb(a : El(A)) : Nat,
  B(a : El(A)) : U(sb(a))
) : U(1 + max(sa, sb));

U(s) = self(T) -> (
  C{s}(T : U(s)) : Set,
  n : C(nat),
  f{sa, sb}(A, B) : C(pi{sa, sb}(A, B)),
) -> C(T);
El{l}(T) =
  T.@(
    C{s}(T) = Set,
    n = Nat,
    // is El(B(a)) going down
    f{l}(A, B) = Π(a : El(A)) -> El(B(a)),
  );
univ(l)(C, p, u, f) = u(l);
pi{l}(A, B)(C, p, u, f) = a(A, B);


//
Prop : Type(0)
Type(l) : Type(1 + l)

data Nat =
  | zero : Nat
  | succ(n : Nat) : Nat;

f(b : Bool : Type(0)) : Type(1) =
  b ? Type(0) : Type(0);

// impredicative Set
Set : Type

Prop : Type = (
  f(A : Set, B(a : A) : Set) : Set,
) -> Set;
El_P(T) =
  T(
    f(A, B) = Π(a : A) -> B(a),
  );

U_H(l : Nat) : Type = Π(
  u(r : Nat, p : 1 + r == l) : Set,
  f(A : Set, B(a : A) : Set) : Set,
) -> Set;
univ_h(l)(u, f) : U_H(1 + l) = u(l, 1 + l);
pi_h{l}(
  A : U_H(l),
  B(a : El_U_H(A)) : U_H(l)
) : U_H(l) =
  (u, f) => f(El_U_H(A), (a) => B(a)(u, f));
U_L(l, H : U_H(l)) : Set =
  Π(
    C,
    u(r, p : 1 + r == l) = C(univ_h(r)),
    f(A, B) = C(pi_h(A, B)),
  ) -> C(H);

U : Set = [H : U_H];

El_U{l}(T : U_H) =
  T(
    u(r, p) = U_L(r),
    f(A, B) = Π(a : A) -> B(a),
  );

f(a : A : U(l)) : Prop


all{l}(A : U(l), B(a : El(A)) : U(0)) : U(0) =
  (u, f) => f(El(A), );

P : Type = (
  u(A : U(l), B(a : El(A)) : P) -> P
)

univ(l) : U(1 + l) = (u, f) => u(l);
El{l}(T) =
  T(
    Set,
    u(r, p) = U(r),
    f(A, B) = Π(a : A) -> B(a),
  );

//
U(l : Nat) : Set;
El{l}(T : U(l)) : Set;

U(l : Nat) = Π(
  C : *,
  u(r : Nat, p : 1 + r == l) : C,
  f(
    A : U(l),
    B(a : El(A)) : U(l) $ 0,
    BH(a : El(A)) : C,
  ) : C
) -> C;
El{l}(T) =
  T(
    C = Set,
    u(r, p) = U(r),
    f(A, B, BH) = Π(a : El(A)) -> BH(a),
  );

univ(l) : U(1 + l) = (C, u, f) => u(l);
pi{l}(A : U(l), B(a : El(A)) : U(l)) : U(l) =
  λ(C, u, f) => f(A, B, BH(a) = B(a)(C, u, f));

//
U(l : Nat, s : Nat) : Set;
El{l, s}(T : U(l, s)) : Set;

U(l, s) = Π(
  C : *,
  u(r == l - 1, 0 == s) : C,
  f(
    sa == s - 1,
    A : U(l, sa),
    B(a : El(A)) : C,
  ) : C
) -> C;
El{l, s}(T) =
  T(
    C = Set,
    u(r, w) = U(r),
    f(A, B) = Π(a : El(A)) -> B(a),
  );

univ(l) : U(1 + l) = (C, u, f) => u(l);
pi{l}(A : U(l), B(a : El(A)) : U(l)) : U(l) =
  λ(C, u, f) => f(A, B(a) = B(a)(C, u, f));

//
U1(s : Nat) : Set;
El_1{s}(T : U1(s)) : Set;

U0(s : Nat) : Set;
El_0{s}(T : U0(s)) : Set;

U1(s) = Π(
  C : *,
  u0(r == s - 1) : C,
  f1(A : C, B : C) : C,
) -> C;
El_1(T) =
  T(
    C = Set,
    p(r) = U0(r),
    f1(A, B) = Π(a : A) -> B,
  );

U0(s) = Π(
  C : *,
  f0(sa == s - 1, A : U0(sa), B(a : El_0(A)) : C) : C,
  f1(A : U1(?), B(a : El_1(A)) : C) : C
) -> C,
El_0{s}(T) =
  T(
    C = Set,
    f0(sa, A, B) = Π(a : El_0(A)) -> B(a),
    f1(A, B) = Π(a : El_1(A)) -> B(a),
  );


//
U0_S(s : Nat) : Set(1);
U0_S(s) = Π(
  C : Set(0),
  all(T(a(r < s) : U0_S(r)) : C) : C,
  arr(A : C, B : C) : C,
) -> C;

El_S{s}(T : U0_S(s)) : Set(1);
El_S{s : Nat}(T : : U0_S(s)) =
  // we can lift as this is a simply typed term + positive recursion
  // so we can do the same as large elimination
  ⇑T(
    C = Set(1),
    all(T) = Π(a(r < s) : U0_S(r)) -> T(a),
    arr(A, B) = Π(a : A) -> B,
  );

all(T(a(r) : U0(r))) : U0(∞) =
  λ(C, all, arr) => all(T);
arr(A : U(∞), B : U(∞)) : U0(∞) =
  λ(C, all, arr) => arr(A(C, all, arr), B(C, all, arr));

U0 = U0_S(∞)
El(T : U0) : Set(1) = El_S(T);

El_0{s}(T : U0(s)) : Set;
El_0{s}(T) =
  T(
    C = Set,
    f0(sa, A, B) = Π(a : El_0(A)) -> B(a),
    f1(A, B) = Π(a : El_1(A)) -> B(a),
  );

//
U0(T : Set) : Set =
  | all(B(A : Set) : Set, BH(A : Set) : U0(B(A))) : U0(Π(A : Set) -> B(A))
  | arr(A : Set, B : Set, AH : U0(A), BH : U0(B)) : U0(Π(a : A) -> B);

F(T : Set) : Set;
F(T) = Π(
  C(T : Set) : *,
  all(
    B(A : Set) : Set,
    BH(A : Set, AH : C(A)) : C(B(A)),
  ) : C(Π(A : Set) -> B(A)),
  arr(
    A : Set,
    B : Set,
    AH : C(A),
    BH : C(B),
  ) : C(A -> B),
) -> C(T);

Id : Set = Π(A : Set) -> A -> A;
Id_U : U0(Id) = λ(s) => λ(C, all, arr) =>
  all(
    B(A) = A -> A,
    BH(A, AH : C(A)) = arr(A, A, AH, AH)
  );

U0 : Set;
U0 =
  | all(T(A : Set) : U0) : U0,
  | arr(A : U0, B : U0) : U0;

Id : Set = (A : Set, a : A) -> A;


El(T : U0) : Set =
  T.is(
  // argument for termination, is that:
  // A only occurs in an erasable position of T
  | all(T) => Π(A : Set) -> El(T(A)),
  | arr(A, B) => Π(a : El(A)) -> El(B)
  );
El(T : U0) : Set =
  U0(
    C = Set,
    all(T) = Π(A : Set) -> T(A),
    arr(A, B) = Π(a : A) -> B(a)
  );

//
U0_S(s : Nat) : Set;
U0_S(s) = Π(
  C : *,
  all(T(a(r < s) : U0_S(r)) : C) : C,
  arr(A : C, B : C) : C,
) -> C;
lift{s, r}(A : U0_S(s)) : U0_S(r);
lift{s, r}(A) =
  A(
    U0_S(r),
    λ(T(a(p < s) : U0_S(p))) => λ(C, all, arr) =>
      all(λ(a(q < r) : U0_S(q)) =>

        (r < s).is(
        | true => T(a)
        )
        _ : U0_S(r)),
    _
  );

U0 = Π(s : Size) -> U0_S(s);

all(T(a : U0) : U0) : U0 = λ(s) => λ(C, all, arr) =>
  all(λ(a(r < s) : U0_S(r)) =>
    T(_ : Π(r) -> U0_S(r)) :);
U0 : Set = Π(
  C : *,
  all(T(a : U0_S(∞)) : C) : C,
  arr(A : C, B : C) : C,
) -> C;

f(T : U0_S(∞)) : U0 =
  λ(C, all, arr) => T(
    C = U0,
    λ(T) => all(λ(a : U0_S(∞)) => T(λ(r) => a)),
    λ(A, B) => arr(A, B)
  );
case(T : U0) :

El(T : U0)(s) : Set =
  T(s)(
    C = Set,
    all(T) = Π(a(r < s) : U0_S(r)) -> T(a),
    arr(A, B) = Π(a : A) -> B
  );

((s : Nat) -> Set) == Set

El_S{s : Nat}(T : : U0_S(s)) =
  // we can lift as this is a simply typed term + positive recursion
  // so we can do the same as large elimination
  ⇑T(
    C = Set(1),
    all(T) = Π(a(r < s) : U0_S(r)) -> T(a),
    arr(A, B) = Π(a : A) -> B,
  );

all(T(a(r) : U0(r))) : U0(∞) =
  λ(C, all, arr) => all(T);
arr(A : U(∞), B : U(∞)) : U0(∞) =
  λ(C, all, arr) => arr(A(C, all, arr), B(C, all, arr));

U0 = U0_S(∞)
El(T : U0) : Set(1) = El_S(T);

El_0{s}(T : U0(s)) : Set;
El_0{s}(T) =
  T(
    C = Set,
    f0(sa, A, B) = Π(a : El_0(A)) -> B(a),
    f1(A, B) = Π(a : El_1(A)) -> B(a),
  );
//
F(T : Set) : Set;
F(T) = Π(
  C(T : Set) : *,
  all(
    B(A : Set) : Set,
    BH(A : Set, AH : C(A)) : C(B(A)),
  ) : C(Π(A : Set) -> B(A)),
  arr(
    A : Set,
    B : Set,
    AH : C(A),
    BH : C(B),
  ) : C(A -> B),
) -> C(T);

//
F(T : Set) : Set;
F(T) = Π(
  C(T : Set) : *,
  all(
    B(A : Set) : Set,
    BH(A : Set, AH : C(A)) : C(B(A)),
  ) : C(Π(A : Set) -> B(A)),
  pi(
    A : Set,
    B(a : A) : Set,
    AH : C(A),
    BH(a : A) : C(B(a)),
  ) : C(Π(a : A) -> B(a)),
) -> C(T);

pi(univ(0), B(A : Set, AH : F(A)) = _);

arr(A : Set, B : Set, U(l))
  : U(l, Π(a : A, AH : U(l, a)) -> B)

univ(0) : Π(T : Set) -> U(0, T)

id = λ(A : U) =>
all(T(A : U) : U) : U =
  λ(C, all, arr) =>
    all(λ(A : C) => T());
El(T : U) : Set = T(
  C = Set,
  all(T(A : Set) : Set) = Π(A : Set) -> T(A),
  arr(A : Set, B : Set) = Π(a : A) -> B
);

U(l : Nat, T : Set) : Set;
El{l}(T : Set, TU : U(l)) : Set;

U(l, T) = Π(
  C(T : Set) : *,
  f : C,
  u(r : Nat, p : 1 + r == l) : C,
  f(A : U(l), B(a : El(A)) : C) : C,
) -> C;
El{l}(T) =
  T(
    C = Set,
    f = ∃[A : Set, AF : F(A)],
    u(r, p) = U(r),
    f(A, B) = Π(a : El(A)) -> B(a),
  );

Prop = ∃[A : Set, AH : F(A)];
true : Prop -> Prop -> Prop = (t, f) => t;



true(Unit, False) == Unit



Π(A : Set, AH : F(A)) -> ∃[A : Set, AH : F(A)]


U(l : Nat) : Set;
U(l) = Π(
  C(T : Set) : *,
  u(r : Nat, p : 1 + r == l) : C([T : Set, TH : U(r, T)]),
  pi(
    A : Set,
    B(a : A) : Set,
    AH : C(A),
    BH(a : A) : C(B(a)),
  ) : C(Π(a : A) -> B(a))
) -> C(T);

U(l : Nat, T : Set) : Set;
El{l}(T : Set, TH : U(l, T)) : Set;

El{l}(T) =
  T(
    C = Set,
    u(r, p) = U(r),
    f(A, B, BH) = Π(a : El(A)) -> BH(a),
  );


U(l : Nat, T : Set) : Set;
El{l}(T : Set, TH : U(l, T)) : Set;

U(l : Nat, T) = Π(
  C(T : Set) : *,
  u(r : Nat, p : 1 + r == l) : C,
  f(
    A : U(l),
    B(a : El(A)) : U(l) $ 0,
    BH(a : El(A)) : C,
  ) : C
) -> C;
El{l}(T) =
  T(
    C = Set,
    u(r, p) = U(r),
    f(A, B, BH) = Π(a : El(A)) -> BH(a),
  );

univ(l) : U(1 + l) = (C, u, f) => u(l);
pi{l}(A : U(l), B(a : El(A)) : U(l)) : U(l) =
  λ(C, u, f) => f(A, B, BH(a) = B(a)(C, u, f));


//
U1 : Set = Π(
  C : *,
  u0 : C,
  arr(A : C, B : C) : C
) -> C;
U0 : Set = Π(
  C : *,
  all(
    A : U1,
    // TODO: this is weird / bad
    B(A : U1(*, C, a(A, B) = A -> B)) : C
  ),
  arr(A : C, B : C) : C
) -> C;

El(T : U) : Set = T(
  C = Set,
  all(T) = Π(A : Set, AF : F(A)) -> T(A),
  arr(A, B) = Π(a : A) -> B
);

id = λ(A : Set) =>

Id : U = λ(C, all, arr) =>
  all(λ(A) => arr(A, A));

//
U(l : Nat) : Set;
El{l}(T : U(l)) : Set;

U(l) = self(T) -> (
  C(l : Nat, T : Set) : *,
  univ(r == l - 1) : C(1 + r, U(r)),
  pi(
    A : Set,
    AH : C(l, A),
    B(a : A) : Set,
    BH(a : A) : C(l, B(a)),
  ) : C(l, Π(a : A) -> B(a)),
  all{l}(
    A : Set,
    AH : C(l, A),
    B(a : A) : Set,
    BH(a : A) : C(0, B(a))
  ) : C(0, Π(a : A) -> B(a)),
) -> C(l, El(T));
El{l}(T) =
  T(
    C(r, T) = Set,
    univ(r) = U(r),
    pi(A, AH, B, BH) = Π(a : A) -> B(a),
    all{l}(A, AH, B, BH) = Π(a : A) -> B(a),
  );

univ(r : Nat) : U(1 + r) = λ(C, univ, pi, all) => univ(r);
pi{l}(A : U(l), B(a : El(A)) : U(l)) : U(l) = λ(C, univ, pi, all) =>
  pi(
    El(A), A(C, univ, pi, all),
    B(a) = El(B(a)),
    BH(a) = B(a)(C, univ, pi, all)
  );
all{l}(A : U(l), B(a : El(A)) : U(0)) : U(0) = λ(C, univ, pi, all) =>
  all(
    El(A), A(C, univ, pi, all),
    B(a) = El(B(a)),
    BH(a) = B(a)(C, univ, pi, all)
  );

arr{l}(A : U(l), B : U(l)) : U(l) = pi(A, λ(a) => B);

Id : U(0) = all(univ(0), B(A : U(0)) = arr(A, A))

//
U(l : Nat) : Set;
El_U{l}(T : U(l)) : Set;
prop : U(0);
univ(l) : U(1 + l);
pi{l}(A : U(l), B(a : El_U(A)) : U(l)) : U(l);

Prop : Set;
El_P(T : Prop) : Set;

U(l) = self(T) -> (
  C(l : Nat, A : Set, T : U(l)) : *,
  prop : C(0, Prop, prop),
  univ(r == l - 1) : C(1 + r, U(r), univ(r)),
  pi(
    A : Set $ 0,
    AU : U(l) $ 0,
    AH : C(l, A, AU),
    B(a : A) : Set $ 0,
    BU(a : A) : U(l) $ 0,
    BH(a : A) : C(l, B(a), BU(a)),
  ) : C(l, Π(a : A) -> B(a), pi(AU, BU)),
) -> C(l, El_U(T), T);
El_U{l}(T : U(l)) : Set =
  T(
    C(l, A, T) =
      T.@(
        C(l, A, T) = Set,
        Unit,
        univ(r) = Unit,
        pi(A, AU, AH, _, _, _) = AH == A,
      ) -> Set,
    prop = λ(unit) => Prop,
    univ(r) = λ(unit) => U(r),
    pi(A, AU, AH, B, BU, BH(a : A) : Set) =
      λ(p : AH == A) => Π(a : AH) -> B(coe(p, T, a)),
  )(
    T.@(
      C(l, A, T) =
        T.@(
          C(l, A, T) = Set,
          Unit,
          univ(r) = Unit,
          pi(A, AU, AH, _, _, _) = AH == A,
        ),
      unit,
      univ(r) = unit,
      pi(A, AU, AH :
        pi(AU, BU).@(
          C(l, A, T) = Set,
          Unit,
          univ(r) = Unit,
          pi(A, AU, AH, _, _, _) = AH == A,
        ), _, _) = _ : AH == A,
    )
    _ :
    T.@(
        C(l, A, T) = Set,
        Unit,
        univ(r) = Unit,
        pi(A, AH, _, _) = AH == A,
      )
  );

Prop = self(T) -> Π(
  C(T : Set) : *,
  all(
    l : Nat,
    A : U(l),
    B(a : El_U(A)) : Set,
    BH(a : El_U(A)) : C(B(a))
  ) : C(Π(a : El_U(A)) -> B(a)),
  pi(
    A : Set,
    AH : C(A),
    B(a : A) : Set,
    BH(a : A) : C(B(a))
  ) : C(Π(a : A) -> B(a)),
) -> C(El_P(T))
El_P(T : Prop) : Set =
  T(
    C(T) = Set,
    all(l, A, B, BH) = Π(a : El_U(A)) -> B(a),
    pi(A, AH, B, BH) = Π(a : A) -> B(a),
  );
//

Id : U(0) = λ(C, El, univ, pi, all) =>
  all(
    A = univ(0),
    B(a : El(univ(0))) = pi(),
  );

pi{l}(A : U(l), B(a : El(A)) : U(l)) : U(l) =
  λ(C, El, univ, pi, all) =>
    pi(
      A(C, El, univ, pi, all),
      B(a) = B(a),
    );

//
U(l : Nat, T : Set) : Set;
U(l, T) = (
  C(l : Nat, T : Set) : Set,
  univ(r == l - 1) : C(1 + r, U(r, _)),
  pi(
    A : Set,
    B(a : A) : Set,
    AH : C(l, A),
    BH(a : A) : C(l, B(a)),
  ) : C(l, Π(a : A) -> B(a)),
  all{l}(
    A : Set,
    B(a : A) : Set,
    AH : C(l, A),
    BH(a : A) : C(0, B(a))
  ) : C(0, Π(a : A) -> B(a)),
) -> C(l, T);
El{l}(T : U(l)) : Set =
  T(
    C(r, T) = Set,
    univ(r) = U(r),
    pi(A, AH, B, BH) = Π(a : A) -> B(a),
    all{l}(A, AH, B, BH) = Π(a : A) -> B(a),
  );



univ(r : Nat) : U(1 + r) = λ(C, univ, pi, all) => univ(r);
pi{l}(A : U(l), B(a : El(A)) : U(l)) : U(l) = λ(C, univ, pi, all) =>
  pi(
    El(A), A(C, univ, pi, all),
    B(a) = El(B(a)),
    BH(a) = B(a)(C, univ, pi, all)
  );
all{l}(A : U(l), B(a : El(A)) : U(0)) : U(0) = λ(C, univ, pi, all) =>
  all(
    El(A), A(C, univ, pi, all),
    B(a) = El(B(a)),
    BH(a) = B(a)(C, univ, pi, all)
  );

arr{l}(A : U(l), B : U(l)) : U(l) = pi(A, λ(a) => B);

//
Prop : Set = Π(
  C : *,
  f : C,
  t(A : Set, x : A, y : A) : C,
  pi(A : Set, B(a : A) : C) : C,
) -> C;
El_P(T : Prop) : Set = T(
  C = Set,
  f = False,
  t(A, x, y) = x == y,
  pi(A)
);

True : Prop = λ(C, f, t, pi) =>
  pi(
    Prop,
    B()
  );

Π(C : Prop -> Prop) -> C(x) -> C(y)

U(l : Nat) : Set;
El_U(T : U(l)) : Set;

prop : U(0);
univ(l) : U(1 + l);
pi{l}(A : U(l), B(a : El_U(A)) : U(l)) : U(l);

U(l) = (
  C(l) : *,
  prop : C(0),
  univ(r == l - 1) : C(1 + r, U(r)),
  pi(A : U(l), B(a : El_U(A)) : C(l)) : C(l)
) : C(l);

Eq(A : U(0), x : El_U(A), y : El_U(A)) : Prop;

prop : U(0);


(A : U0) -> (B : U1)
//
U2 : Set = Π(
  C : *,
  u1 : C,
  arr2(A : C, B : C) : C,
);

U1 : Set = Π(
  C : *,
  u0 : C,
  arr1(A : C, B : C) : C,
) : C;

El2(T : U1) : Set = T(
  C = Set,
  u1 = U1,
  arr0(A, B) = Π(a : A) -> B
);

U0 : Set;
El1(T : U1) : Set

U0 = Π(
  C : *,
  all1(A : U1, B(a : El1(A)) : C) : C,
  arr0(A : C, B : C) : C,
) : C;
El1(T) = T(
  C = Set,
  u0 = U0,
  arr0(A, B) = Π(a : A) -> B
);

El0(T : U0) : Set = T(
  C = Set,
  all1(A, B) = Π(a : El1(A)) -> B(a),
  arr0(A, B) = Π(a : A) -> B
);



U : Type(0) =
  | ? atom
  | ? arrow(A : U, B : U);
Inst{l}(T : U, K : Type(l)) : Type(l);
Inst{l}(T, K) =
  T.is(
  | ? atom => K,
  | ? arrow(A, B) =>
    Π(a : Inst{l}(A, K)) -> Inst{l}(B, K)
  );

// this is then acceptable
inst{l, T}(
  x : Π(A : Type(0)) -> Inst{0}(T, A)
) : Π(A : Type(l)) -> Inst{l}(T, A);
// which when untyped is just id
inst{l, T}(x) = λ(A) => x(A);

//
U{l}(A : Type(l)) : Type(l) =
  | ? atom(a : A)
  | ? arrow(L : U(A), R : U(A));
Inst{l, r, A : Type(l)}(
  T : U(A),
  C(a : A) : Type(max(l, r))
) : Type(max(l, r));
Inst{l, r, A}(T, C) =
  T.is(
  | ? atom(a) => C(a),
  | ? arrow(L, R) =>
    Π(a : Inst(L, C)) -> Inst(R, C)
  );
// this is then acceptable
inst{l, r, A : Type(l), T : U(A)}(
  x : Π(C(a : A) : Type(l)) -> Inst(T, C)
) : Π(C(a : A) : Type(max(l, r))) -> Inst(T, C);
// which when untyped is just id
inst{l, r, A, T}(x) = λ(C) => x(C);







Bool_U : U = ["arrow", ["atom"], ["atom"]];
Bool : Type(1) = Π(A : Type(0)) -> Inst{0}(Bool_U, A);



V(T : Type(0)) : Type(0) = Π(A : Type(0)) -> U{0}(T, A);
// given that it should be possible to
inst{T, l}(H : V(T)) : Type(l);

//
U(l) : Set;
U(l) =
  | ? atom
  | ? arrow(L : U(l), R : U(l))
  | ? box(L : Set, R(l : L) : U(l));

El{l}(T : U(l)) : Set;
El_P{l}(T : U(l), A : Set) : Set;

El{l}(T) = Π(A : Set) -> El_P(T, A);
El_P{l}(T, A) =
  T.is(
  | ? atom => A,
  | ? arrow(L, R) => Π(a : El_P(L)) -> El_P(R)
  | ? box(L, R) => Π(a : L) -> El_P(R(l))
  );
Lift{l, r}(A : U(l)) : U(r);
Lift{l, r}(A) =
  A.is(
  | ? atom(a) => atom(a),
  | ? arrow(L, R) => arrow(Lift(L), Lift(R))
  | ? box(L, R) => box(L, λ(l) => Lift(R(l)))
  );

lift{l, r, T}(x : El(T)) : El(Lift{l, r}(T));
lift_p{l, r, T, A}(x : El_P(T, A)) : El_P(Lift{l, r}(T), A);

lower{l, r, T}(x : El(Lift{l, r}(T))) : El(T);
lower_p{l, r, T, A}(x : El_P(Lift{l, r}(T), A)) : El_P(T, A);

lift{l, r, T}(x) = λ(A) => lift_p(x(A));
lift_p{l, r, T, A}(x) =
  T.is(
  | ? atom => x,
  | ? arrow(L, R) => λ(a) => lift_p(x(lower_p(a)))
  | ? box(L, R) => λ(a) => lift_p(x(lower_p(a)))
  );

lower{l, r, T}(x) = λ(A) => lower_p(x(A));
lower_p{l, r, T, A}(x) =
  T.is(
  | ? atom => x,
  | ? arrow(L, R) => λ(a) => lower_p(x(lift_p(a)))
  );

//
T : Type = Π(x : T $ 0) -> Type;
// this is not a valid instance
T = λ(x) => Π() -> x(x);

//
P(C : Set) = Π(
  all(T(A : C) : C) : C,
  arr(A : C, B : C) : C
) -> C;
U = Π(C : Set) -> P(C);

all{C}(T(A : C) : P(C)) : P(C) =
  λ(all, arr) => all(T(A) = T(C, A)(all, arr));

arr{C}(A : C, B : C) : P(C) =
  λ(all, arr) => arr(A, B);

Id : U = λ(C) : P(C) => all(T(A) = arr(A, A));

// the following type is safe
P : ι(P) ->
  (C : ι(C) -> (p : P(C)) -> Set) -> Set;
// which is the same as this
@P(@C(p : P(C)) : Set) : Set;
all{C}(T(A : P(C) $ 0, AH : C(A)) : P(C)) : P(C);
arr{C}(A : P(C), B : P(C)) : P(C);

P(C) = ι(T) -> Π(
  all(
    (T $ 0)(A : P(C) $ 0, AH : C(A)) : P(C)
    TH(A : P(C) $ 0, AH : C(A)) : C(T(A, AH))
  ) : C(all(T)),
  arr(A : P(C) $ 0, B : P(C) $ 0,
    AH : C(A), BH : C(B)) : C(arr(A, B))
) -> C(T);
all{C}(T(A : P(C) $ 0, AH : C(A)) : P(C)) : P(C) =
  λ(all, arr) => all(
    T(A, AH) = T(A, AH),
    TH(A, AH) = T(A, AH).@(all, arr)
  );
arr{C}(A : P(C), B : P(C)) : P(C) =
  λ(all, arr) => arr(A, B, A(all, arr), B(all, arr));



U : Set;
box(T(C(p : P(C)) -> Set) : P(C)) : U;

U = Π(@C(p : P(C)) : Set) -> P(C);
box(T) = λ(C) => T(C);


U = ι(T) -> Π(
  C(T : U) : Set,
  all(
    T(A : U $ 0, AH : C(A)) : U,
    TH(A : U $ 0, AH : C(A)) : C(T(A, AH))
  ) : C(all()),
  arr(A : U, B : U)
) -> C(T);

Id : U = λ(C, all, arr) =>
  all(T(A) = arr(A, A));

//
U : Set;
all(T(A : U) : U) : U;

U = ι(T) -> Π(
  C(T : U) : Set,
  all(
    T(A : U $ 0, AH : C(A)) : U,
    TH(A : U $ 0, AH : C(A)) : C(T(A, AH))
  ) : C(all()),
  arr(A : U, B : U)
) -> C(T);
all(T) =
  λ(C, all, arr) =>
    all(
      T(A, AH : C(A)) =
        T(A) : U
    );
//
U : Set;
U = ι(T) ->
  | ? all(T(A : U $ 0) : U) : U,
  | ? arr(A : U, B : U) : U;

all(T(A) = )
//
U : Set;
U = ι(T) -> Π(
  C(l, T : U) : Set,
  all(T(A : U) : U) : C(),
  arr(A : U, B : U)
) -> C(T);
//
U(l : Nat) : Set;
U(l) = ι(T) -> Π(
  C(l, T : U(l)) : Set,
  all(T(A(r) : C(r)) : C(l)) : C(l),
  arr(A : C(l), B : C(l))
) -> C(l);
Lift{l, w}(T : U(l)) : U(w) =
  λ(C, all, arr) => T(
    C(l) = C(w),
    all(T(A(r) : C(r)) : C(w)) = all(T(A)),
    arr(A : C(l), B : C(l)) = arr(A, B),
  );
El(T : U) : Set;
El(T) =
  T(
    Set,
    all(T) = Π(A : Set) -> T(A),
    arr(A, B) = Π(a : A) -> B,
  );

//
U(l) : Set;
univ : U(l);
all{l}(T(A : U(l)) : U(l)) : U(l);
arr{l}(A : U(l), B : U(l)) : U(l);

U(l) = ι(T) -> Π(
  C{l}(T : U(l)) : Set,
  univ : C(univ),
  all(T) : C(all(T)),
  arr(A, B) : C(arr(A, B)),
) -> C(T);
univ{l} = λ(C, atom, all, arr) => atom;
all{l}(T) = λ(C, atom, all, arr) => all(T);
arr{l}(A, B) = λ(C, atom, all, arr) => arr(A, B);

Lift{l, r}(T : U(l)) : U(l + r);
Down{l, r}(T : U(l + r)) : U(l);

Lift{l, r}(T) =
  T.@(
    C{l}(_) = C(l + r),
    atom = univ,
    all(T(A : U(l)) : U(l)) = all(T(A) = Lift(T(Down(A)))),
    arr(A, B) = arr(Lift(A), Lift(B)),
  );
//
U(l) : Set;
El{l}(T : U(l)) : Set;

U(l) =
  | ? (univ(l) : U(l)),
  | ? (pi{x, y}(A : U(x), B(a : El(A)) : U(y)) : U(l));

U(l) : Set;
El{l}(T : U(l)) : Set;

U(l) = Π(
  C(l) : Set,
  univ(l) : C(1 + l),
  pi{x, y}(A : U(x), B(a : El(A)) : U(y)) : C(max(x, y)),
) -> C(l);
El{l}(T) =
  T(
    C(l) = Set,
    univ(l) = U(l),
    pi{x, y}(A, B) = Π(a : El(A)) -> El(B(a))
  )

P(l : Nat, C(l : Nat) : Set) : Set = Π(
  univ(l) : C(l),
  all{x, y}(T(A : C(x)) : C(y)) : C(max(x, y)),
  arr{x, y}(A : C(x), B : C(y)) : C(max(x, y)),
) -> C;
U : Set = Π(C : Set) -> P(C);

Id : U = λ(C) => λ(atom, all, arr) =>
  all(T(A) = arr(A, A));

atom(C) = λ(atom, all, arr) => atom;
all(T) = λ(C, atom, all, arr) => all(T);
arr(A, B) = λ(C, atom, all, arr) => arr(A, B);

Lift{l, r}(T : U(l)) : U(l + r);
Down{l, r}(T : U(l + r)) : U(l);

Lift{l, r}(T) =
  T.@(
    C{l}(_) = C(l + r),
    atom = univ,
    all(T(A : U(l)) : U(l)) = all(T(A) = Lift(T(Down(A)))),
    arr(A, B) = arr(Lift(A), Lift(B)),
  );


//

U1 : Set = Π(
  C : Set,
  univ : C,
  arr(A : C, B : C) : C,
) -> C;
El1(T : U1, U0 : Set) : Set = T(
  C = Set,
  univ = U0,
  arr(A, B) = Π(a : A) -> B
);

U0 : Set = Π(
  C : Set,
  all(A : U1, T(A : El1(A, C)) : C) : C,
  arr(A : C, B : C) : C,
) -> C;


//
U(l) : Set;
El(T : U(l)) : Set;

U(l) : Set =
  | ? (univ(l) : U(1 + l))
  | ? (pi{l}(A : U(l), B(a : El(A)) : U(1 + l)) : U(1 + l));
El(T) =
  T.is(
  | univ(l) = U(l),
  | pi{l}(A, B) = Π(a : El(A)) -> El(B(a))
  );

Lift{l}(T : U(l)) : U(l + 1);
Down{l}(T : U(l + 1)) : U(l);
lift{l, T}(x : El{l}(T)) : El(Lift(T));
down{l, T}(x : El{l}(Lift(T))) : El(T);


Lift{l}(T) =
  T.is(
  | univ(l) = univ(l + 1),
  | pi{l}(A, B) = pi{l + 1}(
      A = Lift(A),
      B(a : El(Lift(A))) = Lift(B(down(a)))
    )
  );
Down{l}(T) =
  T.is(
  | univ(l) =
    l.is(
    | 0 = False
    | 1 + l = U(l)
    )
  | pi{l + 1}(A, B) = pi{l}(
      A = Down(A),
      B(a : El(Down(A))) = Down(B(lift(a)))
    )
  );
lift{l, T}(x) =
  T.is(
  | univ(l) = Lift(x),
  | pi{l}(A, B) = λ(a : El(Lift(A))) => lift(x(down(a)))
  );
down{l, T}(x) =
  T.is(
  | univ(l) = Down(x),
  | pi{l}(A, B) = λ(a : El(A)) => down(x(lift(a)))
  );
//
El_H(T : U(l)) : Set;
Lift{l}(T : U(l)) : U(l + 1);

El_H(T) = [x : El(T), y : El_H(Lift(T))];
Lift{l}(T : U(l)) : U(l + 1) =
  T.is(
  | univ(l) = univ(l + 1),
  | pi{l}(A, B) = pi{l + 1}(
      A = Lift(A),
      B(a : El(Lift(A))) = Lift(B(down(a)))
    )
  );



Lift{l}(T) =
  T.is(
  | univ(l) = univ(l + 1),
  | pi{l}(A, B) = pi{l + 1}(
      A = Lift(A),
      B(a : El(Lift(A))) = Lift(B(down(a)))
    )
  );
Down{l}(T) =
  T.is(
  | univ(l) = _,
  | pi{l + 1}(A, B) = pi{l}(
      A = Down(A),
      B(a : El(Down(A))) = Down(B(lift(a)))
    )
  );
lift{l, T}(x) =
  T.is(
  | univ(l) = Lift(x),
  | pi{l}(A, B) = λ(a : El(Lift(A))) => lift(x(down(a)))
  );
down{l, T}(x) =
  T.is(
  | univ(l) =
    x : El{l}(Lift(T))
    x : U(l + 1)
    Down(x),
  | pi{l}(A, B) = λ(a : El(A)) => down(x(lift(a)))
  );
//
U(l) : Set;
El(T : U(l)) : Set;
Lift{l}(T : U(l)) : U(1 + l);
lift{l, T}(x : El{l}(T)) : El(Lift(T));

U(l) : Set =
  | ? (univ(l) : U(1 + l))
  | ? (arr(A : U(1 + l), B : U(1 + l)) : U(1 + l))
  | ? (all{l}(B(A : U(l)) : U(1 + l)) : U(1 + l));
El(T) =
  T.is(
  | univ(l) = U(l),
  | arr(A, B) = Π(a : El(A)) -> El(B),
  | all(B) = Π(A : U(l)) -> El(B(A))
  );
Lift{l}(T) =
  T.is(
  | univ(l) = univ(1 + l),
  | arr(A, B) = arr(Lift(A), Lift(B)),
  | all(B) = all(B(A : U(1 + l)) =
    B()
    _)
  );

//
U(l) : Set;
El{l}(T : U(l)) : Set;

U(l) : Set =
  | univ(l) : U(1 + l)
  | arr{l}(A : U(l), B : U(l)) : U(l)
  | pi{l}(
      A(r < l) : U(r),
      B{r < l}(a : El(A(r))) : U(r)
    ) : U(l);
El(T) =
  T.is(
  | univ(l) = U(l),
  | arr{l}(A, B) = Π(a : El(A)) -> El(B),
  | pi{l}(A, B) = Π(r < l, a : El(A(r))) -> El(B(a))
  );

//
U(l) : Set;
El{l}(T : U(l)) : Set;

U(l) : Set =
  | univ(l) : U(1 + l)
  | arr{l}(A : U(l), B : U(l)) : U(l)
  | pi{l}(A : U(l), B(a : El(A)) : U(1 + l)) : U(1 + l);
  | all{l}(T(A : U(l)) : U(1 + l)) : U(1 + l)
El(T) =
  T.is(
  | univ(l) = U(l),
  | arr{l}(A, B) = Π(a : El(A)) -> El(B),
  | pi{l}(A, B) = Π(a : El(A)) -> El(B(a))
  );
//


El(T) =
  T.is(
  | univ(l) = U(l),
  | arr{l}(A, B) = Π(a : El(A)) -> El(B),
  | pi{l}(A, B) = Π(a : El(A)) -> El(B(a))
  );

//
Term, M, N, K ::=
  | x | λ(a) = K | M(N)
  | Nat | 0 | 1 + M | rec(M, N, K)

(λ(a) = K)(M) |-> K{a := M}  (count(a) in K == 1)
rec(n, N, K) |-> N⁰...(Nⁿ(K))  (fv(N) == ∅)


T(rec(M, N, K)) = T(M) + T(K) + (ω * T(N))
T(N...(Nⁿ(K))) = T(K) + (n * T(N))


//
U(l) : Set;
El{l}(T : U(l)) : Set;

U(l) : Set = Π(
  C : Set,
  univ(r == l - 1) : C,
  arr(A : C, B : C) : C,
  pi(
    A : C,
    B(a : El(A)) : C
  )
) : C

//
U(l) : Set;
El(l, r, T : U(r + l)) : Set;

U(l) : Set =
  | univ(l, r) : U(1 + r + l)
  | arr(l, A : U(l), B : U(l)) : U(l)
  | pi(l, A(r) : U(r + l), B(a : El(l, r, A)) : U(1 + r + l)) : U(1 + l);
El(l, r, T) =
  T.is(
  | univ(l, r) = U(l),
  | arr(l, A, B) = Π(a : El(l, r, A)) -> El(l, r, B),
  | pi(l, A, B) = Π(r, a : El(l, r, A)) -> El(l, 1 + r, B(a))
  );

//
U(l) : Set;
El{l}(T : U(l), C(l : Nat) : Set) : Set;

U(l) : Set =
  Π(
    C(l) : Set,
    univ(r == l - 1) : C(r),
    arr{l}(A : C(l), B : C(l)) : C(l),
    pi{l}(
      // strictly positive
      A(r) : U(r + l),
      B(a : El(A(r), C)) : C(l)
    ) : C(l)
  ) : C;
El{l}(T, C) =
  T(
    C(l) = Set,
    univ(r) = C(r),

  );


//
Type, A, B ::=
Term, M, N, K ::=
  | Prop | Type(0)
  | x
  | Π(a : A) -> B | λ(a : A) => K | M(N)
  | (@x : T) | (@x = K) | M.@

Prop : Type(0)
Type(l) : Type(1 + l)

Γ |- A : Type(0)  Γ, a : A |- B : Prop
--------------------------------------
Γ |- Π(a : A) -> B : Prop

data Bool =
  | true
  | false
Γ |- A : Type(l)  Γ, a : A |- B : Type(r)
-----------------------------------------
Γ |- Π(a : A) -> B : Type(max(l, r))

Γ |- Type(l) : Type(1 + l)

Bool : Type(1) = Π(l : Level, A : Type(l)) -> Π(t : A, f : A) -> A;
true : Bool = λ(l, A : Type(l)) => λ(t : A, f : A) => t;

b : Bool;

//

Bool : Type(1) = Π(A : Type(0)) -> Π(t : A, f : A) -> A;
true : Bool = λ(A : Type(0)) => λ(t : A, f : A) => t;

b : Bool;
b(Bool)(b)

Id : Type(1) = Π(A : Type(0)) -> Π(a : A) -> A;
id : Id = λ(A : Type(0)) => λ(a : A) => a;

id(Id : Type(1))(id)


U : Set =
  Π(
    C : Set,
    arr(T(A : C) : C) : C,
    all(A : C, B : C) : C,
  ) : C;

//
U : Set;
U =
  | arr(A : U, B : U) : U
  | all(B(P : U, R : U) : U) : U
  | mu(T(A : U) : U) : U
  | nu(T(A : U) : U) : U;

El_F(T : U) : Set;
El_F(T) =
  T.is(
  | arr(A, B) = Π(a : El_F(A)) -> El_F(B)
  | all(B) = Π(A : U) -> El_F(B(A, A))
  );


U : Set = Π(
  C : Set,
  arr(A : C, B : C) : C,
  all(B(P : C, R : C) : C) : C,
  mu(T(A : C) : C) : C
) -> C;

El_F(T : U) : Set =
  T(
    Set,
    arr(A, B) = Π(a : A) -> B,
    all(B) = Π(A : Set) -> B(A, A),
    mu(T) = μ(X) -> T(X)
  );
El_P(T : U) : Set =
  T(
    Set,
    arr(A, B) = Π(a : A) -> B,
    all(B) = μ(X) -> Π(A : Set) -> B(X, A),
    mu(T) = μ(X) -> T(X)
  );
F_to_P{T}(x : El_F(T)) : El_P(T);
P_to_F{T}(x : El_P(T)) : El_F(T);

F_to_P{T}(x : El_F(T)) : El_P(T);=
  T.is(
  | arr(A, B) = λ(f : El_F(A) -> El_F(B)) =>
      λ(a : El_P(A)) => F_to_P(f(P_to_F(a))),
  | all(B) = λ(f : Π(A : U) -> El_F(B(A, A))) =>
      μ(X) => λ(A : U) =>
        F_to_P(f(a)) : El_P(B(A, A)),
  | mu(T) = λ(f : μ(X) -> T(X)) =>
      μ(X) -> F_to_P(f(X))
  );

El_P(T : U) : Set;
El_N(T : U) : Set;

El_P(T) =
  T.is(
  | arr(A, B) = Π(a : El_N(A)) -> El_P(B),
  | all(B) =

    Π(A : U) -> El_P(B(X, A))
  | mu(B) = \mu
  );
El_N(T) =
  T.is(
  | arr(A, B) = Π(a : El_P(A)) -> El_N(B),
  | all(B) = Π(A : U) -> El_N(B(A))
  );

U : Set;
U =
  | arr(A : F, B : F) : F
  | all(B(A : F) : F) : F;

Lower_P(T : U) : U;
Lower_N(T : U) : U;

Lower_P(T) =
  T.is(
  | arr(A, B) = arr(Lower_P(A), Lower_P(B)),
  | all(B) =
    all(B(A) = Lower_P(B(A))),
  )

∀X -> X -> (X -> X) -> X
// to
μNat. ∀X -> X -> (Nat -> X) -> X


All System F types can be transformed into an equivalent type in a different system where all non strictly positive occurrences were replaced by recursive occurrences.

Goal: Calculus of Constructions with large elimination of the naturals can describe all System F types without any non strictly positive occurrences of type variables introduced by forall.


// by βη
A ≅ ∀X -> B{X} -> X

System F ≅ PA2

//


El_F(T : U(Set)) : Set;
El_F(T) =
  T.is(
  | var(x) = x,
  | arr(A, B) = Π(a : El_F(A)) -> El_F(B),
  | all(B) = Π(A : Set) -> El_F(B(A, A)) -> A,
  );

El_K(T : U(Set)) : Set;
El_K(T) =
  T.is(
  | var(x) = x,
  | arr(A, B) = Π(a : El_K(A)) -> El_K(B),
  | all(B) = Π(A : Set) -> El_K(B(A, A)) -> A,
  );

F(Var : Type) : Type;
F(Var : Type) =
  | var(x : Var)
  | arr(A : F(Var), B : F(Var))
  | all(B(A : Var) : F(Var));

U(Var : Type) : Type;
U(Var : Type) =
  | var(A : Var)
  | arr(A : U(Var), B : U(Var))
  | all(B(P : Var, N : Var) : U(Var));

El_F(T : U(Set)) : Set;
El_F(T) =
  T.is(
  | var(A) = A,
  | arr(A, B) = Π(a : El_F(A)) -> El_F(B),
  | all(B) = Π(A : Set) -> El_F(B(A, A)) -> A,
  );

El_K(T : U(Set)) : Set;
El_K(T) =
  T.is(
  | var(A) = A,
  | arr(A, B) = Π(a : El_K(A)) -> El_K(B),
  | all(B) = μ(X : Set) -> Π(A : Set) -> El_K(B(X, A)) -> A,
  );

F_to_K{T : U(Set)}(x : El_F(T)) : El_K(T);
K_to_F{T : U(Set)}(x : El_K(T)) : El_F(T);

F_to_K{T}(x) =
  T.is(
  | var(A) = x
  | arr(A, B) = λ(f) => λ(a) => F_to_K(f(K_to_F(a)))
  | all(B) = λ(f : Π(A : Set) -> El_F(B(A, A)) -> A) =>
      μ(X : Set) => λ(A : Set) =>
        λ(x : El_K(B(X, A))) => f(A)(_ : El_F(B(A, A)))
  )


U = Π(C : Set) -> W(C);




CoVariant(F(A : U) : U) = Π(A : U, B : U, a : El(F(A))) -> El(F(B));

//


f{A, X}(x : B(A, A)) : B(X, A)

El_F(T : F) : Set;
El_F(T) =
  T.is(
  | arr(A, B) = Π(a : El_F(A)) -> El_F(B),
  | all(B) = Π(A : F) -> El_F(B(A)),
  );

id{T}(x : El(T)) : El(T);
id{T}(x) =
  T.is(
  | arr(A, B) = λ(f) => λ(a) => id(f(id(a))),
  | all(B) = λ(f) => λ(A) => id(f(A)),
  );


// any type of this shape
∀A -> F(A) -> A
// can be transformed into
∀A -> G(A, A) -> A
// which then can be transformed into
μX -> ∀A -> G(X, A) -> A


//
F{A} = forall B. (B -> A) -> B;
T = forall A. (forall B. (B -> A) -> B) -> A;

G{X} = mu Y. forall B. (Y -> X) -> B;
U = mu X. forall A. G{X} -> A;


f_map{X, Y}(f : F{X}) : F{Y} =
  Λ(B) => λ(b_y : B -> Y) =>
    f{B}(λ(b_x : B -> X) => )
    _ : B

mu Y. forall B. (Y -> X) -> B

G{G{U}}

forall B. (Y -> X) -> B

f_u_to_g_u(f : F{U}) : G{U} =
  f<B>(λ(b) =>

    _ : B -> U)
forall B. (B -> U) -> B

t_to_u(x : T) : U =
  x<U> : (forall B. (B -> U) -> B) -> A

f : U = mu X => ΛA => λ(f : Y{X}) =>
  unroll(f)<A>(λ(y : Y{X}) =>

    _) :
    forall B. (Y{X} -> X) -> B
  and its Negative translation:

  μy. ∀a. (μx. ∀b. (x → y) → b) → a
//
T = forall A. ((A -> A) -> A) -> A;
x : T = Λ(A) => λ(s : (A -> A) -> A) =>
  s(λ(a) => a);
y : T = Λ(A) => λ(s : (A -> A) -> A) =>
  s(λ(a) => s(λ(a) => a));
T = forall A. ((A -> A) -> A) -> A;

U = mu X. forall A. ((A -> X) -> A) -> A;

u_to_t(x : U) : T = Λ(A) => λ(a_a_a : (A -> A) -> A) =>
  unroll(x)<T>(λ(t_u : T -> U) => _ : T)

u_to_t(x : U) : T = Λ(A) => λ(a_a_a) =>
  unroll(x)<A>(λ(a_x) =>
    a_a_a(λ(a) => a_x(a) : U)
    a_a_a(λ(a_a) = _));

f : forall A. ((A -> A) -> A) -> A

g : mu X. forall A. ((A -> X) -> A) -> A =

g : mu X. forall A. ((A -> X) -> A) -> A =
  ΛA => λ(a_x_a : (A -> X) -> A) =>
    f{A}(λ(a_a : A -> A) =>
      a_x_a(λ(a : A) => _ : X))

S = forall A. ((A -> A) -> A) -> A;
T = mu T. forall A. ((A -> T) -> A) -> A;

t : T;
unroll(t) : forall A. ((A -> T) -> A) -> A

(((A -> T) -> (A -> T)) -> (A -> T)) -> (A -> T)

s_to_t(x : S) : T =
  roll(ΛA => λ(a_to_t_to_a : (A -> T) -> A) =>
    x{A}(λ())

    a_to_t_to_a(λ(a : A) => x{A}(λ(a_to_a) => a_to_a(a))));

  x{T}(λ(t_to_t : T -> T) =>
    roll(ΛA => λ(a_to_t_to_a : (A -> T) -> A) =>
      a_to_t_to_a(λ(a : A) => _ : A -> T)))

s_to_t(x : S) : T =
  x{T}(λ(t_to_t : T -> T) =>
    roll(ΛA => λ(a_to_t_to_a : (A -> T) -> A) =>
      a_to_t_to_a(λ(a : A) => _ : A -> T)))

s_to_t(x : S) : F{T} =
  ΛA -> λ(a_to_t_to_a : (A -> T) -> A) =>
    x{A}(λ(a_to_a : A -> A) =>

      a_to_t_to_a(_ : A -> T)
      )
  s{T}(λ(t_to_t : T -> T) = _ : T)

//
T = forall A. A -> (Nat -> A) -> ((A -> A) -> A) -> A;


T = mu T. forall A. A -> (T -> A) -> ((A -> A) -> A) -> A;

a0 : T = roll(Λ(A) => λ(x) => λ(s : T -> A) => λ(w) => x);
a1 : T = roll(Λ(A) => λ(x) => λ(s : T -> A) => λ(w) => s(a0));
b : T = Λ(A) => λ(x) => λ(w) => w(λ(a) => _);
c : T = Λ(A) => λ(x) => λ(w) => w(λ(a) => a);

T = forall A. A -> (X -> A) -> ((A -> Y) -> A) -> A;
T = forall A. A -> (A -> A) -> ((A -> A) -> A) -> A;
x : T = Λ(A) => λ(x) => λ(w) => w(λ(a) => a);

T = mu X. forall A. A -> (X -> A) -> ((A -> X) -> A) -> A;
x : T = Λ(A) => λ(x) => λ(w) => w(λ(a) => _ : X?);


T = forall A. A -> (A -> A) -> ((A -> A) -> A) -> A;
a0 : T = Λ(A) => λ(x) => λ(s) => λ(w) => x;
a1 : T = Λ(A) => λ(x) => λ(s) => λ(w) => s(a);
b : T = Λ(A) => λ(x) => λ(w) => w(s);
c : T = Λ(A) => λ(x) => λ(w) => w(λ(a) => a);

y : T = Λ(A) => λ(s : (A -> A) -> A) =>
  s(λ(a) => s(λ(a) => a));
T = forall A. ((A -> A) -> A) -> A;

//
M A :=
  ∀Y.
    (A → Y) →
    (∀R. (A → R) → (R → Y) → F R → Y) →
    Y

Nat = μNat -> Unit + Nat;



T = ∀(A) -> (Unit -> A) -> (A -> A) -> A;
// same as
T = ∀(A) -> ((Unit + A) -> A) -> A;
// same as
T = ∀(A) -> ((μ(X) -> Unit + X) -> A) -> A;
// same as
T = μ(X) -> Unit + X;


T = ∀(A) -> (A -> B) -> A -> (A -> A) -> A;

T = ∀(A) -> (A -> B) -> (B -> A) -> (Unit -> A) -> A;
// to
T = ∀(A) -> (A -> B) -> ((B + Unit) -> A) -> A;
// to
T = ∀(A) -> ((((A -> B) -> B) + Unit) -> A) -> A;

T = ∀(X) -> ∀(A) -> (F{A} -> X) -> ((G{A} + H{A}) -> A) -> A;
// same as
T = ∀(X) -> ∀(A) -> ((((F{A} -> X) -> G{A}) + ((F{A} -> X) -> H{A})) -> A) -> A;
// same as
T = ∀(A) -> ((((F{A} -> X) -> G{A}) + ((F{A} -> X) -> H{A})) -> A) -> A;

T = ∀(A) -> ((F{A} -> B) × ((Unit + A) -> A)) -> A;
T = ∀(A) -> ((B -> F{A}) -> (Unit + A) -> A) -> A;

T = ∀(A) -> A -> (A -> A) -> ((A -> A) -> A) -> A;
// same as
T = ∀(A) -> (Unit -> A) -> (A -> A) -> ((A -> A) -> A) -> A;
// which is the same as
T = ∀(A) -> ((Unit + A + (A -> A)) -> A) -> A;
// I think this is the same as
T = ∀(A) -> ((μ(X) -> Unit + X + (A -> A)) -> A) -> A;
// so what to do next? Which lessons to take from this?

T = ∀(A) -> ((Unit + A + (A -> A)) -> A) -> A;
a0 : T = Λ(A) => λ(f) => f(l(unit));
a1 : T = Λ(A) => λ(f) => f(r(l(f(l(unit)))));
b : T = Λ(A) => λ(f) => f(r(r(λ(a) = f(r(l(a))))));
c : T = Λ(A) => λ(f) => f(r(r(λ(a) = a)));



Unit + (μ(X) -> Unit + X + (A -> X)) +
  (A -> (μ(X) -> Unit + X + (A -> X)))

T = ∀(A) -> ((μ(X) -> Unit + X + (A -> A)) -> A) -> A;
a0 : T = Λ(A) => λ(f) => f(roll(l(unit)));
a1 : T = Λ(A) => λ(f) =>
  f(roll(r(l(_ : ))));

c : T = Λ(A) => λ(f) =>
  f(roll(r(r(λ(a) => _))));

T = ∀(A) -> (Unit -> A) -> (A -> A) -> ((A -> A) -> A) -> A;


T = ∀(A) -> ((Unit + A + (A -> A)) -> A) -> A;


T = ∀(A) -> Unit + A + (A -> A) @ A;


μL : Type;
νR : Type;

μL = ∀(A) -> A -> (μL -> A) -> ((A -> νR) -> A) -> A;
νR = ∀(A) -> A -> (μL -> A) -> ((A -> νR) -> A) -> A;

//
T = ∀(A) -> A -> (A -> A) -> (A -> A -> A) -> A;
// same as
T = ∀(A) -> (Unit -> A) -> (A -> A) -> (A -> A -> A) -> A;
// same as
T = ∀(A) -> ((Unit + A + (A -> A)) -> A) -> A;
// same as
T = ∀(A) -> ((A + (A -> A)) -> A) -> A;
// equivalent to
T = ∀(A) -> (((Unit -> A) + (A -> A)) -> A) -> A;
// equivalent to
T = ∀(A) -> (((Unit + A) -> A) -> A) -> A;

T = ∀(A) -> (((Unit -> A) + (A -> A)) -> A) -> A;
// those are the general shape of instances
x : T = Λ(A) => λ(f) => f(l(_));
y : T = Λ(A) => λ(f) => f(r(_));


T = ∀(A) -> ((A + (Unit -> A) + (A -> A)) -> A) -> A;
T = ∀(A) -> ((A + (Unit + A -> A)) -> A) -> A;

T = ∀(A) -> (((Unit + A) -> A) -> A) -> A;
// those are the general shape of instances
x : T = Λ(A) => λ(f) => f(_ : (Unit + A) -> A);


T = ∀(A) -> (∀(B) -> B -> B -> B) -> (K{A} -> A) -> A;
// can go to
T = ∀(A) -> (A -> A -> A) -> (((∀(B) -> B -> B -> B) -> K{A}) -> A) -> A;


T = ∀(A) -> Bool × (K{A} -> A) -> A;
// weaken
T = ∀(A) -> (A × A -> A) × ((Bool -> K{A}) -> A) -> A;
// to
T = ∀(A) -> (((Bool -> K{A}) + (A × A)) -> A) -> A;

// every System F type can be described as
T = ∀(A) -> (F{A} -> A) -> A;


// λP2 Type, same as
T = ∀(A) -> Π(x : A -> A -> A) ×
  ((Π(b : Bool) -> b{A} == x -> K{A}) -> A) -> A;

// all closed System F types can have this form
∀(A) -> (F{A} -> A) -> A


//
A -> B |-> B  (when A cannot influence B)
//
Item, S, T ::=
  | a | S × T | A // closed type
Payload, P ::=
  | S | S + P
Type, A ::=
  | ∀(a) -> (P -> a) -> a

A -> Unit |-> Unit





// T means closed
∀(A) -> ((((X + ...Y) -> A) + ...T) -> A) -> A



// can it be weakened to?
∀(A) -> ((Unit + ((Unit + A) -> A)) -> A) -> A

(Unit -> A) + (A -> A)


∀(K) -> (A + T -> K) -> K

∀(A) -> ((Unit + (A -> A)) -> A) -> A

∀(A) -> ((Unit + ((A -> Bool) -> Bool)) -> A) -> A


∀(A) -> F{A} -> A;

∀(A) -> ((F{A} -> Bool) -> A) -> A

(X -> A) × (Y -> A) |-> (X + Y) -> A

∀(A) -> (X -> A) -> (Y -> A) -> A
∀(A) -> ((X + Y) -> A) -> A

∀(A) -> ((Unit + ((A -> Bool) -> Bool)) -> A) -> A

∀(A) -> A -> (((A -> Bool) -> Bool) -> A) -> A
// same as
∀(A) -> ((Bool -> Bool) -> A) -> A


∀(A) -> ((((A -> Bool) -> A) -> Bool) -> A) -> A  (K is closed)




∀(A) -> (((A -> A) -> Bool) -> A) -> A
∀(A) -> (((_ -> A) -> Bool) -> A) -> A
// same as
∀(A) -> (Bool -> A) -> A

∀(A) -> ((Bool -> Bool) -> A) -> A

∀(A) -> (Unit -> A) -> (Bool -> A) -> A

(Bool × Bool)

∀(A) -> (Unit -> A) -> (Bool -> A) -> A

∀(A) -> (Unit -> A) -> ((Bool -> Bool) -> A) -> A
// same as
∀(A) -> (Unit -> A) -> (Bool -> A) -> A

T ::= ∀(A) -> (P[A] -> A) -> A;

∀(A) -> (P[A] -> A) -> A


∀(A) -> ((Unit + A + Bool) -> A) -> A


Unit + A

//
Unit × Unit |-> Unit          // 1 × 1
Unit + Unit |-> Bool          // 1 + 1
Unit -> A |-> A               // A ^ 1
A -> Unit |-> Unit            // 1 ^ A
Bool -> Bool |-> Bool + Bool  // 2 ^ 2
//
A -> B |-> B  (when A cannot influence B)

// a most general form for System F
T ::= ∀(A) -> (S[A] -> A) -> A;
Sum Poly, SP[A] ::=
  | (? -> A) + SC[A] | SC[A]
Sum Const, SC[A] ::=
  | Void | -> T

μ(A) -> Unit + A + (A -> A)
// same as
μ(A) -> Unit + ((Unit + A) -> A)


// same as
μ(A) -> Unit + (μ(X) -> X + (A -> Unit))

∀(A) -> (((μ(X) -> X + A) -> Bool) -> A) -> A

∀(A) -> ((Unit + (∀(X) -> ((X + A) -> X) -> X)) -> A) -> A
// same as
μ(A) -> Unit + (μ(X) -> X + A)


∀(A) -> ((
  ∀(X) -> (((_ -> A) + T) -> X) -> X
  U
) -> A) -> A





List(A) = Unit + (A × List(A))
∀(A) -> ((Unit + A) -> A) -> A





∀(A) -> ((Unit + (∀(X) -> ((X + A) -> X) -> X)) -> A) -> A
Λ(A) => λ(f : (∀(X) -> ((X + A) -> X) -> X) -> A) =>
  f(Λ(X) => λ(k : (X + A) -> X) => k())


∀(A) -> (S[A] -> A) -> A

∀(A) -> (S[A] -> A) -> A

∀(A) -> (((A -> Bool) -> Bool) -> A) -> A

// same as
forall A. T × A × A -> A


μX. (X -> Bool) -> Bool
μX. ∀(A) -> ((X -> Bool) × A × A) -> A

μX. Bool + (X ^ 3)
X0 == 0
X1 == μX. Bool × (X0 ^ 3) == 2
X2 == μX. Bool + 8 == _

8 + 2
2 ^ 3

2 + 2 ^ 3

μX. Bool + (X ^ 3)

μX. Bool

μX. (X -> Bool) -> Bool × X
2 ^ (2 ^ X)

μX. (X -> Bool) × X

μX. ((X -> Bool) -> Unit) + ((X -> Bool) -> Unit)

2 ^ (2 ^ A)

Bool -> (((A -> Bool) -> Bool) -> A) -> A




∀(A) -> (((A -> Bool) -> Bool) -> A) -> A

∀(A) -> (((A -> Bool) -> Bool) -> A) -> A
Λ(A) => λ(f : ((A -> Bool) -> Bool) -> A) =>
  f(λ(k : A -> Bool) => not(k(f(λ))))

∀(A) -> ((Unit + (Unit -> A) + (A -> A)) -> A) -> A
∀(A) -> ((Unit + ((Unit + A) -> A)) -> A) -> A

∀(A) -> ((Unit + ((Unit -> A) × (A -> A))) -> A) -> A

P[A] ::= | Void | M[A] + P[A]
M[A] ::= | Unit | W[A] × M[A]

C[A] ::= | A | N[A] -> P[A] | N() ->
N[A] ::=

C[A] ::= | T | ? -> T

// T and U are closed
∀(A) -> (
  (
    ((X[A] + ...Y[A]) -> A) +
    ...((Z[A] -> U) -> T)
  ) -> A
) -> A


//
W(X). F{X} = ∀A. (F(A) -> A) -> A

F ::= ∀(A) -> (S[A] -> A) -> A;
Sum Poly, SP[A] ::=
  | (? -> A) + SC[A] | SC[A]
Sum Const, SC[A] ::=
  | Void

T1 = ∀(A) -> ((Unit + (X[A] -> A) + (Y[A] -> A)) -> A) -> A

T2 = ∀(A) -> ((Unit + ((X[A] + Y[A]) -> A)) -> A) -> A;

// what about this
T1 = ∀(A) -> ((Unit + (μ(B) -> (X[A, B] -> A)) + (μ(B) -> (Y[A, B] -> A))) -> A) -> A



λ(P : A) => K

[M, N]

λ(P : A) => K

λ(x) => P : A = x; K
λ(x) => y : A = x; P = y; K

λ(x) => P(Q) : T = x; K
λ(x) => P : Π(_X, λ(Q) => T) = x; K
λ(x) => P : Π(_X, λ(Q) => T) = x; K


P : A = N; K
P = (N : A); K

(P(Q : A) : B) = N; K

P(Q) : T = N; K
P = λ(Q) => (N : T); K

([] : A) ⇐

(λ(x) => K)(N)


(M : A) |->

(M : A) // bad
(x : A = M) // good

(f(a : A) : B)

f(x : A, y : B) = _;
// before
l = f(M : A, N : B);
// after
r = f(x : A = M, y : B = N);
// you can also just ignore the name
w = f(_ : A = M, _ : B = N);


P : A = _;

drop(A : Type, u : Unit, x : A) : A;


[P, Q] = x; K
P = x.0; Q = x.1; K
[P, Q] = N; K

(x : A : B) = N; K
(x : A : B) = N; K

//
Not : Type -> Type
Not(A) = A -> ⊥

P : Type(1) -> Type(0)
P(A) = A -> Type(0)

U : Type(1)
U = (X : Type(1) $ 0) -> (P(P(X)) -> X) -> P(P(X))

tau : P(P(U)) -> U;
tau(t) = X => f => p -> t(x => p(f(x(X)(f))));

sigma : U -> P(P(U));
sigma(s) = s(U)(tau);

Delta : P(U);
Delta(y) = Not((p : P(U)) -> sigma(y)(p) -> p(tau(sigma(y))));

Omega : U;
Omega = tau(p => (x : U) -> sigma(x)(p) -> p(x));

D : Type(1)
D = (p : P(U)) -> sigma(Omega)(p) -> p(tau(sigma(Omega)))

lem1 : (p : P(U)) -> ((x : U) -> sigma(x)(p) -> p(x)) -> p(Omega)
lem1(p)(h) = h(Omega)(x => h(tau(sigma(x))))

lem2 : Not(D)
lem2 = lem1(Delta)(x => h2 => h3 =>
  h3(Delta)(h2)(p => h3(y => p(tau(sigma(y))))))

lem3 : D
lem3(p) = lem1(y => p(tau(sigma(y))))

boom : ⊥
boom = lem2(lem3)

//

U(l : Level) : Set;
El_U{l}(T : U(l)) : Set;
F(l : Level) : Set;
El_F{l}(T : F(l)) : Set;

U(l) =
  | univ(l) : U(1 + l)
  | univ_f
  | pi(A : U(l), B(a : El(A)) : U(l))
  | ind(A : F)
F(l) =
  | all(B(+ A : F(l)) : F(l))
  | arr(A : F(l), B : F(l))
  | box(A : U(l));
El_U{l}(T) =
  T.is(
  | univ(l) => U(l)
  | univ_f => F
  | pi(A, B) => (a : El_U(A)) -> El_U(B(a))
  | ind(A) => El_F(A)
  );
El_F{l}(T) =
  T.is(
  | all(B(+ A : F) : F) => (A : F) -> El_F(B(A))
  | arr(A : F, B : F) => (a : El_F(A)) -> El_F(B)
  | box(A) => El_U(A)
  );

//
F(l, X(l : Level) : Set) : Set;
F(l, X(l)) =
  | all(B(+ A : F(l, X)) : F(l, X))
  | arr(A : F(l, X), B : F(l, X))
  | box(A : X(l));

U(l : Level) : Set;
El_U{l}(T : U(l)) : Set;
El_F{l}(T : F(U(l))) : Set;

U(l) =
  | univ(l) : U(1 + l)
  | pi(A : U(l), B(a : El_U(A)) : U(l))
  | ind(l, A(X) : F(l, X))
El_U{l}(T) =
  T.is(
  | univ(l) => U(l)
  | univ_f => F
  | pi(A, B) => Π(a : El_U(A)) -> El_U(B(a))
  | ind(A) => El_F(A(U(l)))
  );
El_F{l}(T) =
  T.is(
  | all(B) => Π(A : F(U(l))) -> El_F(B(A))
  | arr(A, B) => Π(a : El_F(A)) -> El_F(B)
  | box(A) => El_U(A)
  );

F_Id(X) : F(X) = all(λ(A) => arr(A, A));

U_Id : U(0) = ind(F_Id);
id : El_F(U_Id);

id(A : F(U(0)))

El_U{l}(T : U(l)) : Set;

El_F{l}(T : F(l)) : Set;


F : Set;
F =
  | all(B(+ A : F) : F)
  | arr(A : F, B : F)
  | atom;
El_F(T : F, X : Set) : Set;
El_F(T, X) =
  T.is(
  | all(B) => Π(A : Set) -> El_F(B(A), X)
  | arr(A, B) => Π(a : El_F(A, X)) -> El_F(B, X)
  | atom => X
  );

//
F : Set;
F = Π(
  C : Set,
  all(B(A : C) : C) : C,
  arr(A : C, B : C) : C,
) -> C;
El_F(T : F, X : Set) : Set;
El_F(T, X) =
  T(
    Set,
    all(B) = Π(A : Set) -> B(A),
    arr(A, B) = Π(a : A) -> B,
  );

F_Id : F = λ(C, all, arr) => all(λ(A) => arr(A, A));
id : El_F(F_Id);

id(F)(atom) : F -> F

U(l : Level) : Set;
El_U{l}(T : U(l)) : Set;

U(l) =
  | univ(l) : U(1 + l)
  | pi(A : U(l), B(a : El(A)) : U(l))
  | ind(A : F) : U(0);
El_U{l}(T) =
  T.is(
  | univ(l) => U(l)
  | pi(A, B) => Π(a : El_U(A)) -> El_U(B(a))
  | ind(A) => Π(X : Set) -> El_F(A, X)
  );

F_Id : F = all(λ(A) => arr(A, A));
U_Id : U(0) = ind(F_Id);

inst(
  B(+ A : F) : F,
  x : Π(X : Set) -> El_F(all()))

id : El_U(U_Id);
id : Π(X : Set) -> El_F(F_Id, X);
id : Π(X : Set) -> Π(A : F) -> El_F(A, X) -> El_F(A, X);
id(U(0)) : Π(A : F) -> El_F(A, U(0)) -> El_F(A, U(0))
id(U(0), atom) : U(0) -> U(0)




map{L, R, T}(
  f(l : L) : R,
  g(r : R) : L,
  x : El_F(T, L)
) : El_F(T, R);
map{L, R, T}(f, x) =
  T.is(
  | all(B) => λ(A) => map(f, g, x(A))
  | arr(A, B) => λ(a : El_F(A, R)) =>
    map(f, g, x(map(g, f, a)))
  | atom => f(x)
  );

El_U{l}(T) =
  T.is(
  | univ(l) => U(l)
  | univ_f => F
  | pi(A, B) => (a : El_U(A)) -> El_U(B(a))
  | ind(A) => El_F(A)
  );
El_F{l}(T) =
  T.is(
  | all(B(+ A : F) : F) => (A : F) -> El_F(B(A))
  | arr(A : F, B : F) => (a : El_F(A)) -> El_F(B)
  | box(A) => El_U(A)
  );


//
F : Set;
F = Π(
  C : Set,
  all(B(A : C) : C) : C,
  arr(A : C, B : C) : C,
) -> C;
// this is allowed because the body is simply typed
El_F(T : F) =
  T(
    Set,
    all(B) = Π(A : Set) -> B(A),
    arr(A, B) = Π(a : A) -> B,
  );

U(l : Level) : Set;
El_U{l}(T : U(l)) : Set;

U(l) =
  | univ(l) : U(1 + l)
  | pi(l, A : U(l), B(a : El(A)) : U(l))
  | ind(A : F) : U(0);
El_U{l}(T) =
  T.is(
  | univ(l) => U(l)
  | pi(l, A, B) => Π(a : El_U(A)) -> El_U(B(a))
  | ind(A) => El_F(A)
  );


(Π(A : Type(l)) -> (B : Set) : Set)
Γ |- Set(l)
□
Γ |- Π(a : A)
---------------------------
Γ |- Π(a : A) -> B : Set(l)



// this is allowed because the body is simply typed
El_F(T : F) =
  T(
    Set,
    all(B) = Π(A : Set) -> B(A),
    arr(A, B) = Π(a : A) -> B,
  );

U(l : Level) : Set;
El_U{l}(T : U(l)) : Set;

U(l) =
  | univ(l) : U(1 + l)
  | pi(l, A : U(l), B(a : El(A)) : U(l))
  | ind(l, A : F(U(l))) : U(l);
El_U{l}(T) =
  T.is(
  | univ(l) => U(l)
  | pi(l, A, B) => Π(a : El_U(A)) -> El_U(B(a))
  | ind(l, A) =>
    A(
      Set,
      all(B) = Π(A : Set) -> B(A),
      arr(A, B) = Π(a : A) -> B,
      pi(B(A : U(l)) : Set) =
        Π(A : U(l)) -> B(A),
    )
    El_F(A)
  );

F1 : Set;
F1 = Π(
  C : Set,
  set : C,
  arr(A : C, B : C) : C,
) -> C;

F2 = Π(
  C : Set,
  all(A : F1, B(a : ) : C) : C,
  arr(A : C, B : C) : C,
) -> C;

//
U1 : Set;
U1 =
  | set0
  | arr1(A : U1, B : U1);

El_U1(A : U1, X : Set) : Set;
El_U1(A, X) =
  X.is(
  | set0 => U0
  | arr1(A, B) => Π(a : El_U1(A, X)) -> El_U1(B, X)
  )

U0 : Set;
U0 =
  | all0(l, A : U1, B(a : El_U1(A, U0)) : U0(l)) : U0(l)
  | arr0(l, A : U0(l), B : U0(l)) : U0(l);

U0(l) = Π(X : Set) -> U0(l, X);

Π(X : Set -> Set) -> (r : X())

Lift{l, r, X}(T : U0(l, X)) : U0(r, X);
Lift{l, r, X}(T) =
  T.is(
  | all0(l, A, B) =>
    all0(r, A, λ(a : El_U1(A, X)) =>
      B(a)
      _)
  )

//
U1(l) : Set;
U1(l) =
  | set0(l)
  | arr1(A : U1(l), B : U1(l));

U0(l : Level) : Set;
El_U1(A : U0)
U0(l : Level) =
  | all0(A : U1(l), B(a : El_U1(A)) : U0(l)) : U0(l)
  | arr0(A : U0(l), B : U0(l)) : U0(l);


U(s : Size) : Set;
U(s) =
  | (all(s, B(t, s < t, A : U(t)) : U(t)) : U(s))
  | (arr(s, A : U(s), B : U(s)) : U(s));


El{s}(T : U(s)) : Set;
El{s}(T) =
  T.is(
  | all(s, B) => Π(A : U(s)) -> El(B(1 + s, _, A))
  | arr(s, A, B) => El(A) -> El(B)
  );

Lift{l, r}(T : U(l)) : U(r + l);
Lift{l, r}(T) =
  T.is(
  | all(l, B(t, l_lt_t, A)) =>
    all(r + l, λ(t, r_and_l_lt_t, A) =>
      B(t, _ : l < l, A))
  | arr(l, A, B) =>
    arr(r, Map(A), Map(B))
  );

El{s}(T : U(s)) : Set;
El{s}(T) =
  T.is(
  | all(s, B) => Π(A : U(s)) -> El(B(A))
  | arr(s, A, B) => El(A) -> El(B)
  );

map{l, r}(T : U(l)) : U(r);
map{l, r}(T) =
  T.is(
  | all(s, B) =>
    all(s, λ(A) => map(B(Map{r, l}(A))))
  | arr(s, A, B) =>
    arr(s, map(A), map(B))
  );



U(l : Level) : Set;
U(l) =
  | (all(s, B(t, s < t, A : U(t)) : U(t)) : U(s))
  | (arr(s, A : U(s), B : U(s)) : U(s));

//
U(l : Level) : Set;
El{l}(T : U(l)) : Type(0);

U(l) =
  | (univ(l) : U(l))
  | (pi(l, A : U(l), B(a : El(A)) : U(l)));
El{l}(T) =
  T.is(
  | univ(l) => Set
  | pi(l, A, B) => Π(a : El(A)) -> El(B(a))
  );
//
U(l : Level) : Set;
El{l}(T : U(l)) : Type(0);

U(l) =
  | (univ(l) : U(l))
  | (pi(l, A : U(l), B(a : El(A)) : U(l)));
El{l}(T) =
  T.is(
  | univ(l) => Set
  | pi(l, A, B) => Π(a : El(A)) -> El(B(a))
  );
//
U(l : Level, El{r < l}(T : U(r)) : Type(0)) : Set;
U(l, El) =
  | (univ(l) : U(l))
  | (pi(l, A : U(l), B(a : El(A)) : U(l)) : U(l));

El{l}(T : U(l, El)) : Type(0);
El{l}(T) =
  T.is(
  | univ(l) => Set
  | pi(l, A, B) => Π(a : El(A)) -> El(B(a))
  );


Id : U(1, El) = pi(1, univ(0), B(A) = pi(A, λ(a) = A));

id : El(Id);
id : Π(A : Set) -> A -> A;


//
U(El{r < l}(T : U(r)) : Type(0)) : Set;
U(El) =
  | (univ(l) : U(l))
  | (pi(l, A : U(l), B(a : El(A)) : U(l)) : U(l));

El{l}(T : U(l, El)) : Type(0);
El{l}(T) =
  T.is(
  | univ(l) => U(l)
  | pi(l, A, B) => Π(a : El(A)) -> El(B(a))
  );

//






// Γ is only valid if it includes Type and Π
Type : Type;
// context is very dependent, so this is valid
Π : Π(Type)(λ(A) => Π(Π(A)(λ(a) => Type))(λ(B) => Type));

// but with sugar
(A -> B) == Π(A)(λ(a) => B)
Π(a : A) -> B == Π(A)(λ(a) => B)

// this is initializing the context
Type : Type;
Π : Π(A : Type) -> (A -> Type) -> Type;

// those are more or less standard rules
Γ, a : A |- M : B(a)
-----------------------
Γ |- λ(a) => M : Π(A)(B)

Γ |- M : Π(A)(B)  Γ |- N : A
----------------------------
Γ |- M(N) : B(N)


// sugar
(Π(a : A) -> B) == El

// this surprisingly works as a predicative hierarchy
Type : Type;
(×)_and_(->) :
  (Type × Type -> Type) ×
  (Type × Type -> Type);

(->) : Type -> Type -> Type;
(×) : Type × Type -> Type;

Set : Level -> Type;

Level : Type;
0 : Level;
(1 + _) : Level -> Level;
(∀) : (Level -> Type) -> Type;


// this is sugar to ×
Set : ∀(l) -> El(1 + l)(Set(1 + l));
El : ∀(l) -> El(l)(Set(l)) -> Type;

(⇑) : ∀(l) -> El(Set(l)) -> El(Set(1 + l));
(Π) : ∀(l) -> Π(A : El(l)(Set(l))) ->
  (El(l)(A) -> El(l)(Set(l))) -> El(l)(Set(l));



Π(1)(Set(0))(λ(A : El(0)(Set(0))) => ⇑A -> ⇑A) : El(1)(Set(1))

λ(A : Set(0)) => (a : ⇑A)


// three types of functions
Γ, a : A |- M : B
----------------------- // meta arr
Γ |- λ(a) => M : A -> B

Γ, l : Level |- M : T(l)
------------------------ // meta all
Γ |- λ(l) => M : ∀(T)

Γ, a : A |- M : B(a)
---------------------------------- // obj pi
Γ |- λ(a) => M : El(l)(Π(l)(A)(B))



Set : ∀(l) -> (El : ∀(l) -> El(Set(l)) -> Type) -> Type
Set : Level -> Type;
El :
(l, T : El(Set(l))) : Type;


[Type, λ(A) => (A -> Type)] : Σ([Type, λ(A) => (A -> Type)])

(Σ) : (Σ([Type, λ(A) => _])) -> Type
Level : Type;
0 : Level;
(1 + _) : Level -> Level;
(∀) : (Level -> Type) -> Type;

Set : Level -> Type;
El : ∀(l) -> Set(l) -> Type;
(Π) : ∀(l) -> Π(A : Set(l)) -> (A -> Set(l)) -> Set(l);

(Π) : ∀(l) ->
  _ -> Type;

U : ∀(l) -> Set(1 + l);
(⇑) : ∀(l) -> Set(l) -> Set(1 + l);
(Π) : ∀(l) -> Π() -> Type;

(Π) : ∀(l) -> El(
  Π(1 + l)(U(l))(λ(A : El(1 + l, U(l))) => A -> U(l)));
(Π) : ∀(l) -> El(Π(A : Set(l)) -> (El(A) -> Set(l)) -> Set(l));

El(U(l)) |-> Set(l);

⇑ : ∀(l) -> Set(l) -> Set(1 + l);
(->) : ∀(l) -> Set(l) -> Set(l) -> Set(l);
(∀) : ∀(l) -> (Set(l) -> Set(l)) -> Type;
(Π) : ∀(l) -> ∀(A : Set(1 + l)) -> (A -> U(l)) -> U(l)

  _ : Set(l)
  (A -> U(l))
  (Set(1 + l)) -> (_ -> Set(l)) -> Set(1 + l)
  Π(A : Set(1 + l)) -> (A -> Set(l)) -> Set(1 + l);
El : ∀(l) ->
  Π(A : U(0))

  (T : U(0))

Π(1)(U(0))(λ(A : U(0)) =>
  Π(_ : )
  ⇑A -> A)


Π : ∀(l) -> Π(A : Set(l)) -> (El(A) -> Set(l)) -> Set(l);
// computing was added for convenience
El(1 + l)(U(l)) |-> Set(l)
El(l)(⇑(l)(A)) |-> El(A)
El(l)(Π(l)(A)(B)) |-> El()


Π(1)(U(0))(λ(A : El(U(0))) => )

El : ∀(l) -> Set(l) -> Type;
U : ∀(l) -> Set(1 + l);
⇑ : ∀(l) -> Set(l) -> Set(1 + l);
Π : ∀(l) -> Π(A : Set(l)) -> (A -> Set(l)) -> Set(l);

El()

Π(1)(U(0))

// three types of functions
Γ, a : A |- M : B
----------------------- // meta arr
Γ |- λ(a) => M : A -> B

Γ, l : Level |- M : T(l)
------------------------ // meta all
Γ |- λ(l) => M : ∀(T)

Γ, a : A |- M : B(a)
------------------------------ // obj pi
Γ |- λ(a) => M : Π(l)(r)(A)(B)




Set(l) : Type


El : ∀(l) -> (l, T : Set(l)) : Type;

Π : ∀(l) -> Π(A : Type) -> (A -> Type) -> Type;

ULevel : ULevel;
(->) : ULevel -> ULevel -> ULevel;
Level : ULevel;
zero : Level;
succ : Level -> Level;
Level : Set(ω);
Set(0) : Set(1);
Set(1 + l) : Set(1)
Set(l) : case(
  l,
  Set(1),
  (T) => Set(succ(l))
)

Set : Π(l : Level) -> Set(l + 1);
Π : Π(l : Level)

Set :

suc : Level -> Level;
l : Level;


Π(A : Type, B : Π(A, λ(a) => Type))

(->) : Type -> (Type -> Type);
(@⁰) : (Type -> Type -> Type) -> Type -> Type;

(∀) : (Type -> Type) -> Type;
(Π) : ∀(λ())
($) : (A -> B)
(@) :
arr : arr(Type, fam);
app_fam :

A -> Type

A -> Type

Γ |- A : Type  Γ |- B : Π(A, Type)
----------------------------------
Γ |- Π(A, B)

Γ |- A : Type  Γ |- B : Π(a : A) -> Type
-----------------------------------------
Γ |- Π(a : A) -> B

arr(A : Type, B : Type) : Type;
app : arr()

Γ, a : A |- A : Type
--------------------
Γ, a : A is Context

------------------------
(Type : Type) is Context

//
Type : Type;
(×)_and_(->) :
  (Type × Type -> Type) ×
  (Type × Type -> Type);

//
Type : Type;
(->) : Type -> Type -> Type;

Level : Type;
0 : Level;
(_ + 1) : Level -> Level;
(∀) : (Level -> Type) -> Type;

F : Type;
(->) : F -> F -> F;
(∀) : (F -> F) -> F;

Level : F = F -> (F -> F) -> F;
0 : Level = λ(z) => λ(s) => z;
(_ + 1) : Level -> Level =
  λ(n) => λ(z) => λ(s) => s(n(z)(s));
(∀) : (Level -> Type) -> Type = λ(T) =>
  ∀()
  ;

Set : Level -> Type;

(Π) : ∀(l) -> Π(1 + l)(Set(l))(λ(A) => (A -> Set(l)) -> Set(l));


Π(1 + l)(Set(l))(λ(A) =>
    (A -> Set(l)) -> Set(l) : Set(1 + l)
    ) : Set(1 + l)

Π : Π(A : Set(l)) -> (El(A) -> Set(l)) -> Set(l)
El : ∀ˡ(l) -> Set(l) -> Type;

univ : ∀ˡd(l) -> Set(1 + l);
pi : ∀()

Level : Type;
0 : Level;
(_ + 1) : Level -> Level;
(∀) : (Level -> Type) -> Type;


arr : ∀(l) -> Set(l) -> (Set(l) -> Set(l));
all : ∀(l) -> (Set(l) -> Set(l)) -> Set(l);

El(1 + l)(univ(l)) |-> Set(l)
El(1 + l)(arr(l)(A)(B)) |-> El(l)(A) -> El(l)(B)
El(1 + l)(all(l)(T)) |->

  El(l)(A) -> El(l)(B)

pi : ∀(l) -> El(all(1 + l)(λ(A : Set(1 + l)) => arr(A, U(l))))

(Π) : ∀(l) ->
  Π(l)()
  ∀ˢ(l)(λ(A) => (A ->ˢ ?) -> ?)

El(1 + l, U(l)) : Type;

(->ˢ)(1)(U(0))
(->ˢ) : ∀(l) -> Set(l) -> Set(l) -> Set(l);

Set(l)

(->ˢ) : ∀(l) -> Set(l) -> Set(l) -> Set(l);
(∀ˢ) : ∀(l) -> (Set(l) -> Set(l)) -> Set(l);

Π : ∀(l) -> ∀ˢ(l)(λ(A) => (A ->ˢ ?) -> ?)
  Π(A : Set(l)) -> (El(A) -> Set(l)) -> Set(l);

Set(l) =
  | univ(l) : Set(1 + l)
  | pi()


Set(0) : Type

Π(A : Set(l)) -> (El(A) -> Set(l)) -> Set(l);




//
Type : Type;
(->ᵗ) : Type ->ᵗ Type ->ᵗ Type;

F : Type;
(->ᶠ) : F ->ᶠ F ->ᶠ F;
(∀ᶠ) : (F ->ᶠ F) ->ᶠ F;

Kind : Type =
  | atom : Type
  | arr : Kind -> Kind -> Type;

Level : Type;
0 : Level;
(_ + 1) : Level -> Level;
(∀) : (Level -> Type) -> Type;

f : ∀(T)

app(f : ∀(T), )


Set : T_Set;

f(l : Level) : Type =


Set.0 : Set.1.0
Set.1.0 : Set.1.1.0
Set.1.1.0 : Set.1.1.1.0


T_Set : Type;
T_Set = (Set : Set.1 × T_Set);
Set : T_Set;

Set : Level -> Type;
El : ∀(l) -> Set(l) -> Type;

(arr) : ∀(l) -> Set(l) -> Set(l) -> Set(l);

(Π) : Π(A : Set(l)) -> (El(A) -> Type) -> Type;

Π()



Set : Level -> Type;
El : ∀(l) -> Set(l) -> Type;

univ : ∀(l) -> Set(1 + l);
El(1 + l)(univ(l)) |-> Set(l)

arr : ∀(l) -> Set(l) -> Set(l) -> Set(l);
El(l)(arr())

pi : ∀(l) ->
  pi(1 + l)(univ(l))(λ(A) =>
    (El(A) -> Set(l)) -> Set(l));


pi(l) : pi(1 + l)(univ(l))(λ(A) =>
    (El(A) -> Set(l)) -> Set(l))



//
Type : Type;


id = Λ(A : Type) => λ(x : A) => x;
A : Type = id(Type)(Nat);

Id : Type = ∀(A : Type) -> A -> A;

Bool : Type = ∀(A : Type) -> A -> A -> A;
T = Bool -> Type;
f : T = λ(b) => b(Type)(Bool)(Type);

∀(X) -> (((X -> Type) -> Type) -> X) -> X

∀()


∀(A)

λP2?
System ω



id(A : Type, a : A) : A = a;


Grade, G, H ::=
  | 0 | 1 | G + H | G * H

Term, M, N, K ::=
  | x
  | Π(a : A) -> B | λ(a) => K | M(N)
  | A $ G | #many(M, G) | $x = N; K

T(x) == 0
T(Π(a : A) -> B) == T(A) + T(B)
T(λ(a) => K) == 1 + T(K)
T(M(N)) == T(M) + T(N)
T(A $ G) == T(A) + T(G)
T(#many(M, G)) == T(M) * G
$x = N; K == T(N) + T(K)


Γ |- Prop is Type

Γ |- A is Prop  Γ |- B is Prop
------------------------------
Γ |- Π(a : A) -> B is Prop

Γ |- A is Prop  Γ |- B is Prop
------------------------------
Γ |- Π(a : A) -> B is Prop

ind(n) : Π(C : Nat -> Type) ->
  C(0) -> (Π(n : Nat) -> C(n) -> C(S(n))) -> C(n);


C_Bool : Type = Π(C : Type) -> C -> C -> C;
I_Bool(c : C_Bool) = Π(C : C_Bool -> Type) ->
  C(c_true) -> C(c_false) -> C(c);
Bool = [c : C_Bool, i : I_Bool(c)];

// then you can derive
eta(b : Bool) = fst(b)(Bool, true, false);

// and translate every proposition from
Π(b : Bool) -> T{b}
// to proving it for the eta case
Π(b : Bool) -> T{eta(b)}
CCω


T_Bool : □ = * -> * -> *;
C_Bool(t : T_Bool) : * =
  Π(C : T_Bool -> Type) -> C(t_true) -> C(t_false) -> C(t);

// then from
Π(b : Bool) -> T{b}
// you get
Π(t_b : T_Bool, c_b : C_Bool(t_b)) -> T{* := c_b | □ := t_b}


Type : Type;
Π : Π(Type)(λ(A) => Π(Π(A)(λ(a) => Type))(λ(B) => Type));


//

Type : Type;
// this is sugar to Π itself
Π : Π(a : A) -> (A -> Type) -> Type;

// then this is potentially FCU
_A(Π(a, b)) == N



//


Type : Type;
(->) : Type -> Type -> Type;

Level : Type;
∀ : (Level -> Type) -> Type;

Set : ∀(l : Level) -> Set(1 + l);
⇑ : ∀(l : Level) -> Set(l) -> Set(1 + l);
Π : ∀(l : Level) -> Π(A : Set(l)) -> (⇑A -> Set(l)) -> Set(l);


//
Type : Type;
(->) : Type -> Type -> Type;

Level : Type;


Type : Type;
∀ : ∀(A : Type) -> (A -> Type) -> Type;


//
ULevel : ULevel;
(->) : ULevel -> ULevel -> ULevel;
(∀) : (ULevel -> ULevel) -> ULevel;

Level : ULevel;
0 : Level
(1 + _) : Level -> Level;
max : Level -> Level -> Level;

Type(l : Level) : Type(1 + l);
Π{l, r}(A : Type(l), B(a : A) : Type(r)) : Type(max(l, r));

//
Kind : Kind;
(->) : Kind -> Kind -> Kind;

Type : Kind;
(∀) : ∀(A : Kind) -> (A -> Type) -> Type;
(Π) : ∀(A : Type) -> (A -> Type) -> Type;


id : ∀(A : Type) -> A -> A;


∀(A : -> Type)
Π(a : A) -> B


//
Type : Type;
(Π) : Π(A : Type) -> (A -> Type) -> Type;



```
