```rust
Unit : Type;
unit : Unit;

Unit = @self(u) -> (C : (u : P.Unit) -> Type, r : C(P.unit)) -> C(u);
unit = @fix(u) => (C, r) => r;

//
A : Type;
a : A;

A = @self(a) -> B{a};
a = @fix(a) => M{a};

out(a : A) : B{a};
fix(b : B{fix(b)}) : A;
//
A : Type;
a : A;

A = @self(a) -> B{a};
a = @fix(a) => M{a};

out(a : A) : B{a};
fix(b : B{fix(b)}) : A;

//
A : Type;
B(a : A) : Type;
f(p : [a : A, b : B(a)]) : [a : A, b : B(a)];

A = [
  p : [
    a : A,
    b : B(a)
  ],
  c : f(p) == p
];
B(a) = _;
f(p) = p.a.p;

spin(a : A) = [a.p.a.p, ap(f, a.c)];
spin_id(a) : spin(a) == a = _;

ext(l, r, H : l.a.p == r.a.p) : f(l) == f(r) = H;
ext_ap(p, q, H : p.a.p == q.a.p) : ap(f, l) == ap(f, r);

a.p.a.c : f(a.p.a.p) == a.p.a.p
ap(f, a.c) : f(a.p.a.p) == a.p.a.p


step(x, x_c : f(f(a.p)) == x) : f(f(x)) == f(x) =

(p : a == b) . (q : b == c) : a == c

coe(q, (x) => a == x, p)

coe(p, (z) => f(z) == z, q) == sym(ap(f, p)) . q . p

coe(p, (z) => f(a) == z, coe(p, (z) => f(z) == f(a), q))
coe(p, (z) => f(z) == f(a), q) . p
coe(ap(f, p), (z) => z == f(a), q) . p
sym(ap(f, p)) . q . p

sym(p) : y == x
sym p . q

coe(q, (x) => a == x, p)
coe(p, (z) => z == x, q)

coe(p, (z) => f(z) == f(a), q)
coe(ap(f, p), (z) => z == a, q)

sym(ap(f, p)) . q

coe(p, (z) => f(z) == f(a), q)

coe(p, (z) => f(z) == f(a), q) . p




coe(p, (z) => f(z) == z, q)

p : f(a) == a
q : f(f(a)) == f(a)
coe(p, (z) => f(z) == z, q)

coe(p, (z) => f(a) == z, coe(p, (z) => f(z) == f(a), q))

coe(p, (z) => f(z) == f(a), q) . p

coe(
  J(a.c),
  (x, x_c : f(f(a.p)) == x) =>

    (x.a.c : f(f(x)) == f(x)) ==
      ap(f, x_c).a.p : f(f(f(a.p)).a.p == x.a.p.a.p
    _ : coe(x_c, C_Eq(f), _ : f(f(a.p.a.p)) == f(a.p.a.p)) : f(x) == x,
  _
));

a.p.a.c.a.p == ap(f, a.c).a.p

mod(a : A) : a.p.a == spin(a) = [
  refl() : a.p.a.p == a.p.a.p,
  _ : a.p.a.c == ap(f, a.c)
];

mod(a : A) : a.p.a == spin(a) = [
  refl() : a.p.a.p == a.p.a.p,
  _ : a.p.a.c == ap(f, a.c)
];

mod(a : A) : a.p.a == spin(a) = [
  refl() : a.p.a.p == a.p.a.p,
  _ : a.p.a.c == ap(f, a.c)
];


f_I(p) : f(f(p)) == f(p) =
  coe(spin_id(p.a), (a) => f(f(p)) == a.p, refl());
f_J(p) : f_I(f(p)) == ap(f, f_I(p)) =
  _ : ;

coe(spin_id(f(p).a), (a) => f(f(f(p))) == a.p, refl()) ==
ap(f, coe(spin_id(p.a), (a) => f(f(p)) == a.p, refl()))


p.a.m : p.a.p.a.p == p.a.p



a : A;
a == [f(p), ap(f, p)];

mod(l : A, r) : l == r = [

]



coe(a.m, C_Eq(f), _ : )

a.p.p == a.p
a = @fix(a) => M{a};

out(a : A) : B{a};
fix(b : B{fix(b)}) : A;

//
Bool : Type;
I_Bool(b : Bool) : Type;
self(b : Bool, i : I_Bool(b)) : A;
true : Bool;
false : Bool;

Bool = [b : Bool, i : I_Bool(b), m : self(b, i) == b];
I_Bool(b) = (C, t : C(true), f : C(false)) -> C(b);
self(i) = i((b) => Bool, true, false);
true = [true, (C, t, f) => t, refl()];
false = [false, (C, t, f) => f, refl()];


//
A : Type;
a : A;

A = @self(a) -> B{a};
a = @fix(a) => M{a};

out(a : A) : B{a};
fix(b : B{fix(b)}) : A;

// strong fix
A : Type;
f(a : A) : A;
f(f(a)) ≡ f(a);

Fix : Type;
fix(a : A) : Fix;

Fix = @self(x) -> [a : A, k : fix(a) == x];
fix(a) = [f(a), refl()];

I_Fix(x : Fix) = [a : A, k : fix(a) == x];

coe(p, I_Fix, q) == [q.a, q.k . p]
p : fix(a) == fix(a);
p.@ : [f(a), p] == [f(a), refl()];

p.@ : [
  a : f(a) == f(a),
  k : coe(a, (b) => fix(b) == fix(f(a)), p) == refl()
];

p.@.k : sym(ap(fix, p.@.a)) . p == refl();

is_hset(a : A, p : fix(a) == fix(a)) : sym(ap(fix, p.@.a)) . p = refl()
  coe(
    J(p),
    ([x, k : fix(a) == x]) =>
      x.@.k == k : fix(x.@.a) == x,

  );

is_hset(a : A, p : fix(a) == fix(a)) : sym(ap(fix, p.@.a)) . p = refl()
  coe(
    J(p),
    ([x, k : fix(a) == x]) =>
      x.@.k == k : fix(x.@.a) == x,

  );

// strong fix
K_Eq(a : A) : Type;
k_refl(a) : K_Eq(f(a));
k_mod(a) : k_refl(f(a)) == k_refl(a);

K_Eq(a) = @self(k) -> [f(a), k_refl(a)] == [a, k];
k_refl(a) = [refl(), k_mod(a)];
k_mod(a) = refl();

k_ap{a}(k : K_Eq(a)) : K_Eq(f(a)) =
  coe(sym(k.@.p), K_Eq, k);
k_mod{a}(k) : k_refl(a) == k_ap(k) =
  coe(
    k.@,
    (c, k : K_Eq(c)) => k_refl(c) == k_ap(k),
    refl()
  );

// strong fix
K_Eq(a : A) : Type;
k_refl(a) : K_Eq(f(a));

K_Eq(a) = @self(k) -> [p = f(a), refl()] == [a, k.p];
k_refl(a) = [refl(), refl()];

k_ap{a}(k : K_Eq(a)) : K_Eq(f(a));
k_ap{a}(k) = ap(m_eta, k.@);

ap(g, k.@) : g([p = f(a), refl()]) == g([a, k.p]);

k_mod(a, k) : [f(a), k_ap(k)] == [a, k] = [
  k.@.p,
  _ : coe(k.@.p, K_Eq, k_ap(k)) == k
]
k_ap{a}(k : K_Eq(a)) : K_Eq(f(a));
k_ap{a}(k) = (i) => [
  f(k.@(i).p),
  ap(f, k.@(i).k)
  _ : f(f(k.@(i).p)) == f(k.@(i).p)
];

k.@(i).k : f(k.@(i).p) == k.@(i).p

[p = f(f(a)), refl()] == [f(a), k_ap(k).p]



(i) => [f(k.@(i).p), ap(f, k.@(i).k)]

ap(m_eta, k.@)

k_ap{a}(k : K_Eq(a)) : K_Eq(f(a)) =
  coe(sym(k.@.p), K_Eq, k);
k_mod{a}(k) : k_refl(a) == k_ap(k) =
  coe(
    J(k.@),
    (x : Fix, ) =>

      ,
    refl()
  );


k_from(x, r : [f(a), f_I(a)] == x) : K_Eq(a) = [
  coe(r.p, C_Eq(f), f_I(a)) : f(x.p) == x.p,
  ap(c_f(f_I,))

  r.q : coe(r.p, C_Eq(f), f_I(a)) == x.p
  r.q
  _ : coe(coe(r.p, C_Eq(f), f_I(a)), C_Eq(f), f_I(a)) == k_from(x, r)

  _ : coe(r.p, C_Eq(f), f_I(a)) == coe(r.p, C_Eq(f), refl())
];
k.@ : [f(a), f_I(a)] == [a, k.p];

k.@.k : coe(k.@.p, K_Eq, k_refl(a)) == k
k.@.k.@.p : coe(k.@.p, C_Eq(f), refl()) == k.@.p

mod(k : K_Eq(a)) : [f(a), k_refl(a)] == [a, k] = k.@;

mod(k : K_Eq(a)) : refl() = ap(f, k.@.p) =


// still strong fix
K_Eq(a : A) : Type;
k_refl(a) : K_Eq(f(a));

K_Eq(a) = @self(k) -> [
  p : f(a) == a,
  q : sym(ap(f, k.@.p)) . k.@.p == p
];
k_refl(a) = [refl(), refl()];

k_next(k : K_Eq(a)) : K_Eq(a);
k_next(k) = [sym(ap(f, k.@.p)) . k.@.p, refl()];

[
  sym(ap(f, k.@.p)) . k.@.p,
  k_mod(k)
] == k.@
[
  p : sym(ap(f, k.@.p)) . k.@.p == k.@.p,
  q : coe(p, _, k_mod(k)) == k.@.q
];
k.@.q : sym(ap(f, k.@.p)) . k.@.p == k.@.p

k_mod(k : K_Eq(a)) : k_next(k) == k = [
  k.@.q
]



mod(k : K_Eq(a)) : [
  coe(k.@.p, C_Eq(f), refl()),
  refl()
] == k.@ = [k.@.q, refl()];

k_ap(k : K_Eq(a)) : K_Eq(a) =
  coe(sym(k.@.p), K_Eq, k);
k_mod(k : K_Eq(a)) : refl() == k_ap(k).@.p =

coe()

k.@ : [
  p : f(a) == a,
  q : coe(k.@.p, C_Eq(f), refl()) == p
];

k.@ == [
  coe(k.@.p, C_Eq(f), refl()),
  refl()
]

k.@.q : coe(k.@.p, C_Eq(f), refl()) == k.p

mod(k : K_Eq(a)) : [f(a), k_refl(a)] == [a, k] =
  k.@ : [f(a), refl()] == [a, k.p]

mod(a, p, q) : [f(a), [c_f(p), ap(c_f, q)]] == [refl(), refl()] =

c_f(p : f(a) == a) = sym(ap(f, p)) . p;

L(a) = [
  p : f(a) == a,
  q : c_f(p) == p,
];




mod(p, q) : [ap(f, p), q] == [refl(), refl()] =
  [
    sym(q),
    _ : sym(ap(f, q)) . q == refl()
    sym(ap(f, q)) : ap(f, ap(f, p)) == ap(f, p)

    coe(sym(q), (p) => refl() == ap(f, p), q) == refl
    coe(ap(f, q), (p) => refl() == q, q) == refl(),
  ]

K_Eq(a)
k.@ : [
  p : f(a) == a,
  k : coe(p, K_Eq, k_refl(a)) == k
];

k.@.p : f(a) == a,
k.@.k : coe(k.@.p, K_Eq, k_refl(a)) == k

h_set(a : A, k : K_Eq(f(a))) : refl() == k.@.p_I =
  k.@.p_J


(c, k) =>
  k.@ : [f(a), k_refl(a)] == [c, k]




// strong fix
A : Type;
f(a : A) : A;
f(f(a)) ≡ f(a);

K_Eq(a : A) : Type;
k_refl(a) : K_Eq(f(a));

K_Eq(a) = @self(k) -> [
  p : f(a) == a,
  k : coe((i) => K_Eq(p(i)), k_refl(a)) == k
];
k_refl(a) = [refl(), refl()];

k_ap{a}(k : K_Eq(a)) : K_Eq(f(a)) =
  coe((i) => K_Eq(k.@.p(i)), k);

coe((i) => @self(x). T(x, i), y : @self(x). T(x, 0)) : @self(x). T(x, 1)

coe((i) => [a : A(i), B(i, a)], p) == [
  coe(A, p.a),
  coe((i) => B(i, coe(A, 0, i, p.a)), p)
];
coe((i) => @self(x). T(x, i), y)

coe(p, (y) => @self(x). T(x, y), z)

@fix(x) => coe((i) => T(x, i), y.@ : T(y, 0)) : T(x, 1)

k_ap{a}(k : K_Eq(a)) : K_Eq(f(a)) = [
  (i) => f(k.@.p(i)),


  _ : coe((i) => K_Eq(f(k.@.p(i))), k_refl(a)) == k_ap(k)
]

k.@.q : coe((i) => K_Eq(k.@.p(i)), k_refl(a)) == k

coe(
  (i) => k.@.q
)

coe(
  (i) =>
    @self(k) -> [
      p : f(l(i)) == l(i),
      k : coe((i) => K_Eq(p(i)), k_refl(l(i))) == k
    ],
  r
)

@fix(k) =>
  coe(
    (i) =>
      @self(k) -> [
        p : f(l(i)) == l(i),
        k : coe((i) => K_Eq(p(i)), k_refl(l(i))) == k
      ],
    r
  )

coe((i) => K_Eq(q(i)), k)


k_trunct{a}(k : K_Eq(a)) : k_refl(a) == k_ap(k) =
  _;

k.@ : [
  p : f(a) == a,
  k : coe(p, K_Eq, k_refl(a)) == k
];
k.@.k.@.p : coe(k.@.p, C_Eq(f), refl()) == k.@.p

coe(p, K_Eq, q).@ == [
  coe(p, C_Eq(f), q.@.p),
  _ : coe(coe(p, C_Eq(f), q.@.p), K_Eq, k_refl(a)) == coe(p, K_Eq, q)
];
coe(p : a == b, K_Eq, k).@ == [
  coe(p, C_Eq(f), k.@.p),
  coe(
    J(p),
    ([c, q : a == c]) =>
      coe(
        coe(q, C_Eq(f), k.@.p),
        K_Eq,
        k_refl(c)
      ) == coe(q, K_Eq, k),
    k.@.k
  )
];



K_Eq(a) = [
  c_I : f(a) == a,
  c_J : coe(c_I, C_Eq(f), f(a)) == c_I];
k_refl(a) = [refl(), refl()];

k_merge(a, k : K_Eq(a)) : [f(a), f_I(a)] == [a, k.c_I] = [c_I, c_J];
k_merge_inj(
  a, l_J : C_Eq(c_f(f_I), f_I(a)), r_J,
  H : k_merge(a, [f_I(a), l_J]) == k_merge(a, [f_I(a), r_J])
) : l_J == r_J =
  _;

H.c_I : f_I
H : [
  c_I : f_I(a) == f_I(a),
  c_J : coe(c_I, C_Eq(c_f(f_I)), l_J) == r_J
];
H.c_J : coe(H.c_I, C_Eq(c_f(f_I)), l_J) == r_J


// strong fix


S_Eq(a : A) : Type;
s_refl(a) : S_Eq(f(a));

S_Eq(a) = @self(s) -> ([f(a), refl()] : M_Fix(f)) == [a, s.@.a];
s_refl(a) = refl();

K_Eq(a : A) : Type;
k_refl(a) : K_Eq(f(a));

K_Eq(a) = @self(k) -> [f(a), k_refl(a)] == [a, k];
k_refl(a) = [refl(), refl()];


s_to_k{a}(s : S_Eq(a)) : K_Eq(a) =
  coe(s.@.a, K_Eq, k_refl(a));

s_to_k{a}(s) = [
  s.@.a,
  _ : coe(s.@.a, K_Eq, k_refl()) == s_to_k{a}(s)
]
 = [s.@.a, s.@.p];

s.@ : [f(a), refl()] == [a, s.@.a];

ap(m_eta, s.@)
coe(s.@, (m) => [f(f(a)), refl()] == [f(m.a), ap(f. m.p)], refl())
coe(s.@,
  (m) => [
    p_I : f(f(a)) == f(m.a),
    p_J : coe(p_I, C_Eq(f), refl()) == ap(f, m.p)
  ], refl()
)
[
  p_I : f(f(a)) == f(a),
  p_J : coe(p_I, C_Eq(f), refl()) == ap(f, s.@.a)
]
[
  ap(f, s.@.a),
  _ : coe(ap(f, s.@.a), C_Eq(f), refl()) == ap(f. s.@.a)
]

S_Eq(a : A) : Type;
s_refl(a) : S_Eq(f(a));

S_Eq(a) = @self(s) -> [
  p_I : f(a) == a,
  p_J : sym(ap(f, p_I)) @ p_I == s.@.a
];
s_refl(a) = [refl(), refl()];

s_ap{a}(s : S_Eq(a), g) : S_Eq(g([a, s.@.a]).a)
s_ap{a}(s) = [
  _ :
];
s_ap{a}(s : S_Eq(a), g) : S_Eq(g([a, s.@.a]).a)
s_ap{a}(s, g) = [
  _ :
];

(p : _) -> g([a, p]).a == f(a)

g([f(a), refl()]) == g([a, s.@.a])

[f(g([a, s.@.a]).a), refl()] ==
[g([a, s.@.a]).a, s_ap{a}(s, g).@.a]





([f(a), refl()] : M_Fix(f)) == [a, s.@.a]

s_ap{a}(s : S_Eq(a)) : S_Eq(f(a))
s_ap{a}(s) = ap(m_eta, s.@);


// equivalent to
K_Eq(a : A) : Type = [
  p_I : f(a) == a,
  p_J : coe(p_I, C_Eq(f), refl()) == p_I
];
k_refl(a) : K_Eq(f(a)) = [refl(), refl()];




[
  p_I : g([f(a), refl()]).a == g([a, s.@.a]).a,
  p_J : coe(p_I, C_Eq(g), refl()) == p
]
s_ap{a}(s : S_Eq(a)) : S_Eq(f(a));
s_ap{a}(s) = ap(m_eta, s.@);

([f(f(a)), refl()] : M_Fix(f)) == [f(a), s_ap(s).@.a]

ap_m_eta_id(p) : ap(m_eta, p) == p

s_ap_refl(s : S_Eq(f(a))) : s_ap(s) == s;
s_ap_refl(s) = (i) => ap_m_eta_id(s.@)(i);

K_Eq(a : A) : Type = [
  p_I : f(a) == a,
  p_J : coe(p_I, C_Eq(f), refl()) == p_I
];
k_refl(a) : K_Eq(f(a)) = [refl(), refl()];

k_ap{a}(k : K_Eq(a)) : K_Eq(f(a)) = [
  ap(f, k.p_I),
  ap(ap(f), k.p_J),
];
k_ap_refl{a}(k : K_Eq(f(a))) : k_ap(k) == k =
  (i) => [
    f(k.p_I(i)),
    _ : coe(f(k.p_I(i)), C_Eq(f), refl()) == f(k.p_I(i))
    _ : f(f(a)) == f(a)
  ];

s_to_k{a}(s : S_Eq(a)) : K_Eq(a) = [s.@.a, s.@.p];

k_to_s{a}(k : K_Eq(a)) : S_Eq(a);
k_to_s{a}(k) = ([k.p_I, k.p_J] : [
  a : f(a) == a,
  p : coe(a, C_Eq(f), refl()) == k_to_s(k).a
]);



ap_m_eta_id(p) : ap(m_eta, p) == p

s_ap_refl(s : S_Eq(f(a))) : s_ap(s) == s;
s_ap_refl(s) = (i) => ap_m_eta_id(s.@)(i)

s_ap{a}(s : S_Eq(a)) : S_Eq(f(a));
s_ap{a}(s) = ap(m_eta, s.@);

s.@ : [f(a), refl()] == [a, s.@.a];

k : K_Eq(a);
ap(m_eta, k_to_s(k).@) : [
  p_I : f(f(a)) == f(a),
  p_J : coe(p_I, C_Eq(f), refl()) == ap(f, s.@.a)
]

[
  ap(f, k_to_s(k).@.a),
  ap(ap(f), k_to_s(k).@.p)
]

ap(m_eta, k_to_s(k).@)

ap(m_eta, s.@) : [f(f(a)), refl()] == [f(a), ap(f, s.@.a)]

ap(m_eta, s.@) : [
  p_I : f(f(a)) == f(a),
  p_J : coe(p_I, C_Eq(f), refl()) == ap(f, s.@.a)
]

s.@.p : coe(s.@.a, C_Eq(f), refl()) == s.@.a

[
  p_I : f(f(a)) == f(a),
  p_J : coe(p_I, C_Eq(f), refl()) == ap(f, s.@.a)
]

[
  ap(f, s.@.a),
  _ : coe(ap(f, s.@.a), C_Eq(f), refl()) == ap(f, s.@.a)
]

s.@.p : sym(ap(f, s.@.a)) . s.@.a == s.@.a

ap(f, sym(ap(f, s.@.a)) . s.@.a) == ap(f, s.@.a)
ap(f, sym(ap(f, s.@.a))) . ap(f, s.@.a) == ap(f, s.@.a)
ap(ap(f), s.@.p) :


sym(ap(f, ap(f, s.@.a))) . ap(f, s.@.a)

sym(ap(f, p)) . p
coe(p, C_Eq(f), refl())



(i) => [f(s.@(i).a), ap(f, s.@(i).p)];

[
  ap(f, s.@.a),
  ap(m_eta, s.@).p_J
] : [
  p_I : f(f(a)) == f(a),
  p_J : coe(p_I, C_Eq(f), refl()) == p_I
]
[
  ap
] : K_Eq(f(a))

s.@ : [
  a : f(a) == a,
  p : coe(a, C_Eq(f), refl()) == s.@.a
];

k_ap_refl(k : K_Eq(f(a))) : s_ap(k_to_s(k)) == k_to_s(k) = [

];

k_ap_refl(k : K_Eq(f(a))) : s_ap(k_to_s(k)) == k_to_s(k) = s_ap_refl(k_to_s(k));

s.@ : [
  a : f(a) == a,
  p : coe(a, C_Eq(f), refl()) == s.@.a
];

R_Eq(a)

k.@ : [f(a), refl()] == [a, k.@.a];

J(k.@) : [[f(a), refl()], refl()] == [[a, k.@.a], k.@]

c_f(p_I) = sym(ap(f, p_I)) . p_I;
pin(i, p_I) = (j) => p_I(min(i, j));

K_Eq(a : A) : Type = [
  p_I : f(a) == a,
  p_J : c_f(p_I) == p_I,
];
k_refl(a) : K_Eq(f(a)) = [refl(), refl()];

k_ap{a}(k : K_Eq(a)) : K_Eq(f(a)) = [
  ap(f, k.p_I),
  ap(ap(f), k.p_J),
];

k_ap_refl{a}(k : K_Eq(a)) : Path((i) => K_Eq(k.p_I(i)), k_ap(k), k) =
  (i) => [
    sym(ap(f, pin(i, k.p_I))) . ap(f, k.p_I) . pin(i, k.p_I) : f(k.p_I(i)) == k.p_I(i),
    _ :
  ];

// left
sym(ap(f, pin(i, k.p_I))) . pin(i, k.p_I)


pin(i, p_I) = (j) => p_I(min(i, j));

pi(i, p . q)

(j) => (p . q)(min(i, j))

(j) => (p . q)(min(i, j))


(j) => p(min(i, j)) . (j) => p(min(i, q))

sym(ap(f, pin(i, sym(ap(f, k.p_I)) . k.p_I)))
  . pin(i, sym(ap(f, k.p_I)) . k.p_I)


(h) => sym(ap(f, pin(i, k.p_J(h)))) . ap(f, k.p_J(h)) . pin(i, k.p_J(h))

coe(
  (h) => refl() == ap(f, k.p_J(min(i, h))),
  _ :
)

refl() == ap(f, ap(f, k.p_I))

refl() == ap(f, ap(f, k.p_I))
refl() == ap(f, sym(ap(f, pin(i, k.p_I))) . ap(f, k.p_I) . pin(i, k.p_I))

ap(f, sym(ap(f, pin(i, k.p_I)))) . ap(f, ap(f, k.p_I)) . ap(f, pin(i, k.p_I))


// left
p : x == y
q : y == z

sym(p . q) : z == x

sym(ap(f, pin(i, k.p_I))) . pin(i, k.p_I)= sym(ap(f, pin(i, k.p_I))) . ap(f, k.p_I) . pin(i, k.p_I)

coe(
  (j) => f(k.p_I(min(i, j))) == k.p_I(min(i, j)),
  ap(f, k.p_I)
);


sym(ap(f, p_I)) . p_I

sym(ap(f, (j) => f(a))) . (j) => f(a)
c_f(pin(i, k.p_I))

(h) => c_f(pin(i, k.p_J(h)))

c_f(c_f(pin(i, k.p_I))) == c_f(pin(i, c_f(k.p_I)))

(h) => c_f(pin(i, k.p_J(h)))

c_f(pin(i, c_f(k.p_I)))

sym(ap(f, c_f((j) => k.p_I(min(i, j))))) . c_f((j) => k.p_I(min(i, j)))

(h) => c_f(pin(k.p_J(h)))

sym(ap(f, p_I)) . p_I



sym(ap(f, ((j) => (sym(ap(f, k.p_I)) . k.p_I)(min(i, j))))) .
  ((j) => (sym(ap(f, k.p_I)) . k.p_I)(min(i, j)))

coe(
  (j) => f(k.p_I(min(i, j))) == k.p_I(min(i, j)),
  ap(f, k.p_I)
)

((j) => k.p_I(min(i, j))) : f(a) == k.p_I(i)

f(k.p_I(i)) == f(f(a))

sym(ap(f, (j) => k.p_I(min(i, j)))) . ((j) => k.p_I(min(i, j)))



sym(ap(f, k.p_I)) . k.p_I

coe(
  (j) => f(k.p_J(h)(min(i, j))) == k.p_J(h)(min(i, j)),
  ap(f, k.p_J(0))
)


coe((i) => f(k.p_I(i)) == k.p_I(i), refl())

coe(
  (j) => f(k.p_I(min(i, j))) == k.p_I(min(i, j)),
  ap(f, k.p_I)
)
coe((i) => f(p_I(i)) == p_I(i), refl()) == p_I

coe
k.p_J : coe((i) => f(p_I(i)) == p_I(i), refl()) == p_I

K_Eq(k.p_I(i))
coe(
  (j) => f(k.p_I(min(i, j))) == k.p_I(min(i, j)),
  ap(f, k.p_I)
) : f(k.p_I(i)) == k.p_I(i)
(i) => [
  (j) => f(k.p_I(1)),
  _
]
f(k.p_I(i)) == f(a)

(i) => [
    (j) => f(k.p_I(max(i, j))),
    _
  ]
(j) => f(k.p_I(i))


Γ, x : @self(x). T{x} |- T{x} ⇐ Type
------------------------------------
Γ |- @self(x). T{x} ⇐ Type

Fix = self(x). [a : A, p : f(a) == x.unfold.a];
fix(a) = fold([f(a), refl()]);

ext(
  a : self(x). T{x},
  b : self(x). T{x},
  p : self(p). coe(ext(a, b, p), T, a.unfold) == b.unfold
) : a == b

fix(out(x)).unfold : [a : A, p : f(a) == f(x.unfold.a)]
mod(x) : fix(out(x)) == x = ext(
  fix(out(x)), x,
  [
    x.unfold.p,
    _ : coe(x.unfold.p, (a) => f(a) == f(x.unfold.a), refl()) == x.unfold.p
  ]

  x.unfold.p : f()
  _ : self(p). coe(ext(a, b, p), T, [f(x.unfold.a), refl()]) == x.unfold
)

[
  f(x.unfold.a),
  ap(.unfold.a, ext(a, b, p))
] == [x.unfold.a, x.unfold.p]
coe(ext(a, b, p), T, [f(x.unfold.a), refl()]) == x.unfold

mod(x) : fix(out(x)) == x = [
  unfold(x).p,

  coe(unfold(x).p, _, refl()) == unfold(x).p
  _ : f(out(x)) == out(x),
  _ : _
]
x : Fix;
unfold(x) : [a : A, p : f(a) == unfold(x).a]

unfold(x).p : f(unfold(x).a) == unfold(x).a



[f(a), refl()]
[
  f(a),
  refl() : f(f(a)) == f(a)
]

Γ |- M ⇐ B(x)
-------------------------------------
Γ |- fold(M) ⇐ @self(x). B(x)

Unit : Type;
I_Unit(u : Unit) : Type;
unit : Unit;
i_unit : I_Unit(unit);


Unit = @self(u). I_Unit(u);
I_Unit(u) = (C, r : C(unit)) -> C(u);
unit = fold(i_unit);

//
K_Eq(a : A) : Type;
k_refl(a) : K_Eq(f(a));
k_ap{a}(k : K_Eq(a)) : K_Eq(f(a));
k_mod(a) : k_refl(f(a)) == k_ap(k_refl(a));

K_Eq(a) = @self(k) -> [p : f(a) == a, k : k_refl(a) == k_ap(k)];
k_refl(a) = [refl(), k_mod(a)];
k_ap{a}(k) = [ap(f, k.@.p), k_mod(a) @ ap(k_ap, k.@.k)];
k_mod(a) = refl();




[
  ap(f, refl()),
  k_mod(a) @ ap(k_ap, k_mod(a))
]
[refl(), k_mod(a) @ ap(k_ap, k_mod(a))]
coe(k_mod(a), (k) => _, k_refl(f(a)).@) == k_ap(k_refl(a)).@




eq : Eq{A}(x, y)

eq.@(0) : [
  a : A,
  p : case(0, x, y, eq) == a
]


K_Eq(a : A) : Type;
k_refl(a) : K_Eq(f(a));

K_Eq(a) = @self(k) -> (i : I) ->
  case(
    i,
    [f(a), k_refl()],
    [a, k],
    _ :
  );

case(
  i,
  [f(a), refl()],
  a,
  _ : mod()
)
x : A;
y : B{x};

x = M{x, y};
y = N{x, y};

A : Type;
B(a : A) : Type;

A = @self(a) -> (b : B(a)) -> A;
B(a) = @self(b) -> B(a.unfold(b));

T_y : Type;
x(y : T_y) : A;

T_y = @self(y) -> B{x(y)};
x(y : B{x(y)}) : A;



Bool : Type;
case(b : Bool) : (C, t : C, f : C) -> C;

Bool = @self(b) -> [
  c : (C, t : C, f : C) -> C,
  p : case(b) == c
];
// no matter the case defintion, the following is always true
mod(b : Bool) : b.@ == [case(b), refl()]
// because the body is contractible
b.@ : [c : (C, t : C, f : C) -> C, p : case(b) == c]


T : Type;
out(x : T) : A;

T = @self(x) -> [
  a : A,
  p : out(x) == a
];

K_Eq(a : A) : Type;
k_out{a}(k : K_Eq(a)) : f(a) == a;

K_Eq(a) = @self(k) -> [p : f(a) == a, k : k_out(k) == p];
k_out{a}(k) = sym(ap(f, k.@.p)) @ k.@.p;

k_refl(a) : K_Eq(f(a));
k_refl_k(a) : k_refl(a).@.p == refl();

k_refl(a) = [refl(), k_refl_k(a)];
k_refl_k(a) = refl();

//
k_ap{a}(k : K_Eq(a)) : K_Eq(f(a)) = coe(sym(k.@.p), K_Eq, k);
k_mod_ap{a}(k : K_Eq(a)) : [f(a), k_ap(a)] == [a, k];
k_mod_ap{a}(k) = [k.@.p, refl()];

k_f{a}(k : K_Eq(a)) : K_Eq(a) = coe(k_out(k), K_Eq, k_refl(a));
k_mod_f{a}(k : K_Eq(a)) : k_f(k) == k;
k_mod_f{a}(k) =
  coe(mod_center(k.@), (k) => _ == k)
  [
  k.@.k,
  _ : coe(k.@.k, (p) => k_out(k_mod_f(a)) == p, sym(ap(k_out, k_mod_f(f)))) ==
    k.@.k
];

mod_center(k.@) : [sym(ap(f, k.@.p)) @ k.@.p, refl()] == k.@

[
  sym(ap(f, k.@.p)) @ k.@.p,
  sym(ap(k_out, k_mod_f(f)))
] == [sym(ap(f, k.@.p)) @ k.@.p, refl()]


refl()

[
  sym(ap(f, k.@.p)) @ k.@.p,
  sym(ap(k_out, k_mod_f(f)))
] == k.@

k_f(k).@

k.@.k : sym(ap(f, k.@.p)) @ k.@.p == k.@.p



mod_center(k.@) : k.@ == [sym(ap(f, k.@.p)) @ k.@.p, refl()] =
k.@

k.@ == [sym(ap(f, k.@.p)) @ k.@.p, refl()]

sym(ap(k_out, k_mod(f)))

k_out(k) == coe()

coe(k_mod_f(k), _, k_f(k).@)
coe(k_mod_f(k), _, k_f(k).@) == k.@

k_f(k).@ : [
  p : f(a) == a,
  k : k_out(k_spin(k)) == p
]
k_f(k).@ == [
  sym(ap(f, k.@.p)) @ k.@.p,
  refl()
];



[
  sym(ap(f, k.@.p)) @ k.@.p,
  refl()
] ==

k_mod{a}(k : K_Eq(a)) : k_spin(k) == k;
k_mod{a}(k) = [

];
coe(k_mod(k), _, coe(k_out(k), K_Eq, k_refl(a)).@)
coe(k_mod(k), _, coe(k_out(k), K_Eq, k_refl(a)).@) == [a, k]
k_mod{a}(k : K_Eq(a)) : [f(a), k_refl(a)] == [a, k];
k_mod{a}(k) = [
  k_out(k),
  _ : coe(k_out(k), K_Eq, k_refl(a)) == k
];


coe(k_out(k), K_Eq, k_refl(a))

[
  coe(k_out(k), C_Eq(f), refl()),
  _ : _
]

coe(k.@.p, K_Eq, k_refl(a)) == k

k.@ : [
  p : f(a) == a,
  k : k_out(k) == p
]

k.@ == [k_out(k), refl()]





Fix : Type;
out(x : Fix) : A;

Fix = @self(x) -> [a : A, p : out(x) == a];
out(x) = f(x.@.a);

spin(x : Fix) : Fix;
spin_p(x : Fix) : out(spin(x)) == out(x);

spin(x) = [out(x), spin_p(x)];
spin_p(x) = refl();

mod(x) : spin(x) == x;
mod_p(x, i : I) : out(mod(x)(i)) == x.@.p(i)

mod(x) = (i) => [x.@.p(i), mod_p(x, i)];
mod_p(x, i) =
  coe(
    (j) => f(x.@.p(min(i, j))) == x.@.p(min(i, j)),
    ap(f, x.@.p),
  );

mod_fix{b}(a : A, p : f(a) == a) : [f(a), ap(f, p)] == [a, p] = (i) => [
  p(i),
  _ : f(p(i)) == p(i),
];



mod(x) : spin(x) == x;
mod(x) = [
  x.@.p,
  _ : coe(x.@.p, (a) => out(mod(x)) == a, sym(ap(out, mod(x)))) == x.@.p
];

[
  out(x),
  sym(ap(out, mod(x)))
] == [x.@.a, x.@.p]


mod(x) : spin(x) == x;
mod_p(x, i : I) : out(mod(x)(i)) == x.@.p(i)

mod(x) = (i) => [x.@.p(i), mod_p(x, i)];
mod_p(x, i) = _;

coe(
  (j) => _,

)
f(x.@.p(i)) == x.@.p(i)


out(spin(x)) == out(x)

out(mod(x)(i)) == x.@.p(i)

x.@.p(i) : out(x) == x.@.a
f(mod(x)(i).@.a)

[
  a : A,
  p : out(mod(x)(i)) == a
]


mod(x) : spin(x) == x;
mod(x) = [
  x.@.p,
  _ : coe(x.@.p, (a) => out(x) == a, sym(ap(out, mod(x))))
];

sym(ap(out, mod(x))) : out(spin(x)) == out(x)

coe(x.@.p, (a) => out(x) == a, sym(ap(out, mod(x))))

[
  out(x),
  sym(ap(out, mod(x)))
] == [x.@.a, x.@.p]

coe(p : x == y, (z) => z == w, q : x == w) : y == w

sym(p) @ q

mod(|)

coe(mod(x), _, spin(x).@) == x.@



x.@ : [a : A, p : out(x) == a]
x.@ == [out(x), refl()]


K_Eq(a : A) : Type;
k_out{a}(k : K_Eq(a)) : f(a) == a;

K_Eq(a) = @self(k) -> [p : f(a) == a, k : k_out(k) == p];
k_out{a}(k) = sym(ap(f, k.@.p)) @ k.@.p;

k_refl(a) : K_Eq(f(a));
k_refl_k(a) : k_out(k_refl(a)) == refl();

k_refl(a) = [refl(), k_refl_k(a)];
k_refl_k(a) = refl();

k_ap{a}(k : K_Eq(a)) : K_Eq(f(a)) = coe(sym(k.@.p), K_Eq, k);

k.@.k : sym(ap(f, k.@.p)) @ k.@.p == k.@.p

sym(ap(f, ap(f, k.@.p))) @ ap(f, k.@.p)

k_out() == ap(f, k.@.p)

coe(k_mod(k), _, k_refl(a).@) == k_ap(k).@

k_spin{a}(k : K_Eq(a)) : K_Eq(a) = coe(k.@.p, K_Eq, k);

k_spin_k{a}(k) : k_out(k_spin(k)) == sym(ap(f, k.@.p)) @ k.@.p;


k.@ : [
  p : f(a) == a,
  k : k.@.a == p
]


//
k_mod{a}(k : K_Eq(a)) : k_spin(k) == k;
k_mod_k{a}(k, i) : k_out(k_mod(k)(i)) == k.@.k(i);

k_mod{a}(k) = (i) => [x.@.k(i), k_mod_k(k, i)];
k_mod_k{a}(k, i) = _ ;

[
  sym(ap(f, k.@.p)) @ k.@.p,
  sym(ap(k_out, k_mod(k))),
] == k.@

mod_center(k.@) : []

sym(ap(f, k.@.k(i))) @ k.@.k == k.@.k(i)

k_mod{a}(k : K_Eq(a)) : k_spin(k) == k;
k_mod_k{a}(k) : sym(ap(k_out, k_mod(k))) @ k.@.k == k.@.k;

k_mod{a}(k) = [k.@.k, k_mod_k(k)];
k_mod_k{a}(k) = _



sym(ap((k) => sym(ap(f, k.@.p)) @ k.@.p, k_mod(k)))

sym(ap(k_out, k_mod(k))) @ k.@.k == k.@.k




sym(ap(ap(f), k.@.k)) @ k.@.k @ k.@.k == k.@.k

sym(ap(k_out, k_mod(k))) @ k.@.k == k.@.k

sym(ap(f, k.@.k)) @ k.@.k == k.@.k



coe(k_mod(k), _, k_spin(k).@) == k.@



k.@ : [
  p : f(a) == a,
  k : k_out(k) == p
]


//
Fix : Type;
out(x : Fix(f)) : A;

Fix = @self(x) -> [a : A, p : out(x) == a];
out(x) = f(x.@.a);

fix(a, p : f(a) == a) : Fix;
fix_p(a, p) : out(fix(a, p)) == a;

fix(a, p) = [a, fix_p(a, p)];
fix_p(a, p) = p;


x.@ == [f(x.@.a), refl()]

mod(a, p) : fix(f(a), refl()) == fix(f(a), ap(f, x.p));
mod(a, p) = [
  refl(),
  _ : coe
]
mod_k(x, i) : out(mod(x)(i)) == x.@.p(i);
// f == identity
mod(x : Fix) : fix(out(x), refl()) == x;
mod_k(x, i) : out(mod(x)(i)) == x.@.p(i);

mod(x) = (i) => [x.@.p(i), mod_k(x, i)];
mod_k(x, i) =
  coe(
    (j) => f(x.@.p(j)) == x.@.p(j),
    ap(f, x.@.p)
  );

ap(f, x.@.p)

coe(
  (j) => f(x.@.p(min(i, j))) == x.@.p(min(i, j)),
  ap(f, x.@.p)
);
f(x.@.p(i)) == x.@.p(i)

//
Fix : Type;
Fix = @self(x) -> [a : A, p : f(x.@.a) == a];

x == @fix(y) -> [x.@.a, x.@.p];
x == @fix(y) -> [out(x), refl()];

spin(x : Fix) : Fix;
spin_p(x) : out(spin(x)) == out(x);

spin(x) = [out(x), spin_p(x)];
spin_p(x) = ap(f, x.@.p);

out(x : Fix(f)) : A;

Fix = @self(x) -> [a : A, p : out(x) == a];
out(x) = f(x.@.a);

spin(x : Fix) : Fix;
spin_p(x) : out(spin(x)) == out(x);

spin(x) = [out(x), spin_p(x)];
spin_p(x) = ap(f, x.@.p);

x.@ : [a : A, p : out(x) == a]
x.@.p : out(x) == x.@.a
f(out(x)) == out(x)
//
x.@ : [a : A, p : out(x) == a]
x.@ == [out(x), refl()]


Fix : Type;
Fix = @self(x) -> [a : A, p : f(x.@.a) == a];

fix(a) : Fix = fold(([f(a), refl()] : [b : A, p : f(f(a)) == b]));


// strong fix
center(s.unfold)(i).unfold == s.unfold
mod(s) = () => [
  s,
  center(s.unfold)(i),
  mod_k(s, i)
];
f(s.unfold.p(i)) == s.unfold.p(i)
mod_p(s, i) =
ext(l : Fix, r : Fix, H) : l == r;
ext(l, r) = (i) => [
  exit(l, r)(i) : Fix,
  [

  ]
  _ : [a : A, p : f(exit(l, r)(i).unfold.a) == a]
]

contr(s : Fix) : [f(s.x.unfold.a), refl()] == s.unfold;

ext(l : Fix, r : Fix) : l == r;
ext(l, r) = [
  ext(l, r) : l.x == r.x,
  _ : coe(ext(l, r), (s) => [a : A, p : f(s.x.unfold.a) == a], l.unfold) ==
    r.unfold,
  _
]
[
  l.unfold.a,
  coe(ap((s) => f(s.x.unfold.a), ext(l, r), (s) => f(s.x.unfold.a) == a, l.unfold.p),

  _,
]
coe(ext(l, r), (s) => [a : A, p : f(s.x.unfold.a) == a], l.unfold)

[
  p_x : fix(s.unfold.a).x == s.x,
  p_unfold :
    [
      f(s.unfold.a),
      sym(ap((x) => f(x.unfold.a), p_x))
    ] == s.unfold,
  _,
]

[
  p_a : f(s.unfold.a) == s.unfold.a,
  p_p : sym(ap((x) => f(x.unfold.a), mod(s))) @ p_a == s.unfold.p,
]

sym(ap((x) => f(x.unfold.a), mod(s))) @ p_a == s.unfold.p

s.unfold.p
mod(s) : fix(s.unfold.a) == s;
mod(s) = [
  mod(s),
  [
    s.unfold.p
  ]
  _,
];

[
  fix(a),
  [f(a), refl()],
  s_refl()
]


ext(
  l : @self(x). T{x},
  r : @self(x). T{x},
  p : @self(p). coe(ext(l, r, p), T, l.unfold) == r.unfold
) -> l == r;


x == @fix(y) -> [x.@.a, x.@.p];
x == @fix(y) -> [x.@.a, refl()];


//

Fix : Type;
Fix = [
  x : Fix,
  y : [a : A, p : f(x.y.a) == a],
];

self(s : Fix) : Fix;
self_p(s : Fix) : f(self(s).y.a) == f(s.y.a);

self(s) = [self(s), [f(s.y.a), mod_p(s)]];
self_p(s) = refl();

mod(s : Fix) : self(s) == s;
mod_p(s : Fix, i) : f(mod(s)(i).y.a) == s.y.p(i);

mod(s) = (i) => [mod(s)(i), [s.y.p(i), mod_p(s, i)]];
mod_p(s, i) =

f(s.y.p(i)) == s.y.p(i)

f(s.x.y.p(i)) == s.y.p(i)
s.y == [f(s.x.y.a), refl()]

contr{A, y : A}(c : [x : A, p : y == x]) : [y, refl()] == c = (i) => [
  c.p(i),
  coe(
    (j) => y == c.p(min(i, j)),
    refl()
  )
];

center_y(s : Fix) = [
  s.x,
  [s.x.y.z, refl()]
];
mod_center_y(s) : center_y(s) == s = (i) => [
  s.x,
  center(s.y)(i)
];


center_z(s : Fix) = [
  s.x,
  [
    [f(s.x.y.z.a), refl()],
    sym(contr(s.x.y.z))
  ]
];
mod_center_z(s) : center_z(s) == s;



fix(a) : Fix;
fix(a) = [fix(a), [f(a), refl()], refl()];

s.r : [
  unfold : [a : A, p : f(s.x.unfold.a) == a],
  k : s.x.unfold == unfold
];
s.r == [s.x.unfold, refl()]

s.r == [[f(s.x.x.unfold.a), refl()], refl()]


fix(a) : Fix;
fix(a) = [fix(a), [f(a), refl()], refl()];

s.unfold.p : f(s.x.unfold.a) == s.unfold.a

s.unfold : [
  a : A,
  p : f(s.x.unfold.a) == a
]

center{b}(c : [a : A, p : b == a]) : [b, refl()] == c = (i) => [
  c.p(i),
  coe(
    (j) => b == c.p(min(i, j)),
    refl()
  )
];

mod(s) : fix(s.x.unfold.a) == s;
mod_k(s, i) : mod(s.x)(i).unfold == s.unfold;

mod(s) = (i) => [
  mod(s.x)(i),
  center(s.unfold)(i),
  mod_k(s, i)
];

// strong fix

K_Eq(a : A) : Type;
k_refl(a) : K_Eq(f(a));

K_Eq(a) = @self(x) -> [
  p_I : f(a) == a,
  p_J : ap(K_Eq, p_I) # k_refl(a) == k
];
k_refl(a) = [refl(), refl()];

k_ap{a}(k : K_Eq(a)) : K_Eq(f(a)) = ap(K_Eq, sym(k.@.p_I)) # k;

k_mod(a, k : K_Eq(a)) : [f(a), k_ap(k)] == [a, k] =
  (i) => [
    k.@.p_I(i),
    coe(
      (j) => K_Eq(k.@.p_I(min(i, j))),

      _ : K_Eq(f(a))
    )
    _ : K_Eq(k.@.p_I(i))
  ]

// hmm
K_Eq(a) = [
  p_I : f(a) == a,
  p_J : refl() == ap(f, p_I)
];
k_refl(a) = [refl(), refl()];
k_ap{a}(k : K_Eq(a)) = ap(K_Eq, sym(k.p_I)) # k;


[
  coe((i) => C_Eq(f, sym(k.p_I(i))), q.p_I),
  coe(
    (i) => refl() == ap(f, coe((i) => C_Eq(f, C_Eq(f, sym(k.p_I(i)))),
      0, i, q.p_I)),
    q.p_J
  )
]
coe(
    (i) => refl() == ap(f, coe((i) => C_Eq(f, C_Eq(f, sym(k.p_I(i)))),
      0, i, q.p_I)),
    q.p_J
  )
[
  ap(C_Eq(f), sym(k.p_I)) # p_I,
  _ :
]
ap(K_Eq, sym(k.p_I)) # k


k.p_J : refl() == ap(K_Eq, sym(k.p_I)) # k.p_I
k_mod{a}(k : K_Eq(a)) : k_refl(a) == k_ap(k) = (i) => [
  k.p_J(i),
  coe(
    (j) => refl() == ap(f, k.p_I(min(i, j))),
    refl()
  )
];

k.p_J : refl() == ap(f, k.p_I);

ap(ap(f), k.p_J)

refl() == ((i) => ap(f)(k.p_J(i)))

refl() == ((i) => ap(f)(k.p_J(i)))

ap(ap(f), k.p_J)


T : Type;
x : T;

T = @self(x) -> [a : A, p : f(a) == x];

//
T : Type;
out(a : A) : T;

T = @self(x) -> [a : A, p : out(a) == x];

// weak fix
K_Eq(a) : Type;
k_out(k : K_Eq(a)) : f(a) == a;

K_Eq(a) = @self(k) -> [
  p_I : f(a) == a,
  p_J : k_out(k) == p,
];
k_out(k) = c_f(f_I, k.@.p_I);

k_ap(a, k : K_Eq(a)) : K_Eq(f(a));
k_ap_j(a, k) : k_out(k_ap(a, k)) == ap(f, k.@.p_I);

k_out(k_ap(a, k)) == ap(f, k.@.p_I)

k_ap(a, k) = [ap(f, k.@.p_I), k_ap_j(a, k)];
k_ap_j(a, k) = _;


k_mod(a, k) : ([f(a), ] : [a : A, k : K_Eq(a)])



Fix = @self(x). [a : A, p : fold([f(a), refl()]) == x]

x : @self(x). [a : A, p : fold([f(a), refl()]) == x]

fold([f(a), refl()]) : @self(x). [a : A, p : fold([f(a), refl()]) == x]

[a : A, p : fold([f(a), refl()]) == fold([f(a), refl()])]

@self(x). [a : A, p : fold([f(a), refl()]) == x]

Γ, x : @self(x). T{x} |- T{x} ⇐ Type
------------------------------------
Γ |- @self(x). T{x} ⇐ Type



Γ |- f : (a : A) -> A
---------------------
Γ |- Fix(f) : Type

Γ |- f(a) == a
--------------------
Γ |- fix(a) : Fix(f)

Γ |- x : Fix(f)
---------------
Γ |- out(x) : A

-------------------------
Γ |- f(out(x)) |-> out(x)

Γ |- out(fix(a, p)) |-> a
Γ |- mod(fix(a, p)) |-> sym(ap(f, p)) @ p
Γ |- fix(out(x), mod(x)) |-> x


sym(ap(f, sym(ap(f, p)) @ p)) @ p

// strong fix
K_Eq(a : A) : Type;
k_p{a}(p : f(a) == a) : K_Eq(a);

K_Eq(a) = @self(k) -> [
  p_I : f(a) == a,
  p_J : k_p(p) == k
];
k_p(a, p) = [sym(ap(f, p)) @ p, refl()];


coe(p, K_Eq, q)

[
  coe(p, C_Eq(f), q.p_I),
  coe(
    J(p),
    ([a, q]) => coe(coe(q, C_Eq(f), q.p_I), K_Eq, k_refl(a)) == k,
    q.p_J
  )
]


k_p(a, p : f(a) == a) : K_Eq(a);
k_p(a, p : f(a) == a) = [
  coe(p, C_Eq(f), refl()),
  _ : coe(coe(p, C_Eq(f), refl()), K_Eq, k_refl(a)) == k
];

[
  coe(coe(p, C_Eq(f), refl()), C_Eq(f), q.p_I),
  coe(
    J(coe(p, C_Eq(f), refl())),
    ([a, q]) => coe(coe(q, C_Eq(f), q.p_I), K_Eq, k_refl(a)) == k,
    q.p_J
  )
]

coe(coe(p, C_Eq(f), refl()), K_Eq, k_refl(a))


k_refl(a) == ap(K_Eq, sym(p_I)) # k_p()

Fix = [a : A, k : K_Eq(a)];
fix(a : A, p : f(a) == a) =
  []

Fix(f) |-> [a : A, k : K_Eq(f, a)];

// f(a) ≡ a
fix(a) |-> [a, k_refl(a)]
out(x) |-> x.a

fix(out(x))


c_next(c_b) = c_b(w_true, w_false).c_b;

Bool = Fix(c_next);
true : Fix(c_next) = fix(c_true);



A : Type;
A = @self(a) -> B{a};


// to
A : Type;
self(a : A) : [a : A, b : B{a}];

A = [
  l : [
    a : A,
    b : B{a}
  ],
  p : self(l.a) == l
];
self(a) = a.l;

// extensionality of sigma + UIP
mod(a) : a.l.a == a = [a.p, UIP()]
unfold(a) : B{a} = coe(mod(a), B, a.l.b);

// extensionality of sigma here
mod(x) = [
  x.x.x == x.x
  _ : x.x == x
];

∘
x.x

x : A;

B = @self(x) -> B{x};
x = @fix(x) -> M{x};

// to
A : Type;
x : A;

self(x : A) : A;
out(x : A) : B{self(x)};
mod(x : A) : self(x) == x;


B = [
  x : A;
  b : B{x};
  p : coe(mod(x), B, out(x)) == b,
];
x = [
  x,
  M{x}

];

//
A : Type;
A = @self(a) -> B{A, a};

// to



CoEq[A](f : (a : A) -> A) : Type;
box[A, f](a : A) : CoEq[A](f);

CoEq[A](f) = (
  C,
  k : (a : A) -> C,
  m : (a) -> k(f(a)) == k(a)
) -> C,

Bool = (
  C,
  k : (c_b : C_Bool) -> C,
  m : (c_b)
) -> _

//

Term, M, N, K ::=
  | x | Type
  | (x : A) -> B | (x) => M | M(N)
  | self(x) -> T | fix(x) => M | M.@


//
Γ, x : A |- B : Type
--------------------------------
Γ | A |- self(x : A) -> B : Type

Γ |- M{x := S} : B(S)
---------------------------------------
Γ | S |- fix(x) => M ⇐ self(x : A) -> B


Γ |-
--------------------------------
Γ |- [Π : A, (a) => Type] : Type


Γ |- A : Type  Γ, a : A |- B : Type
-----------------------------------
Γ |- [Π : A, (a) => B] : Type





Γ |- A : Type  Γ |- B : [Π : A, (a) => Type]
--------------------------------------------
Γ |- (a) => Typ : Type

Γ |- A ⇐ Type  Γ |- B ⇐ [Π : A, (a) => Type]
--------------------------------------------
Γ |- [Π : A, B] ⇒ Type

Γ, x : self(x) -> T |- T : Type
-------------------------------
Γ |- fix(x) => M ⇐ self(x) -> T


Γ, x : self(x) -> T |- T : Type
-------------------------------
Γ |- self(x) -> T : Type

Γ |- T : Type
------------------------
Γ |- self(x) -> T : Type


Γ | x |- M : A
--------------
Γ |- M : A


Γ |- T : Type
------------------------
Γ |- self(_) -> T : Type

Γ, x : A |- B : Type
----------------------------
Γ | A |- self(x) -> B : Type

self(x) -> (T ⇒ Type) |-> (self(x) -> T) ⇒ Type

Γ |- T : Type
---------------------------------------
Γ | A |- fix(x) => M ⇐ self(x : A) -> B

(@fix(P) : [
  Unit : Type,
  unit : Unit
] = [
  self(u) -> (C, r : C(P.unit)) -> C(u),
  @fix(u) => (C, r) => u,
]).@

self(x) -> Type

self(x : self(x) -> Type) -> Type

A : Type;
B(a : A) : Type;

A = self(a : A) -> B(a);
B(a) = _;



//
Term, M, N, K ::=
  | Type | x | x = N; K
  | (x : A) -> B | (x : A) => K | M(N)
  | self(x) -> T | fix(x) => M | M.@



Γ |- A ⇐ Type  Γ |- B ⇐ [Π : A, (a) => Type]
--------------------------------------------
Γ |- [Π : A, B] ⇒ Type

Γ, a : A |- M ⇐ B(a)
--------------------------
Γ |- (a) => M ⇐ [Π : A, B]

Γ |- M ⇒ [Π : A, B]  Γ |- N ⇐ A
-------------------------------
Γ |- M(N) ⇒ B(N)


--------------------
Γ |- self(x) -> Type

Γ, x : A |- B ⇒ Type
----------------------------
Γ | A |- self(x) -> B ⇒ Type

Γ | S.@ |- M{x := S} : B{x := S}
---------------------------------------
Γ | S |- fix(x) => M ⇐ self(x : A) -> B


-----------
Γ |- [ι = M]

Γ, x : self(x) -> Type |- M
------------------------------------------
Γ |- fix(x) : Type => M : self(x) -> Type


Γ |- M : self(x) -> B
---------------------
Γ |- M.@ : B{x := M}

----------------
Γ |- Type : Type

Γ is Context  Γ |- A : Type
---------------------------
Γ |- M : A


(M : A : Type : Type)
(M : A : Type)


self(x : self(x) -> Type) -> Type

Γ, a : A |- M ⇐ B(a)
--------------------------
Γ |- (a) => M ⇐ [Π : A, B]

Γ |- M ⇒ [Π : A, B]  Γ |- N ⇐ A
-------------------------------
Γ |- M(N) ⇒ B(N)

Γ |- T ⇐ [Π : [ι : T], (a) => Type]
-----------------------------------
Γ |- self(x) -> T ⇒ Type


// naive self
Term, M, N, K ::=
  | Type | x | x = N; K
  | (x : A) -> B | (x : A) => K | M(N)
  | self(x) -> T | fix(x) => M | M.@

Γ, x : self(x) -> T |- T : Type
-------------------------------
Γ |- self(x) -> T : Type

Γ, x == fix(x) : T => M |- M : T
-----------------------------------
Γ |- fix(x) : T => M : self(x) -> T

Γ |- M : self(x) -> T
---------------------
Γ |- M.@ : T{x := M}

//

x.@ : x.@

x.@ : Type

self(x) -> (x.@ : Ω)

Γ |- M : A
Γ |- A == B
----------- // conv
Γ |- M : B


Γ |- T : Type
------------------------
Γ |- self(x) -> T : Type

Γ, x : self(x) -> T |- T : Type

Γ, x : self(x) -> T |- T : Type
-------------------------------
Γ |- self(x) -> T : Type

Term, M, N, K ::=
  | Type | x
  | (x : A) -> B | (x : A) => K | M(N)
  | self(x) -> T | fix(x) => M | M.@

self(x) -> (a : A) -> B
self(x) -> M(N)

self(x) -> self(y) -> T
self(x) -> M.@



f : self(f) -> (x : self(x) -> f.@(x)) -> Type
x : self(x) -> f.@(x)

f = f; (x : self(x) -> f.@(x)) -> Type

self(x) -> VS<f>

f :
self(x) -> (f : self(f) -> (x : self(x) -> f(x)) -> Type)(x)

Γ |- B(N) : Type

self(x) -> (M : (a : A) -> B(a))(N)    B(N) == Type

x : self(x) -> f(N) |- x.@(N) : (a : A) -> B(a)



self(x) -> (M : (a : A) -> Type)(N)

self(x) -> (a : A) -> B
self(x) -> M(N)

self(x) -> self(y) -> T
self(x) -> (M : self(y) -> Type).@


-----------------------------------------------
Γ, x : self(x) -> (x.@ : Ω) |- (x.@ : Ω) : Type



//


Γ | S.0 |- M : A  Γ, S.0 == M | S.1 |- N : B{a := S.0}
------------------------------------------------------
Γ | S |- [M, N] : [a : A, b : B]
P.Unit == self(u) -> (C : (u : P.Unit) -> Type, r : C(P.unit)) -> C(u)




Bool = [c_b : C_Bool | c_next(c_b) |-> c_b];

b : Bool;
w_ind(
  b.c_b,
  (c_b) => C([c_b]),
  t,
  f
) : C(c_b(true, false))


// strong fix
K_Eq[A](a : A, f : (a : A) -> A) : Type;
K_Eq[A](a, f) = [
  p : f(a) == a,
  q : K_Eq()
];


k_wrap[a](p : f(a) == a) : K_Eq(a) =
  coe(p, K_Eq, k_refl(a));

Fix : Type;
fix(a : A, p : ) :
Fix = self(x)-> [
  a : A,
  x : K_Eq(a)
];
out(x : Fix) : A = x.a;
elim(x : Fix) : [f(x.a), k_refl(x.a)] == x = x.k.@;



------------------------
Γ |- pack(a) : Fix(A, f)



Fix(A : Type, f : (a : A) -> A) : Type;
pack(a : A, p : f(a) == a) : Fix;

elim(
  x : Fix(A, f),
  C : (x : Fix(A, f)) -> Type,
  c : (a : A, p : f(a) == a) -> C(pack(a, p))
) -> C(x);

elim(a, k) : [f(a), k_refl()] == [a, k]

//

Γ |- A : Type  Γ |- f : (a : A) -> A
Γ, a : A |- f(f(a)) == f(a)
-------------------------------------
Γ |- Fix(A, f) : Type

Γ |- a : A
------------------------
Γ |- pack(a) : Fix(A, f)

elim(
  x : Fix(A, f),
  C : (x : Fix(A, f)) -> Type,
  c : (a : A) -> C(pack(f(a)))
) : C(x);


// f(f(a)) == f(a)
C_Fix = [a : A, p : f(a) == a];
c_f(x : C_Fix) : C_Fix = [f(x.a), refl()];

// k_f(k_f(x)) == k_f(x)
K_Fix = Fix(C_Fix, c_f);
k_fix(a, p) : K_Fix = pack([a, p]);
k_elim(
  k : K_Fix,
  C : (k : K_Fix) -> Type,
  c : (a : A) -> C(k_fix(f(a), refl())),
) : C(k) =
  elim(
    k,
    (x) => (p : f(out(x)) == x) -> C(pack([out(x), p])),
  ) :  : C()

k == pack([f(out(k)), refl()])

pack([f(out(x)), refl()])

elim(
  k,
  (x) =>
)


Γ |- A : Type
Γ |- f : (a : A) -> A  Γ, a : A |- f(f(a)) == f(a)
Γ |- a : A
--------------------------------------------------
Γ |- F_Eq(A, f, a) : Type

------------------------------
Γ |- f_refl() : F_Eq(A, f, a)

Γ |- p : F_Eq(A, f, a)
Γ |- C : (a : A, p : F_Eq(A, f, a)) -> Type
Γ |- r : C(f(a), f_refl())
-------------------------------------------
Γ |- f_elim(p, C, r) : C(a, p)


f : (a : A) -> A;
f_I : (a) -> f(f(a)) == f(a);

C_Fix = [a : A, p : f(a) == a];
c_f(x : C_Fix) : C_Fix = [f(x.a), f_I(a)];

[f(f(x.a)), f_I(f(a))]

c_f(c) = sym(ap(f, c)) . c;

//

Γ, x : A |- B : Type
--------------------------------
Γ | A |- self(x : A) -> B : Type


Γ |- M : self(x : A) -> B
Γ |- A == self(x : A) -> B
--------------------------
Γ |- M.@ : B{x := M}

Γ | S.0 |- M : A  Γ | S.1 : B |- N : B{a := M}
----------------------------------------------
Γ | S |- [M, N] : [a : A, b : B]



//

------------
• is Context

Γ |- A is Type
--------------
Γ, a : A

Γ is Context
--------------
Γ |- A is Type

Γ |- A is Type
--------------
Γ |- M : A

//
Unit : Type;
unit : Unit;

Unit = (C : (u : Unit) -> Type, r : C(unit)) -> C(unit),
unit = (C, r) => r;



//
f(a : A) : A;
f_I(a) : f(f(a)) == f(a);

K_Eq(a : A) : Type
k_wrap(a, p : f(a) == a) : K_Eq(a);




P.Unit == (C : (u : P.Unit) -> Type, r : C(P.unit)) -> C(P.unit)


Sigma(A, B) = [
  Sigma : Type;
  pair(a : A, b : B(a)) : Sigma;

  Sigma = self(s) -> (
    C : (s : Sigma) -> Type,
    c : (a : A, b : B(a)) -> C(pair(a, b))
  ) -> C(s);
  pair(a, b) = (C, c) => c(a, b);
];

Eq[A](x, y) = [
  Eq(y : A) : Type;
  refl(x) : Eq(x);

  Eq(y) = self(e) -> (
    C : (z : A, e : Eq(z)) -> Type,
    c : C(x, refl(x))
  ) -> C(y, e);
  refl(x) = (C, c) => c;
];


Bool = Sigma(
  C_Bool,
  (c_b) => Eq(c_next(c_b), c_b)
);


Bool : Type;
pack(c_b : C_Bool, e : Eq(c_next(c_b), c_b)) : Bool;

Eq[A](x) = [
  Eq(y : A) : Type;
  refl(x) : Eq(f(x));

  Eq(y) = self(e) -> (
    C : (z : A, e : Eq(z)) -> Type,
    c : C(x, refl(x))
  ) -> C(y, e);
  refl(x) = (C, c) => c;
];

ap[A, B, x, y](e : Eq(x, y), f : (a : A) -> B(a)) : Eq(f(x), f(y)) =
  e.@((z, _) => Eq(f(x), f(z)), refl());

Fix[A](f : (a : A) -> A) : Type;
pack[A](a : A, e : Eq(f(a), a)) : Fix[A];

Fix[A](f) =
  self(x) -> (
    C : (s : Fix(f)) -> Type,
    c : (a : A, e : Eq(f(a), a)) -> C(pack(a, e))
  ) -> C(x);
pack(a, e) = (C, c) => c(a, e);


mod(a, e) : pack(f(a), ap(f, e)) == pack(a, e);

Bool =
  self(s) -> (
    C : (s : Bool) -> Type,
    c : (
      c_b : C_Bool,
      e :
        self(e) -> (
          C : (z, e : Eq(c_next(c_b), z)) -> Type,
          c : C(x, refl(x))
        ) -> C(y, e)
    ) -> C(pack(c_b, e))
  ) -> C(s);
pack(c_b, e) = pair(c_b, e);



a : A;
p : f(a) == a;

mod(a, p) : [
  f(a),
  (i) => f(p(i))
] == [
  a,
  (i) => p(i)
] = (i) => [

]


// f(a) ≡ a
K_Eq(a) : Type;
k_refl(a) : K_Eq(f(a))

K_Eq(a) = self(k) -> [f(a), k_refl(a)] == [a, k];
k_refl(a) = refl();

k_ap[a](k : K_Eq(a)) : K_Eq(f(a)) =
  ap(K_Eq, sym(k.@.0)) # k;

k_mod[a](k : K_Eq(a)) : k_ap(k) == k_refl(a);


C_Eq(a) = f(a) == a;
c_refl(a) : C_Eq(f(a)) = refl();

c_ap(c : C_Eq(a)) : C_Eq(a) = ap(f, c);


f_prop2[A, f, x, y](f_prop : (x, y) -> f(x) == f(y)) : f(x) == f(y) =
  sym(f_prop(x, x)) . f_prop(x, y);

ap_f_K[
  A,
  B,
  f : (a : A) -> B,
  x,
  y
](
  f_prop : (x : A, y : A) -> f(x) == f(y),
  p : x == y
) : ap(f, p) == f_prop2(f_prop, x, y) =
  J(
    p,
    (z, q) => ap(f, q) == f_prop2(f_prop, x, z),
    refl() : refl() == f_prop2(f_prop, x, x)
  );
//

apD(f, p) : ap(B, p) # f(x) == f(y);

k_ap_prop[a](x : K_Eq(a), y) : k_ap(x) == k_ap(y),

apD_f_K[
  a,
  f : (k : K_Eq(a)) -> K_Eq(f(a)),
  x : K_Eq(a),
  y : K_Eq(a),
](
  b_prop : (x, y) -> B(x) == B(y),
  f_prop : (x, y) -> k_ap(x) == k_ap(y) # f(y),
  p : x == y
) :


apD_f_K[
  A : Type,
  B : (a : A) -> Type,
  f : (a : A) -> B(a),
  x,
  y,
](
  b_prop : (x, y) -> B(x) == B(y),
  f_prop : (x, y) -> f(x) == b_prop(x, y) # f(y),
  p : x == y
) :
  coe(
    p,
    (z) => f(x) == b_prop(x, z) # f(z),
  )
  apD(f, p) == f_prop2(f_prop, x, y) =
  J(
    p,
    (z, q) => ap(f, q) == f_prop2(f_prop, x, z),
    refl() == f_prop2(f_prop, x, x)
    _
  )


K_Eq(a : A) : Type;
k_refl(a) : K_Eq(f(a));

K_Eq(a) = self(k) -> [
  p_I : f(a) == a,
  p_J : ap(K_Eq, p_I) # k_refl(a) == k
];
k_refl(a) = [refl(), refl()];

//
K_Eq(A, f, f_I, a) = [
  p_I : f(a) == a,
  p_J : ap(f, p_I) == f_I(a)
];


c_f(p) = sym(ap(f, p)) . p;

//
Fix = [a : A, p : a == b];
mod(a, p) : [f(a), refl()] == [a, p];

mod(a, p).a == p
mod(a, p).p == refl()

K_Eq(a) = [p : f(a) == a, q : c_f(p) == p];
Fix = [a : A, k : K_Eq(a)];



k_ext(x, y) : [
  p : c_f(x.p) == c_f(y.p),
  q : coe(p, C_Eq(c_f), ap(c_f, x.q)) == ap(c_f, y.q)
] -> x == y = _;

k_ext(x, y) : [
  p : x.p == y.p,
  q : coe(p, C_Eq(c_f), x.q) == y.q
] -> x == y = _;

ext(x, y) : [
  a : x.a == y.a,
  p : coe(x.k, C_Eq(f), x.k.p) == y.k.p,
  q : coe(p, C_Eq(c_f), coe(a, K_Eq, x.k).q) == y.q
] -> x == y;

mod(a, p, q) : [f(a), refl(), refl()] == [a, p, q];

mod(a, p, q).a == p
mod(a, p, q).p == q


ext(x, y) = [
  a : x.a == y.a,
  p : coe(a, C_Eq(f), x.p) == y.a,
  q : c_f(p) == p
]


mod(a, p, q).p == q : c_f(p) == p

split(p) = [c_f(p), ap(f, p)];
merge([p, q]) = coe(p, C_Eq(f), q);


merge(split(p, q)) == p


p : f(a) == a;
q : c_f(p) == p;

(j) => c_f(p)

ap(c_f, q) == c_f_I(p)


//



//
Bool : Type;
true : Bool;
false : Bool;

Bool = self(b) -> (
  C : (b : Bool) -> Type,
  t : C(true),
  f : C(false)
) -> C(b);
true = (C, t, f) => t;
false = (C, t, f) => f;

Bool =
  self(s) -> (
    C : (s : Bool) -> Type,
    c : (
      c_b : C_Bool,
      e : c_next(c_b) == c_b,
    ) -> C(pack(c_b, e))
  ) -> C(s);


//
K_Eq(a) : Type;
k_refl(a) : K_Eq(f(a));

K_Eq(a) = self(k) -> [
  p_I : f(a) == a,
  p_J : ap(K_Eq, p_I) # k_refl(a) == k
];
k_refl(a) = [refl(), refl()];


c_f(p) = sym(ap(f, p)) . p;
S_Eq(a) = [
  p_I : f(a) == a,
  p_J : c_f(p_I) == p_I
];

k.@.p_J : ap(K_Eq, k.@.p_I) # k_refl(a) == k


k.@.p_J.@.p_I : c_f(k.@.p_I) ==

in(s : S_Eq(a)) : K_Eq(a) = ap(K_Eq, s.p_I) # k_refl(a);
out(k : K_Eq(a)) : S_Eq(a) = [k.@.p_I, k.@.p_J.@.p_I];

ap(K_Eq, s.p_I) # k_refl(a)

[
  (ap(K_Eq, s.p_I) # k_refl(a)).@.p_I,
  (ap(K_Eq, s.p_I) # k_refl(a)).@.p_J.@.p_I
]

out(in(s))


left(k) : in(out(k)) == k = k.@.p_J;
right(s) : out(in(s)) == s = ;

//
K_Eq(a, f) : Type;
K_Eq(a, f) = [
  p_I : f(a) == a,
  p_J : K_Eq(p_I, c_f)
];

k_refl(a, f) : K_Eq(f(a));
k_refl(a, f) = [refl(), k_refl(refl(), c_f)];


c_f(p) = sym(ap(f, p)) . p;
S_Eq(a) = [
  p_I : f(a) == a,
  p_J : c_f(p_I) == p_I
];

(ap(K_Eq, s.p_I) # k_refl(a)).@.p_J :
  ap(K_Eq, c_f(s.p_I)) # k_refl(a) == (ap(K_Eq, s.p_I) # k_refl(a)).@.p_J

in(s : S_Eq(a)) : K_Eq(a);
in_J(s : S_Eq(a)) : ap(K_Eq, s.p_I) # k_refl(a) == in(s);

in(s) = [s.p_I, in_J(s)];
in_J(s) = [
  s.p_J,
  _
];

[
  p_I : c_f(s.p_I) == s.p_I,
  p_J :
]
ap(K_Eq, s.p_I) # k_refl(a) == in(s)


[s.p_I, in_J(s)]
in(s).@

ap(K_Eq, s.p_I) # k_refl(a) == @fix(k) => [
  c_f(s.p_I),
  (ap(K_Eq, s.p_I) # k_refl(a)).@.p_J
]


// coinductive
K_Eq[A](f, a) : Type;
K_Eq[A](f, a) = [
  p_I : f(a) == a,
  p_J : K_Eq(c_f, p_I)
];

k_refl(f, a) : K_Eq(f, f(a));
k_refl(f, a) = [refl(), k_refl(c_f, refl())];

S_Eq(a) = [
  p_I : f(a) == a,
  p_J : c_f(p_I) == p_I
];
k_refl(s.p_I, c_f) : K_Eq(c_f(s.p_I), c_f);

in(s : S_Eq(a)) : K_Eq(a, f) = [
  s.p_I,
  ap(K_Eq(c_f), s.p_J) # k_refl(c_f, s.p_I)
];
out(k : K_Eq(a)) : S_Eq(a) = [k.p_I, k.p_J.p_I];


in(s) == [
  s.p_I,
  ap(K_Eq(c_f), s.p_J) # k_refl(c_f, s.p_I)
]

out([
  s.p_I,
  ap(K_Eq(c_f), s.p_J) # k_refl(c_f, s.p_I)
])

(ap(K_Eq(c_f), s.p_J) # k_refl(c_f, s.p_I)).p_I

[
  s.p_I,
  (ap(K_Eq(c_f), s.p_J) # k_refl(c_f, s.p_I)).p_J.p_I
]


// left(k) : in(out(k)) == k = k.@.p_J;

//

f ≡ box ∘ fst

c_f(p_I) = sym(ap(f, p_I)) . p_I;

c_f(p_I) = sym(ap(box, ap(fst, p_I))) . p_I;

box(fst(a)) == a



S_Eq(a) = [
  p_I : box(fst(a)) == a,
  p_J : ap(fst, p_I) == refl()
];
s_refl(a) : S_Eq(f(a)) = [refl(), refl()];
s_ap[a](s : S_Eq(a)) : S_Eq(f(a)) =

  [ap(, s.p_I), ap(ap(fst), s.p_J)];

fst(box(fst(a))) == fst(a)



// f(f(a)) == f(a)
Fix : Type;
fix(a : A) : Fix;

Fix = self(x) -> [a : A, p : fix(a) == x];
fix(a) = [f(a), refl()];

a : A == f(a);

P : [
  K_Eq(a) : Type,
  k_refl(a) : K_Eq(f(a)),
];
P = [
  K_Eq(a)
]


K_Eq(a) : Type;
k_refl(a) : K_Eq(f(a));

K_Eq(a) = self(k) -> [
  p_I : f(a) == a,
  p_J : ap(K_Eq, p_I) # k_refl(a) == k
];
k_refl(a) = [refl(), refl()];

// strong fix
P : [
  K_Eq(a) : Type;
  k_refl(a) : K_Eq(f(a));
];
P = [
  K_Eq(a) = self(k) ->
    ([f(a), P.k_refl(a)] : [a : A, k : P.K_Eq(a)]) == ([a, k]);
  k_refl(a) = refl()
];

K_Eq(a) = self(k) -> [
  p_I : f(a) == a,
  p_J : ap(K_Eq, p_I) # k_refl(a) == k
];
k_refl(a) = [refl(), refl()];

// weak fix
f(a : A) : A;
f_I(a) : f(f(a)) == f(a);
f_J(a) : f_I(f(a)) == ap(f, f_I(a));

K_Eq(a) : Type;
k_ap(a, k : K_Eq(a)) : K_Eq(f(a));

k_refl(a) : K_Eq(f(a));
k_mod(a) : k_refl(f(a)) == k_ap(a, k_refl(a));



//
C_Bool : Type;
c_true : C_Bool;
c_false : C_Bool;

c_i(w : W_Bool) : C_Bool = w.c;
c_r(c : C_Bool) : W_Bool = c(w_true, w_false);

c_next(c : C_Bool) : C_Bool = c_i(c_r(c));

c_r(c_i(w)) == w.c(w_true, w_false)

Bool : Type = [c : C_Bool, p : c_next(c) == c];
true : Bool;
false : Bool;

i(b : Bool) : C_Bool = b.c;
r(c : C_Bool) : Bool = [c_next(c), c_next_I(c)];

r(i(b)) == [c_next(b.c), c_next_I(b.c)]
r(i(b)) == b

i(r(c)) == c_next(c)

C_Either : Type;
c_left(a : A) : C_Either;
c_right(b : B) : C_Either;

c_i(w : W_Either) : C_Either = w.c;
c_r(c : C_Either) : W_Either = c(w_left, w_right);

c_next(c : C_Either) : C_Either = c_i(c_r(c));

K_Eq(w) = [
  c_I : c_r(c_i(w)) == w,
  w.i()(ap(c_i, c_I))
  ap(c_i, c_I) : c_i(c_r(c_i(w))) == c_i(w)
  _ :
]

w = [c_left(a), _];
[
  p : f(c) == c,
  q : ap(f, p) == refl()
]

p : f(c) == c
q : ap(f, p) == refl()

ap((p : f(c) == c) => [f(c), ap(f, p)], q) ==
ap((p) => [c, p], q)

ap(
  (p : f(c) == c) => ap(f, p),
  q
) ==
ap(id, q)

ap(
  (p : f(f(c)) == f(c)) => [f(c), p],
  ap(
    (p : f(c) == c) => ap(f, p),
    q
  )
) ==
ap(
  (p : f(f(c)) == f(c)) => [c, p]),
ap(id, q)

ap(c_i, p)

ap((p : w) => ap(c_r, p))

ap(ap(c_r), q) : ap(c_r, ap(c_i, p)) == refl()

ap(c_r ∘ c_i, )
c_i(c_r(c_i(w))) == c_i(w)

c_r(c_i(c_r(c))) == c_r(c)

Equiv(
  C_Either,
  [
    b : Bool,
    c : C_Either,
    p : to_bool(c) == b,
  ]
)
f_eq([b, c, p]) ==
[b, ]

out_left(w, p : to_bool(w.c) == true) : A
out_right(w, p : to_bool(w.c) == true) : B


// f(f(a)) == f(a)
Fix : Type;
fix(a : A) : Fix;

Fix = self(x) -> [a : A, p : fix(a) == x];
fix(a) = [f(a), refl()];
//
K_Eq(a) : Type;
k_refl(a) : K_Eq(f(a));

K_Eq(a) = self(k) -> [
  p_I : f(a) == a,
  p_J : ap(K_Eq, p_I) # k_refl(a) == k
];


//
Fix : Type;
#fix[a : A] : Fix;

Fix = self(x) -> [a : A, p : #fix[a] == x];
#fix[a] = [f(a), refl()];

S : Type = [
  A : Type;
  f(a : A) : A;
  f_I(a) : f(f(a)) == f(a);
  f_J(a) : f_I(f(a)) == ap(f, f_I(a))
];

f_I(a) : [
  p : f(f(a)) == f(a),
  q : ap()
]

//

P : [
  Fix : Type;
  fix(a : A) : Fix;
];
P = [
  Fix = self(x) -> [a : A, p : #fix[a] == x];
  #fix[a] = [f(a), refl()];
];

//
L : Type;
L = [a : L, b : B];

R : Type;
R = [a : [a : R, b : B], b : B];

unfold(L, M : L) : [a : L, b : B]
fold(L, M : [a : L, b : B]) : L

L == [a : L, b : B]


[a : L, b : B] == [a : [a : R, b : B], b : B]
L == [a : R, b : B]
L == R
[a : L, b : B] == [a : [a : R, b : B], b : B]

//
Sigma : Type;
pair(a : A, b : B(a)) : Sigma(A, B);

Sigma = self(s) -> (
  C : (s : Sigma) -> Type,
  c : (a : A, b : B(a)) -> C(pair(a, b))
) -> C(s);
pair(a, b) = (C, c) => c(a, b);

//
Sigma : Type;
Sigma = self(s) -> (
  C : (s : Sigma) -> Type $ 0,
  c : (a : A, b : B(a)) -> C((C, c) => c(a, b))
) -> C(s);

//
Fix : Type;
fix(a : A) : Fix;

Fix = self(x) -> [a : A, p : [f(a), refl()] == x];
fix(a) = [f(a), refl()];

//
Fix : Type;
Fix = self(x) -> Σ(A, (a) => [f(a), refl()] == x]);
//
Sigma : Type;
Sigma = self(s) -> (
  C : (s : Sigma) -> Type $ 0,
  c : (a : A, b : B(a)) -> C((C, c) => c(a, b))
) -> C(s);

Γ, x : self(x) -> T |- T : Type
-------------------------------
Γ |- self(x) -> T : Type




Bool = [c : C_Bool == c_next(c)];
true = [c_true];
false = [c_false];

p : c_true == c_b;

coe(
  p,
  (z) => [c : C_Bool == z(c_true, c_next(c))],
  [c_true]
) : [a : A == f(y)]

[c : C_Bool, ]


Γ, a : A |- N : A
------------------------
Γ |- [a : A == N] : Type

Γ |- M : A  Γ |- M == N{a := M}
-------------------------------
Γ |- [M] ⇐ [a : A == N]

Γ |- K{a := M} == K{a := N} : A
-------------------------------
Γ |- M == N : [a : A == K]


w_ind(b : C_Bool, C, t : C(c_true), f : C(c_false)) : C(c_next(c));

Bool = [c : C_Bool == c_next(c)];
true = [c_true];
false = [c_false];

c_spin(z : C_Bool) : [c : C_Bool == c_next(z)] =
  w_ind(z, (z) => [c : C_Bool == z], [c_true], [c_false]);

// because c_spin(b.c).c |-> c_spin(b.c) |-> b.c and ([b.c] : Bool)
mod_spin(b) : [c_spin(b.c).c] == b = refl();

mod(b : Bool) : b.c(true, false) == b =



// because
mod(u : Unit) : unit == u =
  w_ind(
    u.c,
    (z) => (p : z == c_unit) =>
      unit ==
        coe(
          p,
          (z) => [c : C_Unit == z],
          [z]
        ),
    (p) => refl(),
  )

mod1(b : Bool) : b.c(true, false).c == b.c =
  w_ind(
    b.c,
    (c) => c(true, false).c == c,
    refl(),
    refl()
  );

mod2(b : Bool) : b.c(true, false) == b =
  w_ind(
    (c) => (p : c(true, false).c == b.c) ->
      c(true, false) == b,
    (p : c_true == b.c) =>
      coe(
        p,
        (z) => (p : c_true == z) ->
          coe(p, (z) => (true : [c : C_Bool == c_true])) ==
            coe(sym(p), (z) => [c : C_Bool == z], [z]),
        (p) => refl()
      )(p) : (true : [c : C_Bool == c_true]) == [b.c],
      _ : true == b,
  )

[w_ind(b.c, (z) => [c : C_Bool == z], true, false).c] == b

ind(b : Bool, C, t : C(true), f : C(false)) : C(b) =
  w_ind(
    b.c,
    (c) =>
      C(
        [
          w_ind(
            c,
            _,
            _,
            _
          )
        ]
      ),
    (t) =>
      w_ind(
        b.c,
        (c) => (p : c_true == c) =>
          [
            q : c_true == c,
          ]
      ),
    (f) => _,
    _
  )


u.c(unit) == unit

(b.c(true, false) : [c : C_Bool == b.c(true, false).c]) == b.c
[]
w_ind(b.c, (z) => [c : C_Bool == z], true, false) :
  [c : C_Bool == b.c]
mod(b : Bool) : b.c(true, false) == b =
  w_ind(
    b.c,
    (z) => (p : c_next(z) == b.c) =>
      (b : Bool) ==
        [
          coe(
            p,
            (z) => [c : C_Bool == z],
            w_ind(
              z,
              (z) => [c : C_Bool == z],
              true,
              false
            )
          ).c
        ],
    (p) => refl(),
    (p) => refl()
  )(refl());

(b : Bool) ==
(w_ind(
  b.c,
  (z) => [c : C_Bool == z],
  true,
  false
) : Bool)

w_ind(
  b.c,
  (c) =>
    w_ind(
      c,
      (z) => [c : C_Bool == z],
      true,
      false
    ) == c(true, false),
  refl(),
  refl()
)

b.c(true, false) ==
  coe(
    p,
    (z) => [c : C_Bool == z],
    b
  )

ind(b : Bool, C, t : C(true), f : C(false)) : C(b) =
  w_ind(
    b.c,
    (z) =>
ind(b : Bool, C, t : C(true), f : C(false)) : C(b) =
  w_ind(
    b.c,
    (z) => (p : z == b.c) =>
      b == coe(p, (z) => [c : C_Bool == z], [z]) =>
    ];
  )


//
w_ind(
  b.c,
  (z) =>
    C(
      [
        w_ind(
          z,
          (z) => [c : C_Bool == z],
          true,
          false
        ).c
      ]
    ),
  t,
  f
)

C(
  [
    true.c
  ]
)

C([b.c])
C(
  w_ind(
    b.c,
    (z) => [c : C_Bool == z],
    true,
    false
  )
)

[c : C_Bool == c_true] == [c : C_Bool == c_next(c)]

f() == g(a)

a == f(a)

[a : A == f(a)] == [a : A == g(a)]



M : [c : C_Bool == b.c]


[a : ]

Bool = [c : C_Bool == c_next(c)];
b : Bool;



p : c_true == b.c;

w_ind(
  b.c,
  (z) => (p : c_next(z) == b.c) =>
    (b : Bool) ==
      (coe(
        p,
        (z) => [c : C_Bool == z],
        w_ind(
          z,
          (z) => [c : C_Bool == z],
          true,
          false
        )
      ) : [c : C_Bool == b.c]),
  (p) => refl(),
  (p) => refl(),
)(refl()) :

w_ind(
  b.c,
  (z) =>
    C(
      w_ind(
        z,
        (z) => [c : C_Bool == z],
        true,
        false
      )
    ),
  t,
  f
)
C(
  w_ind(
    b.c,
    (z) => [c : C_Bool == z],
    true,
    false
  )
)

[
  p : c_next(z) == b.c,
  c :
    C(
      coe(
        p,
        (z) => [c : C_Bool == z],
        w_ind(
          z,
          (z) => [c : C_Bool == z],
          true,
          false
        )
      ) : [c : C_Bool == b.c]
    )
]

(b : Bool) ==
      w_ind(b.c, (z) => [c : C_Bool == z], true, false)

b == w_ind(
  b.c,
  (z) => [c : C_Bool == z],
  true,
  false
)

coe(
  J(p),
  (z, q : c_true == z) =>
    (b : Bool) ==
      coe(
        sym(q),
        (z) => [c : C_Bool == z],
        _ : [c : C_Bool == z]
      )
      (_ : [c : C_Bool == c_true]) : Bool),
  _ : true ==
    (coe(
        sym(p),
        (z) => [c : C_Bool == z],
        [c_true]
      ) : Bool)
  _ : true == b.c(b, false)
)
coe(
  sym(p),
  (z) => [c : C_Bool == z],
  [c_true]
) : [c : C_Bool == b.c]
coe(
  w,
  (z) => [c : C_Bool == z],
  coe(
    q,
    (z) => [c : C_Bool == z],
    b
  )
)

b.c(b, true) == true

coe(
  p,
  (z) => z(true, b) == true,
  refl()
) : b.c(true, b) == true
true == b.c(true, b)

([c_true] : [c : C_Bool == c_true]) ==
    coe(q, (z) => [c : C_Bool == z], b)
w_ind(
  b.c,
  (z) => z(true, false) ==
    [
      x : Bool,
      p : z == x.c,
      c :
        C(
          coe(
            p,
            (z) => [c : C_Bool == z],
            [z]
          )
        )
    ],
  (q : c_true == b.c) => [
    true,

  ]
  (p : c_true == b.c) => [
    _ : c_true == b.c,
    _ :
      C(
        coe(
          q,
          (z) => [c : C_Bool == z],
          [z]
        )
      )
  ]

    _ : C(
    coe(
      p,
      (z) => [c : C_Bool == z],
      [c_true]
    )
  )
    _,
)

[c : C_Bool == f()]
b == coe(
  ap(c_next, p),
  (z) => [c : C_Bool == z],
  true
)
true : [c : C_Bool == b.c]

p : [c : C_Bool == c_true] == [c : C_Bool == b.c];

g : sym(p).cast(b) == true;

coe(
  p,
  (T, q : [c : C_Bool == c_true] == T) =>
    q.cast(sym(p).cast(b)) == q.cast(true),
  refl()
) : p.cast(sym(p).cast(b)) == p.cast(true)

p.cast(true) : [c : C_Bool == b.c]

p.cast(true).c

b == p.cast(true)
b == p.cast(true)
sym(p).cast(b) == b


coe()
coe(
  J(p),
  (c, w) => [c : C_Bool == c_true] == [c : C_Bool == c],
  refl()
) : [c : C_Bool == c_true] == [c : C_Bool == b.c]
[c : C_Bool == c_true] == [c : C_Bool == b.c]

[c : C_Bool == true] ==
[c : C_Bool == c_next(c_b)]




w : [c : C_Bool == b.c] == [c : C_Bool == c_true]

p : b.c

b.c(true, false) == b
w_ind(
  b.c,
  (z) => [z],
  (C) => _,
  _
)()


Unit = [c : C_Unit == c_next(c)];

r : C(unit);

u : Unit;
u : [c : C_Unit == u.c];

p : c_unit == u.c;

coe(sym(p), (z) => [c : C_Unit == z], u) : [c : C_Unit == c_unit]
coe(sym(ap(c_next, p)), (z) => [c : C_Unit == z], u) == unit

g : unit == u;
g = coe2(
  J(p),
  (z, q : u.c == z) =>
    coe(ap(c_next, q), (z) => [c : C_Unit == z], u) == u,

)

sym(ap(c_next, q)) : c_next(z) == c_next(c_unit)


coe(
  p,
  (z, q : c_unit == z) =>
    unit == (u : [c : C_Unit == z]),
  refl()
) : [c : C_Unit == c_unit] == [c : C_Unit == u.c]

w_ind(
  u.c,
  (z) => [c_u : C_Unit == c_next(c_u)] == [c_u : C_Unit == z],
  _
)

mod(c_b, c) : c_next(c_b)(c_true, c_next(c)) == c_next(c) =
  w_ind(
    (c_b) =>
      (p : c_b == c_next(c)) ->
      c_next(c_b)(c_true, c_next(c)) == c_next(c),
    (p) => _ : c_true == c_next(c)
  )

c_next(c_b) == c_next(c)
[c : C_Bool == c_next(c_b)(c_true, c_next(c))]
[a : A == c(c_true, c_next(c))]
[y]

f(c : C_Bool, p : c_next(c) == c) : Bool =
  [c]
(c) => [c_next(c_b)]
w_ind(
  b.c,
  (c) => (w : [c_b : C_Bool == c]) -> C(w.b),
  _
  _
)(



  _ : [b : Bool, b.c(true, false) == b]) : C([c_next(b.c)])

[b.c] : [b : Bool == c_next(b.c)]

M : [c_b : C_Bool == c_true]
M : Bool

c_next()
w_ind(
  b.c_b,
  (c_b) => C([c_next(c_b)]),
  t,
  f
) : C(b.c_b(true, false))

M(N) ⇐ _T
(M ⇐ (a : [a : _A == N]) -> _T)(N ⇐ _A)

// then
term : M
received : (a : A) -> B
expected : (a : [a : _A == N]) -> _T

expected : A
received : [a : _A == N]

term : M(N)
received : B{a := N}
expected : _T



// then by coercion

((M ⇒ (a : A) -> B) ⇐ (a : _A == N) -> _B)(N ⇐ _A)

received : (a : A) -> B
expected : (a : _A == N) -> _B

received : B{a := N}
expected : _B{a := N}

(M ⇐ (a : _A) -> _B)(N ⇐ _A)

Γ, x : self(x) -> _A[x] |- T : Type
Γ, x : self(x) -> _A[x] |- _A[x] == T
-------------------------------------
Γ |- self(x) -> T : Type


(M(N) ⇐ _B == _)
(M ⇐ (a : _A) -> _B == _(N))(N ⇐ _A)

(M : (a : A) -> B) :> (a : _A) -> _B == _(N ⇐ A)
_A <: A
(M(N) : B(N)) :> _B



Γ |- M : (a : A) -> B  Γ |- N : A
---------------------------------
Γ |- M(N) ⇒ B(N) ⇐ _B
// vs
Γ |- M : (a : A) -> B  Γ |- x : A
---------------------------------
Γ |- M(x) ⇒ B(x) ⇐ _B

(M : (a : A) -> B) :> (a : _A) -> _B == _(x ⇐ A)
_A <: A
(M(x) : B(x)) :> _B{x := a}

(M : (a : A) -> C)(x) : C(x)

(M : (a : A) -> B) :> (a : _A) -> _B

(M ⇐ (a : [a : _A == N]) -> _B)(N ⇐ A)

(M(N) ⇐ _B)
(M ⇐ (a : [a : _A == N]) -> _B)(N ⇐ A)
// let's focus on the M
M ⇒ (a : A) -> B ⇐ (a : [a : _A == N]) -> _B
// and just do coercive subtyping
([a : _A == N]) <: A -| _.a
a : [a : A == N] |- B{a := a.a} <: _B
// which is the same as
a : [a : A == N] |- B{a := N} <: _B

// same as
(M ⇒ (a : A) -> B)(N) ⇐ _B
M(N ⇐ A) ⇒ B{a := N} ⇐ _B



Γ, a : E_A |- M(C[a]) ⇒ L_B(C[a]) ⇐ E_B(a)
--------------------------------------------------
Γ |- M ⇒ (a : L_A) -> L_B(a) ⇐ (a : E_A) -> E_B(a)


-----------------------------
Γ |- [a : A == N] <: A -| _.a

M ⇒ (a : A) -> B ⇐ (a : [a : A == N]) -> _B

M(N) ⇒ B{a := N} ⇐ _B{a := N}

M ⇒ (a : A) -> B ⇐ (a : [a : A == N]) -> _B

(M(N))

(M ⇒ (a : A) -> B ⇐ (a : [a : A == N])

M ⇒ (a : A) -> B ⇐ (a : _A == N) -> _B


(M ⇒ (a : A) -> B) ⇐ (a : _A == N) -> _B)(N ⇐ _A)



received : (a : A) -> B
expected : (a : _A == N) -> _B

received : B{a := N}
expected : _B{a := N}


((x) => M ⇐ _T as S)

(M : A) ⇐ B




f : <(a : Nat) -> Nat>

~(f)(1)

Γ |- M ⇐ A  Γ |- M == N
-----------------------
Γ |- M ⇐ [x : A == N]

(((A) => (a) => a) ⇐ [x : Id == id])

([((A) => (a) => a) ⇐ Id])


((M ⇒ (a : A) -> ⇑B)(N) ⇐ (_B : Code) | _)

((M ⇒ (a : A) -> ⇑B)(N) ⇐ (_B : Code) | _)

(M : (a : A) -> ⇑B) ⇐ ((a : _A) -> _B) : Code | _(N)
(M : (a : A) -> ⇑B) ⇐ ((a : A) -> _B) : Code | _(N)
(M(N) ⇒ ⇑B(N) ⇐ (_B : Code) | _(N))
(M(N) ⇒ ⇑B(N) ⇐ (⇑_B : Meta) | ~(_(N)))
(M : (a : A) -> ⇑B) ⇐ ((a : A) -> _B) | ~(_(N))
(M : (a : A) -> ⇑B) ⇐ (((a : _A) -> _B) : Code) | _(N)


Γ, _S == self(x) -> _A, x : self(x) -> _A | _A |- T : Type
----------------------------------------------------------
Γ, _S |- self(x) -> T : Type

Γ, _S == fix(x) => _M, x == _S | _M |- M : T
--------------------------------------------
Γ | _S |- fix(x) => M : self(x) -> T

Γ, _S == _M.@ | _M |- M : self(x) -> T
---------------------------------
Γ | _S |- M.@ : T{x := M}

Γ, _S == (a : _A) -> _B | _A |- A : Type
Γ, _S == (a : A) -> _B | _B, a : A |- B : Type
-----------------------------------------------
Γ | _S |- (a : A) -> B : Type

Γ, _S == (x) => _M, a : A | _M |- M : B
---------------------------------------
Γ | _S |- (x) => M : (a : A) -> B

Γ, _S == _M(_N) | _M |- M : (a : A) -> B
Γ, _S == M(_N) | _N |- N : A
----------------------------------------
Γ |- _S |- M(N) : B{a := N}

//
Γ, _S == self(x) -> _A, x : self(x) -> _A | _A |- T : Type
----------------------------------------------------------
Γ, _S |- self(x) -> T : Type

Γ, _S == fix(x) => _M, x == _S | _M |- M : T
--------------------------------------------
Γ | _S |- fix(x) => M : self(x) -> T

Γ, _S == _M.@ | _M |- M : self(x) -> T
---------------------------------
Γ | _S |- M.@ : T{x := M}

Γ, _S == (a : _A) -> _B | _A |- A : Type
Γ, _S == (a : A) -> _B | _B, a : A |- B : Type
-----------------------------------------------
Γ | _S |- (a : A) -> B : Type

Γ, _S == (x) => _M, a : A | _M |- M : B
---------------------------------------
Γ | _S |- (x) => M : (a : A) -> B

Γ, _S == _M(_N) | _M |- M : (a : A) -> B
Γ, _S == M(_N) | _N |- N : A
----------------------------------------
Γ |- _S := M(N) : B{a := N}



fix(False) : Type => (C : (f : False.@) -> Type)

p : self(p) -> (
  C : (a : A) -> Type,
  c : (a : A, b : B(a)) -> C(a)
) -> C(_p)


Sigma(A, B) =
  self(p) -> (
    C : (a : A) -> Type,
    c : (a : A, b : B(a)) -> C(a)
  ) -> C(p.@((_) => A, (a, b) => a));

Pair(A, B) =
  (C, c : (a : A, b : B) -> C) -> C
P_Bool = Sigma(Type, (Bool) => Pair(Bool, Bool));

fix(P : P_Bool) =>
  (C, c) =>
    c(
      self(b) -> (
        C : (b : fstD(P)) -> Type,
        t : C(fst(sndD(P))),
        f : C(snd(sndD(P))
      ) -> C(b),
      (C, c) => c()
    )
fix(P : P_Bool) =>
  (C, c) =>
    c(
      self(b) -> (
        C : (b : P.@.Bool) -> Type,
        t : C(true),
        f : C(false)
      ) -> C(b),
      _
    )
    _ :
  [
  Bool = self(b) -> (
    C : (b : P.@.Bool) -> Type,
    t : C(true),
    f : C(false)
  ) -> C(b),
  true = fix(b : P.@.Bool) => (C, t, f) => t,
  false = fix(b : P.@.Bool) => (C, t, f) => f
]

A : Type;
B(a : A) : Type;

C_Pair = (
  C,
  c : (a : A, b : B(a)) -> C
) -> C
c_pair(a, b) : C_Pair = (C, c) => c(a, b);
c_fst(c : C_Pair) : A = c(A, (a, b) => a);


P_Sigma = [
  p : C_Pair ==
    (C, c) => c(c_fst(p), _ : B(c_fst(p)))
  c(C_Pair, c_pair)];

fst(s : Sigma) = s.c(A, (a, b) => a);
snd(s : Sigma) : B(s.c(A, (a, b) => a)) =
  s.c(A, (a, b) => b);


Γ |- C<x> : A
Γ |- N : A
-------------
Γ |- C<N> : A

Γ |- A : Type
-------------------
Γ, x : A is Context

Γ |- A : Type
-----------------
Γ, x : A |- x : A


Γ is Context
Γ |- A : Type
-------------
Γ |- M : A

Γ |- M : B
-----------------
Γ |- M : B


A : Type, x : A |- M : B
--------------------------
x : Nat |- M : B{A := Nat}

Γ[_X] |- M : B[_X]
------------------
Γ[N] |- M : B[N]

Self_x[T] == self(x) -> T.@(x)
Self_T == self(T) -> (x : Self_x[T]) -> Type : Type


------------------------------
T : _A |- self(x) -> T.@(x) : Type


----------------------------------------
|- self(T) -> (x : _X[T]) -> Type : Type



--------------------------------------
T : Self_T |- Self_x[T] : Type


// in all the positions
Γ == T : self(U) -> (x : _1) -> Type, x : _2 |- T.@(x)

// checking _1
T : self(U) -> (x : A) -> Type |- Self_x[T] : Type

// checking _2
T : self(U) -> (x : A) -> Type, x : A |- Self_x[T] : Type


------------------------------------------------------
T : self(U) -> (x : A) -> Type, x : A |- T.@(x) : Type

--------------------------------------------------------------
T : Self_T, x : Self_x[T] |- T.@(x) : Type
--------------------------------------
T : Self_T |- self(x) -> T.@(x) : Type
----------------------------------------------------
T : Self_T |- (x : self(x) -> T.@(x)) -> Type : Type
----------------------------------------------------
|- self(T) -> (x : self(x) -> T.@(x)) -> Type : Type


|- self(T) -> (x : A[T]) -> Type : Type


Γ, x : self(x). T |- T : Type
-----------------------------
Γ |-


//

A : Type;
B(a : A) : Type;

A = self(a) -> (b : B(a)) -> Type;
B(a) = self(b) -> a.@(b);


self(a) -> (b : self(b) -> a.@(b)) -> Type



A : Type, B(a : A) |- (b : B(a)) -> Type : Type

x : A, Γ[x] | x |- A : Type
--------------------------
Γ[A] |- A : Type



A : Type, a : A, B : (a : A) -> Type |- (b : B(a)) -> Type : Type

A : Type, B : (a : A) -> Type, a : self(a : A) -> (b : B(a)) -> Type | B(a) |-
  self(b) -> a.@(b) : Type
---------------------------------------------
|- self(a) -> (b : self(b) -> a.@(b)) -> Type : Type



Γ |- M : A  Γ | M |- N : A
--------------------------
Γ, M == N

A : Type, B : (a : A) -> Type, A == self(a) -> (b : B(a)) -> Type |-

//
Γ, x : self(x) -> T |- T : Type
-------------------------------
Γ |- self(x) -> T : Type

Γ, x == fix(x) : T => M |-
-----------------------------------
Γ |- fix(x) : T => M : self(x) -> T

Γ |- M : self(x) -> T
---------------------
Γ |- M : T{x := M}


A : Type, x : A |- T : Type  (x ∉ fv(T))
---------------------------------------- // generalize
x : self(x) -> M |- T : Type
----------------------------------------
|- self(x) -> T : Type


// using built-in pairs for simplicity
(fix(P) => [
  self(a) -> (b : P.B(a)) -> Type,
  (a) => self(b) -> a.@(b)
] : self(P) -> [A : Type, B : (a : A) -> Type]);

A{P} := self(a) -> (b : P.B(a)) -> Type
B{P} := (a) => self(b) -> a.@(b)

// focus on left

-----------------------------------------------------------------------------
|- fix(P) => [A{P}, B{P}] : self(P) -> [A : Type, B : (a : A) -> Type]


//
fix(False) : Type => self(f) -> (C : (f : False.@) -> Type) -> C(f)



F(T : Type, x : T) = (C : (f : T.@) -> Type) -> C(x)

Γ, x : A, Δ[x] |- M : B
Γ |- N : A
-----------------------
Γ, Δ[N] |-


X : _, T == fix(T) => X(T), x : X(T) |- x : T.@ // generalized

self(x) -> F(T.@)(x) : Type
------------------------------------------------------------------------
T == fix(T) => self(x) -> F(T.@)(x), x : self(x) -> F(T.@)(x) |- x : T.@

T : self(T) -> Type |- F(T.@) : (x : T.@) -> Type
T == fix(T) => self(x) -> F(T.@)(x), x : self(x) -> F(T.@)(x) |- x : T.@
-------------------------------------------------------------------------
|- fix(T) => self(x) -> F(T.@)(x)

S_x[T] := self(x) -> T_x[T, x]
T_x[T, x] := (C : (f : T.@) -> Type) -> C(x)



(T == fix(T) => T_x[T]) |- T_x[T] : Type // generalize

X : _, T == fix(T) => X(T), x : X(T) |- (C : (f : T.@) -> Type) -> C(x) : Type
------------------------------------------------------------------------------
X : _, T == fix(T) => X(T), x : X(T) |- T_x[T, x] : Type
X : _, T == fix(T) => X(T), x : X(T) |-
--------------------------------------------------------
(T == fix(T) => S_x[T]), x : S_x[T] |- T_x[T, x] : Type
-------------------------------------------------------
(T == fix(T) => S_x[T]) |- self(x) -> T_x[T, x] : Type
------------------------------------------------------
|- fix(T) => S_x[T] : self(T) -> Type




Γ | S.0 |- M : A  Γ, S.0 == M | S.1 |- N : B(M)
-----------------------------------------------
Γ | S |- [M, N] ⇐ [a : A, b : B(a)]


// affine
Bool : Type;
true : Bool;
false : Bool;
case(b : Bool, {C}, t : C(true), f : C(false)) : C(b);

Bool = self(b) -> (
  C : Type $ 0,
  t : C $ case(b, 0, 1),
  f : C $ case(b, 1, 0)
) -> C;


//
f(a : A) : A;


Nu(F : (X : Type) -> Type) : Type = [
  X : Type,
  f : (x : X) -> F(X),
  x : X
];

C_Eq(A, x, y) = (C : (a : A) -> Type, r : C(x)) -> C(y);
c_refl[A, x] : C_Eq[A, x, x] = (C, r) => r;
c_next =

Eq(A, x, y) : Type;
Eq(A, x, y) = [
  c : C_Eq(A, x, y),
  e : Eq(
    [z : A, C_Eq(A, x, z)],
    [x, c_refl(A, x)],
    [y, c]
  )
];
refl(A, x) : Eq(A, x, x);
refl(A, x) = [
  c_refl(A, x),
  refl(_, [x, c_refl(A, x)])
];
//
Eq(A, x, y) : Type = [
  X : (A : Type, x : A, y : A) -> Type,
  f : (x : X(A, x, y)) -> [
    c : C_Eq(A, x, y),
    e : X(
      [z : A, C_Eq(A, x, z)],
      [x, c_refl(A, x)],
      [y, c]
    )
  ],
  x : X(A, x, y)
];
refl(A, x, y) : Eq(A, x, x) = [
  X = C_Eq(A, x, y),
  f = (c) => _,
  x = c_refl()
];

S = [
  A : Type,
  f : (a : A) -> A,
  f_I : (a : A) -> f(f(a)) == f(a),
  f_J : (a : A) -> ap(f, f_I(a)) == f_I(f(a)),
];
next(P : S, a : P.A) : S = [
  A = f(a) == a,
  f = c_f(P.f_I),
  f_I = c_f_I(P.f_J),
  f_J = c_f_J(P.f_J)
];

Fix(P : S) = [
  X : (P : S) -> Type,
  f : (x : X) -> [
    a : P.A,
    p : X(next(P, a)),
  ],
  x : X(P)
];

Fix(A, f, f_I, f_J) = [
  a : A,
  p : Fix(f(a) == a, c_f, c_f_I, c_f_J)
];

s(a : A) : B;
r(b : B) : A;


left : (a) -> r(s(a)) == f(a);
right : (b) -> r(s(b)) == b;

//

T : Type;
T = F(T);

in(x : F(T)) : T;
out(x : T) : F(T);

left(x : T) : in(out(x)) == x;
right(x : F(T)) : out(in(x)) == x;


Eq(A, x, y) = [
  c : C_Eq(A, x, y),
  k : (w : W_Nat) ->
    [A, f, a] = w.c(
      [A : Type, f : (a : A) -> A, a : A]
      ,
      (h)
    );
    f(a) == a
];

f(0) : c_next(c) == c
f(1) : c_f(f(0)) == f(0)
f(2) : c_f(f(1)) == f(1)

// (n : Nat) -> T(n)
F(T : (n : Nat) -> Type) : Type;
// TODO: self(f) -> (n : Nat) -> T(n, f(n - 1))?
F(T) = [l : T(0), r : F((n) => T(1 + n))];

// (n : Nat) -> T
F(T : Type) : Type;
F(T) = [l : T, r : F(T)];

//
T : Type;

A = (n : Nat) -> T;

X : Type;
X = [l : T, r : X];

s(n : Nat, a : A) : X;
s(n : Nat, a) = [a(n), s(1 + n, a)];
s(a) = s(0, a);

r(x : X) : A = (n) =>
  rec(n, X, x, (x) => x.r).l;

f(a : A) : A = r(s(a));

f_I(a : A) : f(f(a)) == f(a);
// TODO: is this true?
f_I(a) = _;

//
A = (n : Nat) -> T;

X : Type;
X = [l : T, r : X];

s(n : Nat, a : A) : X;
s(n : Nat, a) = [a(n), s(1 + n, a)];
s(a) = s(0, a);

r(x : X) : A = (n) =>
  rec(n, X, x, (x) => x.r).l;

f(a : A) : A = r(s(a));

funext(l, r, p : (n) -> f(l)(n) == f(r)(n)) : s(l) == s(r);
funext(l, r, p) = [
  p(0),
  _
];
Fix : Type;

// universe of inductive types
U : Type;
self : U;
either(A : U, B : U) : U;
arrow(A : U, B : U) : U;

U = self(T) -> (
  C : (T : U) -> Type,
  s : C(self),
  e : (A : U, B : U) -> C(either(A, B)),
  a : (A : U, B : U) -> C(arrow(A, B)),
) -> C(T);
self = (C, s, e, a) => s;
either(A, B) = (C, s, e, a) => e(A, B);
arrow(A, B) = (C, s, e, a) => a(A, B);

Constructor : Type;
El(T : U) : Type;
El_P(T : U, C : (x : El(T)) -> Type) -> Type;
El_N();

Constructor = _;
El(T) = self(x) -> (C : (x : El(T)) -> Type) -> El_P(T, C, ?);
El_P(T, C, x) =
  T(
    (T) => Type,
    C(x),
    (A, B) => [l : El_P(A, C, _), r : El_P(B, C, _)],
    (A, B) => (x : El_N(A, C, ?) -> El_P(A, C, ?)
  );
El_N(T, C, x) = T(
  (T) => Type,
  ?,
  (A, B) =>
    l_x = ;
    [l : El_N(A, C, _), r : El_N(B, C, _)],
  (A, B) => (x : El_P(A, C, _)) -> El_N(B, C, _)
);

Unit = self(x) -> (C) ->
  (p : C((C, t) => p)) -> C(x);

arrow(either(self, self), self)
Bool = self(x) -> (C) ->
  (p : [
    t : C((C) => (p) => p.0),
    f : C((C) => (p) => p.1)
  ]) -> C(x);

Nat = arrow(either(self, arrow(self, self)), self);
Nat = self(x) -> (C) ->
  (p : [
    z : C((C, [z, s]) => z),
    s : ([n : Nat, IH : C(n)]) ->
      C((C, [z, s]) => s(n, n(C, [z, s])))
  ]) -> C(x);

Nat = self(x) -> (C) ->
  (p : [
    z : C((C, [t, f]) => t),
    s : self(r) -> ([n : Nat, IH : C(n)]) ->
      C((C, [z, s]) => s(n, n(C, [z, s])))
  ]) -> C(x);

arrow(either(self, self), self)

arrow(self, self)

Stream(A) : Type;
Stream(A) = self(s) -> (C : (s : Stream(A)) -> Type) ->
  (c : (hd : A, tl : Stream(A)) -> C((C, c) => c(hd, tl))) -> C(s);


(C : Type) -> (p : (s : [l : C, r : C]) -> C) -> C

Nat : Type;
Nat = self(n) -> (
  C : (n : Nat) -> Type,
  z : C(zero),
  s : ([n : Nat, IH : C(n)]) ->
    C((C, z, s) => s(n, n(C, z, s))))
) -> C(n);

Unit : Type;
unit : Unit;

Unit = self(u) -> (
  C : (u : Unit) -> Type,
  r : C(unit)
) -> C(u);
unit = (C, r) => r;



Mu(F) = (C : Type, c : (x : F(F))) -> C) -> C(Mu(F));

[T : (u : C_Unit) -> Type, c : C_Unit, ]

T_Unit = self(Unit) -> (unit : self(unit) -> Unit.@(unit)) -> Type;
fold(M) : T_Unit


(unit == u) == (fold(unit) == fold(u))

Unit : T_Unit = (unit) =>


M_Unit = self(p) -> [
  c : C_Unit,
  m : self(m) -> Eq(
    [c : C_Unit, m : self(m) -> Eq()],
    _,
    _
  );
];


Unit : Type;
unit : Unit;

in : self(in) -> (u : self(u) -> Eq(Unit, unit, in.@(u))) -> Unit;
out(u : Unit) : self(u) -> Eq(Unit, unit, in.@(u));

Bool = self(p) -> [
  c : C_Bool,
  b : c(_, [c_true, refl()], [c_false, refl()]) == p
];
unit = _;

M_Unit(
  Unit : Type,
  unit : Unit,
  u : Unit,
) = (C, r : C(unit)) -> C(u);
Unit = [
  n : Nat,
  c : n.fold(
    [
      Unit : Type,
      unit : Unit,
    ],
    [
      M_Unit(
        C_Unit,
        c_unit,
        c_unit
      )
    ],
    ([Unit, unit]) =>
      M_Unit(Unit, unit, u)
  )
];

out(n2 : Nat2) : M_Nat(Nat2) =
  (C, z, s) =>
    n2.n.ind(
      (n) => _,
      (c : Nat(False)) => (C, z, s) =>
        c(C, z, (p) => s(p(Nat2))),
      _
    );

M.1 : (C : (c : C_Unit) -> Type, c : C(c_unit)) -> C(M.0)

in(u : self(u) -> Eq(Unit, unit, in(u))) =
  (_M[u] : _Unit);


in(u : self(u) -> Eq(_Unit, _unit, in(u))) =
  u.@ : Eq(_Unit, _unit, in(u));
out(u : Unit) : self(u) -> Eq(Unit, unit, in.@(u)) = (u) => unit;




Nu(F) = (
  C : Type,
  c : (x : F(C)) -> C
) -> C;

Nat : Type;
Nat = (C : Type, z : C, s : (n : Nat) -> C) -> C;

T(α : Size) : Type;
T(α) = (x : (β < α) -> T(β)) -> Type;

x(α) : T(α);
// bad because?
x(α) = (x) => () -> x(?)(x);

(C : Type) -> (c : (x : (x : C) -> Type) -> C) -> C

T : Type;
T = (x : T) -> Type;

T = [
  G : Grade,
]
x : T;
// bad because?
x = (x) => () -> x(?)(x);

C_Nat = [
  G : Grade,
  f : (
    C : Type,
    z : C,
    s : (n : C) -> C $ G
  ) -> C
];
c_next(c_n) = _;
Nat = [c_n : C_Nat, p : c_next(c_n) == c_n];


fix(
  C : Type,
  w : (f : (m : Nat, p : 1 + m == n) -> C) -> C,
  n : Nat,
) : C;

T(α : Size) : Type;
T(α) = (x : (β < α) -> T(β)) -> Type;

T = fix(Type, (T) =>
  (x : (m : Nat, p : 1 + m == n) -> T(m, p)) -> Type)

K_Eq(A, f, f_I, f_J, a : A) : Type;
K_Eq(A, f, f_I, f_J, a) = [
  c : C_Eq(f(a), a),
  k : K_Eq
];

Fix(A, f, f_I, f_J) : Type;
Fix(A, f, f_I, f_J) = [
  a : A,
  k : Fix(C_Eq(f(a), a), c_f, c_f_I, c_f_J)
];

Eq(A, x, y) : Type;
Eq(A, x, y) = [
  c : C_Eq(A, x, y),
  k : Eq([z : A, c : C_Eq(A, x, z)], [x, c_refl(A, x)], [y, c])
];

Eq(A, x, y) : Type = (w_n : W_Nat) =>
  w_n.c(
    _,
    (A, x, y) => C_Eq(A, x, y),
    (F) => (A, x, y) => [
      c : C_Eq(A, x, y),
      k : F([z : A, C_Eq(A, x, z)], [x, c_refl(A, x)], [y, c])
    ]
  )(A, x, y);



Eq(A, x, y)(2)

[
  c : C_Eq(A, x, y),
  k :
    A = [z : A, C_Eq(A, x, z)];
    x = [x, c_refl(A, x)];
    y = [y, c];
    [
      c : C_Eq(A, x, [y, c]),
      k : [z : A, C_Eq(A, x, z)]
    ]
];


Fix(A, f, f_I) = (w_n) =>
  w_n.c(
  | 0 => (A, f, f_I, f_J) => A
  | [1 + m, F] =>
    (A, f, f_I, f_J) =>
      [a : A, k : F(C_Eq(f(a), a), c_f, c_f_I, c_f_J)]
  )(A, f, f_I, f_J);

Fix(A, f, f_I, f_J) = (w_n) =>
  w_n.c.rec(
  | 0 => (A, f, f_I, f_J) => A
  | [1 + m, F] =>
    (A, f, f_I, f_J) =>
      [a : A, k : F(C_Eq(f(a), a), c_f, c_f_I, c_f_J)]
  )(A, f, f_I, f_J);

Fix(A, f, f_I, f_J)(2)
[a : A, k : [a : C_Eq(f(a), a), k : C_Eq(c_f(a), a)]]


  [
  c : C_Eq(f(a), a),
  k : (w : W_Nat) ->
    [A, f, a] = w.c(
      [A : Type, f : (a : A) -> A, a : A]
      ,
      (h)
    );
    f(a) == a
];

Bool = [
  c : C_Bool,
  p : (w_n : W_Nat) ->
    w_n.c.is(
    | 0 => c_next(c) == c
    | 1 +
    );
];


fix(
  C(n : Nat, x : C(n, x)) : Type,
  w(
    n : Nat,
    f : (m : Nat, p : 1 + m == n) -> C
  ) -> C,
  n,
) : C;


fix(
  C(n : Nat, x : C(n, x)) : Type,
  w(
    n : Nat,
    f : (m : Nat, p : 1 + m == n) -> C(m, fix(C, w, m))
  ) -> C(n, fix(C, w, n)),
  n,
) : C(n, fix(C, w, n));

//
fix(
  C(n : Nat, x : C(n, x)) : Type,
  w(
    n : Nat,
    f : (m : Nat, p : 1 + m == n) -> C(m, fix(C, w, m))
  ) -> C(n, fix(C, w, n)),
  n,
) : C(n, fix(C, w, n));


A : Type;
A = self(a : A) -> B(A, a);

fix(
  C : Type,
  w : (f : (m : Nat, p : 1 + m == n) -> C) -> C,
  n : Nat,
) : C;

x : A;
y : B(a);

M_x(x : A, y : B(a)) : A;
M_y(x : A, y : B(M_x(x, y)), H : M_x(x, y) == x) : B(x);

M_Unit(Unit : Type, unit : Unit) = [
  u : Unit,

]

A : Type;
x : B(a);


Bool(n : Nat) : Type;
true(n) : Bool(n);
false(n) : Bool(n);

Bool(n) =
  n.is(
  | 0 => C_Bool
  | 1 + m =>
    [
      b : Bool(m),
      i : (
        C : (n : Bool(m)) -> Type,
        t : C(true(m)),
        f : C(false(m))
      ) -> C(b(m)),
      k : i((_) => Bool(m), true(m), false(m)) == b
    ];
  );
true(n) =
  n.is(
  | 0 => c_true
  | 1 => (b, C, t, f) =>
    b(1)(b, C, t, f)
    C(b(0))
  | 1 + m => (b, C, t, f) =>
    b(0)
  );
//
Bool = (n : Nat) -> Bool(n);
true = (n : Nat) => true(n);
false = (n : Nat) => false(n);

case(b : Bool, C : Type, t : C, f : C) : C
  b(0)(C, t, f);
ind(b : Bool) : case(b, _, true, false) == b;
ind(b) = (n) =>
  n.is(
  | 0 => b(1).i

  )


//


[c_true, i_true]
Bool(w_n : W_Nat) : Type =
  w_n.c(
    _,
    (Bool, true, false) => Bool,
    (F) => (Bool, true, false) => [
      b : Bool,
      k : F(
        [b : Bool, (C : (b : Bool), t : C(true), f : C(false)) -> C(b)],
        [true, (C, t, f) => t],
        [false, (C, t, f) => f]
      )
    ]
  )(C_Bool, c_true, c_false);

Bool(2)

1(
  _,
  (Bool, true, false) => Bool,
  (F) => (Bool, true, false) => [
    b : Bool,
    k : F(
      [b : Bool, (C : (b : Bool), t : C(true), f : C(false)) -> C(b)],
      [true, (C, t, f) => t],
      [false, (C, t, f) => f]
    )
  ]
)
(
  (F) => (Bool, true, false) => [
    b : Bool,
    k : F(
      [b : Bool, (C : (b : Bool), t : C(true), f : C(false)) -> C(b)],
      [true, (C, t, f) => t],
      [false, (C, t, f) => f]
    )
  ]
)(Bool, true, false) => Bool

((F) => (Bool, true, false) => [
    b : Bool,
    k : F(
      [b : Bool, (C : (b : Bool), t : C(true), f : C(false)) -> C(b)],
      [true, (C, t, f) => t],
      [false, (C, t, f) => f]
    )
  ])(1(
  _,
  (Bool, true, false) => Bool,
  (F) => (Bool, true, false) => [
    b : Bool,
    k : F(
      [b : Bool, (C : (b : Bool), t : C(true), f : C(false)) -> C(b)],
      [true, (C, t, f) => t],
      [false, (C, t, f) => f]
    )
  ]
))(C_Bool, c_true, c_false)

Bool(n : Nat) : Type;
true(n) : Bool(n);
false(n) : Bool(n);

Bool(n) = [
  b : (m < n) -> Bool(m),
  i : (C, t : C(true), f : C(false)) -> C(b)
];
true(n) =
  n.is(
  | 0 => c_true
  | 1 + m => [true, (C, t, f) => t]
  );
false(n) =
  n.is(
  | 0 => c_false
  | 1 + m => [false, (C, t, f) => f]
  );

Bool = [
  c : (n : Nat) -> Bool(n);
  r : (n : Nat) -> c(1 + n).b(n) == c(n)
];
mod(b : Bool) : ((n : Nat) => b(1 + n)) == b;
mod(b) = [
  (n) => _ : ,
  _
]
  (n) =>
  n.is(
  | 0 => _
  | 1 + m => _
  );
  b(1 + n).i(
    (x) => x,
    () => _,
  )
  n.is(
  | 0
  )
mod(b, p) = (n) =>
  b(1 + n).b == b(n)
  n.is(
  | 0 => p
  | 1 + m =>

    _ : b(1 + m) == true(1 + m)
  );

_ : b(1 + m).i == true(1 + m).i

//
Eq(A : Type, x : A, y : A) : (n : Nat) -> Type;
refl(A : Type, x : A) : (n) -> Eq(A, x, x, n);

Eq(A, x, y) = (n) => [
  c : C_Eq(A, x, y),
  k : (m < n) ->
    Eq(
      C_Eq(A, x, y),
      coe(c, (z) => C_Eq(A, x, z), c_refl(A, x)),
      c
    )(m)
];
refl(A, x, y) = (n) => [
  c_refl(A, x),
  (m < n) => refl([z : A, C_Eq(A, x, z)], [x, c_refl(A, x)])(m)
];

mod(A, x, y, eq : Eq(A, x, x)) : Eq(_, [x, refl(A, x)], [y, eq]);
mod(A, x, y, eq) =
  eq.c

W_Eq(A, x, y) = [
  c : (n : Nat) -> Eq(A, x, y)(n);
  r : (n : Nat) ->
    Eq(
      C_Eq(A, x, y),
      c(1 + n).c,
      c(n).c
    )
];
c(1 + n).k(n) : Eq()

//
Eq(A : Type, x : A, y : A) : Type;
refl(A : Type, x : A) : Eq(A, x, x);

Eq(A, x, y) = [
  c : C_Eq(A, x, y),
  k :
    Eq(
      [z : A, C_Eq(A, x, z)],
      [x, c_refl(A, x)],
      [y, c]
    )
];
refl(A, x) = [c_refl(A, x), refl(A, x)];
mod(A, x, eq : Eq(A, x, x)) : Eq(_, [x, refl(A, x)], [x, eq]);
mod(A, x, eq) =
  eq.k.c(
    ([z, c_eq]) => (k) -> Eq(_, [x, refl(A, x)], [z, [c_eq, k]]),
    (k) => _
  );
mod(A, x, y, eq : Eq(A, x, y)) : Eq(_, [x, refl(A, x)], [y, eq]);
mod(A, x, y, eq) =
  eq.k.c(
    ([z, c_eq]) => (k) -> Eq(_, [x, refl(A, x)], [z, [c_eq, k]]),
    (k) =>
      mod(_, _, _, k)(

      )
    _ : (k) -> [x, [c_refl(A, x), refl(A, x)]] == [x, [c_refl(A, x), k]]
  )(eq.k);

  [
  eq.c,
  _ : coe(
    eq.c,
    (z) => Eq(A, x, z),
    refl(A, x)
  ) == eq.k
]

coe(
  eq.c,
  (z) => Eq(A, x, z),
  refl(A, x)
)
[
  eq.c,
]

eq.k :

coe(
  eq.c,
  (z) => Eq(A, x, z),
  refl(A, x)
)

eq.k.c : C_Eq()

[
  eq.c,
  coe(
    eq.c,
    (z) => Eq(A, x, z),
    refl(A, x)
  ).k
]
coe(
  eq.c,
  (z) => Eq(A, x, z),
  refl(A, x)
)

Bool = [
  c : (n : Nat) -> Bool(n);
  r : (n : Nat) -> c(1 + n).b(n) == c(n)
];

//
Eq(A : Type, x : A, y : A) : (n : Nat) -> Type;
refl(A : Type, x : A) : (n) -> Eq(A, x, x, n);

Eq(A, x, y) = (n) => [
  c : C_Eq(A, x, y),
  k : (m < n) ->
    Eq(
      C_Eq(A, x, y),
      coe(c, (z) => C_Eq(A, x, z), c_refl(A, x)),
      c
    )(m)
];
refl(A, x, y) = (n) => [
  c_refl(A, x),
  (m < n) => refl([z : A, C_Eq(A, x, z)], [x, c_refl(A, x)])(m)
];


true : (n : Nat $ 0) -> Bool(n);
false : (n : Nat $ 0) -> Bool(n);


Inf(α) : Type;
Inf(α) = (C : Type, c : (β < α, n : Inf(β)) -> C) -> C;

Inf(α) : Type;
Inf(α) = (C : Type, c : (n : (β < α) -> Inf(β)) -> C) -> C;

Nat : Type;
Nat = (C : Type, z : C, s : (n : Nat) -> C) -> C;

fold(n : Nat, C, z, s) : C;
fold(n, C, z, s) = n(C, z, (n) => s(fold(n, C, z, s)));

x : Inf;
x = (C, c) => c(x);

Eq(A : Type, x : A, y : A) : Type;
refl(A : Type, x : A) : Eq(A, x, x);

Eq(A, x, y) = self(s) ->
  (
    C : (z : A, e : Eq(A, x, z)) -> Type $ 0,
    c : C(x, refl(A, x)),
  ) -> C(y, s);
refl(A, x) = (C, c) => c;

T : Type;
T = (x : T) -> Type;

x : T = (x) => () -> x();

x : () -> Type;
x = () => () -> x();

//

T_Unit : Type;
T_unit(Unit : T_unit) : Type;

T_Unit = self(Unit) -> (unit : T_unit(Unit)) -> Type;
T_unit(Unit) = self(unit) -> Unit.@(unit);


T_Unit : Type;
T_Unit =
  T_unit(Unit : T_Unit) : Type;
  T_unit(Unit : T_Unit) : Type = self(unit) -> Unit.@(unit);
  self(Unit) -> (unit : T_unit(Unit)) -> Type;
//
T_Unit : Type;
T_Unit =
  ((T_unit : self(x) -> (Unit : T_Unit) -> Type) =>
    self(Unit) -> (unit : T_unit(Unit)) -> Type)(
    (Unit) =>
      T_unit : Type;
      T_unit = self(unit) -> Unit.@(unit);
      T_unit
  );
  T_unit(Unit : T_Unit) : Type;
  T_unit(Unit : T_Unit) : Type = self(unit) -> Unit.@(unit);
  self(Unit) -> (unit : T_unit(Unit)) -> Type;

Pair(A : Type, B : (a : A) -> Type) : Type;
Pair(A, B) =
  ((fst : (p : Pair(A, B)) -> A) =>
    self(p) -> (C, c : (a : A, b : B(a)) -> C(a)) -> C(fst(p))
  )((p) => p.@((a) => A, (a, b) => a));

Pair(A : Type, B : (a : A) -> Type) : Type;
Pair(A, B) =
  fst(p : Pair(A, B)) : A = p.@((a) => A, (a, b) => a);
  self(p) -> (C, c : (a : A, b : B(a)) -> C(a)) -> C(fst(p));
// the normal form will be something like
Pair(A : Type, B : (a : A) -> Type) : Type;
Pair(A, B) =
  (
    self(p) -> (C, c : (a : A, b : B(a)) -> C(a)) ->
      C(p.@((a) => A, (a, b) => a))
  ) as (
    fst(p : Pair(A, B)) : A = p.@((a) => A, (a, b) => a);
    self(p) -> (C, c : (a : A, b : B(a)) -> C(a)) -> C(fst(p))
  );

// when it reduces
Pair(A, B) =
  fst(p : Pair(A, B)) : A = p.@((a) => A, (a, b) => a);
  self(p) -> (C, c : (a : A, b : B(a)) -> C(a)) -> C(fst(p));

Pair(A, B) =
  self(p) -> (C, c : (a : A, b : B(a)) -> C(a)) -> C(fst(p)) as
    self(p) -> (C, c : (a : A, b : B(a)) -> C(_)) -> C(_);


(self(p) -> (C, c : (a : A, b : B(a)) -> C(a)) ->
  C(p.@((a) => A, (a, b) => a))) as Pair(A, B).@

Unit : Type;
Unit =
  (self(u) -> u == (fix(u) => refl())) as
    (self(u) -> _ == _);

Unit : Type;
Unit = ((unit : Unit) => self(u) -> unit == u)(fix(u) => refl());

(self(u) -> (fix(u) => refl()) == u) as Unit

unit : Unit = fix(u) => (_ as refl());
  fix(unit) => (C,) : refl() == u.@
  _ : (fix(u) => refl()) == u;

unit : T_unit;
unit = fix(unit) => fix(u) => (C, r) => r;


---------------------------
#body(self(x)) |-> (x) => T

unit.@ : self(u) ->
  (C : (u : Unit) -> Type, r : C(unit.@)) -> C(u)

A : Type;
A = self(a) -> (x : A == a) -> B(x);

x : A;
x : (x : A) -> B(x);

//
Pair(A, B) =
  ((fst : (p : Pair(A, B)) -> A) =>
    self(p) -> (C, c : (a : A, b : B(a)) -> C(a)) -> C(fst(p)))
  fst(p : Pair(A, B)) : A = p.@((a) => A, (a, b) => a);
  ;

Γ |- M : (a : A) -> B  Γ, a : A, S == M(a) | a |- N
---------------------------------------------------
Γ | S |- M(N) : B{x := N}

// then this will work
Pair(A, B) =
  ((fst : (p : Pair(A, B)) -> A) =>
    self(p) -> (C, c : (a : A, b : B(a)) -> C(a)) -> C(fst(p)))(
    (p) => p.@((a) => A, (a, b) => a))

//

Unit : Type;
Unit =
  unit : Unit = fix(u) => (C, r) => r;
  self(u) -> (C : (u : Unit) -> Type, r : C(unit)) -> C(u);

// this could also work
Unit : Type;
Unit = self(u) ->
  (C : (u : Unit) -> Type, r : C(fix(u) => (C, r) => r)) -> C(u);

//
Unit : Type;
Unit = self(u) -> ((fix(u) => refl()) : Unit) == u;

// is in the normal form, annotation just makes life easier
fix(Unit) : Type =>
  self(u) -> ((fix(u) => refl()) : Unit) == u;

// but to type it, you may require
fix(Unit) : Type =>
  ((unit : Unit) => self(u) -> unit == u)(fix(u) => refl());
// so in the derivation of the former, you will see the latter

// is in the normal form, annotation just makes life easier
fix(Unit) : Type =>
  self(u) -> ((fix(u) => refl()) : Unit) == u;

// but to type it, you may require
fix(Unit) : Type =>
  unit : Unit = fix(u) => refl();
  self(u) -> unit == u;
// so in the derivation of the former, you will see the latter

//
Γ, a : A |- M : B  Γ |- N : A
-----------------------------
Γ |- M{a := N} : B{a := N}

Γ |- (x) => M : (a : A) -> B  Γ |- N : A
----------------------------------------
Γ |- M{x := N} : A

//
G_Nat(Nat : Type) = [
  b : Bool,
  s : b ? Unit : Nat
];

Nat = (A : Type) -> G_Nat(A);
one : Nat = (A) => [false, _]


in[A](f : G_Nat(A)) : G_Nat(G_Nat(A));
in[A](f) = [
  f.b,
  f.b
    ? []
    : []
];

in[A](f : G_Nat(A)) : G_Nat(G_Nat(A));
in[A](f) = (C, z, s) =>
  f(
    C,
    z,
    (n : A) =>
      s((C, z, s) => _ : G_Nat(A)));


//
Term, M, N, K ::=
  | x
  | (a : A) -> B | (x) => K | M(N)
  | self(x) -> K | fix(x) => K | M.@


P : [Bool : Type, true : Bool, false : Bool];
P = [
  self(b : Bool) -> (C, t : C(true), f : C(false)) -> C(b),
  fix(b) => (C, t, f) => t,
  fix(b) => (C, t, f) => f
];

P(n) : [Bool : Type, true : Bool, false : Bool];
P(n) =
  n.fold(
  | [] => Unit
  | [Bool, true, false] =>
    [
      [b : Bool, (C, t : C(true), f : C(false)) -> C(b)],
      [true, (C, t, f) => t],
      [false, (C, t, f) => f]
    ]
  );



Bool = CoEq((n : Nat) -> P(n).Bool, );
true = (n : Nat) => P(n).true;
false = (n : Nat) => P(n).false;



T_b : Type;
m_a(b : T_b) : A;

T_b = self(b : T_b) -> B(m_a(b));
m_a(b) = M[m_a(b), b.@];

//
P : [T_b : Type, m_a(b : T_b) : A];
P = [
  T_b = self(b : P.T_b) -> B(P.m_a(b)),
  m_a(b) = M[P.m_a(b), b.@];
]

T_a : Type;
T_b(m_a : T_a) : Type;

T_a = self(m_a) -> (m_b : T_b(m_a)) -> Type;
T_b(m_a) = self(m_b) -> B(m_a.@(m_b));

S : Type = [T_a : Type, T_b(m_a : T_a) : Type];
M(P : S) : S = [
  T_a = [m_a : P.T_a, (m_b : P.T_b(m_a)) -> Type];
  T_b(m_a) = [m_b : P.T_b(m_a), B(m_a.1(b))]
];

T = (P : S, m : S == M(S)) -> S;

P : [T_a : Type, T_b(m_a : T_a) : Type];
P = [
  T_a = self(m_a) -> (m_b : P.T_b(m_a)) -> Type;
  T_b(m_a) = self(m_b) -> B(m_a.@(m_b));
];

m_a(a : A, b : B(a)) : A = M[a, b];
m_b(a : A, b : B(a), m : a == M[a, b]) : B[m_a(a, b)] = N[a, b];

(a : A, b : B(a), m : a == M[a, b]) ->
  [r : A, p : ]

  m_a(a, b) == m_a(m_a(a, b), m_b(a, b, m))




T : Type;
T = [x : T, _]

a : A;
a =
  b : B[a];
  b = N[a, b];
  M[a, b];

m_a(b : B(m_a(b))) : A = M[m_a(b), b];

//
S(n : Nat) : Type = [a : A(n), b : B(n, a)];

P(n) : S(n);
P(n) = M[n, (m : Nat, p : n == 1 + m) => P(m)];

S2 : Type = (n : Nat) -> S(n);
P2 : S2 = (n : Nat) =>
  n.ind(
  | 0 => Unit
  | [m, P : S(m)] =>
    M[1 + m, (r, p : 1 + m == 1 + r) => coe(s_inj(p), S, P)]
  );

fix{T}(f(n, g(m, p : n == 1 + m) : T(m)) : T(n))(n : Nat) : T(n) =
  n.ind(
    T,
    z = f(0, g(m, p) = s_neq_z(p)),
    s(m : Nat, IH : T(m)) =
      f(
        1 + m,
        g(r, p : 1 + m == 1 + r) =
          coe(s_inj(p), T, IH)
      )
  );

Bool(n) : Type;
true(n) : Bool(n);
false(n) : Bool(n);

Bool(n) = [
  b(m < n) : Bool(m),
  i : (C, t : C(true), f : C(false)) -> C(b),
];
true(n) = [true, (C, t, f) => t];
false(n) = [false, (C, t, f) => f];

lift{n}(b : Bool(n)) : Bool(1 + n) =
  [b(m < 1 + n) = b.0(m), b.1]

P_Bool = (n : Nat) ->
  [
    r : Bool(n),
    p :
      n.is(
      | 0 => Unit
      | 1 + m => lift(r.0(m)) == r
      )
  ];

p_b : P_Bool;
p_b(1) == p_b(2)

p_b(2) == [
  r : Bool(2),
  p : lift(r.0(1)) == r
];
p_b(2)


Bool : Type;
true : Bool;
false : Bool;

Bool = [b : Bool, i(C, t : C(true), f : C(false)) : C(b)];
true = [true, (C, t, f) => t];
false = [false, (C, t, f) => f];

P_Bool = [b : Bool, x : b.b == b];


//

A : Type;
A = self(x) -> B(x);

// goes to
A1 : Type;
self(a1 : A1) : A1;

A1 = [l : [a1 : A1, b : B(x)], p : self(l.a1) == l.a1];
self(a1 : A1) : A1 = a1.l.a1;

A = [a1 : A1, p : self(a1) == a1];


in_(a1 : A1) : A = [a1.l.a1, a1.p];
out(a : A) : A1 := a.a1;

out(in_(a1)) == out([a1.l.a1, a1.p])

in_(out(a)) == [a.a1.l.a1, a.a1.p]

mod(a : A) : [a.a1.l.a1, a.a1.p] == a;
mod(a) = [
  a.p,
  _ : coe(a.p, C_Eq(self), a.a1.p) == a.p
]


a == [self(a.a1), ap(self, a.p)]

in_(out(a)) == [a1.a1, a1.p]


ind(
  C(c : Circle) : Type,
  b : C(base),
  m : coe(loop, C, b) == b
)


T = ∀(C : Set) -> ((Set) -> C) -> C;
// there is no such an injective function
fst : T -> Set;

I_Eq{A}(f(a : A) : A) : Type;
in{A, f}(a : A) : I_Eq{A}(f);
glue{A, f}(a : A) : in(f(a)) == in(a);

I_Eq{A}(f) = self(x) -> (
  C(x : I_Eq(f)) -> Type,
  c(a) : C(in(a)),
  g(a) : coe(glue(a), C, c(f(a))) == c(a)
) -> C(x);
in{A, f}(a) = (C, c, g) => c(a);
glue{A, f}(a) = (C, c, g) => g(a);

//
Sigma(A, B) : Type;
fst{A, B}(s : Sigma(A, B)) : A;

Sigma(A, B) = ι(s) ->
  Π(b : Bool) -> b ? A : B(fst(s));
fst{A, B}(s) = s(true);

//
I_Eq{A}(f(a : A) : A) : Type;
in{A, f}(a : A) : I_Eq{A}(f);
glue{A, f}(a : A) : in(f(a)) == in(a);

I_Eq{A}(f) = self(x) -> (
  C(x : I_Eq(f)) -> Type,
  c(a) : C(in(a)),
  g(a) : coe(glue(a), C, c(f(a))) == c(a)
) -> C(x);

in{A, f}(a) = λ(i) = [
  a,
  p =
    ind(
      i,
      C(i) = in(a) == i ? in(f(a)) : in(a) % glue(a),
      sym(glue(a)),
      refl(),
      _ : coe(seg, _, sym(glue(a))) == refl()
    )
];
glue{A, f}(a) = λ(i) => _ : ;


Either(A, B) = [b : Bool, p : b ? A : B];

K_Eq(A, f(a : A) : A, a : A) : Type;
k_f{A, f, a}(k : K_Eq(A, f, a)) : K_Eq(A, f, a);
k_f_I{A, f, a}(k : K_Eq(A, f, a)) : k_f(k_f(k)) == k_f(k);

K_Eq(A, f, a) = self(k) -> [
  c : f(a) == a,
  p : k_f(k) == k,
];
k_f{A, f, a}(k) = [c_f(k.@.c), k_f_I(k)];
k_f_I{A, f, a}(k) =
  [
    c_f_I() : c_f(c_f(k.@.c)) == c_f(k.@.c),
    _ :
  ]
  coe(
    k_f_I(k),
    C(k) = k_f(k) == k;
    k_f_I(k_f(k))
  ) == k_f_I(k)

Fix(A, f(a : A) : A) : Type;
fix{A, f}(a : A) : Fix(A, f);
mod{A, f}(a : A) : fix(f(a)) == fix(a);

Fix(A, f) = self(x) -> [a : A, p : fix(a) == x];
fix{A, f}(a) = [f(a), mod(a)];
mod{A, f}(a) =
  coe(
    mod(a),
    C(x) = [a : A, p : fix(a) == x]
    [f(f(a)), mod(f(a))]
  ) == [f(a), mod(a)]
fix(f(a)) == fix(a)


Γ, x : self(x) -> T |- T : Type
-------------------------------
Γ |- self(x) -> T : Type


y : self(x) -> T
y.@ : T{x := y}

y.@ : (a : A) -> B

T == A
y.@(M : T)

Γ |- M : A  Γ |- A == B
-----------------------
Γ |- M : B


self(x) -> M(N)

self(s) -> ((x : A) => x(x))((x : A) => x(x))

self(x) -> A -> B

self(T) -> (() => (x : self(y) -> T.@(y)) -> Type)()

T.@ : (() => (x : self(y) -> T.@(y)) -> Type)()

#ret((() => (x : self(y) -> T.@(y)) -> Type)()) == Type

(() => (x : self(y) -> T.@(y)) -> Type)() ==
T.@ : (x : self(y) -> T.@(y)) -> Type

self(T) -> (() => (x : self(y) -> T.@(y)) -> Type)()

#param(T.@) == self(y) -> T.@(y)
#typeof(T.@(y)) == Type

#param((() => (x : self(y) -> T.@(y)) -> Type)().@) == self(y) -> T.@(y)

_A == true;


b{T} := self(y) -> T.@(y);
a := self(T) -> (x : b{T}) -> Type;

b{T} := self(y) -> T.@(y);
a := self(T) -> (x : b{T}) -> Type;

b[T] := self(y) -> T.@(y);
r[T] := (x : b[T]) -> Type;
a := self(T) -> r[T];

//
b[T] := self(y) -> T.@(y);
r[T] := (x : b[T]) -> Type;
a := self(T) -> r[T];

//
Type, A, B ::=
Term, M, N, K ::=
  | Type(l) | (M ⇒ A) | (M ⇐ A) | x
  | (x : A) -> B | (x : A) => K | M(N)
  | self(x) -> T | fix(x : A) => T | M.@

(x : A) -> C{x} |-> (x : A) -> C{x ⇒ A}
(x : A) => C{x} |-> (x : A) => C{x ⇒ A}
self(x) -> C{x} |-> self(x) -> C{x ⇒ [self(x) -> C{x}]}

Type(l) |-> Type(l) ⇒ Type(1 + l)

(x : A ⇒ Type(l)) -> (B ⇒ Type(r)) |-> (x : A) -> B ⇒ Type(max(l, r))
(x : (A ⇒ Type(l))) => (M ⇒ B) |-> (x : A) => M ⇒ (a : A) -> B
(M ⇒ (a : A) -> B)(N) |-> M(N ⇐ A) ⇒ B{x := (N ⇐ A)}

self(x) -> (T ⇒ Type(l)) |-> self(x) -> T ⇒ Type(l)
(M ⇒ self(x) -> T).@ |-> M.@ ⇒ T{x := M}

//
[Type] |-> Type
[(x : A) -> B] |-> (x : [A]) -> [B]
[self(x) -> T] |-> self(x) -> [T]

[x] |-> x
[M(N)] |-> [M]([N])

[M ⇒ A] |-> M



self(T) -> (y : self(x) -> T.@(x)) -> Type
self(T) -> (y : self(x) -> T.@(x ⇐ [self(x) -> T.@(x)])) -> Type

[(x : A) => K] |-> (x : [A]) => [B]



P(n) : [
  Bool : Type;
  true : Bool;
  false : Bool;
];
P(n) =
  n.fold(
    [C_Bool, c_true, c_false],
    s([Bool, true, false]) =
      [b : Bool, i(C, t : C(true), f : C(false)) : C(b)]
  );

Bool(n) : Type;
true(n) : Bool;
false(n) : Bool;

Bool(n) = [
  b(m == n - 1) : Bool(m),
  i(C, t : C(true), f : C(false)) : C(b)
];
true(n) = [true, i(C, t, f) = t];
false(n) = [false, i(C, t, f) = f];

S_Bool = self(x) -> Π(n : Nat) -> [
  b : Bool(n),
  p :
    n.is(
    | 0 = Unit
    | 1 + n = x.@(n).b == b.b(n)
    );
];
s(n) : S_Bool(n)
s(1 + n) = [
  b : Bool(1 + n),
  p : x.@(n).b == b.b(n)
]

  n.is(
  | 0 => [b : Bool(0), p : ]
  )
  [
  b : Bool(n),
  p : x.@(n).b == b
];

b(n) : Bool(n)
b(n) == b(1 + n)

mod(b)

b(1).b(?) == b(0)


b(1 + n).i(_, _) : C(b(1 + n).b)
Bool(1) == Bool(0)

P(n) = [
  Bool = [b :]
]
  n.fold(
    [C_Bool, c_true, c_false],
    s([Bool, true, false]) =
      [b : Bool, i(C, t : C(true), f : C(false)) : C(b)]
  );

Bool : Type = Π(n) : P(n).Bool;
true : Bool = (_(n) = P(n).true);
false : Bool = (_(n) = P(n).false);

shift(M(0) : C_Bool) : [c : C_Bool, i : I_Bool(c)];
mod : shift(M(0)) == M(1);

shift(M : T(n)) : T(1 + n);
mod(M(n)) : shift(M(n)) == M(1 + n)
b : Bool;
b(1)

Bool : Type;
true : Bool;
false : Bool;

Bool = [
  b : Bool,
  i(C(b : Bool) : Type, t : C(true), f : C(false)) : C(b)
];
true = [true, (_(C, t, f) = t)];
false = [false, (_(C, t, f) = f)];


Γ |- T : Type  Γ, x : T |- fst(x) : T
-------------------------------------
Γ |- Self(T) : Type

Γ |- M : T  Γ |- fst(M) == M
----------------------------
Γ |- fold(M) : Self(T)

Γ |- M : Self(T)
------------------
Γ |- unfold(M) : T

------------------------
Γ |- fst(unfold(M)) |-> M

unfold(M) |-> [M, snd(M)]

self(x) -> [b : Bool, ]

C_Bool = (Π(C : Type, t : C, f : C) : C);

I_Bool(c : C_Bool) = (Π(C, t : C(c_true), f : C(c_false)) : C(c));

f(p : [c : C_Bool, i : I_Bool(c)]) : C_Bool = fst(p);
g(c : C_Bool) : [c : C_Bool, i : I_Bool(c)] =
  c(_, [c_true, i_true], [c_false, i_false]);

f(g(c)) ==


fold(
  i_true
)

S_Bool = self(x) -> [
  b : Bool,
  p : (x.@ : [
    b : Bool,
    p : _
  ]) == [true, refl()]
];
s_true = fold(true);
s_false = fold(false);

ind(
  s : S_Bool,
  C : Type,
  c(b : Bool, p : b.b == b) = _;
) :

P_Stream(n, A : Type) : Type;
P_Stream(n, A) = [
  hd : A,
  tl(m == n - 1) : P_Stream(m, A)
];

Stream(A : Type) = Π(n : Nat) -> P_Stream(n, A);


Mu(F : Set -> Set) : Set =
  Π(X : Set) -> (F(X) -> X) -> X;
// but if you instantiate it with
Mu(F(X) = X -> X)
// it doesn't behave how you would expect

// this works
Term = Π(C : Set) -> (C -> C -> C) ->
  ((C -> C) -> C) -> C;



//
Unit_S : Type = (Unit : [A : Type, roll : Unit.@(A, roll) -> A] -> Type)
roll_S(A : Type) : Type =
  (roll : (a : Π(C(a : A) : Type) -> C(roll((@ = λ(C, u) => u))) -> C(roll(a))) -> A);
Unit : Unit_S = (@ = λ(A : Type, roll : roll_S(A)) =>
  (a : Π(C(a : A) : Type) -> C(roll((@ = λ(C, u) => u))) -> C(roll(a))));
roll_S(A : Type) : Type = (roll : Unit.@(A, roll) -> A);

roll : roll_S(W_Unit) = (@ = λ(a) => a.@(C(_) = W_Unit, w_unit));

M_Unit : Type = Unit.@(W_Unit, roll);

unit : M_Unit;


unit.@(C(_) = W_Unit, w_unit).1 : I_Unit(unit.@(C(_) = W_Unit, w_unit).0)

roll(unit) == unit
unit.@ : Π(C) -> C(w_unit) -> C(roll(unit));

unit : Unit;

unit(C(_) = Type) == (@ = λ(A) => A)

unit(C(f) = f == (@ = λ(a) => a)).@(refl()) :
  u == (@ = λ(a) => a)

Π(C : (u : C((@ = λ(u) => u)) -> C(u)) -> Type) ->
    (u : C((@ = λ(u) => u)) -> C(u))

Pair_S = (Pair : (fst : Pair.@(fst) -> A) -> Type);
fst_S = (fst : (p : Π(C, c(a : A, b : B(a)) : C(a)) -> C(fst(p))) -> A);

Pair : Pair_S = (@ = λ(fst : fst_S) =>
  (p : Π(C, c(a : A, b : B(a)) : C(a)) -> C(fst(p))))

(fst : Pair.@(fst) -> A) -> Type

fst : (p : Π(C, c(a : A, b : B(a)) : C(a)) -> C(fst(p))) -> A

(@ = λ(fst : (p : Π(C, c(a : A, b : B(a)) : C(a)) -> C(fst(p))) -> A) =>
  (p : Π(C, c(a : A, b : B(a)) : C(a)) -> C(fst(p))))

Pair : Pair_S = (@ = λ(fst) =>
  (p : Π(C, c(a : A, b : B(a)) : C(a)) -> C(fst(p))));
Pair(fst : M(fst)) =
  (p : Π(C, c(a : A, b : B(a)) : C(a)) -> C(fst(p)));

(Pair : (fst : Pair(fst)) -> Type)

fold()

Bool(P : [true : Bool(P), false : Bool(P)]) =
  (b : Π(C(b : Bool(P)) : Type) -> C(P.true) -> C(P.false) -> C(b));


Pair{A}(B(a : A) : Type) : Type;
fst{A, B}(p : Pair{A}(B)) : A;

Pair{A}(B) = (p : Π(C, c(a : A, b : B(a)) : C(a)) -> C(fst(p)));
fst{A, B}(p) = p.@(c(a, b) = a);

//
pair{A, B}(a : A, b : B(a)) : Pair{A}(B);
mod{A, B}(a, b) : a == fst(pair(a, b));

pair{A, B}(a, b) = λ(C, c) => coe(mod(a, b), C, c(a, b));
// this refl is only well typed if you can reduce equality
// a == mod(a, b)(C(_) = A, a)
mod{A, B}(a, b) = λ(C, c) => c;


// this can work but you need a weird assumption
pair{A, B}(a : A, b : B(a)) : Pair{A}(B);
mod{A, B}(a, b) : a == fst(pair(a, b));

pair{A, B}(a, b) = λ(C, c) => coe(mod(a, b), C, c(a, b));
// this refl is only well typed if you can reduce it
// a == coe(mod(a, b), C(_) = A, a)
mod{A, B}(a, b) = refl();

//
Pair{A}(B(a : A) : Type) : Type;
fst{A, B}(p : Pair{A}(B)) : A;

Pair{A}(B) = (p : Π(C, c(a : A, b : B(a)) : C(a)) -> C(fst(p)));
fst{A, B}(p) = p.@(c(a, b) = a);

//
pair{A, B}(a : A, b : B(a)) : Pair{A}(B);
mod{A, B}(a, b) : a == fst(pair(a, b));

pair{A, B}(a, b) =λ(C, c) => coe(mod(a, b), C, c(a, b));
mod{A, B}(a, b) = refl();


//

Pair{A}(B(a : A) : Type) : Type;
fst{A, B}(p : Pair{A}(B)) : A;

Pair{A}(B) = (p : Π(C, c(a : A, b : B(a)) : C(a)) -> C(fst(p)));
fst{A, B}(p) = p.@(c(a, b) = a);

pair{A, B}(a : A, b : B(a)) : Pair{A}(B);
mod{A, B}(a, b) : a == fst(pair(a, b));

pair{A, B}(a, b) = λ(C, c) => coe(mod(a, b), C, c(a, b));
mod{A, B}(a, b) = λ(C, c) => c;
// the last one works because
mod{A, B}(a, b) : a == coe(mod(a, b), C(_) = A, a);
// which is the same as
mod{A, B}(a, b) : Π(C, r : C(a)) -> C(coe(mod(a, b), C(_) = A, a))
// which if you just assume it in a blocked way
mod{A, B}(a, b) == λ(C, c) => [c]
// so you want
C(a) == C(coe(λ(C, c) => [c], C(_) = A, a))
// same as the following, which is true
C(a) == C([a])

//
Pair{A}(B(a : A) : Type) : Type =
  (p : Π(C, c(a : A, b : B(a)) : C(a)) -> C(p.@(C(_) = A, c(a, b) = a)));

pair{A, B}(a : A, b : B(a)) : Pair{A}(B);
pair{A, B}(a, b) =
  mod : a == fst(pair(a, b)) = λ(C, c) => c;
  λ(C, c) => coe(mod, C, c(a, b));

pair{A, B}(a : A, b : B(a)) : Pair{A}(B);
pair{A, B}(a, b) = λ(C, c) => c(a, b);




M(N) ⇐ T |->
  f = (M ⇐ [x : _A == N] -> T);
  x = (N ⇐ _A); f(x)
//
M(N) ⇐ T |->
  f = (M ⇐ [x : _A == N] -> T);
  x = (N ⇐ _A); f(x)

λ(f) => f(N)


(λ(x) => K).@

(@ = M) ⇐ Type


Bool : (Bool : Type);
Bool = (b : Π(C(b : [Bool].@) : Type) -> C(true) -> C(false) -> C(b));


Type, A, B ::=
Term, M, N, K ::=
  | Type | (M ⇒ A) | (M ⇐ A) | x
  | (x : A) -> B | (x : A) => K | M(N)
  | self(x) -> T | fix(x) : T => K | M.@

(x : A) -> C{x} |-> (x : A) -> C{x ⇒ A}
(x : A) => C{x} |-> (x : A) => C{x ⇒ A}

(T := self(x) -> C{x}) |-> (T := self(x) -> C{x ⇒ [T]})
(s := fix(x) : T => C[x]) |-> fix(x) : T => C[s]

Type |-> Type ⇒ Type

(x : A ⇒ Type) -> (B ⇒ Type) |-> (x : A) -> B ⇒ Type
(x : (A ⇒ Type)) => (M ⇒ B) |-> (x : A) => M ⇒ (a : A) -> B
(M ⇒ (a : A) -> B)(N) |-> M(N ⇐ A) ⇒ B{x := (N ⇐ A)}

self(x) -> (T ⇒ Type(l)) |-> self(x) -> T ⇒ Type(l)
fix(x) : (T ⇒ Type) => M |-> fix(x) : T => (M ⇐ T) ⇒ self(x) -> T
(M ⇒ self(x) -> T).@ |-> M.@ ⇒ T{x := M}

//
[Type] |-> Type
[(a : A) -> B] |-> (a : [A]) -> [B]
[self(x) -> T] |-> self(x) -> [T]

//

S : Type = [Bool : Type, true : Bool, false : Bool];
F_Bool(P : S) : S =
  [
    Bool = Σ(b : P.Bool) ->
      Π(C(b : P.Bool) : Type) -> C(P.true) -> C(P.false) -> C(b),
    true = [P.true, (C, t, f) => t],
    false = [P.false, (C, t, f) => f]
  ];

// bad name?
fmap{A : S, B : S}(
  f(a : A.Bool) : B.Bool,
  f_t : f(A.true) == B.true,
  f_f : f(A.false) == B.false,
  x : F_Bool(A).Bool
) : F_Bool(B).Bool = [
  f(x.0),
  λ(C, t, f) =>
    x.1(
      C(a) = C(f(a)),
      coe(f_t, C, t),
      coe(f_f, C, f)
    )
];

S : Type = [Bool : Type, true : Bool, false : Bool];

P : S;
P = [
  Bool = ι(b : P.Bool) -> // self
    Π(C, t : C(P.true), f : C(P.false)) -> C(b),
  true = (b = (C, t, f) => t),
  false = (b = (C, t, f) => f)
];


//
S : Type = [Bool : Type, true : Bool, false : Bool];
Hom_S(A : S, B : S) = [
  map : A.Bool -> B.Bool,
  true : f(A.true) == B.true,
  false : f(A.false) == B.false
];

F_Bool(P : S) : S = [
  Bool = Σ(b : P.Bool) ->
    Π(C(b : P.Bool) : Type) -> C(P.true) -> C(P.false) -> C(b),
  true = [P.true, (C, t, f) => t],
  false = [P.false, (C, t, f) => f]
];

m_map{A, B}(m : Hom_S(A, B), x : F_Bool(A).Bool) : F_Bool(B).Bool = [
  m.map(x.0),
  λ(C, t, f) =>
    x.1(
      C(a) = C(m.map(a)),
      coe(m.true, C, t),
      coe(m.false, C, f)
    )
];
F : End(S) = [
  obj = F_Bool,
  map = [
    map = m_map;
    true = _;
    false = _;
  ],
  map_id = _,
  map_comp = _
];

[Bool, true, false] = μ(F_Bool);
unroll(F_Bool)(true) == F_Bool(μ(F_Bool)).true
// same as
unroll(F_Bool)(true) == [μ(F_Bool).true, (C, t, f) => t]

b : Bool

roll(F_Bool).map : F_Bool(μ(F_Bool)).Bool -> μ(F_Bool).Bool



S : Type = [Bool : Type, true : Bool, false : Bool];
Hom_S(A : S, B : S) = [
  map : A.Bool -> B.Bool,
  true : f(A.true) == B.true,
  false : f(A.false) == B.false
];

F_Bool(P : S) : S = [
  Bool = Σ(b : P.Bool) ->
    Π(C(b : P.Bool) : Type) -> C(P.true) -> C(P.false) -> C(b),
  true = [P.true, (C, t, f) => t],
  false = [P.false, (C, t, f) => f]
];
F : End(S) = [obj = F_Bool, map = _, map_id = _, map_comp = _];

μ{S}(F : End(S)) : S
roll{S, F} : Homˢ(F(μ(F)), μ(F))
unroll{S, F} : Homˢ(μ(F), F(μ(F)))

[Bool, true, false] = μ(F_Bool);
unroll.map(true) == F_Bool(μ(F_Bool)).true
// same as
unroll.map(true) == [P.true, (C, t, f) => t]

[Bool, true, false] = μ(F_Bool);
b : Bool
unroll.map(b) : Σ(b : Bool) ->
  Π(C(b : Bool) : Type) -> C(true) -> C(false) -> C(b)

unroll :
true : unroll.map(F_Bool(true)) == true,
false : unroll.map(F_Bool(false)) == false

fst_inj(x, y, h : unroll.map(x).0 == unroll.map(y).0) :
  unroll.map(x) == unroll.map(y);
fst_inj(x, y, h) =
  [
    h,
    λ(C, t, f) =>
      _ : coe(h, C, unroll.map(x).1(C, t, f)) == unroll.map(y).1(C, t, f)
  ]



unroll.map(b).0 == unroll.map(true).0 -> true == unroll.map(b)

unroll.map(b).0 == b

unroll.map(b).1 : C(unroll.map(b).0)

W_Unit : Type = [c : C_Unit, i : I_Unit(c)];
Unit : Type = [c : C_Unit, p : c(w_unit).c == c];

fold(F, A : S)(a : Hom_S(F(A), A)) : Hom_S(μ(F), A);

μ{S : Category}(F : End(S)) : S.Obj;

in{F : End(S)}
  : Hom_S(F(μ(F)), μ(F));

fold{F : End(S), A : S.Obj}(
  a : Hom_S(F(A), A)
) : Hom_S(μ(F), A);


//


unroll{S, F}.map(b : μ(F).Bool : F(μ(F)).Bool) =

cata{A : Type}(f(x : F(A)) : A, x : μ(F)) : A

unroll = [
  map :
    (μ F).Bool ->
    F(μ F).Bool,

  true :
    map((μ F).true) ==
    F(μ F).true,

  false :
    map((μ F).false) ==
    F(μ F).false
];


F_Bool(P : S) : S = [
  Bool = Σ(b : P.Bool) ->
    Π(C(b : P.Bool) : Type) -> C(P.true) -> C(P.false) -> C(b),
  true = [P.true, (C, t, f) => t],
  false = [P.false, (C, t, f) => f]
];

[Bool, true, false] = μ(F_Bool);
unroll.map(true) == F_Bool(μ(F_Bool)).true

F : End(S) = [obj = F_Bool, map = _, map_id = _, map_comp = _];
// same as
unroll.map(true) == [P.true, (C, t, f) => t]

[Bool, true, false] = μ(F_Bool);
b : Bool
unroll.map(b) : Σ(b : Bool) ->
  Π(C(b : Bool) : Type) -> C(true) -> C(false) -> C(b)


S : Type = [Bool : Type, true : Bool, false : Bool];
Hom(A : S, B : S) = [
  f : A.Bool -> B.Bool,
  f_true : f(A.true) == B.true,
  f_false : f(A.false) == B.false
];

F_Bool(P : S) : S = [
  Bool = Σ(b : P.Bool) ->
    Π(C(b : P.Bool) : Type) -> C(P.true) -> C(P.false) -> C(b),
  true = [P.true, (C, t, f) => t],
  false = [P.false, (C, t, f) => f]
];
cata{A : S}(m : Hom(F_Bool(A), A)) : Hom(μ(F_Bool), A) =
// TODO: better name
alg{A : S} : Hom(F_Bool(A), A) = [
  f(x) = x.0,
  f_true = refl(),
  f_false = refl()
];

initial_F :
  Π(A : S) ->
  Π(alg : Hom(F(A), A)) ->
  ∃! f : Hom(μ(F), A),
    f ∘ fold == alg ∘ F.map(f)

μ{S}(F : End(S)) : S
roll{S, F} : Hom_S(F(μ(F)), μ(F))
unroll{S, F} : Hom_S(μ(F), F(μ(F)))

unroll.map(unroll.map(x).0).1(
  C(_) = F_Bool(A).Bool,
  F_Bool(A).true,
  F_Bool(A).false
)
cata_m{A : S}(
  m : Hom(F_Bool(A), A),
  x : μ(F_Bool).Bool
) : A.Bool =
  m.f(
    unroll.map(x).1(
      C(_) = F_Bool(A).Bool,
      F_Bool(A).true,
      F_Bool(A).false
    )
  );
cata{A : S}(m : Hom(F_Bool(A), A)) : Hom(μ(F_Bool), A) =
  [
    f(x) = cata_m(m, x),
    // cata_m(m, μ(F_Bool).true) |-> m.f(F_Bool(A).true)
    f_true = m.f_true,
    f_false = m.f_false
  ];
cata_m{A : S}(m : Hom(F_Bool(A), A), μ(F_Bool).true)

m.f(F_Bool(A).true)



μ(F_Bool)




F : End(S) = [obj = F_Bool, map = _, map_id = _, map_comp = _];



//
cata{A : S}(m : Hom(F_Bool(A), A)) : Hom(μ(F_Bool), A) = _;

in(A : S) : Hom(μ(F_Bool), A) =
  [
    f(x : μ(F_Bool).Bool) : A.Bool =
      unroll.f(x).1(C(_) = A.Bool, A.true, A.false),
    f_true = refl(),
    f_false = refl()
  ];
out(A : S) : Hom(A, μ(F_Bool)) =
  [
    f(x : μ(F_Bool).Bool) : A.Bool =
      unroll.f(x).1(C(_) = A.Bool, A.true, A.false),
    f_true = refl(),
    f_false = refl()
  ];

id{A : S} : Hom(A, A) = _;
End(S) : Type = [
  obj : S -> S,

  map{A B}(m : Hom(A, B)) : Hom(obj(A), obj(B));
  map_id{A} : map(id{A}) == id{obj(A)};
  map_comp{A B C}(f, g) : map(f ∘ g) == map(f) ∘ map(g);
];



// [
//   m.map(x.0),
//   λ(C, t, f) =>
//     x.1(
//       C(a) = C(m.map(a)),
//       coe(m.f_true, C, t),
//       coe(m.f_false, C, f)
//     )
// ];


//
End(S : Type) : Type = [
  obj : S -> S,

  map{A B : S} :
    Homˢ(A, B) ->
    Homˢ(obj(A), obj(B)),

  map_id{A : S} :
    map(id{A}) == id,

  map_comp{A B C : S}(
    f : Homˢ(A, B),
    g : Homˢ(B, C)
  ) :
    map(g ∘ f) == map(g) ∘ map(f)
];
//
μ(F : End(S)) : S
roll(F : End(S)) : Hom(F(μ(F)), μ(F))
unroll(F : End(S)) : Hom(μ(F), F(μ(F)))

S : Type = [Bool : Type, true : Bool, false : Bool];
Hom(A : S, B : S) = [
  f : A.Bool -> B.Bool,
  f_true : f(A.true) == B.true,
  f_false : f(A.false) == B.false
];

F_Bool(P : S) : S = [
  Bool = Σ(b : P.Bool) ->
    Π(C(b : P.Bool) : Type) -> C(P.true) -> C(P.false) -> C(b),
  true = [P.true, (C, t, f) => t],
  false = [P.false, (C, t, f) => f]
];
map{A, B}(m : Hom(A, B)) : Hom(F_Bool(A), F_Bool(B)) = [
  f(x) =
    x.1(C(_) = F_Bool(B).Bool, F_Bool(B).true, F_Bool(B).false)
  f_true = refl();
  f_false = refl();
];
id{A} : Hom(A, A) = [
  f(x) = x;
  f_true = refl();
  f_false = refl();
];

F : End(S) = [
  obj = F_Bool,
  map{A, B}(m) = [
    f(x) = [
      m.f(x.0),
      λ(C, t, f) =>
        x.1(
          C(a) = C(m.f(a)),
          coe(m.f_true, C, t),
          coe(m.f_false, C, f)
        ) : C(x.0)
    ],
    f_true = refl();
    f_false = refl();
  ];
  map_id{A} = refl();
  map_comp = _;
];


//

[Bool, true, false] = μ(F);
unroll : Hom(μ(F), F(μ(F)))

b : Bool;

unroll.f(b) : F(μ(F)).Bool
P_Bool = [
  b : Bool,
  p : unroll.f(b).0 == b
];

b : Bool;

F.map.f(unroll, b : F_Bool(μ(F)).Bool) : F_Bool(F(μ(F))).Bool

 : Hom(F_Bool(μ(F)), F_Bool(F(μ(F))))

F.map(unroll).f(
  _ : F_Bool(μ(F)).Bool
)

unroll.f(true)
unroll.f_true : unroll(F).f(true) == F_Bool(μ(F)).true




(F.map(unroll) ∘ F.map(roll)).map(x)

unroll(F).f(true) : F_Bool(μ(F)).Bool;

[
  unroll(F).f(x.0),
  λ(C, t, f) =>
    x.1(
      C(a) = C(unroll(F).f(a)),
      coe(unroll(F).f_true, C, t),
      coe(unroll(F).f_false, C, f)
    ) : C(x.0)
]


(F.map(unroll) ∘ F.map(roll)).map(x)




F.map(unroll) ∘ F.map(roll)
  ==
F.map(unroll ∘ roll)
  ==
F.map(id)
  ==
id;



F.map(unroll(F)).f(F.map(roll(F)).f(x)) ↦ x

F : End(F_Bool) = [obj = F_Bool, map = map, map_id = _, map_comp = _];

F.map(g).f(F.map(f).f(x))
↦ F.map(g ∘ f).f(x)

F.map(id).f(x)
↦ x
F.map(unroll(F)).f(F.map(roll(F)).f(x)) ↦ x
F.map(roll(F)).f(F.map(unroll(F)).f(x)) ↦ x

roll.map(unroll.map(x)) == x
unroll.map(roll.map(x)) == x

F.map(unroll(F)).f(F.map(roll(F)).f(x)) ↦ x

unroll.map(roll.map(unroll.map(x))) == x


μ(F_Bool).Bool
[Bool, true, false] = μ(F_Bool);
ind(b : Bool, C(b : Bool) : Type, t : C(true), f : C(false)) : C(b) =
  unroll.map(b).1
  in(unroll.map(b).0) : μ(F_Bool).Bool
   :
    Σ(b : Bool) ->
      Π(C(b : Bool) : Type) -> C(true) -> C(false) -> C(b);
//
Morph(A : S, B : S) = [
  f : A.Bool -> B.Bool,
  f_true : f(A.true) == B.true,
  f_false : f(A.false) == B.false,
  g : B.Bool -> A.Bool,
  g_true : g(B.true) == A.true,
  g_false : g(B.false) == A.false
];

F_Bool(P : S) : S = [
  Bool = Σ(b : P.Bool) ->
    Π(C(b : P.Bool) : Type) -> C(P.true) -> C(P.false) -> C(b),
  true = [P.true, (C, t, f) => t],
  false = [P.false, (C, t, f) => f]
];
//
Retract(A : S, B : S) = [
  embed : A.Bool -> B.Bool,
  project : B.Bool -> A.Bool,

  embed_true : embed(A.true) == B.true,
  embed_false : embed(A.false) == B.false,

  project_true : project(B.true) == A.true,
  project_false : project(B.false) == A.false,

  project_embed(x : A.Bool) : project(embed(x)) == x
];
//

Retract(A : S, B : S) = [
  embed : A.Bool -> B.Bool,
  project : B.Bool -> A.Bool,

  embed_true : embed(A.true) == B.true,
  embed_false : embed(A.false) == B.false,

  project_true : project(B.true) == A.true,
  project_false : project(B.false) == A.false,

  project_embed(x : A.Bool) : project(embed(x)) == x
];

Hom(A : S, B : S) = [
  f : A.Bool -> B.Bool,
  f_true : f(A.true) == B.true,
  f_false : f(A.false) == B.false,
];

map{A, B}(m : Hom(A, B)) : Hom(F_Bool(A), F_Bool(B)) = [
  f(x) = [
    m.f(x.0),
    λ(C, t, f) =>
      x.1(
        C(a) = C(m.f(a)),
        coe(m.f_true, C, t),
        coe(m.f_false, C, f)
      ) : C(x.0)
  ]
  f_true = refl();
  f_false = refl();
];

RetractFunctor(S : Type) : Type = [
  obj : S -> S,
  map{A : S, B : S}(f : Retract(A, B)) : Retract(obj(A), obj(B))
],

F_Bool(P : S) : S = [
  Bool = Σ(b : P.Bool) ->
    Π(C(b : P.Bool) : Type) -> C(P.true) -> C(P.false) -> C(b),
  true = [P.true, (C, t, f) => t],
  false = [P.false, (C, t, f) => f]
];

Self_Functor(S) = [
  El(T : S) : S,
  F(x : El(T : S)) : End(S),
];

μ{S}(F : Self_Functor(S)) : S
unroll{S, F} : Retract(μ(F), F(μ(F)));



End(S : Type) : Type = [
  obj : S -> S,

  map{A B}(m : Hom(A, B)) : Hom(obj(A), obj(B));
  map_id{A} : map(id{A}) == id{obj(A)};
  map_comp{A B C}(f, g) : map(f ∘ g) == map(f) ∘ map(g);
  idem{A}(m : Hom(A, A)) : map(m) ∘ map(m) == id;
];

map(x : Hom(A, B)) ∘ map()

map{A : S, B : S}(r : Hom(A, B)) : Retract(F_Bool(A), F_Bool(B))

map{A : S, B : S}(r : Retract(A, B)) : Retract(F_Bool(A), F_Bool(B)) = [
  embed(a : F_Bool(A).Bool) = [
    r.embed(a.0),
    λ(C, t, f) =>
      a.1(
        C(b) = C(r.embed(b)),
        coe(r.embed_true, C, t),
        coe(r.embed_false, C, f)
      ) : C(a.0)
  ];
  project(b : F_Bool(B).Bool) = [
    r.project(b.0),
    λ(C, t, f) =>
      b.1(
        C(c) = C(r.project(c)),
        coe(r.project_true, C, t),
        coe(r.project_false, C, f)
      ) : C(b.0)
  ];

  project_embed(x : A.Bool) =
    project(embed(x))
  _
]

RetractFunctor(S : Type) : Type = [
  obj : S -> S,

  map{A : S, B : S}(f : Retract(A, B)) : Retract(obj(A), obj(B)),
  map_id{A : S} :
    EqRetract(
      map(id{A}),
      id{obj(A)}
    ),

  map_comp{
    A : S,
    B : S,
    C : S
  }(
    f : Retract(A, B),
    g : Retract(B, C)
  ) :
    EqRetract(
      map(compose(f, g)),
      compose(map(f), map(g))
    )
];



ι{S}(F : Retract_Functor(S)) : S;
unroll{S}(F) : Retract(ι(F), F(ι(F)));


μ{S, Hom}


μ{S}(F : Retract_End())
unroll(F) : Retract(μ(F), F(μ(F)))


End(S : Type) : Type = [
  obj : S -> S,

  map{A B}(m : Hom(A, B)) : Hom(obj(A), obj(B));
  map_id{A} : map(id{A}) == id{obj(A)};
  map_comp{A B C}(f, g) : map(f ∘ g) == map(f) ∘ map(g);
];

F : End(S) = [
  obj = F_Bool,
  map{A, B}(m) = [
    f(x) = [
      m.f(x.0),
      λ(C, t, f) =>
        x.1(
          C(a) = C(m.f(a)),
          coe(m.f_true, C, t),
          coe(m.f_false, C, f)
        ) : C(x.0)
    ],
    f_true = refl();
    f_false = refl();
  ];
  map_id{A} = refl();
  map_comp = _;
];


//

[Bool, true, false] = μ(F);
unroll : Hom(μ(F), F(μ(F)))



S : Type = [Bool : Type, true : Bool, false : Bool];

Self(S : ???) : Type = [
  obj(A : S.T, x : P.Bool) : S.T;
  map{A, B}(m : Hom(A, B), x : S.El(A)) :
    Hom(obj(A, x), obj(B, S.map_el(x)));
  map_id{A}(x : S.El(A)) : map(S.id, x) == S.id;
  map_comp : ...;
];
ι{S}(F : Self(S)) : S;
F_Bool(P : S) : S = [
  Bool = Σ(b : P.Bool) ->
    Π(C(b : P.Bool) : Type) -> C(P.true) -> C(P.false) -> C(b),
  true = [P.true, (C, t, f) => t],
  false = [P.false, (C, t, f) => f]
];

F_Bool(P : S) : S =
  Bool = Σ(b : P.Bool) ->
    Π(C(b : P.Bool) : Type) -> C(P.true) -> C(P.false) -> C(b),

ι{S}(F : Self(S)) : S;
unroll{S}(F, x :) : Hom(ι(F), F(ι(F)));

Self(S : Type) : Type = [
  F : End(S),
  self(A : S) : Hom(F.obj(A), A),
  self_nat{A B}(m : Hom(A, B)) : self(B) ∘ F.map(m) == m ∘ self(A),
];
ι{S}(F : Self(S)) : S;
unroll{S}(F, x :) : Hom(ι(F), F(ι(F)));



ι{S}()

ι{S}(F : Retract_Functor(S)) : S;
unroll{S}(F) : Retract(ι(F), F(ι(F)));


Dependent_Functor(S : Type) : Type = [
  El(T : S) : Type,
  map_el{A, B}(m : Hom(A, B)) : El(A) -> El(B),

  obj(A : S, x : El(A)) : S,

  map{A, B}(m : Hom(A, B), x : El(A)) :
    Hom(obj(A, x), obj(B, map_el(m, x))),

  map_id{A : S}(x : El(A)) : map(id, x) == id,

  map_comp{A, B, C}(f : Hom(A, B), g : Hom(B, C), x : El(A)) :
    map(g ∘ f, x) == map(g, map_el(f, x)) ∘ map(f, x)
];

ι{S}(F : Dependent_Functor(S)) : S;
unroll{S, F}(x : F.El(ι(F))) : F.El(F.obj(ι(F), x));

S : Type = [
  Bool : Type,
  true : Bool,
  false : Bool
];

Displayed(A : S) : Type = [
  Bool(a : A.Bool) : Type,
  true : Bool(A.true),
  false : Bool(A.false)
];

Displayed(S : Type) : Type =
  S -> Type;


Displayed_Hom{
  S,
  D : Displayed(S),
  A B
}(
  m : Hom(A, B),
  P : D(A),
  Q : D(B)
) : Type =
  ...;

DisplayedHom{A B}(
  m : Hom(A, B),
  P : Displayed(A),
  Q : Displayed(B)
) : Type = [
  f(b : A.Bool, r : P.Bool(b)) : Q.Bool(m.f(b)),

  f_true :
    coe(m.f_true, Q.Bool, f(A.true, P.true))
    ==
    Q.true,

  f_false :
    coe(m.f_false, Q.Bool, f(A.false, P.false))
    ==
    Q.false
];

Dependent_Functor(S : Type) : Type = [
  obj(A : S) : Displayed(A),
  map{A, B}(m : Hom(A, B)) : DisplayedHom(m, obj(A), obj(B)),
  map_id{A} : map(id) == id(obj(A)),
  map_comp{A, B, C}(f : Hom(A, B), g : Hom(B, C)) : map(f ∘ g) == map(f) ∘ map(g)
];

Dependent_Functor(S : Type) : Type = [
  D : Displayed(S),
  obj(A : S) : D(A),

  map{A B : S}(m : Hom(A, B)) :
    DisplayedHom(
      m,
      obj(A),
      obj(B)
    ),

  map_id = ...,
  map_comp = ...
];


μ{S}(F : Dependent_Functor(S)) : S;
unroll{S, F : Dependent_Functor(S)} : F.D(μ(F));

S : Type = [Bool : Type, true : Bool, false : Bool];
F_Bool(A : S, b : A.Bool) : S = [
  Bool = Π(C(b : A.Bool) : Type) -> C(A.true) -> C(A.false) -> C(b),
  true = (C, t, f) => (t : C(b)) // fails,
  false = (C, t, f) => (f : C(b))
]



μ{
  S : Type,
  D : Displayed(S)
}(
  F : DependentEnd(D)
) :
  S;


unroll{
  S : Type,
  D : Displayed(S),
  F : DependentEnd(D)
} :
  D.Section(
    F.obj(μ(F))
  );


//
Retract(A : S, B : S) = [
  embed : A.Bool -> B.Bool,
  project : B.Bool -> A.Bool,

  embed_true : embed(A.true) == B.true,
  embed_false : embed(A.false) == B.false,

  project_true : project(B.true) == A.true,
  project_false : project(B.false) == A.false,

  project_embed(x : A.Bool) : project(embed(x)) == x
];

ι{S}(F : W(S)) : S;
unroll{S, F : W(S)}(x) : Retract(ι(F), F(ι(F)))


//
S : Type;

Hom(A : S, B : S) : Type;
id{A : S} : Hom(A, A);

(_∘_){A B C : S} :
  Hom(B, C) -> Hom(A, B) -> Hom(A, C);

id_left(f)  : id ∘ f == f;
id_right(f) : f ∘ id == f;
assoc(f, g, h) : h ∘ (g ∘ f) == (h ∘ g) ∘ f;

hom_set{A B : S} : isSet(Hom(A, B));


Displayed(S : Type) : Type = [
  Obj(T : S) : Type,

  Hom{A B, m : Hom(A, B)}(P : Obj(A), Q : Obj(B)) : Type,

  id{A : S, P : Obj(A)} : Hom{id}(P, P),

  (_∘_){
    A B C : S,
    m : Hom(A, B),
    n : Hom(B, C),
    P : Obj(A),
    Q : Obj(B),
    R : Obj(C)
  } :
    Hom{n}(Q, R) ->
    Hom{m}(P, Q) ->
    Hom{n ∘ m}(P, R),

  id_left(f)  : id ∘ f == f,
  id_right(f) : f ∘ id == f,
  assoc(f, g, h) : h ∘ (g ∘ f) == (h ∘ g) ∘ f,

  hom_set{A B m P Q} : isSet(Hom{A, B, m}(P, Q)),

  Sect{A : S}(P : Obj(A)) : Type
];


DepSection{S : Type}(D : Displayed(S)) : Type = [
  obj(A : S) : D.Obj(A),

  map{A B : S}(m : Hom(A, B)) : D.Hom{m}(obj(A), obj(B)),
  map_id{A : S} : map(id{A}) == D.id,
  map_comp{A B C}(m : Hom(A, B), n : Hom(B, C)) :
    map(n ∘ m) == map(n) ∘ map(m)
];

//
Γ |- M ⇐ T(roll(M))
----------------------
Γ |- roll(M) ⇐ Self(T)


//
A : Type;
B(a : A) : Type;
roll(b : B(roll(a))) : A;
unroll(a : A) : B(a);

//
S : Type = [Bool : Type, true : Bool, false : Bool];
Self(T : S) : S;

S : Type = [Bool : Type, true : Bool, false : Bool];

self(S, El(T : S) = T.Bool, P : S, b : El(P)) : Type =
  Π(C(b : P.Bool) : Type) -> C(P.true) -> C(P.false) -> C(b);

self{S : Type, El(T : S) : Type}(F(P : S, x : El(P)) : Type) : Type;
fix{S, El, F}(f(P : S, x : El(P)) : S) : S;

//
self{S : Type, El(T : S) : Type}(F(P : S, x : El(P)) : Type) : Type;

self{S(A : Type) : Type}(F(A : Type, P : S(A), b : A) : Type) : Type;

S(Bool : Type) : Type = [true : Bool, false : Bool];
F(Bool, [true, false], b) =
  Π(C(b : Bool) : Type) -> C(true) -> C(false) -> C(b);
Bool = self{S}(F);

L : S(Bool);
R : S(Bool);
mod : L == R;


mod_true : L.true == R.true;
mod_false : L.false == R.false;

mod_true = λ(C, t, f) =>
  _;
mod_false = _;

unroll(L.true) == coe(mod_true, )

Π(C, t, f) -> unroll(L.true)(C, t, f) ==
  coe(mod_true, C, unroll(R.true)(C, coe(mod_true, C, t), coe(mod_false, C, f)))


unroll(L.true)(
  C(b) =
    unroll(b)(C, t, f) ==
    unroll(R.true)(C, coe(mod_true, C, t), coe(mod_false, C, f))
)
unroll(L.true) :
  Π(C(b : Bool) : Type) -> C(L.true) -> C(L.false) -> C(L.true)


unroll(R.true)
data(L.true) : S(Bool)

unroll(L.true) : F(Bool, data(L.true), L.true)





unroll(L.true)(
  C(b) = (H : b == L.true) -> b == R.true,
  t(H) =
)
unroll(R.true) :


(L.true : Bool) == (R.true : Bool)






fix(

)


fix{S, El, F}(
  f(P : S) : S,
  p : El()
)

Γ |- M ⇒ self{S, El, F}
-----------------------
Γ |- unfold(M) ⇒ F()


//
S : Type = [Bool : Type, true : Bool, false : Bool];
El(T : S) = T.Bool;
F(P : S, b : El(P)) =
  Π(C(b : El(P)) : Type) -> C(P.true) -> C(P.false) -> C(b);

Bool = self{S, El}(F);


//
self{S(A : Type) : Type}(F(A : Type, P : S(A), x : A) : Type) : Type;
S(Bool : Type) : Type = [
  true : Bool,
  false : Bool,
];
F(Bool, [true, false, true_neq_false], b) =
  Π(C(b : Bool) : Type) -> C(true) -> C(false) -> C(b);
Bool = self{S}(F);

Γ |- M() : S(self{S}(F))
----------------------
Γ |- fix(M) : S

fix{S(A : Type) : Type}(
  f(P : S(self(F))) : S(self(F)) = [
    true :
  ]
)

L : S(Bool);
R : S(Bool);

mod_true : L.true == R.true;
mod_false : L.false == R.false;

mod_true =
  _ : unroll(L.true) ==
    λ(C) => coe(mod_true, C(b) =
      C(data(b).false) -> C(data(b).true) -> C(b),
      unroll(R.true)(C)
    )
  _ : unroll(L.true)(C, t, f) ==
    coe(mod_true, C, unroll(R.true)(
      C, coe(mod_true, C, t), coe(mod_false, C, f)))
  _;

mod : L == R;

Bool : Type;
P : [true : Bool, false : Bool];

Bool = ι(b : Bool) ->
  Π(C(b : Bool) : Type) -> C(P.true) -> C(P.false) -> C(b);
P = [λ(C, t, f) => t, λ(C, t, f) => f];

self(
  S(A : Type) : Type,
  F(A : Type, P : S(A), a : A) : Type,
  f(a : self(S, f))
) : Almost_Type;

Γ, A == self(A, x), x : A |- B : Type
-------------------------------------
Γ |- self(A, x) -> B : Type

Γ |- M ⇐ B{self(A, x), fix(M)}
------------------------------
Γ |- fix(M) ⇐ self(A, x) -> B

Γ |- M ⇒ self(A, x) -> B
---------------------------------
Γ |- unroll(M) ⇒ B{self(A, x), M}

Bool = self(Bool, b) -> Π(C(b : Bool) : Type) ->
  C(fix(λ(C, t, f) => t)) -> C(fix(λ(C, t, f) => f)) -> C(b);
true : Bool = fix(λ(C, t, f) => t);
false : Bool = fix(λ(C, t, f) => f);





S(Bool : Type) : Type = [true : Bool, false : Bool],
F(Bool, P : S(Bool), b : Bool) =
  Π(C(b : Bool) : Type) -> C(P.true) -> C(P.false) -> C(b);

self(
  S, F,
  f([true, false]) = [
    true = fold(λ(C, t, f) => t),
    false = fold(λ(C, t, f) => f)
  ];
)




self(Bool, b) ->
  (Π(R : Type) -> (R -> Bool) -> R -> R -> A) -> A



Bool = ∀A -> (∀R -> (R -> A) -> F(R) -> A) -> A;

true : Bool = λ(A) => λ(f : ∀R -> (R -> A) -> R -> R -> A) =>
  f(A)(λ(c) => _);



P : S;
P = [ι(b) -> Bool(P, b), true(P), false(P)];

//
S : Type = [Unit : Type, unit : Unit];
Unit(P : S, u : P.Unit) = Π(C(u : P.Unit) : Type) -> C(P.unit) -> C(u);
unit(P : S) : Unit(P, P.unit) = λ(C, r) => r;

P : S;
P = [ι(u) -> Unit(P, u), unit(P)];

//
S : Type = [Bool : Type, true : Bool, false : Bool];
Bool(P : S, b : P.Bool) =
  Π(C(b : P.Bool) : Type) -> C(P.true) -> C(P.false) -> C(b);
true(P : S) : Bool(P, P.true) = λ(C, t, f) => t;
false(P : S) : Bool(P, P.false) = λ(C, t, f) => f;


b : Bool_S;

true : Bool = λ(A, f) =>
  f([Bool, c_true, c_false])(c_true)(λ(C, t, f) => t);
false : Bool = λ(A, f) =>
  f([C_Bool, c_true, c_false])(c_false)(λ(C, t, f) => f);



b : Bool;
b(A, λ(R, rec : R.Bool -> A) => λ(b : R.Bool, i) =>
  _)

Bool : Type;
true : Bool;
false : Bool;

Bool = Π(C(b : Bool) : Type) -> C(true) -> C(false) -> C(b);

Bool = Π(A : Type) ->
  (Π(R : Type, rec : R -> A) ->
    (Π(true : R, false : R) ->
      Π(C(r : R) : Type) -> C(true) -> C(false) -> _) -> A) -> A;

b : Bool
b(A)(λ(R, rec) => λ(x) =>
  x(_ : R, _ : R)(_))

true : Bool = λ(A) => λ(f) ->
  f(Bool)(λ(b) => b(A)(f))(
    λ(true, false) =>
      λ(C, t, f) => t
  )

MFix(F) = ∀A -> (∀R -> (R -> A) -> F(R) -> A) -> A;


Bool : Type = self(b) ->
  (C(b : #typeof(b)) : Type) -> C(fix(λ(C, t, f) => t)) ->
    C(fix(λ(C, t, f) => f)) -> C(b);
true : Bool = fold(λ(C, t, f) => t);
false : Bool = fold(λ(C, t, f) => f);



Γ |- M ⇒ self(A, x) -> B
---------------------------------
Γ |- unroll(M) ⇒ B{self(A, x), M}

//
Bool : Type;
true : Bool;
false : Bool;

Bool = Π(C(b : Bool) : Type) -> C(true) -> C(false) -> C(b);
true = λ(C, t, f) => t;
false = λ(C, t, f) => f;


S : Type = [Bool : Type, true : Bool, false : Bool];

S : Type = [Bool : Type, true : Bool, false : Bool];
Bool(P : S, b : P.Bool) =
  Π(C(b : P.Bool) : Type) -> C(P.true) -> C(P.false) -> C(b);
true(P : S) : Bool(P, P.true) = λ(C, t, f) => t;
false(P : S) : Bool(P, P.false) = λ(C, t, f) => f;

Bool : Type = (b : Π(A : Type) ->
  (f : Π(R : S, in : A -> R.Bool) ->

    (Π(C(r : R) : Type) -> C(true) -> C(false) ->
        C(b)) -> A) -> A);
true : Bool = λ(A, f) =>
  f(
    R = [C_Bool, c_true, c_false],
    in(c) =
      f()
      (_ : A)
  );


//
// format
Γ |- M ⇒ A // infer
Γ |- M ⇐ A // check
Γ | S |- M ⇒ A // infer-with-self
Γ | S |- M ⇐ A // check-with-self

// rules
----------------
Γ |- Type ⇒ Type

-----------------
Γ, a : A |- a ⇒ A

Γ, a : A | S |- K ⇒ B  Γ, a : A, S == K | a |- N : A
---------------------------------------------------- // self-let
Γ | S |- a = N; K : B{a := N}

Γ |- A ⇐ Type  Γ, a : A |- B ⇐ Type
-----------------------------------
Γ |- (a : A) -> B ⇒ Type

Γ, a : A | S(a) |- K ⇐ B
--------------------------------
Γ | S |- (a) => K ⇐ (a : A) -> B

// apply doesn't propagate to simplify things
Γ |- M ⇒ (a : A) -> B  Γ |- N ⇐ A
---------------------------------
Γ |- M(N) ⇒ B{a := N}

Γ |- A ⇐ Type  Γ, a : A |- B ⇐ Type
-----------------------------------
Γ | A |- self(a) -> B ⇒ Type

Γ, a : self(a) -> B == S | S.@ |- K ⇐ B
---------------------------------------
Γ | S |- fix(a) => K ⇐ self(a) -> B

Γ |- M ⇒ self(a) -> B
---------------------
Γ |- M.@ ⇒ B{a := M}

// rules about modes
Γ |- M ⇒ B  Γ |- A == B
----------------------- // check-with-self-to-infer
Γ | S |- M ⇐ A

Γ | S |- M ⇒ A
-------------- // infer-with-self-to-check-with-self
Γ | S |- M ⇐ A

Γ, a : A | a |- M ⇐ A
--------------------- // check-to-check-with-self
Γ |- M ⇐ A


S : Type = [
  Bool : Type,
  true : Bool,
  false : Bool
];
D(P : S) : Type = [
  Bool(b : P.Bool) : Type,
  true : Bool(P.true),
  false : Bool(P.false)
];
F(P : S) : D(P) = [
  Bool(b) = Π(C(b : P.Bool) : Type) ->
    C(P.true) -> C(P.false) -> C(b),
  true = λ(C, t, f) => t,
  false = λ(C, t, f) => f
];

P : S;
P = [
  self(b) -> F(P).Bool(b),
  fold(F(P).true),
  fold(F(P).false)
];
s(P : S) : Σ(P : S) -> D(P) = [P, F(P)];
s_fst_id : (s ∘ fst)(P) == id;


// self marks the Ob
Γ, a : A |- B(a) : Type
--------------------------------
Γ | A |- self(a : A) -> B(a) : Type

// fold introduces the objects themselves
Γ |- M : B(S)
--------------------------------------
Γ | S |- fold(M) : self(a : A) -> B(a)

// also required for completeness
Γ | S(a) |- K : B(a)
-------------------------------------
Γ | S |- λ(a) => K : Π(a : A) -> B(a)

Γ | S.0 |- M : A  Γ, S.0 == M | S.1 |- N : B(M)
-----------------------------------------------
Γ | S |- [M, N] : Σ(a : A) -> B(a)




*p : (
  C : Type, I_C(a : C) : Type,
  c, i_c(a : A, b : B(a)) : I_C(c(a, b))
) : I_C(p(C, c));

*p(
  Pair(A, B), I_C(r) = p == r,
  c(a, b) = pair(a, b),
  i_c(a, b) = (_ : )
);


p == p(Pair(A, B), pair);


b : Pair(C_Bool, I_Bool);
*b(A, B, c(a, b) = a, i_c(a, b) = b) : B(b(A, c(a, b) = a))

//

Display(T : Type)

S : Type = Σ(Bool : Type) -> [true : Bool, false : Bool];
Display(S) |-> λ(P : S) = Σ(Bool(b : S.Bool) : Type) -> [
  true : Bool(S.true),
  false : Bool(S.false)
];

p ∈ Σ(x : A) -> B |-> Σ(x : |A|(p.0)) -> |B|(p.1)

P ∈ Σ(Bool : Type) -> [true : Bool, false : Bool] |->
  Σ(Bool : Π(b : P.0) -> Type) -> [Bool(P.1.0), Bool(P.1.1)]

p : Π(x : A) -> Π(C(h : B)) -> C(f(x)) -> C(g(x));
p : Π(C(h : B)) -> Π(x : A) -> C(f(x)) -> C(g(x));
// given that A is irrelevant but still polymorphic
p : Π(C(h : B)) -> C(λ(x) => f(x)) -> C(λ(x) => g(x));


b : C_Bool;
b : Π(C : Type) -> C -> C -> C;
*b : Π(
  C, I_C(a : C) : Type,
  t, i_t : I_C(t),
  f, i_f : I_C(f)
) : I_C(b(C, t, f))
// push into I_C
Π(
  C, I_C(a : C -> C -> C) : Type,
  i_t : I_C(λ(t, f) => t),
  i_f : I_C(λ(t, f) => f)
) : I_C(b(C))
// push further and rename
Π(
  C(a : Π(C : Type) -> C -> C -> C) : Type,
  t : C(λ(C, t, f) => t),
  f : C(λ(C, t, f) => f)
) : I_C(b)

// every impredicative type can have this shape
Π(X : S, C : I(X) -> Type) ->
  F(X, C) -> C(M)

Π(X : S, C : I(X) -> Type) ->
  F(X, C) × C(N) -> C(M)

p()


μ()

A ∈ |Type| |-> λ(x : A) -> Type
f ∈ |Π(a : A) -> B| |->
  λ(a : A, aD : a ∈ Display(A)) -> f ∈ Display(B(a))

|Π(C : Type) -> C -> C| ==
  λ(x) -> Π(C(y : Id) : Type) -> C(λ(C, r) => r) -> C(x)


p : Π(x) -> f(x) == g(x);
// same as
p : Π(x : A) -> Π(C(h : B)) -> C(f(x)) -> C(g(x));
// which is the same as
p : Π(C(h : B)) -> Π(x : A) -> C(f(x)) -> C(g(x));
// which by parametricity is the same as
p : Π(C(h : A -> B)) -> C(λ(x) => f(x)) -> C(λ(x) => g(x));



p : f == g;
// same as
p : Π(C(h : A -> B) : Type) -> C(f) -> C(g);
// which reduces to
p : Π(a : A) -> Π(C(h : B) : Type) -> C(f(a)) -> C(g(a));



p : f == g;
// same as
p : Π(C(h : Π(a : A) -> B(a)) : Type) -> C(f) -> C(g);
// which reduces to
p : Π(a : A) -> Π(C(h : B(a)) : Type) -> C(f(a)) -> C(g(a));

p : Π(C(h : Π(a : A) -> B(a)) : Type) -> C(f) -> C(g);
p = λ(a) => ap(C(h) = h(a), p)

//
p : Π(C(h : Π(a : A) -> B(a)) : Type) -> F(C) -> C(g);
p = λ(a) => λ(C) =>
  p(_(h) = C(h(a))) : F(C) : C(g(a))

//

A : Type;
x : A;
y : A;
p : x == y

B : Type;
b : B;


ap(A, x, y, _(a) = M(b)(a), p) == λ(C, r) =>
  p(_(a) = C(M(b)(a)))

ap(A, x, y, _(a) = M(b)(a), p) : M(b)(x) == M(b)(y)





C : Type -> Type;
b : Π(C : Type) -> C -> C -> C;
b(C(A -> B))

//
ap(A : Type, x : A, y : B, f(a : A) : B, p : x == y) : f(x) == f(y);
ap(A, x, y, f, p) = p(C(a) = f(x) == f(a), refl());
ap(A, x, y, f, p) = λ(C, r) => p(_(a) = C(f(a)), r);


refl(M)(refl(b))(p) : M(b) == M(b)
ap()

λ(C, r) => p(_(a) = C(M(b)(a)), r)

ap(A, x, y, _(a) = M(b)(a), p) : M(b)(x) == M(b)(y)

C : A -> Type
*C : Π(a : A, i : a ∈ A) -> C(a) -> Type;

id(C(M), N : C(M)) : C(M)

id(
  Π(a : A) -> C(a),
  λ()
)(M)

*id(
  C, I_C(a : C) : Type,
  r, i_r : I_C(r)
) : I_C(id(C, r));

*id(
  L, R, C(l : L, r : R) : Type,
  l, r, w : C(l, r)
) : C(id(L, l), id(R, r))


*id(
  C(M), C(M), _(l, r) = l == r,

  I_C(),
  N, _
) :

id(C(M), N : C(M))

id(Π(a : A) -> Type, C)



*id(
  (A -> Type), _(C) = id()
  C
)

id(C(M), N : C(M))

*C(
  M,
  |A|(M),
  N
)

Π(a : A, i : |A|(a)) -> C(a) -> Type


ap(A, x, y, _(a) = M(b)(a), p) == λ(C, r) =>
  p(_(a) = C(M(b)(a)))

*p(
  C, I_C(a : C) : Type,
  c, i_c(a, b) : I_C(c(a, b)),
) : I_C(p(C, c))


Π(C(h : A -> B)) -> C(f) -> C(g)

Π(C(h : A -> B)) -> C(f) -> C(g) -> C(h)
// lower
Π(a : A) -> Π(C(h : A -> B)) -> C(f(a)) -> C(g(a)) -> C(h(a))

// lower
Π(a : A) -> Π(C(h : B)) -> C(f(a)) -> C(g(a))
// goal
Π(C(b : Bool) : Type) -> C(true) -> C(false) -> C(b)
// lower
Π(A : Type, C(b : A -> A -> A) : Type) ->
  C(true(A)) -> C(false(A)) -> C(b(A))
// keep going
Π(A : Type, t : A, f : A, C(b : A) : Type) ->
  C(true(A, t, f)) -> C(false(A, t, f)) : C(b(A, t, f))
// same as
Π(A : Type, t : A, f : A, C(b : A) : Type) ->
  C(t) -> C(f) : C(b(A, t, f))
// compare it to unary parametricity
*b(A, C(a : A) : Type,
  t, i_t : I_C(t),
  f, i_f : I_C(f)
) : I_C(b(A, t, f))
// aka the same type


(f == h) + (g == h)

Π(a : A) -> (f(a) == h(a)) + (g(a) == h(a))

f : Π(a : Bool) -> (f(a) == h(a)) + (g(a) == h(a))

f(true) == h(true)
g(false) == h(false)

w(a : Bool) = a ? inl(refl()) : right(refl())


A = B = Bool
f = λ_. true
g = λ_. false
h = λa. a

Π(a : A) -> Π(C : B -> Type) -> C(f(a)) -> C(g(a)) -> C(h(a))

Π(C : (A -> B) -> Type) ->
  C(f) ->
  C(g) ->
  C(h)

// equivalent by funext to


Π(C : (A -> B) -> Type) ->
  C(f) ->
  C(g) ->
  Π(k : A -> B) -> (Π(a : A) -> k(a) == h(a)) ->
  C(k)

Π(C : (A -> B) -> Type) -> F(C) -> C(h)

Π(C : (A -> B) -> Type) ->
  C(f) -> C(g) -> C(h)
// impossible with C(k) := (k = f) + (k = g), since h ≠ f and h ≠ g

p(a : Bool) : (f == g) + () =


Π(C : (A -> B) -> Type) -> C()



λ(C, t, f) =>
  *b(
    C, I_C(r) = b(Bool, true, false),
    t, _,
    f, _
  );
b(Bool, true, false)(C, t, f) == b(C, t, f)

Π(C) -> C(f) -> C(g) -> C(h)
// to
Π(C : Type) ->
  ((Π(a : A) -> h(a) == f(a)) -> C) ->
  ((Π(a : A) -> h(a) == g(a)) -> C) ->
  C
// which on the simple case
Π(C) -> C(f) -> C(g)
Π(C : Type) -> ((Π(a : A) -> g(a) == f(a)) -> C) -> C
// which by yoneda
Π(a : A) -> g(a) == f(a)


//
Π(C : (A -> B) -> Type) -> C(f) -> C(g) -> C(h)

// to
Π(C : Type) ->
  ((Π(a : A) -> Π(R : B -> Type) -> R(h(a)) -> R(f(a))) -> C) ->
  ((Π(a : A) -> Π(R : B -> Type) -> R(h(a)) -> R(g(a))) -> C) ->
  C
//
Π(C : Type) ->
  (((Π(a : A) -> Π(R : B -> Type) -> R(h(a)) -> R(f(a))) +
    (Π(a : A) -> Π(R : B -> Type) -> R(h(a)) -> R(g(a)))) -> C) ->
  C
//

Π(C : Type) ->
  (((Π(a : A) -> h(a) == f(a)) +
    (Π(a : A) -> h(a) == g(a))) -> C) ->
  C
//
Π(C : Type) ->
  (([
    b : Bool,
    payload : b ? (Π(a : A) -> h(a) == f(a)) :
      (Π(a : A) -> h(a) == g(a))
  ]) -> C) ->
  C

Π(C : Type) ->
  (([b : Bool, payload : A -> b ? L : R]) -> C) -> C

Π(C : Type) -> (((A -> L + R)) -> C) -> C

Π(C : Type) ->
  (((A -> [b : Bool, payload : b ? L : R])) -> C) -> C

Π(C : Type) ->
  ((Π(C, t, f) -> t == b(C, t, f)) -> C) ->
  ((Π(C, t, f) -> f == b(C, t, f)) -> C) ->
  C
//
*b(A, C(a : A) : Type,
  t, i_t : I_C(t),
  f, i_f : I_C(f)
) : I_C(b(A, t, f))


Π(C : (A -> B) -> Type) -> F(C) -> C(h)
// which is equivalent to
Π(C : (A -> B) -> Type $ 0) -> F(C) -> C(h)
// equivalent to
Π(C : B -> Type $ 0) -> Π(a : A $ 0) -> F(C) -> C(h)
// which is equivalent to
Π(a : A $ 0) -> Π(C : B -> Type $ 0) -> F(C) -> C(h)


Term, M, N, K ::=
  | Type | Π(a : A) -> B
  | x | λ(a) => K | M(N)
  | ua(f, g)


(λ(a) => K)(N) |-> K{a := N}
Π(C(f : Π(a : A) -> B) : Type) -> C(f) -> C(g) |->
  Π(a : A) -> Π(C(r : B) : Type) -> C(f(a)) -> C(g(a))

Π(C(a : A) : Type) -> F(C)
Π(C(a : A) : Type) -> F(C) -> C(r)
Π(C : Type) -> F(λ(a) => a == r -> C) -> C(r)

Π(C(a : A) : Type) -> C(x) -> C(y) -> C(z)
Π(C : Type) -> (x == z -> C) -> (y == z -> C) -> C



A -> B -> C |-> A × B -> C



ua(f, g)(C(T) = T)

Π(C(f : Π(a : A) -> B) : Type) -> F(C) |->
  Π(a : A $ 0) -> Π(C(r : B) : Type) -> F(λ(f) => C(f(a)))

λ

coe(C(a : A) : Type, r : C(x)) : C(y)



A : Type;
f(a : A) : A;
f_I(a : A) : f(f(a)) == f(a);
f_J(a : A) : ap(f, f_I(a)) == f_I(f(a));

Fix : Type;
fix(a : A) : Fix;
mod_I(a : A) : fix(a) == fix(f(a));
mod_J(a : A) : sym(mod_I(f(a))) == ap(fix, f_I(a));

mod_I(a) : fix(f(a)) == fix(a)

Fix = self(x) -> Σ(A, λ(a) => fix(a) == x);
fix(a : A) = [f(a), sym(mod_I(a))];
// [f(a), refl()] == [f(f(a)), mod_I(f(a))]
mod_I(a) = [sym(f_I(a)), sym(mod_J(a))];
mod_J(a) = _


mod_I(a).@ : [f(a), refl()] == [f(f(a)), sym(mod_I(f(a)))]

mod_I(f(a)).@ : [f(f(a)), refl()] == [f(f(f(a))), sym(mod_I(f(f(a))))]

sym(mod_I(f(a)).@) : [f(f(f(a))), sym(mod_I(f(f(a))))] == [f(f(a)), refl()]
ap(fix, f_I(a)).@ : [f(f(f(a))), sym(mod_I(f(f(a))))] == [f(f(a)), sym(mod_I(f(a)))]

mod_J(a).@ :
  coe(
    mod_J(a),
    _,
    _
  ) == ap(fix, f_I(a)).@

sym(mod_I(f(a))) == ap(fix, f_I(a))
coe(
  mod_J(a),
  λ(r) => Σ(A, λ(a) => fix(a) == x),
  sym(mod_I(f(a)))
) == ap(fix, f_I(a))

mod_I(f(a)) == ap(fix, f_I(a))




coe(ap(fix, sym(f_I(a))), λ(r) => fix(r) == x, refl())

λ(i) => fix(f_I(a)(i))
λ(i) => [f(f_I(a)(i)), mod_I(f_I(a)(i))]

ap(fix, f_I(a))

sym(ap(fix, f_I(a))) @ mod_I(f(a)) @ mod_I(a)

ap(fix, f_I(a)) : fix(f(f(a))) == fix(f(a))

coe(ap(fix, f_I(a)), λ(x) => x == fix(a), ) == mod_I(a)

coe(f_I(a), λ(r) => fix(r) == fix(a), mod_I(f(a)) @ mod_I(a)) == mod_I(a)

coe(f_I(a), λ(r) => fix(r) == fix(a), mod_I(f(a)) @ mod_I(a)) == mod_I(a)

[
  f(f(a)),
  coe(mod_I(a), λ(x) => fix(f(f(a))) == x, mod_I(f(a)))
] == [f(a), mod_I(a)]

coe(mod_I(a), fix(f(a)) == x, fix(f(a)).@.) == fix(a).@
[f(f(a)), coe(mod_I(a), λ(x) => fix(a) == x, mod_I(f(a)))]


//
A : Type;
f(a : A) : A;
f_I(a : A) : f(f(a)) == f(a);
f_J(a : A) : ap(f, f_I(a)) == f_I(f(a));

K_Eq(a : A) : Type;
k_refl(a : A) : K_Eq(f(a));

K_Eq(a) = self(k) -> [
  p : f(a) == a,
  q : ap(K_Eq, p) # k_refl(a) == k
];
k_refl(a) = [
  f_I(a),
  k_mod(a)
  _ : ap(K_Eq, f_I(a)) # k_refl(f(a)) == k_refl(a)
]


coe(f_I(a), K_Eq, k_refl(f(a)))

k_refl(f(a)).@ : [
  p : f(f(f(a))) == f(f(a)),
  q : ap(K_Eq, p) # k_refl(f(f(a))) == k_refl(f(a))
]



coe(f_I(a), K_Eq, k_refl(f(a)))
coe(f_I(a), K_Eq, k_refl(f(a))) : K_Eq(a)
coe(f_I(a), K_Eq, k_refl(f(a)))

// when f(f(a)) ≡ f(a)
Fix : Type;
fix(a : A) : Fix;
mod_I(a : A) : fix(a) == fix(f(a));
mod_J(a : A) : sym(mod_I(f(a))) == refl();

Fix = self(x) -> [a : A, p : fix(a) == x];
fix(a) = [f(a), sym(mod_I(a))];
mod_I(a) = [refl(), sym(mod_J(a))];
mod_J(a) = refl()ç


mod_J(a) : sym(mod_I(f(a))) == refl()
mod_J(a) : mod_I(f(a)) == refl()

mod_J(a).@ :
  coe(
    mod_J(a),
    λ(p) => [f(f(a)), refl()] == [f(f(f(a))), p],
    [refl(), sym(mod_J(a))],
  ) == [refl(), refl()]

mod_I(a).@ : [f(a), refl()] == [f(f(a)), sym(mod_I(f(a)))]


refl() == sym(mod(f(a)))

refl() == sym(mod(f(a)))
coe(mod(a), λ(x) => fix(a) == x, sym(mod(a))) == sym(mod(f(a)))

mod(a).@ :
  [
    f(a),
    coe(mod(a), λ(x) => fix(a) == x, sym(mod(a)))
  ] == [f(f(a)), sym(mod(f(a)))]
K_Eq(a : A) : Type;
k_refl(a : A) : K_Eq(f(a));
k_mod(a : A) : k_refl(a) == k_refl(f(a));

K_Eq(a) = self(k) -> [
  p : f(a) == a,
  q : k_refl(a) == ap(K_Eq, p, k)
];
k_refl(a) = [refl(), sym(k_mod(a))];
k_mod(a) = _;

k_mod(a).@ :
  [refl(), refl()] == [refl(), sym(k_mod(f(a)))]

[
  p : refl() == refl(),
  q : _ == sym(k_mod(f(a)))
]

J(k_mod(a))
k_mod(a).@ :
  [
    refl(),
    coe(
      k_mod(a),
      λ(k) => k_refl(f(a)) == k,
      sym(k_mod(a))
    )
  ] == [refl(), sym(k_mod(f(a)))]


k_mod(a).@
  coe(
    k_mod(a),
    λ(k) => [
      p : f(f(a)) == f(a),
      q : k_refl(f(a)) == ap(K_Eq, p, k)
    ],
    [refl(), sym(k_mod(a))]
  ) == [refl(), sym(k_mod(f(a)))]
k_mod(a).@ :
  [
    refl(),
    k_mod(f(a)) @ k_mod(a)
  ] == [refl(), k_mod(a)]
  coe(
    k_mod(a),
    λ(k) => k_refl(f(a)) == k,
    k_mod(f(a))
  ) == k_mod(a)
k_mod(a).@ :
  coe(
    k_mod(a),
    λ(k) => [
      p : f(f(a)) == f(a),
      q : k_refl(f(a)) == ap(K_Eq, p, k)
    ],
    [refl(), k_mod(f(a))]
  ) == [refl(), k_mod(a)]

k_refl(f(a)) == k_refl(a)

k_refl(a) = [refl(), _];
k_mod(a) = _;

k_refl(f(a)) == k_ap(k_refl(a))

//
A : Type;
f(a : A) : A;
f_I(a : A) : f(f(a)) == f(a);
f_J(a : A) : ap(f, f_I(a)) == f_I(f(a));

Fix : Type;
fix(a : A) : Fix;
mod_I(a : A) : fix(a) == fix(f(a));
mod_J(a : A) : mod_I(f(a)) == ap(fix, f_I(a));

Fix = self(x) -> [a : A, p : fix(a) == x];
fix(a) = [f(a), sym(mod_I(a))];
mod_I(a) = [sym(f_I(a)), ap(sym, mod_J(a))];
mod_J(a) = _;

mod_J(a).@ ==
  coe(
    mod_J(a),
    _,
    [sym(f_I(f(a))), ap(sym, mod_J(f(a)))]
  ) == ap(fix, f_I(a)).@

ap(fix, f_I(a)).@ :
  [
    f(f(f(a))),
    coe(
      ap(fix, f_I(a)),
      λ(x) => fix(f(f(f(a)))) == x,
      sym(mod_I(f(f(a))))
    )
  ] == [f(f(a)), sym(mod_I(f(a)))]

λ(i) => [f(f_I(a)(i)), sym(mod_I(f_I(a)(i)))]
[
  ap(f, f_I(a)),
  ap(sym, ap(mod_I, f_I(a)))
]
ap(fix, f_I(a)).@

ap(fix, f_I(a)).@
_ : sym(ap(fix, f_I(a))) == sym(mod_I(f(a)))

mod_I(a).@ : [f(a), refl()] == [f(f(a)), sym(mod_I(f(a)))];


Π(λ(x, a) => _)

Bool = Π(
  λ(b, C) => [
    Bool -> Type,
    C(true) -> C(false) -> C(b)
  ]);

Γ |- M : Π(A, B)  Γ |- N ⇐ A(M, N)
----------------------------------
Γ |- M(N) ⇒ B(M, N)



Γ |- M : Π(T)  Γ |- N ⇐ T(M, N).A
---------------------------------
Γ |- M(N) ⇒ T(M, N).B

(ι : λ(b) => Π(Bool -> Type, λ(C) => ))

Bool :=
  (b : Π(C(b : Bool) : Type, t : C(true), f : C(false)) -> C(b));


Type, A, B
Term, M, N, K ::=
  | Type
  | Var(l)(...N)
  | (l := Self(M))
  | (l := A -> B)
  | (l := λ. K)

// omitting the non referenced nodes
Id := (Type -> Var(Id) -> Var(Id))
id := λ. (r := λ. Var(r))

// cyclical
Bool := (
  (Bool -> Type) ->
  Var(Bool)(true) ->
  Var(Bool)(false) ->
  Var(Bool)(Self(Bool))
)

//


Type, A, B
Term, M, N, K ::=
  | Type | l(...N) | Fix(M)
  | (l := Self(M)) | (l := A -> B) | (l := λ. K)

Bool := Self(
  (Bool_C := (
      (Fix(Bool) -> Type) ->
      Bool_C(true) ->
      Bool_C(false) ->
      Bool_C(Bool)
    )
  )
)


//

Type, A, B
Term, M, N, K ::=
  | Type | l(...N)
  | (l := A -> B) | (l := λ. K)

Bool := (
  // can only be the cycle
  (Bool -> Type) ->
  // can only be the parameter
  Bool(true) ->
  Bool(false) ->
  // the parameter applied to itself
  Bool(Bool)
);


// which
Bool := (
  (Bool -> Type) ->
  Bool(true) ->
  Bool(false) ->
  Bool(Bool)
)



Term, M, N, K ::=
  | Type | (x : T) | (Π(x : A) : B)
  | x | λ(x) => K | M(N)

(l := (x : C[x])) |-> (x : C[x ⇒ [l]])

(u : Π(C) -> C(unit) -> C(unit))
(u : Π(C) -> C(unit) -> C(u))

unit : (u : Π(C) -> C(unit) -> C(u))
  = (u = λ(C) => λ(r) => r);

(u = unit.@) : (u : Π(C) -> C(unit) -> C(unit))

_X.@ := M
_X := (x = M)

_X := (x = _Y{x})

_Y{(x = _Y{x})} == M
_Y{(x = _Y{x})} == M

_X(a).@ := M
_X(b).@ := N


_X := (x = M)

Γ |- A : Type  Γ, f(a : A) : B, a : A |- B : Type
-------------------------------------------------
Γ |- (f(a : A) : B) : Type

Sigma(A, B) ==
  (p(C(a : A) : Type, c(a : A, b : B(a)) : C(a)) : C(fst(p)));
fst(p) == p(C(_) = A, c(a, b) = a);

// the return is sugar to sigma church-encoded
Type |-> (T(f : T) : (A(a : A(a)) : Type, B(a : A(a)) : Type))
// then we get something like this
(f(a : A) : B) |-> (T(f : T) = (A(a) = A, B(a) = B))


Γ, f(a : A) : B, a : A |- A : Type
Γ, f(a : A) : B |- N : B
----------------------------------
Γ |- f(a : A) = N : f(a : A) : B

Γ |- M : T  Γ |- N : T(M).0(N)
------------------------------
Γ |- M(N) : T(M).1(N)

Type(l) |-> (
  1 + l,
  T(f : T) : (
    A(a : A(a)) : Type(l),
    B(a : A(a)) : Type(l)
  )
)
// then we get somdething like this
(f(a : A) : B) |-> (
  max(A.0, B.0),
  T(f : T) = (A(a) = A, B(a) = B)
)



//
Γ, x : A |- A : Type
Γ, x : A |- B : Type
-------------------------
Γ |- Π(x : A) -> B : Type

Γ, x : A |- A : Type
Γ, x : A |- B : Type
-------------------------
Γ |- Σ(x : A) -> B : Type

Γ, x : T |- T : Type
--------------------
Γ |- (x : T) : Type


Bool = [
  c : C_Bool,
  p(
    C : _,
    r : C([c, p])
  ) : C([
    c_next(c),
    c_next_I(c)
  ]),
];


Γ, a : (a : A) |- A : Type
--------------------------
Γ |- (a : A) : Type

Γ |- M : A{a := M}
------------------ // fold
Γ |- M : (a : A)

Γ |- M : (a : A)
------------------ // unfold
Γ |- M : A{a := M}

Γ, f(a : A) : B, a : A |- A : Type
Γ, f(a : A) : B, a : A |- B : Type
----------------------------------
Γ |- (f(a : A) -> B) : Type

--------------------------
Γ |- λ(a) => K : ()

Γ |- M : f(a : A) -> B  Γ |- N : A
----------------------------------
Γ |- M(N) : B{f := M}{a := N}

//
Γ, f : f(x) -> T |- T : Type
-----------------------------
Γ |- f(x) -> T : Type

Γ, f : f(x) -> T |-
Γ, x : A |- K : T{f := λ(x) => K}
---------------------------------
Γ |- λ(x) => K : f(x) -> T




Bool = (b(C) -> C(λ(C, t, f) => t) -> C(λ(C, t, f) => f) -> C(b));


Γ |- M : Π(a : A) -> B
Γ, a : A, S == M(a) | a |- N : A
---------------------------------
Γ | S |- M(N) : B{a := N}


Nat = Π(
  K : Type,
  k(n(C : Type, z : C, s(k : K) : C) : C) : K
) -> K

Bool_S = [
  K : Type,
  k(x : K) : [
    true : K,
    false : K,
    b(
      C(b : K) : Type,
      t : C(true),
      f : C(false)
    ) : C(P(K)(k)),
   ] : K,
  x : K,
]

Bool_S = [
  K : Type,
  mod :
    (b(
      C(b : K) : Type,
      t : C(coe(mod, _(T) = T, fold(λ(C, t, f) => t))),
      f : C(coe(mod, _(T) = T, fold(λ(C, t, f) => f)))
    ) : C(coe(mod, _(T) = T, b))) == K
];
Bool =
  (b(
    C(b : #typeof(b)) : Type,
    t : C(fold(λ(C, t, f) => t)),
    f : C(fold(λ(C, t, f) => f))
  ) : C(b));
Bool_P : Bool_S = [
  (b(
    C(b : #typeof(b)) : Type,
    t : C(fold(λ(C, t, f) => t)),
    f : C(fold(λ(C, t, f) => f))
  ) : C(b)),
  refl()
];
Bool = Bool_P.K;


x : T;
x : (
  K(k : K(k)) : Type,
  k(x : F(K)) : K
) : K(x(K, k))

f : False;

f(
  False,
  k(f(C(f : False) : Type) : C(f(False, k))) = _
)


Unit : Type;
Unit =
  unit : Unit = (u(C, r) = r);
  u(C, r : C(unit)) : C(u)


Unit : Type;
Unit = (u(C(u : Unit) : Type, r : C(u(C, r : C(u)) = r)) : C(u));

Bool_S = (P : (
  K : Type,
  k(
    Bool : Type,
    true : P(Type, k(Bool, _, _) = Bool),
    false : P(Type, k(Bool, _, _) = Bool),
  ) : K
) -> K);


A : Type;
B : Type;
f(a : A) : B;
g(b : B) : A;
w(a : A) : Π(C(b : B) : Type) -> C(t) -> C(f) -> C(f(a));
g_f_id : g ∘ f == id;

w(a : A)(λ(b) = C(g(b)), t, f)

//

Unit = (u(C(u : C_Unit) : Type, r : C(c_unit)) -> C(u(C(_) = C_Unit, c_unit)));

Unit = (u(C(c : C_Unit) : Type, r : C(c_unit) :
  C(λ(C, r) => u(λ(_) = C, r)));

Unit = (u(C(c : C_Unit) : Type, r : C(c_unit) :
  C(λ(C, r) => u(λ(_) = C, r)));

u : Unit;
u.@(C(c : C_Unit) : Type, r : C(c_unit)) :
  C(λ(C, r) => u(λ(_) = C, r)))

u(
  C(u : Unit) : Type,
  r : C(unit),
) : C(u)

u(
  C : Type,
  r(H : unit == u) -> C,
) : C

u(
  C : Type,
  r(H(C, r) : r == u(C)(r)) -> C,
) : C

Unit = (u(K(k : C(k)) : Type, k : K(k)) : K(u(K, k)));
unit : Unit = λ(K, k) => k;

u : Unit;
// this works because
u : unit == u;
//
u(C(u : Unit) : Type, r : C(unit)) : C(u)
u : Π(
  K $ 0, k $ 0,
  C(k : K(k)) : Type,
  r : C(k)
) : C(u(K, k))


u : Unit;
u.@(
  C : Type $ 0,
  r : C $ 0,
  K : Type,
  k : (u(λ(_) = C, r)) == r) -> K
) : K

λ(C, r) =>
  c_unit(C(c_unit), r) == u(λ(_) = C(c_unit), r)

u.@(
  C(c) = (λ(C, r) => c(Unit, unit)(λ(_) = C, r)) == c,
  refl()
) : (λ(C, r) => u(λ(_) = C, r)(Unit, unit)(λ(_) = C, r)) ==
    (λ(C, r) => u(λ(_) = C, r))

u : Unit;
u.@(
  C(c : C_Unit, i : I_Unit(c)) : Type,
  r : C(c_unit)
) -> C(c_unit)

//
Unit = (u(K(k : C(k)) : Type, k : K(k)) : K(u(K, k)));
unit : Unit = λ(K, k) => k;

Unit(u : Unit(u)) = (
  C(u : Unit(u)) : Type,
  r : C(unit)
) : C(u)


Unit : Type;
I_Unit(u : Unit) : Type;
fold(u : I_Unit(fold(u))) : Unit;
unfold(u : Unit) : I_Unit(u);

Unit = [
  A : Type,
  fold(u : fold(λ(C, r) => r) == (fold(u) : A)) : A,
  unfold(a : A) : fold(λ(C, r) => r) == a,
  a : A
];

unfold(u : Unit) =
  u.unfold(a.a) :

Unit = [
  A = C_Unit,
  fold(u : fold(λ(C, r) => r) == (fold(u))) =
    u(
      λ(u) = C_Unit,
      c_unit
    ),
  unfold()
];

Nat = Π(
  K,
  k(n(C, z : C, s(p : K) : C) : C) : K,
) : K
X = Π(
  K : Type,
  k(
    u(C(k : K), r : C(unit?)) : C(u?)
  ) : K,
) : K


u : Unit;
u(
  C(a : Unit_A) =
    unit == g(a),
  refl()
) : unit == g(f(u))

(u(C(a : A) : Type, r : C(f(λ(C, r) => r))) -> C(f(u)))

f(u : Unit_M(Unit, f)) = λ(K, k) =>
  k(
    c(
      K,
      λ(k) => k,
      f(u : Unit_M(K, f)) = _
    )
  )

unit : Unit = λ(K, k) =>
  k(
    c(
      Unit_M,
      λ(u) => u(K, k),
      f(u : Unit_M(K, f)) =
        u(λ(C = _) = K, _ : k)
        _,
      _
    )
  )
f(u : Unit_M(Unit, f)) :

u : Unit;
u(

)


//
Unit = (u(
  C(x : X) : Type,
  r : C(unit)
) : C(λ(K, k) => k(X)(u)));

λ(C, r) => u(C, r)

f(u : Unit) : X;
g(x : X) : Unit;
g_f_id(u : Unit) : g(f(u)) == id(u);

unit : Unit = λ(K, k) => k;


//
S : Type = [Unit : Type, unit : Unit];
D(P : S) = [
  Unit(u : P.Unit) : Type;
  unit : Unit(P.unit)
];
F_Unit(P : S) = [
  Unit(u) = Π(C(u : P.Unit) : Type, r : C(P.unit)) -> C(u),
  unit = λ(C, r) => r,
];

S_Unit = (M_Unit(
  A : Type,
  f(a : M_Unit(A, f)) : A
) : Type);
(M_Unit : S_Unit)(A, f) =
  (u : f(λ(C, r) => r) == f(u));

Unit_Alg(X : Type) =
  Π(
    R : Type,
    in(a : M_Unit(R, in)) : R
  ) -> M_Unit(R, in);

Unit_R = [
  X : Type,
  f(x : X) : Unit_Alg(X),
  x : X
];
in(u : M_Unit(Unit_R, in)) = [
  X = (λ(C, r) => r) == (λ(C, r) => r),
  f(x) = λ(R, in) => λ(C, r) => r;
  refl()
];
out(u : Unit_R) : M_Unit(Unit_R, in) =
  λ(C, r) => u.f(u.x)(Unit_R, in);

Unit = M_Unit(Unit_R, in);
unit : Unit = λ(C, r) => r;

(λ(C, r) => r) == u
u.@ : in((λ(C, r) => r)) == in(u)

u.@(C(r) = (λ(C, r) => r) == out(r))



out(u : Unit) : M_Unit(Unit_R, in) = _;


ap(out, u.@) : out(in(λ(C, r) => r)) == out(in(u))

Bool = (b(
  C : Type,
  t(H : (λ(C, t, f) => t(refl())) == b) : C,
  f(H : (λ(C, t, f) => f(refl())) == b) : C,
) : C);


Self(T(x : T(x)) : Type) =
  Π(C : Type, c(x : T(x)) : C) : C;


Bool(b : Bool(b)) : Type =
  λ(C(b : Self(Bool)), t : C(true), f : C(false)) -> C(?)

Fix(
  T(x : T(x)),
  f(x : T(x)) : T(f(x))
) : T(fix(T, f));
Fix(T, f) = λ(C, c) =>
  c(f(?))


A(α) : Type;
A(α) = [
  a(β < α) : A(β),
  i : B,
  p(β < α) : a(β).i == i,
];
a(α) : Type;
a(α) = [a, x, refl()];

true = [true, λ(C, t, f) => t, refl()];
false = [false, λ(C, t, f) => f, refl()];

b.p : b.i(C(_) = Bool, true, false) == b
mod(
  i,
  p : i_true == i
) : true == [true, i, p];
mod(i, b) =
  [
    refl(),
    p,
    _ : coe(p, λ(i) => i_true == i, refl()) == p
  ]

//
A(α) : Type;
A(α) = [
  a(β < α) : A(β),
  i : B,
  p(β < α) : a(β).i == i,
];
a(α) : Type;
a(α) = [a, x, refl()];

//
Γ, X : Type, x : X |- T
------------------------------
Γ |- ν(X). Σ(x : X) -> F(X, x)

Σ(a : A) ->

A : Type;
A = [a : A, b : B];

K_Eq{A}(a : A) : Type;
k_refl{A}(a : A) : K_Eq(fst(a));
k_ind{A, a : A}(k : K_Eq(a)) : [fst(a), k_refl(a)] == [a, k];

K_Eq(a : A) : Type;
k_refl(a : A) : K_Eq(a);


P ⇒ (Π(a : A) : B)  Q ⇐ A
-------------------------
P(Q) ⇒ B(Q)

Γ |- P ⇒ (Π(a : A) : B) -| Δ
Γ, Δ |- Q ⇐ A -| Φ | _
----------------------------------
Γ |- P(Q) : B(Q) -| Γ, Δ | Γ, Δ, Φ


----------------------
Γ |- x ⇒ A -| (x ⇒ A)

Γ |- (P : A) -| Δ
-------------------
Γ |- (P : A) : Type


S : Type = [
  Bool : Type,
  true : Bool,
  false : Bool,
];
Bool_F(P : S, b : S.Bool) : Type =
  Π(C(b : P.Bool) : Type, t : C(P.true), f : C(P.false)) -> C(b);

eta{A : S}(b : Bool) = A.case(b, A.true, A.false);

map{A : S, B : S}(b, x : Bool_F(A, b))
  : Bool_F(B, A.case(b, B.true, B.false)) =
  (λ(C, t, f) = x(λ(b) = C(_), t, f));

map{A : S, B : S}(b, x : Bool_F(A, b))
  : Bool_F(B, A.case(b, A.true, A.false)) =
  (λ(C, t, f) = x(λ(b) = C(_), t, f));
map{A, A} == id

fmap{A : S, B : S}(f, b, x : Bool_F(A, b)) : Bool_F(B, f(b)) =
  (λ(C, t, f) = x(λ(b) = C(f(b))), t, f);

S : Type = [Bool : Type, true : Bool, false : Bool];
D(P : S) = [
  Bool(b : P.Bool) : Type,
  true : P.Bool(true),
  false : P.Bool(false),
];

Bool_I(P : S, b : S.Bool) : Type =
  Π(C(b : P.Bool) : Type, t : C(P.true), f : C(P.false)) -> C(b);
fmap{A : S, B : S}(f, b, x : Bool_I(A, b)) : Bool_I(B, f(b)) =
  (λ(C, t, f) = x(λ(b) = C(f(b))), t, f);

Bool_F(P : S) : D(P);
Bool_W(P : S) : S

f(P : S) : Bool_F(P),

S : Type = [
  Bool(b : Bool(b)) : Type,
  true : Bool(true),
  false : Bool(false),
];
Bool_F(P : S) : S = [
  Bool(b) = Π(
    C(b : P.Bool(b)) : Type,
    t : C(P.true),
    f : C(P.false)
  ) -> C(b);
  true = λ(C, t, f) => t,
  false = λ(C, t, f) => f
];

Bool_X : Type = [
  P : S,
  p : Bool_F(P) == P
];
Bool_P(b : Bool_P(b)) = [
  X : Bool_X,
  x : X.P.Bool(x),
]


A : Type;
F(A : Type) : Type;
p : Equiv(A, F(A));

//
Mu(F(X : Type) : Type) =
  Π(X : Type, k(x : F(X)) : X) -> X;
Nu(F(X : Type) : Type) =
  [X : Type, f(x : X) : F(X), x : X];

Stream_F(Stream : Type) : Type =
  Π(C : Type, c(p : Stream) : C) -> C;

Stream : Type = Nu(Stream_F);
omega : Stream = [
  X = Unit,
  f(x) = λ(C, s) => s(unit),
  unit
];

out(s : Stream) : Stream_F(Stream) =
  λ(C, c) => s.f(s.x)(C, λ(p) = c([X = s.X, f = s.f, x = p]))
in_(s : Stream_F(Stream)) : Stream =
  [
    X = Stream_F(Stream),
    f(s : Stream_F(Stream)) =
      λ(C, c) => s(C, λ(p : Stream) = c(out(p))),
    s
  ];

Nu(F(X : Type) : Type) =
  [X : Type $ 0, x : X, f(x : X) : F(X)];

Bool = [
  P : S $ 0,
  b : P.Bool,
  f(b : P.Bool) : Bool_F()
]
F(X : Type) : Type;

a : Nu(F);
b : Nu(F);
p(H : fmap(f, a.f(a.x)) == b.f(b.x)) : a == b;
p(H : fmap(f, a.f(a.x)) == b.f(b.x)) : a == b;



Stream : Type = Nu(Stream_F);


out(s : Stream) : Stream_F(Stream) =
  λ(C, c) => s.f(s.x)(C, λ(p) = c([X = s.X, f = s.f, x = p]))
in_(s : Stream_F(Stream)) : Stream =
  [
    X = Stream_F(Stream),
    f(s : Stream_F(Stream)) =
      λ(C, c) => s(C, λ(p : Stream) = c(out(p))),
    s
  ];
f(s : Stream) : Stream_F(Stream) =
  λ(C, s) => s.f(s.x) : Stream_F(s.X)
  (C, λ(p : s.X) =

    ) : Stream_F(s.X)


(ι : T)
(ι = λ(a : (ι : T), p : a(a, p) == N) => N)

Γ |- N(ι = N) : T(ι = N)
------------------------
Γ |- (ι = N) : (ι : T)


Interval : Type;
left : Interval;
right : Interval;
seg : left == right;

Interval = (i{C}(
  t : C(left),
  f : C(right),
  m : coe(seg, C, t) == f,
) : C(b));
left = (i{C}(t, f, m) = t);
right = (i{C}(t, f, m) = f);
seg = (s{C}(t, f, m) = m);

//
Interval : Type;
left : Interval;
right : Interval;
seg : left == right;
#close(Interval);

//
Interval =
  | ? left : Interval
  | ? right : Interval
  | ? seg : left == right
[left, right, seg] = Interval;

x = Interval.left;

//
Bool : Type;
true : Bool;
false : Bool;

Bool = (b{C}(t : C(true), f : C(false)) : C(b));
true = (b{C}(t, f) = t);
false = (b{C}(t, f) = f);


Either(A : Type, B : Type) : Type =
  [b : Bool, p : b(A, B)];

Either(A : Type, B : Type) : Type =
  | [tag == "left", p : A]
  | [tag == "right", p : B];

Bool : Type;
Bool =
  | ? true
  | ? false;


User : Type;
Valid(user : User) : Type;

User = (user : [
  id : Nat;
  name : String;
  valid : Valid(user);
]);
Valid(user) = [
  valid_id : user.id > 0;
  valid_name : user.name != "";
];


(b ? [M, N] : [L, R]) |->
  [b ? M : L, b ? N : R]

(b ? [M, N] : [L, R]) ==
  [b ? M : L, b ? N : R]


//
b : Bool;

b.is(
| true => _
| false => _
);

U(l) : Type;
El{l}(T : U(l)) : Type;

U(l) =
  | univ(l) : U(1 + l)
  | pi(A : U(l), B(a : El{l}(A)) : U(l)) : U(l)
El(T) =
  T.is(
  | univ(l) => U(l)
  | pi(A, B) => Π(a : El(A)) -> El(B(a))
  )

//
U : Type;
El(T : U) : Type;

U =
  | nat
  | arrow(A : U, B : U) : U
El(T) =
  T.is(
  | nat => Nat
  | arrow(A, B) => El(A) -> El(B)
  )

U(l, El{r < l}(T : U(r, El)) : Type) : Type;
U(l, El) =
  | univ(l) : U(1 + l)
  | pi(A : U(l, El), B(a : El{l}(A)) : U(l, El)) : U(l, El)

El{l}(T : U(l, El)) : Type;
El{l}(T) =
  T.is(
  | univ(l) => U(l, El)
  | pi(A, B) => Π(a : El(A)) -> El(B(a))
  );

Id{A}(x : A, y : A) : Type;
refl{A, x : A}() : Id(x, x);

Fun(A : Type, B : Type) : Type;
app{A, B}(f : Fun(A, B), x : A) : B;
ext{A, B, l, r}(
  f : Fun(A, B),
  m : Equiv(l, r)
) : Equiv(app(f, l), app(f, r))

Id(A, x, y) = (
  c(
    C : Fun([z : A, p : Id(x, z)], Type),
    r : app(C, [x, refl()])
  ) : app(C, [y, c])
);

U : Type;
Equiv(A : U, B : U) : Type;

U =
  | univ
  | pi(A : U, B(a : El(A)) : U) : U
  | ua{A, B}(a : Equiv(A, B)) : A == B;
Equiv(A, B) = _;



Fun(A : Type, B(a : A) : Type) : Type;
app{A, B}(f : Fun(A, B), x : A) : B;

Fun(A, B) = (l : [
  f(a : A) : B,
  m(
    g,
    H(a) : f(a) == g(a))
  ext(
    r : Fun(A, B),
    H(a : A) : app(l, a) == app(r, a)
  ) : l == r,
]);

Unit : Type;
unit : Unit;

Unit = (u : unit == u);
unit = refl();

//
Bool(b : Bool(b)) : Type;
true : Bool(true);
false : Bool(false);

Bool(b) = Π(
  C(b : Bool(b)) : Type,
  t : C(true),
  f : C(false)
) -> C(b);
true = λ(C, t, f) => t;
false = λ(C, t, f) => f;
//
Bool : Type;
true : Bool;
false : Bool;

Bool = λ(b) => Π(
  C(b : Bool) : Type,
  t : C(true),
  f : C(false)
) -> C(b);
true = λ(C, t, f) => t;
false = λ(C, t, f) => f;

Γ |- M : A  Γ |- A(M) == B(M)
-----------------------------
Γ |- M : B

Γ, a : T |- T is Type
---------------------
Γ |- λ(a) => T : Type


S = [Bool : Type, true : Bool, false : Bool];


P : Type =
  λ(Unit, u) => Π(
    C(u : Unit) : Type,
    r : C(λ(C, r) => r),
  ) -> C(u);

x : El(
  λ(Unit, u) => Π(
    C(u : Unit) : Type,
    r : C(λ(C, r) => r),
  ) -> C(u)
)


P : [Bool : Type, true : Bool, false : Bool] =
  μ(P) => [
    Bool = λ(b) => Π(
      C(b : P.Bool) : Type,
      t : C(P.true),
      f : C(P.false)
    ) -> C(b);
    true = λ(C, t, f) => t;
    false = λ(C, t, f) => f;
  ];

Γ, (a : A) == S |- M ⇐ A
------------------------
Γ, S |- μ(a) => M ⇐ A


Γ, x : A |- x : A

Eq{A}(x : A, y : A) : Type =
  λ(C(a : A) : Type, r : C) => C

true == false

//
A : Type;
X : Type;
L(x : X) : Type;

A = [x : X, l : L(x)];
X = [a : A, b : B(a)]
L(x) = [
  p : x.a.x == x,
  l : L([
    [
      x.a.x,
      x.a.l : L(x.a.x)
      _ : L(x.a.x)
    ],
    x.a.b
  ])
];


A : Type;
X : Type;
in_(x : X) : A;
ind(x : X) : B(in_(x));
out(a : A) : X;
K(x : X) : Type;

A = [x : X, k : K(x)];
X = [a : A, b : B(a), q : in_(out(a)) == a]
in_(x) = x.a;
ind(x) = x.b;
out(a) = a.x;
K(x) = [
  p : out(in_(x)) == x,
  k : K([in_(x), ind(x), ap(in_, p)])
];


K(A : Type, f(a : A) : A, l : A, r : A)
K(A, f, l, r) = [
  p : f(l) == f(r),
  q : K(
    f(l) == f(r)
  )
]

Fix(A, f) = [a : A, k : K(A, f, a)];
K(A, f, a) = [
  p : f(a) == a,
  k : K(
    Fix(A, f),
    ([a, k] : Fix(A, f)) => [f(a), coe(k.p, K(A, f), k.k)],
    [f(a), _ : K(A, f, f(a))]
  )
];

Fix(A : Type, f(a : A) : A) : Type;
K(A : Type, f(a : A) : A, a : A)
k(A, f, a, p : f(a) == a) : K(A, f, a);

Fix(A, f) = [a : A, k : K(A, f, a)];
K(A, f, a) = self(k) -> [
  p : f(a) == a,
  k : k(A, f, a, p) == k,
];
k(A, f, a, p) = [
  p,

]

K(x) = [
  p0 : f(x) == x,
  p1 : ap(f, p0) == f_I(x),
  p2 : _ == ,
  p2 : _
];

p1 :


_ :

x.q : in_(out(in_(x))) == in_(x)

ap(in_, p0) == in_(x)

ap(in_, p0) : in_(out(in_(x))) == in_(x)

K(x) = [
  p0 : out(in_(x)) == x,
  k0 : [
    p1 : out(in_(x)) == [in_(x), ind(x), ap(in_, p0)],
    k1 : K([in_(x), in_(x), ap(in_, p1)])
  ]
];
k_prop(x : A, l : K(x), r : K(x)) : l == r;
k_prop(x, l, r) = [
  _ : l.p == r.p,

]
K(x) = [
  p0 : out(in_(x)) == x,
  k0 : [
    p1 : out(in_(x)) == [in_(x), ind(x), ap(in_, p0)],
    k1 : [
      p2 : out(in_(x)) == [in_(x), ind(x), ap(in_, p1)],
      k2 : K([in_(x), in_(x), ap(in_, p2)])
    ]
  ]
];


in_(out(in_(x))) == in_(x)

self(a : A) = in_(out(a));



a.k.p : out(in_(out(a))) == out(a)

self_id(a : A) : self(a) = a;
self_id(a) = [
  a.k.p,
  _ : coe(a.k.p, K, self(a).k) == a.k,
  _ : out(in_(out(a))) == out(a)
  a.k.p :
]


self(a) = a.a;

K(a : A) : Type;
K(a) = [
  p : self(a) == a;
  k : K([
    self(a).a,
    self(a).b,
    ap(self, p)
  ]);
]ç


Fix(A, f) = [a : A, k : K(A, f, a)];
K(A, f, a) = [
  p : f(a) == a,
  k : K(
    Fix(A, f),
    ([a, k] : Fix(A, f)) => [f(a), coe(k.p, K(A, f), k.k)],
    [f(a), _ : K(A, f, f(a))]
  )
];


Fix(A, f(a : A) : A) : Type;
fix{A, f}(a : Fix(A, f), p : f(a) == a) : Fix(A, f);

out{A, f}(a : Fix(A, f)) : A;
mod{A, f}(a : Fix(A, f)) : f(out(a)) == out(a);

Fix : Type;
K(x : Fix, a : A) : Type;
fix(x : Fix, q : x.x == x) : Fix;

Fix = [
  x : Fix,
  a : A, k : K(a, x)]
K(a, x) = [
  p : x.a == a;
  q : x.x == x;
  k : K(x.a, fix(x, q))
];
fix(x, q) = [
  x.x,
  x.a,
  [
    q.a,

    _ : x.x.a == x.a
  ]
  _ : K(x.a, x.x)
];

Fix(A : Type, f(a : A) : A) : Type;
K(A : Type, f(a : A) : A, a : A)
k(A, f, a, p : f(a) == a) : K(A, f, a);

Fix(A, f) = [a : A, k : K(A, f, a)];
K(A, f, a) = self(k) -> [
  p : f(a) == a,
  k : k(A, f, a, p) == k,
];
k(A, f, a, p) = [
  p,

]

K(x) = [
  p0 : f(x) == x,
  p1 : ap(f, p0) == f_I(x),
  p2 : _ == ,
  p2 : _
];


//
Fix : Type;
Fix = [x : Fix, a : A, p : x.a == a];

//
Fix(α) : Type;
Fix(α) = [
  a : A,
  x(β < α) : Fix(β),
  p(β < α) : x(β).a == a;
];


inj(α, l : Fix(α), r : Fix(α), H : l.a == r.a) : l == r;
inj_x(α, l : Fix(α), r : Fix(α), H : l.a == r.a) : l.x == r.x;

inj(α, l, r, H) = [
  inj_x(α, l, r, H),
  H,
  (β < α) =>
    _ :
      coe(
        inj_x(α, l, r, H),
        (x) => x(β).a == r.a,
        l.p(β) @ H
      ) == r.p(β)
];
inj_x(α, l, r, H) = (β < α) =>
  inj(β, l.x(β), r.x(β), l.p(β) @ H @ sym(r.p(β)));

//
Fix(α) : Type;
Fix(α) = [
  a : A,
  x(β < α) : Fix(β),
  p(β < α) : x(β).a == a;
];

inj(α, l : Fix(α), r : Fix(α), H : l.a == r.a) : l == r;
inj_x(α, l : Fix(α), r : Fix(α), H : l.a == r.a) : l.x == r.x;

inj(α, l, r, H) = [
  H,
  inj_x(α, l, r, H),
  (β < α) =>
    _ :
        l.p(β) @ H @ sym(l.p(β)) @ sym(H) @ r.p(β) == r.p(β)
];

l.p(β) @ H @ sym(r.p(β))

inj(β, l.x(β), r.x(β), l.p(β) @ H @ sym(r.p(β))).a

inj_x(α, l, r, H) = (β < α) =>
  inj(β, l.x(β), r.x(β), l.p(β) @ H @ sym(r.p(β)));

K(a, x) = [
  p : x.a == a;
  q : x.x == x;
  k : K(x.a, fix(x, q))
];
fix(x, q) = [
  x.x,
  x.a,
  [
    q.a,

    _ : x.x.a == x.a
  ]
  _ : K(x.a, x.x)
];

Fix(A : Type, f(a : A) : A) : Type;
K(A : Type, f(a : A) : A, a : A)
k(A, f, a, p : f(a) == a) : K(A, f, a);


//
Fix(α) : Type;
Fix(α) = [
  a : A,
  x(β < α) : Fix(β),
  p(β < α) : f(x(β).a) == a;
];

spin(α, x : Fix(α)) :) Fix(α);
spin(α, x : Fix(α)) = [
  a = f(x(β)),
  x(β < α) : spin(β, x.x(β)),
  p(β < α) : x.p(β)
];

mod(α, x : Fix(α)) ==

inj(α, l : Fix(α), r : Fix(α), H : l.a == r.a) : l == r;
inj_x(α, l : Fix(α), r : Fix(α), H : l.a == r.a) : l.x == r.x;

inj(α, l, r, H) = [
  H,
  inj_x(α, l, r, H),
  (β < α) =>
    _ :
      l.p(β) @ H @ sym(ap(f, l.p(β) @ H @ sym(r.p(β)))) == r.p(β)
];
inj_x(α, l, r, H) = (β < α) =>
  inj(β, l.x(β), r.x(β), l.p(β) @ H @ sym(r.p(β)));



//
Fix : Type;
Fix = [a : A, x : Fix, p : f(x.a) == a];

spin(x : Fix) : Fix;
spin(x : Fix) = [f(x.a), spin(x.x), ap(f, x.p)];

mod(x : Fix) : spin(x) == x;
mod(x) = [
  x.p,
  mod(x.x),
  _ :
    coe(
      x.p,
      (a) => f(x.x.a) == a,
      coe(
        mod(x.x),
        (r) => f(r.x.a) == f(x.x.a),
        ap(f, x.p)
      )
    ) == x.p
]

sym(ap(f, x.p)) @ ap(f, mod(x.x.x).a) @ x.p == x.p

coe(
  ap(f, mod(x.x).x.a),
  (a) => a == f(x.x.a),
  ap(f, x.p)
) @ x.p == x.p

coe(
  mod(x.x),
  (r) => f(r.x.a) == f(x.x.a),
  ap(f, x.p)
) @ x.p == x.p

spin(x).p : f(f(x.x.a)) == f(x.a)
x.p : f(x.x.a) == x.a

inj(l : Fix, r : Fix, H : l.a == r.a) : l == r;
inj_x(l : Fix, r : Fix, H : l.a == r.a) : l.x == r.x;

inj(l, r, H) = [
  H,
  inj_x(l, r, H),
  _
];
inj_x(l, r, H) =
  inj(l.x, r.x, l.p @ H @ sym(r.p));


//
S : Type = [
  Bool : Type,
  true : Bool,
  false : Bool
];
D(P : S) : Type = [
  Bool(b : P.Bool) : Type,
  true : Bool(P.true),
  false : Bool(P.false)
];
F(P : S) : D(P);

P : S;
P = [
  Bool = [
    b : P.Bool,
    i : D(P).Bool(b),
    x :
  ]
];
//
Eq(A : Type, x : A, y : A) : Type;
Eq(A, x, y) = [
  c : C_Eq(A, x, y),
  k : Eq(
    [a : A, c : C_Eq(A, x, a)],
    [x, c_refl()],
    [y, c]
  )
];

K_Eq(a : A) : Type;
k_ap{a}(k : K_Eq(a)) : K_Eq(f(a));

K_Eq(a) = self(k) -> [
  p : f(a) == a,
  q : ap(f, p)
  q : coe(p, K_Eq, k_ap(k)) == k
];


k_mod{a}(k : K_Eq(a)) : f(a) == a;
k_ind{a}(k : K_Eq(a)) : [f(a), k_ap(k)] == [a, k];


Eq(A : Type, f, g, x : A, y : A) : Type;
Eq(A, f, g, x, y) = [
  c : C_Eq(A, f(x), g(y)),
  k : Eq(
    [a : A, c : C_Eq(A, f(x), a)],
    id,
    id,

    [f(x), c_refl()],
    [g(y), c]
  )
];


Fix(A : Type, f(a : A) : A) : Type;
K_Eq(A : Type, f(a : A) : A, a : A) : Type;

Fix(A, f) = [a : A, k : K_Eq(A, f, a)];
K_Eq(A, f, a) = self(k) -> [
  p : f(a) == a;
  k : [a, k] == x;
];

Fix(A : Type, f(a : A) : A) : Type;
fix{A, f}(a : A, p : f(a) == a) : Fix;



A : Type;
K(a : A, b : B(a)) : Type;

A = [a : A, b : B(a), k : K(a, b)];
K(a, b) = (k : a.a == [a, b, k]);

//
A : Type;
K(a : A, b : B(a)) : Type;

A = [a : A, b : B(a), k : K(a, b)];
K(a, b) = (k : [
  p : [a.a.a, a.a.b] == [a, b],
  q : coe(p, K, a.a.k) == k,
]);

//
A : Type;
X : Type;
sect(a : A) : X;
retr(x : X) : A;

A = [x : X, k : sect(retr(x)) == x];
X = [a : A, b : B(a)];
sect(a) = a.x;
retr(x) = x.a;


//
A(α) : Type;
X(α) : Type;
sect{α}(a(α) : A(α)) : X(α);
retr{α}(x : X(α)) : (α) -> A(α);

A(α) = (β < α) -> [x : X(β), k : sect(retr(x)) == x];
X(α) = [a : (α) -> A(α), b : B(a)];
sect{α}(a) = a(1 + α)(α).x;
retr{α}(x) = x.a;

self{α}(a : (α) -> A(α)) : A(α) = retr(sect{α}(a))(α);
self{α}(a : A(α)) : retr{α}(sect{α}(a)) == a;
self_I(a) : self(self(a)) == self(a);
self_I(a) = [
  [
    self_I(self(a)),
    _ : coe(
      self_I(self(a)),
      B,
      sect(self(self(a))).b) ==
      sect(self(a)).b
  ],
  _
];


//
A(α) : Type;
X(α) : Type;
sect{α}(a : A(α)) : (β < α) -> X(β);
retr{α}(x : X(α)) : (β < α) -> A(β);
A.fmap{α, β}(a : A(α)) : A(β);
B.fmap{α, β}(a : A(α)) : A(β);

A(α) = [
  x(β < α) : X(β),
  k(β < α, δ < β) : sect(retr(x(β))(δ)) == x
];
X(α) = [a(β < α) : A(β), b : B(a)];
sect{α}(a) = a.x;
retr{α}(x) = x.a;

//
self{α}(a : A{α}) : (β < α) -> A = retr(sect(a));
self_I(a) : self(self(a)) == self(a);
self_I(a) = [
  [
    self_I(self(a)),
    _ : coe(
      self_I(self(a)),
      B,
      sect(self(self(a))).b) ==
      sect(self(a)).b
  ],
  _
];

self(a).k : sect(self(self(a))) == sect(self(a))

retr(sect(self(self(a)))) == retr(sect(self(a)))
self(self(self(a))) == self(self(a))

self(a : A) : A = retr(sect(a));
self_I(a) : self(self(a)) == self(a);
self_I(a) = [
  [
    self_I(self(a)),
    _ : coe(
      self_I(self(a)),
      B,
      sect(self(self(a))).b) ==
      sect(self(a)).b
  ],
  _
];
self_I(a) = [
  self(a).k,
  _ : coe(self(a).k, (x) => sect(retr(x)) == x, self(self(a)).k) == self(a).k
];

ap(sect, ap(retr, sym(self(a).k))) @ self(self(a)).k @ self(a).k == self(a).k
sym(ap(sect, ap(retr, a.x.a.k))) @ a.x.a.x.a.k == refl()


a.k.J(
  (x, k : sect(self(a)) == x) =>
    _
)
a.x.a.k.J(
  (x, k : sect(self(self(a))) == x) =>

    sym(ap(sect, ap(retr, k))) :
      sect(retr(x)) == sect(retr(sect(self(self(a)))))

    x.a.k : sect(retr(sect(retr(x)))) == sect(retr(x))

    _ == refl
)
sect(retr(sect(self(self(a)))))
sect(retr(sect(retr(x))))

a.x.a.k : _ == a.x.a.x

self(a).k : sect(self(self(a))) == sect(self(a))

self(self(a)).k : sect(self(self(self(a)))) == sect(self(self(a)))





coe(self(a).k, (x) => sect(retr(x)) == x, self(self(a)).k)


retr(sect(retr(sect(a))))

retr(sect(retr(sect(a)))) == retr(sect(a))

a.k : sect(retr(sect(a))) == sect(a)
a.k : a.x.a.x == a.x

a.k.a.x.a == a.x.a.k.a

ap(retr, ap(sect, ap(retr, a.k))) ==
  ap(retr, retr(sect(a)))


self_J(a) : ap(self, self_I(a)) == self_I(self(a));
self_J(a) =
  _

a.k : sect(retr(sect(a))) == sect(a)
ap(retr, a.k) : self(self(a)) == self(a)
a.k : sect(retr((a))) == sect(a)


//
A : Type;
X : Type;
sect(a : A) : X;
retr(x : X) : A;

A = [x : X, k : sect(retr(x)) == x];
X = [a : A, b : B(a)];
sect(a) = a.x;
retr(x) = x.a;

ap(sect, ap(retr, a.x.a.k)) == retr(a.x).k

ap(sect, ap(retr, a.x.a.k)) == a.x.a.k

self(a).k : sect(self(self(a))) == sect(self(a))

ap(sect, ap(retr, k)) :
  : sect(self(self(self(a)))) == sect(retr(x))

sym(ap(sect, ap(retr, k)))
  : sect(retr(x)) == sect(self(self(self(a))))

k : sect(self(self(a))) == x

a.x.a.k.J(
  (x, k : sect(self(self(a))) == x) =>
    _
)


sym(ap(sect, ap(retr, a.x.a.k))) @ a.x.a.x.a.k == refl()

sym(ap(sect, ap(retr, a.x.a.k))) @ a.x.a.k == refl()

a.x.a.k == a.x.a.x.a.k

a.x.a.k.a.x == a.x.a.x.a.k



A : Type;
out(a : A) : [a : A, b : B(a)];
fst_out_is_id(a) : fst(out(a)) == a;

//
A : Type;
self(a : A) : A;
out(a : A) : B(self(a));
self_is_id(a) : self(a) == a;

A = [
  a : A,
  b : B(a),
  k : Path((j) => B(self_is_id(a)(j)), out(a), b)
];
self(a) = a.a;
out(a) = a.b;
self_is_id(a) = (i) => [
  self_is_id(self(a))(i),
  a.k(i),
  (j) => self_is_id(a)(i).k(j)
]


[
  self(self(a)),
  out(self(a)),
  self(a).k
]

self_is_id(a)(i).k(j) :
  B(self_is_id(self(self_is_id(a)(i)))(j))
(j) : B(self_is_id(self_is_id(self(a))(i))(j)) =>
  self_is_id(a)(i).k(j)
Path(
  (j) => B(self_is_id(self_is_id(self(a))(i))(j)),
  self(a).k(i),
  a.k(i)
)

coe(
  (h) =>
    Path(
      (j) => B(self_is_id(self_is_id(self(a))(i))(j)),
      self(a).k(i),
      a.k(i)
    ),
)

coe(
  (j) => B(self_is_id(a)(min(i, j))),
  a.k(0)
)

self_is_id(a) = [
  self(a),
  coe((j) => B(self_is_id(self(a))(j)), out(self(a))),
  _
]


coe(
  (j) => B(self_is_id(self_is_id(a)(i))(j)),
  out(self_is_id(a)(i))
) ==
coe(
  (j) => B(self_is_id(a)(min(i, j))),
  a.k(i)
)


coe(
  (j) => B(self_is_id(self(a))(min(i, j))),
  self(a).k(i)
)

coe(
  (j) => B(self_is_id(self(a))(min(i, j))),
  a.k(i)
) == coe(
  (j) => B(self_is_id(self_is_id(self(a)(i)))(j)),
  out(self_is_id(self(a))(i))
)





B(self_is_id(self(a))(0))

B(self_is_id(self(a))(i))
coe(
  (j) => B(self_is_id(a)(j)),
  a.b
)

coe(
  (j) => B(self_is_id(a)(j)),
  a.b
)
a.k : coe((i) => B(self_is_id(self(a))(i)), out(self(a))) ==
  a.b : B(self(a))

a.k : coe((i) => B(self_is_id(a.a)(i)), out(a.a)) == a.b

coe(
  (j) => B(self_is_id(self(a))(j)),
  out(self(a))
)

[
  self(a),

]

[
  self(self(a)),
  out(self(a))
] == self(a)


coe(
  (j) => B(self_is_id(self(a))(min(i, j))),
  out(self(a))
) : B(self_is_id(self(a))(i))

a.k(i) : B(a.a)

coe(
  (j) => B(a.k(max(i, j))),
  _ : B(a.k(i))
)

a.k(i) : B(a.a)

self()

self_is_id(a) = [
  self_is_id(self(a)),
  _ : coe(
        self_is_id(self(a)),
        (a) => [b : B(a), k : coe(self_is_id(a), B, out(a)) == b],
        [a.a.b, a.a.k]
      ) == [a.b, a.k]
]

coe(
  (i) => [
    b : B(self_is_id(self(a))(i)),
    k : coe(
          self_is_id(self(self_is_id(a)(i))),
          B, self_is_id(self(a))(i).b) == b
  ], [a.a.b, a.a.k])

coe(
  (i) => [
    b : B(self_is_id(self(a))(i)),
    k : coe(
          self_is_id(self_is_id(self(a))(i)),
          B, out(self_is_id(self(a))(i))) == b
  ], [a.a.b, a.a.k])

self_is_id(a) = [
  self_is_id(self(a)),
  _ : coe(
        self_is_id(self(a)),
        (a) => [b : B(a), k : coe(self_is_id(a), B, out(a)) == b],
        [a.a.b, a.a.k]
      ) == [a.b, a.k]
]


self_is_id(self(a))

coe(
  a.k,
)

a.a.k : : coe(self_is_id(a.a.a), B, a.a.a.b) == a.a.b

a.k : coe(self_is_id(a.a), B, a.a.b) == a.b


fst_is_id_fst(a) : ap(fst, fst_is_id(a)) == fst_is_id(fst(a));
fst_is_id(a) = [
  fst_is_id(fst(a)) : fst(fst(a)) == fst(a),
  _ : coe(fst_is_id(fst(a)), B, fst(a).b) == a.b,
  _
]


out(a : A) : [a : A, b : B(a)];
fst_out_is_id(a) : fst(out(a)) == a;



//
A : Type;
self(a : A) : A;
out(a : A) : B(self(a));
self_is_id(a) : self(a) == a;

A = [
  a : A,
  b : B(a),
  k : Path((j) => B(self_is_id(a)(j)), out(a), b)
];
self(a) = a.a;
out(a) = a.b;
self_is_id(a) = (i) => [
  self_is_id(self(a))(i),
  a.k(i),
  // ?????

  self_is_id(a)(i).k
];

(j) : B(self_is_id(self_is_id(self(a))(i))(j)) =>
  coe(
    (h) => B(self_is_id(self_is_id(self(a))(i))(j)),
    _
  );


a.k(i, j, h)

Path(
  (j) => B(self_is_id(self_is_id(self(a))(i))(j)),
  self(a).k(i),
  a.k(i)
)

self_is_id(a)(i).k :

[
  self_is_id(self(a))(i),
  a.k(i),
  (j) => self_is_id(a)(i).k(j)
]

self(self_is_id(self_is_id(a)(i))(j))

(j) : B(self_is_id(self_is_id(self(a))(i))(j)) =>
  coe(
    (h) => self(self_is_id(self_is_id(a)(i))(j))
  )
  _




//
A(α) : Type;
self{α}(a : A(α)) : (β < α) -> A(β);
out{α}(a : A(α)) : (β < α) -> B{β}(self(a)(β));
self_is_id{α}(a : A(α)) : (β < α) -> self(a)(β) == a;

A(α) = (β < α) -> [
  a : A(β),
  b : B{β}(a),
  k(δ < β) : Path(
    (j) => B{δ}(self_is_id{β}(a)(δ)(j)),
    out(a)(δ),
    b
  )
];
self{α}(a) = (β < α) => a(β).a;
out{α}(a) = (β < α) => a(β).b;
self_is_id{α}(a) = (β < α) => (i) => (δ < β) => [
  self_is_id(self(a))(i),
  a.k(β)(i),
  _ :
  (δ < β) => self_is_id{β}(a)(δ)(i).k
]

//
A : Type;
X : Type;
sect(a : A) : X;
retr(x : X) : A;

A = [x : X, k : sect(retr(x)) == x];
X = [a : A, b : B(a), j : retr(sect(a)) == a];

a.k : sect(retr(sect(a))) == sect(a)
self(a : A) : A = retr(sect(a));

self_I(a : A) : self(self(a)) == self(a);
self_I(a) = ap(retr, a.k);

(i) => retr(a.k(i))

(i) => retr(retr(sect(a)).k(i))
(i) => retr(sect(retr(a.k(i))))

(i) => retr(sect(a)).k(i)
(i) => sect(retr(a.k(i)))

a.k(i).j : retr(sect(a.k(i).a)) == a.k(i).a
a.x.a
a.x.j : retr(sect(a.x.a)) == a.x.a

retr(sect(a))


a.k(i).j : retr(sect(retr(a.k(i)))) == retr(a.k(i))

self_J(a) : self_I(self(a)) == ap(self, self_I(a));
self_J(a) = (i) =>

  coe(
    (j) =>
  )
  _ :

out(a : A) : B(self(a));
self_is_id(a) : self(a) == a;

//
//
A : Type;
X : Type;
sect(a : A) : X;
retr(x : X) : A;

K(x : X) : Type;
fix(x : X, p : sect(retr(x)) == x) : A;
fix_K(x, p) : K(sect(fix(x, p)));

A = [x : X, k : K(x)];
X = [a : A, b : B(a), j : retr(sect(a)) == a];
sect(a) = a.x;
retr(x) = x.a;

K(x) = [
  p : sect(retr(x)) == x,
  q : ap(retr, p) == x.j
];
fix(x, p) = [
  sect(retr(x)),
  fix_K(x, p)
];
fix_K(x, p) = [
  _ : sect(retr(sect(retr(x)))) == sect(retr(x));
];

K(x) = [p : x.a.x == x, q : K(fold(x, p))];
fold(x, p) = [
  [
    fold(x, p)
    _ : B(fold(x, p))
  ],
  _ : K()
];
x.b : B(x.a)
fold(x, p) = [x, coe((i) => K(p(i)), x.a.k)];

coe(
  (i) =>
    [p1 : p(i).a.x == p(i), q1 : K(fold(p(i), p1))],
  x.a.k
)

sym(p.a.x) @ x.a.k.p @ p

coe(
  (i) => p(i).a.x == p(i),
  x.a.k.p
)




K(a, b) = [
  p : a.a == b,
  k : _
];

//
A : Type;
X : Type;
K(x : X) : Type;
self(x : X) : X;
self_I(x) : self(self(x)) == self(x);
self_J(x) : ap(self, self_I(x)) == self_I(self(x));

A = [x : X, k : K(x)];
X = [a : A, b : B(a)];
K(x) = [p : self(x) == x, q : ap(self, p) == self_I(x)];
self(x) = x.a.x;
self_I(x) = x.a.k.p;
self_J(x) = x.a.k.q;

ap(self, x.a.k.p)

self_J(x : X) : ap(self, self_I(x)) == self_I(self(x)) =
  x.a.k.q;
  _


p : x.a.x == x
p.a.x : x.a.x.a.x == x.a.x

x.a.k.p : x.a.x.a.x

sect(retr(sect(retr(x)))) == sect(retr(x))

self(x : X) : X = sect(retr(x));
self_I(x : X) : self(self(x)) == self(x) = x.a.k.p;

x.a.k.p.a.x == x.a.x.a.k.p
x.j : x.a.x.a == x.a

x.a.k.q : x.a.k.p.a == x.j

self_J(x : X) : ap(self, self_I(x)) == self_I(self(x)) =
  _




//
A : Type;
X : Type;
K(x : X) : Type;
self(x : X) : X;
self_I(x) : self(self(x)) == self(x);
self_J(x) : ap(self, self_I(x)) == self_I(self(x));

A = [x : X, k : K(x)];
X = [a : A, b : B(a)];
K(x) = [p : self(x) == x, q : ap(self, p) == self_I(x)];
self(x) = x.a.x;
self_I(x) = x.a.k.p;
self_J(x) = x.a.k.q;


//
A : Type;
self(a : A) : A;
self_I(a : A) : self(self(a)) == self(a);
self_J(a : A) : self_I(self(a)) == ap(self, self_I(a));

A = [
  a : A,
  b : B(a),
  p : self(a) == a,
  q : self_I(a) == ap(self, p)
];
self(a) = a.a;
self_I(a) = a.p;
self_J(a) = a.q;



funext{A, B}(
  f(a : A) : B, g(a : A) : B,
  H(a) : f(a) == g(a)) : f == g =
    h(i : Interval)(a : A) = i ? f(a) : g(a) % H(a);
    (ap(h, seg) : h(left) == h(right));

Sorted_list(A : Type) =
  (l : List(A)) % sort(l) == l;


// using self types
A : Type;
A = (a : B(a));

// using quotients
A : Type;
X : Type;
self(x : X) : X;

A = (x : X) % self(x) == x;
X = [a : A, b : B(a)];
self(x) = x.a.x;



//
incr : Nat -> Nat = λ(n) => 1 + n;

id : <A>(x : A) => A;

get{A}(i : Int >= 0, arr : Array(A)) : A;

log(msg : String) : [] % IO;

main() : [] % IO;
main() =
  [] = log("tuturu");
  _;

sock0 : Socket;
sock1 = sock0.write("hello\n");
[] = sock1.close();

// compile time error
get(-1, _)


make(i : Int, p : i >= 0) = _;


_ : 1 + 1 == 3 = refl();

f(b : Bool, x : b ? Nat : String) = _;

f(true, 1 : Nat)
f(false, "a" : String)

x : String = id<String>("a");
y : Nat = id<Nat>(1);


//
Γ |- M : (a : A == N) -> B(a)
Γ |- M(N) == N
----------------------------
Γ |- fold(M) : (ι : B) as A

Bool : Type;
true : Bool;
false : Bool;

Bool = (b(C(b : Bool) : Type) -> C(true) -> C(false) -> C(b));
true = λ(b : Bool == true) =>
  λ(C, t, f) => t;

b.@ == λ(C, t, f) => t


A : Type;
// by def
f(f(a)) ≡ f(a);

Fix : Type;
fix(a : A) : Fix;
mod(a : A) : fix(f(a)) == fix(a);

Fix = (x(C(x : Fix) : Type, r(a : A) : C(fix(a))) : C(x));
fix(a) = (x(C, r) = coe(mod(a), C, r(f(a))));


fix(a : A) : Fix;
fix_I(a : A) : fix(f(a)) == fix(a);

Fix = (x : [a : A, p : fix(a) == x]);
fix(a) = (x = [f(a), fix_I(a)]);
fix_I(a) = λ(i) =>
  (x = [
    f(a),
    _ : fix(f(a)) == fix_I(a)(i)
  ]);

_ : fix(f(a)) == fix_I(a)(i)

fix_I(a)(0) == fix(f(a))
fix_I(a)(1) == fix(f(a))

(x = [
  f(a),
  fix_I(a)
])(0) == fix(f(a))

(x = [
  f(a),
  coe(
    (j) => fix(f(a)) == fix_I(a)(min(1, j)),
    refl()
  )
])

[
  f(a),
  refl()
]

fix_J(a).@ : fix_I(f(a)).@ == refl();
fix_J(a).@ : [
  q_a : refl() == refl(),
  q_j : fix_J(f(a)) @@ fix_J(a) == refl()
]
fix_I(a).@ : [
  p_a : f(f(a)) == f(a),
  p_j : fix_I(f(a)) @@ fix_I(a) == fix_I(a)
]

fix_I(a).@ : [
  p_a : f(f(a)) == f(a),
  p_j : fix_I(f(a)) @@ fix_I(a) == fix_I(a)
]
fix_I(a).@ :
  coe(fix_I(a), λ())
 [f(f(a)), fix_I(f(a))] == [f(a), fix_I(a)]



fix(f(a)) == fix(a)

(l := refl()) == (l := refl())

(x = [f(f(a)), fix_f_id(f(a))]) == (x = [f(a), fix_f_id(a)])
fix_f_id(f(a)) == fix_f_id(a)

λ(C, r) => (r : _) == λ(C, r) => (r : _)

fix_f_id(f(a)) == fix_f_id(a)

[f(f(a)), fix_f_id(f(a))] == [f(a), fix_f_id(a)]

//
Fix : Type;
fix(a : A) : Fix;
out(a : A) : A;
mod(x : Fix) : f(out(a)) == out(a);
mod_I(a : A) : fix(f(a)) == fix(a);
mod_J(a) : mod_I(f(a)) == refl();

Fix = (x : [a : A, p : fix(a) == x]);
fix(a) = [f(a), mod_I(a)];
mod_I(a) = [refl(), mod_J(a)];
mod_J(a) = λ(i) => λ(j) => [f(a), _ : fix(f(a)) == mod_J(a)(i)(j)]

fix(f(a)) == mod_J(a)(i)(j)

mod_I(a).@.p_a : f(f(a)) == f(a);
mod_I(a).@.p_j : mod_I(f(a)) @ mod_I(a) == mod_I(a);


//
Fix : Type;
fix(a : A) : Fix;
out(x : Fix) : A;
mod(a : A) : fix(f(a)) == fix(a);
ext{x, y}(H : out(x) == out(y)) : fix(x) == fix(y);


Fix = (x : [a : A, p : fix(a) == x]);
fix(a) = [f(a), mod(a)];
out(x) = x.@.a;
mod(a) = _ : fix(f(a)) == fix(a);


K_Eq(a : A) : Type;
k_refl(a : A) : K_Eq(f(a));
k_mod(a) : k_refl(f(a)) == k_refl(a);

K_Eq(a : A) = (k : [
  p : f(a) == a,
  k : coe(p, K_Eq, k_refl(a)) == k
]);
k_refl(a) = [refl(), k_mod(a)];
k_mod(a) = λ(i) => [
  refl(),
  coe(
    (j) => k_refl(f(a)) == k_mod(a)(max(i, j)),
    refl()
  )
];

refl() == k_mod(f(a))
refl() ==
  coe(
    (j) => k_refl(f(a)) == k_mod(a)(max(i, j)),
    refl()
  )

coe(
  (j) => k_refl(f(a)) == k_mod(a)(max(0, j)),
  refl()
)

K_Eq(
  A : Type,
  f(a : A) : A,
  ∀(a) -> f(f(a)) == f(a),
  a : A
) : Type;
k_refl(a : A) : K_Eq(f(a));

K_Eq(a : A) = (k : [
  p : f(a) == a,
  k : coe(p, K_Eq, k_refl(a)) == k
]);
k_refl(a) = [refl(), k_mod(a)];


A : Type;
f(a : A) : A;
f(f(x)) ≡ f(x)

L(a : A) = [
  p0 : f(a) == a,
  p1 : sym(ap(f, p0)) @ p0 == p0,
  p2 : ap(λ(p0) => sym(ap(f, p0)) @ p0, p1) == refl(),

]

sym(ap(f, p)) @ p == sym(ap(f, p)) @ p

R(a : A) = [
  p0 : f(a) == a,
  p1 : ap(f, p) == refl(),
  p2 : ap(ap(f), p1) == p1
  ...
]

K_Eq(x : A, p : f(x) == x) : Type;
K_Eq(x, p) = [
  q : f(x) == x,
  w : p == coe(q, (x) => f(x) == x, f_I(x))
  k :
];



t_refl(a : A) : T_Eq(f(a));
t_refl(a) = [f(a), refl(), t_refl(a)];

Fix : Type;
T_Eq(a : A) : Type;

Fix = [a : A, t : T_Eq(a)];
T_Eq(a) = [x : Fix, p : f(x.a) == a];

fix(a : A) : Fix
t_refl(a : A) : T_Eq(f(a));

fix(a) = [f(a), t_refl(a)];
t_refl(a) = [fix(a), refl()];

mod(x : Fix) : fix(x.t.x.a) == x;
mod_t(t : _) : _;

mod(x) = [
  x.t.p,
  _ : [fix(x.t.x.a), x.t.p] == x.t,
];

[fix(x.t.x.a), x.t.p] == x.t
[
  p : fix(x.t.x.a) == x.t.x,
  q : coe(p, λ(l) => f(l.a) == x.a, x.t.p) == x.t.p
]

sym(ap(f, ap(.a, p))) @ x.t.p == x.t.p

[f(x.t.x.a), t_refl(x.t.x.a)] == x.t.x


coe(x.t.p, T_Eq, [fix(x.t.x.a), refl()])

x.t.x
x.t.p

[fix(x.t.x.a), x.t.p]
coe(x.t.p, T_Eq, t_refl(x.t.x.a))
mod_t()

t_ap{a}(t : T_Eq(a)) : T_Eq(f(a));
t_ap{a}(t) =
  [
    f(t.y),
    ap(f, t.p),
    t_ap(t.t)
  ];



sect(x : Fix) : A = x.a;
retr(a : A) : Fix = _


K(a : A) : Type;
K(a) = (k : [y : A, p : f(y) == a]);



//
Fix : Type;
T_Eq(a : A) : Type;

Fix = [a : A, t : T_Eq(a)];
T_Eq(a) = [x : Fix, p : f(x.a) == a];

fix(a : A) : Fix
t_refl(a : A) : T_Eq(f(a));

fix(a) = [f(a), t_refl(a)];
t_refl(a) = [fix(a), refl()];

mod1(x : Fix) : [f(x.t.x.a), [x.t.x, refl()]] == x;
mod1(x) = [x.t.p, refl()];

mod2(l : Fix, r : Fix, H : l.t.x == r.t.x) : l == r;
mod2(l, r, H) =
  coe(mod1(l), λ(x) => x == r,
    coe(mod1(r), λ(x) => _ == x,
      ap((x) => [f(x.a), [x, refl()]], H)));

mod3(x : Fix) : x.t.x == x;
mod3(x) = [
  sym(x.t.x.t.p) @ ap(f, x.t.x.t.p) @ x.t.p,
  // relying on transparent fixpoint
  [mod3(x.t.x), refl()]
];

sym(ap(f, x.t.x.t.p)) @
    ap(f, x.t.x.t.p) @ x.t.p == x.t.p

sym(ap(f, sym(x.t.x.t.x.t.p) @ ap(f, x.t.x.t.x.t.p) @ x.t.x.t.p)) @
    ap(f, x.t.x.t.p) @ x.t.p == x.t.p

ap(f, x.t.x.t.p) : f(f(x.t.x.t.x.a)) == f(x.t.x.a)

sym(x.t.x.t.p) : x.t.x.a == f(x.t.x.t.x.a)
sym(x.t.x.t.p) @ ap(f, x.t.x.t.p) : x.t.x.a == f(x.t.x.a)

sym(x.t.x.t.p) @ ap(f, x.t.x.t.p) @ x.t.p

x.t.x.t.p : f(x.t.x.t.x.a) == x.t.x.a

x.t.p : f(x.t.x.a) == x.a

mod3(x : Fix) : fix(x.t.x.a) == x;
mod3(x) = [
  x.t.p,
  [
    p : fix(x.t.x.a)
  ]
  _ : [fix(x.t.x.a), x.t.p] == [x.t.x, x.t.p]
]

mod2(x : Fix) : fix(x.a) == x;
mod2(x) = [
  sym(ap(f, x.t.p)) @ x.t.p,
  _ : [
    p : fix(x.a) == x.t.x,
    q : sym(ap(f, ap(.a, p))) @ sym(ap(f, x.t.p)) @ x.t.p
        == x.t.p
  ]
  _ : [
    fix(x.a),
    sym(ap(f, x.t.p)) @ x.t.p
  ] == [x.t.x, x.t.p]
]
sym(ap(f, ap(.a, p))) : f(x.t.x.a) == f(f(x.a))

x.t.p : f(x.t.x.a) == x.a

mod2(x : Fix) : fix(x.t.x.a) == x;
mod2(x) = [
  x.t.p,
  _ : [
    p : fix(x.t.x.a) == x.t.x,
    q : coe(ap(f, ap(.a, p)), λ(l) => l == x.a, t_refl(x.t.x.a)) == x.t.p
  ]
  x.t.p : f(x.t.x.a) == x.a
  coe(
    J(x.t.p),
    λ(r, p : f(x.t.x.a) == r) =>
      ([fix(x.t.x.a), r] : T_Eq(r)) == [x.t.x, r],
    [
      mod2(x)
    ]
    _ : [fix(x.t.x.a), refl()] == [x.t.x, refl()]
  )

  x.a
  _ : [fix(x.t.x.a), x.t.p] == [x.t.x, x.t.p],
]


mod2(x : Fix) : fix(x.t.x.a) == [f(x.t.x.a), [x.t.x, refl()]];
mod2(x) = [
  refl(),
  [
    p : fix(x.t.x.a) == x.t.x,
    q : coe(ap(f, ap(.a, p)), λ(l) => l == f(x.t.x.a), refl()) == refl()
  ]
  _ : t_refl(x.t.x.a) == [x.t.x, refl()]
]

mod1(x : Fix) : [f(x.t.x.a), [x.t.x, refl()]] == x;
mod1(x) = [
  x.t.p,
  _ : coe(x.t.p, T_Eq, [x.t.x, refl()]) == x.t,
]

mod1(x : Fix) : [
  f(x.t.x.a),
  coe(sym(x.t.p), T_Eq, x.t)
] == x;


t_ap{a}(t : T_Eq(a)) : T_Eq(f(a))
t_ap{a}(t) = [
  [f(t.x.a), t_ap(t.x.t)],
  ap(f, t.p)
];

mod(x : Fix) : [f(x.a), t_ap(x.t)] == x;
// relies on mod(x) being internally transparent
mod(x) = [x.t.p, [mod(x.t.x), refl()]];

mod(x : Fix) : fix(x.a) == x;
t_mod{a}(t : T_Eq(a)) : t_refl(a) == t_ap(t);

mod(x) = [
  sym(ap(f, x.t.p)) @ x.t.p,
  _ : [fix(x.a), sym(ap(f, x.t.p)) @ x.t.p] == x.t
]
t_mod{a}(t) = [
  [
    _ : ,
    t.x.t : T_Eq(f(t.x.a))
    _ : t_ap(t.x.t) == t_refl(t.x.a)
    t_mod(t.x.t)
    _ : t_ap(t.x.t) == t_refl(t.x.a)
  ]
  _ : [f(t.x.a), t_ap(t.x.t)] == fix(t.x.a)
  [
    ap(f, t.p),
    _ : coe(
      ap(f, t.p),
      T_Eq,
      t_ap(t.x.t)
    ) == t_refl(a)
  ],
]

//
Fix : Type;
fix(a : A) : Fix;
K_Eq(a : A) : Type;
k_refl(a : A) : K_Eq(f(a));

Fix = [a : A, k : K_Eq(a)];
K_Eq(a) = (k : [
  x : Fix,
  p : f(x.a) == a,
  q : k_refl(a) == k
]);
t_refl(a : A) : T_Eq(f(a));

fix(a : A) : Fix


//
Fix : Type;
T_Eq(a : A) : Type;

Fix = [a : A, t : T_Eq(a)];
T_Eq(a) = [x : Fix, p : f(x.a) == a];

fix(a : A) : Fix
t_refl(a : A) : T_Eq(f(a));

fix(a) = [f(a), t_refl(a)];
t_refl(a) = [fix(a), refl()];

mod1(x : Fix) : [f(x.t.x.a), [x.t.x, refl()]] == x;
mod1(x) = [x.t.p, refl()];

mod2(l : Fix, r : Fix, H : l.t.x == r.t.x) : l == r;
mod2(l, r, H) =
  coe(mod1(l), λ(x) => x == r,
    coe(mod1(r), λ(x) => _ == x,
      ap((x) => [f(x.a), [x, refl()]], H)));

mod(l : T_Eq(a), r : T_Eq(a)) : ([a, l] : Fix) == [a, r];
mod(l, r) = [
  _ :
];

mod3(x : Fix) : fix(x.t.x.a) == x;
mod3(x) = [
  x.t.p,
  _ : [
    fix(x.t.x.a),
    x.t.p
  ] == x.t
]

mod3(x : Fix) : x.t.x == x;
mod3(x) = [
  sym(x.t.x.t.p) @ ap(f, x.t.x.t.p) @ x.t.p,
  // relying on transparent fixpoint
  [mod3(x.t.x), refl()]
];

mod4(x : Fix) : fix(x.t.x.a) == x;
mod4(x) = [
  x.t.p,
  [
    sym(ap(fix, ap(.a, mod3(x.t.x)))) @ mod4(x.t.x),
    _ : sym(ap(f, ap(.a, p))) == x.t.p
  ]

  refl()

  sym(ap(f, ap(.a, sym(ap(fix, ap(.a, mod3(x.t.x))))))) @ sym(ap(f, ap(.a, mod4(x.t.x))))

  sym(ap(f, ap(.a, sym(ap(fix, ap(.a, mod3(x.t.x))))) @ ap(.a, mod4(x.t.x))))

  _ : [
    p : fix(x.t.x.a) == x.t.x,
    q : coe(p, λ(l) => f(l.a) == x.t.x.a) == x.t.p
  ]
  [
    fix(x.t.x.a),
    x.t.p
  ] == [x.t.x, x.t.p],

  _ : [
    p : fix(x.t.x.a) == x.t.x,
    q : coe(p, λ(l) => f(l.a) == x.t.x.a) == x.t.p
  ]
  _ : [
    fix(x.t.x.a),
    x.t.p
  ] == [x.t.x, x.t.p],
]

mod4(x : Fix) : fix(x.a) == x;
mod4(x) = [
  sym(ap(f, x.t.p)) @ x.t.p,

  _ : [
    fix(x.a),
    sym(ap(f, x.t.p)) @ x.t.p
  ] == x.t
]
sym(ap(f, x.t.p)) @ x.t.p
x.t.p : f(x.t.x.a) == x.a

x.t.x
x.t.p : f(x.t.x.a) == x.a

x.t.x.t.p : f(x.t.x.t.x.a) == x.t.x.a
mod4(x.t.x) : fix(x.t.x.t.x.a) == x.t.x;



//
eta(w : W_Bool) = w.c(W_Bool, w_true, w_false);

Bool : Type;
true : Bool;
false : Bool;

K_Bool(w : W_Bool) : Bool;
k_true : K_Bool(w_true);
k_false : K_Bool(w_false);

Bool = [w : W_Bool, k : K_Bool(w)];
true = [w_true, k_true];
false = [w_false, k_false];

K_Bool(w) = [b : Bool, p : eta(b.w) == w];
k_true = [true, refl()];
k_false = [false, refl()];

b_wrap(w : W_Bool) : Bool;
k_wrap(w : W_Bool) : K_Bool(eta(w))

b_wrap(w) = [eta(w), k_wrap(w)];
k_wrap(w) = [b_wrap(w), eta_I(w)];

mod1(b : Bool) : [eta(b.k.b.w), [b.k.b, refl()]] == b;
mod1(b) = [b.k.p, refl()]

mod2(l, r : Bool, H : l.k.b == r.k.b) : l == r;
mod2(l, r, H) =
  coe(mod1(l), λ(x) => x == r,
    coe(mod1(r), λ(x) => _ == x,
      ap(λ(b) => [eta(b.w), [b, refl()]], H)));

mod3(b : Bool) : b_wrap(b.k.b.w) == [eta(b.k.b.w), [b.k.b, refl()]];
mod4{w}(k : K_Bool(w)) : k_wrap(k.b.w) == [k.b, refl()];

mod3(b) = [refl(), mod4(b.k)];
mod4{w}(k) = [
  _ : b_wrap(k.b.w) == k.b
]

mod2_true(k : K_Bool(w_true)) : true == [w_true, k];
mod2_true(k) = [
  refl(),
  sym(ap(eta, ap(.w, k.p))) @ eta_I(k.b.w) == refl()
];

mod2_true(k : K_Bool(w_true)) : true == [eta(k.b.k.b.w), [k.b.k.b, refl()]];
mod2_true(k) = [
  sym(k.p) @ sym(ap(eta, k.b.k.p)) @ eta_I(k.b.k.b.w),
  [
    mod2_true(k.b.k) @ mod1(k.b.k.b)
    p : true == k.b.k.b,
    q :
      coe(p, λ(l) => eta(l.w) == k.b.k.b.w) == refl()
  ]
  _ : [
    true,
    sym(k.p) @ sym(ap(eta, k.b.k.p)) @ eta_I(k.b.k.b.w)
  ] == [k.b.k.b, refl()]
]

sym(k.p) @ sym(ap(eta, k.b.k.p)) @ eta_I(k.b.k.b.w) : w_true == eta(eta(k.b.k.b.w))

 : eta(eta(k.b.k.b.w)) == eta(k.b.w)

k.b.k.p : eta(k.b.k.b.w) == k.b.w;
k.p : eta(k.b.w) == w_true;

sym(k.p) @ sym(ap(eta, k.b.k.p)) @ eta_I(k.b.k.b.w) @ k.b.k.p


mod2_true(k : K_Bool(w_true)) : k_true == k.b;
mod2_true(k) = [
  sym(k.p) @ sym(ap(eta, k.b.k.p)) @ eta_I(k.b.k.b.w) @ k.b.k.p,
  _ : coe(
    sym(k.p) @ sym(ap(eta, k.b.k.p)) @ eta_I(k.b.k.b.w) @ k.b.k.p,
    K_Bool,
    k_true
  ) == k.b
]


mod2_true(k : K_Bool(w_true)) : k_true == r.b;
mod2_true(l, r) = [
  l.p @ ap(eta, l.b.w.p) @ sym(eta_I(l.b.w.b.w)) @ sym(l.b.w.p) @
    sym(r.p) @ sym(ap(eta, r.b.w.p)) @ eta_I(r.b.w.b.w) @ r.b.w.p,
  mod1_true(_, _)
];

mod2_true(k : K_Bool(w_true)) : true == k.b;
mod2_true(k) = mod1_true(k_true, k);

mod3_true(k : K_Bool(w_true)) : k_true == k;
mod3_true(k) = [
  mod2_true(k) : k_true == k.b,
  _ : ap(eta, k.p) @ ap(eta, ap(eta, k.b.w.p)) @ ap(eta, sym(eta_I(k.b.w.b.w))) @ ap(eta, sym(k.b.w.p)) == k.p
];




mod1_true(k : K_Bool(w_true)) : [true, k.b.k] == k;
true == k.b
mod1_true(k) = [
  _
  _ :
]

mod_true(k : K_Bool(w_true)) : k_true == k;
mod_true(k) = [
  [
    sym(k.p) @ sym(ap(eta, k.b.w.p)) @ eta_I(_) @ k.b.w.p,
    _ : [
      w_true,
      sym(k.p) @ sym(ap(eta, k.b.w.p)) @ eta_I(_) @ k.b.w.p
    ] == k.b.k
  ]
  _ : [w_true, k_true] == [k.b.w, k.b.k]
]


mod_b_true(l : K_Bool(w_true)) : true == k.b;
mod_b_true(l) = [
  sym(k.p) @ sym(ap(eta, k.b.w.p)) @ k.b.w.p,
  _ : [
    w_true,
    sym(k.p) @ sym(ap(eta, k.b.w.p)) @ k.b.w.p
  ] == k.b.w
]


k.p : eta(k.b.w) == w_true
coe(k.p, λ(w) => eta(w) == w, eta_I(k.b.w))

mod_true(k : K_Bool(w_true)) : true == [w_true, k];
mod_true(k) = [
  sym(ap(eta, k.p)) @ eta_I(k.b.w) @ k.p,
  _ : [true, sym(ap(eta, k.p)) @ eta_I(k.b.w) @ k.p] == [k.b, k.p]
];


//
Fix : Type;
T_Eq(a : A) : Type;

Fix = [a : A, t : T_Eq(a)];
T_Eq(a) = [x : Fix, p : f(x.a) == a];


//
Fix : Type;
Fix = [x : Fix, a : A, p : f(x.a) == a];

mod1(x : Fix) : [x.x, f(x.x.a), refl()] == x =
  [refl(), x.p, refl()];

mod2(l : Fix, r : Fix, H : l.x == r.x) : l == r =
  coe(mod1(l), λ(x) => x.x == r.x,
    coe(mod1(r), λ(x) => _ == x.x,
      ap(λ(x) => [x, f(x.a), refl()], H)));

mod3(x : Fix) : x.x == x;
mod3(x) = [
  mod3(x.x),
  sym(x.x.p) @ ap(f, x.x.p) @ x.p,
  refl()
];


x.p == x.p

sym(ap(f, sym(x.x.x.p) @ ap(f, x.x.x.p) @ x.x.p)) @
  ap(f, x.x.p) @ x.p == x.p

mod3(x : Fix) : x.x == x;
mod3(x) =
  coe(mod1(x.x), λ(x) => x.x == r.x,
    coe(mod1(x), λ(x) => _ == x.x,
      [
        mod3(x.x),
        ap(f, x.x.p),
        _ :
          sym(ap(f, ap(.a, mod3(x.x)))) @ ap(f, x.x.p) == refl()
      ]
      _ : [x.x.x, f(x.x.x.a), refl()] ==
        [x.x, f(x.x.a), refl()]
      ));

mod3(l : Fix, r, H : l.a == r.a) : l == r;
mod3(l, r, H) =
  mod2(l, r, [
  ]
    _ : l.x == r.x)
  [
]
mod3(l : Fix, r, H : l.a == r.a) : l == r;
mod3(l, r, H) =
  [
  _ : l.x == r.x,
  mod3(l.x, r.x, mod2(l, r, H)),
  H,
  _ :
    coe(
      mod3(l.x, r.x, mod2(l, r, H)),
      λ(x) => f(x.a) == r.a,
      l.p @ H
    ) == r.p
]

ap(f, l.x.p) @
sym(ap(f, l.x.p)) @
sym(ap(f, l.p)) @
sym(ap(f, H)) @
ap(f, r.p) @
ap(f, r.x.p) @
sym(ap(f, r.x.p)) @
l.p @
H == r.p

sym(ap(f,
  sym(l.x.p) @ ap(f, l.x.p) @ l.p @ H @
  sym(r.p) @ sym(ap(f, r.x.p)) @ r.x.p
)) @ l.p @ H == r.p

sym(ap(f, mod2(l, r, H))) @ l.p @ H == r.p

sym(ap(f, mod2(l, r, H))) @ l.p @ H == r.p

coe(
  ap(f, ap(.a, mod3(l.x, r.x, mod2(l, r, H)))),
  λ(a) => a == r.a,
  l.p @ H
) == r.p

[
  [x.x.x, f(x.x.x.a), refl()],
  f(f(x.x.x.a)),
  refl()
]

coe(
  mod1(x),
  λ(x) => [x.x, f(x.x.a), refl()] == x,
  _ :
)

[[x.x.x, f(x.x.x.a), refl()], f(x.x.a), refl()]

x == [x.x, f(x.x.a), refl()]
x.x == [x.x.x, f(x.x.x.a), refl()]


mod2(x : Fix) : x.x == x;
mod2(x) = [
  [
    _ : x.x.x.a == x.
  ]
  _ : x.x.x == x.x,
]

x.x == x

x.x.x == x.x

[x.x, f(x.x.a), refl()]

mod{l : Fix, r}(H : l.x == r.x) : l == r =
  [refl(), x.p, refl()];


mod(l : Fix, r : Fix, H : l.a == r.a) : l == r = [
  _ : l.x == r.x,
  _,
  _
]
x.p : f(x.x.a) == x.a


//
Fix : Type;
I_Fix(x : Fix) : Type;

Fix = [x : Fix, i : I_Fix(x)];
I_Fix(x) = [a : A, p : f(x.i.a) == a];

fix(a : A) : Fix;
i_fix(a) : I_Fix(fix(a));

fix(a) = [fix(a), i_fix(a)];
// TODO: this is relying on transparent fix
i_fix(a) = [f(a), refl()];


mod1{x}(i : I_Fix(x)) : [f(x.x.a), refl()] == i = [i.p, refl()];

mod2(x : Fix) : x.x == x;
mod2(x) = [mod2(x.x), [sym(x.x.i.p) @ ap(f, x.x.i.p) @ x.i.p, refl()]];

mod3(x : Fix) : fix(x.i.a) == x;
mod3(x) = [
  mod3(x.x)
  _ : fix(x.i.a) == x.x
]
sym(ap(fix, sym(x.x.i.p) @ ap(f, x.x.i.p) @ x.i.p)) : fix(x.i.a) == fix(x.x.i.a)

mod3(x : Fix) : fix(x.i.a) == x;
mod3(x) = [
  sym(ap(fix, sym(x.x.i.p))) @
  sym(ap(fix, ap(f, x.x.i.p))) @
  sym(ap(fix, x.i.p)) @ mod3(x.x),
  [
    sym(x.x.i.p) @ ap(f, x.x.i.p) @ x.i.p,

  ]
  _ : coe(p, I_Fix, i_fix(x.i.a)) == x.i
]

sym(ap(f, ap(.i.a, ap(fix, x.x.i.p)))) @
ap(f, ap(.i.a, ap(fix, ap(f, x.x.i.p)))) @
ap(f, ap(.i.a, ap(fix, x.i.p))) @
sym(x.x.i.p) @ x.i.p == x.i.p



sym(x.x.i.p) @ ap(f, x.x.i.p) @ x.i.p : f(f(x.x.x.i.a)) == f(x.x.i.a)

x.x.i.p : f(x.x.x.i.a) == x.x.i.a
x.i.p : f(x.x.i.a) == x.i.a
[
  p : f(x.i.a) == x.i.a,
  q : sym(ap(f, ap(.i.a, r))) @ p == x.i.p
]


sym(ap(f, ap(.i.a, ap(fix, x.x.i.p)))) @
ap(f, ap(.i.a, ap(fix, ap(f, x.x.i.p)))) @
ap(f, ap(.i.a, ap(fix, x.i.p))) @
sym(ap(f, ap(.i.a, mod3(x.x))))

sym(ap(f, ap(.i.a, p)))

[
  f(x.i.a),
  sym(ap(f, ap(.i.a, p)))
]
coe(p, I_Fix, i_fix(x.i.a))
I_Fix(fix(x.i.a))
x.i : I_Fix(x.x)
x.i.a

mod3(l, r)



x.i.p : f(x.x.i.a) == x.i.a
x.x.i.a == x.i.a

x.i : I_Fix(x)

sym(ap(f, ap(.a, ap(.i, mod3(x.x))))) @ x.x.i.p

mod1(x : Fix) : [x.x, [f(x.x.a), refl()]] == x =
  [refl(), [x.p, refl()]];

mod2(l : Fix, r : Fix, H : l.x == r.x) : l == r =
  coe(mod1(l), λ(x) => x.x == r.x,
    coe(mod1(r), λ(x) => _ == x.x,
      ap(λ(x) => [x, f(x.a), refl()], H)));

mod3(x : Fix) : x.x == x;
mod3(x) = [
  mod3(x.x),
  sym(x.x.p) @ ap(f, x.x.p) @ x.p,
  refl()
];

//
Fix : Type;
I_Fix(x : Fix) : Type;

Fix = [x : Fix, i : I_Fix(x)];
I_Fix(x) = [a : A, p : f(x.i.a) == a];


fix(a : A) : Fix;
i_fix(a) : I_Fix(fix(a));

fix(a) = [fix(a), i_fix(a)];
// TODO: this is relying on transparent fix
i_fix(a) = [f(a), refl()];


mod1{x}(i : I_Fix(x)) : [f(x.x.a), refl()] == i = [i.p, refl()];

mod2(x : Fix) : x.x == x;
mod2(x) = [mod2(x.x), [sym(x.x.i.p) @ ap(f, x.x.i.p) @ x.i.p, refl()]];



Fix : Type;
fix(a : A) : Fix;
mod(a) : fix(f(a)) == fix(a);

Fix = [a : A, p : fix(a) == a];
fix(a) = [f(a), mod(a)];
mod(a) = λ(C, r) => ((r ⇒ C(fix(f(a)))) ⇐ C(fix(a)))

(r ⇒ C(fix(f(a)))) ⇐ C(fix(a))

fix(f(a)) == fix(a)
mod(f(a)) == mod(a)

//
Fix : Type;
fix(x : Fix) : Fix;
mod(x) : eta(x) == x;

Fix = (x : [a : A, p : eta(x) == x]);
eta(x) = [f(x.@.a), ap(eta, mod(x))];
mod(x) = [
  _ : f(x.@.a) == x.@.a,
  _ :
]

ap(.@.a, x.@.p) : f(x.@.a) == x.@.a

eta_I(x) = λ(C, r) => (r ⇒ C(eta(eta(x))) ⇐ C(eta(x)));

eta(eta(x)) == eta(x)

eta_I(eta(x)) == eta(x)

eta_I(eta(x)) == eta_I(x)

[f(x.@.a), eta_I(x)]
eta_I((a = [f(x.@.a), eta_I(x)]))

eta_I(eta(x))

eta_I(x).@ :
  [
    f(f(x.@.a)),
    sym(ap(eta, eta_I(x))) @ eta_I(eta(x)) @ eta_I(x),
  ] == [f(x.@.a), eta_I(x)];


coe(
  eta_I(x),
  λ(x) => eta(x) == x,
  eta_I(eta(x)),
)


//
Fix : Type;
fix(a : A, p : f(a) == a) : Fix;

Fix = (x : [a : A, p : f(a) == a, k : fix(a, p) == x]);
fix(a, p) = [
  a,
  sym(ap(f, p)) @ p,
  coe(
    J(p),
    λ(l, q : f(a) == l) => fix(l, sym(ap(f, q)) @ q) == fix(a, p),
    _ : fix(f(a), refl()) == fix(a, p)
  )
  _ : fix(f(a), refl()) == fix(a, p)
];

sym(ap(f, q)) : f(l) == f(a)

sym(ap(f, q)) @ q


f(l) == f(a)

sym(ap(f, q)) : f(a) == f(l)

sym(ap(f, p)) : f(a) == f(f(a))
sym(ap(f, p)) @ p


K_Eq(a : A) : Type;
k_wrap(a, p : f(a) == a) : K_Eq(a);
k_mod(a, p) : k_wrap(a, sym(ap(f, p)) @ p) == k_wrap(a, p);

K_Eq(a) = (k : [
  p : f(a) == a,
  q : k_wrap(a, p) == k
]);
k_wrap(a, p) = [sym(ap(f, p)) @ p, k_mod(a, p)];
k_mod(a, p) =
  λ(i) => [
    sym(ap(f, p)) @ p,
    _ : k_wrap(a, sym(ap(f, p)) @ p) == k_mod(a, p)
  ];


Fix : Type;
fix(a : A) : Fix;
mod(a) : fix(f(a)) == fix(a);

Fix = (x : [a : A, p : fix(a) == x]);
fix(a) = [f(a), mod(a)];
mod(a) = λ(i) => [f(a), λ(j) => mod(a)(min(i, j))];

//
Fix : Type;
fix(a : A, p : f(a) == a) : Fix;
mod(a, p) : fix(f(a), refl()) == fix(a, p);

Fix = (x : [a : A, p : f(a) == a, k : fix(a, p) == x]);
fix(a, p) = [
  a,
  sym(ap(f, p)) @ p,
  _ : fix(a, sym(ap(f, p)) @ p) == fix(a, p)
];
mod(a, p) = _

fix(f(a), refl()) == fix(f(a), ap(f, p))

fix(a, p) == fix(f(a), ap(f, p))

[f(f(a)), refl(), mod(f(a), refl()) @ mod(f(a), ap(f, p))] == [f(a), refl(), mod(f(a), ap(f, p))]

coe(
  J(p),
  λ(l, q : f(a) == l) =>
    [f(l), refl(), mod(f(l), refl()) @ mod(a, p)] == [l, refl(), mod(a, p)]
)
mod(a, p) : fix(f(a), refl()) == fix(a, p)
mod(a, p).@ : [f(f(a)), refl(), mod(f(a), refl()) @ mod(a, p)] == [f(a), refl(), mod(a, p)]


//
Fix : Type;
fix(a : A) : Fix;
mod(a) : fix(f(a)) == fix(a);

Fix = (x : [a : A, p : fix(a) == x]);
fix(a) = [f(a), mod(a)];
mod(a) = refl();

fix(f(a)) ≡ fix(a)
mod(f(a)) ≡ mod(a)
(refl() ⇒ _ ⇐ _){a := f(a)} ≡ (refl() ⇒ _ ⇐ _)

(refl() ⇒ fix(f(f(a))) == fix(f(f(a))) ⇐ fix(f(f(a))) == fix(f(a))) ==
  (refl() ⇒ fix(f(a)) == fix(f(a)) ⇐ fix(f(a)) == fix(a))

(l == l) ≡ (l == r)
l ≡ r



(refl() ⇒ l == l ⇐ l == l){a := f(a)} == (refl() ⇒ l == l ⇐ l == l)

(refl() ⇒ fix(f(a)) == fix(f(a)) ⇐ fix(f(a)) == fix(a))

fix(f(f(a))) == fix(f(a))


id(
  A : Data,
  len(a : A # len(a)) : Length;
  a : A # len(a)
) : (r : A # len(a))

//

w_mod(a : A, p : f(a) == a) : [f(a), ap(f, p)] == [a, p] =
  λ(i) => [
    p(i),
    coe(
      λ(j) => f(p(min(i, j))) == p(min(i, j)),
      refl()
    )
  ];


[f(a), p]

Fix : Type;
fix(a : A, p : f(a) == a) : Fix;
mod1(a, p) : fix(f(a), refl()) == fix(a, p);
mod2(a, p) : fix(f(a), refl()) == fix(f(a), ap(f, p));

Fix = (x : [a : A, p : fix(f(a), refl()) == x]);
fix(a, p) = [a, mod1(a, p)];
mod1(a, p) = λ(i) => [
  p(i),
  λ(j) => mod1(p(i), _ : f(p(i)) == p(i))
  _ :
  _ : fix(f(p(i)), refl()) == mod1(a, p)(i)
];

K_Eq(a : A) : Type;
k_wrap(a, p : f(a) == a) : K_Eq(a);
k_mod(a, p) : k_wrap(a, sym(ap(f, p)) @ p) == k_wrap(a, p);

K_Eq(a) = (k : [
  p : f(a) == a,
  q : k_wrap(a, p) == k,
]);
k_wrap(a, p) = [sym(ap(f, p)) @ p, k_mod(a, p)];
k_mod(a, p) = [

];

k_mod(a, p).@ :
  [
    sym(ap(f, p)) @ p,
    k_mod(a, sym(ap(f, p)) @ p) @ k_mod(a, p)
  ] == [
    sym(ap(f, p)) @ p,
    k_mod(a, p)
  ]

[
  p : sym(ap(f, p)) @ p == sym(ap(f, p)) @ p,
  q : sym(ap(k_wrap(a), p)) @ k_mod(a, sym(ap(f, p)) @ p) @ k_mod(a, p) == k_mod(a, p)
]

[
  p : sym(ap(f, p)) @ p == sym(ap(f, p)) @ p,
  q : sym(ap(k_wrap(a), p)) @ k_mod(a, sym(ap(f, p)) @ p) @ k_mod(a, p) == k_mod(a, p)
]

sym(ap(k_wrap(a), p)) @ k_mod(a, sym(ap(f, p)) @ p)

k_mod(a, p).@ : coe(k_mod(a, p), _, k_wrap(a, sym(ap(f, p)) @ p).@) == k_wrap(a, p).@

//
A : Type;
f(a : A) : A;
f_I(a) : f(f(a)) == f(a);
f_J(a) : (λ(i) => f(f_I(a)(i))) == f_I(f(a));

K_Eq(a : A) : Type;
k_refl(a) : K_Eq(f(a));
k_mod(a) : Path(λ(i) => K_Eq(f_I(a)(i)), k_refl(f(a)), k_refl(a));

K_Eq(a) = (k : [f(a), k_refl(a)] == [a, k : K_Eq(a)]);
k_refl(a) = λ(i) => [f_I(a)(i), k_mod(a)(i)];


f_I(f(a))(i)

λ(i) => [f_I(f(a))(i), k_mod(f(a))(i)]
λ(i) => [f_I(a)(i), k_mod(a)(i)]

[f(f_I(a)(i)), k_refl(f_I(a)(i))] ==
[f_I(a)(i), k_mod(a)(i) : K_Eq(f_I(a)(i))]


Γ, C : A -> Type |- K : C(x) -> C(y)
------------------------------------
Γ |- λ(C) => K ⇐ (x : A) == y

Γ |- M ⇒ (x : A) == y  Γ |- N ⇐ A -> Type
-----------------------------------------
Γ |- M(N) ⇒ C(x) -> C(y)

Bool = (b(
  C : Type,
  t : (λ(C, t, f) => t(λ(C) => λ(r) => r)) == b -> C,
  f : (λ(C, t, f) => f(λ(C) => λ(r) => r)) == b -> C) : C);

true : Bool = λ(C, t, f) => t(refl());
false : Bool = λ(C, t, f) => f(refl());


Γ, C : A -> Type |- K : C(x) -> C(y)
------------------------------------
Γ |- λ(C) => K ⇐ (x : A) == y

Γ |- x : A  Γ |- y : A
----------------------
Γ |- x == y : Type

Γ |- x : A  Γ |- y : A  Γ |- x == y : Type
------------------------------------------------
Γ |- x == y |-> Π(C : A -> Type) -> C(x) -> C(y)

Bool = (b : [
  c : C_Bool,
  p : c_next(c) == c,
  q : ([

  ] : ) == _
  p : [
    c_next(c),
    ap(f)
  ] ==
    [b.c, b.p]
]);





-----------------------------------------
Γ |- M : (x : A) == y

pack = [a : A]
Bool = (b(
  C : #typeof(b) -> Type,
  t : C(λ(C, t, f) => t),
  f : C(λ(C, t, f) => f)) : C(b));

true : Bool = λ(C, t, f) => t(refl());
false : Bool = λ(C, t, f) => f(refl());

T = (x : #typeof(x) -> False);
x : T = λ(x) => x(x);


//
Fix = (x : [
  a : A,
  p : f(a) == a,
  q : ([

  ] : (r : [

  ])) == [a, p, q]
]);


//
S : (P(
  A : Type,
  x : A,
  f(u : P(A, x, f)) : A
) -> Type);
P : S = λ(A, x, f) =>
  (u : Π(C(a : A) -> Type) -> C(x) -> C(f(u)));

Unit = Π(
  A : Type,
  x : A,
  f(u : P(A, x, f)) : A,
  f_x : x == f(λ(C, r) => coe(f_x, C, r))
) -> P(A, x, f);
unit : Unit = λ(A, x, f, f_x) => λ(C, r) => coe(f_x, C, r);

u : Unit;
u(
  Unit,
  unit,
  f(u : P(Unit, unit, _)) =
    λ(A, x, f, f_x) =>
  u(C(_) = Unit, unit),
  f_x : unit == f(λ(C, r) => coe(f_x, C, r)) = refl();
) : Π(C(a : Unit) -> Type) -> C(unit) ->
  C(u(
      Unit,
      unit,
      f(u : P(Unit, unit, _)) = u(C(_) = Unit, unit),
      f_x : unit == f(λ(C, r) => coe(f_x, C, r)) = refl();
    )(C(_) = Unit, unit))

//
Unit : Type;
unit : Unit;
ind(u : Unit) : (unit : Unit) == u;

//
Unit : Type;
unit : Unit;

X : Type;
f(u : Unit) : X;
g(x : X) : Unit;
g_f(u) : g(f(u)) == u;

ind(u : Unit) : f(unit) == f(u);

//
Unit = (u : Π(
  C(A : Type, a : A, rec(a : A) : C_Unit) : Type,
  r : C(C_Unit, c_unit, id),

) : C(_, u, ));

//
Unit = (u : Π(
  Unit : Type,
  unit : Unit,
  u : Unit,

) : C(_, u, ));





// equality is just sugar to leibniz equality here
M_Unit(f(u : f(refl()) == f(u)) : M_Unit(f)) : Type;
M_Unit(f) = (u : (f(refl()) : M_Unit(f)) == f(u))


Unit : Type = M_Unit(id);
unit : Unit = refl();

S_Unit : Type =
S_Unit = (M_Unit(
  f(M_Unit : S_Unit) : Type,
  g(u : g(refl()) == g(u)) : f(M_Unit)
) : Type);

S_Unit
Unit(unit : Unit(unit)) = (u)



S_Unit : Type =
  (M_Unit(
    P : [
      A : Type,
      x : A,
      f(u : M_Unit(P)) : A,
      g(c : A) : A,
    ]
  ) : Type);



M_Unit(
  [
    f(u : M_Unit(P)) : C_Unit =
      u(C(_) = W_Unit, w_unit),
    g(c : C_Unit) : M_Unit(P) = _,
    g_f(u) : g(f(u)) == u = _,
  ]
)


// mutual recursion
a : A;
b : B{a};

a = M{a, b};
// N knows that a == M{a, b}
b = N{a, b};

// to
m_a(b : B{m_a(b)}) : A
m_a(b) = M{m_a(b), b};

b : B{m_a(b)};
b = N{m_a(b), b};

a : A = m_a(b);


//
S = [Unit : Type, unit : Unit];
D(P : S) : Type = [
  Unit(u : P.Unit) : Type;
  unit : Unit(P.unit);
]
Hom_S(A : S, B : S) : Type = [
  map(a : A.Unit) : B.Unit;
  unit : map(A.unit) == B.unit;
];

F(P : S) : D(P) = [
  Unit(u) = P.unit == u;
  unit = refl();
];

T = (P :
  D([
    Unit = (u : P.Unit(u)),
    unit = P.unit,
  ])
);

U : T = [
  Unit(u : (u : U.Unit(u))) = _,
  unit = _,
];


G(F(P : S) : D(P)) : S

fmap(

) : Hom(F(A), F(B))
Fix(F(P : S) : D(P)) : S;





P : S;
P = [
  ((u : P.Unit) : P.unit == u),
  (refl() : P.unit == P.unit)
]

//
S = [
  Unit : Type,
  unit : Unit,
  case(u : Unit, C : Type, r : C) : C,
  case_unit : case(unit) == (λ(C, r) => r)a
];

//
S = [Unit : Type, unit : Unit];
D_S(P : S) : Type = [
  Unit(u : P.Unit) : Type;
  unit : Unit(P.unit);
]
D(P : S) : D_S(P) = [
  Unit(u) = P.unit == u;
  unit = refl();
];
F(P : S) : S = [
  Unit = [u : P.Unit, r : D(P).Unit(u)];
  unit = [P.unit, D(P).unit]
];
Equiv_S(A : S, B : S) : Type = [
  obj : Equiv(A.Unit, B.Unit);
  unit : Equiv(obj.f(A.unit), B.unit);
];
f_equiv(A : S, B : S) : Equiv_S
Hom_S(A : S, B : S) : Type = [
  map(a : A.Unit) : B.Unit;
  unit : map(A.unit) == B.unit;
];



[c : C_Unit, r : c_unit == c] == C_Unit
T : Type = (X : [
  P : S,
  p : F(X.P) == P
]);
X : T = [
  [C_Unit, c_unit],
  _ : F([C_Unit, c_unit]) == [C_Unit, c_unit]
]
X : T = [
  [C_Unit, c_unit],
  f(P) = [
    Unit(u) = P.unit == u;
    unit = refl();
  ];
];

//
S = [Unit(u : Unit(u)) : Type, unit : Unit(unit)];
D_S(P : S) : Type = [
  Unit(u : P.Unit(u)) : Type;
  unit : Unit(P.unit);
];
D(P : S) : D_S(P) = [
  Unit(u : P.Unit(u)) =
    (P.unit : (u : P.Unit(u))) == u;
  unit = refl();
];

T : Type = (X : D_S([
  Unit(u : Unit(u)) =
    refl() == u;
  unit : X.unit == unit = refl();
]));
X : T;
X.unit : X.Unit(refl());

//
S = [Unit : Type, unit : Unit];
D_S(P : S) : Type = [
  Unit(u : P.Unit) : Type;
  unit : Unit(P.unit);
]
D(P : S) : D_S(P) = [
  Unit(u) = P.unit == u;
  unit = refl();
];
G(P : S, D : D_S(P)) : S = [
  Unit = [u : P.Unit, d : D.Unit(u)];
  unit = [P.unit, D.unit];
];
fst(P : S) : S = [
  Unit = [l : G(P, D(P)), _]
];
F(P : S) : S = G(P, D(P));


T : Type = (X : [
  P : S;
  p : ([D_S, D(P)] : [
    F(P : S) : Type;
    X : F(P)
  ]) == [λ(_) => S, P];
]);

X : T = [
  [C_Unit, c_unit],
  _ : [D_S, D([C_Unit, c_unit])] == [λ(_) => S, [C_Unit, c_unit]]
]
D_S(P) == S


//
S(A : Type) : Type = [
  X : Type;
  x : X;
  f(a : A) : X
]
sect(A : Type, P : S(A)) : Type =
  [a : A, p : P.f(a) == P.x];
retr(A : Type, P : S(A)) : Type =
  [a : A, x : P.x];
retr_P(A : Type, P : S(A)) : S(retr(A, P)) = [
  P.X,
  P.x,
  f(a) : X = a.x
];

//
S = [
  Unit : Type,
  unit : Unit,

];
D_S(P : S) : Type = [
  Unit(u : P.Unit) : Type;
  unit : Unit(P.unit);
]
D(P : S) : D_S(P) = [
  Unit(u) = P.unit == u;
  unit = refl();
];
G(P : S, D : D_S(P)) : S = [
  Unit = [u : P.Unit, d : D.Unit(u)];
  unit = [P.unit, D.unit];
];

sect(P : S) : S = G(P, D(P));
retr(P : S) : S = G(P, D(P));

[sect(retr(P)), ap(sect ∘ retr, p)]
[sect(retr(P)), _ : sect(retr(sect(retr(P)))) == sect(retr(P))]



Fix : Type;
fix(a : A) : Fix;
mod(a) : fix(f(a)) == fix(a);

Fix = (x : [a : A; p : fix(a) == x]);
fix(a) = [f(a), mod(a)];
mod(a) = λ(i) => [
  f_I(a)(i),
  λ(j) =>
    _ :
  _ : fix(f_I(a)(i)) == mod(a)
]

Eq(A : Type, x : A, y : A) : Type;
refl{A}(x : A) : Eq(A, x, x);

Eq(A, x, y) =
  Π(C(z : A, p : Eq(A, x, )))


out(a : A) : [a : A, b : B(a)];
mod(a) : fst(out(a)) == a;

Bool : Type;
I_Bool(b : Bool) : Type;
true : Bool;
false : Bool;

Bool = [b : Bool, i : I_Bool(b)];
I_Bool(b) = Π(C, t : C(true), f : C(false)) -> C(b);
true = [true, λ(C, t : C(true), f : C(false)) => t];
false = [false, λ(C, t : C(true), f : C(false)) => f];

out(b : Bool) : [b : Bool, i : I_Bool(b)];
// if you could somehow get
mod(b : Bool) : fst(out(b)) == b;
// then you could derive
ind(b : Bool) : I_Bool(b);


A : Type;
f : A -> A;
out(a : A) : B(f(a));

X : Type;
sect : X -> A;
retr : A -> X;
sect_retr : sect ∘ retr == f;
retr_sect : retr ∘ sect == id;

// then
x : X
out(sect(x)) : B(f(sect(x)))
// which is by sect_retr
out(sect(x)) : B(sect(retr(sect(x))))
// and by retr_sect
out(sect(x)) : B(sect(x))
// and given some properties of B
C : X -> Type;
c_sect(x) : B(sect(x)) == C(x)
// such that
out(sect(x)) : C(x)

elim == out ∘ sect


//
A : Type;
B : A -> Type;
f : A -> A;
out(a : A) : B(f(a));

X : Type;
Y : X -> Type;
sect : X -> A;
retr : A -> X;
sect_retr : sect ∘ retr == f;
retr_sect : retr ∘ sect == id;
map(a : A, b : B(a)) : Y(retr(a))

// then
x : X
out(sect(x)) : B(f(sect(x)))
// which is by sect_retr
out(sect(x)) : B(sect(retr(sect(x))))
// and by retr_sect
out(sect(x)) : B(sect(x))
// then you can use map
map(sect(x), out(sect(x))) : Y(retr(sect(x)))
// which by retr_sect again
map(sect(x), out(sect(x))) : Y(x)


// and given some properties of B
C : X -> Type;
c_sect(x) : B(sect(x)) == C(x)
// such that
out(sect(x)) : C(x)

elim == out ∘ sect

f(a : A, b : B(a)) : C(retr(a))

L_Bool(c) : Π(C : C_Bool -> Type) -> C(c_true) -> C(c_false) -> C(c);
R_Bool(b : X) : Π(C : X -> Type) -> C(x_true) -> C(x_false) -> C(x)

out(sect(x)) : L_Bool(sect(x))
m : R_Bool(retr(sect(x)))

out(sect(x)) : Π(C : C_Bool -> Type) -> C(c_true) -> C(c_false) -> C(sect(x))
λ(C, t, f) =>

  out(sect(x))(λ(c) => C(retr(c)), t, f) : C(retr(sect(x)))

//
C_Bool : Type = Π(C : Type) -> C -> C -> C;
c_true : C_Bool = λ(C, t, f) => t;
c_false : C_Bool = λ(C, t, f) => f;

I_Bool(c) : Type =
  Π(C : C_Bool -> Type) -> C(c_true) -> C(c_false) -> C(c);
i_true : I_Bool(c_true) = λ(C, t, f) => t;
i_false : I_Bool(c_false) = λ(C, t, f) => f;

W_Bool : Type = [c : C_Bool, i : I_Bool(c)];
w_true : W_Bool = [c_true, i_true];
w_false : W_Bool = [c_false, i_false];

// the idempotent
in_(c : C_Bool) : W_Bool = c(W_Bool, w_true, w_false);
out(w : W_Bool) : C_Bool = w.c;

c_next(c : C_Bool) : C_Bool = out(in_(c));
//
C : Type = [Bool : Type, true : Bool, false : Bool];
Hom_C(A : C, B : C) = [
  map(a : A.Bool) : B.Bool;
  map_true : map(A.true) == B.true;
  map_false : map(A.false) == B.false;
];

D_C(P : C) : Type = [
  Bool(b : P.Bool) : Type;
  true : Bool(P.true);
  false : Bool(P.false);
];
D(P : C) : D_C(P) = [
  Bool(b) = Π(C : P.Bool -> Type, t : C(P.true), f : C(P.false)) -> C(b);
  true = λ(C, t, f) => t;
  false = λ(C, t, f) => f;
];

W(P : C) : C = [
  Bool = [b : P.Bool, i : D(P).Bool(b)];
  true = [P.true, D(P).true];
  false = [P.false, D(P).false];
];

P0 : C = [C_Bool, c_true, c_false];
P1 : C = W(P0);

S : Hom_C(P0, P1) = [
  map(b) = b(P1.Bool, P1.true, P1.false);
  map_true = refl();
  map_false = refl();
];
R : Hom_C(P1, P0) = [
  map(b) = b.b;
  map_true = refl();
  map_false = refl();
];
F : Hom_C(P0, P0) = [
  map(b) = R.map(S.map(b));
  map_true = refl();
  map_false = refl();
];

Section(D : D_C(C))

S(P : C) : D_C(P) = [
  Bool(b : P.Bool) = D(P).Bool(F.map(b));
  true : Bool(P.true) = _;
  false : D(P0).Bool(F.map(P0.false));
];
S0 : (b : P0.Bool) -> D0.Bool(b);

P_S : [
  obj(b : P0.Bool) : D0.Bool(b);
  map :
] = [
  _
];

PX : C;
DX : PX;

sect : PX -> P0;
retr : P0 -> PX;

sect_retr : sect ∘ retr == F;
retr_sect : retr ∘ sect == id;


map
S0(sect(b)) : D0.Bool(sect(b))

//


A : C;
B : D_C(A) = D(A);
F : Hom_C(A, A);
out(a : A.Obj) : B.Obj(F.map(a));

X : C;
Y : D_C(X) = D(X);
sect : Hom_C(X, A);
retr : Hom_C(A, X);
sect_retr : sect ∘ retr == F;
retr_sect : retr ∘ sect == id;

ind(x : X.Obj) : Y.Obj(x);


x : X.Obj;
out(sect.map(x)) : B.Obj(F.map(sect.map(x)));
//
out(sect.map(x)) : B.Obj(sect.map(retr.map(sect.map(x))));
//
out(sect.map(x)) : B.Obj(sect.map(x));
//
out(sect.map(x)) : D(X).Obj(x)

map(a : A, b : B(a)) : Y(retr(a))

// then
x : X
out(sect(x)) : B(f(sect(x)))
// which is by sect_retr
out(sect(x)) : B(sect(retr(sect(x))))
// and by retr_sect
out(sect(x)) : B(sect(x))
// then you can use map
map(sect(x), out(sect(x))) : Y(retr(sect(x)))
// which by retr_sect again
map(sect(x), out(sect(x))) : Y(x)


//
S : Type;
D : S -> Type;

A : S;
B : D_S(A) = D(A);
F : Hom_C(A, A);
out(a : A.Obj) : B.Obj(F.map(a));

X : C;
Y : D_C(X) = D(X);
sect : Hom_C(X, A);
retr : Hom_C(A, X);
sect_retr : sect ∘ retr == F;
retr_sect : retr ∘ sect == id;

map(a : A.Obj) : B.Obj(a) -> Y.Obj(retr.map(a))

out(sect.map(x)) : B.Obj(F.map(sect.map(x)));
//
out(sect.map(x)) : B.Obj(sect.map(retr.map(sect.map(x))));
//
out(sect.map(x)) : B.Obj(sect.map(x));

//

S : Type = [Bool : Type, true : Bool, false : Bool];
D_S(C : S) : Type = [
  Bool(b : C.Bool) : Type;
  true : Bool(C.true);
  false : Bool(C.false);
];
D(C : S) : D_S = [
  Bool(b) = Π(K : C.Bool -> Type) ->
    K(C.true) -> K(C.false) -> K(b);
  true = λ(K, t, f) => t;
  false = λ(K, t, f) => f;
];

C : S;
C = [
  Bool = (b : D(C).Bool(b)),
  true = D(C).true;
  false = D(C).false;
];






A : S = [C_Bool, c_true, c_false];
B : D_S(A) = D(A);
F : Hom_C(A, A);
out(a : A.Obj) : B.Obj(F.map(a));

X : C = [
  Bool = (b : D(X).Bool(b)),
  true = D(X).true;
  false = D(X).false;
];
Y : D_C(X) = D(X);


sect : Hom_C(X, A) = [

];
retr : Hom_C(A, X) = [
  map(a : A.Bool) =
    [
      a,
      _
    ],
  _,
  _
];
sect_retr : sect ∘ retr == F;
retr_sect : retr ∘ sect == id;


S : Type;
D_S : S -> Type;
D(C : S) : D_S(C);

Fix(D) : S
sect(D) : [
  obj(b : Fix(D).Bool) : D(Fix(D)).Bool(b)
  _
];

Fix(D).Bool |-> [b : Fix(D).Bool, i : D(Fix(D)).Bool(b)]
mod(D).Bool(b : Fix(D).Bool) : fst(b) == b

sect(D) : D(Fix(D))


A : Type;
f(a : A) : A;
f_const(l, r) : Equiv_A(f(l), f(r));

hed(l, p : l == l) : refl() == ap(f, p) =
  coe(
    J(p),
    λ(a, p : l == a) =>
      refl() == ap(f, q) : f(l) == f(a),
    λ(q : l == l) =>
      coe(
        J(q),
        λ(a, p : l == a) =>
          _ : refl()
      )
      _ : refl() == ap(f, q)
  )(q)
T = [a : A, p : f(a) == a];
is_singleton(p : T) : [f(x), f_x] == p


//
Equiv(A : Type, B : Type) : Type = [
  f : A -> B;
  f_equiv(b : B) : [
    c : [a : A, p : f(a) == b],
    is_center(x : [a : A, p : f(a) == b]) : c == x
  ]
];

IsContr(B(a))
[a : A, b : B(a)]

i : Idem(A);
g(a : A) : A = i.f_equiv(l).c.a;

i.f_equiv(l).c : [a : A, p : i.f(a) == l]

i.f_equiv(l).c.p : i.f(g(l)) == l


fib{A, B}(f : A -> B) : B -> Type = λ(b) => [a : A, p : f(a) == b];

P : B -> Type = fib(f);
E : Type = [b : B, p : P(b)];
π1 : E -> B = λ(e) => e.b;

fib(π1) == fib(f)
[e : E, p : π1(e) == b] == [a : A, p : f(a) == b]
[e : [b : B, p : fib(b)], p : e.b == b] == [a : A, p : f(a) == b]

Fix = [a : A, p : f(a) == a];
L(b : B) = [a : A, p : f(a) == b];
R(c : C) = [b : B, q : g(b) == c];

L ∘ R

L(b : B) = [a : A, p : f(a) == b];
R(c : C) = [b : B, q : g(b) == c];

L = fib(f);
R = fib(g);

fib(f ∘ g)(x)

[a : A, p : f(g(a)) == x]


L(b : B) = [a : A, p : f(a) == b];
R(c : C) = [b : B, q : g(b) == c];


L(b : B) = [a : A, p : f(a) == b];
R(c : C) = [b : B, q : g(b) == c];

L(a : A) = [l : A, p : l == a];
R(a : A) = [r : A, p : f(r) == a];
X = [a : A, p : f(a) == a];
Y = (x : [
  a : A,
  p : f(x.a) == a,
]);
in_(x : X) : Y = [x.a, x.a, x.p];
out(y : Y) : X = [y.r, sym(ap(f, y.p)) @ y.p];

out(in_(x)) |-> [x.a, sym(ap(f, x.p)) @ x.p]

out(in_(x)) == x
out(in_(x)) == out([x.a, [f(x.a), refl()]])

out([x.a, [f(x.a), refl()]]) |-> [f(x.a), refl()]



f(y.l) == y.r

sym(ap(f, y.p)) @ y.p : f(y.r) == y.r

y.p : f(y.l) == y.r

f(f(y.l)) == f(y.l)



r.p @ sym(l.p) : f(r.r) == l.l
r.p : f(r.r) == a



[a : A, p : f(a) == g(a)]


//
S : Type = [Unit : Type, unit : Unit];
C : S = [
  Unit = [u : C.Unit, p : C.unit == u];
  unit = [C.unit, refl()]
];


//
T = [
  l : A,
  q : [
    r : A,
    p : f(l) == r
  ];
  w : q.r == l;
];

t : T;
t.p : f(t.l) == t.r
t.p : f(t.l) == t.r


//
X = [a : A, p : f(a) == a];
Y = (y : [
  a : A,
  p : f(y.a) == a,
]);

L(a : A) = [x : A, p : f(a) == x];
R = [a : A, l : L(a), q : l.x == a]

T = [a : A, b : B(a), p : π(b) == a];


Bool : Type;
I_Bool(b : Bool) : Type;
true : Bool;
false : Bool;

Bool = [b : Bool, i : I_Bool(b)];
I_Bool(b : Bool) = Π(C : Bool -> Type) ->
  C(true) -> C(false) -> C(b);
true = [true, λ(C, t, f) => t];
false = [false, λ(C, t, f) => f];

mod(b : Bool) : b.b == b;
mod_true : refl() == mod(true);
mod_false : refl() == mod(false);

mod(b) = λ(i) => [
  mod(b)(i),
  λ(C, t, f) => b.i(
    λ(b) => C(mod(b)(i)),
    coe(λ(j) => C(mod_true(i)(j)), t),
    coe(λ(j) => C(mod_false(i)(j)), f),
  )
];
mod_true = λ(i) => λ(j) => true;
mod_false = λ(i) => λ(j) => false;



//
K_Eq(a : A) : Type;
k_refl(a) : K_Eq(f(a));

K_Eq(a) = (k : ([f(a), k_refl(a)] : [a : A, k : K_Eq(a)]) == [a, k]);
k_refl(a) = λ(i) => [f(a), k_refl(a)];


K_Eq(a : A) : Type;
k_refl(a) : K_Eq(f(a));

K_Eq(a) = (k : ([f(a), k_refl(a)] : [a : A, k : K_Eq(a)]) == [a, k]);
k_refl(a) = refl();

[f(f(a)), k_refl(f(a))] == [f(a), k_refl(a)]
k_refl(f(a)) == k_refl(a)





//
I : Type;
0 : I;
1 : I;
seg(C : I -> Type) : C(0) -> C(1);

I = (i(C, l : C(0), r : C(1), m : seg(C, l) == r) : C(i));
0 = (i(C, l, r, m) = l);
1 = (i(C, l, r, m) = r);
// relying on built-in funext
seg(C) = #coe(i(C, l, r, m) = m, C);

//
Path(T : I -> Type, x : T(0), y : T(1)) : Type;
Path(T, x, y) = [
  f(i : I) : T(i),
  l : Path(_(i) = T(0), f(0), x),
  r : Path(_(i) = T(1), f(1), y),
];


//
//
f_I(a : A) : f(f(a)) == f(a);
f_J(a) : Path(
  λ(i) => f(f_I(a)(i)) == f_I(a)(i),
  ap(f, f_I(a)),
  f_I(a)
)

K_Eq(a : A) : Type;
k_refl(a) : K_Eq(f(a));
k_mod(
  a,
  f_I(a) : f(f(a)) == f(a),
  f_J(a) : Path(λ(i) => f(f_I(i)) == f_I(i), ap(f, f_I(a)), f_I(a))
) : Path(λ(i) => K_Eq(f_I(a)(i)), k_refl(f(a)), k_refl(a));

K_Eq(a) = (k : ([f(a), k_refl(a)] : [a : A, k : K_Eq(a)]) == [a, k]);
k_refl(a) = λ(i) => [f_I(a)(i), k_mod(a, f_I, f_J)(i)];
k_mod(a, p) = λ(i) => λ(j) => [
  f_J(a)(i)(j),

  _ : K_Eq(f_J(a)(i)(j))
];

//

Eq(A : Type, x : A, y : A) : Type;
Eq(A, x, y) = [
  c : C_Eq(A, x, y),
  p : Eq([z : A, p : C_Eq(A, x, z)], [x, c_refl()], [y, c])
];

refl{A, x}() : Eq(A, x, x);
refl{A, x}() = [c_refl(), refl()];


J{A, x, y}(p : Eq(A, x, y)) : [x, refl()] == [y, p];
J{A, x, y}(p) =
  [
    p.c,
    coe(
      J(p.p),
      λ([z, q]) => coe(q.c, Eq(A, x), refl()) == q,
      refl()
    )
  ];



K_Eq(a : A) : Type;
K_refl(a) : K_Eq(f(a));

K_Eq(a) = (k : ([f(a), k_refl(a)] : [a : A, k : K_Eq(a)]) == [a, k]);
k_refl(a) = (λ(i) => [f(f(a)), k_refl(f(a))] ⇒ _);

k_refl(f(a)) == k_refl(a)
(λ(i) => [f(f(a)), k_refl(f(a))] ⇒ _){a := f(a)} ==
  (λ(i) => [f(f(a)), k_refl(f(a))] ⇒ _)

(λ(i) => [f(f(a)), k_refl(f(a))] ⇒ _){a := f(a)} ==
  (λ(i) => [f(f(a)), k_refl(f(a))] ⇒ _)

[f(f(a)), k_refl(f(a))] == [f(a), k_refl(a)]

k_refl(f(a)) == k_refl(a)

k_refl(f(a)) == k_refl(a)
(λ(a) => k_refl(f(a))) == k_refl

_A(f(a)) == _A(a)
(λ(a) => _A(f(a))) == _A

(λ(i) => [f(a), k_refl(a)] ⇒ _){a := f(a)} == (λ(i) => [f(a), k_refl(a)] ⇒ _)

k_refl(f(a)) == k_refl(a)

k_refl(a) : [f(f(a)), k_refl(f(a))] == [f(a), k_refl(a)]


Fix : Type;
fix(a : A) : Fix;
mod(a) : fix(f(a)) == fix(a);

Fix = (x : [a : A, p : fix(a) == x]);
fix(a) = [f(a), mod(a)];
mod(a) = λ(i) => [f(a), λ(j) => mod(a)(min(i, j))];


fix(f(a)) == [f(a), λ(j) => fix(f(a))]
[f(f(a)), mod(f(a))] == [f(a), λ(j) => fix(f(a))]
mod(f(a)) == λ(j) => fix(f(a))
[f(f(a)), λ(j) => mod(f(a))(min(i, j))] == fix(f(a))
λ(j) => mod(f(a))(min(i, j)) == mod(f(a))
mod(f(a))(min(i, j)) == mod(f(a))(j)


fix(f(a)) == [f(f(a)), mod(f(a))]
fix(f(a)) == fix(a)


//
K_Eq(a : A) : Type;
K_refl(a) : K_Eq(f(a));

K_Eq(a) = (k : ([f(a), k_refl(a)] : [a : A, k : K_Eq(a)]) == [a, k]);
k_refl(a) = refl();


// this will happen at typing
(refl() : [f(f(a)), k_refl(f(a))] == [f(f(a)), k_refl(f(a))]) ⇐
  (_A : ([f(f(a)), k_refl(f(a))] : [a : A, k : K_Eq(a)]) == [f(a), k_refl(a)])

// which is unified like
[([f(f(a)), k_refl(f(a))] == [f(f(a)), k_refl(f(a))]), refl()] ≡
[([f(f(a)), k_refl(f(a))] == [f(a), k_refl(a)]), _A]

[f(f(a)), k_refl(f(a))] ≡ [f(a), k_refl(a)]
k_refl(f(a)) ≡ k_refl(a)
// prunning saves the day?
_A(f(a)) ≡ _A(a)
_A ≡ _A


(λ(a) => _A(f(a))) ≡ _A

// back to the right hand
refl() ≡ _A


[f(f(a)), k_refl(f(a))] == [f(a), k_refl(a)]
k_refl(f(a)) == [f(a), k_refl(a)]


//
K_Eq(a : A) : Type;
K_refl(a) : K_Eq(f(a));

K_Eq(a) = (k : ([f(a), k_refl(a)] : [a : A, k : K_Eq(a)]) == [a, k]);
k_refl(a) = refl();


P : [
  K_Eq(a : A) : Type;
  K_refl(a) : K_Eq(f(a));
];
P = [
  K_Eq(a) = (k : ([f(a), P.k_refl(a)] : [a : A, k : P.K_Eq(a)]) == [a, k]);
  k_refl(a) = refl();
];

P == fix(P) => [
  K_Eq(a) = (k : ([f(a), P.k_refl(a)] : [a : A, k : P.K_Eq(a)]) == [a, k]);
  k_refl(a) = _A;
]

(fix(P) => [
  K_Eq(a) = (k : ([f(a), P.k_refl(a)] : [a : A, k : P.K_Eq(a)]) == [a, k]);
  k_refl(a) = _A;
]).k_refl(f(a)) == (fix(P) => [
  K_Eq(a) = (k : ([f(a), P.k_refl(a)] : [a : A, k : P.K_Eq(a)]) == [a, k]);
  k_refl(a) = _A;
]).k_refl(a)

k_refl(a)



[([f(f(a)), k_refl(f(a))] == [f(f(a)), k_refl(f(a))]), refl()] ≡
[([f(f(a)), k_refl(f(a))] == [f(a), k_refl(a)]), _A]

k_refl(f(a)) ≡ k_refl(a)
_A(f(a)) ≡ _A(a)




```
