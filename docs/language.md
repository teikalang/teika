```rust
Term, M, N, K ::=
  | x
  | (_(x : A) : B) | (_(x) = K) | M(N)
  | [M] | (x : M, N) | M.x
Pattern, P, Q ::=
  | (P : A) | P(Q) | [] | [P, Q]


Point = [x : Nat64, y : Nat64];
Point = (x : Nat64, y : Nat64);

Vec(n : Nat64, A : Type) : Type;
Vec(n, A) =
  n.is(
  | 0 => ()
  | 1 + n => (hd : A, tl : Vec(n, A))
  );
Array(A : Type) = [len : Nat64, buf : Vec(len, A)];


Γ, f(a : A) : B, a : A |- A : Type
Γ, f(a : A) : B, a : A |- B : Type
----------------------------------
Γ |- (f(a : A) : B) : Type

Bool : Type;
true : Bool;
false : Bool;

Bool = (b(C(b : Bool) : Type, t : C(true), f : C(false)) : C(b))
true = (b(C, t, f) = t);
false = (b(C, t, f) = f);

not(b : Bool) = b(false, true);
not(b : Bool) = b ? false : true;

incr(x) = 1 + x;

map{A, B}(f(a : A) : B, l : List(A)) : List(B);
map{A, B}(f, l) =
  l.is(
  | [] => []
  | [hd, ...tl] => [f(hd), ...map{A, B}(f, tl)]
  );


[x, y] = M;
[x, y] |-> M
x |-> M.0
y |-> M.1

f(x) = M;
f(x) |-> M
f |-> (_(x) = M)

@map = M
@map |-> M
map |-> (@map = M)

// "anonymous function types"
T : Type = (_(x : Nat) : Nat);
// the name is not very useful
T : Type = (f(x : Nat) : Nat);
// unless you make it recursive
@False : Type = (@f(C(f : False) : Type) : C(f));

// "anonymous functions"
incr = (_(x) = 1 + x);
// same as
incr = (incr(x) = 1 + x);
// or just
incr(x) = 1 + x;

// introduction
f = (f(x) = M);
f = (f(x : A) : B = M);
// elimination
y = f(x);
y = (f(x : A) : B);
// formation
T : Type = (f(x : A) : B);


// introduction
p = [x = M, y = N];
// elimination
[x, y] = p;
// formation
T : Type = [x : A, y : B];

// 1-tuple for self
T : Type = (f : (x : A) -> B);

// it's very cute on packages
T = [
  A : Type;
  f(a : A) : B;
];

// could be either apply fully annotated
// or it could be a type
T = (f(x : A) : B);

// so you need to annotate
T : Type = (f(x : A) : B);

// but it doesn't matter most of the time
// because it's mostly in a pattern already
g(f(x : A) : B) = M;





y = (f(x : A) : B);





T = # (f(x : A) : B);

Bool : Type;
true : Bool;
false : Bool;

Bool2 =#
  | [b : Bool == true]
  | [b : Bool == false];


List(A : Type) : Type;
List(A) =#
  | null
  | cons(A, List(A));

//
List(A : Type) : Type;
List(A) =#
  | [tag == "null"]
  | [tag == "cons", hd : A, tl : List(A)];

List.null(A) : List(A);
null : List(A);

//
Bool : Type & (
  true : Bool;
  false : Bool;
);

User_Data : Type;

User1 = User_Data;

tamwile : User1;
tamwile.name()

User2 = User_Data;
liro : User2;
liro.name()


El(A : Name) : Type;

Show = [
  A : Name,
  show(a : A) : String;
];
show{A : Show}(x : A) : String = x.show();


User : Type = [id : Nat, name : String];
User.show(x : User) : String;
User.show(x : User) : Show =& [
  show(u : User) : String = _;
];

impl Show for User {

}
show{A : Show}(x : A) : String = x.show();

Company : Type;
User : Type;

show : (_(x : User | Company) : String);

show{User}(eduardo : User)

x = case(true | false);
A & B & C

[P : A; Q : B]

x : A = M; N

[] = M;
if let Some x = M {

}


Γ, f(a : A) : B, a : A |- A : Type
Γ, f(a : A) : B, a : A |- B : Type
----------------------------------
Γ |- (f(a : A) : B) : Type

Γ |- A : Type  Γ, a : A |- B : Type
-----------------------------------
Γ |- (Π(a : A) : B) : Type

Γ, a : A |- A : Type
--------------------
Γ |- (a : A) : Type

(["some", x] = M; N) |
K

f(x)(y) = N;

Bool = (b : [
  c : C_Bool,
  p : [f(c), f_I(c)] == [c, p]
]);

[12, [99, [37, null]]]

Term, M, N, K ::=
  | Type // univ
  | x // var
  | P = N; K // let
  | (P : A) // self
  | (P = N) // fix
  | M(N) // apply

Pattern, P, Q ::=
  | x // var
  | (P : A) // annot
  | P(Q) // apply

(P : A) |-> (ι : λ(P) => A)

(P(Q) : T)
```
