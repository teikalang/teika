export {};

namespace Tuturu {
  type Id<A> = A;
  type T = Id<T>;

  const id = <A>(x: A) => x;
  const x = id<string>;

  const g: (f: <A>() => never) => never = (f) => f();
}
namespace Test {
  type t = { a: string; b: number } | { a: number; b: boolean };
  const f = (x: t) => {
    if (typeof x.a == "number") {
      console.log(x);
    } else {
      console.log(x);
    }
  };
  type Unknown = <K>(k: <A>(x: A) => K) => K;

  const f = (x: Unknown) =>
    x(<A>(x: A) => {
      if (typeof x === "number") {
        console.log(x + 1);
      }
    });

  const refute = <A>(x: never): A => {
    throw new Error("unexpected");
  };

  type Never = <A>() => A;
  const never: Never = () => never();

  type Id = <A>(x: A) => A;
  const id: Id = <A>(x: A) => x;

  type Bool = <A>(x: A) => (y: A) => A;
  const true_: Bool = (x) => (y) => x;
  const false_: Bool = (x) => (y) => y;
  const toBool = (b: Bool) => b(true)(false);

  type Nat = <A>(z: A) => (s: (acc: A) => A) => A;
  const zero: Nat = (z) => (s) => z;
  const succ: (n: Nat) => Nat = (n) => (z) => (s) => s(n(z)(s));
  const n_reduce: <A>(map: (acc: A) => A) => (initial: A) => (n: Nat) => A =
    (map) => (initial) => (n) =>
      n(initial)(map);

  type List<T> = <A>(nil: A) => (cons: (acc: A) => (el: T) => A) => A;
  const nil =
    <T>(): List<T> =>
    (nil) =>
    (cons) =>
      nil;
  const cons: <T>(el: T) => (tail: List<T>) => List<T> =
    (el) => (tail) => (nil) => (cons) =>
      cons(tail(nil)(cons))(el);
  const l_reduce: <T, A>(
    map: (acc: A) => (el: T) => A
  ) => (initial: A) => (l: List<T>) => A = (map) => (initial) => (l) =>
    l(initial)(map);

  const el = cons(1)(nil());

  const loop = <A>(x: Id): A => loop(x);
}
type Nat_G<INat> = <R>(zero: R, succ: (pred: INat) => R) => R;
type Nat = <INat>(w: (nat: Nat_G<INat>) => INat) => INat;
const zero: Nat = (w) => w((zero, succ) => zero);
const succ: (pred: Nat) => Nat = (pred) => (w) =>
  w((zero, succ) => succ(pred(w)));

namespace Algebraic {
  const f: <A extends number>(x: A) => A = (x) => x;
  const g: <A>(x: A & number) => A = (x) => x;

  const x = g<string | number>;

  // const y = g();
}

namespace Equality {
  // enumerate
  // Type -> Type
  // type App1<F, A> =
  //   | (F extends "unit" ? Unit<A> : never)
  //   | (F extends "option" ? Option<A> : never)
  //   | (F extends "list" ? List<A> : never);

  // // Type -> Type -> Type
  // type KBool = "true" | "false";
  // type App2<F, A, B> =
  //   | (F extends "true" ? True<A, B> : never)
  //   | (F extends "false" ? False<A, B> : never)
  //   | (F extends "eq" ? Eq<A, B> : never)
  //   | (F extends "eq_symm" ? Eq_symm<A, B> : never);

  // type Unit<A> = A;

  // type True<A, B> = A;
  // type False<A, B> = B;
  // type Eq<A, B> = <P, E>(x: App2<P, E, A>) => App2<P, E, B>;

  // // data
  // type Option<A> = { tag: "none" } | { tag: "some"; payload: A };
  // type List<A> = { tag: "null" } | { tag: "cons"; content: A; next: List<A> };

  // defunctionalized

  type Eq<A, B> = <P, E>(x: App2<P, E, A>) => App2<P, E, B>;

  type App2<F, A, B> =
    | (F extends "eq" ? Eq<A, B> : never)
    | (F extends "eq_symm" ? Eq_symm<A, B> : never);

  type Eq_symm<E, A> = Eq<A, E>;

  const refl =
    <A>(): Eq<A, A> =>
    <P, E>(x: App2<P, E, A>) =>
      x;

  const symm = <A, B>(H: Eq<A, B>): Eq<B, A> => H<"eq_symm", A>(refl());
}

const g = (x: Promise<Promise<number>>) =>
  x.then((a) => a.then((b) => console.log(1 + b)));
const pure = <A>(x: A) => Promise.resolve(x);

const a = g(pure(pure(1)));
namespace F {
  type W<S> = (x: S) => boolean;
  type U = <X>(f: (x: W<W<X>>) => X) => W<W<X>>;
  const T =
    (t: W<W<U>>): U =>
    (f) =>
    (p) =>
      t((x) => p(f(x(f))));

  const S = (s: U): W<W<U>> => s(T);
  const O: U = T((p): boolean => {
    const x = S;
    return null as any;
  });
}

namespace Scott {
  type Unit = <X>(w: (x: [X, <A>(x: A) => A]) => X) => X;
  const unit: Unit = <X>(w: (x: [X, <A>(x: A) => A]) => X): X => w(null as any);
}

namespace T {
  const f = <A extends boolean>(
    b: A,
    x: A extends true ? string : A extends false ? number : never
  ) => {};

  const x = f(true, "a");
  const y = f(false, 1);
}
