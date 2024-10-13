var Tuturu;
(function (Tuturu) {
  const id = (x) => x;
  const x = id;
  const g = (f) => f();
})(Tuturu || (Tuturu = {}));
var Test;
(function (Test) {
  const f = (x) => {
    if (typeof x.a == "number") {
      console.log(x);
    } else {
      console.log(x);
    }
  };
  const f = (x) =>
    x((x) => {
      if (typeof x === "number") {
        console.log(x + 1);
      }
    });
  const refute = (x) => {
    throw new Error("unexpected");
  };
  const never = () => never();
  const id = (x) => x;
  const true_ = (x) => (y) => x;
  const false_ = (x) => (y) => y;
  const toBool = (b) => b(true)(false);
  const zero = (z) => (s) => z;
  const succ = (n) => (z) => (s) => s(n(z)(s));
  const n_reduce = (map) => (initial) => (n) => n(initial)(map);
  const nil = () => (nil) => (cons) => nil;
  const cons = (el) => (tail) => (nil) => (cons) => cons(tail(nil)(cons))(el);
  const l_reduce = (map) => (initial) => (l) => l(initial)(map);
  const el = cons(1)(nil());
  const loop = (x) => loop(x);
})(Test || (Test = {}));
const zero = (w) => w((zero, succ) => zero);
const succ = (pred) => (w) => w((zero, succ) => succ(pred(w)));
var Algebraic;
(function (Algebraic) {
  const f = (x) => x;
  const g = (x) => x;
  const x = g;
  // const y = g();
})(Algebraic || (Algebraic = {}));
const g = (x) => x.then((a) => a.then((b) => console.log(1 + b)));
const pure = (x) => Promise.resolve(x);
const a = g(pure(pure(1)));
export {};
