type Nat = <A>(z: A) => (s: (acc: A) => A) => A;
const zero: () => Nat = () => (z) => (s) => z;
const succ =
  (pred: Nat): Nat =>
  (z) =>
  (s) =>
    s(pred(z)(s));

const add = (a: Nat) => (b: Nat) => a(b)(succ);
const mul = (a: Nat) => (b: Nat) => a(zero())((n) => add(n)(b));

const one = () => succ(zero());
const two = () => succ(one());
const n4 = () => mul(two())(two());
const n8 = () => mul(n4())(two());
const n16 = () => mul(n8())(two());
const n32 = () => mul(n16())(two());
const n64 = () => mul(n32())(two());
const n128 = () => mul(n64())(two());
const n256 = () => mul(n128())(two());
const n512 = () => mul(n256())(two());

const bench = (f: () => unknown) => {
  const t1 = performance.now();
  f();
  console.log(performance.now() - t1);
};
console.log(bench(n512));
