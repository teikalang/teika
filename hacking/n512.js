var zero = function () { return function (z) { return function (s) { return z; }; }; };
var succ = function (pred) {
    return function (z) {
        return function (s) {
            return s(pred(z)(s));
        };
    };
};
var add = function (a) { return function (b) { return a(b)(succ); }; };
var mul = function (a) { return function (b) { return a(zero())(function (n) { return add(n)(b); }); }; };
var one = function () { return succ(zero()); };
var two = function () { return succ(one()); };
var n4 = function () { return mul(two())(two()); };
var n8 = function () { return mul(n4())(two()); };
var n16 = function () { return mul(n8())(two()); };
var n32 = function () { return mul(n16())(two()); };
var n64 = function () { return mul(n32())(two()); };
var n128 = function () { return mul(n64())(two()); };
var n256 = function () { return mul(n128())(two()); };
var n512 = function () { return mul(n256())(two()); };
var bench = function (f) {
    var t1 = performance.now();
    f();
    console.log(performance.now() - t1);
};
console.log(bench(n512));
