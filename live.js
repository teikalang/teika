const exhaustive = (x) => x;
var LList;
(function (LList) {
    const nil = () => ({ tag: "nil" });
    const cons = (value, list) => ({
        tag: "cons",
        value,
        next: list,
    });
    const foldLeft = (f, initial, list) => {
        if (list.tag === "nil") {
            return initial;
        }
        else if (list.tag === "cons") {
            return foldLeft(f, f(initial, list.value), list.next);
        }
        else {
            return exhaustive(list);
        }
    };
    const rev = (list) => foldLeft((acc, value) => cons(value, acc), nil(), list);
    const foldRight = (f, initial, list) => foldLeft(f, initial, rev(list));
    const revMap = (f, list) => foldLeft((acc, value) => cons(f(value), acc), nil(), list);
    const map = (f, list) => foldRight((acc, value) => cons(f(value), acc), nil(), list);
})(LList || (LList = {}));
const exhaustive = (x) => x;
var DLList;
(function (DLList) {
    const nil = () => ({ tag: "nil" });
    const append = (value, list) => {
        if (list.tag === "nil") {
            return { tag: "cons", value: value, left: nil(), right: list };
        }
        else if (list.tag === "cons") {
            list.left = { tag: "cons", value: value, left: list.left, right: list };
            return list.left;
        }
        else {
            return exhaustive(list);
        }
    };
    const remove = (list) => {
        if (list.tag === "nil") {
        }
        else if (list.tag === "cons") {
            if (list.left.tag === "nil") {
            }
            else if (list.left.tag === "cons") {
                list.left.right = list.right;
            }
            else {
                return exhaustive(list.left);
            }
            if (list.right.tag === "nil") {
            }
            else if (list.right.tag === "cons") {
                list.right.left = list.left;
            }
            else {
                return exhaustive(list.right);
            }
        }
        else {
            return exhaustive(list);
        }
    };
    const foldLeftToRight = (f, initial, list) => {
        if (list.tag === "nil") {
            return initial;
        }
        else if (list.tag === "cons") {
            return foldLeftToRight(f, f(initial, list.value), list.right);
        }
        else {
            return exhaustive(list);
        }
    };
    const foldLeftToLeft = (f, initial, list) => {
        if (list.tag === "nil") {
            return initial;
        }
        else if (list.tag === "cons") {
            return foldLeftToLeft(f, f(initial, list.value), list.left);
        }
        else {
            return exhaustive(list);
        }
    };
    const foldRightToRight = (f, initial, list) => {
        if (list.tag === "nil") {
            return initial;
        }
        else if (list.tag === "cons") {
            return f(foldLeftToRight(f, initial, list.right), list.value);
        }
        else {
            return exhaustive(list);
        }
    };
    const foldRightToLeft = (f, initial, list) => {
        if (list.tag === "nil") {
            return initial;
        }
        else if (list.tag === "cons") {
            return f(foldLeftToRight(f, initial, list.left), list.value);
        }
        else {
            return exhaustive(list);
        }
    };
})(DLList || (DLList = {}));
export {};
