const nil = null;
const cons = (hd, tl) => ({ hd, tl });
const rev = function* (acc, l) {
  if (l === null) {
    return acc;
  }
  const { hd, tl } = l;
  return rev({ hd, tl: acc }, tl);
};
const trampoline = (iter) => {
  let { done, value } = iter.next();
  while (value instanceof Iterator) {
    const step = value.next();
    value = step.value;
  }
  return value;
};
console.log(trampoline(rev(nil, cons(1, cons(2, cons(3, nil))))));

// const x = trampoline(rev(nil, cons(1, cons(2, nil))));
// console.log(x);
