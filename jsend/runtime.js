const $type = "$type";
const $debug = function* (value) {
  console.debug("debug", value);
  return value;
};

// TODO: instanceof vs special tag maybe symbol
// TODO: $jmp(f1)(...args)
function $jmp(instance) {
  this.instance = instance;
}

// TODO: benchmark against class
// TODO: unboxed by default, except call and jmp
// TODO: this function is probably bad
const $curry = function (f) {
  const fLength = f.length;
  return function* self() {
    // TODO: ...args?
    const args = Array.from(arguments);
    if (fLength === 0 || args.length === fLength) {
      // ...args instead of apply?
      return new $jmp(f.apply(null, args));
    } else if (args.length < fLength) {
      return function () {
        const additional = Array.from(arguments);
        return self.apply(null, args.concat(additional));
      };
    }
    // TODO: additional arguments on $jmp so no yield here
    const next = yield f.apply(null, args.slice(0, fLength));
    const remaining = args.slice(fLength);
    return function () {
      // TODO: test this branch
      const additional = Array.from(arguments);
      return next.apply(null, remaining.concat(additional));
    };
  };
};

/* TODO:
  have multiple versions same function
  single counter, when stack get's too deep it trampolines
*/
const $run = function (initial) {
  // TODO: microoptimizations
  let stack = [];
  let current = initial;
  let value = undefined;
  while (current !== undefined) {
    // TODO: could also rely on this of generators
    // TODO: could check for current.is_tco
    // TODO: better way of detecting if it is an instance
    const step = current.next(value);
    if (step.done) {
      if (step.value instanceof $jmp) {
        // this is jmp
        current = step.value.instance;
        value = undefined;
      } else {
        // this is ret
        current = stack.pop();
        value = step.value;
      }
    } else {
      // this is call
      stack.push(current);
      current = step.value;
      value = undefined;
    }
  }
  return value;
};

const $code = function* () {
  return yield $curry(function* (A$6, x$7) {
    return x$7;
  })($type, "Hello World");
};
console.log($run($code()));
