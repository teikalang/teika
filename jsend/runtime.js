const $type = "$type";
const $debug = function* (value) {
  console.log("debug", value);
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
  // TODO: remove stack detection
  let biggest_stack = 0;
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
      biggest_stack = Math.max(stack.length, biggest_stack);
      current = step.value;
      value = undefined;
    }
  }
  console.error(biggest_stack);
  return value;
};

const main = function* () {
  return yield (function* () {
    const Nat$20 = $type;
    const zero$25 = $curry(function* (A$21, z$22, s$23, k$24) {
      return new $jmp(k$24(z$22));
    });
    const succ$32 = $curry(function* (n$26, A$27, z$28, s$29, k$30) {
      return new $jmp(
        n$26(
          A$27,
          z$28,
          s$29,
          $curry(function* (x$31) {
            return new $jmp(k$30(yield s$29(x$31)));
          })
        )
      );
    });
    const add$36 = $curry(function* (n$33, m$34) {
      return new $jmp(
        n$33(
          Nat$20,
          m$34,
          succ$32,
          $curry(function* (x$35) {
            return x$35;
          })
        )
      );
    });
    const mul$40 = $curry(function* (n$37, m$38) {
      return new $jmp(
        n$37(
          Nat$20,
          zero$25,
          yield add$36(m$38),
          $curry(function* (x$39) {
            return x$39;
          })
        )
      );
    });
    const one$41 = yield succ$32(zero$25);
    const two$42 = yield succ$32(one$41);
    const four$43 = yield mul$40(two$42, two$42);
    const eight$44 = yield mul$40(two$42, four$43);
    const sixteen$45 = yield mul$40(two$42, eight$44);
    const byte$46 = yield mul$40(sixteen$45, sixteen$45);
    const short$47 = yield mul$40(byte$46, byte$46);
    return new $jmp(
      short$47(
        $type,
        "zero",
        $curry(function* (_$48) {
          return new $jmp($debug("hello"));
        }),
        $curry(function* (x$49) {
          return x$49;
        })
      )
    );
  })();
};

console.log($run(main()));
