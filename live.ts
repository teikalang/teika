export {};
const exhaustive = <A>(x: never): A => x;

module LList {
  // Immutable Linked List
  type LList<A> =
    | {
        readonly tag: "cons";
        readonly value: A;
        readonly next: LList<A>;
      }
    | { readonly tag: "nil" };
  const nil = <A>(): LList<A> => ({ tag: "nil" });
  const cons = <A>(value: A, list: LList<A>): LList<A> => ({
    tag: "cons",
    value,
    next: list,
  });

  const foldLeft = <A, B>(
    f: (acc: A, value: B) => A,
    initial: A,
    list: LList<B>
  ): A => {
    if (list.tag === "nil") {
      return initial;
    } else if (list.tag === "cons") {
      return foldLeft(f, f(initial, list.value), list.next);
    } else {
      return exhaustive(list);
    }
  };
  const rev = <A>(list: LList<A>): LList<A> =>
    foldLeft((acc, value) => cons(value, acc), nil(), list);

  const foldRight = <A, B>(
    f: (acc: A, value: B) => A,
    initial: A,
    list: LList<B>
  ): A => foldLeft(f, initial, rev(list));

  const revMap = <A, B>(f: (value: A) => B, list: LList<A>): LList<B> =>
    foldLeft((acc, value) => cons(f(value), acc), nil(), list);
  const map = <A, B>(f: (value: A) => B, list: LList<A>): LList<B> =>
    foldRight((acc, value) => cons(f(value), acc), nil(), list);
}

const exhaustive = <A>(x: never): A => x;

module DLList {
  // Double Linked List
  type DLList<A> =
    | {
        readonly tag: "cons";
        value: A;
        left: DLList<A>;
        right: DLList<A>;
      }
    | { readonly tag: "nil" };
  const nil = <A>(): DLList<A> => ({ tag: "nil" });
  const append = <A>(value: A, list: DLList<A>): DLList<A> => {
    if (list.tag === "nil") {
      return { tag: "cons", value: value, left: nil(), right: list };
    } else if (list.tag === "cons") {
      list.left = { tag: "cons", value: value, left: list.left, right: list };
      return list.left;
    } else {
      return exhaustive(list);
    }
  };
  const remove = <A>(list: DLList<A>) => {
    if (list.tag === "nil") {
    } else if (list.tag === "cons") {
      if (list.left.tag === "nil") {
      } else if (list.left.tag === "cons") {
        list.left.right = list.right;
      } else {
        return exhaustive(list.left);
      }
      if (list.right.tag === "nil") {
      } else if (list.right.tag === "cons") {
        list.right.left = list.left;
      } else {
        return exhaustive(list.right);
      }
    } else {
      return exhaustive(list);
    }
  };
  const foldLeftToRight = <A, B>(
    f: (acc: A, value: B) => A,
    initial: A,
    list: DLList<B>
  ): A => {
    if (list.tag === "nil") {
      return initial;
    } else if (list.tag === "cons") {
      return foldLeftToRight(f, f(initial, list.value), list.right);
    } else {
      return exhaustive(list);
    }
  };
  const foldLeftToLeft = <A, B>(
    f: (acc: A, value: B) => A,
    initial: A,
    list: DLList<B>
  ): A => {
    if (list.tag === "nil") {
      return initial;
    } else if (list.tag === "cons") {
      return foldLeftToLeft(f, f(initial, list.value), list.left);
    } else {
      return exhaustive(list);
    }
  };
  const foldRightToRight = <A, B>(
    f: (acc: A, value: B) => A,
    initial: A,
    list: DLList<B>
  ): A => {
    if (list.tag === "nil") {
      return initial;
    } else if (list.tag === "cons") {
      return f(foldLeftToRight(f, initial, list.right), list.value);
    } else {
      return exhaustive(list);
    }
  };
  const foldRightToLeft = <A, B>(
    f: (acc: A, value: B) => A,
    initial: A,
    list: DLList<B>
  ): A => {
    if (list.tag === "nil") {
      return initial;
    } else if (list.tag === "cons") {
      return f(foldLeftToRight(f, initial, list.left), list.value);
    } else {
      return exhaustive(list);
    }
  };
}
