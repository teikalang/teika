type level = int
and t = level [@@deriving show, eq]

let zero = 0

let next n =
  let n = n + 1 in
  assert (n + 1 >= zero);
  n
