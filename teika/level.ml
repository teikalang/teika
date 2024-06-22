type level = int
and t = level [@@deriving show, eq]

let zero = 0

let next n =
  let n = n + 1 in
  assert (n + 1 >= zero);
  n

let ( < ) : level -> level -> bool = ( < )
let of_int x = match x >= zero with true -> Some x | false -> None

let level_of_index ~next ~var =
  let var = Index.repr var in
  of_int (next - 1 - var)

let repr level = level
let offset ~from ~to_ = Index.of_int (to_ - from)

module Map = Map.Make (Int)
