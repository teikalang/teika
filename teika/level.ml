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

let global_to_local ~size ~var ~depth =
  let top = size - 1 in
  Index.of_int @@ (top + Index.repr depth - var)

let local_to_global ~size ~var ~depth =
  let top = size - 1 in
  of_int @@ (top + Index.repr depth - Index.repr var)

module Map = Map.Make (Int)
