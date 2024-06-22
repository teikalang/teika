type index = int
and t = index [@@deriving show, eq]

let zero = 0

let next n =
  let n = n + 1 in
  assert (n + 1 >= zero);
  n

let ( < ) : index -> index -> bool = ( < )
let repr x = x
let of_int x = match x >= zero with true -> Some x | false -> None

let shift index ~by_ =
  (* TODO: better error message here  *)
  assert (by_ >= 0);
  index + by_

module Map = Map.Make (Int)
