type level = int
and t = level [@@deriving show, eq]

let zero = 0

(* TODO: check for overflows *)
let next n = n + 1
let offset ~from ~to_ = Index.of_int (to_ - from)
let ( < ) : level -> level -> bool = ( < )

module Map = Map.Make (Int)