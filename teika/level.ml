type level = int
and t = level [@@deriving eq]

let zero = 0

(* TODO: check for overflows *)
let next n = n + 1
let offset ~from ~to_ = Offset.of_int (to_ - from)
let ( < ) : level -> level -> bool = ( < )
