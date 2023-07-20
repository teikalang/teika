type level = int
and t = level [@@deriving show, eq]

let zero = 0

(* TODO: check for overflows *)
let next n = n + 1
let ( < ) : level -> level -> bool = ( < )
