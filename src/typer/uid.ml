(* TODO: 32 bits *)
assert (Sys.word_size = 64)

type t = int [@@deriving show]

let equal = Int.equal
let compare = Int.compare

(* TODO: discuss immutable API *)
let acc = Atomic.make 0
let next () = Atomic.fetch_and_add acc 1
