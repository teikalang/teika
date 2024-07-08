type level
type t = level [@@deriving show, eq]

val zero : level
val next : level -> level
val ( < ) : level -> level -> bool

(* TODO: not great to expose this *)
val repr : level -> int
val offset : from:level -> to_:level -> Index.t option

module Map : Map.S with type key = level
