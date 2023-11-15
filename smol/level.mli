type level
type t = level [@@deriving show, eq]

val zero : level
val next : level -> level
val offset : from:level -> to_:level -> Index.t
val ( < ) : level -> level -> bool

module Map : Map.S with type key = level
