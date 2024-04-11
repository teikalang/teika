type level
type t = level [@@deriving show, eq]

val nil : level
val zero : level
val next : level -> level
val ( < ) : level -> level -> bool

module Map : Map.S with type key = level
