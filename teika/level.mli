type level
type t = level [@@deriving show, eq]

val zero : level
val next : level -> level
val ( < ) : level -> level -> bool
