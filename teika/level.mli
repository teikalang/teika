type level
type t = level [@@deriving eq]

val zero : level
val next : level -> level
val ( < ) : level -> level -> bool
