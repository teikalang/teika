type level
type t = level [@@deriving eq]

val zero : level
val next : level -> level
val offset : from:level -> to_:level -> Offset.t
val ( < ) : level -> level -> bool
