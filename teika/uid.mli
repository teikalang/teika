type t [@@deriving show, eq, ord]

val initial : t
val next : t -> t
val repr : t -> int
