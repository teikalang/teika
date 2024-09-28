type level = private int
type t = level [@@deriving show, eq]

val zero : level
val next : level -> level
