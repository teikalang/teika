type t [@@deriving show]

val next : unit -> t
val equal : t -> t -> bool
val compare : t -> t -> int
