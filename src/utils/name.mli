type t [@@deriving show]

val make : string -> t
val equal : t -> t -> bool
val compare : t -> t -> int
val repr : t -> string

(* TODO: stop exposing this *)
module Map : Map.S with type key = t
