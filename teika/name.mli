type t [@@deriving show, eq, ord]

val make : string -> t
val repr : t -> string

(* TODO: stop exposing this *)
module Map : Map.S with type key = t
