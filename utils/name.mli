type name
type t = name [@@deriving show, eq, ord]

val make : string -> name
val repr : name -> string

(* TODO: stop exposing this? *)
module Map : Map.S with type key = name
