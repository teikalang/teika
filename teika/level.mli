type level
type t = level [@@deriving show, eq]

val zero : level
val next : level -> level
val ( < ) : level -> level -> bool

(* TODO: bad to expose this *)
val repr : level -> int

(* TODO: better place for this *)
val global_to_local : size:level -> var:level -> depth:Index.t -> Index.t option
val local_to_global : size:level -> var:Index.t -> depth:Index.t -> level option

module Map : Map.S with type key = level
