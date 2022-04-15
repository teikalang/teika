type t

val make : Name.t -> t
val name : t -> Name.t
val equal : t -> t -> bool
val compare : t -> t -> int

module Map : Map.S with type key = t
