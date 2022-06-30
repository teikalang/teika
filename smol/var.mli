type var
type t = var [@@deriving show]

val create : Name.t -> var
val equal : var -> var -> bool
val compare : var -> var -> int
val name : var -> Name.t
