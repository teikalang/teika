type var
type t = var [@@deriving show, eq, ord]

val make : Name.t -> var
val name : var -> Name.t

module Map : Map.S with type key = var
