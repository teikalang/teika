type var
type t = var [@@deriving show]

val create : Name.t -> var
val equal : var -> var -> bool
val compare : var -> var -> int
val name : var -> Name.t

(* predefined *)
(* Type *)
val type_ : var

module Map : Map.S with type key = t
