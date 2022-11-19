type var
type t = var [@@deriving show, eq, ord]

val create : Name.t -> var
val name : var -> Name.t

(* predefined *)
(* Type *)
val type_ : var

module Map : Map.S with type key = var
