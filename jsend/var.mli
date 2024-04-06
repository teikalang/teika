open Utils

type var
type t = var [@@deriving show]

val create : Name.t -> var
val equal : var -> var -> bool
val compare : var -> var -> int
val name : var -> Name.t

(* predefined *)
val type_ : var
val fix : var
val unit : var
val debug : var
val curry : var
val jmp : var

module Map : Map.S with type key = t
