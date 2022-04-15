type error = private Unknown_name of { name : Name.t }
type exn += private Error of { loc : Location.t; error : error }
type t

val empty : t
val enter : Location.t -> Name.t -> Type.t -> t -> Ident.t * t
val lookup : Location.t -> Name.t -> t -> Ident.t * Type.t

(* TODO: weird API, used on generalization *)
val types : t -> Type.t list
