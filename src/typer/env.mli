type error = private Unknown_name of { name : Name.t }
type exn += private Error of { loc : Location.t; error : error }
type t

(* include all base primitives *)
val base : t
val int_ident : Ident.t
val int_type : Type.t

(* insertion *)
val enter : Location.t -> Name.t -> Type.t -> t -> Ident.t * t
val lookup : Location.t -> Name.t -> t -> Ident.t * Type.t

(* rank *)
val current_rank : t -> Rank.t
val enter_rank : t -> t

(* types *)
val new_weak_var : t -> Type.t
