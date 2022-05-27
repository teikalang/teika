open Utils
open Type

type error = private Unknown_name of { name : Name.t }
type exn += private Error of { loc : Location.t; error : error }
type t

(* include all base primitives *)
val base : t
val int_ident : Ident.t
val int_type : Type.t

(* loc *)
(* TODO: is this a good idea? It's essentially a shadow stack *)
val set_loc : Location.t -> t -> t
val current_loc : t -> Location.t

(* insertion *)
val add : Name.t -> type_ -> t -> Ident.t * t
val lookup : Name.t -> t -> Ident.t * type_

(* forall *)
val current_forall : t -> Forall_id.t
val current_rank : t -> Rank.t
val enter_forall : forall:Forall_id.t -> t -> t
val find_forall : forall:Forall_id.t -> t -> Rank.t option

(* types *)
val new_weak_var : t -> Type.t
