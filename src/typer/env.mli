open Utils
open Type

type error = private Unknown_name of { name : Name.t }
type exn += private Error of { loc : Location.t; error : error }
type t

(* include all base primitives *)
val base : t
val int_ident : Ident.t
val int_type : Type.t

(* insertion *)
val add : Location.t -> Name.t -> type_ -> t -> Ident.t * t
val lookup : Location.t -> Name.t -> t -> Ident.t * type_

(* rank *)
val current_rank : t -> Rank.t
val enter_rank : t -> t

(* TODO: should enter_forall receive the rank? *)
val enter_forall : forall:Forall_id.t -> t -> t
val find_forall : forall:Forall_id.t -> t -> Rank.t option

(* types *)
val new_weak_var : t -> Type.t
