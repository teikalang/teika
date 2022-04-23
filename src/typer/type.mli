open Utils
module Forall_id : Uid.S

type type_ [@@deriving show]
type t = type_ [@@deriving show]

type desc = private
  | T_forall of { forall : Forall_id.t; body : type_ }
  | T_var of var
  | T_arrow of { param : type_; return : type_ }
  | T_link of type_
[@@deriving show]

and var = private
  | Weak of Rank.t
  (* TODO: should we have this name here? It's duplicated from Tree.t *)
  (* TODO: check name across codebase *)
  | Bound of { forall : Forall_id.t; name : Name.t option }

val same : type_ -> type_ -> bool
val desc : type_ -> desc
val link : to_:type_ -> type_ -> unit

(* constructors *)
val new_forall : Forall_id.t -> body:type_ -> type_
val new_weak_var : Rank.t -> type_
val new_bound_var : name:Name.t option -> Forall_id.t -> type_
val new_arrow : param:type_ -> return:type_ -> type_

(* rank *)
val lower : var:type_ -> Rank.t -> unit
