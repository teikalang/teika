open Utils
module Forall_id : Uid.S

(* WARNING: only physical identity safe to use is on type_ *)
type type_ [@@deriving show]
type t = type_ [@@deriving show]

type desc =
  | T_forall of { forall : Forall_id.t; body : type_ }
  | T_var of var
  | T_arrow of { param : type_; return : type_ }
[@@deriving show]

and var =
  | Weak of Rank.t
  (* TODO: should we have this name here? It's duplicated from Tree.t *)
  (* TODO: check name across codebase *)
  | Bound of { forall : Forall_id.t; name : Name.t option }

val same : type_ -> type_ -> bool
(** [same a b] returns true if both types reference the same type
    it is the only comparison that should be done on types *)

val desc : type_ -> desc

val link : to_:type_ -> type_ -> unit
(** [link ~to var] links a var such that `same to var = true` *)

val new_type : desc -> type_
(** [new_type desc] create a new type *)

(* helpers *)
val new_forall : Forall_id.t -> body:type_ -> type_
val new_weak_var : Rank.t -> type_
val new_bound_var : name:Name.t option -> Forall_id.t -> type_
val new_arrow : param:type_ -> return:type_ -> type_
