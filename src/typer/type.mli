open Utils
module Forall_id : Uid.S

(* WARNING: only physical identity safe to use is on type_ *)
type type_
type t = type_

type desc = private
  | T_forall of { forall : Forall_id.t; body : type_ }
  | T_var of var
  | T_arrow of { param : type_; return : type_ }
  (* TODO: enforce that field list doesn't contain any duplicated name *)
  | T_struct of field list

and var = private
  | Weak of { rank : Rank.t; mutable link : link }
  | Bound of { forall : Forall_id.t; name : Name.t option }

and field = { name : Name.t; type_ : type_ }
and link

val same : type_ -> type_ -> bool
(** [same a b] returns true if both types reference the same type
    it is the only comparison that should be done on types *)

val desc : type_ -> desc
(** [desc type_] the type with all of it's link resolved*)

val link : to_:type_ -> type_ -> unit
(** [link ~to var] links a var such that `same to var = true` *)

(* helpers *)
(* TODO: are those helpers useful? *)
val new_forall : Forall_id.t -> body:type_ -> type_
val new_weak_var : Rank.t -> type_
val new_bound_var : name:Name.t option -> Forall_id.t -> type_
val new_arrow : param:type_ -> return:type_ -> type_
val new_struct : fields:field list -> type_
