open Utils

(* WARNING: only physical identity safe to use is on type_ *)
type type_
type t = type_

type desc = private
  | T_forall of { forall : Forall.t; body : type_ }
  | T_var of var
  | T_arrow of { param : type_; return : type_ }
  (* TODO: enforce that field list doesn't contain any duplicated name *)
  | T_struct of { fields : field list }
  (* TODO: T_type is weird *)
  | T_type of { forall : Forall.t; type_ : type_ }

and var = private
  | Weak of { mutable forall : Forall.t }
  | Bound of { mutable forall : Forall.t }

and field = { name : Name.t; type_ : type_ }
and link

val same : type_ -> type_ -> bool
(** [same a b] returns true if both types reference the same type
    it is the only comparison that should be done on types *)

val desc : type_ -> desc
(** [desc type_] the type with all of it's link resolved*)

val link : to_:type_ -> type_ -> unit
(** [link ~to var] links a var such that `same to var = true` *)

val lower_var : to_:Forall.t -> type_ -> unit
(** [lower ~to var] update the variable forall to a lower forall *)

(* helpers *)
(* TODO: are those helpers useful? *)
val new_forall : Forall.t -> body:type_ -> type_
val new_weak_var : Forall.t -> type_
val new_bound_var : Forall.t -> type_
val new_arrow : param:type_ -> return:type_ -> type_
val new_struct : fields:field list -> type_
val new_type : Forall.t -> type_:type_ -> type_
