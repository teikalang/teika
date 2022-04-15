module Forall_id : sig
  type t

  val next : unit -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

type type_ [@@deriving show]
type t = type_ [@@deriving show]

type desc = private
  | T_forall of { forall : Forall_id.t; body : type_ }
  | T_weak_var
  | T_bound_var of { forall : Forall_id.t }
  | T_int
  | T_arrow of { param : type_; return : type_ }
  | T_link of type_

val same : type_ -> type_ -> bool
val desc : type_ -> desc
val link : to_:type_ -> type_ -> unit

(* constructors *)
val new_forall : Forall_id.t -> body:type_ -> type_
val new_weak_var : unit -> type_
val new_bound_var : Forall_id.t -> type_
val new_int : unit -> type_
val new_arrow : param:type_ -> return:type_ -> type_
