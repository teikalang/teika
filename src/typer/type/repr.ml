open Utils
module Forall_id = Uid
module Lambda_id = Uid

type type_ =
  | T_forall of { forall : Forall_id.t; body : type_ }
  | T_var of var
  | T_arrow of { param : type_; return : type_ }

and var =
  (* when link points to type_ itself, then this is not linked *)
  | Weak of { rank : Rank.t; mutable link : type_ }
  (* TODO: should we have this name here? It's duplicated from Tree.t *)
  (* TODO: check name across codebase *)
  | Bound of { forall : Forall_id.t; name : Name.t option }

type t = type_

(* externally not a link *)
type desc = type_ =
  | T_forall of { forall : Forall_id.t; body : type_ }
  | T_var of var
  | T_arrow of { param : type_; return : type_ }

(* externally opaque *)
type link = type_

let rec repr type_ =
  (* TODO: path compression *)
  match type_ with
  | T_var (Weak { rank = _; link }) ->
      if link == type_ then type_ else repr link
  | _ -> type_

let same a b = repr a == repr b
let desc type_ = repr type_

let link ~to_ type_ =
  let type_ = repr type_ in
  match type_ with T_var (Weak weak) -> weak.link <- to_ | _ -> assert false

let new_forall forall ~body = T_forall { forall; body }

let new_weak_var rank =
  let rec var = T_var (Weak { rank; link = var }) in
  var

let new_bound_var ~name forall = T_var (Bound { forall; name })
let new_arrow ~param ~return = T_arrow { param; return }
