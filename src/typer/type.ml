open Utils
module Forall_id = Uid
module Lambda_id = Uid

type type_ = {
  (* TODO: this id is for debugging, can be removed with proper debugging tools *)
  id : Uid.t;
  mutable desc : desc;
}

and desc =
  | T_forall of { forall : Forall_id.t; body : type_ }
  | T_var of var
  | T_arrow of { param : type_; return : type_ }
  | T_link of type_
[@@deriving show]

and var =
  | Weak of Rank.t
  (* TODO: should we have this name here? It's duplicated from Tree.t *)
  (* TODO: check name across codebase *)
  | Bound of { forall : Forall_id.t; name : Name.t option }

type t = type_ [@@deriving show]

let rec repr type_ =
  match type_.desc with T_link type_ -> repr type_ | _ -> type_

let same a b = repr a == repr b
let desc type_ = (repr type_).desc

let link ~to_ type_ =
  let type_ = repr type_ in
  type_.desc <- T_link to_

(* constructors *)
let new_type desc = { id = Uid.next (); desc }
let new_forall forall ~body = new_type (T_forall { forall; body })
let new_var var = new_type (T_var var)
let new_weak_var rank = new_var (Weak rank)
let new_bound_var ~name forall = new_var (Bound { forall; name })
let new_arrow ~param ~return = new_type (T_arrow { param; return })

(* rank *)
let lower ~var rank =
  match desc var with
  | T_var (Weak _) ->
      let var' = new_weak_var rank in
      link var ~to_:var'
  | _ -> assert false
