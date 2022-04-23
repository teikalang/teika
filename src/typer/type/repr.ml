open Utils
module Forall_id = Uid
module Lambda_id = Uid

type type_ = {
  (* TODO: this id is for debugging, can be removed with proper debugging tools *)
  id : Uid.t;
  mutable desc : internal_desc;
}

and internal_desc =
  | D_forall of { forall : Forall_id.t; body : type_ }
  | D_var of var
  | D_arrow of { param : type_; return : type_ }
  | D_link of type_
[@@deriving show]

and var =
  | Weak of Rank.t
  (* TODO: should we have this name here? It's duplicated from Tree.t *)
  (* TODO: check name across codebase *)
  | Bound of { forall : Forall_id.t; name : Name.t option }

type desc =
  | T_forall of { forall : Forall_id.t; body : type_ }
  | T_var of var
  | T_arrow of { param : type_; return : type_ }
[@@deriving show]

type t = type_ [@@deriving show]

let rec repr type_ =
  match type_.desc with D_link type_ -> repr type_ | _ -> type_

let same a b = repr a == repr b

let rec desc type_ =
  match type_.desc with
  | D_forall { forall; body } -> T_forall { forall; body }
  | D_var var -> T_var var
  | D_arrow { param; return } -> T_arrow { param; return }
  | D_link type_ -> desc type_

let link ~to_ type_ =
  let type_ = repr type_ in
  type_.desc <- D_link to_

(* constructors *)
let new_type desc = { id = Uid.next (); desc }
let new_forall forall ~body = new_type (D_forall { forall; body })
let new_var var = new_type (D_var var)
let new_weak_var rank = new_var (Weak rank)
let new_bound_var ~name forall = new_var (Bound { forall; name })
let new_arrow ~param ~return = new_type (D_arrow { param; return })

(* rank *)
let lower ~var rank =
  match desc var with
  | T_var (Weak _) ->
      let var' = new_weak_var rank in
      link var ~to_:var'
  | _ -> assert false
