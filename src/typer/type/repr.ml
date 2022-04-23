open Utils
module Forall_id = Uid
module Lambda_id = Uid

type type_ = { mutable desc : internal_desc }

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

let of_desc desc =
  match desc with
  | T_forall { forall; body } -> D_forall { forall; body }
  | T_var var -> D_var var
  | T_arrow { param; return } -> D_arrow { param; return }

let new_type desc =
  let desc = of_desc desc in
  { desc }

let with_type f =
  let temp = T_var (Weak Rank.initial) in
  let type_ = new_type temp in
  let desc = of_desc (f type_) in
  type_.desc <- desc;
  type_

let new_forall forall ~body = new_type (T_forall { forall; body })
let new_var var = new_type (T_var var)
let new_weak_var rank = new_var (Weak rank)
let new_bound_var ~name forall = new_var (Bound { forall; name })
let new_arrow ~param ~return = new_type (T_arrow { param; return })
