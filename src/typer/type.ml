module Forall_id = Uid
module Lambda_id = Uid

type type_ = {
  (* TODO: this id is for debugging, can be removed with proper debugging tools *)
  id : Uid.t;
  mutable desc : desc;
}

and desc =
  | T_forall of { forall : Forall_id.t; body : type_ }
  | T_weak_var
  (* TODO: should we have this name here? It's duplicated from Tree.t *)
  (* TODO: check name across codebase *)
  | T_bound_var of { forall : Forall_id.t; name : Name.t option }
  | T_arrow of { param : type_; return : type_ }
  | T_link of type_
[@@deriving show]

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
let new_weak_var () = new_type T_weak_var
let new_bound_var ~name forall = new_type (T_bound_var { forall; name })
let new_arrow ~param ~return = new_type (T_arrow { param; return })
