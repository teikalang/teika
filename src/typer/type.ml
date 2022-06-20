open Utils

type type_ =
  | T_forall of { forall : Forall.t; body : type_ }
  | T_var of { mutable var : type_var }
  | T_arrow of { param : type_; return : type_ }
  | T_record of { fields : field list }
  | T_type of { forall : Forall.t; type_ : type_ }

and type_var =
  (* when link points to type_ itself, then this is not linked *)
  (* TODO: weak may be bound if forall is bound *)
  | Weak of { mutable forall : Forall.t; mutable link : type_ }
  (* TODO: should we have this name here? It's duplicated from Tree.t *)
  (* TODO: check name across codebase *)
  (* TODO: rename bound to rigid *)
  | Bound of { mutable forall : Forall.t }

and field = { name : Name.t; type_ : type_ }

type t = type_

type desc =
  | T_forall of { forall : Forall.t; body : type_ }
  | T_var of var
  | T_arrow of { param : type_; return : type_ }
  | T_record of { fields : field list }
  | T_type of { forall : Forall.t; type_ : type_ }
[@@ocaml.warning "-unused-constructor"]

and var =
  | Weak of { mutable forall : Forall.t }
  | Bound of { mutable forall : Forall.t }
[@@ocaml.warning "-unused-constructor"]

(* externally opaque *)
type link = type_

let rec repr (type_ : type_) =
  (* TODO: path compression *)
  match type_ with
  | T_var ({ var = Weak { forall; link } } as var) ->
      if link == type_ then (
        (* fix outdated weak vars *)
        if Forall.is_generic forall then var.var <- Bound { forall };
        type_)
      else repr link
  | _ -> type_

let same a b = repr a == repr b
let desc type_ : desc = Obj.magic (repr type_)
let invariant x = assert x

let link ~to_ type_ =
  match repr type_ with
  | T_var { var = Weak weak } ->
      invariant (not (Forall.is_generic weak.forall));
      weak.link <- to_
  | _ -> assert false

let lower_var ~to_ type_ =
  let check ~from_forall =
    let from_rank = Forall.rank from_forall in
    let to_rank = Forall.rank to_ in

    invariant Rank.(to_rank < from_rank);
    invariant (not (Forall.is_generic from_forall))
  in
  match repr type_ with
  | T_var { var = Weak weak } ->
      check ~from_forall:weak.forall;
      weak.forall <- to_
  | T_var { var = Bound bound } ->
      check ~from_forall:bound.forall;
      bound.forall <- to_
  | _ -> assert false

let new_forall forall ~body : type_ = T_forall { forall; body }

let new_weak_var forall : type_ =
  let rec var : type_ = T_var { var = Weak { forall; link = var } } in
  var

let new_bound_var forall : type_ = T_var { var = Bound { forall } }
let new_arrow ~param ~return : type_ = T_arrow { param; return }
let new_record ~fields : type_ = T_record { fields }
let new_type forall ~type_ : type_ = T_type { forall; type_ }
