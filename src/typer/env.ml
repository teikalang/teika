open Utils
open Type

type error = Unknown_name of { name : Name.t }

exception Error of { loc : Location.t; error : error }

type ident_decl = {
  (* TODO: maybe we should use id everywhere? *)
  ident : Ident.t;
  ident_type : Type.t;
  ident_loc : Location.t; (* where it was declared *)
}

type t = {
  (* TODO: maybe current_loc here? *)
  names : ident_decl Name.Map.t;
  current_forall : Forall.t;
  current_rank : Rank.t;
  current_loc : Location.t;
}

(* loc *)
let set_loc loc t =
  let { names; current_forall; current_rank; current_loc = _ } = t in
  { names; current_forall; current_rank; current_loc = loc }

let current_loc t =
  let { names = _; current_forall = _; current_rank = _; current_loc } = t in
  current_loc

let raise t error = raise (Error { loc = current_loc t; error })

let insert decl t =
  let { names; current_forall; current_rank; current_loc } = t in
  let ident = decl.ident in
  let name = Ident.name ident in
  let names = Name.Map.add name decl names in
  { names; current_forall; current_rank; current_loc }

let add name type_ t =
  let loc = current_loc t in
  let ident = Ident.make name in
  let decl = { ident; ident_type = type_; ident_loc = loc } in

  let t = insert decl t in
  (ident, t)

let lookup name t =
  let { names; current_forall = _; current_rank = _; current_loc = _ } = t in
  match Name.Map.find_opt name names with
  | Some decl ->
      (* TODO: use loc for something? *)
      let { ident; ident_type; ident_loc = _ } = decl in
      (ident, ident_type)
  | None -> raise t (Unknown_name { name })

(* forall *)
(* TODO: invariant to keep, current_forall is never bound *)
let current_rank t =
  let { names = _; current_forall = _; current_rank; current_loc = _ } = t in
  current_rank

let with_forall current_forall t =
  let { names; current_forall = _; current_rank; current_loc } = t in
  { names; current_forall; current_rank; current_loc }

let current_forall t =
  let { names = _; current_forall; current_rank = _; current_loc = _ } = t in
  current_forall

let enter_forall t =
  let { names; current_forall = _; current_rank = previous_rank; current_loc } =
    t
  in
  let current_rank = Rank.next previous_rank in
  let current_forall = Forall.weak current_rank in
  (current_forall, { names; current_forall; current_rank; current_loc })

let new_weak_var env =
  let forall = current_forall env in
  new_weak_var forall

(* base *)
let base, int_type, int_ident =
  let predef_rank = Rank.initial in
  let predef_forall = Forall.weak predef_rank in
  Forall.existential predef_forall;

  let predef =
    {
      names = Name.Map.empty;
      current_forall = predef_forall;
      current_rank = predef_rank;
      (* TODO: what goes here for predef?
         - maybe points to stdlib code when available *)
      current_loc = Location.none;
    }
  in

  let int_name = Name.make "Int" in
  let int_type = new_bound_var ~name:(Some int_name) predef_forall in
  let int_ident, predef =
    predef |> add int_name (new_type (Forall.generic ()) ~type_:int_type)
  in

  let _forall, env = enter_forall predef in
  (env, int_type, int_ident)
