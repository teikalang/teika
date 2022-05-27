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
  foralls : Rank.t Forall_id.Map.t;
  current_forall : Forall_id.t * Rank.t;
  current_loc : Location.t;
}

(* loc *)
let set_loc loc t =
  let { names; foralls; current_forall; current_loc = _ } = t in
  { names; foralls; current_forall; current_loc = loc }

let current_loc t =
  let { names = _; foralls = _; current_forall = _; current_loc } = t in
  current_loc

let raise t error = raise (Error { loc = current_loc t; error })

let insert decl t =
  let { names; foralls; current_forall; current_loc } = t in
  let ident = decl.ident in
  let name = Ident.name ident in
  let names = Name.Map.add name decl names in
  { names; foralls; current_forall; current_loc }

let add name type_ t =
  let loc = current_loc t in
  let ident = Ident.make name in
  let decl = { ident; ident_type = type_; ident_loc = loc } in

  let t = insert decl t in
  (ident, t)

let lookup name t =
  let { names; foralls = _; current_forall = _; current_loc = _ } = t in
  match Name.Map.find_opt name names with
  | Some decl ->
      (* TODO: use loc for something? *)
      let { ident; ident_type; ident_loc = _ } = decl in
      (ident, ident_type)
  | None -> raise t (Unknown_name { name })

(* forall *)
let current_forall t =
  let { names = _; foralls = _; current_forall; current_loc = _ } = t in
  fst current_forall

let current_rank t =
  let { names = _; foralls = _; current_forall; current_loc = _ } = t in
  snd current_forall

let enter_forall ~forall t =
  (* TODO: does it make sense to always increase rank here? *)
  (* no bound var at initial env rank? *)
  let { names; foralls; current_forall; current_loc } = t in
  let current_rank = Rank.next (snd current_forall) in
  (* TODO: what should happen on duplicated forall? *)
  let foralls = Forall_id.Map.add forall current_rank foralls in
  let current_forall = (forall, current_rank) in
  { names; foralls; current_forall; current_loc }

let find_forall ~forall t =
  let { names = _; foralls; current_forall = _; current_loc = _ } = t in
  Forall_id.Map.find_opt forall foralls

let new_weak_var env =
  let rank = current_rank env in
  new_weak_var rank

(* base *)
let base, int_type, int_ident =
  let base =
    {
      names = Name.Map.empty;
      foralls = Forall_id.Map.empty;
      current_forall = (Forall_id.next (), Rank.initial);
      (* TODO: what goes here for predef?
         - maybe points to stdlib code when available *)
      current_loc = Location.none;
    }
  in

  let forall = Forall_id.next () in
  let base = enter_forall ~forall base in

  let int_name = Name.make "Int" in
  let int_type = new_bound_var ~name:(Some int_name) forall in
  let int_ident, base =
    base |> add int_name (new_type (Forall_id.next ()) ~type_:int_type)
  in

  (base, int_type, int_ident)
