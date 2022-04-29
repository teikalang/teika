open Utils
open Type

type error = Unknown_name of { name : Name.t }

exception Error of { loc : Location.t; error : error }

let raise loc error = raise (Error { loc; error })

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
  current_rank : Rank.t;
}

let insert decl t =
  let { names; foralls; current_rank } = t in
  let ident = decl.ident in
  let name = Ident.name ident in
  let names = Name.Map.add name decl names in
  { names; foralls; current_rank }

let add loc name type_ t =
  let ident = Ident.make name in
  let decl = { ident; ident_type = type_; ident_loc = loc } in

  let t = insert decl t in
  (ident, t)

let lookup loc name t =
  let { names; foralls = _; current_rank = _ } = t in
  match Name.Map.find_opt name names with
  | Some decl ->
      (* TODO: use loc for something? *)
      let { ident; ident_type; ident_loc = _ } = decl in
      (ident, ident_type)
  | None -> raise loc (Unknown_name { name })

(* rank *)
let current_rank t =
  let { names = _; foralls = _; current_rank } = t in
  current_rank

let enter_rank t =
  let { names; foralls; current_rank } = t in
  let current_rank = Rank.next current_rank in
  { names; foralls; current_rank }

let enter_forall ~forall t =
  (* TODO: does it make sense to always increase rank here? *)
  (* no bound var at initial env rank? *)
  let t = enter_rank t in

  let { names; foralls; current_rank } = t in
  (* TODO: what should happen on duplicated forall? *)
  let foralls = Forall_id.Map.add forall current_rank foralls in
  { names; foralls; current_rank }

let find_forall ~forall t =
  let { names = _; foralls; current_rank = _ } = t in
  Forall_id.Map.find_opt forall foralls

let new_weak_var env =
  let rank = current_rank env in
  new_weak_var rank

(* base *)
let base, int_type, int_ident =
  (* TODO: what goes here for predef?
       - maybe points to stdlib code when available *)
  let loc = Location.none in

  let base =
    {
      names = Name.Map.empty;
      foralls = Forall_id.Map.empty;
      current_rank = Rank.initial;
    }
  in

  let forall = Forall_id.next () in
  let base = enter_forall ~forall base in

  let dot_name = Name.make "." in
  let new_dot_struct type_ =
    new_struct ~fields:[ { name = dot_name; type_ } ]
  in

  let int_name = Name.make "Int" in
  let int_type = new_bound_var ~name:(Some int_name) forall in
  let int_ident, base = base |> add loc int_name (new_dot_struct int_type) in

  (base, int_type, int_ident)
