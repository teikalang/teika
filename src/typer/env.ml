type error = Unknown_name of { name : Name.t }

exception Error of { loc : Location.t; error : error }

let raise loc error = raise (Error { loc; error })

type ident_decl = {
  (* TODO: maybe we should use id everywhere? *)
  ident : Ident.t;
  ident_type : Type.t;
  ident_loc : Location.t; (* where it was declared *)
}

type t = { (* TODO: maybe current_loc here? *)
           names : ident_decl Name.Map.t }

let insert decl t =
  let { names } = t in
  let ident = decl.ident in
  let name = Ident.name ident in
  let names = Name.Map.add name decl names in
  { names }

let enter loc name type_ t =
  let ident = Ident.make name in
  let decl = { ident; ident_type = type_; ident_loc = loc } in

  let t = insert decl t in
  (ident, t)

let lookup loc name t =
  let { names } = t in
  match Name.Map.find_opt name names with
  | Some decl ->
      (* TODO: use loc for something? *)
      let { ident; ident_type; ident_loc = _ } = decl in
      (ident, ident_type)
  | None -> raise loc (Unknown_name { name })

(* TODO: this is a bad name *)
let types t =
  let { names } = t in
  Name.Map.fold (fun _ decl types -> decl.ident_type :: types) names []

(* base *)
let empty = { names = Name.Map.empty }

(* TODO: what goes here for predef?
   - maybe points to stdlib code when available *)
let loc = Location.none

open Type

let forall = Forall_id.next ()
let int_name = Name.make "Int"
let int_type = new_bound_var ~name:(Some int_name) forall
let int_ident, base = empty |> enter loc int_name int_type
