type error = Unknown_name of { name : Name.t }

exception Error of { loc : Location.t; error : error }

let raise loc error = raise (Error { loc; error })

type ident_decl = {
  (* TODO: maybe we should use id everywhere? *)
  ident : Ident.t;
  id_type : Type.t;
  (* where it was declared *)
  id_loc : Location.t;
}

type t = { (* TODO: maybe current_loc here? *)
           names : ident_decl Name.Map.t }

let empty = { names = Name.Map.empty }

let insert decl t =
  let { names } = t in
  let ident = decl.ident in
  let name = Ident.name ident in
  let names = Name.Map.add name decl names in
  { names }

let enter loc name type_ t =
  let ident = Ident.make name in
  let decl = { ident; id_type = type_; id_loc = loc } in

  let t = insert decl t in
  (ident, t)

let lookup loc name t =
  let { names } = t in
  match Name.Map.find_opt name names with
  | Some decl ->
      let { ident; id_type = type_; id_loc = _ } = decl in
      (ident, type_)
  | None -> raise loc (Unknown_name { name })

let types t =
  let { names } = t in
  Name.Map.fold (fun _ decl types -> decl.id_type :: types) names []
