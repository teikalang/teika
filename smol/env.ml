module Name_map = Map.Make (Name)

exception Unbound_variable of { var : Name.t }

type env = Term.t Name_map.t
type t = env

let empty = Name_map.empty

let enter var type_ env =
  let name = Var.name var in
  let env = Name_map.add name type_ env in
  env

let lookup name env =
  match Name_map.find_opt name env with
  | Some type_ -> type_
  | None -> raise (Unbound_variable { var = name })
