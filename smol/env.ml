module Name_map = Map.Make (Name)

exception Unbound_variable of { var : Name.t }

type env = (Var.t * Type.t) Name_map.t
type t = env

let empty = Name_map.empty

let enter var type_ env =
  let name = Var.name var in
  let env = Name_map.add name (var, type_) env in
  env

let lookup name env =
  match Name_map.find_opt name env with
  | Some (var, type_) -> (var, type_)
  | None -> raise (Unbound_variable { var = name })
