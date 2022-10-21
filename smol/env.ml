module Name_map = Map.Make (Name)

exception Unbound_variable of { var : Name.t }

type env = Term.t Name_map.t
type t = env

let enter var type_ env =
  let name = Var.name var in
  let env = Name_map.add name type_ env in
  env

let lookup name env =
  match Name_map.find_opt name env with
  | Some type_ -> type_
  | None -> raise (Unbound_variable { var = name })

let initial =
  let open Term in
  let env = Name_map.empty in
  let env = enter Var.type_ Term.t_type env in

  let debug_type =
    (* debug : (M : (A : Type, A)) -> ((A, _) = M; A) *)
    let var = Var.create (Name.make "M") in
    let param =
      let var = Var.create (Name.make "A") in
      let left = t_type in
      let right = t_var ~var ~type_:left in
      t_sigma ~var ~left ~right
    in
    let return =
      let left = Var.create (Name.make "A") in
      let right = Var.create (Name.make "_") in
      let pair = t_var ~var ~type_:param in
      let return = t_var ~var:left ~type_:t_type in
      t_unpair ~left ~right ~pair ~return
    in
    t_arrow ~var ~param ~return
  in
  let debug_term = t_var ~var:Var.debug ~type_:debug_type in
  enter Var.debug debug_term env
