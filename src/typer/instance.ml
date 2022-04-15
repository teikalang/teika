open Type

(* TODO: this function is terrible *)
(* TODO: remove single forall *)
(* TODO: this makes perfect copy(except link), optimization if avoid generating
   duplicated generics, where weaken on unify can be O(1) *)
let rec instance ~forall foralls types weak_vars type_ =
  match List.find_opt (fun (key, _type') -> same key type_) !types with
  | Some (_key, type') -> type'
  | None ->
      let type' = instance_desc ~forall foralls types weak_vars type_ in
      types := (type_, type') :: !types;
      type'

and instance_desc ~forall foralls types weak_vars type_ =
  let instance type_ = instance ~forall foralls types weak_vars type_ in
  match desc type_ with
  | T_forall { forall; body } ->
      let forall' = Forall_id.next () in
      foralls := (forall, forall') :: !foralls;

      let body = instance body in
      new_forall forall' ~body
  | T_weak_var -> (* eak not copied *) type_
  | T_bound_var { forall = var_forall } -> (
      if Forall_id.equal forall var_forall then (
        let weak_var = new_weak_var () in
        weak_vars := weak_var :: !weak_vars;
        weak_var)
      else
        match
          List.find_opt
            (fun (key, _forall') -> Forall_id.equal key var_forall)
            !foralls
        with
        | Some (_key, forall') -> new_bound_var forall'
        | None -> (* TODO: isn't this breaking an invariant? *) type_)
  | T_int -> new_int ()
  | T_arrow { param; return } ->
      let param = instance param in
      let return = instance return in
      new_arrow ~param ~return
  | _ -> assert false

let instance ~forall body =
  let weak_vars = ref [] in
  let body = instance ~forall (ref []) (ref []) weak_vars body in
  (body, !weak_vars)
