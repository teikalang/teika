open Type
open Env

(* TODO: this function is terrible *)
(* TODO: remove single forall *)
(* TODO: this makes perfect copy(except link), optimization if avoid generating
   duplicated generics, where weaken on unify can be O(1) *)
let rec instance env ~bound_when_free ~forall foralls types type_ =
  match List.find_opt (fun (key, _type') -> same key type_) !types with
  | Some (_key, type') -> type'
  | None ->
      let type' =
        instance_desc env ~bound_when_free ~forall foralls types type_
      in
      types := (type_, type') :: !types;
      type'

and instance_desc env ~bound_when_free ~forall foralls types type_ =
  let instance type_ =
    instance env ~bound_when_free ~forall foralls types type_
  in
  match desc type_ with
  | T_forall { forall; body } ->
      let forall' = Forall.make () in
      foralls := (forall, forall') :: !foralls;

      let body = instance body in
      new_forall forall' ~body
  | T_var (Weak _) -> (* weak not copied *) type_
  | T_var (Bound { forall = var_forall; name }) -> (
      if Forall.same forall var_forall then
        if bound_when_free then
          let forall = current_forall env in
          new_bound_var ~name forall
        else new_weak_var env
      else
        match
          List.find_opt
            (fun (key, _forall') -> Forall.same key var_forall)
            !foralls
        with
        | Some (_key, forall') -> new_bound_var ~name forall'
        | None ->
            (* TODO: isn't this breaking an invariant? *)
            type_)
  | T_arrow { param; return } ->
      let param = instance param in
      let return = instance return in
      new_arrow ~param ~return
  (* TODO: should this type_ be also copied? *)
  | T_struct { fields } ->
      let fields =
        List.map
          (fun { name; type_ } ->
            let type_ = instance type_ in
            { name; type_ })
          fields
      in
      new_struct ~fields
  | T_type { forall; type_ } ->
      let forall' = Forall.make () in
      foralls := (forall, forall') :: !foralls;

      let type_ = instance type_ in
      new_type forall' ~type_

(* TODO: those functions are badly named *)
let instance env ~bound_when_free ~forall body =
  instance env ~bound_when_free ~forall (ref []) (ref []) body

let instance_bound env ~forall body =
  instance env ~bound_when_free:true ~forall body

let instance_weaken env ~forall body =
  instance env ~bound_when_free:false ~forall body
