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
  | T_forall { forall; return } ->
      let forall' = Forall.generic () in
      foralls := (forall, forall') :: !foralls;

      let return = instance return in
      new_forall forall' ~return
  | T_var (Weak _) -> (* weak not copied *) type_
  | T_var (Bound { forall = var_forall }) -> (
      if Forall.same forall var_forall then
        let forall = current_forall env in
        if bound_when_free then new_bound_var forall else new_weak_var forall
      else
        match
          List.find_opt
            (fun (key, _forall') -> Forall.same key var_forall)
            !foralls
        with
        | Some (_key, forall') -> new_bound_var forall'
        | None ->
            (* TODO: isn't this breaking an invariant? *)
            type_)
  | T_arrow { param; return } ->
      let param = instance param in
      let return = instance return in
      new_arrow ~param ~return
  (* TODO: should this type_ be also copied? *)
  | T_record { fields } ->
      let fields =
        List.map
          (fun field ->
            let (T_field { forall; name; type_ }) = field in

            let forall =
              let forall' = Forall.generic () in
              foralls := (forall, forall') :: !foralls;
              forall'
            in

            let type_ = instance type_ in
            new_field ~forall ~name ~type_)
          fields
      in
      new_record ~fields
  | T_type type_ ->
      let type_ = instance type_ in
      new_type ~type_

(* TODO: those functions are badly named *)
let instance env ~bound_when_free ~forall body =
  instance env ~bound_when_free ~forall (ref []) (ref []) body

let instance_bound env ~forall body =
  instance env ~bound_when_free:true ~forall body

let instance_weaken env ~forall body =
  instance env ~bound_when_free:false ~forall body
