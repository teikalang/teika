open Type
open Type_utils

(* TODO: value restriction? *)

let generalize vars body =
  (* TODO: same logic as Instance.weaken *)
  let forall = Forall_id.next () in
  List.iter
    (fun weak_var ->
      let bound_var = new_bound_var ~name:None forall in
      (* TODO: is link here okay?
              Also, why not just replace it by bound_var? *)
      link weak_var ~to_:bound_var)
    vars;
  new_forall forall ~body

let free_vars env type_ =
  let weak_vars = weak_vars type_ in
  let types = Env.types env in
  let in_types ~var = List.exists (fun type_ -> in_type ~var type_) types in

  List.filter (fun var -> not (in_types ~var)) weak_vars

let generalize env type_ =
  let vars = free_vars env type_ in
  (* TODO: vars not in the context *)
  match vars with [] -> type_ | vars -> generalize vars type_
