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

let generalize env type_ =
  let vars = free_vars_in_env env type_ in
  (* TODO: vars not in the context *)
  match vars with [] -> type_ | vars -> generalize vars type_
