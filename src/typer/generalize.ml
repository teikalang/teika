open Type

(* TODO: value restriction? *)

let free_vars_in_env env type_ =
  let env_rank = Env.current_rank env in
  List.filter
    (fun var ->
      match desc var with
      | T_var (Weak { rank = var_rank; link = _ }) ->
          (* TODO: check all rank comparison *)
          Rank.(var_rank > env_rank)
      | _ -> assert false)
    (Helpers.weak_vars type_)

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
