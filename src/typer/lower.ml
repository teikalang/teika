open Type

let lower ~var rank =
  (* TODO: should this check that rank is not increasing? *)
  match desc var with
  | T_var (Weak _) ->
      let var' = new_weak_var rank in
      link var ~to_:var'
  | _ -> assert false
