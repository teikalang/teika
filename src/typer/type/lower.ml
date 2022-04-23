open Repr

let lower ~var rank =
  match desc var with
  | T_var (Weak _) ->
      let var' = new_type (T_var (Weak rank)) in
      link var ~to_:var'
  | _ -> assert false
