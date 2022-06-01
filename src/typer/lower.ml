open Type

let rec lower ~to_ type_ =
  let lower type_ = lower ~to_ type_ in

  match desc type_ with
  | T_forall { forall = _; body } -> lower body
  | T_var (Weak { forall } | Bound { forall; name = _ }) ->
      let to_rank = Forall.rank to_ in
      let var_rank = Forall.rank forall in
      if Rank.(to_rank < var_rank) then lower_var ~to_ type_ else ()
  | T_arrow { param; return } ->
      lower param;
      lower return
  | T_struct { fields } ->
      List.iter (fun { name = _; type_ } -> lower type_) fields
  | T_type { forall = _; type_ } -> lower type_
