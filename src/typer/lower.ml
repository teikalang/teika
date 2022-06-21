open Type

let rec lower ~to_ type_ =
  let lower type_ = lower ~to_ type_ in

  match desc type_ with
  | T_forall { forall = _; return } -> lower return
  | T_var (Weak { forall } | Bound { forall }) ->
      let to_rank = Forall.rank to_ in
      let var_rank = Forall.rank forall in
      if Rank.(to_rank < var_rank) then lower_var ~to_ type_ else ()
  | T_arrow { param; return } ->
      lower param;
      lower return
  | T_record { fields } ->
      List.iter
        (fun field ->
          let (T_field { forall = _; name = _; type_ }) = field in
          lower type_)
        fields
  | T_type type_ -> lower type_
