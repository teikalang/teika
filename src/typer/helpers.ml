open Type

let rec in_type ~var type_ =
  if same var type_ then true else in_type_desc ~var type_

and in_type_desc ~var type_ =
  let in_type type_ = in_type ~var type_ in
  match desc type_ with
  | T_var _ -> false
  | T_forall { forall = _; return } -> in_type return
  | T_arrow { param; return } -> in_type param || in_type return
  | T_record { fields } ->
      List.exists
        (fun field ->
          let (T_field { forall = _; name = _; type_ }) = field in
          in_type type_)
        fields
  | T_type type_ -> in_type type_

let in_vars ~var vars = List.exists (fun var' -> same var var') vars

let in_foralls ~forall foralls =
  List.exists (fun forall' -> Forall.same forall forall') foralls

(* TODO: this is a bad name, this is actul*)
let rec free_vars foralls vars type_ =
  match desc type_ with
  | T_var (Weak _) ->
      if not (in_vars ~var:type_ vars) then type_ :: vars else vars
  | T_var (Bound { forall }) ->
      if (not (in_foralls ~forall foralls)) && not (in_vars ~var:type_ vars)
      then type_ :: vars
      else vars
  | T_forall { forall; return } -> free_vars (forall :: foralls) vars return
  | T_arrow { param; return } ->
      let vars = free_vars foralls vars param in
      free_vars foralls vars return
  | T_record { fields } ->
      List.fold_left
        (fun vars field ->
          let (T_field { forall; name = _; type_ }) = field in
          let foralls = forall :: foralls in
          free_vars foralls vars type_)
        vars fields
  | T_type type_ -> free_vars foralls vars type_

let free_vars type_ = free_vars [] [] type_

let forall_vars ~forall type_ =
  List.filter
    (fun var ->
      match desc var with
      | T_var (Weak _) -> false
      | T_var (Bound { forall = var_forall }) -> Forall.same forall var_forall
      | _ -> assert false)
    (free_vars type_)

let weak_vars type_ =
  List.filter
    (fun var ->
      match desc var with
      | T_var (Weak _) -> true
      | T_var (Bound _) -> false
      | _ -> assert false)
    (free_vars type_)
