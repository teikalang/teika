open Repr

let rec in_type ~var type_ =
  if same var type_ then true else in_type_desc ~var type_

and in_type_desc ~var type_ =
  let in_type type_ = in_type ~var type_ in
  match desc type_ with
  | T_var _ -> false
  | T_forall { forall = _; body } -> in_type body
  | T_arrow { param; return } -> in_type param || in_type return

let in_vars ~var vars = List.exists (fun var' -> same var var') vars

let in_foralls ~forall foralls =
  List.exists (fun forall' -> Forall_id.equal forall forall') foralls

(* TODO: this is a bad name, this is actul*)
let rec free_vars foralls vars type_ =
  match desc type_ with
  | T_var (Weak _) ->
      if not (in_vars ~var:type_ vars) then type_ :: vars else vars
  | T_var (Bound { forall; name = _ }) ->
      if (not (in_foralls ~forall foralls)) && not (in_vars ~var:type_ vars)
      then type_ :: vars
      else vars
  | T_forall { forall; body } -> free_vars (forall :: foralls) vars body
  | T_arrow { param; return } ->
      let vars = free_vars foralls vars param in
      free_vars foralls vars return

let free_vars type_ = free_vars [] [] type_

let forall_vars ~forall type_ =
  List.filter
    (fun var ->
      match desc var with
      | T_var (Weak _) -> false
      | T_var (Bound { forall = var_forall; name = _ }) ->
          Forall_id.equal forall var_forall
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
