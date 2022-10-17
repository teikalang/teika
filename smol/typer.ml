open Ltree
open Env
open Term
open Machinery

let enter ~var ~type_ env =
  (* TODO: this is non ideal *)
  let value = t_var ~var ~type_ in
  enter var value env

let rec type_term env term =
  (* TODO: use location *)
  let (LT { loc = _; desc = term }) = term in
  match term with
  | LT_type -> t_type
  | LT_var { var } -> lookup var env
  | LT_arrow { var; param; return } ->
      let var = Var.create var in
      let param = type_term env param in
      let return =
        let env = enter ~var ~type_:param env in
        type_term env return
      in
      t_arrow ~var ~param ~return
  | LT_lambda { var; param; return } ->
      let var = Var.create var in
      let param = type_term env param in
      let return =
        let env = enter ~var ~type_:param env in
        type_term env return
      in
      t_lambda ~var ~param ~return
  | LT_apply { lambda; arg } ->
      let lambda = type_term env lambda in
      let arg = type_term env arg in
      let () = apply ~lambda ~arg in
      t_apply ~lambda ~arg
  | LT_sigma { var; left; right } ->
      let var = Var.create var in
      let left = type_term env left in
      let right =
        let env = enter ~var ~type_:left env in
        type_term env right
      in
      t_sigma ~var ~left ~right
  | LT_pair { var; left; right; annot } ->
      let var = Var.create var in
      let left = type_term env left in
      let right = type_term env right in
      let annot =
        let left = typeof left in
        let env = enter ~var ~type_:left env in
        type_term env annot
      in
      let () = pair ~var ~left ~right ~annot in
      t_pair ~var ~left ~right ~annot
  | LT_unpair { left; right; pair; return } ->
      let left = Var.create left in
      let right = Var.create right in
      let pair = type_term env pair in
      let return =
        let left_value, right_value = unpair ~left ~pair in
        let env = enter ~var:left ~type_:left_value env in
        let env = enter ~var:right ~type_:right_value env in
        type_term env return
      in
      t_unpair ~left ~right ~pair ~return
  | LT_let { var; value; return } ->
      let var = Var.create var in
      let value = type_term env value in
      let param = typeof value in
      let return =
        let env = enter ~var ~type_:param env in
        type_term env return
      in
      let lambda = t_lambda ~var ~param ~return in
      t_apply ~lambda ~arg:value
  | LT_annot { value; type_ } ->
      let value = type_term env value in
      let type_ = type_term env type_ in
      let () = annot ~value ~type_ in
      (* TODO: why is this erased? *)
      value
