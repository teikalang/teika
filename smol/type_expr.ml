open Ltree
open Type
open Ttree
open Machinery
open Transl_type
open Env

let rec type_expr env expr =
  (* TODO: use location *)
  let (LE { loc = _; desc = expr }) = expr in
  match expr with
  | LE_var { var } ->
      let var, type_ = lookup var env in
      te_var type_ ~var
  | LE_lambda { var; param; return } ->
      let var = Var.create var in
      let param = transl_type env param in
      let env =
        let (TT { type_ = param; desc = _ }) = param in
        enter var param env
      in
      let return = type_expr env return in
      te_lambda ~var ~param ~return
  | LE_forall { var; return } ->
      let var = Var.create var in
      let env =
        let param = t_alias ~type_:(t_var ~var) in
        enter var param env
      in
      let return = type_expr env return in
      te_forall ~var ~return
  | LE_apply { funct; arg } ->
      let funct = type_expr env funct in
      let arg = type_expr env arg in
      let type_ =
        let (TE { type_ = funct; desc = _ }) = funct in
        let (TE { type_ = arg; desc = _ }) = arg in
        apply ~funct ~arg
      in
      te_apply type_ ~funct ~arg
  | LE_pair { left; right } ->
      let env, left = type_bind env left in
      let _env, right = type_bind env right in
      te_pair ~left ~right
  | LE_exists { var; right } ->
      let var = Var.create var in
      let env =
        let left = t_alias ~type_:(t_var ~var) in
        enter var left env
      in
      let _env, right = type_bind env right in
      te_exists ~var ~right
  | LE_unpair { unpair; return } ->
      let env, unpair = type_unpair env unpair in
      let return = type_expr env return in
      te_unpair ~unpair ~return
  | LE_type { type_ } ->
      let type_ = transl_type env type_ in
      te_type ~type_
  (* TODO: maybe let and annot should be translated to lambdas? *)
  | LE_let { bind; return } ->
      let env, bind = type_bind env bind in
      let return = type_expr env return in
      te_let ~bind ~return
  | LE_annot { expr; type_ } ->
      let expr = type_expr env expr in
      let type_ = transl_type env type_ in
      let () =
        let (TE { type_ = expr; desc = _ }) = expr in
        let (TT { type_; desc = _ }) = type_ in
        subtype ~expected:type_ ~received:expr
      in
      te_annot ~expr ~type_

and type_unpair env unpair =
  (* TODO: use location *)
  let (LU { loc = _; left; right; value }) = unpair in
  let left = Var.create left in
  let right = Var.create right in
  let value = type_expr env value in

  let env =
    let (TE { type_ = value; desc = _ }) = value in
    (* TODO: name clash *)
    let left_type, right_type = Machinery.unpair ~pair:value in
    let env = enter left left_type env in
    let env = enter right right_type env in
    env
  in
  let unpair = tu ~left ~right ~value in
  (env, unpair)

and type_bind env bind =
  (* TODO: use location *)
  let (LB { loc = _; var; value }) = bind in
  let var = Var.create var in
  let value = type_expr env value in
  let env =
    let (TE { type_ = value; desc = _ }) = value in
    enter var value env
  in
  let bind = tb ~var ~value in
  (env, bind)
