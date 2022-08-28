open Ltree
open Ttree
open Type
open Machinery

let enter var type_ env =
  let type_ = t_alias ~type_ in
  Env.enter var type_ env

let lookup var env =
  let var, wrapped = Env.lookup var env in
  let type_ = extract ~wrapped in
  (var, type_)

let rec transl_type env type_ =
  let (LT { loc = _; desc = type_ }) = type_ in
  match type_ with
  | LT_var { var } ->
      let var, type_ = lookup var env in
      tt_var type_ ~var
  | LT_arrow { param; return } ->
      let param = transl_type env param in
      let return = transl_type env return in
      tt_arrow ~param ~return
  | LT_forall { var; return } ->
      let var = Var.create var in
      let env =
        let type_ = t_var ~var in
        enter var type_ env
      in
      let return = transl_type env return in
      tt_forall ~var ~return
  | LT_pair { left; right } ->
      let left = transl_type env left in
      let right = transl_type env right in
      tt_pair ~left ~right
  | LT_exists { var; right } ->
      let var = Var.create var in
      let env =
        let type_ = t_var ~var in
        enter var type_ env
      in
      let right = transl_type env right in
      tt_exists ~var ~right
  | LT_alias { type_ } ->
      let type_ = transl_type env type_ in
      tt_alias ~type_
