open Ltree
open Ttree
open Context
open Typer_context
open Tmachinery
open Unify

let escape_check_term term = with_var_context @@ fun () -> tt_escape_check term

let unify_term ~expected ~received =
  with_unify_context @@ fun () -> tt_unify ~expected ~received

let tt_open_level term =
  let* to_ = level () in
  let to_ = TT_free_var { level = to_ } in
  pure @@ tt_open term ~to_

let tt_close_level term =
  let* from = level () in
  pure @@ tt_close term ~from

let split_forall type_ =
  (* TODO: optimization, avoid the holes when annotated *)
  let param_type = tt_hole () in
  let param = TPat { pat = tp_hole (); type_ = param_type } in
  let return = tt_hole () in
  let expected = TT_forall { param; return } in
  let* () = unify_term ~received:type_ ~expected in
  pure (param_type, return)

let split_self type_ =
  let var = tp_hole () in
  let body = tt_hole () in
  let expected = TT_self { var; body } in
  let* () = unify_term ~received:type_ ~expected in
  pure body

(* TODO: does having expected_term also improves inference?
     Maybe with self and fix? But maybe not worth it
   Seems to help with many cases such as expected on annotation *)
let rec check_term term ~expected =
  (* TODO: propagation through dependent things *)
  let wrapped term =
    let* () = escape_check_term expected in
    pure @@ term
  in
  match term with
  | LT_var { var = name } ->
      let* level, received = lookup_var ~name in
      let* () = unify_term ~received ~expected in
      wrapped @@ TT_free_var { level }
  | LT_extension { extension; payload } ->
      check_term_extension ~extension ~payload ~expected
  | LT_forall { param; return } ->
      (* TODO: this could in theory be improved by expected term *)
      (* TODO: this could also be checked after the return *)
      let* () = unify_term ~received:tt_type ~expected in
      let param_type = tt_hole () in
      let* param, return =
        check_typed_pat param ~expected:param_type ~alias:None @@ fun param ->
        let* return = check_annot return in
        let* return = tt_close_level return in
        pure (param, return)
      in
      wrapped @@ TT_forall { param; return }
  | LT_lambda { param; return } ->
      (* TODO: maybe unify param? *)
      let* param_type, return_type = split_forall expected in
      let* param, return =
        check_typed_pat param ~expected:param_type ~alias:None @@ fun param ->
        let* return =
          let* return_type = tt_open_level return_type in
          check_term return ~expected:return_type
        in
        let* return = tt_close_level return in
        pure (param, return)
      in
      wrapped @@ TT_lambda { param; return }
  | LT_apply { lambda; arg } ->
      let lambda_type = tt_hole () in
      let* lambda = check_term lambda ~expected:lambda_type in
      (* TODO: this could be better? avoiding split forall? *)
      let* arg_type, return_type = split_forall lambda_type in
      let* arg = check_term arg ~expected:arg_type in
      let* () =
        let received = tt_open return_type ~to_:arg in
        unify_term ~received ~expected
      in
      wrapped @@ TT_apply { lambda; arg }
  | LT_exists _ -> error_pairs_not_implemented ()
  | LT_pair _ -> error_pairs_not_implemented ()
  | LT_let { bound; return } ->
      (* TODO: use this loc *)
      let (LBind { loc = _; pat; value }) = bound in
      let value_type = tt_hole () in
      let* value = check_term value ~expected:value_type in
      (* TODO: type pattern first? *)
      let* bound, return_type, return =
        check_typed_pat pat ~expected:value_type ~alias:(Some value)
        @@ fun bound ->
        let return_type = tt_hole () in
        let* return = check_term return ~expected:return_type in
        let* return_type = tt_close_level return_type in
        let* return = tt_close_level return in
        pure (bound, return_type, return)
      in
      let* () =
        let received = tt_open return_type ~to_:value in
        unify_term ~received ~expected
      in
      wrapped @@ TT_let { bound; value; return }
  | LT_annot { term; annot } ->
      (* TODO: expected term could propagate here *)
      let* annot = check_annot annot in
      let* () = unify_term ~received:annot ~expected in
      (* TODO: unify annot before or after check term *)
      let* term = check_term term ~expected:annot in
      wrapped @@ TT_annot { term; annot }
  | LT_string { literal } ->
      let* () = unify_term ~expected ~received:string_type in
      wrapped @@ TT_string { literal }
  | LT_loc { term; loc = _ } ->
      (* TODO: use loc *)
      check_term term ~expected

and check_term_extension ~extension ~payload ~expected =
  let wrapped term =
    let* () = escape_check_term expected in
    pure @@ term
  in
  match (Name.repr extension, payload) with
  | _, LT_loc { term = payload; loc = _ } ->
      (* TODO: use location *)
      check_term_extension ~extension ~payload ~expected
  | "@self", LT_forall { param = self; return = body } ->
      (* TODO: this could in theory be improved by expected term *)
      (* TODO: this could also be checked after the return *)
      let* () = unify_term ~received:tt_type ~expected in
      let self_type = tt_hole () in
      let* expected_body = split_self self_type in
      let* var, body =
        check_core_pat self ~expected:self_type ~alias:None @@ fun var ->
        (* TODO: pattern on self *)
        let* body = check_annot body in
        let* body = tt_close_level body in
        let* () = unify_term ~received:body ~expected:expected_body in
        pure (var, body)
      in
      wrapped @@ TT_self { var; body }
  | "@fix", LT_lambda { param = self; return = body } ->
      let* expected_body_type = split_self expected in
      let* var, body =
        check_core_pat self ~expected ~alias:None @@ fun var ->
        (* TODO: pattern on fix *)
        let* body =
          let* expected_body_type = tt_open_level expected_body_type in
          check_term body ~expected:expected_body_type
        in
        let* body = tt_close_level body in
        pure (var, body)
      in
      wrapped @@ TT_fix { var; body }
  | "@unroll", fix ->
      let fix_type = tt_hole () in
      let* fix = check_term fix ~expected:fix_type in
      (* TODO: this could be better? avoiding split self? *)
      let* body_type = split_self fix_type in
      let* () =
        let received = tt_open body_type ~to_:fix in
        unify_term ~received ~expected
      in
      wrapped @@ TT_unroll { term = fix }
  | "@unfold", term ->
      (* TODO: breaks propagation *)
      let term_type = tt_hole () in
      let* term = check_term term ~expected:term_type in
      let* () =
        let received = tt_unfold_fix term_type in
        unify_term ~received ~expected
      in
      wrapped @@ TT_unfold { term }
  | "@native", LT_string { literal = native } ->
      check_term_native ~native ~expected
  | _ -> error_unknown_extension ~extension ~payload

and check_term_native ~native ~expected =
  let wrapped term =
    let* () = escape_check_term expected in
    pure @@ term
  in
  match native with
  | "debug" ->
      (* TODO: use this types? *)
      let* _param_type, _return = split_forall expected in
      wrapped @@ TT_native { native = TN_debug }
  | native -> error_unknown_native ~native

and check_annot term = check_term term ~expected:tt_type

and check_typed_pat :
    type k.
    _ -> expected:_ -> alias:_ -> (_ -> k typer_context) -> k typer_context =
 fun pat ~expected ~alias f ->
  (* TODO: why this escape check? *)
  let* () = escape_check_term expected in
  check_core_pat pat ~expected ~alias @@ fun pat ->
  f @@ TPat { pat; type_ = expected }

and check_core_pat :
    type k.
    pat ->
    expected:_ ->
    alias:_ ->
    (core_pat -> k typer_context) ->
    k typer_context =
 fun pat ~expected ~alias f ->
  (* TODO: expected should be a pattern, to achieve strictness *)
  match (pat, expected) with
  | LP_var { var = name }, expected ->
      (* TODO: add different names for left and right *)
      with_free_vars ~name ~type_:expected ~alias @@ fun () ->
      f @@ TP_var { name }
  | LP_pair _, _ -> error_pairs_not_implemented ()
  | LP_annot { pat; annot }, _expected_desc ->
      (* TODO: TP_annot *)
      let* annot = check_annot annot in
      let* () = unify_term ~received:annot ~expected in
      check_core_pat pat ~expected:annot ~alias f
  | LP_loc { pat; loc }, _expected_desc ->
      with_loc ~loc @@ fun () -> check_core_pat pat ~expected ~alias f

let infer_term term =
  let expected = tt_hole () in
  check_term term ~expected
