open Syntax
open Ltree
open Ttree
open Context
open Typer_context
open Tmachinery
open Unify

let unify_term ~expected ~received =
  with_unify_context @@ fun ~aliases -> tt_unify ~aliases ~expected ~received

let tt_open_level term =
  let* to_ = level () in
  let to_ = TT_free_var { level = to_ } in
  pure @@ tt_open term ~to_

let tt_close_level term =
  let* from = level () in
  pure @@ tt_close term ~from

let split_forall type_ =
  (* TODO: optimization, avoid the holes when annotated *)
  let* param_type = tt_hole () in
  let param = TPat { pat = tp_hole (); type_ = param_type } in
  let* return = tt_hole () in
  let expected = TT_forall { param; return } in
  let* () = unify_term ~received:type_ ~expected in
  pure (param_type, return)

(* TODO: does having expected_term also improves inference?
     Maybe with self and fix? But maybe not worth it
   Seems to help with many cases such as expected on annotation *)
let rec check_term term ~expected =
  (* TODO: propagation through dependent things *)
  match term with
  | LT_var { var = name } ->
      let* level, received = lookup_var ~name in
      let* () = unify_term ~received ~expected in
      pure @@ TT_free_var { level }
  | LT_extension { extension; payload } ->
      check_term_extension ~extension ~payload ~expected
  | LT_forall { param; return } ->
      (* TODO: this could in theory be improved by expected term *)
      (* TODO: this could also be checked after the return *)
      let* () = unify_term ~received:tt_type ~expected in
      let* param_type = tt_hole () in
      let* name, param = check_typed_pat param ~expected:param_type in
      let* return =
        with_free_vars ~name ~type_:param_type ~alias:None @@ fun () ->
        let* return = check_annot return in
        tt_close_level return
      in
      pure @@ TT_forall { param; return }
  | LT_lambda { param; return } ->
      (* TODO: maybe unify param? *)
      let* param_type, return_type = split_forall expected in
      let* name, param = check_typed_pat param ~expected:param_type in
      let* return =
        with_free_vars ~name ~type_:param_type ~alias:None @@ fun () ->
        let* return_type = tt_open_level return_type in
        let* return = check_term return ~expected:return_type in
        tt_close_level return
      in
      pure @@ TT_lambda { param; return }
  | LT_apply { lambda; arg } ->
      let* lambda_type = tt_hole () in
      let* lambda = check_term lambda ~expected:lambda_type in
      (* TODO: this could be better? avoiding split forall? *)
      let* arg_type, return_type = split_forall lambda_type in
      let* arg = check_term arg ~expected:arg_type in
      let* () =
        let received = tt_open return_type ~to_:arg in
        unify_term ~received ~expected
      in
      pure @@ TT_apply { lambda; arg }
  | LT_let { bound; return } ->
      (* TODO: use this loc *)
      let (LBind { loc = _; pat; value }) = bound in
      let* value_type = tt_hole () in
      let* name, bound = check_typed_pat pat ~expected:value_type in
      let* value = check_term value ~expected:value_type in
      let* return_type, return =
        with_free_vars ~name ~type_:value_type ~alias:(Some value) @@ fun () ->
        let* return_type = tt_hole () in
        let* return = check_term return ~expected:return_type in
        let* return_type = tt_close_level return_type in
        let* return = tt_close_level return in
        pure (return_type, return)
      in
      let* () =
        let received = tt_open return_type ~to_:value in
        unify_term ~received ~expected
      in
      pure @@ TT_let { bound; value; return }
  | LT_annot { term; annot } ->
      (* TODO: expected term could propagate here *)
      let* annot = check_annot annot in
      let* () = unify_term ~received:annot ~expected in
      (* TODO: unify annot before or after check term *)
      let* term = check_term term ~expected:annot in
      pure @@ TT_annot { term; annot }
  | LT_string { literal } ->
      let* () = unify_term ~expected ~received:string_type in
      pure @@ TT_string { literal }
  | LT_loc { term; loc = _ } ->
      (* TODO: use loc *)
      check_term term ~expected

and check_term_extension ~extension ~payload ~expected =
  match (Name.repr extension, payload) with
  | _, LT_loc { term = payload; loc = _ } ->
      (* TODO: use location *)
      check_term_extension ~extension ~payload ~expected
  | "@native", LT_string { literal = native } ->
      check_term_native ~native ~expected
  | _ -> error_unknown_extension ~extension ~payload

and check_term_native ~native ~expected =
  match native with
  | "debug" ->
      (* TODO: use this types? *)
      let* _param_type, _return = split_forall expected in
      pure @@ TT_native { native = TN_debug }
  | native -> error_unknown_native ~native

and check_annot term = check_term term ~expected:tt_type

and check_typed_pat pat ~expected =
  (* TODO: why this escape check? *)
  let* name, pat = check_core_pat pat ~expected in
  (* TODO: returning name is not ideal *)
  pure (name, TPat { pat; type_ = expected })

and check_core_pat pat ~expected =
  (* TODO: expected should be a pattern, to achieve strictness *)
  match pat with
  | LP_var { var = name } -> pure (name, TP_var { name })
  | LP_annot { pat; annot } ->
      (* TODO: TP_annot *)
      let* annot = check_annot annot in
      let* () = unify_term ~received:annot ~expected in
      check_core_pat pat ~expected:annot
  | LP_erasable _ -> error_erasable_not_implemented ()
  | LP_loc { pat; loc } ->
      with_loc ~loc @@ fun () -> check_core_pat pat ~expected

let infer_term term =
  let* expected = tt_hole () in
  check_term term ~expected
