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

let tt_expand_head term =
  let* aliases = aliases () in
  pure @@ tt_expand_head ~aliases term

let split_forall type_ =
  let* type_ = tt_expand_head type_ in
  match type_ with
  | TT_forall { param; return } ->
      let (TPat { pat = TP_var _; type_ = param_type }) = param in
      pure (param_type, return)
  | _ ->
      (* TODO: print non expanded *)
      error_not_a_forall ~type_

(* TODO: does having expected_term also improves inference?
     Maybe with self and fix? But maybe not worth it
   Seems to help with many cases such as expected on annotation *)
let rec infer_term term =
  match term with
  | LT_loc { term; loc = _ } ->
      (* TODO: use loc *)
      infer_term term
  | LT_var { var = name } ->
      let* level, type_ = lookup_var ~name in
      pure @@ (TT_free_var { level }, type_)
  | LT_extension _ ->
      (* TODO: support annotations here? *)
      error_missing_annotation ()
  | LT_forall { param; return } ->
      let* name, param, param_type = infer_typed_pat param in
      let* return =
        with_free_vars ~name ~type_:param_type ~alias:None @@ fun () ->
        let* return = check_annot return in
        tt_close_level return
      in
      pure @@ (TT_forall { param; return }, tt_type)
  | LT_lambda { param; return } ->
      let* name, param, param_type = infer_typed_pat param in
      let* return, return_type =
        with_free_vars ~name ~type_:param_type ~alias:None @@ fun () ->
        let* return, return_type = infer_term return in
        let* return = tt_close_level return in
        let* return_type = tt_close_level return_type in
        pure (return, return_type)
      in
      let type_ = TT_forall { param; return = return_type } in
      pure @@ (TT_lambda { param; return }, type_)
  | LT_apply { lambda; arg } ->
      let* lambda, lambda_type = infer_term lambda in
      (* TODO: this could be better? avoiding split forall? *)
      let* arg_type, return_type = split_forall lambda_type in
      let* arg = check_term arg ~expected:arg_type in
      let type_ = tt_open return_type ~to_:arg in
      pure @@ (TT_apply { lambda; arg }, type_)
  | LT_let { bound; return } ->
      (* TODO: use this loc *)
      let (LBind { loc = _; pat; value }) = bound in
      let* name, bound, value_type = infer_typed_pat pat in
      (* TODO: propagate from value to pat *)
      let* value = check_term value ~expected:value_type in
      let* return, return_type =
        with_free_vars ~name ~type_:value_type ~alias:(Some value) @@ fun () ->
        let* return, return_type = infer_term return in
        let* return = tt_close_level return in
        let* return_type = tt_close_level return_type in
        pure (return, return_type)
      in
      let type_ = tt_open return_type ~to_:value in
      pure @@ (TT_let { bound; value; return }, type_)
  | LT_annot { term; annot } ->
      (* TODO: expected term could propagate here *)
      let* annot = check_annot annot in
      (* TODO: unify annot before or after check term *)
      let* term = check_term term ~expected:annot in
      pure @@ (TT_annot { term; annot }, annot)
  | LT_string { literal } -> pure @@ (TT_string { literal }, string_type)

and check_term term ~expected =
  (* TODO: propagation through dependent things
      aka substitution inversion *)
  match term with
  | LT_loc { term; loc = _ } ->
      (* TODO: use loc *)
      check_term term ~expected
  | LT_extension { extension; payload } ->
      check_term_extension ~extension ~payload ~expected
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
  | LT_var _ | LT_forall _ | LT_apply _ | LT_let _ | LT_annot _ | LT_string _ ->
      (* TODO: forall could in theory be improved by expected term *)
      (* TODO: apply could propagate when arg is var *)
      (* TODO: could propagate through let? *)
      let* term, type_ = infer_term term in
      let* () = unify_term ~received:type_ ~expected in
      pure term

and check_annot term = check_term term ~expected:tt_type

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

and infer_typed_pat pat =
  match pat with
  | LP_var { var = _ } -> error_missing_annotation ()
  | LP_annot { pat; annot } ->
      (* TODO: TP_annot *)
      let* annot = check_annot annot in
      let* name, pat = check_typed_pat pat ~expected:annot in
      pure (name, pat, annot)
  | LP_erasable _ -> error_erasable_not_implemented ()
  | LP_loc { pat; loc } -> with_loc ~loc @@ fun () -> infer_typed_pat pat

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

(* TODO: this is weird *)
let infer_term term =
  let* term, _type = infer_term term in
  pure term
