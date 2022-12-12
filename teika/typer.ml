open Ltree
open Ttree
module Typer_context = Context.Typer_context (Normalize) (Unify)
open Typer_context

let rec typeof_term term =
  match term with
  | TT_var { offset = var } -> error_term_var_not_annotated ~var
  | TT_forall { param = _; return = _ } -> tt_type ()
  | TT_lambda { param; return } ->
      let* return = typeof_term return in
      tt_forall ~param ~return
  | TT_apply { lambda; arg } ->
      let* forall = typeof_term lambda in
      let+ param, return = split_forall forall in
      let lambda = TT_lambda { param; return } in
      TT_apply { lambda; arg }
  | TT_annot { term = _; annot } -> return annot
  | TT_loc { term; loc = _ } -> typeof_term term
  | TT_offset { term; offset } ->
      let* term = typeof_term term in
      return @@ TT_offset { term; offset }

and typeof_pat pat =
  match pat with
  | TP_var { var } -> error_pat_var_not_annotated ~var
  | TP_annot { pat = _; annot } -> return @@ annot
  | TP_loc { pat; loc = _ } -> typeof_pat pat

let rec infer_term term =
  match term with
  | LT_var { var } ->
      let* offset, annot = instance ~var in
      tt_var ~annot ~offset
  | LT_forall { param; return } ->
      infer_pat param @@ fun param ->
      let* return =
        let* expected = tt_type () in
        check_term return ~expected
      in
      tt_forall ~param ~return
  | LT_lambda { param; return } ->
      infer_pat param @@ fun param ->
      let* return = infer_term return in
      tt_lambda ~param ~return
  | LT_apply { lambda; arg } ->
      let* lambda = infer_term lambda in
      let* arg =
        let* expected =
          let* forall = typeof_term lambda in
          let* param, _return = split_forall forall in
          typeof_pat param
        in
        check_term arg ~expected
      in
      tt_apply ~lambda ~arg
  | LT_exists _ -> error_pairs_not_implemented ()
  | LT_pair _ -> error_pairs_not_implemented ()
  | LT_let { bound; return } ->
      (* TODO: use this loc *)
      let (LBind { loc = _; pat; value }) = bound in
      let* value = infer_term value in
      let* param = typeof_term value in
      let* lambda =
        check_pat pat ~expected:param @@ fun param ->
        let* return = infer_term return in
        tt_lambda ~param ~return
      in
      tt_apply ~lambda ~arg:value
  | LT_annot { term; annot } ->
      let* annot =
        let* expected = tt_type () in
        check_term annot ~expected
      in
      let* term = check_term term ~expected:annot in
      tt_annot ~term ~annot
  | LT_loc { term; loc } -> with_tt_loc ~loc @@ fun () -> infer_term term

and check_term term ~expected =
  (* TODO: repr function for term, maybe with_term? *)
  match (term, expected) with
  | ( LT_lambda { param; return },
      TT_forall { param = expected_param; return = expected_return } ) ->
      let* expected_param_type = typeof_pat expected_param in
      check_pat param ~expected:expected_param_type @@ fun param ->
      let* return = check_term return ~expected:expected_return in
      tt_lambda ~param ~return
  | LT_loc { term; loc }, expected ->
      with_tt_loc ~loc @@ fun () -> check_term term ~expected
      (* TODO: what about this loc? *)
  | term, TT_loc { term = expected; loc = _ } -> check_term term ~expected
  (* TODO: maybe LT_annot? *)
  | ( ( LT_var _ | LT_forall _ | LT_lambda _ | LT_apply _ | LT_exists _
      | LT_pair _ | LT_let _ | LT_annot _ ),
      expected ) ->
      let* term = infer_term term in
      let+ () =
        let* received = typeof_term term in
        unify_term ~expected ~received
      in
      term

and infer_pat : type a. _ -> (_ -> a typer_context) -> a typer_context =
 fun pat f ->
  match pat with
  | LP_annot { pat; annot } ->
      let* annot =
        let* expected = tt_type () in
        check_term annot ~expected
      in
      check_pat pat ~expected:annot f
  | LP_loc { pat; loc } ->
      with_tp_loc ~loc @@ fun k ->
      infer_pat pat @@ fun pat -> k pat f
  | LP_var _ | LP_pair _ -> error_pat_not_annotated ~pat

and check_pat :
    type a. _ -> expected:_ -> (_ -> a typer_context) -> a typer_context =
 fun pat ~expected f ->
  (* TODO: expected should be a pattern, to achieve strictness *)
  match (pat, expected) with
  | LP_var { var }, expected ->
      with_binder ~var ~type_:expected @@ fun () ->
      let* pat = tp_var ~annot:expected ~var in
      f pat
  | LP_pair _, _ -> error_pairs_not_implemented ()
  | LP_annot { pat; annot }, _expected_desc ->
      let* annot =
        let* expected = tt_type () in
        check_term annot ~expected
      in
      let* () = unify_term ~expected ~received:annot in
      check_pat pat ~expected:annot f
  | LP_loc { pat; loc }, _expected_desc ->
      with_tp_loc ~loc @@ fun k ->
      check_pat pat ~expected @@ fun pat -> k pat f
