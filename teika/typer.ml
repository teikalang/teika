open Ltree
open Ttree
open Context
open Typer_context

let unify_term ~expected ~received =
  with_unify_context @@ fun () -> Unify.unify_term ~expected ~received

let normalize_received_term term =
  with_received_normalize_context @@ fun () -> Normalize.normalize_term term

let split_forall (type a) (type_ : a term) =
  let* (Ex_term type_) = normalize_received_term type_ in
  let rec split_forall : type a. a term -> _ =
   fun type_ ->
    match type_ with
    | TT_loc { term; loc = _ } -> split_forall term
    | TT_annot { term; annot = _ } -> split_forall term
    | TT_forall { param; return } -> Typer_context.return (param, Ex_term return)
    | TT_var _ | TT_lambda _ | TT_apply _ -> error_not_a_forall ~type_
  in
  split_forall type_

let typeof_term term =
  let (TT_annot { term = _; annot }) = term in
  Ex_term annot

let typeof_pat pat =
  let (TP_annot { pat = _; annot }) = pat in
  Ex_term annot

let tt_annot ~annot term = TT_annot { term; annot }
let tp_annot ~annot pat = TP_annot { pat; annot }

let with_tt_loc ~loc f =
  with_loc ~loc @@ fun () ->
  let+ (TT_annot { term; annot }) = f () in
  let term = TT_loc { term; loc } in
  tt_annot ~annot term

let with_tp_loc ~loc k =
  with_loc ~loc @@ fun () ->
  k @@ fun (TP_annot { pat; annot }) k ->
  let pat = TP_loc { pat; loc } in
  k @@ tp_annot ~annot pat

let rec infer_term term =
  match term with
  | LT_var { var = name } ->
      let+ offset, Ex_term annot = instance ~name in
      tt_annot ~annot @@ TT_var { offset }
  | LT_forall { param; return } ->
      let* annot = tt_type () in
      infer_pat param @@ fun param ->
      let+ return =
        let* expected = tt_type () in
        check_term return ~expected
      in
      tt_annot ~annot @@ TT_forall { param; return }
  | LT_lambda { param; return } ->
      infer_pat param @@ fun param ->
      let+ return = infer_term return in
      let annot =
        let (Ex_term return) = typeof_term return in
        TT_forall { param; return }
      in
      tt_annot ~annot @@ TT_lambda { param; return }
  | LT_apply { lambda; arg } ->
      let* lambda = infer_term lambda in
      let (Ex_term forall) = typeof_term lambda in
      let* param, Ex_term return = split_forall forall in
      let+ arg =
        let (Ex_term param) = typeof_pat param in
        check_term arg ~expected:param
      in
      let annot =
        (* TODO: this is not ideal, generating core terms *)
        let lambda = TT_lambda { param; return } in
        TT_apply { lambda; arg }
      in
      tt_annot ~annot @@ TT_apply { lambda; arg }
  | LT_exists _ -> error_pairs_not_implemented ()
  | LT_pair _ -> error_pairs_not_implemented ()
  | LT_let { bound; return } ->
      (* TODO: use this loc *)
      let (LBind { loc = _; pat; value }) = bound in

      (* TODO: this is not ideal, generating core terms *)
      let* value = infer_term value in
      let+ param, return =
        let (Ex_term param) = typeof_term value in
        check_pat pat ~expected:param @@ fun param ->
        let+ return = infer_term return in
        (param, return)
      in
      let apply =
        let lambda = TT_lambda { param; return } in
        TT_apply { lambda; arg = value }
      in
      let annot =
        let (Ex_term return) = typeof_term return in
        let lambda = TT_lambda { param; return } in
        TT_apply { lambda; arg = value }
      in
      tt_annot ~annot apply
  | LT_annot { term; annot } ->
      let* annot =
        let* expected = tt_type () in
        check_term annot ~expected
      in
      let+ term = check_term term ~expected:annot in
      tt_annot ~annot term
  | LT_loc { term; loc } -> with_tt_loc ~loc @@ fun () -> infer_term term

and check_term : type a. _ -> expected:a term -> _ =
 fun term ~expected ->
  (* TODO: repr function for term, maybe with_term? *)
  match (term, expected) with
  | ( LT_lambda { param; return },
      TT_forall { param = expected_param; return = expected_return } ) ->
      let (Ex_term expected_param_type) = typeof_pat expected_param in
      check_pat param ~expected:expected_param_type @@ fun param ->
      let+ return = check_term return ~expected:expected_return in
      tt_annot ~annot:expected @@ TT_lambda { param; return }
  | LT_loc { term; loc }, expected ->
      with_tt_loc ~loc @@ fun () -> check_term term ~expected
      (* TODO: what about this loc? *)
  | term, TT_loc { term = expected; loc = _ } -> check_term term ~expected
  | term, TT_annot { term = expected; annot = _ } -> check_term term ~expected
  (* TODO: maybe LT_annot? *)
  | ( ( LT_var _ | LT_forall _ | LT_lambda _ | LT_apply _ | LT_exists _
      | LT_pair _ | LT_let _ | LT_annot _ ),
      expected ) ->
      let* term = infer_term term in
      let+ () =
        let (Ex_term received) = typeof_term term in
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
    type a k. _ -> expected:k term -> (_ -> a typer_context) -> a typer_context
    =
 fun pat ~expected f ->
  (* TODO: expected should be a pattern, to achieve strictness *)
  match (pat, expected) with
  | LP_var { var = name }, expected ->
      (* TODO: add different names for left and right *)
      with_expected_var @@ fun () ->
      with_received_var ~name ~type_:expected @@ fun () ->
      f @@ tp_annot ~annot:expected @@ TP_var { var = name }
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
