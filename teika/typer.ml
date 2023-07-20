open Ltree
open Ttree
open Context
open Typer_context
open Expand_head

let unify_term ~expected ~received =
  with_unify_context @@ fun () -> Unify.unify_term ~expected ~received

let split_forall (type a) (type_ : a term) =
  match expand_head_term type_ with
  | TT_forall { var = _; param; return } ->
      Typer_context.return (Ex_term param, Ex_term return)
      (* TODO: hole should behave differently here *)
  | TT_bound_var _ | TT_free_var _ | TT_hole _ | TT_lambda _ | TT_apply _ ->
      error_not_a_forall ~type_

let typeof_term term =
  let (TT_typed { term = _; annot }) = term in
  Ex_term annot

let tt_typed ~annot term = TT_typed { term; annot }

let with_tt_loc ~loc f =
  with_loc ~loc @@ fun () ->
  let+ (TT_typed { term; annot }) = f () in
  let term = TT_loc { term; loc } in
  tt_typed ~annot term

let rec infer_term term =
  match term with
  | LT_var { var = name } ->
      let+ level, Ex_term annot = lookup_var ~name in
      tt_typed ~annot @@ TT_free_var { level }
  | LT_forall { param; return } ->
      infer_pat param @@ fun var (Ex_term param) ->
      let+ return = check_term return ~expected:tt_type in
      (* *)
      tt_typed ~annot:tt_type @@ TT_forall { var; param; return }
  | LT_lambda { param; return } ->
      infer_pat param @@ fun var (Ex_term param) ->
      let+ return = infer_term return in
      let annot =
        let (Ex_term return) = typeof_term return in
        TT_forall { var; param; return }
      in
      tt_typed ~annot @@ TT_lambda { var; param; return }
  | LT_apply { lambda; arg } ->
      let* lambda = infer_term lambda in
      let (Ex_term forall) = typeof_term lambda in
      let* Ex_term param, Ex_term return = split_forall forall in
      let+ arg = check_term arg ~expected:param in
      let (Ex_term annot) =
        (* TODO: this technically works here, but bad *)
        Subst.subst_bound ~from:Index.zero ~to_:arg return
      in
      tt_typed ~annot @@ TT_apply { lambda; arg }
  | LT_exists _ -> error_pairs_not_implemented ()
  | LT_pair _ -> error_pairs_not_implemented ()
  | LT_let { bound; return } ->
      (* TODO: use this loc *)
      let (LBind { loc = _; pat; value }) = bound in
      let* value = infer_term value in
      let+ var, return =
        let (Ex_term value_type) = typeof_term value in
        check_pat pat ~expected:value_type @@ fun var _annot ->
        (* TODO: this annotation here is not used *)
        let+ return = infer_term return in
        (var, return)
      in
      let annot =
        let (Ex_term return) = typeof_term return in
        TT_let { var; value; return }
      in
      tt_typed ~annot @@ TT_let { var; value; return }
  | LT_annot { term; annot } ->
      let* annot = check_term annot ~expected:tt_type in
      let+ term = check_term term ~expected:annot in
      tt_typed ~annot @@ TT_annot { term; annot }
  | LT_loc { term; loc } -> with_tt_loc ~loc @@ fun () -> infer_term term

and check_term : type a. _ -> expected:a term -> _ =
 fun term ~expected ->
  (* TODO: repr function for term, maybe with_term? *)
  match (term, expand_head_term expected) with
  | LT_loc { term; loc }, expected ->
      with_tt_loc ~loc @@ fun () -> check_term term ~expected
  | ( LT_lambda { param; return },
      TT_forall { var = _; param = expected_param; return = expected_return } )
    ->
      check_pat param ~expected:expected_param @@ fun var (Ex_term param) ->
      let+ return = check_term return ~expected:expected_return in
      tt_typed ~annot:expected @@ TT_lambda { var; param; return }
  | ( ( LT_var _ | LT_forall _ | LT_lambda _ | LT_apply _ | LT_exists _
      | LT_pair _ | LT_let _ | LT_annot _ ),
      expected ) ->
      let* term = infer_term term in
      let+ () =
        let (Ex_term received) = typeof_term term in
        unify_term ~expected ~received
      in
      term

and infer_pat : type a. _ -> (_ -> _ -> a typer_context) -> a typer_context =
 fun pat f ->
  match pat with
  | LP_annot { pat; annot } ->
      let* annot = check_term annot ~expected:tt_type in
      check_pat pat ~expected:annot f
  | LP_loc { pat; loc } -> with_loc ~loc @@ fun () -> infer_pat pat f
  | LP_var _ | LP_pair _ -> error_pat_not_annotated ~pat

and check_pat :
    type a k.
    _ -> expected:k term -> (Name.t -> _ -> a typer_context) -> a typer_context
    =
 fun pat ~expected f ->
  (* TODO: expected should be a pattern, to achieve strictness *)
  match (pat, expected) with
  | LP_var { var = name }, expected ->
      (* TODO: add different names for left and right *)
      with_expected_var @@ fun () ->
      with_received_var ~name ~type_:expected @@ fun () ->
      f name (Ex_term expected)
  | LP_pair _, _ -> error_pairs_not_implemented ()
  | LP_annot { pat; annot }, _expected_desc ->
      let* annot = check_term annot ~expected:tt_type in
      let* () = unify_term ~expected ~received:annot in
      check_pat pat ~expected:annot f
  | LP_loc { pat; loc }, _expected_desc ->
      with_loc ~loc @@ fun () -> check_pat pat ~expected f
