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
  | TT_exists { left = _; right = _ } -> tt_type ()
  | TT_pair { left; right } ->
      let* left = typeof_bind left in
      let* right = typeof_bind right in
      tt_exists ~left ~right
  | TT_let { bound; return } ->
      let* return = typeof_term return in
      tt_let ~bound ~return
  | TT_annot { term = _; annot } -> return annot
  | TT_loc { term; loc = _ } -> typeof_term term
  | TT_offset { term; offset } ->
      let* term = typeof_term term in
      return @@ TT_offset { term; offset }

and typeof_bind bind =
  let (TBind { loc; pat; value }) = bind in
  let+ annot = typeof_term value in
  TAnnot { loc; pat; annot }

and typeof_pat pat =
  match pat with
  | TP_var { var } -> error_pat_var_not_annotated ~var
  | TP_pair { left; right } ->
      (* TODO: proper locs *)
      let loc = Location.none in
      let* left =
        let* annot = typeof_pat left in
        tannot ~loc ~pat:left ~annot
      in
      let* right =
        let* annot = typeof_pat right in
        tannot ~loc ~pat:right ~annot
      in
      tt_exists ~left ~right
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
  | LT_exists { left; right } ->
      infer_annot left @@ fun left ->
      infer_annot right @@ fun right -> tt_exists ~left ~right
  | LT_pair { left; right } ->
      infer_bind left @@ fun left ->
      infer_bind right @@ fun right -> tt_pair ~left ~right
  | LT_let { bound; return } ->
      infer_bind bound @@ fun bound ->
      let* return = infer_term return in
      tt_let ~bound ~return
  | LT_annot { term; annot } ->
      let* annot =
        let* expected = tt_type () in
        check_term annot ~expected
      in
      let* term = check_term term ~expected:annot in
      tt_annot ~term ~annot
  | LT_loc { term; loc } -> with_tt_loc ~loc @@ fun () -> infer_term term

and infer_annot : type a. _ -> (_ -> a typer_context) -> a typer_context =
 fun annot f ->
  let (LAnnot { loc; pat; annot }) = annot in
  let* annot =
    let* expected = tt_type () in
    check_term annot ~expected
  in
  check_pat pat ~expected:annot @@ fun pat ->
  let* annot = tannot ~loc ~pat ~annot in
  f annot

and infer_bind : type a. _ -> (_ -> a typer_context) -> a typer_context =
 fun bind f ->
  let (LBind { loc; pat; value }) = bind in
  let* value = infer_term value in
  let* annot = typeof_term value in
  check_pat pat ~expected:annot @@ fun pat ->
  let* bind = tbind ~loc ~pat ~value in
  f bind

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
  | ( LP_pair { left; right },
      TT_exists { left = left_expected; right = right_expected } ) ->
      (* TODO: strict mode here *)
      let (TAnnot { loc = _; pat = _; annot = left_type }) = left_expected in
      let (TAnnot { loc = _; pat = _; annot = right_type }) = right_expected in
      check_pat left ~expected:left_type @@ fun left ->
      check_pat right ~expected:right_type @@ fun right ->
      let* pat = tp_pair ~left ~right in
      f pat
  | LP_pair _, expected -> error_pat_not_pair ~pat ~expected
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
