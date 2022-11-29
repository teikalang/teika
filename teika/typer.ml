open Ltree
open Ttree
module Typer_context = Context.Typer_context (Normalize) (Unify)
open Typer_context

let extract_type term =
  let (TTerm { loc = _; desc = _; type_ }) = term in
  type_

let annot_of_bind bind =
  let (TBind { loc; pat; value }) = bind in
  with_loc ~loc @@ fun () ->
  let annot = extract_type value in
  tannot ~pat ~annot

(* TODO: this is a hack *)
let rec with_pat pat f =
  let (TPat { loc = _; desc; type_ }) = pat in
  match desc with
  | TP_var { var } -> with_binder ~var ~type_ f
  | TP_pair { left; right } -> with_pat left @@ fun () -> with_pat right f
  | TP_annot { pat; annot = _ } -> with_pat pat f

let apply ~lambda ~arg =
  let lambda_type = extract_type lambda in
  let arg_type = extract_type arg in
  let* param, return = split_forall lambda_type in
  let (TAnnot { loc = _; pat; annot = param_type }) = param in
  let* () = unify_type ~expected:param_type ~received:arg_type in
  let* type_ =
    let* type_ = tt_type () in
    let* lambda =
      with_pat pat @@ fun () ->
      let* type_ =
        let* return = tt_type () in
        tt_forall ~param ~return
      in
      let* return = term_of_type return in
      tt_lambda type_ ~param ~return
    in
    tt_apply type_ ~lambda ~arg
  in
  let* type_ = type_of_term type_ in
  tt_apply type_ ~lambda ~arg

let let_ ~bound ~return =
  let* type_ =
    let* type_ = tt_type () in
    let return_type = extract_type return in
    let* return = term_of_type return_type in
    let* let_ = tt_let type_ ~bound ~return in
    type_of_term let_
  in
  tt_let type_ ~bound ~return

let rec infer_term term =
  let (LTerm { loc; desc }) = term in
  with_loc ~loc @@ fun () -> infer_desc desc

and infer_annot : type a. _ -> (_ -> a typer_context) -> a typer_context =
 fun annot f ->
  let (LAnnot { loc; pat; annot }) = annot in
  with_loc ~loc @@ fun () ->
  let* annot = infer_term annot in
  let* annot = type_of_term annot in
  check_pat pat ~expected:annot @@ fun pat ->
  let* annot = tannot ~pat ~annot in
  f annot

and infer_bind : type a. _ -> (_ -> a typer_context) -> a typer_context =
 fun bind f ->
  let (LBind { loc; pat; value }) = bind in
  with_loc ~loc @@ fun () ->
  let* value = infer_term value in
  let annot = extract_type value in
  check_pat pat ~expected:annot @@ fun pat ->
  let* bind = tbind ~pat ~value in
  f bind

and infer_desc desc =
  match desc with
  | LT_var { var } ->
      let* offset, type_ = instance ~var in
      tt_var type_ ~offset
  | LT_forall { param; return } ->
      let* forall =
        infer_annot param @@ fun param ->
        let* return = infer_term return in
        let* return = type_of_term return in
        tt_forall ~param ~return
      in
      term_of_type forall
  | LT_lambda { param; return } ->
      infer_annot param @@ fun param ->
      let* return = infer_term return in
      let* type_ =
        let return = extract_type return in
        tt_forall ~param ~return
      in
      tt_lambda type_ ~param ~return
  | LT_apply { lambda; arg } ->
      let* lambda = infer_term lambda in
      let* arg = infer_term arg in
      apply ~lambda ~arg
  | LT_exists { left; right } ->
      let* exists =
        infer_annot left @@ fun left ->
        infer_annot right @@ fun right -> tt_exists ~left ~right
      in
      term_of_type exists
  | LT_pair { left; right } ->
      infer_bind left @@ fun left ->
      infer_bind right @@ fun right ->
      let* type_ =
        let* left = annot_of_bind left in
        let* right = annot_of_bind right in
        tt_exists ~left ~right
      in
      tt_pair type_ ~left ~right
  | LT_let { bound; return } ->
      infer_bind bound @@ fun bound ->
      let* return = infer_term return in
      let_ ~bound ~return
  | LT_annot { value; annot } ->
      let* annot = infer_term annot in
      let* annot = type_of_term annot in
      let* value = infer_term value in
      let* () =
        let value = extract_type value in
        unify_type ~expected:annot ~received:value
      in
      tt_annot ~value ~annot

and check_pat :
    type a. _ -> expected:_ -> (_ -> a typer_context) -> a typer_context =
 fun pat ~expected f ->
  let (LPat { loc; desc }) = pat in
  with_loc ~loc @@ fun () -> check_pat_desc desc ~expected f

and check_pat_desc :
    type a. _ -> expected:_ -> (_ -> a typer_context) -> a typer_context =
 fun pat_desc ~expected f ->
  let (TType { loc = _; desc = expected_desc }) = expected in
  match (pat_desc, expected_desc) with
  | LP_var { var }, _expected_desc ->
      with_binder ~var ~type_:expected @@ fun () ->
      let* pat = tp_var expected ~var in
      f pat
  | ( LP_pair { left; right },
      TT_exists { left = left_expected; right = right_expected } ) ->
      (* TODO: strict mode here *)
      let (TAnnot { loc = _; pat = _; annot = left_type }) = left_expected in
      let (TAnnot { loc = _; pat = _; annot = right_type }) = right_expected in
      check_pat left ~expected:left_type @@ fun left ->
      check_pat right ~expected:right_type @@ fun right ->
      let* pat = tp_pair expected ~left ~right in
      f pat
  | LP_pair _, _expected_desc ->
      error_typer_pat_not_pair ~pat:pat_desc ~expected
  | LP_annot { pat; annot }, _expected_desc ->
      let* annot = infer_term annot in
      let* annot = type_of_term annot in
      let* () = unify_type ~expected ~received:annot in
      check_pat pat ~expected:annot f
