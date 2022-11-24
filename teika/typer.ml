open Ltree
open Ttree

module Typer_context =
  Context.Typer_context (Instance) (Subst) (Normalize) (Unify)

open Typer_context

let extract_type term =
  let (TTerm { loc = _; desc = _; type_ }) = term in
  type_

let annot_of_bind bind =
  let (TBind { loc; var; value }) = bind in
  with_loc ~loc @@ fun () ->
  let annot = extract_type value in
  tannot ~var ~annot

let apply ~lambda ~arg =
  let lambda_type = extract_type lambda in
  let arg_type = extract_type arg in
  let* param, return = split_forall lambda_type in
  let (TAnnot { loc = _; var = _; annot = param }) = param in
  let* () = unify_type ~expected:param ~received:arg_type in
  let* type_ =
    let (TType { loc = _; desc = arg_type }) = arg_type in
    let* return = lower_type ~offset:Offset.one return in
    subst_type ~from:Offset.zero ~to_:arg_type return
  in
  tt_apply type_ ~lambda ~arg

let unpair ~left ~right ~pair return =
  let pair_type = extract_type pair in
  let* left_annot, right_annot = split_exists pair_type in
  let (TAnnot { loc = _; var = _; annot = left_type }) = left_annot in
  let (TAnnot { loc = _; var = _; annot = right_type }) = right_annot in
  with_binder ~var:left ~type_:left_type @@ fun () ->
  with_binder ~var:right ~type_:right_type @@ fun () ->
  let* return = return () in
  let return_type_ = extract_type return in
  let* type_ =
    let* type_ = tt_type () in
    let* return = term_of_type return_type_ in
    let* unpair = tt_unpair type_ ~left ~right ~pair ~return in
    type_of_term unpair
  in
  tt_unpair type_ ~left ~right ~pair ~return

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
  let (LAnnot { loc; var; annot }) = annot in
  with_loc ~loc @@ fun () ->
  let* annot = infer_term annot in
  let* annot = type_of_term annot in
  with_binder ~var ~type_:annot @@ fun () ->
  let* annot = tannot ~var ~annot in
  f annot

and infer_bind : type a. _ -> (_ -> a typer_context) -> a typer_context =
 fun bind f ->
  let (LBind { loc; var; value }) = bind in
  with_loc ~loc @@ fun () ->
  let* value = infer_term value in
  let annot = extract_type value in
  with_binder ~var ~type_:annot @@ fun () ->
  let* annot = tbind ~var ~value in
  f annot

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
  | LT_unpair { left; right; pair; return } ->
      let* pair = infer_term pair in
      unpair ~left ~right ~pair @@ fun () -> infer_term return
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
