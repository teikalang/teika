open Ttree
module Normalize_context = Context.Normalize_context (Instance) (Subst)
open Normalize_context

let rec normalize_term term =
  let (TTerm { loc; desc; type_ }) = term in
  let* type_ = normalize_type type_ in
  let+ desc = normalize_desc desc in
  TTerm { loc; desc; type_ }

and normalize_type type_ =
  let (TType { loc; desc }) = type_ in
  let+ desc = normalize_desc desc in
  TType { loc; desc }

and normalize_annot annot =
  let (TAnnot { loc; var; annot }) = annot in
  let+ annot = normalize_type annot in
  tannot loc ~var ~annot

and normalize_bind bind =
  let (TBind { loc; var; value }) = bind in
  let+ value = normalize_term value in
  let (TTerm { loc = _; desc = value_desc; type_ = _ }) = value in
  (value_desc, tbind loc ~var ~value)

and normalize_desc desc =
  match desc with
  | TT_var { offset } -> return @@ TT_var { offset }
  | TT_forall { param; return } ->
      let* param = normalize_annot param in
      let+ return = normalize_type return in
      TT_forall { param; return }
  | TT_lambda { param; return } ->
      let* param = normalize_annot param in
      let+ return = normalize_term return in
      TT_lambda { param; return }
  | TT_apply { lambda; arg } -> (
      let* lambda = normalize_term lambda in
      let* arg = normalize_term arg in
      match
        let (TTerm { loc = _; desc; type_ = _ }) = lambda in
        desc
      with
      | TT_lambda { param; return } ->
          let (TTerm { loc = _; desc = return; type_ = _ }) = return in
          let (TTerm { loc = _; desc = arg; type_ = _ }) = arg in
          let (TAnnot { loc = _; var = _; annot = _ }) = param in
          let* return = subst_desc ~from:Offset.zero ~to_:arg return in
          normalize_desc return
      | _ -> return @@ TT_apply { lambda; arg })
  | TT_exists { left; right } ->
      let* left = normalize_annot left in
      let+ right = normalize_annot right in
      TT_exists { left; right }
  | TT_pair { left; right } ->
      let* desc, left = normalize_bind left in
      let* right = subst_bind ~from:Offset.zero ~to_:desc right in
      let+ _desc, right = normalize_bind right in
      TT_pair { left; right }
  | TT_unpair { left; right; pair; return } -> (
      let* pair = normalize_term pair in
      let* return = normalize_term return in
      match
        let (TTerm { loc = _; desc; type_ = _ }) = pair in
        desc
      with
      | TT_pair { left = left_bind; right = right_bind } ->
          let (TBind { loc = _; var = _; value = left_value }) = left_bind in
          let (TTerm { loc = _; desc = left_desc; type_ = _ }) = left_value in
          let (TBind { loc = _; var = _; value = right_value }) = right_bind in
          let (TTerm { loc = _; desc = right_desc; type_ = _ }) = right_value in
          let (TTerm { loc = _; desc = return; type_ = _ }) = return in
          let* return = subst_desc ~from:Offset.one ~to_:left_desc return in
          subst_desc ~from:Offset.zero ~to_:right_desc return
      | _ -> Normalize_context.return @@ TT_unpair { left; right; pair; return }
      )
  | TT_let { bound; return } ->
      let* desc, _bound = normalize_bind bound in
      let (TTerm { loc = _; desc = return; type_ = _ }) = return in
      (* TODO: let may be let dependent *)
      let* return = subst_desc ~from:Offset.zero ~to_:desc return in
      normalize_desc return
  | TT_annot { value; annot = _ } ->
      let (TTerm { loc = _; desc = value; type_ = _ }) = value in
      normalize_desc value
