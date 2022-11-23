open Ttree
open Subst

let rec normalize_term term =
  let (TTerm { loc; desc; type_ }) = term in
  let type_ = normalize_type type_ in
  let desc = normalize_desc desc in
  TTerm { loc; desc; type_ }

and normalize_type type_ =
  let (TType { loc; desc }) = type_ in
  let desc = normalize_desc desc in
  TType { loc; desc }

and normalize_annot annot =
  let (TAnnot { loc; var; annot }) = annot in
  let annot = normalize_type annot in
  tannot loc ~var ~annot

and normalize_bind bind =
  let (TBind { loc; var; value }) = bind in
  let value = normalize_term value in
  let (TTerm { loc = _; desc = value_desc; type_ = _ }) = value in
  (var, value_desc, tbind loc ~var ~value)

and normalize_desc desc =
  match desc with
  | TT_var { var } -> TT_var { var }
  | TT_forall { param; return } ->
      let param = normalize_annot param in
      let return = normalize_type return in
      TT_forall { param; return }
  | TT_lambda { param; return } ->
      let param = normalize_annot param in
      let return = normalize_term return in
      TT_lambda { param; return }
  | TT_apply { lambda; arg } -> (
      let lambda = normalize_term lambda in
      let arg = normalize_term arg in
      match
        let (TTerm { loc = _; desc; type_ = _ }) = lambda in
        desc
      with
      | TT_lambda { param; return } ->
          let (TTerm { loc = _; desc = return; type_ = _ }) = return in
          let (TTerm { loc = _; desc = arg; type_ = _ }) = arg in
          let (TAnnot { loc = _; var; annot = _ }) = param in
          normalize_desc @@ subst_desc ~from:var ~to_:arg return
      | _ -> TT_apply { lambda; arg })
  | TT_exists { left; right } ->
      let left = normalize_annot left in
      let right = normalize_annot right in
      TT_exists { left; right }
  | TT_pair { left; right } ->
      let var, desc, left = normalize_bind left in
      let right = subst_bind ~from:var ~to_:desc right in
      let _var, _desc, right = normalize_bind right in
      TT_pair { left; right }
  | TT_unpair { left; right; pair; return } -> (
      let pair = normalize_term pair in
      let return = normalize_term return in
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
          let return = subst_desc ~from:left ~to_:left_desc return in
          subst_desc ~from:right ~to_:right_desc return
      | _ -> TT_unpair { left; right; pair; return })
  | TT_let { bound; return } ->
      let var, desc, _bound = normalize_bind bound in
      let (TTerm { loc = _; desc = return; type_ = _ }) = return in
      normalize_desc @@ subst_desc ~from:var ~to_:desc return
  | TT_annot { value; annot = _ } ->
      let (TTerm { loc = _; desc = value; type_ = _ }) = value in
      normalize_desc value
