open Ttree
open Context.Normalize_context

let rec normalize_term term =
  let (TTerm { loc; desc; type_ }) = term in
  let* type_ = normalize_type type_ in
  let+ desc = normalize_desc desc in
  TTerm { loc; desc; type_ }

and normalize_type type_ =
  let (TType { loc; desc }) = type_ in
  let+ desc = normalize_desc desc in
  TType { loc; desc }

and normalize_pat pat =
  let (TPat { loc; desc; type_ }) = pat in
  let* type_ = normalize_type type_ in
  let+ desc = normalize_pat_desc desc in
  TPat { loc; desc; type_ }

and normalize_pat_desc pat_desc =
  match pat_desc with
  | TP_var { var } -> return @@ TP_var { var }
  | TP_pair { left; right } ->
      let* left = normalize_pat left in
      let+ right = normalize_pat right in
      TP_pair { left; right }
  | TP_annot { pat; annot = _ } ->
      let (TPat { loc = _; desc; type_ = _ }) = pat in
      normalize_pat_desc desc

and normalize_annot annot f =
  let (TAnnot { loc; pat; annot }) = annot in
  let* annot = normalize_type annot in
  let* pat = normalize_pat pat in
  with_var @@ fun () -> f (tannot loc ~pat ~annot)

and normalize_bind bind f =
  let (TBind { loc; pat; value }) = bind in
  let* value = normalize_term value in
  let (TTerm { loc = _; desc = value_desc; type_ = _ }) = value in
  let* pat = normalize_pat pat in
  elim_var ~to_:value_desc @@ fun () ->
  let bind = tbind loc ~pat ~value in
  f bind

and normalize_desc desc =
  match desc with
  | TT_var { offset } -> repr_var ~var:offset
  | TT_forall { param; return } ->
      normalize_annot param @@ fun param ->
      let+ return = normalize_type return in
      TT_forall { param; return }
  | TT_lambda { param; return } ->
      normalize_annot param @@ fun param ->
      let+ return = normalize_term return in
      TT_lambda { param; return }
  | TT_apply { lambda; arg } -> (
      let* lambda = normalize_term lambda in
      let* arg = normalize_term arg in
      match
        let (TTerm { loc = _; desc; type_ = _ }) = lambda in
        desc
      with
      | TT_lambda { param = _; return } ->
          let (TTerm { loc = _; desc = return; type_ = _ }) = return in
          let (TTerm { loc = _; desc = arg; type_ = _ }) = arg in
          (* TODO: return normalized twice? *)
          with_offset ~offset:Offset.(zero - one) @@ fun () ->
          elim_var ~to_:arg @@ fun () -> normalize_desc return
      | _ -> return @@ TT_apply { lambda; arg })
  | TT_exists { left; right } ->
      normalize_annot left @@ fun left ->
      normalize_annot right @@ fun right -> return @@ TT_exists { left; right }
  | TT_pair { left; right } ->
      normalize_bind left @@ fun left ->
      normalize_bind right @@ fun right -> return @@ TT_pair { left; right }
  | TT_let { bound; return } ->
      normalize_bind bound @@ fun bound ->
      let (TBind { loc = lambda_loc; pat = param; value = arg }) = bound in
      let (TPat { loc = param_loc; desc = _; type_ = annot }) = param in
      let param = tannot param_loc ~pat:param ~annot in
      let forall =
        let (TTerm { loc = _; desc = _; type_ = return }) = return in
        tt_forall lambda_loc ~param ~return
      in
      let lambda = tt_lambda lambda_loc forall ~param ~return in
      let apply = TT_apply { lambda; arg } in
      normalize_desc apply
  | TT_annot { value; annot = _ } ->
      let (TTerm { loc = _; desc = value; type_ = _ }) = value in
      normalize_desc value
  | TT_offset { desc; offset } ->
      with_offset ~offset @@ fun () -> normalize_desc desc
