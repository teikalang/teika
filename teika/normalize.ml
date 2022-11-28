open Ttree
module Normalize_context = Context.Normalize_context (Instance)
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

and normalize_annot annot f =
  let (TAnnot { loc; var; annot }) = annot in
  let* annot = normalize_type annot in
  with_var @@ fun () -> f (tannot loc ~var ~annot)

and normalize_bind bind f =
  let (TBind { loc; var; value }) = bind in
  let* value = normalize_term value in
  let (TTerm { loc = _; desc = value_desc; type_ = _ }) = value in
  elim_var ~to_:value_desc @@ fun () ->
  let bind = tbind loc ~var ~value in
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
          elim_var ~to_:arg @@ fun () ->
          let* return = normalize_desc return in
          lower_desc ~offset:Offset.(one) return
      | _ -> return @@ TT_apply { lambda; arg })
  | TT_exists { left; right } ->
      normalize_annot left @@ fun left ->
      normalize_annot right @@ fun right -> return @@ TT_exists { left; right }
  | TT_pair { left; right } ->
      normalize_bind left @@ fun left ->
      normalize_bind right @@ fun right -> return @@ TT_pair { left; right }
  | TT_unpair { left; right; pair; return } -> (
      let* pair = normalize_term pair in
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
          elim_var ~to_:left_desc @@ fun () ->
          elim_var ~to_:right_desc @@ fun () -> normalize_desc return
      | _ ->
          let+ return = normalize_term return in
          TT_unpair { left; right; pair; return })
  | TT_let { bound; return } ->
      normalize_bind bound @@ fun _bound ->
      let (TTerm { loc = _; desc = return; type_ = _ }) = return in
      (* TODO: let may be let dependent *)
      normalize_desc return
  | TT_annot { value; annot = _ } ->
      let (TTerm { loc = _; desc = value; type_ = _ }) = value in
      normalize_desc value
