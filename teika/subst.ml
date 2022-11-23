open Ttree

let rec subst_term ~from ~to_ term =
  let (TTerm { loc; desc; type_ }) = term in
  let desc = subst_desc ~from ~to_ desc in
  TTerm { loc; desc; type_ }

and subst_type ~from ~to_ type_ =
  let (TType { loc; desc }) = type_ in
  let desc = subst_desc ~from ~to_ desc in
  TType { loc; desc }

and subst_annot ~from ~to_ annot =
  let (TAnnot { loc; var; annot }) = annot in
  let annot = subst_type ~from ~to_ annot in
  (var, tannot loc ~var ~annot)

and subst_bind ~from ~to_ bind =
  let (TBind { loc; var; value }) = bind in
  let value = subst_term ~from ~to_ value in
  (var, tbind loc ~var ~value)

and subst_desc ~from ~to_ desc =
  let subst_term term = subst_term ~from ~to_ term in
  let subst_type type_ = subst_type ~from ~to_ type_ in
  let subst_annot annot = subst_annot ~from ~to_ annot in
  let subst_bind bind = subst_bind ~from ~to_ bind in
  match desc with
  | TT_var { var } -> (
      match Var.equal from var with true -> to_ | false -> TT_var { var })
  | TT_forall { param; return } ->
      let var, param = subst_annot param in
      let return =
        match Var.equal from var with
        | true -> return
        | false -> subst_type return
      in
      TT_forall { param; return }
  | TT_lambda { param; return } ->
      let var, param = subst_annot param in
      let return =
        match Var.equal from var with
        | true -> return
        | false -> subst_term return
      in
      TT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let lambda = subst_term lambda in
      let arg = subst_term arg in
      TT_apply { lambda; arg }
  | TT_exists { left; right } ->
      let var, left = subst_annot left in
      let right =
        match Var.equal from var with
        | true -> right
        | false ->
            let _var, right = subst_annot right in
            right
      in
      TT_exists { left; right }
  | TT_pair { left; right } ->
      let var, left = subst_bind left in
      let right =
        match Var.equal from var with
        | true -> right
        | false ->
            let _var, right = subst_bind right in
            right
      in
      TT_pair { left; right }
  | TT_unpair { left; right; pair; return } ->
      let pair = subst_term pair in
      let return =
        (* TODO: is this guard needed? *)
        match Var.equal from left || Var.equal from right with
        | true -> return
        | false -> subst_term return
      in
      TT_unpair { left; right; pair; return }
  | TT_let { bound; return } ->
      let var, bound = subst_bind bound in
      let return =
        match Var.equal from var with
        | true -> return
        | false -> subst_term return
      in
      TT_let { bound; return }
  | TT_annot { value; annot } ->
      let annot = subst_type annot in
      let value = subst_term value in
      TT_annot { value; annot }

let subst_annot ~from ~to_ annot =
  let _var, annot = subst_annot ~from ~to_ annot in
  annot

let subst_bind ~from ~to_ bind =
  let _var, bind = subst_bind ~from ~to_ bind in
  bind
