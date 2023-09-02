open Ttree

let rec expand_subst_term ~subst term =
  (* TODO: check if term has same type as subst *)
  tt_map_desc term @@ fun ~wrap term desc ->
  let tt_subst term subst = wrap @@ TT_subst { term; subst } in
  let with_var subst =
    match subst with
    | TS_subst_bound { from; to_ } ->
        let from = Index.(from + one) in
        TS_subst_bound { from; to_ }
    | TS_subst_free { from; to_ } ->
        (* TODO: shifting here? *)
        TS_subst_free { from; to_ }
    | TS_open_bound { from; to_ } ->
        let from = Index.(from + one) in
        TS_open_bound { from; to_ }
    | TS_close_free { from; to_ } ->
        let to_ = Index.(to_ + one) in
        TS_close_free { from; to_ }
  in
  match desc with
  | TT_subst { term; subst = subst' } ->
      let term = expand_subst_term ~subst:subst' term in
      expand_subst_term ~subst term
  | TT_bound_var { index } -> (
      match subst with
      | TS_subst_bound { from; to_ } -> (
          match Index.equal from index with true -> to_ | false -> term)
      | TS_subst_free { from = _; to_ = _ } -> term
      | TS_open_bound { from; to_ } -> (
          match Index.equal from index with
          | true -> wrap @@ TT_free_var { level = to_; alias = None }
          | false -> term)
      | TS_close_free { from = _; to_ = _ } -> term)
  | TT_free_var { level; alias = _ } -> (
      match subst with
      | TS_subst_bound { from = _; to_ = _ } -> term
      | TS_subst_free { from; to_ } -> (
          match Level.equal from level with true -> to_ | false -> term)
      | TS_open_bound { from = _; to_ = _ } -> term
      | TS_close_free { from; to_ } -> (
          match Level.equal from level with
          | true -> wrap @@ TT_bound_var { index = to_ }
          | false -> term))
  (* TODO: subst and hole *)
  | TT_hole { hole = _ } -> term
  | TT_forall { param; return } ->
      let param = expand_subst_typed_pat ~subst param in
      let return = tt_subst return @@ with_var subst in
      wrap @@ TT_forall { param; return }
  | TT_lambda { param; return } ->
      let param = expand_subst_typed_pat ~subst param in
      let return = tt_subst return @@ with_var subst in
      wrap @@ TT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let lambda = tt_subst lambda subst in
      let arg = tt_subst arg subst in
      wrap @@ TT_apply { lambda; arg }
  | TT_self { var; body } ->
      let body = tt_subst body @@ with_var subst in
      wrap @@ TT_self { var; body }
  | TT_fix { var; body } ->
      let body = tt_subst body @@ with_var subst in
      wrap @@ TT_fix { var; body }
  | TT_unroll { term } ->
      let term = tt_subst term subst in
      wrap @@ TT_unroll { term }
  | TT_unfold { term } ->
      let term = tt_subst term subst in
      wrap @@ TT_unfold { term }
  | TT_let { bound; value; return } ->
      let bound = expand_subst_typed_pat ~subst bound in
      let value = tt_subst value subst in
      let return = tt_subst return @@ with_var subst in
      wrap @@ TT_let { bound; value; return }
  | TT_annot { term; annot } ->
      let term = tt_subst term subst in
      let annot = tt_subst annot subst in
      wrap @@ TT_annot { term; annot }
  | TT_string _ -> term
  | TT_native _ -> term

and expand_subst_typed_pat : subst:subst -> _ -> _ =
 fun ~subst pat ->
  let (TPat { pat; type_ }) = pat in
  let type_ = expand_subst_term ~subst type_ in
  TPat { pat; type_ }

let rec expand_head_term term =
  tt_map_desc term @@ fun ~wrap:_ term desc ->
  match desc with
  | TT_subst { term; subst } ->
      expand_head_term @@ expand_subst_term ~subst term
  | TT_bound_var _ -> term
  | TT_free_var { level = _; alias = Some alias } -> expand_head_term alias
  | TT_free_var _ -> term
  | TT_hole { hole } -> (
      (* TODO: path compression *)
      (* TODO: move this to machinery *)
      match hole.link with
      | None -> term
      | Some link ->
          (* TODO: path compression *)
          expand_head_term link)
  | TT_forall _ -> term
  | TT_lambda _ -> term
  | TT_apply { lambda; arg } -> (
      (* TODO: use expanded lambda? *)
      match tt_match (expand_head_term lambda) with
      | TT_lambda { param = _; return } ->
          (* TODO: param is not used here,
             but it would be cool to check when in debug *)
          (* TODO: this could be done in O(1) with context extending *)
          let subst = TS_subst_bound { from = Index.zero; to_ = arg } in
          expand_head_term @@ expand_subst_term ~subst return
      | TT_native { native } -> expand_head_native native ~arg
      | _lambda -> term)
  | TT_self _ -> term
  | TT_fix _ -> term
  | TT_unroll _ -> term
  | TT_unfold { term } -> expand_head_term term
  | TT_let { bound = _; value; return } ->
      (* TODO: param is not used here,
         but it would be cool to check when in debug *)
      let subst = TS_subst_bound { from = Index.zero; to_ = value } in
      expand_head_term @@ expand_subst_term ~subst return
  | TT_annot { term; annot = _ } -> expand_head_term term
  | TT_string _ -> term
  | TT_native _ -> term

and expand_head_native native ~arg =
  match native with TN_debug -> expand_head_term arg
