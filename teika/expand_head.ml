open Ttree

let rec expand_head_term : type a. a term -> core term =
 fun term ->
  match term with
  | TT_loc { term; loc = _ } -> expand_head_term term
  | TT_typed { term; annot = _ } -> expand_head_term term
  | TT_subst_bound { from; to_; term } -> expand_subst_bound ~from ~to_ term
  | TT_subst_free { from; to_; term } -> expand_subst_free ~from ~to_ term
  | TT_open_bound { from; to_; term } -> expand_open_bound ~from ~to_ term
  | TT_close_free { from; to_; term } -> expand_close_free ~from ~to_ term
  | TT_bound_var _ as term -> term
  | TT_free_var _ as term -> term
  | TT_hole { link } as term -> (
      (* TODO: path compression *)
      (* TODO: move this to machinery *)
      match link == tt_nil with true -> term | false -> expand_head_term link)
  | TT_forall _ as term -> term
  | TT_lambda _ as term -> term
  | TT_apply { lambda; arg } as term -> (
      match expand_head_term lambda with
      | TT_lambda { param = _; return } ->
          (* TODO: param is not used here,
              but it would be cool to check when in debug *)
          (* TODO: this could be done in O(1) with context extending *)
          expand_head_term @@ tt_subst_bound ~from:Index.zero ~to_:arg return
      | _lambda ->
          (* TODO: use expanded? *)
          term)
  | TT_let { value; return } ->
      expand_head_term @@ tt_subst_bound ~from:Index.zero ~to_:value return
  | TT_annot { term; annot = _ } -> expand_head_term term

and expand_subst_bound : type a t. from:_ -> to_:t term -> a term -> core term =
 fun ~from ~to_ term ->
  let tt_subst_bound ~from term = tt_subst_bound ~from ~to_ term in
  match expand_head_term term with
  | TT_bound_var { index } as term -> (
      match Index.equal from index with
      | true -> expand_head_term to_
      | false -> term)
  | TT_free_var { level = _ } as term -> term
  (* TODO: expand subst into hole, magic could be done here *)
  | TT_hole _hole as term -> term
  | TT_forall { param; return } ->
      let param = tt_subst_bound ~from param in
      let return =
        let from = Index.(from + one) in
        tt_subst_bound ~from return
      in
      TT_forall { param; return }
  | TT_lambda { param; return } ->
      let param = tt_subst_bound ~from param in
      let return =
        let from = Index.(from + one) in
        tt_subst_bound ~from return
      in
      TT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let lambda = tt_subst_bound ~from lambda in
      let arg = tt_subst_bound ~from arg in
      TT_apply { lambda; arg }

and expand_subst_free : type a t. from:_ -> to_:t term -> a term -> core term =
 fun ~from ~to_ term ->
  let tt_subst_free term = tt_subst_free ~from ~to_ term in
  match expand_head_term term with
  | TT_bound_var { index = _ } as term -> term
  | TT_free_var { level } as term -> (
      match Level.equal from level with
      | true -> expand_head_term to_
      | false -> term)
  (* TODO: expand subst into hole, magic could be done here *)
  | TT_hole _hole as term -> term
  | TT_forall { param; return } ->
      let param = tt_subst_free param in
      let return = tt_subst_free return in
      TT_forall { param; return }
  | TT_lambda { param; return } ->
      let param = tt_subst_free param in
      let return = tt_subst_free return in
      TT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let lambda = tt_subst_free lambda in
      let arg = tt_subst_free arg in
      TT_apply { lambda; arg }

and expand_open_bound : type a. from:_ -> to_:_ -> a term -> core term =
 fun ~from ~to_ term ->
  let tt_open_bound ~from term = tt_open_bound ~from ~to_ term in
  match expand_head_term term with
  | TT_bound_var { index } as term -> (
      match Index.equal from index with
      | true -> TT_free_var { level = to_ }
      | false -> term)
  | TT_free_var { level = _ } as term -> term
  (* TODO: expand subst into hole, magic could be done here *)
  | TT_hole _hole as term -> term
  | TT_forall { param; return } ->
      let param = tt_open_bound ~from param in
      let return =
        let from = Index.(from + one) in
        tt_open_bound ~from return
      in
      TT_forall { param; return }
  | TT_lambda { param; return } ->
      let param = tt_open_bound ~from param in
      let return =
        let from = Index.(from + one) in
        tt_open_bound ~from return
      in
      TT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let lambda = tt_open_bound ~from lambda in
      let arg = tt_open_bound ~from arg in
      TT_apply { lambda; arg }

and expand_close_free : type a. from:_ -> to_:_ -> a term -> core term =
 fun ~from ~to_ term ->
  let tt_close_free ~to_ term = tt_close_free ~from ~to_ term in
  match expand_head_term term with
  | TT_bound_var { index = _ } as term -> term
  | TT_free_var { level } as term -> (
      match Level.equal from level with
      | true -> TT_bound_var { index = to_ }
      | false -> term)
  (* TODO: expand subst into hole, magic could be done here *)
  | TT_hole _hole as term -> term
  | TT_forall { param; return } ->
      let param = tt_close_free ~to_ param in
      let return =
        let to_ = Index.(to_ + one) in
        tt_close_free ~to_ return
      in
      TT_forall { param; return }
  | TT_lambda { param; return } ->
      let param = tt_close_free ~to_ param in
      let return =
        let to_ = Index.(to_ + one) in
        tt_close_free ~to_ return
      in
      TT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let lambda = tt_close_free ~to_ lambda in
      let arg = tt_close_free ~to_ arg in
      TT_apply { lambda; arg }
