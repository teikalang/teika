open Ttree

let rec expand_head_term : type a. a term -> core term =
 fun term ->
  match term with
  | TT_loc { term; loc = _ } -> expand_head_term term
  | TT_typed { term; annot = _ } -> expand_head_term term
  | TT_subst { subst; term } -> expand_subst ~subst term
  | TT_bound_var _ as term -> term
  | TT_free_var { level = _; alias = Some alias } -> expand_head_term alias
  | TT_free_var _ as term -> term
  | TT_hole { hole } as term -> (
      (* TODO: path compression *)
      (* TODO: move this to machinery *)
      let (Ex_term link) = hole.link in
      match is_tt_nil link with
      | true -> term
      | false ->
          (* TODO: path compression *)
          expand_head_term link)
  | TT_forall _ as term -> term
  | TT_lambda _ as term -> term
  | TT_apply { lambda; arg } as term -> (
      match expand_head_term lambda with
      | TT_lambda { param = _; return } ->
          (* TODO: param is not used here,
             but it would be cool to check when in debug *)
          (* TODO: this could be done in O(1) with context extending *)
          expand_head_term @@ tt_subst_bound ~from:Index.zero ~to_:arg return
      | TT_native { native } -> expand_head_native native ~arg
      | _lambda ->
          (* TODO: use expanded? *)
          term)
  | TT_self _ as term -> term
  | TT_fix _ as term -> term
  | TT_unroll _ as term -> term
  | TT_unfold { term } -> expand_head_term term
  | TT_let { bound = _; value; return } ->
      expand_head_term @@ tt_subst_bound ~from:Index.zero ~to_:value return
  | TT_annot { term; annot = _ } -> expand_head_term term
  | TT_string _ as term -> term
  | TT_native _ as term -> term

and expand_head_native : type a. _ -> arg:a term -> core term =
 fun native ~arg -> match native with TN_debug -> expand_head_term arg

and expand_subst : type a. subst:subst -> a term -> core term =
 fun ~subst term ->
  match subst with
  | TS_subst_bound { from; to_ } -> expand_subst_bound_term ~from ~to_ term
  | TS_subst_free { from; to_ } -> expand_subst_free_term ~from ~to_ term
  | TS_open_bound { from; to_ } -> expand_open_bound_term ~from ~to_ term
  | TS_close_free { from; to_ } -> expand_close_free_term ~from ~to_ term

and expand_subst_bound_term :
    type a t. from:_ -> to_:t term -> a term -> core term =
 fun ~from ~to_ term ->
  let expand_subst_bound_param ~from pat =
    expand_subst_bound_param ~from ~to_ pat
  in
  let tt_subst_bound ~from term = tt_subst_bound ~from ~to_ term in
  match expand_head_term term with
  | TT_bound_var { index } as term -> (
      match Index.equal from index with
      | true -> expand_head_term to_
      | false -> term)
  | TT_free_var { level = _; alias = _ } as term -> term
  | TT_hole { hole = _ } as term -> term
  | TT_forall { param; return } ->
      let param = expand_subst_bound_param ~from param in
      let return =
        let from = Index.(from + one) in
        tt_subst_bound ~from return
      in
      TT_forall { param; return }
  | TT_lambda { param; return } ->
      let param = expand_subst_bound_param ~from param in
      let return =
        let from = Index.(from + one) in
        tt_subst_bound ~from return
      in
      TT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let lambda = tt_subst_bound ~from lambda in
      let arg = tt_subst_bound ~from arg in
      TT_apply { lambda; arg }
  | TT_self { var; body } ->
      let body =
        let from = Index.(from + one) in
        tt_subst_bound ~from body
      in
      TT_self { var; body }
  | TT_fix { var; body } ->
      let body =
        let from = Index.(from + one) in
        tt_subst_bound ~from body
      in
      TT_fix { var; body }
  | TT_unroll { term } ->
      let term = tt_subst_bound ~from term in
      TT_unroll { term }
  | TT_string _ as term -> term
  | TT_native _ as term -> term

and expand_subst_bound_param : type t. from:_ -> to_:t term -> _ -> _ =
 fun ~from ~to_ pat ->
  let (TP_typed { pat; annot }) = pat in
  let annot = tt_subst_bound ~from ~to_ annot in
  TP_typed { pat; annot }

and expand_subst_free_term :
    type a t. from:_ -> to_:t term -> a term -> core term =
 fun ~from ~to_ term ->
  let expand_subst_free_param pat = expand_subst_free_param ~from ~to_ pat in
  let tt_subst_free term = tt_subst_free ~from ~to_ term in
  match expand_head_term term with
  | TT_bound_var { index = _ } as term -> term
  | TT_free_var { level; alias = _ } as term -> (
      match Level.equal from level with
      | true -> expand_head_term to_
      | false -> term)
  | TT_hole { hole = _ } as term -> term
  | TT_forall { param; return } ->
      let param = expand_subst_free_param param in
      let return = tt_subst_free return in
      TT_forall { param; return }
  | TT_lambda { param; return } ->
      let param = expand_subst_free_param param in
      let return = tt_subst_free return in
      TT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let lambda = tt_subst_free lambda in
      let arg = tt_subst_free arg in
      TT_apply { lambda; arg }
  | TT_self { var; body } ->
      let body = tt_subst_free body in
      TT_self { var; body }
  | TT_fix { var; body } ->
      let body = tt_subst_free body in
      TT_fix { var; body }
  | TT_unroll { term } ->
      let term = tt_subst_free term in
      TT_unroll { term }
  | TT_string _ as term -> term
  | TT_native _ as term -> term

and expand_subst_free_param : type t. from:_ -> to_:t term -> _ -> _ =
 fun ~from ~to_ pat ->
  let (TP_typed { pat; annot }) = pat in
  let annot = tt_subst_free ~from ~to_ annot in
  TP_typed { pat; annot }

and expand_open_bound_term : type a. from:_ -> to_:_ -> a term -> core term =
 fun ~from ~to_ term ->
  let expand_open_bound_param ~from pat =
    expand_open_bound_param ~from ~to_ pat
  in
  let tt_open_bound ~from term = tt_open_bound ~from ~to_ term in
  match expand_head_term term with
  | TT_bound_var { index } as term -> (
      match Index.equal from index with
      | true -> TT_free_var { level = to_; alias = None }
      | false -> term)
  | TT_free_var { level = _; alias = _ } as term -> term
  | TT_hole { hole = _ } as term -> term
  | TT_forall { param; return } ->
      let param = expand_open_bound_param ~from param in
      let return =
        let from = Index.(from + one) in
        tt_open_bound ~from return
      in
      TT_forall { param; return }
  | TT_lambda { param; return } ->
      let param = expand_open_bound_param ~from param in
      let return =
        let from = Index.(from + one) in
        tt_open_bound ~from return
      in
      TT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let lambda = tt_open_bound ~from lambda in
      let arg = tt_open_bound ~from arg in
      TT_apply { lambda; arg }
  | TT_self { var; body } ->
      let body =
        let from = Index.(from + one) in
        tt_open_bound ~from body
      in
      TT_self { var; body }
  | TT_fix { var; body } ->
      let body =
        let from = Index.(from + one) in
        tt_open_bound ~from body
      in
      TT_fix { var; body }
  | TT_unroll { term } ->
      let term = tt_open_bound ~from term in
      TT_unroll { term }
  | TT_string _ as term -> term
  | TT_native _ as term -> term

and expand_open_bound_param ~from ~to_ pat =
  let (TP_typed { pat; annot }) = pat in
  let annot = tt_open_bound ~from ~to_ annot in
  TP_typed { pat; annot }

and expand_close_free_term : type a. from:_ -> to_:_ -> a term -> core term =
 fun ~from ~to_ term ->
  let expand_close_free_param ~to_ pat =
    expand_close_free_param ~from ~to_ pat
  in
  let tt_close_free ~to_ term = tt_close_free ~from ~to_ term in
  match expand_head_term term with
  | TT_bound_var { index = _ } as term -> term
  | TT_free_var { level; alias = _ } as term -> (
      match Level.equal from level with
      | true -> TT_bound_var { index = to_ }
      | false -> term)
  | TT_hole { hole = _ } as term -> term
  | TT_forall { param; return } ->
      let param = expand_close_free_param ~to_ param in
      let return =
        let to_ = Index.(to_ + one) in
        tt_close_free ~to_ return
      in
      TT_forall { param; return }
  | TT_lambda { param; return } ->
      let param = expand_close_free_param ~to_ param in
      let return =
        let to_ = Index.(to_ + one) in
        tt_close_free ~to_ return
      in
      TT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let lambda = tt_close_free ~to_ lambda in
      let arg = tt_close_free ~to_ arg in
      TT_apply { lambda; arg }
  | TT_self { var; body } ->
      let body =
        let to_ = Index.(to_ + one) in
        tt_close_free ~to_ body
      in
      TT_self { var; body }
  | TT_fix { var; body } ->
      let body =
        let to_ = Index.(to_ + one) in
        tt_close_free ~to_ body
      in
      TT_fix { var; body }
  | TT_unroll { term } ->
      let term = tt_close_free ~to_ term in
      TT_unroll { term }
  | TT_string _ as term -> term
  | TT_native _ as term -> term

and expand_close_free_param : from:_ -> to_:_ -> _ -> _ =
 fun ~from ~to_ pat ->
  let (TP_typed { pat; annot }) = pat in
  let annot = tt_close_free ~from ~to_ annot in
  TP_typed { pat; annot }

and expand_head_pat : type a. a pat -> _ =
 fun pat ->
  match pat with
  | TP_typed { pat; annot = _ } -> expand_head_pat pat
  | TP_hole { hole } as pat -> (
      (* TODO: path compression *)
      (* TODO: move this to machinery *)
      let link = hole.link in
      match is_tp_nil link with
      | true -> pat
      | false ->
          (* TODO: path compression *)
          expand_head_pat link)
  | TP_var _ as pat -> pat
