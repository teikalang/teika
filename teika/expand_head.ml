open Ttree

(* TODO: better place than here  *)
let tt_subst term subst = TT_subst { subst; term }

let rec expand_head_term : type a. a term -> core term =
 fun term ->
  match term with
  | TT_loc { term; loc = _ } -> expand_head_term term
  | TT_typed { term; annot = _ } -> expand_head_term term
  | TT_subst { subst; term } -> expand_subst_term ~subst term
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

and expand_subst_term : type a. subst:subst -> a term -> core term =
 fun ~subst term ->
  let expand_subst_param term = expand_subst_param ~subst term in
  let when_bound_var index =
    match subst with
    | TS_subst_bound { from; to_ } -> (
        match Index.equal from index with
        | true -> expand_head_term to_
        | false -> TT_bound_var { index })
    | TS_subst_free { from = _; to_ = _ } -> TT_bound_var { index }
    | TS_open_bound { from; to_ } -> (
        match Index.equal from index with
        | true -> TT_free_var { level = to_; alias = None }
        | false -> TT_bound_var { index })
    | TS_close_free { from = _; to_ = _ } -> TT_bound_var { index }
  in
  let when_free_var level =
    match subst with
    | TS_subst_bound { from = _; to_ = _ } ->
        TT_free_var { level; alias = None }
    | TS_subst_free { from; to_ } -> (
        match Level.equal from level with
        | true -> expand_head_term to_
        | false -> TT_free_var { level; alias = None })
    | TS_open_bound { from = _; to_ = _ } -> TT_free_var { level; alias = None }
    | TS_close_free { from; to_ } -> (
        match Level.equal from level with
        | true -> TT_bound_var { index = to_ }
        | false -> TT_free_var { level; alias = None })
  in
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
  match expand_head_term term with
  | TT_bound_var { index } -> when_bound_var index
  | TT_free_var { level; alias = _ } -> when_free_var level
  | TT_hole { hole = _ } as term -> term
  | TT_forall { param; return } ->
      let param = expand_subst_param param in
      let return = tt_subst return @@ with_var subst in
      TT_forall { param; return }
  | TT_lambda { param; return } ->
      let param = expand_subst_param param in
      let return = tt_subst return @@ with_var subst in
      TT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let lambda = tt_subst lambda subst in
      let arg = tt_subst arg subst in
      TT_apply { lambda; arg }
  | TT_self { var; body } ->
      let body = tt_subst body @@ with_var subst in
      TT_self { var; body }
  | TT_fix { var; body } ->
      let body = tt_subst body @@ with_var subst in
      TT_fix { var; body }
  | TT_unroll { term } ->
      let term = tt_subst term subst in
      TT_unroll { term }
  | TT_string _ as term -> term
  | TT_native _ as term -> term

and expand_subst_param : subst:subst -> _ -> _ =
 fun ~subst pat ->
  let (TP_typed { pat; annot }) = pat in
  let annot = tt_subst annot subst in
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
