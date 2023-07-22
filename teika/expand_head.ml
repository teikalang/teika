open Ttree

let rec expand_head_term : type a. a term -> core term =
 fun term ->
  match term with
  | TT_loc { term; loc = _ } -> expand_head_term term
  | TT_typed { term; annot = _ } -> expand_head_term term
  | TT_bound_var _ as term -> term
  | TT_free_var _ as term -> term
  | TT_hole { link } as term -> (
      (* TODO: move this to machinery *)
      match link == tt_nil with true -> term | false -> expand_head_term link)
  | TT_forall _ as term -> term
  | TT_lambda _ as term -> term
  | TT_apply { lambda; arg } -> (
      match expand_head_term lambda with
      | TT_lambda { param = _; return } ->
          (* TODO: param is not used here,
              but it would be cool to check when in debug *)
          let (Ex_term return) =
            Subst.subst_bound ~from:Index.zero ~to_:arg return
          in
          expand_head_term return
      | _lambda ->
          (* TODO: use expanded? *)
          TT_apply { lambda; arg })
  | TT_let { value; return } ->
      (* TODO: this could be done in O(1) with context extending *)
      let (Ex_term return) =
        Subst.subst_bound ~from:Index.zero ~to_:value return
      in
      expand_head_term return
  | TT_annot { term; annot = _ } -> expand_head_term term
