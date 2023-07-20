open Ttree

let rec expand_head_term : type a. a term -> core term =
 fun term ->
  match term with
  | TT_loc { term; loc = _ } -> expand_head_term term
  | TT_typed { term; annot = _ } -> expand_head_term term
  | TT_bound_var _ as term -> term
  | TT_free_var _ as term -> term
  | TT_forall _ as term -> term
  | TT_lambda _ as term -> term
  | TT_apply { lambda; arg } -> (
      let lambda = expand_head_term lambda in
      match lambda with
      | TT_lambda { param; return } -> elim_apply ~pat:param ~return ~arg
      | _lambda -> TT_apply { lambda; arg })
  | TT_let { pat; value; return } -> elim_apply ~pat ~return ~arg:value
  | TT_annot { term; annot = _ } -> expand_head_term term

and expand_head_pat : type a. a pat -> _ =
 fun pat ->
  match pat with
  | TP_loc { pat; loc = _ } -> expand_head_pat pat
  | TP_typed { pat; annot = _ } -> expand_head_pat pat
  | TP_var { var = _ } as pat -> pat
  | TP_annot { pat; annot = _ } -> expand_head_pat pat

(* TODO: weird *)
and elim_apply : type p r a. pat:p pat -> return:r term -> arg:a term -> _ =
 fun ~pat ~return ~arg ->
  match (expand_head_pat pat, arg) with
  | TP_var { var = _name }, arg ->
      (* TODO: this could be done in O(1) with context extending *)
      let (Ex_term return) =
        Subst.subst_bound ~from:Index.zero ~to_:arg return
      in
      expand_head_term return
