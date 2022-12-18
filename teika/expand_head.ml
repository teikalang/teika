open Ttree

let rec expand_head : type a. a term -> core term =
 fun term ->
  match term with
  | TT_loc { term; loc = _ } -> expand_head term
  | TT_typed { term; annot = _ } -> expand_head term
  | TT_var _ -> term
  | TT_forall _ -> term
  | TT_lambda _ -> term
  | TT_apply { lambda; arg } -> (
      let lambda = expand_head lambda in
      match lambda with
      | TT_lambda { param; return } -> elim_apply ~pat:param ~return ~arg
      | _lambda -> TT_apply { lambda; arg })
  | TT_let { pat; value; return } -> elim_apply ~pat ~return ~arg:value
  | TT_annot { term; annot = _ } -> expand_head term

(* TODO: weird *)
and elim_apply : type p r a. pat:p pat -> return:r term -> arg:a term -> _ =
 fun ~pat ~return ~arg ->
  match (pat, arg) with
  | TP_loc { pat; loc = _ }, arg -> elim_apply ~pat ~return ~arg
  | TP_typed { pat; annot = _ }, arg -> elim_apply ~pat ~return ~arg
  | TP_var { var = _ }, arg ->
      let from = Offset.zero in
      let (Ex_term return) = Subst.subst_term ~from ~to_:arg return in
      expand_head return
  | TP_annot { pat; annot = _ }, arg -> elim_apply ~pat ~return ~arg
