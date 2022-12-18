open Ttree
open Context.Normalize_context

(* TODO: with subtyping, should normalize be able to recover information? *)

let rec normalize_term : type a. a term -> _ =
 fun term ->
  match term with
  (* TODO: is removing those ok / ideal? *)
  | TT_annot { term; annot = _ } -> normalize_term term
  | TT_loc { term; loc = _ } -> normalize_term term
  | TT_var { offset } -> return @@ Ex_term (TT_var { offset })
  | TT_forall { param; return } ->
      normalize_pat param @@ fun param ->
      let+ (Ex_term return) = normalize_term return in
      Ex_term (TT_forall { param; return })
  | TT_lambda { param; return } ->
      normalize_pat param @@ fun param ->
      let+ (Ex_term return) = normalize_term return in
      Ex_term (TT_lambda { param; return })
  | TT_apply { lambda; arg } -> (
      let* (Ex_term lambda) = normalize_term lambda in
      let* (Ex_term arg) = normalize_term arg in
      match lambda with
      | TT_lambda { param; return } ->
          (* TODO: match every case below *)
          elim_apply ~pat:param ~return ~arg
      | _ -> return @@ Ex_term (TT_apply { lambda; arg }))

(* TODO: weird *)
and elim_apply : type p r a. pat:p pat -> return:r term -> arg:a term -> _ =
 fun ~pat ~return ~arg ->
  match (pat, arg) with
  | TP_annot { pat; annot = _ }, arg -> elim_apply ~pat ~return ~arg
  | TP_loc { pat; loc = _ }, arg -> elim_apply ~pat ~return ~arg
  | TP_var { var = _ }, arg ->
      let from = Offset.zero in
      let (Ex_term return) = Subst.subst_term ~from ~to_:arg return in
      (* TODO: is this normalize needed? *)
      normalize_term return

and normalize_pat : type a. a pat -> (a pat -> _) -> _ =
 fun pat f ->
  match pat with
  | TP_var { var = name } ->
      (* TODO: ensure all pat variables are wrapped with annotation *)
      with_var @@ fun () -> f (TP_var { var = name })
  | TP_annot { pat; annot } ->
      let* (Ex_term annot) = normalize_term annot in
      normalize_pat pat @@ fun pat -> f (TP_annot { pat; annot })
  | TP_loc { pat; loc } ->
      normalize_pat pat @@ fun pat -> f (TP_loc { pat; loc })
