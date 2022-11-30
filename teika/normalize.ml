open Ttree
open Context.Normalize_context

(* TODO: with subtyping, should normalize be able to recover information? *)

let rec normalize_term term =
  match term with
  (* TODO: is removing those ok / idea? *)
  | TT_annot { term; annot = _ } -> normalize_term term
  | TT_loc { term; loc = _ } -> normalize_term term
  | TT_offset { term; offset } ->
      with_offset ~offset @@ fun () -> normalize_term term
  | TT_var { offset } -> repr_var ~var:offset
  | TT_forall { param; return } ->
      normalize_pat param @@ fun param ->
      let+ return = normalize_term return in
      TT_forall { param; return }
  | TT_lambda { param; return } ->
      normalize_pat param @@ fun param ->
      let+ return = normalize_term return in
      TT_lambda { param; return }
  | TT_apply { lambda; arg } -> (
      let* lambda = normalize_term lambda in
      let* arg = normalize_term arg in
      match lambda with
      | TT_lambda { param; return } ->
          (* TODO: match every case below *)
          elim_apply ~pat:param ~return ~arg @@ fun () -> normalize_term return
      | _ -> return @@ TT_apply { lambda; arg })
  | TT_exists { left; right } ->
      normalize_annot left @@ fun left ->
      normalize_annot right @@ fun right -> return @@ TT_exists { left; right }
  | TT_pair { left; right } ->
      normalize_bind left @@ fun left ->
      normalize_bind right @@ fun right -> return @@ TT_pair { left; right }
  | TT_let { bound; return } ->
      normalize_bind bound @@ fun bound ->
      let (TBind { loc = _lambda_loc; pat = param; value = arg }) = bound in
      let lambda = TT_lambda { param; return } in
      normalize_term (TT_apply { lambda; arg })

(* TODO: weird *)
and elim_apply ~pat ~return ~arg f =
  match (pat, arg) with
  | TP_var { var = _ }, arg ->
      with_offset ~offset:Offset.(zero - one) @@ fun () -> elim_var ~to_:arg f
  | ( TP_pair { left = left_pat; right = right_pat },
      TT_pair { left = left_arg; right = right_arg } ) ->
      (* TODO: what if bind pat is not just var? This is wrong *)
      let (TBind { loc = _; pat = _; value = left_arg }) = left_arg in
      let (TBind { loc = _; pat = _; value = right_arg }) = right_arg in
      elim_apply ~pat:left_pat ~return ~arg:left_arg @@ fun () ->
      elim_apply ~pat:right_pat ~return ~arg:right_arg f
  | (TP_pair _ as param), arg ->
      (* TODO: proper locations? *)
      let lambda = TT_lambda { param; return } in
      Context.Normalize_context.return @@ TT_apply { lambda; arg }
  | TP_annot { pat; annot = _ }, arg -> elim_apply ~pat ~return ~arg f
  | TP_loc { pat; loc = _ }, arg -> elim_apply ~pat ~return ~arg f

and normalize_pat pat f =
  match pat with
  | TP_var { var } ->
      (* TODO: ensure all pat variables are wrapped with annotation *)
      with_var @@ fun () -> f (TP_var { var })
  | TP_pair { left; right } ->
      normalize_pat left @@ fun left ->
      normalize_pat right @@ fun right -> f (TP_pair { left; right })
  | TP_annot { pat; annot } ->
      let* annot = normalize_term annot in
      normalize_pat pat @@ fun pat -> f (TP_annot { pat; annot })
  | TP_loc { pat; loc = _ } -> normalize_pat pat f

and normalize_annot annot f =
  let (TAnnot { loc; pat; annot }) = annot in
  let* annot = normalize_term annot in
  normalize_pat pat @@ fun pat -> f (TAnnot { loc; pat; annot })

and normalize_bind bind f =
  let (TBind { loc; pat; value }) = bind in
  let* value = normalize_term value in
  normalize_pat pat @@ fun pat ->
  elim_var ~to_:value @@ fun () -> f (TBind { loc; pat; value })
