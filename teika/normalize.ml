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

(* TODO: weird *)
and elim_apply ~pat ~return ~arg f =
  match (pat, arg) with
  | TP_annot { pat; annot = _ }, arg -> elim_apply ~pat ~return ~arg f
  | TP_loc { pat; loc = _ }, arg -> elim_apply ~pat ~return ~arg f
  | TP_var { var = _ }, arg ->
      with_offset ~offset:Offset.(zero - one) @@ fun () -> elim_var ~to_:arg f

and normalize_pat pat f =
  match pat with
  | TP_var { var } ->
      (* TODO: ensure all pat variables are wrapped with annotation *)
      with_var @@ fun () -> f (TP_var { var })
  | TP_annot { pat; annot } ->
      let* annot = normalize_term annot in
      normalize_pat pat @@ fun pat -> f (TP_annot { pat; annot })
  | TP_loc { pat; loc = _ } -> normalize_pat pat f
