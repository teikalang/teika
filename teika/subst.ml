open Ttree
open Shift

let rec subst_term : type a. from:_ -> to_:_ -> a term -> ex_term =
 fun ~from ~to_ term ->
  let subst_term ~from term = subst_term ~from ~to_ term in
  let subst_pat ~from pat f = subst_pat ~from ~to_ pat f in

  match term with
  | TT_loc { term; loc } ->
      let (Ex_term term) = subst_term ~from term in
      Ex_term (TT_loc { term; loc })
  | TT_typed { term; annot } ->
      let (Ex_term annot) = subst_term ~from annot in
      let (Ex_term term) = subst_term ~from term in
      Ex_term (TT_typed { term; annot })
  | TT_var { offset = var } -> (
      match Offset.equal var from with
      | true -> Ex_term (shift_term ~offset:from to_)
      | false ->
          let var =
            match Offset.(var < from) with
            | true -> var
            | false -> Offset.(var - one)
          in
          Ex_term (TT_var { offset = var }))
  | TT_forall { param; return } ->
      subst_pat ~from param @@ fun ~from param ->
      let (Ex_term return) = subst_term ~from return in
      Ex_term (TT_forall { param; return })
  | TT_lambda { param; return } ->
      subst_pat ~from param @@ fun ~from param ->
      let (Ex_term return) = subst_term ~from return in
      Ex_term (TT_lambda { param; return })
  | TT_apply { lambda; arg } ->
      let (Ex_term lambda) = subst_term ~from lambda in
      let (Ex_term arg) = subst_term ~from arg in
      Ex_term (TT_apply { lambda; arg })
  | TT_annot { term; annot } ->
      let (Ex_term annot) = subst_term ~from annot in
      let (Ex_term term) = subst_term ~from term in
      Ex_term (TT_annot { term; annot })

and subst_pat :
    type a k. from:_ -> to_:_ -> a pat -> (from:_ -> a pat -> k) -> k =
 fun ~from ~to_ pat f ->
  let subst_term ~from term = subst_term ~from ~to_ term in
  let subst_pat pat f = subst_pat ~from ~to_ pat f in
  match pat with
  | TP_loc { pat; loc } ->
      subst_pat pat @@ fun ~from pat -> f ~from (TP_loc { pat; loc })
  | TP_typed { pat; annot } ->
      let (Ex_term annot) = subst_term ~from annot in
      subst_pat pat @@ fun ~from pat -> f ~from (TP_typed { pat; annot })
  | TP_var { var } ->
      let from = Offset.(from + one) in
      f ~from (TP_var { var })
  | TP_annot { pat; annot } ->
      let (Ex_term annot) = subst_term ~from annot in
      subst_pat pat @@ fun ~from pat -> f ~from (TP_annot { pat; annot })
