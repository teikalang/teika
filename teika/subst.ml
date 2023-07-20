open Ttree

let rec subst_bound_term : type a. from:_ -> to_:_ -> a term -> ex_term =
 fun ~from ~to_ term ->
  let subst_bound_term ~from term = subst_bound_term ~from ~to_ term in
  let subst_bound_pat ~from pat f = subst_bound_pat ~from ~to_ pat f in

  match term with
  | TT_loc { term; loc } ->
      let (Ex_term term) = subst_bound_term ~from term in
      Ex_term (TT_loc { term; loc })
  | TT_typed { term; annot } ->
      let (Ex_term annot) = subst_bound_term ~from annot in
      let (Ex_term term) = subst_bound_term ~from term in
      Ex_term (TT_typed { term; annot })
  | TT_bound_var { index } -> (
      match Index.equal index from with
      | true -> Ex_term to_
      | false -> Ex_term (TT_bound_var { index }))
  | TT_free_var { level } -> Ex_term (TT_free_var { level })
  | TT_forall { param; return } ->
      subst_bound_pat ~from param @@ fun ~from param ->
      let (Ex_term return) = subst_bound_term ~from return in
      Ex_term (TT_forall { param; return })
  | TT_lambda { param; return } ->
      subst_bound_pat ~from param @@ fun ~from param ->
      let (Ex_term return) = subst_bound_term ~from return in
      Ex_term (TT_lambda { param; return })
  | TT_apply { lambda; arg } ->
      let (Ex_term lambda) = subst_bound_term ~from lambda in
      let (Ex_term arg) = subst_bound_term ~from arg in
      Ex_term (TT_apply { lambda; arg })
  | TT_let { pat; value; return } ->
      let (Ex_term value) = subst_bound_term ~from value in
      subst_bound_pat ~from pat @@ fun ~from pat ->
      let (Ex_term return) = subst_bound_term ~from return in
      Ex_term (TT_let { pat; value; return })
  | TT_annot { term; annot } ->
      let (Ex_term annot) = subst_bound_term ~from annot in
      let (Ex_term term) = subst_bound_term ~from term in
      Ex_term (TT_annot { term; annot })

and subst_bound_pat :
    type a k. from:_ -> to_:_ -> a pat -> (from:_ -> a pat -> k) -> k =
 fun ~from ~to_ pat f ->
  let subst_bound_term ~from term = subst_bound_term ~from ~to_ term in
  let subst_bound_pat pat f = subst_bound_pat ~from ~to_ pat f in
  match pat with
  | TP_loc { pat; loc } ->
      subst_bound_pat pat @@ fun ~from pat -> f ~from (TP_loc { pat; loc })
  | TP_typed { pat; annot } ->
      let (Ex_term annot) = subst_bound_term ~from annot in
      subst_bound_pat pat @@ fun ~from pat -> f ~from (TP_typed { pat; annot })
  | TP_var { var } ->
      let from = Index.(from + one) in
      f ~from (TP_var { var })
  | TP_annot { pat; annot } ->
      let (Ex_term annot) = subst_bound_term ~from annot in
      subst_bound_pat pat @@ fun ~from pat -> f ~from (TP_annot { pat; annot })

let subst_bound = subst_bound_term
