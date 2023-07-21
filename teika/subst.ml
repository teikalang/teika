open Ttree

(* TODO: preserve physical identity more often *)
let rec subst_bound_term : type a. from:_ -> to_:_ -> a term -> ex_term =
 fun ~from ~to_ term ->
  let subst_bound_term ~from term = subst_bound_term ~from ~to_ term in

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
  | TT_hole hole -> (
      (* TODO: this should be in machinery *)
      match hole.link == tt_nil with
      | true -> Ex_term (TT_hole hole)
      | false -> subst_bound_term ~from hole.link)
  | TT_forall { param; return } ->
      let from = Index.(from + one) in
      let (Ex_term param) = subst_bound_term ~from param in
      let (Ex_term return) = subst_bound_term ~from return in
      Ex_term (TT_forall { param; return })
  | TT_lambda { param; return } ->
      let from = Index.(from + one) in
      let (Ex_term param) = subst_bound_term ~from param in
      let (Ex_term return) = subst_bound_term ~from return in
      Ex_term (TT_lambda { param; return })
  | TT_apply { lambda; arg } ->
      let (Ex_term lambda) = subst_bound_term ~from lambda in
      let (Ex_term arg) = subst_bound_term ~from arg in
      Ex_term (TT_apply { lambda; arg })
  | TT_let { value; return } ->
      let (Ex_term value) = subst_bound_term ~from value in
      let from = Index.(from + one) in
      let (Ex_term return) = subst_bound_term ~from return in
      Ex_term (TT_let { value; return })
  | TT_annot { term; annot } ->
      let (Ex_term annot) = subst_bound_term ~from annot in
      let (Ex_term term) = subst_bound_term ~from term in
      Ex_term (TT_annot { term; annot })

let subst_bound = subst_bound_term
