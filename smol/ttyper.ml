open Ttree

(* TODO: remove all failwith *)
let rec pat_var : type a. a pat -> _ =
 fun pat ->
  match pat with
  | TP_loc { pat; loc = _ } -> pat_var pat
  | TP_typed { pat; type_ = _ } -> pat_var pat
  | TP_var { var } -> var

let rec subst_term : type a. from:_ -> to_:_ -> a term -> ex_term =
 fun ~from ~to_ term ->
  let subst_term term = subst_term ~from ~to_ term in
  (* TODO: to go further on lazy substitutions, rename the function below *)
  let lazy_subst_term term = TT_subst { from; to_; term } in
  let subst_pat pat = subst_pat ~from ~to_ pat in
  match term with
  | TT_loc { term; loc } ->
      let (Ex_term term) = subst_term term in
      Ex_term (TT_loc { term; loc })
  | TT_typed { term; type_ } ->
      let (Ex_term term) = subst_term term in
      let type_ = lazy_subst_term type_ in
      Ex_term (TT_typed { term; type_ })
  | TT_subst _ as term -> Ex_term (lazy_subst_term term)
  | TT_var { var } -> (
      match Var.equal var from with
      | true -> Ex_term to_
      | false -> Ex_term (TT_var { var }))
  | TT_arrow { param; return } ->
      let param = subst_pat param in
      let (Ex_term return) =
        match Var.equal from (pat_var param) with
        | true -> Ex_term return
        | false -> subst_term return
      in
      Ex_term (TT_arrow { param; return })
  | TT_lambda { param; return } ->
      let param = subst_pat param in
      let (Ex_term return) =
        match Var.equal from (pat_var param) with
        | true -> Ex_term return
        | false -> subst_term return
      in
      Ex_term (TT_lambda { param; return })
  | TT_apply { lambda; arg } ->
      let (Ex_term lambda) = subst_term lambda in
      let (Ex_term arg) = subst_term arg in
      Ex_term (TT_apply { lambda; arg })

and subst_pat : type a. from:_ -> to_:_ -> a pat -> a pat =
 fun ~from ~to_ pat ->
  let lazy_subst_term term = TT_subst { from; to_; term } in
  let subst_pat pat = subst_pat ~from ~to_ pat in
  match pat with
  | TP_loc { pat; loc } ->
      let pat = subst_pat pat in
      TP_loc { pat; loc }
  | TP_typed { pat; type_ } ->
      let pat = subst_pat pat in
      let type_ = lazy_subst_term type_ in
      TP_typed { pat; type_ }
  | TP_var _ as pat -> pat

let rec expand_head : type a. a term -> _ =
 fun term ->
  match term with
  | TT_loc { term; loc = _ } -> expand_head term
  | TT_typed { term; type_ = _ } -> expand_head term
  | TT_subst { from; to_; term } ->
      let (Ex_term term) = subst_term ~from ~to_ term in
      expand_head term
  | TT_var _ as term -> term
  | TT_arrow _ as term -> term
  | TT_lambda _ as term -> term
  | TT_apply { lambda; arg } -> (
      match expand_head lambda with
      | TT_lambda { param; return } ->
          let (Ex_term term) =
            subst_term ~from:(pat_var param) ~to_:arg return
          in
          expand_head term
      | lambda -> TT_apply { lambda; arg })

let rename ~from ~to_ term =
  let to_ = TT_var { var = to_ } in
  subst_term ~from ~to_ term
