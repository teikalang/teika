open Ttree

let rec shift_term : type a. by:_ -> depth:_ -> a term -> a term =
 fun ~by ~depth term ->
  let shift_term ~depth term = shift_term ~by ~depth term in
  let shift_pat ~depth pat f = shift_pat ~by ~depth pat f in
  match term with
  | TT_loc { term; loc } ->
      let term = shift_term ~depth term in
      TT_loc { term; loc }
  | TT_typed { term; annot } ->
      let term = shift_term ~depth term in
      let annot = shift_term ~depth annot in
      TT_typed { term; annot }
  | TT_var { offset = var } ->
      let var =
        match Offset.(var < depth) with
        | true -> var
        | false -> Offset.(var + by)
      in
      TT_var { offset = var }
  | TT_forall { param; return } ->
      shift_pat ~depth param @@ fun ~depth param ->
      let return = shift_term ~depth return in
      TT_forall { param; return }
  | TT_lambda { param; return } ->
      shift_pat ~depth param @@ fun ~depth param ->
      let return = shift_term ~depth return in
      TT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let lambda = shift_term ~depth lambda in
      let arg = shift_term ~depth arg in
      TT_apply { lambda; arg }
  | TT_annot { term; annot } ->
      let term = shift_term ~depth term in
      let annot = shift_term ~depth annot in
      TT_annot { term; annot }

and shift_pat :
    type a k. by:_ -> depth:_ -> a pat -> (depth:_ -> a pat -> k) -> k =
 fun ~by ~depth pat f ->
  let shift_term ~depth term = shift_term ~by ~depth term in
  let shift_pat ~depth pat f = shift_pat ~by ~depth pat f in
  match pat with
  | TP_loc { pat; loc } ->
      shift_pat ~depth pat @@ fun ~depth pat -> f ~depth (TP_loc { pat; loc })
  | TP_typed { pat; annot } ->
      let annot = shift_term ~depth annot in
      shift_pat ~depth pat @@ fun ~depth pat ->
      f ~depth (TP_typed { pat; annot })
  | TP_var { var } ->
      let depth = Offset.(depth + one) in
      f ~depth (TP_var { var })
  | TP_annot { pat; annot } ->
      let annot = shift_term ~depth annot in
      shift_pat ~depth pat @@ fun ~depth pat ->
      f ~depth (TP_annot { pat; annot })

let shift_term ~offset term =
  let by = offset in
  let depth = Offset.zero in
  let term = shift_term ~by ~depth term in
  term
