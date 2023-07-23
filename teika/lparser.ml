open Stree
open Ltree

exception Invalid_notation of { loc : Location.t }

let invalid_notation loc = raise (Invalid_notation { loc })

let rec parse_term term =
  let (ST { loc; desc }) = term in
  let term =
    match desc with
    | ST_parens { content } -> parse_term content
    | ST_var { var } -> LT_var { var }
    | ST_extension _ -> invalid_notation loc
    | ST_forall { param; return } ->
        let param = parse_pat param in
        let return = parse_term return in
        LT_forall { param; return }
    | ST_lambda { param; return } ->
        let param = parse_pat param in
        let return = parse_term return in
        LT_lambda { param; return }
    | ST_apply { lambda; arg } -> parse_apply lambda arg
    | ST_pair { left; right } -> (
        let (ST { loc; desc }) = left in
        match desc with
        | ST_bind _ ->
            let left = parse_bind left in
            let right = parse_bind right in
            LT_pair { left; right }
        | ST_annot _ ->
            let left = parse_annot left in
            let right = parse_annot right in
            LT_exists { left; right }
        | ST_var _ | ST_extension _ | ST_forall _ | ST_lambda _ | ST_apply _
        | ST_pair _ | ST_both _ | ST_semi _ | ST_parens _ | ST_braces _ ->
            invalid_notation loc)
    | ST_both _ -> invalid_notation loc
    | ST_bind _ -> invalid_notation loc
    | ST_semi { left; right } -> (
        let (ST { loc = left_loc; desc = left }) = left in
        match left with
        | ST_bind { bound = pat; value } ->
            let bound =
              let pat = parse_pat pat in
              let value = parse_term value in
              LBind { loc = left_loc; pat; value }
            in
            let return = parse_term right in
            LT_let { bound; return }
        | ST_var _ | ST_extension _ | ST_forall _ | ST_lambda _ | ST_apply _
        | ST_pair _ | ST_both _ | ST_semi _ | ST_annot _ | ST_parens _
        | ST_braces _ ->
            invalid_notation left_loc)
    | ST_annot { value; annot } ->
        let term = parse_term value in
        let annot = parse_term annot in
        LT_annot { term; annot }
    | ST_braces _ -> invalid_notation loc
  in
  LT_loc { term; loc }

and parse_apply lambda arg =
  let (ST { loc; desc }) = lambda in
  match desc with
  | ST_extension { extension } ->
      let payload = parse_term arg in
      let term = LT_extension { extension; payload } in
      (* TODO: this is not ideal *)
      LT_loc { term; loc }
  | _ ->
      let lambda = parse_term lambda in
      let arg = parse_term arg in
      LT_apply { lambda; arg }

and parse_pat term =
  let (ST { loc; desc }) = term in
  let pat =
    match desc with
    | ST_parens { content } -> parse_pat content
    | ST_var { var } -> LP_var { var }
    | ST_pair { left; right } ->
        let left = parse_pat left in
        let right = parse_pat right in
        LP_pair { left; right }
    | ST_annot { value = pat; annot } ->
        let pat = parse_pat pat in
        let annot = parse_term annot in
        LP_annot { pat; annot }
    | ST_extension _ | ST_forall _ | ST_lambda _ | ST_apply _ | ST_both _
    | ST_bind _ | ST_semi _ | ST_braces _ ->
        invalid_notation loc
  in
  LP_loc { pat; loc }

and parse_annot annot =
  let (ST { loc; desc }) = annot in
  match desc with
  | ST_parens { content } -> parse_annot content
  | ST_annot { value = pat; annot } ->
      let pat = parse_pat pat in
      let annot = parse_term annot in
      LAnnot { loc; pat; annot }
  | ST_var _ | ST_extension _ | ST_forall _ | ST_lambda _ | ST_apply _
  | ST_pair _ | ST_both _ | ST_bind _ | ST_semi _ | ST_braces _ ->
      invalid_notation loc

and parse_bind bind =
  let (ST { loc; desc }) = bind in
  match desc with
  | ST_parens { content } -> parse_bind content
  | ST_bind { bound; value } ->
      let pat = parse_pat bound in
      let value = parse_term value in
      LBind { loc; pat; value }
  | ST_var _ | ST_extension _ | ST_forall _ | ST_lambda _ | ST_apply _
  | ST_pair _ | ST_both _ | ST_semi _ | ST_annot _ | ST_braces _ ->
      invalid_notation loc

(* TODO: rename this*)
let from_stree = parse_term
