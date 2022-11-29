open Stree
open Ltree

exception Invalid_notation of { loc : Location.t }

let invalid_notation loc = raise (Invalid_notation { loc })

let rec from_stree term =
  let (ST { loc; desc }) = term in
  match desc with
  | ST_var { var } -> lt_var loc ~var
  | ST_forall { param; return } ->
      let param = extract_pat param in
      let return = from_stree return in
      lt_forall loc ~param ~return
  | ST_lambda { param; return } ->
      let param = extract_pat param in
      let return = from_stree return in
      lt_lambda loc ~param ~return
  | ST_apply { lambda; arg } ->
      let lambda = from_stree lambda in
      let arg = from_stree arg in
      lt_apply loc ~lambda ~arg
  | ST_pair { left; right } -> (
      let (ST { loc; desc }) = left in
      match desc with
      | ST_bind _ ->
          let left = extract_bind left in
          let right = extract_bind right in
          lt_pair loc ~left ~right
      | ST_annot _ ->
          let left = extract_annot left in
          let right = extract_annot right in
          lt_exists loc ~left ~right
      | ST_var _ | ST_forall _ | ST_lambda _ | ST_apply _ | ST_pair _
      | ST_both _ | ST_semi _ | ST_parens _ | ST_braces _ ->
          invalid_notation loc)
  | ST_both _ -> invalid_notation loc
  | ST_bind _ -> invalid_notation loc
  | ST_semi { left; right } -> (
      let (ST { loc = left_loc; desc = left }) = left in
      match left with
      | ST_bind { bound = pat; value } ->
          let bound =
            let pat = extract_pat pat in
            let value = from_stree value in
            lbind left_loc ~pat ~value
          in
          let return = from_stree right in
          lt_let loc ~bound ~return
      | ST_var _ | ST_forall _ | ST_lambda _ | ST_apply _ | ST_pair _
      | ST_both _ | ST_semi _ | ST_annot _ | ST_parens _ | ST_braces _ ->
          invalid_notation left_loc)
  | ST_annot { value; annot } ->
      let value = from_stree value in
      let annot = from_stree annot in
      lt_annot loc ~value ~annot
  | ST_parens { content } -> from_stree content
  | ST_braces _ -> invalid_notation loc

and extract_pat term =
  let (ST { loc; desc }) = term in
  match desc with
  | ST_parens { content } -> extract_pat content
  | ST_var { var } -> lp_var loc ~var
  | ST_pair { left; right } ->
      let left = extract_pat left in
      let right = extract_pat right in
      lp_pair loc ~left ~right
  | ST_annot { value = pat; annot } ->
      let pat = extract_pat pat in
      let annot = from_stree annot in
      lp_annot loc ~pat ~annot
  | ST_forall _ | ST_lambda _ | ST_apply _ | ST_both _ | ST_bind _ | ST_semi _
  | ST_braces _ ->
      invalid_notation loc

and extract_annot annot =
  let (ST { loc; desc }) = annot in
  match desc with
  | ST_parens { content } -> extract_annot content
  | ST_annot { value = pat; annot } ->
      let pat = extract_pat pat in
      let annot = from_stree annot in
      lannot loc ~pat ~annot
  | ST_var _ | ST_forall _ | ST_lambda _ | ST_apply _ | ST_pair _ | ST_both _
  | ST_bind _ | ST_semi _ | ST_braces _ ->
      invalid_notation loc

and extract_bind bind =
  let (ST { loc; desc }) = bind in
  match desc with
  | ST_parens { content } -> extract_bind content
  | ST_bind { bound; value } ->
      let pat = extract_pat bound in
      let value = from_stree value in
      lbind loc ~pat ~value
  | ST_var _ | ST_forall _ | ST_lambda _ | ST_apply _ | ST_pair _ | ST_both _
  | ST_semi _ | ST_annot _ | ST_braces _ ->
      invalid_notation loc
