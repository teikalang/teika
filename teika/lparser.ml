open Stree
open Ltree

exception Invalid_notation of { loc : Location.t }

let invalid_notation loc = raise (Invalid_notation { loc })

let rec from_stree term =
  let (ST { loc; desc }) = term in
  match desc with
  | ST_var { var } -> lt_var loc ~var
  | ST_forall { param; return } ->
      let param = extract_annot param in
      let return = from_stree return in
      lt_forall loc ~param ~return
  | ST_lambda { param; return } ->
      let param = extract_annot param in
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
      | ST_bind { bound; value } -> extract_semi loc ~bound ~value ~return:right
      | ST_var _ | ST_forall _ | ST_lambda _ | ST_apply _ | ST_pair _
      | ST_both _ | ST_semi _ | ST_annot _ | ST_parens _ | ST_braces _ ->
          invalid_notation left_loc)
  | ST_annot { value; type_ } ->
      let value = from_stree value in
      let type_ = from_stree type_ in
      lt_annot loc ~value ~type_
  | ST_parens { content } -> from_stree content
  | ST_braces _ -> invalid_notation loc

and extract_var term =
  let (ST { loc; desc }) = term in
  match desc with
  | ST_parens { content } -> extract_var content
  | ST_var { var } -> var
  | ST_forall _ | ST_lambda _ | ST_apply _ | ST_pair _ | ST_both _ | ST_bind _
  | ST_semi _ | ST_annot _ | ST_braces _ ->
      invalid_notation loc

and extract_annot annot =
  let (ST { loc; desc }) = annot in
  match desc with
  | ST_parens { content } -> extract_annot content
  | ST_annot { value; type_ } ->
      let var = extract_var value in
      let type_ = from_stree type_ in
      lannot loc ~var ~type_
  | ST_var _ | ST_forall _ | ST_lambda _ | ST_apply _ | ST_pair _ | ST_both _
  | ST_bind _ | ST_semi _ | ST_braces _ ->
      invalid_notation loc

and extract_bind bind =
  let (ST { loc; desc }) = bind in
  match desc with
  | ST_parens { content } -> extract_bind content
  | ST_bind { bound; value } ->
      let var = extract_var bound in
      let value = from_stree value in
      lbind loc ~var ~value
  | ST_var _ | ST_forall _ | ST_lambda _ | ST_apply _ | ST_pair _ | ST_both _
  | ST_semi _ | ST_annot _ | ST_braces _ ->
      invalid_notation loc

and extract_semi loc ~bound ~value ~return =
  let (ST { loc = bound_loc; desc = bound }) = bound in
  match bound with
  | ST_parens { content } -> extract_semi loc ~bound:content ~value ~return
  | ST_var { var } ->
      let value = from_stree value in
      let bound = lbind bound_loc ~var ~value in
      let return = from_stree return in
      lt_let loc ~bound ~return
  | ST_pair { left; right } ->
      let left = extract_var left in
      let right = extract_var right in
      let value = from_stree value in
      let return = from_stree return in
      lt_unpair loc ~left ~right ~pair:value ~return
  | ST_forall _ | ST_lambda _ | ST_apply _ | ST_both _ | ST_bind _ | ST_semi _
  | ST_annot _ | ST_braces _ ->
      invalid_notation bound_loc
