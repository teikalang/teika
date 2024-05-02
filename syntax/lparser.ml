open Ctree
open Ltree

exception Invalid_notation of { loc : Location.t }

let invalid_notation loc = raise (Invalid_notation { loc })

let rec parse_term term =
  let (CTerm { term; loc }) = term in
  match term with
  | CT_parens { content } -> parse_term content
  | CT_var { var } -> lterm ~loc @@ LT_var { var }
  | CT_extension _ -> invalid_notation loc
  | CT_forall { param; return } ->
      let param = parse_pat param in
      let return = parse_term return in
      lterm ~loc @@ LT_forall { param; return }
  | CT_lambda { param; return } ->
      let param = parse_pat param in
      let return = parse_term return in
      lterm ~loc @@ LT_lambda { param; return }
  | CT_apply { lambda; arg } -> parse_apply ~loc ~lambda ~arg
  | CT_pair { left = _; right = _ } -> invalid_notation loc
  | CT_both _ -> invalid_notation loc
  | CT_bind _ -> invalid_notation loc
  | CT_semi { left; right } -> parse_semi ~loc ~left ~right
  | CT_annot { value; annot } ->
      let term = parse_term value in
      let annot = parse_term annot in
      lterm ~loc @@ LT_annot { term; annot }
  | CT_string { literal } -> lterm ~loc @@ LT_string { literal }
  | CT_number _ -> invalid_notation loc
  | CT_braces _ -> invalid_notation loc

and parse_semi ~loc ~left ~right =
  let (CTerm { term = left; loc = left_loc }) = left in
  match left with
  | CT_parens { content = left } -> parse_semi ~loc ~left ~right
  | CT_bind { bound; value } ->
      let bound = parse_pat bound in
      let value = parse_term value in
      let return = parse_term right in
      lterm ~loc @@ LT_let { bound; value; return }
  | CT_annot { value = bound; annot } ->
      let bound = parse_pat bound in
      let annot = parse_term annot in
      let return = parse_term right in
      lterm ~loc @@ LT_hoist { bound; annot; return }
  | CT_var _ | CT_extension _ | CT_forall _ | CT_lambda _ | CT_apply _
  | CT_pair _ | CT_both _ | CT_semi _ | CT_string _ | CT_number _ | CT_braces _
    ->
      invalid_notation left_loc

and parse_apply ~loc ~lambda ~arg =
  (* TODO: is this fallback of locations okay? *)
  match
    let (CTerm { term = lambda; loc = _ }) = lambda in
    lambda
  with
  (* TODO: CT_parens? *)
  | CT_extension { extension } ->
      let payload = parse_term arg in
      lterm ~loc @@ LT_extension { extension; payload }
  (* TODO: remove catch all *)
  | _ ->
      let lambda = parse_term lambda in
      let arg = parse_term arg in
      lterm ~loc @@ LT_apply { lambda; arg }

and parse_pat pat =
  (* TODO: Stack of locs? *)
  let (CTerm { term = pat; loc }) = pat in
  match pat with
  | CT_parens { content } -> parse_pat content
  | CT_var { var } -> lpat ~loc @@ LP_var { var }
  | CT_pair { left = _; right = _ } -> invalid_notation loc
  | CT_annot { value = pat; annot } ->
      let pat = parse_pat pat in
      let annot = parse_term annot in
      lpat ~loc @@ LP_annot { pat; annot }
  | CT_extension _ | CT_forall _ | CT_lambda _ | CT_apply _ | CT_both _
  | CT_bind _ | CT_semi _ | CT_string _ | CT_number _ | CT_braces _ ->
      invalid_notation loc
