open Stree
open Ltree

exception Invalid_notation of { loc : Location.t }

let invalid_notation loc = raise (Invalid_notation { loc })

let rec parse_term ~loc term =
  match term with
  | ST_loc { term; loc } ->
      let term = parse_term ~loc term in
      LT_loc { term; loc }
  | ST_parens { content } -> parse_term ~loc content
  | ST_var { var } -> LT_var { var }
  | ST_extension _ -> invalid_notation loc
  | ST_forall { param; return } ->
      let param = parse_pat ~loc param in
      let return = parse_term ~loc return in
      LT_forall { param; return }
  | ST_lambda { param; return } ->
      let param = parse_pat ~loc param in
      let return = parse_term ~loc return in
      LT_lambda { param; return }
  | ST_apply { lambda; arg } -> parse_apply ~loc ~lambda ~arg
  | ST_self { self; body } ->
      let self = parse_pat ~loc self in
      let body = parse_term ~loc body in
      LT_self { self; body }
  | ST_fix { self; body } ->
      let self = parse_pat ~loc self in
      let body = parse_term ~loc body in
      LT_fix { self; body }
  | ST_unroll { term } ->
      let term = parse_term ~loc term in
      LT_unroll { term }
  | ST_pair { left = _; right = _ } ->
      (* TODO: better error *)
      invalid_notation loc
  | ST_both _ -> invalid_notation loc
  | ST_bind _ -> invalid_notation loc
  | ST_semi { left; right } ->
      let bound = parse_bind ~loc left in
      let return = parse_term ~loc right in
      LT_let { bound; return }
  | ST_annot { value; annot } ->
      let term = parse_term ~loc value in
      let annot = parse_term ~loc annot in
      LT_annot { term; annot }
  | ST_string { literal } -> LT_string { literal }
  | ST_braces _ -> invalid_notation loc

and parse_bind ~loc term =
  match term with
  | ST_loc { term; loc } -> parse_bind ~loc term
  | ST_bind { bound = pat; value } ->
      let pat = parse_pat ~loc pat in
      let value = parse_term ~loc value in
      LBind { loc; pat; value }
  | ST_var _ | ST_extension _ | ST_forall _ | ST_lambda _ | ST_apply _
  | ST_self _ | ST_fix _ | ST_unroll _ | ST_pair _ | ST_both _ | ST_semi _
  | ST_annot _ | ST_string _ | ST_parens _ | ST_braces _ ->
      invalid_notation loc

and parse_apply ~loc ~lambda ~arg =
  (* TODO: is this fallback of locations okay? *)
  match lambda with
  | ST_loc { term = lambda; loc } -> parse_apply ~loc ~lambda ~arg
  (* TODO: ST_parens? *)
  | ST_extension { extension } ->
      let payload = parse_term ~loc arg in
      let term = LT_extension { extension; payload } in
      (* TODO: this is not ideal *)
      LT_loc { term; loc }
  | _ ->
      let lambda = parse_term ~loc lambda in
      let arg = parse_term ~loc arg in
      LT_apply { lambda; arg }

and parse_pat ~loc pat =
  (* TODO: Stack of locs? *)
  match pat with
  | ST_loc { term = pat; loc } ->
      let pat = parse_pat ~loc pat in
      LP_loc { pat; loc }
  | ST_parens { content } -> parse_pat ~loc content
  | ST_var { var } -> LP_var { var }
  | ST_unroll { term = pat } ->
      let pat = parse_pat ~loc pat in
      LP_unroll { pat }
  | ST_pair { left = _; right = _ } -> invalid_notation loc
  | ST_annot { value = pat; annot } ->
      let pat = parse_pat ~loc pat in
      let annot = parse_term ~loc annot in
      LP_annot { pat; annot }
  | ST_extension _ | ST_forall _ | ST_lambda _ | ST_self _ | ST_fix _
  | ST_apply _ | ST_both _ | ST_bind _ | ST_semi _ | ST_string _ | ST_braces _
    ->
      invalid_notation loc
