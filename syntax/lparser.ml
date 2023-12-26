open Ctree
open Ltree

exception Invalid_notation of { loc : Location.t }

let invalid_notation loc = raise (Invalid_notation { loc })

let rec parse_term ~loc term =
  match term with
  | CT_loc { term; loc } ->
      let term = parse_term ~loc term in
      LT_loc { term; loc }
  | CT_parens { content } -> parse_term ~loc content
  | CT_var { var } -> LT_var { var }
  | CT_extension _ -> invalid_notation loc
  | CT_grade _ -> invalid_notation loc
  | CT_forall { param; return } ->
      let param = parse_pat ~loc param in
      let return = parse_term ~loc return in
      LT_forall { param; return }
  | CT_lambda { param; return } ->
      let param = parse_pat ~loc param in
      let return = parse_term ~loc return in
      LT_lambda { param; return }
  | CT_apply { lambda; arg } -> parse_apply ~loc ~lambda ~arg
  | CT_pair { left = _; right = _ } ->
      (* TODO: better error *)
      invalid_notation loc
  | CT_both _ -> invalid_notation loc
  | CT_bind _ -> invalid_notation loc
  | CT_semi { left; right } ->
      let bound = parse_bind ~loc left in
      let return = parse_term ~loc right in
      LT_let { bound; return }
  | CT_annot { value; annot } ->
      let term = parse_term ~loc value in
      let annot = parse_term ~loc annot in
      LT_annot { term; annot }
  | CT_string { literal } -> LT_string { literal }
  | CT_number _ -> invalid_notation loc
  | CT_braces _ -> invalid_notation loc

and parse_bind ~loc term =
  match term with
  | CT_loc { term; loc } -> parse_bind ~loc term
  | CT_bind { bound = pat; value } ->
      let pat = parse_pat ~loc pat in
      let value = parse_term ~loc value in
      LBind { loc; pat; value }
  | CT_var _ | CT_extension _ | CT_grade _ | CT_forall _ | CT_lambda _
  | CT_apply _ | CT_pair _ | CT_both _ | CT_semi _ | CT_annot _ | CT_string _
  | CT_number _ | CT_parens _ | CT_braces _ ->
      invalid_notation loc

and parse_apply ~loc ~lambda ~arg =
  (* TODO: is this fallback of locations okay? *)
  match lambda with
  | CT_loc { term = lambda; loc } -> parse_apply ~loc ~lambda ~arg
  (* TODO: CT_parens? *)
  | CT_extension { extension } ->
      let payload = parse_term ~loc arg in
      let term = LT_extension { extension; payload } in
      (* TODO: this is not ideal *)
      LT_loc { term; loc }
  (* TODO: remove catch all *)
  | _ ->
      let lambda = parse_term ~loc lambda in
      let arg = parse_term ~loc arg in
      LT_apply { lambda; arg }

and parse_pat ~loc pat =
  (* TODO: Stack of locs? *)
  match pat with
  | CT_loc { term = pat; loc } ->
      let pat = parse_pat ~loc pat in
      LP_loc { pat; loc }
  | CT_parens { content } -> parse_pat ~loc content
  | CT_var { var } -> LP_var { var }
  | CT_grade { term = pat; grade } ->
      let pat = parse_pat ~loc pat in
      let () = parse_grade ~loc grade in
      LP_erasable { pat }
  | CT_pair { left = _; right = _ } -> invalid_notation loc
  | CT_annot { value = pat; annot } ->
      let pat = parse_pat ~loc pat in
      let annot = parse_term ~loc annot in
      LP_annot { pat; annot }
  | CT_extension _ | CT_forall _ | CT_lambda _ | CT_apply _ | CT_both _
  | CT_bind _ | CT_semi _ | CT_string _ | CT_number _ | CT_braces _ ->
      invalid_notation loc

and parse_grade ~loc grade =
  match grade with
  | CT_loc { term = grade; loc } ->
      (* TODO: store grade loc *)
      parse_grade ~loc grade
  | CT_parens { content } -> parse_grade ~loc content
  | CT_number { literal } -> (
      match literal with 0 -> () | _ -> invalid_notation loc (* TODO: annot *))
  | CT_var _ | CT_grade _ | CT_extension _ | CT_forall _ | CT_lambda _
  | CT_apply _ | CT_pair _ | CT_both _ | CT_bind _ | CT_semi _ | CT_string _
  | CT_annot _ | CT_braces _ ->
      invalid_notation loc
