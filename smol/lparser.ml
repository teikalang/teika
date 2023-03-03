open Stree
open Ltree

(* TODO: print loc and term *)
exception Invalid_notation of { loc : Location.t }

(* TODO: contextual location, monad would be better? *)
let rec parse_term ~loc term =
  match term with
  | ST_loc { term; loc } ->
      let term = parse_term ~loc term in
      LT_loc { term; loc }
  | ST_parens { term } -> parse_term ~loc term
  | ST_var { var } -> LT_var { var }
  | ST_forall { param; return } ->
      let param = parse_pat ~loc param in
      let return = parse_term ~loc return in
      LT_forall { param; return }
  | ST_lambda { param; return } ->
      let param = parse_pat ~loc param in
      let return = parse_term ~loc return in
      LT_lambda { param; return }
  | ST_apply { lambda; arg } ->
      let lambda = parse_term ~loc lambda in
      let arg = parse_term ~loc arg in
      LT_apply { lambda; arg }
  | ST_self _ | ST_fix _ | ST_unroll _ -> raise (Invalid_notation { loc })
  | ST_alias { bound; value; return } ->
      let bound = parse_pat ~loc bound in
      let value = parse_term ~loc value in
      let return = parse_term ~loc return in
      LT_alias { bound; value; return }
  | ST_annot { term; annot } ->
      let term = parse_term ~loc term in
      let annot = parse_term ~loc annot in
      LT_annot { term; annot }

and parse_pat ~loc term =
  match term with
  | ST_loc { term = pat; loc } ->
      let pat = parse_pat ~loc pat in
      LP_loc { pat; loc }
  | ST_parens { term = pat } -> parse_pat ~loc pat
  | ST_var { var } -> LP_var { var }
  | ST_annot { term = pat; annot } ->
      let pat = parse_pat ~loc pat in
      let annot = parse_term ~loc annot in
      LP_annot { pat; annot }
  | ST_forall _ | ST_lambda _ | ST_apply _ | ST_self _ | ST_fix _ | ST_unroll _
  | ST_alias _ ->
      raise (Invalid_notation { loc })
