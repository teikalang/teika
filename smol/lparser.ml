open Stree
open Ltree

exception Invalid_notation of { loc : Location.t }

let () = Printexc.record_backtrace true
let invalid_notation loc = raise (Invalid_notation { loc })

let extract_var term =
  let (ST { loc; desc }) = term in
  match desc with
  | ST_var { var } -> var
  | ST_literal _ | ST_arrow _ | ST_lambda _ | ST_apply _ | ST_pair _ | ST_bind _
  | ST_semi _ | ST_annot _ ->
      invalid_notation loc

let extract_annot param =
  let (ST { loc; desc }) = param in
  match desc with
  | ST_annot { value; type_ = param } -> (value, param)
  | ST_var _ | ST_literal _ | ST_arrow _ | ST_lambda _ | ST_apply _ | ST_pair _
  | ST_bind _ | ST_semi _ ->
      invalid_notation loc

let rec from_stree term =
  let (ST { loc; desc }) = term in
  match desc with
  | ST_var { var } -> lt_var loc ~var
  | ST_literal { literal } -> lt_literal loc ~literal
  | ST_arrow { param; return } ->
      let var, param = extract_annot param in
      let var = extract_var var in
      let param = from_stree param in
      let return = from_stree return in
      lt_arrow loc ~var ~param ~return
  | ST_lambda { param; return } ->
      let var, param = extract_annot param in
      let var = extract_var var in
      let param = from_stree param in
      let return = from_stree return in
      lt_lambda loc ~var ~param ~return
  | ST_apply { lambda; arg } ->
      let lambda = from_stree lambda in
      let arg = from_stree arg in
      lt_apply loc ~lambda ~arg
  | ST_pair { left; right } -> (
      let (ST { loc; desc }) = left in
      match desc with
      | ST_bind { bound; value = left } ->
          let var = extract_var bound in
          let right, annot = extract_annot right in
          let left = from_stree left in
          let right = from_stree right in
          let annot = from_stree annot in
          lt_pair loc ~var ~left ~right ~annot
      | ST_annot { value; type_ = left } ->
          let var = extract_var value in
          let left = from_stree left in
          let right = from_stree right in
          lt_sigma loc ~var ~left ~right
      | ST_var _ | ST_literal _ | ST_arrow _ | ST_lambda _ | ST_apply _
      | ST_pair _ | ST_semi _ ->
          invalid_notation loc)
  | ST_bind _ ->
      Format.eprintf "%a" Stree.pp_term term;
      invalid_notation loc
  | ST_semi { left; right } -> (
      let bound, value =
        let (ST { loc; desc }) = left in
        match desc with
        | ST_bind { bound; value } -> (bound, value)
        | ST_var _ | ST_literal _ | ST_arrow _ | ST_lambda _ | ST_apply _
        | ST_pair _ | ST_semi _ | ST_annot _ ->
            invalid_notation loc
      in
      let return = from_stree right in
      let (ST { loc; desc }) = bound in
      match desc with
      | ST_var { var } ->
          let value = from_stree value in
          lt_let loc ~var ~value ~return
      | ST_pair { left; right } ->
          let left = extract_var left in
          let right = extract_var right in
          let pair = from_stree value in
          lt_unpair loc ~left ~right ~pair ~return
      | ST_literal _ | ST_arrow _ | ST_lambda _ | ST_apply _ | ST_semi _
      | ST_bind _ | ST_annot _ ->
          invalid_notation loc)
  | ST_annot { value; type_ } ->
      let value = from_stree value in
      let type_ = from_stree type_ in
      lt_annot loc ~value ~type_
