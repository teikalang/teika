open Syntax
open Tree

type error =
  | Let_without_value
  | Let_without_body
  | Record_pattern_with_value
  | Unimplemented

exception Error of { loc : Location.t; error : error }

let raise loc error = raise (Error { loc; error })

(* expr *)
let make_expr loc desc = { le_loc = loc; le_desc = desc }
let le_var loc ~var = make_expr loc (LE_var var)
let le_number loc ~number = make_expr loc (LE_number number)
let le_arrow loc ~param ~body = make_expr loc (LE_arrow { param; body })
let le_lambda loc ~param ~body = make_expr loc (LE_lambda { param; body })
let le_apply loc ~lambda ~arg = make_expr loc (LE_apply { lambda; arg })

let le_let loc ~bound ~value ~body =
  make_expr loc (LE_let { bound; value; body })

let le_record loc ~fields = make_expr loc (LE_record fields)
let le_asterisk loc = make_expr loc LE_asterisk
let le_annot loc ~value ~type_ = make_expr loc (LE_annot { value; type_ })

(* pat *)
let make_pat loc desc = { lp_loc = loc; lp_desc = desc }
let lp_var loc ~var = make_pat loc (LP_var var)
let lp_record loc ~fields = make_pat loc (LP_record fields)
let lp_annot loc ~pat ~type_ = make_pat loc (LP_annot { pat; type_ })

let rec interpret_expr term =
  let { s_loc = loc; s_desc = term } = term in
  match term with
  | S_ident var -> le_var loc ~var
  | S_number number -> le_number loc ~number
  | S_arrow { param; body } ->
      let param = interpret_pat param in
      let body = interpret_expr body in
      le_arrow loc ~param ~body
  | S_apply { lambda; arg } ->
      let lambda = interpret_expr lambda in
      let arg = interpret_expr arg in
      le_apply loc ~lambda ~arg
  | S_lambda { param; body } ->
      let param = interpret_pat param in
      let body = interpret_expr body in
      le_lambda loc ~param ~body
  | S_bind { bound; value; body } ->
      let value =
        match value with
        | Some value -> value
        | None -> raise loc Let_without_value
      in
      let body =
        match body with Some body -> body | None -> raise loc Let_without_body
      in

      let bound = interpret_pat bound in
      let value = interpret_expr value in
      let body = interpret_expr body in
      le_let loc ~bound ~value ~body
  | S_struct content ->
      let fields =
        match content with
        | Some content -> interpret_expr_record content
        | None -> []
      in
      le_record loc ~fields
  | S_field _ -> raise loc Unimplemented
  | S_match _ -> raise loc Unimplemented
  | S_asterisk -> le_asterisk loc
  | S_annot { value; type_ } ->
      let value = interpret_expr value in
      let type_ = interpret_expr type_ in
      le_annot loc ~value ~type_

and interpret_expr_record content =
  let { s_loc = loc; s_desc = content } = content in
  match content with
  | S_bind { bound; value; body } ->
      let bound = interpret_pat bound in
      let value =
        match value with
        | Some value -> Some (interpret_expr value)
        | None -> None
      in
      let fields =
        match body with
        | Some content -> interpret_expr_record content
        | None -> []
      in

      (* TODO: locations manipulation bad *)
      let loc =
        match value with
        | Some value ->
            let loc_start = value.le_loc.loc_start in
            let loc_end = bound.lp_loc.loc_end in
            Location.{ loc_ghost = false; loc_start; loc_end }
        | None -> bound.lp_loc
      in
      LE_record_bind { loc; bound; value } :: fields
  | _ -> raise loc Unimplemented

and interpret_pat term =
  let { s_loc = loc; s_desc = term } = term in
  match term with
  | S_ident var -> lp_var loc ~var
  | S_struct content ->
      let fields =
        match content with
        | Some content -> interpret_pat_record content
        | None -> []
      in
      lp_record loc ~fields
  | S_annot { value = pat; type_ } ->
      let pat = interpret_pat pat in
      let type_ = interpret_expr type_ in
      lp_annot loc ~pat ~type_
  | _ -> raise loc Unimplemented

and interpret_pat_record content =
  let { s_loc = loc; s_desc = content } = content in
  match content with
  | S_bind { bound; value; body } ->
      let bound = interpret_pat bound in
      (match value with
      | Some _ -> raise loc Record_pattern_with_value
      | None -> ());
      let fields =
        match body with
        | Some content -> interpret_pat_record content
        | None -> []
      in

      let loc = bound.lp_loc in
      LP_record_bind { loc; bound } :: fields
  | _ -> raise loc Unimplemented
