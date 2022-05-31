open Syntax
open Tree

type error =
  | Let_without_value
  | Let_without_body
  | Record_without_value
  | Signature_with_value
  | Record_pattern_with_value
  | Unimplemented

exception Error of { loc : Location.t; error : error }

let raise loc error = raise (Error { loc; error })

(* expr *)
let make_expr loc desc = { le_loc = loc; le_desc = desc }
let le_var loc ~var = make_expr loc (LE_var var)
let le_number loc ~number = make_expr loc (LE_number number)

let le_arrow loc ~implicit ~param ~body =
  make_expr loc (LE_arrow { implicit; param; body })

let le_lambda loc ~implicit ~param ~body =
  make_expr loc (LE_lambda { implicit; param; body })

let le_apply loc ~lambda ~arg = make_expr loc (LE_apply { lambda; arg })
let le_let loc ~bind ~body = make_expr loc (LE_let { bind; body })
let le_record loc ~fields = make_expr loc (LE_record fields)
let le_signature loc ~fields = make_expr loc (LE_signature fields)
let le_asterisk loc = make_expr loc LE_asterisk
let le_annot loc ~value ~type_ = make_expr loc (LE_annot { value; type_ })

let le_bind ~bound ~value =
  let loc =
    let loc_start = value.le_loc.loc_start in
    let loc_end = bound.lp_loc.loc_end in
    Location.{ loc_ghost = false; loc_start; loc_end }
  in
  LE_bind { loc : Location.t; bound : pat; value : expr }

(* pat *)
let make_pat loc desc = { lp_loc = loc; lp_desc = desc }
let lp_var loc ~var = make_pat loc (LP_var var)
let lp_record loc ~fields = make_pat loc (LP_record fields)
let lp_annot loc ~pat ~type_ = make_pat loc (LP_annot { pat; type_ })

let is_implicit ~param =
  (* TODO: this about optional arguments *)
  match param.s_desc with
  | S_struct (Some { s_desc = S_bind _; s_loc = _ }) -> (false, param)
  | S_struct (Some param) -> (true, param)
  (* TODO: None being false is quite weird *)
  | S_struct None -> (false, param)
  | _ -> (false, param)

let rec interpret_expr term =
  let { s_loc = loc; s_desc = term } = term in
  match term with
  | S_ident var -> interpret_expr_var loc ~var
  | S_number number -> interpret_expr_number loc ~number
  | S_arrow { param; body } -> interpret_expr_arrow loc ~param ~body
  | S_lambda { param; body } -> interpret_expr_lambda loc ~param ~body
  | S_apply { lambda; arg } -> interpret_expr_apply loc ~lambda ~arg
  | S_bind { bound; value; body } -> interpret_expr_bind loc ~bound ~value ~body
  | S_struct content -> interpret_expr_record_ambiguous loc ~content
  | S_field _ -> raise loc Unimplemented
  | S_match _ -> raise loc Unimplemented
  | S_asterisk -> interpret_expr_asterisk loc
  | S_annot { value; type_ } -> interpret_expr_annot loc ~value ~type_

and interpret_expr_var loc ~var = le_var loc ~var
and interpret_expr_number loc ~number = le_number loc ~number

and interpret_expr_arrow loc ~param ~body =
  let implicit, param = is_implicit ~param in
  let param =
    if implicit then interpret_pat param
    else
      let { s_loc = loc; s_desc = param_desc } = param in
      (* TODO: is this lookahead worth it? *)
      match param_desc with
      | S_annot _ -> interpret_pat param
      | _ ->
          (* TODO: is generating this here bad? *)
          (* TODO: "_" should definitely go away *)
          let pat = lp_var loc ~var:"_" in
          let type_ = interpret_expr param in
          lp_annot loc ~pat ~type_
  in
  let body = interpret_expr body in
  le_arrow loc ~implicit ~param ~body

and interpret_expr_lambda loc ~param ~body =
  let implicit, param = is_implicit ~param in
  let param = interpret_pat param in
  let body = interpret_expr body in
  le_lambda loc ~implicit ~param ~body

and interpret_expr_apply loc ~lambda ~arg =
  let lambda = interpret_expr lambda in
  let arg = interpret_expr arg in
  le_apply loc ~lambda ~arg

and interpret_expr_bind loc ~bound ~value ~body =
  let value =
    match value with Some value -> value | None -> raise loc Let_without_value
  in
  let body =
    match body with Some body -> body | None -> raise loc Let_without_body
  in

  let bind =
    let bound = interpret_pat bound in
    let value = interpret_expr value in
    le_bind ~bound ~value
  in
  let body = interpret_expr body in
  le_let loc ~bind ~body

and interpret_expr_record_ambiguous loc ~content =
  match content with
  | Some content -> (
      (* TODO: weird lookahead *)
      let { s_loc = loc; s_desc } = content in
      match s_desc with
      | S_bind { bound = _; value = None; body = _ } ->
          let fields = interpret_expr_signature content in
          le_signature loc ~fields
      | _ ->
          let fields = interpret_expr_record content in
          le_record loc ~fields)
  | None -> le_signature loc ~fields:[]

and interpret_expr_record content =
  let { s_loc = loc; s_desc = content } = content in
  let bound, value, body =
    match content with
    | S_bind { bound; value; body } ->
        let value =
          match value with
          | Some value -> value
          | None -> (* TODO: better locations *) raise loc Record_without_value
        in
        (bound, value, body)
    | _ -> raise loc Unimplemented
  in

  let bind =
    let bound = interpret_pat bound in
    let value = interpret_expr value in
    le_bind ~bound ~value
  in
  let binds =
    match body with Some content -> interpret_expr_record content | None -> []
  in
  bind :: binds

and interpret_expr_signature content =
  let { s_loc = loc; s_desc = content } = content in
  let bound, body =
    match content with
    | S_bind { bound; value; body } ->
        (match value with
        | Some _ -> (* TODO: better locations *) raise loc Signature_with_value
        | None -> ());

        (bound, body)
    | _ -> raise loc Unimplemented
  in

  let bound = interpret_pat bound in
  let binds =
    match body with
    | Some content -> interpret_expr_signature content
    | None -> []
  in
  bound :: binds

and interpret_expr_asterisk loc = le_asterisk loc

and interpret_expr_annot loc ~value ~type_ =
  let value = interpret_expr value in
  let type_ = interpret_expr type_ in
  le_annot loc ~value ~type_

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
  let bound, body =
    match content with
    | S_bind { bound; value; body } ->
        (match value with
        | Some _ -> raise loc Record_pattern_with_value
        | None -> ());
        (bound, body)
    | _ -> (* TODO: better locs *) raise loc Unimplemented
  in

  let bound = interpret_pat bound in
  let binds =
    match body with Some content -> interpret_pat_record content | None -> []
  in
  bound :: binds
