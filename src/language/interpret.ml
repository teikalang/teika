open Syntax
open Tree

type error =
  | Let_without_value
  | Let_without_body
  | Record_without_value
  | Signature_with_value
  | Signature_pattern_must_be_var
  | Signature_without_annotation
  | Record_pattern_with_value
  | Forall_parameter_must_be_var
  | Unimplemented

exception Error of { loc : Location.t; error : error }

let raise loc error = raise (Error { loc; error })

(* expr *)
let make_expr loc desc = LE { loc; desc }
let le_var loc ~var = make_expr loc (LE_var var)
let le_number loc ~number = make_expr loc (LE_number number)
let le_lambda loc ~param ~body = make_expr loc (LE_lambda { param; body })
let le_apply loc ~lambda ~arg = make_expr loc (LE_apply { lambda; arg })
let le_let loc ~bind ~body = make_expr loc (LE_let { bind; body })
let le_record loc ~fields = make_expr loc (LE_record fields)
let le_annot loc ~value ~annot = make_expr loc (LE_annot { value; annot })
let le_type loc ~type_ = make_expr loc (LE_type type_)

let le_bind ~bound ~value =
  (* TODO: loc arithmetic *)
  let loc =
    let (LP { loc = bound_loc; desc = _ }) = bound in
    let (LE { loc = value_loc; desc = _ }) = value in
    let loc_start = bound_loc.loc_start in
    let loc_end = value_loc.loc_end in
    Location.{ loc_ghost = false; loc_start; loc_end }
  in
  LE_bind { loc : Location.t; bound : pat; value : expr }

(* typ *)
let make_typ loc desc = LT { loc; desc }
let lt_var loc ~var = make_typ loc (LT_var var)

let lt_forall loc ~var ~kind ~body =
  make_typ loc (LT_forall { var; kind; body })

let lt_arrow loc ~param ~body = make_typ loc (LT_arrow { param; body })
let lt_record loc ~fields = make_typ loc (LT_record fields)
let lt_bind ~bound_loc ~var ~type_ = LT_bind { loc = bound_loc; var; type_ }

(* pat *)
let make_pat loc desc = LP { loc; desc }
let lp_var loc ~var = make_pat loc (LP_var var)
let lp_record loc ~fields = make_pat loc (LP_record fields)
let lp_annot loc ~pat ~annot = make_pat loc (LP_annot { pat; annot })

(* annot *)
let la_type expr = LA_type expr
let la_kind kind = LA_kind kind

(* kind *)
let make_kind loc desc = LK { loc; desc }
let lk_type loc = make_kind loc LK_type
(* let lk_arrow loc ~param ~body = make_kind loc (LK_arrow { param; body }) *)

(* ambiguities *)
let _is_implicit ~param =
  (* TODO: this about optional arguments *)
  match param.s_desc with
  | S_struct (Some { s_desc = S_bind _; s_loc = _ }) -> (false, param)
  | S_struct (Some param) -> (true, param)
  (* TODO: None being false is quite weird *)
  | S_struct None -> (false, param)
  | _ -> (false, param)

let rec is_kind term =
  (* TODO: this is clearly not ideal *)
  match term.s_desc with
  | S_asterisk -> true
  | S_arrow { param; body } -> is_kind param && is_kind body
  | _ -> false

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
  | S_asterisk -> raise loc Unimplemented
  | S_annot { value; type_ } -> interpret_expr_annot loc ~value ~annot:type_

and interpret_expr_var loc ~var = le_var loc ~var
and interpret_expr_number loc ~number = le_number loc ~number

and interpret_expr_arrow loc ~param ~body =
  let type_ = interpret_type_arrow loc ~param ~body in
  le_type loc ~type_

and interpret_expr_lambda loc ~param ~body =
  (* let implicit, param = is_implicit ~param in *)
  let param = interpret_pat param in
  let body = interpret_expr body in
  le_lambda loc ~param ~body

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
      let { s_loc = _; s_desc = content_desc } = content in
      match content_desc with
      | S_bind { bound = _; value = None; body = _ } ->
          let type_ = interpret_type_record loc ~content:(Some content) in
          le_type loc ~type_
      | _ ->
          let fields = interpret_expr_record ~content in
          le_record loc ~fields)
  | None -> le_record loc ~fields:[]

and interpret_expr_record ~content =
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
    match body with
    | Some content -> interpret_expr_record ~content
    | None -> []
  in
  bind :: binds

and interpret_expr_annot loc ~value ~annot =
  let value = interpret_expr value in
  let annot = interpret_annot annot in
  le_annot loc ~value ~annot

and interpret_type term =
  let { s_loc = loc; s_desc = term } = term in
  match term with
  | S_ident var -> interpret_type_var loc ~var
  | S_number _ -> raise loc Unimplemented
  | S_arrow { param; body } -> interpret_type_arrow_ambiguous loc ~param ~body
  | S_lambda _ -> raise loc Unimplemented
  | S_apply _ -> raise loc Unimplemented
  | S_bind _ -> raise loc Unimplemented
  | S_struct content -> interpret_type_record loc ~content
  | S_field _ -> raise loc Unimplemented
  | S_match _ -> raise loc Unimplemented
  | S_asterisk -> raise loc Unimplemented
  | S_annot _ -> raise loc Unimplemented

and interpret_type_var loc ~var = lt_var loc ~var

and interpret_type_arrow_ambiguous loc ~param ~body =
  (* TODO:
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
             let annot = interpret_annot param in
             lp_annot loc ~pat ~annot
     in
  *)
  let { s_loc = _; s_desc = param_desc } = param in
  (* TODO: is this lookahead worth it? *)
  match param_desc with
  | S_annot { value = bound; type_ = kind } ->
      interpret_type_forall loc ~bound ~kind ~body
  | _ -> interpret_type_arrow loc ~param ~body

and interpret_type_forall loc ~bound ~kind ~body =
  let var =
    let { s_loc = loc; s_desc = bound } = bound in
    match bound with
    | S_ident var -> var
    | _ -> raise loc Forall_parameter_must_be_var
  in
  let kind = interpret_kind kind in
  let body = interpret_type body in
  lt_forall loc ~var ~kind ~body

and interpret_type_arrow loc ~param ~body =
  let param = interpret_type param in
  let body = interpret_type body in
  lt_arrow loc ~param ~body

and interpret_type_record loc ~content =
  match content with
  | Some content ->
      let fields = interpret_type_record_fields ~content in
      lt_record loc ~fields
  | None -> lt_record loc ~fields:[]

and interpret_type_record_fields ~content =
  let bound, body =
    let { s_loc = loc; s_desc = content } = content in
    match content with
    | S_bind { bound; value; body } ->
        (match value with
        | Some { s_loc = loc; s_desc = _ } ->
            (* TODO: Should loc include the = sign? *)
            raise loc Signature_with_value
        | None -> ());

        (bound, body)
    | _ -> raise loc Unimplemented
  in

  (*| Signature_pattern_must_be_var
    | Signature_without_annotation *)
  let bound_loc, pattern, type_ =
    let { s_loc = loc; s_desc = bound } = bound in
    match bound with
    | S_annot { value = pattern; type_ } -> (loc, pattern, type_)
    | _ -> raise loc Signature_without_annotation
  in
  let var =
    let { s_loc = loc; s_desc = pattern } = pattern in
    match pattern with
    | S_ident var -> var
    | _ -> raise loc Signature_pattern_must_be_var
  in
  let type_ = interpret_type type_ in
  let binds =
    match body with
    | Some content -> interpret_type_record_fields ~content
    | None -> []
  in
  lt_bind ~bound_loc ~var ~type_ :: binds

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
      let annot = interpret_annot type_ in
      lp_annot loc ~pat ~annot
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

and interpret_annot term =
  if is_kind term then
    let kind = interpret_kind term in
    la_kind kind
  else
    let type_ = interpret_expr term in
    la_type type_

and interpret_kind term =
  let { s_loc = loc; s_desc = term } = term in
  match term with
  | S_asterisk -> interpret_kind_asterisk loc
  | S_arrow _ -> raise loc Unimplemented
  | _ -> raise loc Unimplemented

and interpret_kind_asterisk loc = lk_type loc

(* and interpret_kind_arrow loc ~param ~body =
   let param = interpret_kind param in
   let body = interpret_kind body in
   lk_arrow loc ~param ~body *)
