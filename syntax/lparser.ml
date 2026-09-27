open Ctree

module Syntax_tree = struct
  type var = Name.t

  type term = Term of { head : term_head; loc : Location.t }

  and term_head =
    | T_var of { var : var }
    | T_block of { block : block; return : term }
    | T_apply of { funct : term; arg : term }
    | T_self of { bound : pat; body : term }
    | T_fix of { bound : pat; body : term }
    | T_unit
    | T_sigma of { fst : pat; snd : term }
    | T_pair of { fst : term; snd : term }
    | T_box of { content : term }

  and pat = Pat of { head : pat_head; loc : Location.t }

  and pat_head =
    | P_annot of { pat : pat; annot : term }
    | P_var of { var : var }
    | P_apply of { funct : pat; arg : pat }
    | P_unit
    | P_pair of { fst : pat; snd : pat }
    | P_box of { content : pat }

  and block = Block of { head : block_head; loc : Location.t }

  and block_head =
    | B_hole
    | B_def of { bound : pat; body : block }
    | B_fix of { bound : pat; arg : term; body : block }

  let t_wrap loc head = Term { head; loc }
  let p_wrap loc head = Pat { head; loc }
  let b_wrap loc head = Block { head; loc }
end

open Syntax_tree

let rec parse_term term =
  let (CTerm { term = head; loc }) = term in
  match head with
  | CT_hole -> failwith "parse_term: hole not supported"
  | CT_var { var } -> t_wrap loc (T_var { var })
  | CT_apply { funct; arg } ->
      let funct = parse_term funct in
      let arg = parse_term arg in
      t_wrap loc (T_apply { funct; arg })
  | CT_pair { left; right } -> (
      let (CTerm { term = left_head; loc = left_loc }) = left in
      match left_head with
      | CT_annot { value; annot } ->
          let fst = parse_pat left in
          let snd = parse_term right in
          t_wrap loc (T_sigma { fst; snd })
      | CT_hole | CT_var _ | CT_apply _ | CT_pair _ | CT_inter _ | CT_union _
      | CT_bind _ | CT_semi _ | CT_string _ | CT_number _ | CT_parens _
      | CT_brackets _ | CT_braces _ ->
          let fst = parse_term left in
          let snd = parse_term right in
          t_wrap loc (T_pair { fst; snd }))
  | CT_inter _ -> failwith "parse_term: intersection not supported"
  | CT_union _ -> failwith "parse_term: union not supported"
  | CT_bind { bound; value } ->
      let bound = parse_pat bound in
      let body = parse_term value in
      t_wrap loc (T_fix { bound; body })
  | CT_semi _ ->
      let block, return = parse_block term in
      t_wrap loc (T_block { block; return })
  | CT_annot { value; annot } ->
      let bound = parse_pat value in
      let body = parse_term annot in
      t_wrap loc (T_self { bound; body })
  | CT_string _ -> failwith "parse_term: string literal not supported"
  | CT_number _ -> failwith "parse_term: number literal not supported"
  | CT_parens { content } -> (
      match content with
      | None -> t_wrap loc T_unit
      | Some content -> parse_term content)
  | CT_brackets { content } ->
      let content =
        match content with
        | None -> t_wrap loc T_unit
        | Some content -> parse_term content
      in
      t_wrap loc (T_box { content })
  | CT_braces _ -> failwith "parse_term: braces not supported"

and parse_block term =
  let (CTerm { term = head; loc }) = term in
  match head with
  | CT_semi { left; right } -> (
      let (CTerm { term = left_head; loc = left_loc }) = left in
      match left_head with
      | CT_annot { value } ->
          let bound = parse_pat value in
          let body, return = parse_block right in
          (b_wrap loc (B_def { bound; body }), return)
      | CT_bind { bound; value } ->
          let bound = parse_pat bound in
          let arg = parse_term value in
          let body, return = parse_block right in
          (b_wrap loc (B_fix { bound; arg; body }), return)
      | CT_hole | CT_var _ | CT_apply _ | CT_pair _ | CT_inter _ | CT_union _
      | CT_semi _ | CT_string _ | CT_number _ | CT_parens _ | CT_brackets _
      | CT_braces _ ->
          failwith "parse_block: not bind or annot")
  | CT_hole | CT_var _ | CT_apply _ | CT_pair _ | CT_inter _ | CT_union _
  | CT_bind _ | CT_annot _ | CT_string _ | CT_number _ | CT_parens _
  | CT_brackets _ | CT_braces _ ->
      let return = parse_term term in
      (b_wrap loc B_hole, return)

and parse_pat pat =
  let (CTerm { term = head; loc }) = pat in
  match head with
  | CT_hole -> failwith "parse_pat: hole not supported"
  | CT_var { var } -> p_wrap loc (P_var { var })
  | CT_apply { funct; arg } ->
      let funct = parse_pat funct in
      let arg = parse_pat arg in
      p_wrap loc (P_apply { funct; arg })
  | CT_pair { left; right } ->
      let fst = parse_pat left in
      let snd = parse_pat right in
      p_wrap loc (P_pair { fst; snd })
  | CT_inter _ -> failwith "parse_pat: intersection not supported"
  | CT_union _ -> failwith "parse_pat: union not supported"
  | CT_bind _ -> failwith "parse_pat: bind not supported"
  | CT_semi _ -> failwith "parse_pat: semi not supported"
  | CT_annot { value; annot } ->
      let pat = parse_pat value in
      let annot = parse_term annot in
      p_wrap loc (P_annot { pat; annot })
  | CT_string _ -> failwith "parse_pat: string literal not supported"
  | CT_number _ -> failwith "parse_pat: number literal not supported"
  | CT_parens { content } -> (
      match content with
      | None -> p_wrap loc P_unit
      | Some content -> parse_pat content)
  | CT_brackets { content } ->
      let content =
        match content with
        | None -> p_wrap loc P_unit
        | Some content -> parse_pat content
      in
      p_wrap loc (P_box { content })
  | CT_braces _ -> failwith "parse_pat: braces not supported"
