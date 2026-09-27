open Ctree

module Syntax_tree = struct
  type var = Name.t

  type term = Term of { head : term_head; loc : Location.t }

  and term_head =
    | T_hole
    | T_type
    | T_var of { var : var }
    | T_def of { bound : pat; body : term }
    | T_let of { bound : pat; arg : term; body : term }
    | TF_pi of { bound : pat; body : term }
    | TI_lambda of { bound : pat; body : term }
    | TE_apply of { funct : term; arg : term }
    | TE_annot of { value : term; annot : term }
    | TF_self of { bound : pat; body : term }
    | TI_fix of { bound : pat; body : term }
    | TE_unfold of { fix : term }
    | TF_unit
    | TI_unit
    | TE_drop of { arg : term; body : term }
    | TF_sigma of { fst : pat; snd : term }
    | TI_pair of { fst : term; snd : term }
    | TE_fst of { pair : term }
    | TE_snd of { pair : term }

  and pat = Pat of { head : pat_head; loc : Location.t }

  and pat_head =
    | P_annot of { pat : pat; annot : term }
    | P_var of { var : var }
    | P_wildcard
    | P_unit
    | P_pair of { fst : pat; snd : pat }
    | P_apply of { funct : pat; arg : pat }

  let t_wrap loc head = Term { head; loc }
  let p_wrap loc head = Pat { head; loc }
end

open Syntax_tree

let concrete_location (CTerm { loc; _ }) = loc

let rec flatten_pair (CTerm { term; _ } as concrete) =
  match term with
  | CT_pair { left; right } -> left :: flatten_pair right
  | CT_hole | CT_var _ | CT_unit _ | CT_apply _ | CT_pi _ | CT_lambda _
  | CT_self _ | CT_fix _ | CT_unfold _ | CT_fst _ | CT_snd _ | CT_inter _
  | CT_union _ | CT_bind _ | CT_semi _ | CT_annot _ | CT_string _ | CT_number _
  | CT_parens _ | CT_brackets _ | CT_braces _ ->
      [ concrete ]

let inherit_trailing_annotation binders =
  let rec annotate inherited result = function
    | [] -> result
    | (CTerm { term = CT_annot { annot; _ }; _ } as binder) :: rest ->
        annotate (Some annot) (binder :: result) rest
    | (CTerm { term = CT_var _ | CT_hole; loc } as binder) :: rest ->
        let binder =
          match inherited with
          | None -> binder
          | Some annot -> ct_annot loc ~value:binder ~annot
        in
        annotate inherited (binder :: result) rest
    | binder :: rest -> annotate None (binder :: result) rest
  in
  annotate None [] (List.rev binders)

let binder_terms (CTerm { term; _ } as bound) =
  match term with
  | CT_parens { content } -> flatten_pair content |> inherit_trailing_annotation
  | CT_hole | CT_var _ | CT_unit _ | CT_apply _ | CT_pi _ | CT_lambda _
  | CT_self _ | CT_fix _ | CT_unfold _ | CT_fst _ | CT_snd _ | CT_pair _
  | CT_inter _ | CT_union _ | CT_bind _ | CT_semi _ | CT_annot _ | CT_string _
  | CT_number _ | CT_brackets _ | CT_braces _ ->
      [ bound ]

let rec parse_term (CTerm { term; loc }) =
  match term with
  | CT_hole -> t_wrap loc T_hole
  | CT_var { var } when Name.equal var (Name.make "Type") -> t_wrap loc T_type
  | CT_var { var } -> t_wrap loc (T_var { var })
  | CT_unit { delimiter = Parens } -> t_wrap loc TI_unit
  | CT_unit { delimiter = Brackets } -> t_wrap loc TF_unit
  | CT_apply { funct; arg } -> parse_apply loc funct arg
  | CT_pi { bound; body } -> parse_pi loc bound body
  | CT_lambda { bound; body } -> parse_lambda loc bound body
  | CT_self { bound; body } ->
      let bound = parse_single_binder "self" bound in
      let body = parse_term body in
      t_wrap loc (TF_self { bound; body })
  | CT_fix { bound; body } ->
      let bound = parse_single_binder "fix" bound in
      let body = parse_term body in
      t_wrap loc (TI_fix { bound; body })
  | CT_unfold { fix } ->
      let fix = parse_term fix in
      t_wrap loc (TE_unfold { fix })
  | CT_fst { pair } ->
      let pair = parse_term pair in
      t_wrap loc (TE_fst { pair })
  | CT_snd { pair } ->
      let pair = parse_term pair in
      t_wrap loc (TE_snd { pair })
  | CT_pair { left; right } -> parse_pair loc left right
  | CT_inter _ -> failwith "parse_term: intersection is not in the typed core"
  | CT_union _ -> failwith "parse_term: union is not in the typed core"
  | CT_bind _ ->
      failwith "parse_term: a definition must be followed by ';' and a body"
  | CT_semi { left; right } -> parse_semi loc left right
  | CT_annot { value; annot } ->
      let value = parse_term value and annot = parse_term annot in
      t_wrap loc (TE_annot { value; annot })
  | CT_string _ ->
      failwith "parse_term: string literals are not in the typed core"
  | CT_number _ ->
      failwith "parse_term: number literals are not in the typed core"
  | CT_parens { content } -> parse_term content
  | CT_brackets { content } -> parse_bracket_content loc content
  | CT_braces _ ->
      failwith "parse_term: implicit arguments are not in the typed core"

and parse_apply loc funct arg =
  let funct = parse_term funct in
  let arguments =
    match arg with
    | CTerm { term = CT_parens { content }; _ } -> flatten_pair content
    | CTerm _ -> [ arg ]
  in
  List.fold_left
    (fun funct arg ->
      let arg = parse_term arg in
      t_wrap loc (TE_apply { funct; arg }))
    funct arguments

and parse_pi loc bound body =
  let body = parse_term body in
  match bound with
  | CTerm { term = CT_parens _; _ } ->
      List.fold_right
        (fun bound body ->
          let bound = parse_pat bound in
          t_wrap loc (TF_pi { bound; body }))
        (binder_terms bound) body
  | CTerm _ ->
      let annot = parse_term bound in
      let wildcard = p_wrap (concrete_location bound) P_wildcard in
      let bound =
        p_wrap (concrete_location bound) (P_annot { pat = wildcard; annot })
      in
      t_wrap loc (TF_pi { bound; body })

and parse_lambda loc bound body =
  let body = parse_term body in
  List.fold_right
    (fun bound body ->
      let bound = parse_pat bound in
      t_wrap loc (TI_lambda { bound; body }))
    (binder_terms bound) body

and parse_single_binder construct bound =
  match binder_terms bound with
  | [ bound ] -> parse_pat bound
  | _ -> failwith ("parse_term: " ^ construct ^ " binds exactly one pattern")

and parse_pair loc left right =
  let fst = parse_term left and snd = parse_term right in
  t_wrap loc (TI_pair { fst; snd })

and parse_bracket_content loc content =
  let (CTerm { term; _ }) = content in
  match term with
  | CT_pair { left; right } -> (
      match left with
      | CTerm { term = CT_annot _; _ } ->
          let fst = parse_pat left in
          let snd = parse_sigma_tail right in
          t_wrap loc (TF_sigma { fst; snd })
      | CTerm _ -> parse_pair loc left right)
  | CT_hole | CT_var _ | CT_unit _ | CT_apply _ | CT_pi _ | CT_lambda _
  | CT_self _ | CT_fix _ | CT_unfold _ | CT_fst _ | CT_snd _ | CT_inter _
  | CT_union _ | CT_bind _ | CT_semi _ | CT_annot _ | CT_string _ | CT_number _
  | CT_parens _ | CT_brackets _ | CT_braces _ ->
      parse_term content

and parse_sigma_tail (CTerm { term; loc } as content) =
  match term with
  | CT_pair { left = CTerm { term = CT_annot _; _ } as left; right } ->
      let fst = parse_pat left in
      let snd = parse_sigma_tail right in
      t_wrap loc (TF_sigma { fst; snd })
  | CT_annot { annot; _ } -> parse_term annot
  | CT_hole | CT_var _ | CT_unit _ | CT_apply _ | CT_pi _ | CT_lambda _
  | CT_self _ | CT_fix _ | CT_unfold _ | CT_fst _ | CT_snd _ | CT_pair _
  | CT_inter _ | CT_union _ | CT_bind _ | CT_semi _ | CT_string _ | CT_number _
  | CT_parens _ | CT_brackets _ | CT_braces _ ->
      parse_term content

and parse_semi loc left right =
  let (CTerm { term = left_head; _ }) = left in
  match left_head with
  | CT_bind
      {
        bound = CTerm { term = CT_unit { delimiter = Parens }; _ };
        value = arg;
      } ->
      let arg = parse_term arg in
      let body = parse_term right in
      t_wrap loc (TE_drop { arg; body })
  | CT_bind { bound; value = arg } ->
      let bound = parse_pat bound in
      let arg = parse_term arg in
      let body = parse_term right in
      t_wrap loc (T_let { bound; arg; body })
  | CT_annot _ ->
      let bound = parse_pat left in
      let body = parse_term right in
      t_wrap loc (T_def { bound; body })
  | CT_hole | CT_var _ | CT_unit _ | CT_apply _ | CT_pi _ | CT_lambda _
  | CT_self _ | CT_fix _ | CT_unfold _ | CT_fst _ | CT_snd _ | CT_pair _
  | CT_inter _ | CT_union _ | CT_semi _ | CT_string _ | CT_number _
  | CT_parens _ | CT_brackets _ | CT_braces _ ->
      failwith "parse_term: left side of ';' is not a declaration or definition"

and parse_pat (CTerm { term = head; loc }) =
  match head with
  | CT_hole -> p_wrap loc P_wildcard
  | CT_var { var } -> p_wrap loc (P_var { var })
  | CT_unit _ -> p_wrap loc P_unit
  | CT_apply { funct; arg } -> parse_pat_apply loc funct arg
  | CT_pair { left; right } ->
      let fst = parse_pat left and snd = parse_pat right in
      p_wrap loc (P_pair { fst; snd })
  | CT_annot { value; annot } ->
      let pat = parse_pat value and annot = parse_term annot in
      p_wrap loc (P_annot { pat; annot })
  | CT_parens { content } -> parse_pat content
  | CT_brackets { content } -> parse_pat content
  | CT_pi _ | CT_lambda _ | CT_self _ | CT_fix _ | CT_unfold _ | CT_fst _
  | CT_snd _ | CT_inter _ | CT_union _ | CT_bind _ | CT_semi _ | CT_string _
  | CT_number _ | CT_braces _ ->
      failwith "parse_pat: unsupported pattern syntax"

and parse_pat_apply loc funct arg =
  let funct = parse_pat funct in
  let arguments =
    match arg with
    | CTerm { term = CT_parens { content }; _ } ->
        flatten_pair content |> inherit_trailing_annotation
    | CTerm _ -> [ arg ]
  in
  List.fold_left
    (fun funct arg ->
      let arg = parse_pat arg in
      p_wrap loc (P_apply { funct; arg }))
    funct arguments
