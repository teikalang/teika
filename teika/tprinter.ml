[@@@ocaml.warning "-unused-constructor"]

module Ptree = struct
  open Format
  open Utils

  type term =
    (* TODO: use PT_meta for level, subst and shift *)
    | PT_meta of { term : term }
    | PT_annot of { term : term; annot : term }
    | PT_var of { var : Name.t }
    | PT_free_var of { var : Level.t }
    | PT_bound_var of { var : Index.t }
    | PT_hoist of { bound : term; body : term }
    | PT_let of { bound : term; arg : term; body : term }
    | PT_apply of { funct : term; arg : term }
    | PT_lambda of { param : term; body : term }
    | PT_forall of { param : term; body : term }
    | PT_inter of { left : term; right : term }
    | PT_string of { literal : string }

  type term_prec = T_wrapped | T_let | T_funct | T_apply | T_atom

  let pp_term_syntax ~pp_wrapped ~pp_let ~pp_funct ~pp_apply ~pp_atom fmt term =
    match term with
    | PT_meta { term } -> fprintf fmt "#%a" pp_atom term
    | PT_annot { term; annot } ->
        fprintf fmt "%a : %a" pp_funct term pp_wrapped annot
    | PT_var { var } -> fprintf fmt "%s" (Name.repr var)
    | PT_free_var { var } -> fprintf fmt "\\+%a" Level.pp var
    | PT_bound_var { var } -> fprintf fmt "\\-%a" Index.pp var
    | PT_hoist { bound; body } ->
        (* TODO: is pp_wrapped correct here? *)
        fprintf fmt "%a; %a" pp_wrapped bound pp_let body
    | PT_let { bound; arg; body } ->
        fprintf fmt "%a = %a; %a" pp_atom bound pp_funct arg pp_let body
    | PT_lambda { param; body } ->
        fprintf fmt "%a => %a" pp_atom param pp_funct body
    | PT_apply { funct; arg } -> fprintf fmt "%a %a" pp_apply funct pp_atom arg
    | PT_string { literal } ->
        (* TODO: proper escaping *)
        fprintf fmt "%S" literal
    | PT_forall { param; body } ->
        fprintf fmt "(%a) -> %a" pp_wrapped param pp_funct body
    | PT_inter { left; right } ->
        fprintf fmt "(%a) & %a" pp_wrapped left pp_funct right

  let rec pp_term prec fmt term =
    let pp_wrapped fmt term = pp_term T_wrapped fmt term in
    let pp_let fmt term = pp_term T_let fmt term in
    let pp_funct fmt term = pp_term T_funct fmt term in
    let pp_apply fmt term = pp_term T_apply fmt term in
    let pp_atom fmt term = pp_term T_atom fmt term in
    match (term, prec) with
    | ( (PT_meta _ | PT_var _ | PT_free_var _ | PT_bound_var _ | PT_string _),
        (T_wrapped | T_let | T_funct | T_apply | T_atom) )
    | PT_apply _, (T_wrapped | T_let | T_funct | T_apply)
    | (PT_lambda _ | PT_forall _ | PT_inter _), (T_wrapped | T_let | T_funct)
    | (PT_hoist _ | PT_let _), (T_wrapped | T_let)
    | PT_annot _, T_wrapped ->
        pp_term_syntax ~pp_wrapped ~pp_let ~pp_funct ~pp_apply ~pp_atom fmt term
    | PT_apply _, T_atom
    | (PT_lambda _ | PT_forall _ | PT_inter _), (T_apply | T_atom)
    | (PT_hoist _ | PT_let _), (T_funct | T_apply | T_atom)
    | PT_annot _, (T_let | T_funct | T_apply | T_atom) ->
        fprintf fmt "(%a)" pp_wrapped term

  let pp_term fmt term = pp_term T_wrapped fmt term
end

open Ttree
open Ptree

let _pt_with_type ~type_ term =
  PT_meta { term = PT_annot { term; annot = type_ } }

(* TODO: extract substitutions *)
(* TODO: rename all tt_ to term_ *)
let rec tt_print term =
  let (Term { struct_ = term; loc = _ }) = term in
  match term with
  | T_annot { term; annot } ->
      let term = tt_print term in
      let annot = tt_print annot in
      PT_annot { term; annot }
  | T_var { var } -> PT_bound_var { var }
  | T_let { bound; arg; body } ->
      let bound = tp_print bound in
      let arg = tt_print arg in
      let body = tt_print body in
      PT_let { bound; arg; body }
  | T_hoist { bound; body } ->
      let bound = vp_print bound in
      let body = tt_print body in
      PT_hoist { bound; body }
  | T_fix { bound; var = _; arg; body } ->
      (* TODO: proper var renaming *)
      let bound = vp_print bound in
      let arg = tt_print arg in
      let body = tt_print body in
      PT_let { bound; arg; body }
  | T_lambda { bound; body } ->
      let param = tp_print bound in
      let body = tt_print body in
      PT_lambda { param; body }
  | T_apply { funct; arg } ->
      let funct = tt_print funct in
      let arg = tt_print arg in
      PT_apply { funct; arg }
  | T_forall { bound; param; body } ->
      let param =
        let pat = tp_print bound in
        let annot = tt_print param in
        PT_annot { term = pat; annot }
      in
      let body = tt_print body in
      PT_forall { param; body }
  | T_self { bound; body } ->
      let left = vp_print bound in
      let right = tt_print body in
      (* TODO: self *)
      PT_inter { left; right }
  | T_tuple _ | T_exists _ -> failwith "not implemented"

and vp_print pat =
  let (VPat { struct_ = pat; loc = _ }) = pat in
  match pat with
  | VP_annot { pat; annot } ->
      let pat = vp_print pat in
      let annot = tt_print annot in
      PT_annot { term = pat; annot }
  | VP_var { var } -> PT_var { var }

and tp_print pat =
  let (Pat { struct_ = pat; loc = _ }) = pat in
  match pat with
  | P_annot { pat; annot } ->
      let pat = tp_print pat in
      let annot = tt_print annot in
      PT_annot { term = pat; annot }
  | P_var { var } -> PT_var { var }
  | P_tuple _ -> failwith "not implemented"

let pp_term fmt term =
  let term = tt_print term in
  Ptree.pp_term fmt term

let pp_pat fmt pat =
  let pat = tp_print pat in
  Ptree.pp_term fmt pat

module Perror = struct
  open Format
  open Utils
  open Ptree

  type error =
    | PE_loc of { loc : Location.t; error : error }
    | PE_type_clash
    | PE_unknown_var of { name : Name.t }
    | PE_not_a_forall of { type_ : term }
    | PE_hoist_not_implemented
    | PE_extensions_not_implemented
    | PE_pairs_not_implemented
    | PE_unknown_native of { native : string }
    | PE_missing_annotation
    | PE_invalid_notation

  let pp_pos fmt pos =
    let Lexing.{ pos_fname; pos_lnum; pos_bol; pos_cnum = _ } = pos in
    (* TODO: print only file by default? *)
    fprintf fmt "%s:%d:%d" pos_fname pos_lnum pos_bol

  let pp_loc fmt loc =
    let Location.{ loc_start; loc_end; loc_ghost = _ } = loc in
    match Location.is_none loc with
    | true -> fprintf fmt "[__NONE__]"
    | false -> fprintf fmt "[%a .. %a]" pp_pos loc_start pp_pos loc_end

  let rec pp_error fmt error =
    match error with
    | PE_loc { loc; error } -> fprintf fmt "%a\n%a" pp_loc loc pp_error error
    | PE_type_clash -> fprintf fmt "type clash"
    | PE_unknown_var { name } -> fprintf fmt "unknown variable %a" Name.pp name
    | PE_not_a_forall { type_ } ->
        fprintf fmt "expected forall\nreceived : %a" pp_term type_
    | PE_hoist_not_implemented -> fprintf fmt "hoist not implemented"
    | PE_extensions_not_implemented -> fprintf fmt "extensions not implemented"
    | PE_pairs_not_implemented -> fprintf fmt "pairs not implemented"
    | PE_unknown_native { native } -> fprintf fmt "unknown native : %S" native
    (* TODO: rename missing annotation *)
    | PE_missing_annotation -> fprintf fmt "not enough annotations"
    | PE_invalid_notation -> fprintf fmt "invalid notation"
end

let rec te_print error =
  let open Terror in
  let open Perror in
  match error with
  | TError_loc { error; loc } ->
      let rec loop loc error =
        match error with
        | TError_loc { error; loc = loc' } ->
            let loc =
              (* ignore none locations *)
              match Location.is_none loc' with
              | true -> loc
              | false -> loc'
            in
            loop loc error
        | error ->
            let error = te_print error in
            PE_loc { loc; error }
      in
      loop loc error
  | TError_type_clash -> PE_type_clash
  | TError_unknown_var { name } -> PE_unknown_var { name }
  | TError_not_a_forall { type_ } ->
      let type_ = tt_print type_ in
      PE_not_a_forall { type_ }
  | TError_extensions_not_implemented -> PE_extensions_not_implemented
  | TError_hoist_not_implemented -> PE_hoist_not_implemented
  | TError_pairs_not_implemented ->
      PE_pairs_not_implemented (* TODO: print payload *)
  | TError_unknown_native { native } -> PE_unknown_native { native }
  | TError_missing_annotation -> PE_missing_annotation
  | TError_invalid_notation -> PE_invalid_notation

let pp_error fmt error =
  let error = te_print error in
  Perror.pp_error fmt error
