module Ptree = struct
  open Format
  open Utils

  type term =
    (* TODO: use PT_meta for level, subst and shift *)
    | PT_meta of { term : term }
    | PT_annot of { term : term; annot : term }
    | PT_var of { name : Name.t }
    | PT_free_var of { var : Level.t }
    | PT_bound_var of { var : Index.t }
    | PT_forall of { param : term; return : term }
    | PT_lambda of { param : term; return : term }
    | PT_apply of { lambda : term; arg : term }
    | PT_let of { bound : term; value : term; return : term }
    | PT_string of { literal : string }

  type term_prec = T_wrapped | T_let | T_funct | T_apply | T_atom

  let pp_term_syntax ~pp_wrapped ~pp_let ~pp_funct ~pp_apply ~pp_atom fmt term =
    match term with
    | PT_meta { term } -> fprintf fmt "#%a" pp_atom term
    | PT_annot { term; annot } ->
        fprintf fmt "%a : %a" pp_funct term pp_wrapped annot
    | PT_var { name } -> fprintf fmt "%s" (Name.repr name)
    | PT_free_var { var } -> fprintf fmt "\\+%a" Level.pp var
    | PT_bound_var { var } -> fprintf fmt "\\-%a" Index.pp var
    | PT_forall { param; return } ->
        fprintf fmt "%a -> %a" pp_atom param pp_funct return
    | PT_lambda { param; return } ->
        fprintf fmt "%a => %a" pp_atom param pp_funct return
    | PT_apply { lambda; arg } ->
        fprintf fmt "%a %a" pp_apply lambda pp_atom arg
    | PT_let { bound; value; return } ->
        fprintf fmt "%a = %a; %a" pp_atom bound pp_funct value pp_let return
    | PT_string { literal } ->
        (* TODO: proper escaping *)
        fprintf fmt "%S" literal

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
    | (PT_forall _ | PT_lambda _), (T_wrapped | T_let | T_funct)
    | PT_let _, (T_wrapped | T_let)
    | PT_annot _, T_wrapped ->
        pp_term_syntax ~pp_wrapped ~pp_let ~pp_funct ~pp_apply ~pp_atom fmt term
    | PT_apply _, T_atom
    | (PT_forall _ | PT_lambda _), (T_apply | T_atom)
    | PT_let _, (T_funct | T_apply | T_atom)
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
  match term with
  | T_annot { term; annot } ->
      let term = tt_print term in
      let annot = tt_print annot in
      PT_annot { term; annot }
  | T_var { var } -> PT_bound_var { var }
  (* TODO: expand subst sometimes? *)
  | T_bound_var { var } -> PT_bound_var { var }
  | TT_forall { param; return } ->
      let param = tp_print param in
      let return = tt_print return in
      PT_forall { param; return }
  | TT_lambda { param; return } ->
      let param = tp_print param in
      let return = tt_print return in
      PT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let lambda = tt_print lambda in
      let arg = tt_print arg in
      PT_apply { lambda; arg }
  | TT_let { bound; value; return } ->
      let bound = tp_print bound in
      let value = tt_print value in
      let return = tt_print return in
      PT_let { bound; value; return }
  | TT_string { literal } -> PT_string { literal }

and tp_print pat =
  let (TPat { pat; type_ = _ }) = pat in
  match pat with
  | TP_annot { pat; annot } ->
      let pat = tp_print pat in
      let annot = tt_print annot in
      PT_annot { term = pat; annot }
  | TP_var { name } -> PT_var { name }

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
    | PE_type_clash of { left : term; right : term }
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
    | PE_type_clash { left; right } ->
        fprintf fmt "type clash\nreceived : %a\nexpected : %a" pp_term left
          pp_term right
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
              match Location.is_none loc' with true -> loc | false -> loc'
            in
            loop loc error
        | error ->
            let error = te_print error in
            PE_loc { loc; error }
      in
      loop loc error
  (* TODO: drop falback *)
  | TError_type_clash ->
      let left = tt_print left in
      let right = tt_print right in
      PE_type_clash { left; right }
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
