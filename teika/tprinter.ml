module Ptree = struct
  open Format
  open Utils

  type term =
    (* TODO: use PT_meta for level, subst and shift *)
    | PT_meta of { term : term }
    | PT_annot of { term : term; annot : term }
    | PT_var of { var : Ttree.var }
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
    | PT_var { var } ->
        let (TVar var) = var in
        fprintf fmt "%s" (Name.repr var.name)
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
    | ( (PT_meta _ | PT_var _ | PT_string _),
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
let rec tt_print term =
  let term =
    match term with
    | TTerm { term; type_ = _ } ->
        (* let term = tt_print term in
           let type_ = tt_print type_ in
           PT_annot { term; annot = type_ } *)
        term
    | TType { term } -> term
  in
  match term with
  | TT_annot { term; annot } ->
      let term = tt_print term in
      let annot = tt_print annot in
      PT_annot { term; annot }
  | TT_var { var } ->
      (* TODO: expand link sometimes? *)
      PT_var { var }
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
  let pat =
    match pat with
    | TPat { pat; type_ = _ } ->
        (* let pat = tp_print pat in
           let type_ = tt_print type_ in
           pt_with_type ~type_ pat *)
        pat
  in
  match pat with
  | TP_annot { pat; annot } ->
      let pat = tp_print pat in
      let annot = tt_print annot in
      PT_annot { term = pat; annot }
  | TP_var { var } -> PT_var { var }

let pp_term fmt term =
  let term = tt_print term in
  Ptree.pp_term fmt term

let pp_pat fmt pat =
  let pat = tp_print pat in
  Ptree.pp_term fmt pat

let pp_var fmt var = Ptree.pp_term fmt @@ PT_var { var }

module Perror = struct
  open Format
  open Utils
  open Ptree

  type error =
    | PE_loc of { loc : Location.t; error : error }
    | PE_var_clash of { left : var; right : var }
    | PE_type_clash of { left : term; right : term }
    | PE_string_clash of { left : string; right : string }
    | PE_unknown_var of { name : Name.t }
    | PE_not_a_forall of { type_ : term }
    | PE_extensions_not_implemented
    | PE_pairs_not_implemented
    | PE_erasable_not_implemented
    | PE_unknown_extension of { extension : Name.t }
    | PE_unknown_native of { native : string }
    | PE_missing_annotation

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
    | PE_var_clash { left; right } ->
        fprintf fmt "var clash\nexpected : %a\nreceived : %a" pp_var left pp_var
          right
    | PE_type_clash { left; right } ->
        fprintf fmt "type clash\nexpected : %a\nreceived : %a" pp_term left
          pp_term right
    | PE_string_clash { left; right } ->
        fprintf fmt "string clash\nexpected : %S\nreceived : %S" left right
    | PE_unknown_var { name } -> fprintf fmt "unknown variable %a" Name.pp name
    | PE_not_a_forall { type_ } ->
        fprintf fmt "expected forall\nreceived : %a" pp_term type_
    | PE_extensions_not_implemented -> fprintf fmt "extensions not implemented"
    | PE_pairs_not_implemented -> fprintf fmt "pairs not implemented"
    | PE_erasable_not_implemented -> fprintf fmt "erasable not implemented"
    | PE_unknown_extension { extension } ->
        fprintf fmt "unknown extension : %a" Name.pp extension
    | PE_unknown_native { native } -> fprintf fmt "unknown native : %S" native
    (* TODO: rename missing annotation *)
    | PE_missing_annotation -> fprintf fmt "not enough annotations"
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
  | TError_var_clash { left; right } -> PE_var_clash { left; right }
  | TError_type_clash { left; right } ->
      let left = tt_print left in
      let right = tt_print right in
      PE_type_clash { left; right }
  | TError_string_clash { left; right } -> PE_string_clash { left; right }
  | TError_unknown_var { name } -> PE_unknown_var { name }
  | TError_not_a_forall { type_ } ->
      let type_ = tt_print type_ in
      PE_not_a_forall { type_ }
  | TError_extensions_not_implemented -> PE_extensions_not_implemented
  | TError_pairs_not_implemented ->
      PE_pairs_not_implemented (* TODO: print payload *)
  | TError_erasable_not_implemented ->
      PE_erasable_not_implemented (* TODO: print payload *)
  | TError_unknown_extension { extension; payload = _ } ->
      PE_unknown_extension { extension }
  | TError_unknown_native { native } -> PE_unknown_native { native }
  | TError_missing_annotation -> PE_missing_annotation

let pp_error fmt error =
  let error = te_print error in
  Perror.pp_error fmt error
