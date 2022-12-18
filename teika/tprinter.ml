[@@@ocaml.warning "-unused-constructor"]

open Ttree

module Ptree = struct
  open Format

  type term =
    | PT_loc of { term : term; loc : Location.t }
    | PT_var_index of { index : Offset.t }
    | PT_var_name of { name : Name.t }
    | PT_forall of { param : term; return : term }
    | PT_lambda of { param : term; return : term }
    | PT_apply of { lambda : term; arg : term }
    | PT_annot of { term : term; annot : term }

  let pp_loc fmt loc =
    let pp_pos fmt pos =
      let Lexing.{ pos_fname; pos_lnum; pos_bol; pos_cnum = _ } = pos in
      (* TODO: print only file by default? *)
      fprintf fmt "%s:%d:%d" pos_fname pos_lnum pos_bol
    in
    let Location.{ loc_start; loc_end; loc_ghost = _ } = loc in
    match Location.is_none loc with
    | true -> fprintf fmt "[%a .. %a]" pp_pos loc_start pp_pos loc_end
    | false -> fprintf fmt "[__NONE__]"

  let pp_term_syntax ~pp_wrapped ~pp_funct ~pp_apply ~pp_atom fmt term =
    match term with
    | PT_loc { term; loc } -> fprintf fmt "%a#%a" pp_atom term pp_loc loc
    | PT_var_index { index } -> fprintf fmt "\\%d" (Offset.repr index)
    | PT_var_name { name } -> fprintf fmt "%s" (Name.repr name)
    | PT_forall { param; return } ->
        fprintf fmt "%a -> %a" pp_atom param pp_funct return
    | PT_lambda { param; return } ->
        fprintf fmt "%a => %a" pp_atom param pp_funct return
    | PT_apply { lambda; arg } ->
        fprintf fmt "%a %a" pp_apply lambda pp_atom arg
    | PT_annot { term; annot } ->
        fprintf fmt "%a : %a" pp_funct term pp_wrapped annot

  type prec = Wrapped | Funct | Apply | Atom

  let rec pp_term prec fmt term =
    let pp_wrapped fmt term = pp_term Wrapped fmt term in
    let pp_funct fmt term = pp_term Funct fmt term in
    let pp_apply fmt term = pp_term Apply fmt term in
    let pp_atom fmt term = pp_term Atom fmt term in
    match (term, prec) with
    | ( (PT_loc _ | PT_var_index _ | PT_var_name _),
        (Wrapped | Funct | Apply | Atom) )
    | PT_apply _, (Wrapped | Funct | Apply)
    | (PT_forall _ | PT_lambda _), (Wrapped | Funct)
    | PT_annot _, Wrapped ->
        pp_term_syntax ~pp_wrapped ~pp_funct ~pp_apply ~pp_atom fmt term
    | PT_apply _, Atom
    | (PT_forall _ | PT_lambda _), (Apply | Atom)
    | PT_annot _, (Funct | Apply | Atom) ->
        fprintf fmt "(%a)" pp_wrapped term

  let pp_term fmt term = pp_term Wrapped fmt term
end
(* TODO: probably make printer tree *)

type loc_mode = Loc_default | Loc_meaningful | Loc_force
type var_mode = Var_name | Var_index | Var_both

let should_print_loc ~loc_mode ~loc =
  match loc_mode with
  | Loc_default -> false
  | Loc_force -> true
  | Loc_meaningful -> not (Location.is_none loc)

let rec ptree_of_term : type a. loc_mode:_ -> var_mode:_ -> _ -> a term -> _ =
 fun ~loc_mode ~var_mode offset (term : a term) ->
  let open Ptree in
  let ptree_of_term offset term =
    ptree_of_term ~loc_mode ~var_mode offset term
  in
  let ptree_of_pat offset pat = ptree_of_pat ~loc_mode ~var_mode offset pat in
  match term with
  | TT_loc { term; loc } -> (
      let term = ptree_of_term offset term in
      match should_print_loc ~loc_mode ~loc with
      | true -> PT_loc { term; loc }
      | false -> term)
  | TT_var { offset = index } ->
      let index = Offset.(offset + index) in
      PT_var_index { index }
  | TT_forall { param; return } ->
      let param = ptree_of_pat offset param in
      let return = ptree_of_term offset return in
      PT_forall { param; return }
  | TT_lambda { param; return } ->
      let param = ptree_of_pat offset param in
      let return = ptree_of_term offset return in
      PT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let lambda = ptree_of_term offset lambda in
      let arg = ptree_of_term offset arg in
      PT_apply { lambda; arg }
  | TT_annot { term; annot } ->
      let term = ptree_of_term offset term in
      let annot = ptree_of_term offset annot in
      PT_annot { term; annot }

and ptree_of_pat : type a. loc_mode:_ -> var_mode:_ -> _ -> a pat -> _ =
 fun ~loc_mode ~var_mode offset pat ->
  let open Ptree in
  let ptree_of_term term = ptree_of_term ~loc_mode ~var_mode offset term in
  let ptree_of_pat pat = ptree_of_pat ~loc_mode ~var_mode offset pat in
  match pat with
  | TP_loc { pat; loc } -> (
      let pat = ptree_of_pat pat in
      match should_print_loc ~loc_mode ~loc with
      (* TODO: calling this term is weird *)
      | true -> PT_loc { term = pat; loc }
      | false -> pat)
  | TP_var { var = name } -> PT_var_name { name }
  | TP_annot { pat; annot } ->
      let pat = ptree_of_pat pat in
      let annot = ptree_of_term annot in
      PT_annot { term = pat; annot }

let loc_mode = Loc_default
let var_mode = Var_index

let pp_term fmt term =
  let pterm = ptree_of_term ~loc_mode ~var_mode Offset.zero term in
  Ptree.pp_term fmt pterm

let pp_pat fmt pat =
  let pterm = ptree_of_pat ~loc_mode ~var_mode Offset.zero pat in
  Ptree.pp_term fmt pterm

let pp_ex_term fmt (Ex_term term) = pp_term fmt term
let pp_ex_pat fmt (Ex_pat pat) = pp_pat fmt pat
