[@@@ocaml.warning "-unused-constructor"]

open Ttree

module Ptree = struct
  open Format

  type term =
    | PT_loc of { term : term; loc : Location.t }
    | PT_typed of { term : term; annot : term }
    | PT_var_name of { name : Name.t }
    | PT_var_index of { index : Index.t }
    | PT_var_level of { level : Level.t }
    | PT_hole_var_full of { id : int }
    | PT_forall of { var : Name.t; param : term; return : term }
    | PT_lambda of { var : Name.t; param : term; return : term }
    | PT_apply of { lambda : term; arg : term }
    | PT_let of { var : Name.t; value : term; return : term }
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

  let pp_term_syntax ~pp_wrapped ~pp_let ~pp_funct ~pp_apply ~pp_atom fmt term =
    match term with
    | PT_loc { term; loc } -> fprintf fmt "%a#%a" pp_atom term pp_loc loc
    | PT_typed { term; annot } ->
        fprintf fmt "%a #:# %a" pp_funct term pp_wrapped annot
    | PT_var_name { name } -> fprintf fmt "%s" (Name.repr name)
    | PT_var_index { index } -> fprintf fmt "\\-%a" Index.pp index
    | PT_var_level { level } -> fprintf fmt "\\+%a" Level.pp level
    | PT_hole_var_full { id } -> fprintf fmt "_x%d" id
    | PT_forall { var; param; return } ->
        fprintf fmt "(%s : %a) -> %a" (Name.repr var) pp_wrapped param pp_funct
          return
    | PT_lambda { var; param; return } ->
        fprintf fmt "(%s : %a) => %a" (Name.repr var) pp_wrapped param pp_funct
          return
    | PT_apply { lambda; arg } ->
        fprintf fmt "%a %a" pp_apply lambda pp_atom arg
    | PT_let { var; value; return } ->
        fprintf fmt "%s = %a; %a" (Name.repr var) pp_funct value pp_let return
    | PT_annot { term; annot } ->
        fprintf fmt "%a : %a" pp_funct term pp_wrapped annot

  type prec = Wrapped | Let | Funct | Apply | Atom

  let rec pp_term prec fmt term =
    let pp_wrapped fmt term = pp_term Wrapped fmt term in
    let pp_let fmt term = pp_term Let fmt term in
    let pp_funct fmt term = pp_term Funct fmt term in
    let pp_apply fmt term = pp_term Apply fmt term in
    let pp_atom fmt term = pp_term Atom fmt term in
    match (term, prec) with
    | ( ( PT_loc _ | PT_var_name _ | PT_var_index _ | PT_var_level _
        | PT_hole_var_full _ ),
        (Wrapped | Let | Funct | Apply | Atom) )
    | PT_apply _, (Wrapped | Let | Funct | Apply)
    | (PT_forall _ | PT_lambda _), (Wrapped | Let | Funct)
    | PT_let _, (Wrapped | Let)
    | (PT_typed _ | PT_annot _), Wrapped ->
        pp_term_syntax ~pp_wrapped ~pp_let ~pp_funct ~pp_apply ~pp_atom fmt term
    | PT_apply _, Atom
    | (PT_forall _ | PT_lambda _), (Apply | Atom)
    | PT_let _, (Funct | Apply | Atom)
    | (PT_typed _ | PT_annot _), (Let | Funct | Apply | Atom) ->
        fprintf fmt "(%a)" pp_wrapped term

  let pp_term fmt term = pp_term Wrapped fmt term
end
(* TODO: probably make printer tree *)

type typed_mode = Typed_default | Typed_force
type loc_mode = Loc_default | Loc_meaningful | Loc_force
type var_mode = Var_name | Var_full

type config = {
  typed_mode : typed_mode;
  loc_mode : loc_mode;
  var_mode : var_mode;
}

let should_print_typed config =
  match config.typed_mode with Typed_default -> false | Typed_force -> true

let should_print_loc config ~loc =
  match config.loc_mode with
  | Loc_default -> false
  | Loc_force -> true
  | Loc_meaningful -> not (Location.is_none loc)

let rec ptree_of_term : type a. _ -> _ -> _ -> a term -> _ =
 fun config next holes (term : a term) ->
  let open Ptree in
  let ptree_of_term term = ptree_of_term config next holes term in
  let ptree_of_hole term = ptree_of_hole config next holes term in
  match term with
  | TT_loc { term; loc } -> (
      let term = ptree_of_term term in
      match should_print_loc config ~loc with
      | true -> PT_loc { term; loc }
      | false -> term)
  | TT_typed { term; annot } -> (
      let term = ptree_of_term term in
      match should_print_typed config with
      | true ->
          let annot = ptree_of_term annot in
          PT_typed { term; annot }
      | false -> term)
  | TT_bound_var { index } -> PT_var_index { index }
  | TT_free_var { level } -> PT_var_level { level }
  | TT_hole hole -> ptree_of_hole hole
  | TT_forall { param; return } ->
      let var = Name.make "_" in
      let param = ptree_of_term param in
      let return = ptree_of_term return in
      PT_forall { var; param; return }
  | TT_lambda { param; return } ->
      let var = Name.make "_" in
      let param = ptree_of_term param in
      let return = ptree_of_term return in
      PT_lambda { var; param; return }
  | TT_apply { lambda; arg } ->
      let lambda = ptree_of_term lambda in
      let arg = ptree_of_term arg in
      PT_apply { lambda; arg }
  | TT_let { value; return } ->
      let var = Name.make "_" in
      let value = ptree_of_term value in
      let return = ptree_of_term return in
      PT_let { var; value; return }
  | TT_annot { term; annot } ->
      let term = ptree_of_term term in
      let annot = ptree_of_term annot in
      PT_annot { term; annot }

and ptree_of_hole config next holes hole =
  let open Ptree in
  let ptree_of_term term = ptree_of_term config next holes term in
  (* TODO: extract this into machinery tooling *)
  (* TODO: allow to print link *)
  match hole.link == tt_nil with
  | true -> (
      match Hashtbl.find_opt holes hole with
      | Some term -> term
      | None ->
          let id = !next in
          let term = PT_hole_var_full { id } in
          next := id + 1;
          Hashtbl.add holes hole term;
          term)
  | false -> ptree_of_term hole.link

let config =
  { typed_mode = Typed_default; loc_mode = Loc_default; var_mode = Var_name }

let pp_term fmt term =
  let next = ref 0 in
  let holes = Hashtbl.create 8 in
  let pterm = ptree_of_term config next holes term in
  Ptree.pp_term fmt pterm

let pp_hole fmt hole =
  let next = ref 0 in
  let holes = Hashtbl.create 8 in
  let pterm = ptree_of_hole config next holes hole in
  Ptree.pp_term fmt pterm

let pp_ex_term fmt (Ex_term term) = pp_term fmt term
