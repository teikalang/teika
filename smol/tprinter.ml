[@@@ocaml.warning "-unused-constructor"]

open Ttree

module Ptree = struct
  open Format

  type term =
    | PT_typed of { term : term; type_ : term }
    | PT_var_name of { name : Name.t }
    | PT_var_full of { var : Var.t }
    | PT_forall of { param : term; return : term }
    | PT_lambda of { param : term; return : term }
    | PT_apply of { lambda : term; arg : term }
    | PT_self of { bound : term; body : term }
    | PT_fix of { bound : term; body : term }
    | PT_unroll of { term : term }
    | PT_alias of { bound : term; value : term; return : term }
    | PT_annot of { term : term; annot : term }

  let pp_term_syntax ~pp_wrapped ~pp_alias ~pp_funct ~pp_apply ~pp_atom fmt term
      =
    match term with
    | PT_typed { term; type_ } ->
        fprintf fmt "%a #:# %a" pp_funct term pp_wrapped type_
    | PT_var_name { name } -> fprintf fmt "%s" (Name.repr name)
    | PT_var_full { var } -> fprintf fmt "%a" Var.pp var
    | PT_forall { param; return } ->
        fprintf fmt "%a -> %a" pp_atom param pp_funct return
    | PT_lambda { param; return } ->
        fprintf fmt "%a => %a" pp_atom param pp_funct return
    | PT_apply { lambda; arg } ->
        fprintf fmt "%a %a" pp_apply lambda pp_atom arg
    | PT_self { bound; body } ->
        fprintf fmt "%a @-> %a" pp_atom bound pp_funct body
    | PT_fix { bound; body } ->
        fprintf fmt "%a @=> %a" pp_atom bound pp_funct body
    | PT_unroll { term } -> fprintf fmt "@%a" pp_atom term
    | PT_alias { bound; value; return } ->
        fprintf fmt "%a === %a; %a" pp_apply bound pp_funct value pp_alias
          return
    | PT_annot { term; annot } ->
        fprintf fmt "%a : %a" pp_funct term pp_wrapped annot

  type prec = Wrapped | Alias | Funct | Apply | Atom

  let rec pp_term prec fmt term =
    let pp_wrapped fmt term = pp_term Wrapped fmt term in
    let pp_alias fmt term = pp_term Alias fmt term in
    let pp_funct fmt term = pp_term Funct fmt term in
    let pp_apply fmt term = pp_term Apply fmt term in
    let pp_atom fmt term = pp_term Atom fmt term in
    match (term, prec) with
    | (PT_var_full _ | PT_var_name _), (Wrapped | Alias | Funct | Apply | Atom)
    | (PT_apply _ | PT_unroll _), (Wrapped | Alias | Funct | Apply)
    | ( (PT_forall _ | PT_lambda _ | PT_self _ | PT_fix _),
        (Wrapped | Alias | Funct) )
    | PT_alias _, (Wrapped | Alias)
    | (PT_typed _ | PT_annot _), Wrapped ->
        pp_term_syntax ~pp_wrapped ~pp_alias ~pp_funct ~pp_apply ~pp_atom fmt
          term
    | (PT_apply _ | PT_unroll _), Atom
    | (PT_forall _ | PT_lambda _ | PT_self _ | PT_fix _), (Apply | Atom)
    | PT_alias _, (Funct | Apply | Atom)
    | (PT_typed _ | PT_annot _), (Alias | Funct | Apply | Atom) ->
        fprintf fmt "(%a)" pp_wrapped term

  let pp_term fmt term = pp_term Wrapped fmt term
end
(* TODO: probably make printer tree *)

type typed_mode = Typed_default | Typed_force
type var_mode = Var_name | Var_full

(* TODO: mode to expand substitutions *)
type config = { typed_mode : typed_mode; var_mode : var_mode }

let should_print_typed config =
  match config.typed_mode with Typed_default -> false | Typed_force -> true

let ptree_of_var config var =
  let open Ptree in
  match config.var_mode with
  | Var_name ->
      let name = Var.name var in
      PT_var_name { name }
  | Var_full -> PT_var_full { var }

let rec ptree_of_term : type a. _ -> a term -> _ =
 fun config (term : a term) ->
  let open Ptree in
  let ptree_of_var var = ptree_of_var config var in
  let ptree_of_term term = ptree_of_term config term in
  let ptree_of_pat pat = ptree_of_pat config pat in
  match term with
  | TT_var { var } -> ptree_of_var var
  | TT_forall { param; return } ->
      let param = ptree_of_pat param in
      let return = ptree_of_term return in
      PT_forall { param; return }
  | TT_lambda { param; return } ->
      let param = ptree_of_pat param in
      let return = ptree_of_term return in
      PT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let lambda = ptree_of_term lambda in
      let arg = ptree_of_term arg in
      PT_apply { lambda; arg }
  | TT_self { bound; body } ->
      let bound = ptree_of_pat bound in
      let body = ptree_of_term body in
      PT_self { bound; body }
  | TT_fix { bound; body } ->
      let bound = ptree_of_pat bound in
      let body = ptree_of_term body in
      PT_fix { bound; body }
  | TT_unroll { term } ->
      let term = ptree_of_term term in
      PT_unroll { term }

and ptree_of_pat : type a. _ -> a pat -> _ =
 fun config pat ->
  let open Ptree in
  let ptree_of_var var = ptree_of_var config var in
  let ptree_of_term term = ptree_of_term config term in
  let ptree_of_pat pat = ptree_of_pat config pat in
  match pat with
  | TP_typed { pat; type_ } -> (
      let pat = ptree_of_pat pat in
      match should_print_typed config with
      | true ->
          let type_ = ptree_of_term type_ in
          (* TODO: calling this term is weird *)
          PT_typed { term = pat; type_ }
      | false -> pat)
  | TP_var { var } -> ptree_of_var var

let config = { typed_mode = Typed_default; var_mode = Var_name }

let pp_term fmt term =
  let pterm = ptree_of_term config term in
  Ptree.pp_term fmt pterm

let pp_pat fmt pat =
  let pterm = ptree_of_pat config pat in
  Ptree.pp_term fmt pterm

let pp_ex_term fmt (Ex_term term) = pp_term fmt term
let pp_ex_pat fmt (Ex_pat pat) = pp_pat fmt pat
