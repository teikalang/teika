[@@@ocaml.warning "-unused-constructor"]

open Ttree
open Tmachinery

module Ptree = struct
  open Format

  type term =
    | PT_subst of { term : term; subst : subst }
    | PT_var_name of { name : Name.t }
    | PT_var_index of { index : Index.t }
    | PT_var_level of { level : Level.t }
    | PT_hole_var_full of { id : int }
    | PT_forall of { param : term; return : term }
    | PT_lambda of { param : term; return : term }
    | PT_apply of { lambda : term; arg : term }
    | PT_self of { bound : term; body : term }
    | PT_fix of { bound : term; body : term }
    | PT_unroll of { term : term }
    | PT_unfold of { term : term }
    | PT_let of { bound : term; value : term; return : term }
    | PT_annot of { term : term; annot : term }
    | PT_string of { literal : string }
    (* TODO: very weird for native to be a string *)
    | PT_native of { native : string }

  and subst =
    | PS_id
    | PS_open of { to_ : Level.t }
    | PS_close of { from : Level.t }
    | PS_lift of { subst : subst }
    | PS_cons of { subst : subst; next : subst }

  type term_prec = T_wrapped | T_let | T_funct | T_apply | T_atom
  type subst_prec = S_wrapped | S_atom

  let pp_term_syntax ~pp_wrapped ~pp_let ~pp_funct ~pp_apply ~pp_atom
      ~pp_subst_wrapped fmt term =
    match term with
    | PT_subst { term; subst } ->
        (* TODO: better notation *)
        fprintf fmt "%a[%a]" pp_atom term pp_subst_wrapped subst
    | PT_var_name { name } -> fprintf fmt "%s" (Name.repr name)
    | PT_var_index { index } -> fprintf fmt "\\-%a" Index.pp index
    | PT_var_level { level } -> fprintf fmt "\\+%a" Level.pp level
    | PT_hole_var_full { id } -> fprintf fmt "_x%d" id
    | PT_forall { param; return } ->
        fprintf fmt "%a -> %a" pp_atom param pp_funct return
    | PT_lambda { param; return } ->
        fprintf fmt "%a => %a" pp_atom param pp_funct return
    | PT_apply { lambda; arg } ->
        fprintf fmt "%a %a" pp_apply lambda pp_atom arg
    | PT_self { bound; body } ->
        fprintf fmt "@self(%a -> %a)" pp_atom bound pp_wrapped body
    | PT_fix { bound; body } ->
        fprintf fmt "@fix(%a => %a)" pp_atom bound pp_wrapped body
    | PT_unroll { term } -> fprintf fmt "@unroll(%a)" pp_wrapped term
    | PT_unfold { term } -> fprintf fmt "@unfold(%a)" pp_wrapped term
    | PT_let { bound; value; return } ->
        fprintf fmt "%a = %a; %a" pp_atom bound pp_funct value pp_let return
    | PT_annot { term; annot } ->
        fprintf fmt "%a : %a" pp_funct term pp_wrapped annot
    | PT_string { literal } ->
        (* TODO: is this correct *)
        fprintf fmt {|%S|} literal
    | PT_native { native } ->
        (* TODO: this is clearly not the best way*)
        fprintf fmt {|@native(%S)|} native

  let pp_subst_syntax ~pp_wrapped ~pp_atom fmt term =
    match term with
    | PS_id -> fprintf fmt "id"
    | PS_open { to_ } -> fprintf fmt "open +%a" Level.pp to_
    | PS_close { from } -> fprintf fmt "close +%a" Level.pp from
    | PS_lift { subst } -> fprintf fmt "lift %a" pp_atom subst
    | PS_cons { subst; next } ->
        fprintf fmt "%a :: %a" pp_atom subst pp_wrapped next

  let rec pp_term prec fmt term =
    let pp_wrapped fmt term = pp_term T_wrapped fmt term in
    let pp_let fmt term = pp_term T_let fmt term in
    let pp_funct fmt term = pp_term T_funct fmt term in
    let pp_apply fmt term = pp_term T_apply fmt term in
    let pp_atom fmt term = pp_term T_atom fmt term in
    let pp_subst_wrapped fmt subst = pp_subst S_wrapped fmt subst in
    match (term, prec) with
    (* TODO: subst as atom is weird *)
    | ( ( PT_subst _ | PT_var_name _ | PT_var_index _ | PT_var_level _
        | PT_hole_var_full _ | PT_string _ ),
        (T_wrapped | T_let | T_funct | T_apply | T_atom) )
    | ( ( PT_apply _ | PT_self _ | PT_fix _ | PT_unroll _ | PT_unfold _
        | PT_native _ ),
        (T_wrapped | T_let | T_funct | T_apply) )
    | (PT_forall _ | PT_lambda _), (T_wrapped | T_let | T_funct)
    | PT_let _, (T_wrapped | T_let)
    | PT_annot _, T_wrapped ->
        pp_term_syntax ~pp_wrapped ~pp_let ~pp_funct ~pp_apply ~pp_atom
          ~pp_subst_wrapped fmt term
    | ( ( PT_apply _ | PT_self _ | PT_fix _ | PT_unroll _ | PT_unfold _
        | PT_native _ ),
        T_atom )
    | (PT_forall _ | PT_lambda _), (T_apply | T_atom)
    | PT_let _, (T_funct | T_apply | T_atom)
    | PT_annot _, (T_let | T_funct | T_apply | T_atom) ->
        fprintf fmt "(%a)" pp_wrapped term

  and pp_subst prec fmt subst =
    let pp_wrapped fmt subst = pp_subst S_wrapped fmt subst in
    let pp_atom fmt subst = pp_subst S_atom fmt subst in
    match (subst, prec) with
    | PS_id, (S_wrapped | S_atom)
    | (PS_open _ | PS_close _ | PS_lift _ | PS_cons _), S_wrapped ->
        pp_subst_syntax ~pp_wrapped ~pp_atom fmt subst
    | (PS_open _ | PS_close _ | PS_lift _ | PS_cons _), S_atom ->
        fprintf fmt "(%a)" pp_wrapped subst

  let pp_term fmt term = pp_term T_wrapped fmt term
  let pp_subst fmt subst = pp_subst S_wrapped fmt subst
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

let _should_print_typed config =
  match config.typed_mode with Typed_default -> false | Typed_force -> true

let _should_print_loc config ~loc =
  match config.loc_mode with
  | Loc_default -> false
  | Loc_force -> true
  | Loc_meaningful -> not (Location.is_none loc)

(* TODO: this is hackish *)
type ex_hole = Ex_hole : _ hole -> ex_hole [@@ocaml.unboxed]

let rec ptree_of_term config next holes term =
  let open Ptree in
  let ptree_of_term term = ptree_of_term config next holes term in
  let ptree_of_core_pat pat = ptree_of_core_pat config next holes pat in
  let ptree_of_typed_pat pat = ptree_of_typed_pat config next holes pat in
  let ptree_of_tt_hole hole ~subst =
    ptree_of_tt_hole config next holes hole ~subst
  in
  (* TODO: print details *)
  match tt_match term with
  (* TODO: bound var should not be reachable  *)
  | TT_bound_var { index } -> PT_var_index { index }
  | TT_free_var { level } -> PT_var_level { level }
  | TT_hole { hole; subst } ->
      (* TODO: print hole substs *)
      ptree_of_tt_hole ~subst @@ Ex_hole hole
  | TT_forall { param; return } ->
      let param = ptree_of_typed_pat param in
      let return = ptree_of_term return in
      PT_forall { param; return }
  | TT_lambda { param; return } ->
      let param = ptree_of_typed_pat param in
      let return = ptree_of_term return in
      PT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let lambda = ptree_of_term lambda in
      let arg = ptree_of_term arg in
      PT_apply { lambda; arg }
  | TT_self { var; body } ->
      let bound = ptree_of_core_pat var in
      let body = ptree_of_term body in
      PT_self { bound; body }
  | TT_fix { var; body } ->
      let bound = ptree_of_core_pat var in
      let body = ptree_of_term body in
      PT_fix { bound; body }
  | TT_unroll { term } ->
      let term = ptree_of_term term in
      PT_unroll { term }
  | TT_unfold { term } ->
      let term = ptree_of_term term in
      PT_unfold { term }
  | TT_let { bound; value; return } ->
      let bound = ptree_of_typed_pat bound in
      let value = ptree_of_term value in
      let return = ptree_of_term return in
      PT_let { bound; value; return }
  | TT_annot { term; annot } ->
      let term = ptree_of_term term in
      let annot = ptree_of_term annot in
      PT_annot { term; annot }
  | TT_string { literal } -> PT_string { literal }
  | TT_native { native } ->
      let native = match native with TN_debug -> "debug" in
      PT_native { native }

and ptree_of_subst config next holes subst =
  let open Ptree in
  let ptree_of_subst subst = ptree_of_subst config next holes subst in
  (* TODO: print details *)
  match subst with
  | TS_id -> PS_id
  | TS_open { to_ } -> PS_open { to_ }
  | TS_close { from } -> PS_close { from }
  | TS_lift { subst } ->
      let subst = ptree_of_subst subst in
      PS_lift { subst }
  | TS_cons { subst; next } ->
      let subst = ptree_of_subst subst in
      let next = ptree_of_subst next in
      PS_cons { subst; next }

and ptree_of_typed_pat config next holes pat =
  let open Ptree in
  let ptree_of_term term = ptree_of_term config next holes term in
  let ptree_of_core_pat term = ptree_of_core_pat config next holes term in
  let (TPat { pat; type_ }) = pat in
  (* TODO: calling this term is weird *)
  let term = ptree_of_core_pat pat in
  let type_ = ptree_of_term type_ in
  PT_annot { term; annot = type_ }

and ptree_of_core_pat config next holes pat =
  let open Ptree in
  let ptree_of_tpat_hole hole = ptree_of_tpat_hole config next holes hole in
  (* TODO: expand head here? *)
  match tp_repr pat with
  | TP_hole { hole } -> ptree_of_tpat_hole @@ Ex_hole hole
  | TP_var { name } -> PT_var_name { name }

and ptree_of_tt_hole config next holes hole ~subst =
  let open Ptree in
  let ptree_of_subst subst = ptree_of_subst config next holes subst in
  (* TODO: extract this into machinery tooling *)
  (* TODO: allow to print link *)
  match Hashtbl.find_opt holes hole with
  | Some term -> term
  | None ->
      let id = !next in
      let term = PT_hole_var_full { id } in
      let term =
        match subst with
        | TS_id -> term
        | subst ->
            let subst = ptree_of_subst subst in
            PT_subst { term; subst }
      in
      next := id + 1;
      Hashtbl.add holes hole term;
      term

and ptree_of_tpat_hole _config next holes hole =
  let open Ptree in
  (* TODO: extract this into machinery tooling *)
  (* TODO: allow to print link *)
  match Hashtbl.find_opt holes hole with
  | Some term -> term
  | None ->
      let id = !next in
      let term = PT_hole_var_full { id } in
      next := id + 1;
      Hashtbl.add holes hole term;
      term

let config =
  { typed_mode = Typed_default; loc_mode = Loc_default; var_mode = Var_name }

let pp_term fmt term =
  let next = ref 0 in
  let holes = Hashtbl.create 8 in
  let pterm = ptree_of_term config next holes term in
  Ptree.pp_term fmt pterm

let pp_term_hole fmt hole =
  let next = ref 0 in
  let holes = Hashtbl.create 8 in
  (* TODO: this is weird *)
  let pterm = ptree_of_tt_hole config next holes ~subst:TS_id @@ Ex_hole hole in
  Ptree.pp_term fmt pterm

let pp_subst fmt subst =
  let next = ref 0 in
  let holes = Hashtbl.create 8 in
  let psubst = ptree_of_subst config next holes subst in
  Ptree.pp_subst fmt psubst
