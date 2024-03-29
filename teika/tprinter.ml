(* TODO: remove this *)
[@@@ocaml.warning "-unused-constructor"]

open Syntax
open Ttree
open Terror

module Ptree = struct
  open Format

  type term =
    | PT_var of { name : Name.t }
    | PT_hole of { name : Name.t }
    | PT_forall of { param : term; return : term }
    | PT_lambda of { param : term; return : term }
    | PT_apply of { lambda : term; arg : term }
    | PT_let of { bound : term; value : term; return : term }
    | PT_annot of { term : term; annot : term }
    | PT_string of { literal : string }
    (* TODO: very weird for native to be a string *)
    | PT_native of { native : string }
    (* low level *)
    (* TODO: weird to use string *)
    | PT_subst of { term : term; subst : subst }
    | PT_var_index of { name : Name.t option; index : Index.t }
    | PT_var_level of { name : Name.t option; level : Level.t }

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
    | PT_var { name } -> fprintf fmt "%s" (Name.repr name)
    | PT_hole { name } -> fprintf fmt "_%s" (Name.repr name)
    | PT_forall { param; return } ->
        fprintf fmt "%a -> %a" pp_atom param pp_funct return
    | PT_lambda { param; return } ->
        fprintf fmt "%a => %a" pp_atom param pp_funct return
    | PT_apply { lambda; arg } ->
        fprintf fmt "%a %a" pp_apply lambda pp_atom arg
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
    | PT_subst { term; subst } ->
        (* TODO: better notation *)
        fprintf fmt "%a[%a]" pp_atom term pp_subst_wrapped subst
    | PT_var_index { name; index } -> (
        match name with
        | Some name -> fprintf fmt "%s\\-%a" (Name.repr name) Index.pp index
        (* TODO: should this syntax be supported? Maybe as extension *)
        | None -> fprintf fmt "\\-%a" Index.pp index)
    | PT_var_level { name; level } -> (
        match name with
        | Some name -> fprintf fmt "%s\\-%a" (Name.repr name) Level.pp level
        (* TODO: should this syntax be supported? Maybe as extension *)
        | None -> fprintf fmt "\\+%a" Level.pp level)

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
    | ( ( PT_var _ | PT_hole _ | PT_string _ | PT_subst _ | PT_var_index _
        | PT_var_level _ ),
        (T_wrapped | T_let | T_funct | T_apply | T_atom) )
    | (PT_apply _ | PT_native _), (T_wrapped | T_let | T_funct | T_apply)
    | (PT_forall _ | PT_lambda _), (T_wrapped | T_let | T_funct)
    | PT_let _, (T_wrapped | T_let)
    | PT_annot _, T_wrapped ->
        pp_term_syntax ~pp_wrapped ~pp_let ~pp_funct ~pp_apply ~pp_atom
          ~pp_subst_wrapped fmt term
    | (PT_apply _ | PT_native _), T_atom
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

module Perror = struct
  open Format
  open Ptree

  type error =
    | PE_loc of { loc : Location.t; error : error }
    | PE_fallback of { error : Terror.error }
    | PE_var_escape of { var : Level.t }
    | PE_bound_var_clash of { expected : Index.t; received : Index.t }
    | PE_free_var_clash of { expected : Level.t; received : Level.t }
    | PE_type_clash of { expected : term; received : term }
    | PE_string_clash of { expected : string; received : string }
    | PE_unknown_var of { name : Name.t }
    | PE_not_a_forall of { type_ : term }
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
    | PE_fallback { error } -> Terror.pp fmt error
    | PE_var_escape { var } ->
        fprintf fmt "var +%a would escape the scope" Level.pp var
    | PE_bound_var_clash { expected; received } ->
        fprintf fmt "bound var clash\nexpected : -%a\nreceived : -%a" Index.pp
          expected Index.pp received
    | PE_free_var_clash { expected; received } ->
        fprintf fmt "free var clash\nexpected : +%a\nreceived : +%a" Level.pp
          expected Level.pp received
    | PE_type_clash { expected; received } ->
        fprintf fmt "type clash\nexpected : %a\nreceived : %a" pp_term expected
          pp_term received
    | PE_string_clash { expected; received } ->
        fprintf fmt "string clash\nexpected : %S\nreceived : %S" expected
          received
    | PE_unknown_var { name } -> fprintf fmt "unknown variable %a" Name.pp name
    | PE_not_a_forall { type_ } ->
        fprintf fmt "expected forall\nreceived : %a" pp_term type_
    | PE_pairs_not_implemented -> fprintf fmt "pairs not implemented"
    | PE_erasable_not_implemented -> fprintf fmt "erasable not implemented"
    | PE_unknown_extension { extension } ->
        fprintf fmt "unknown extension : %a" Name.pp extension
    | PE_unknown_native { native } -> fprintf fmt "unknown native : %S" native
    (* TODO: rename missing annotation *)
    | PE_missing_annotation -> fprintf fmt "not enough annotations"
end

(* TODO: move this into the context *)
let debug = ref false

module Printer_context : sig
  type printer_context

  val create :
    bound_vars:Name.t list -> free_vars:Name.t Level.Map.t -> printer_context

  val name_of_bound : printer_context -> Index.t -> Name.t option
  val name_of_free : printer_context -> Level.t -> Name.t option
  val with_var_bound : printer_context -> name:Name.t -> (unit -> 'a) -> 'a
end = struct
  (* TODO: aliases? *)
  type printer_context = {
    (* not locally closed terms *)
    (* TODO: use stack *)
    (* TODO: free bound vars *)
    mutable bound_vars : Name.t list;
    (* TODO: use stack *)
    (* TODO: shadowing *)
    free_vars : Name.t Level.Map.t;
  }

  let create ~bound_vars ~free_vars = { bound_vars; free_vars }
  let name_of_bound ctx index = List.nth_opt ctx.bound_vars (Index.repr index)
  let name_of_free ctx level = Level.Map.find_opt level ctx.free_vars

  let with_var_bound ctx ~name k =
    let snapshot = ctx.bound_vars in
    ctx.bound_vars <- name :: ctx.bound_vars;
    let value = k () in
    ctx.bound_vars <- snapshot;
    value
end

open Perror
open Printer_context

(* TODO: this is hackish *)
let name_of_core_pat pat = match pat with TP_var { name } -> name

let name_of_typed_pat pat =
  let (TPat { pat; type_ = _ }) = pat in
  name_of_core_pat pat

let with_var_typed_bound ctx param k =
  let name = name_of_typed_pat param in
  with_var_bound ctx ~name k

let rec ptree_of_term ctx term =
  let open Ptree in
  (* TODO: print details *)
  (* TODO: handle aliases *)
  match term with
  | TT_bound_var { index } -> (
      let name = name_of_bound ctx index in
      match (name, !debug) with
      | None, _ | _, true -> PT_var_index { name; index }
      | Some name, false -> PT_var { name })
  | TT_free_var { level } -> (
      let name = name_of_free ctx level in
      match (name, !debug) with
      | None, _ | _, true -> PT_var_level { name; level }
      | Some name, false -> PT_var { name })
  | TT_forall { param; return } ->
      let return =
        with_var_typed_bound ctx param @@ fun () -> ptree_of_term ctx return
      in
      (* TODO: weird order *)
      let param = ptree_of_typed_pat ctx param in
      PT_forall { param; return }
  | TT_lambda { param; return } ->
      let return =
        with_var_typed_bound ctx param @@ fun () -> ptree_of_term ctx return
      in
      let param = ptree_of_typed_pat ctx param in
      (* TODO: weird order *)
      PT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let lambda = ptree_of_term ctx lambda in
      let arg = ptree_of_term ctx arg in
      PT_apply { lambda; arg }
  | TT_let { bound; value; return } ->
      let value = ptree_of_term ctx value in
      let return =
        with_var_typed_bound ctx bound @@ fun () -> ptree_of_term ctx return
      in
      let bound = ptree_of_typed_pat ctx bound in
      (* TODO: weird order *)
      PT_let { bound; value; return }
  | TT_annot { term; annot } ->
      let term = ptree_of_term ctx term in
      let annot = ptree_of_term ctx annot in
      PT_annot { term; annot }
  | TT_string { literal } -> PT_string { literal }
  | TT_native { native } ->
      let native = match native with TN_debug -> "debug" in
      PT_native { native }

and ptree_of_subst ctx subst =
  let open Ptree in
  (* TODO: print details *)
  match subst with
  | TS_id -> PS_id
  | TS_open { to_ } -> PS_open { to_ }
  | TS_close { from } -> PS_close { from }
  | TS_lift { subst } ->
      let subst = ptree_of_subst ctx subst in
      PS_lift { subst }
  | TS_cons { subst; next } ->
      let subst = ptree_of_subst ctx subst in
      let next = ptree_of_subst ctx next in
      PS_cons { subst; next }

and ptree_of_typed_pat ctx pat =
  let open Ptree in
  let (TPat { pat; type_ }) = pat in
  (* TODO: calling this term is weird *)
  let term = ptree_of_core_pat pat in
  let type_ = ptree_of_term ctx type_ in
  PT_annot { term; annot = type_ }

and ptree_of_core_pat pat =
  let open Ptree in
  (* TODO: expand head here? *)
  match pat with TP_var { name } -> PT_var { name }

and perror_of_error ctx error =
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
            let error = perror_of_error ctx error in
            PE_loc { loc; error }
      in
      loop loc error
  (* TODO: drop falback *)
  | TError_misc_annot_found _ -> PE_fallback { error }
  | TError_misc_var_escape { var } -> PE_var_escape { var }
  | TError_unify_annot_found _ -> PE_fallback { error }
  | TError_unify_bound_var_clash { expected; received } ->
      PE_bound_var_clash { expected; received }
  | TError_unify_free_var_clash { expected; received } ->
      PE_free_var_clash { expected; received }
  | TError_unify_type_clash { expected; received } ->
      let expected = ptree_of_term ctx expected in
      let received = ptree_of_term ctx received in
      PE_type_clash { expected; received }
  | TError_unify_string_clash { expected; received } ->
      PE_string_clash { expected; received }
  | TError_typer_unknown_var { name } -> PE_unknown_var { name }
  | TError_typer_not_a_forall { type_ } ->
      let type_ = ptree_of_term ctx type_ in
      PE_not_a_forall { type_ }
  | TError_typer_pairs_not_implemented ->
      PE_pairs_not_implemented (* TODO: print payload *)
  | TError_typer_erasable_not_implemented ->
      PE_erasable_not_implemented (* TODO: print payload *)
  | TError_typer_unknown_extension { extension; payload = _ } ->
      PE_unknown_extension { extension }
  | TError_typer_unknown_native { native } -> PE_unknown_native { native }
  | TError_typer_missing_annotation -> PE_missing_annotation

let raw_pp_term ~bound_vars ~free_vars fmt term =
  let ctx = Printer_context.create ~bound_vars ~free_vars in
  let pterm = ptree_of_term ctx term in
  Ptree.pp_term fmt pterm

let raw_pp_subst ~bound_vars ~free_vars fmt subst =
  let ctx = Printer_context.create ~bound_vars ~free_vars in
  let psubst = ptree_of_subst ctx subst in
  Ptree.pp_subst fmt psubst

let raw_pp_error ~bound_vars ~free_vars fmt error =
  let ctx = Printer_context.create ~bound_vars ~free_vars in
  let perror = perror_of_error ctx error in
  Perror.pp_error fmt perror

let pp_term fmt term =
  raw_pp_term ~bound_vars:[] ~free_vars:Level.Map.empty fmt term

let pp_subst fmt subst =
  raw_pp_subst ~bound_vars:[] ~free_vars:Level.Map.empty fmt subst

let pp_error fmt error =
  raw_pp_error ~bound_vars:[] ~free_vars:Level.Map.empty fmt error
