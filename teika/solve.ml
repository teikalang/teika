open Utils
open Syntax
open Ctree
open Ttree
open Terror

exception Solve_error of { loc : Location.t; exn : exn }

(* TODO: context vs env *)
type context =
  | Context of { names : (bool * Level.t) Name.Map.t; next : Level.t }

let with_loc ~loc f =
  try f () with
  | Solve_error { loc; exn } ->
      (* TODO: reraise *)
      raise @@ Solve_error { loc; exn }
  | exn -> raise @@ Solve_error { loc; exn }

let name_of_pat pat =
  let (Pat { struct_ = pat; annot = _; loc = _ }) = pat in
  match pat with
  (* | P_annot { pat; annot = _ } -> name_of_pat pat *)
  | P_var { var } -> var

let enter ctx ~hoist pat =
  let name = name_of_pat pat in
  let (Context { names; next }) = ctx in
  let names = Name.Map.add name (hoist, next) names in
  let next = Level.next next in
  Context { names; next }

let lookup ctx name =
  let (Context { names; next }) = ctx in
  match Name.Map.find_opt name names with
  | Some (_is_hoist, from) -> (
      match Level.offset ~from ~to_:next with
      | Some var -> var
      | None -> failwith "compiler bug invalid var")
  | None -> error_unknown_var ~name

let enter_or_close ctx pat =
  let (Context { names; next }) = ctx in
  let name = name_of_pat pat in
  match Name.Map.find_opt name names with
  | Some (true, from) -> (
      match Level.offset ~from ~to_:next with
      | Some var ->
          let names = Name.Map.add name (false, from) names in
          (`Fix var, Context { names; next })
      | None -> failwith "compiler bug invalid var on fix")
  | Some (_, _) | None ->
      let names = Name.Map.add name (false, next) names in
      let next = Level.next next in
      (`Let, Context { names; next })

let rec solve_term ctx term =
  let (CTerm { term; loc }) = term in
  with_loc ~loc @@ fun () ->
  match term with
  | CT_parens { content = term } -> solve_term ctx term
  | CT_annot { value = term; annot } ->
      let annot = solve_term ctx annot in
      let term = solve_term ctx term in
      t_wrap ~loc @@ T_annot { term; annot }
  | CT_var { var = name } ->
      let var = lookup ctx name in
      t_wrap ~loc @@ T_var { var }
  | CT_semi { left; right } -> solve_semi ctx ~loc ~left ~right
  | CT_extension _ -> error_extensions_not_implemented ()
  | CT_apply { funct; arg } ->
      let funct = solve_term ctx funct in
      let arg = solve_term ctx arg in
      t_wrap ~loc @@ T_apply { funct; arg }
  | CT_lambda { param; body } ->
      let param = solve_pat ctx param in
      let body =
        let ctx = enter ctx ~hoist:false param in
        solve_term ctx body
      in
      t_wrap ~loc @@ T_lambda { param; body }
  | CT_forall { param; body } ->
      let param = solve_pat ctx param in
      let body =
        let ctx = enter ctx ~hoist:false param in
        solve_term ctx body
      in
      t_wrap ~loc @@ T_forall { param; body }
  | CT_both { left; right } ->
      let self = solve_pat ctx left in
      let body =
        let ctx = enter ctx ~hoist:false self in
        solve_term ctx right
      in
      t_wrap ~loc @@ T_self { self; body }
  | CT_pair _ | CT_bind _ | CT_number _ | CT_braces _ | CT_string _ ->
      error_invalid_notation ()

and solve_semi ctx ~loc ~left ~right =
  (* TODO: this is a lookahead *)
  match
    let (CTerm { term = left; loc = _ }) = left in
    (* TODO: with loc here? *)
    left
  with
  | CT_bind { bound; value } -> (
      let bound = solve_pat ctx bound in
      let arg = solve_term ctx value in
      let tag, body =
        let tag, ctx = enter_or_close ctx bound in
        (tag, solve_term ctx right)
      in
      match tag with
      | `Let -> t_wrap ~loc @@ T_let { bound; arg; body }
      | `Fix var -> t_wrap ~loc @@ T_fix { bound; var; arg; body })
  | CT_annot { value = _; annot = _ } ->
      let bound = solve_pat ctx left in
      let body =
        let ctx = enter ctx ~hoist:true bound in
        solve_term ctx right
      in
      t_wrap ~loc @@ T_hoist { bound; body }
  | CT_parens _ | CT_var _ | CT_extension _ | CT_forall _ | CT_lambda _
  | CT_apply _ | CT_pair _ | CT_both _ | CT_semi _ | CT_string _ | CT_number _
  | CT_braces _ ->
      error_invalid_notation ()

and solve_pat ctx pat =
  (* TODO: duplicated *)
  (* TODO: to_ here *)
  let (CTerm { term = pat; loc }) = pat in
  with_loc ~loc @@ fun () ->
  match pat with
  | CT_parens { content = pat } -> solve_pat ctx pat
  | CT_var { var } ->
      (* TODO: this sounds bad *)
      let annot = t_null in
      p_wrap ~loc ~annot @@ P_var { var }
  | CT_annot { value = pat; annot } ->
      let annot = solve_term ctx annot in
      (* TODO: P_annot is lost, but otherwise it would duplicate annots *)
      solve_check_pat ctx pat ~annot
  | _ -> error_invalid_notation ()

and solve_check_pat ctx pat ~annot =
  let (CTerm { term = pat; loc }) = pat in
  with_loc ~loc @@ fun () ->
  match pat with
  | CT_parens { content = pat } -> solve_check_pat ctx pat ~annot
  | CT_var { var } -> p_wrap ~loc ~annot @@ P_var { var }
  | CT_annot { value = pat; annot } ->
      let annot = solve_term ctx annot in
      solve_check_pat ctx pat ~annot
      (* TODO: recover P_annot on the tree *)
      (* p_wrap ~loc ~annot @@ P_annot { pat; annot } *)
  | _ -> error_invalid_notation ()

(* external *)
let solve_term ctx term = try Ok (solve_term ctx term) with exn -> Error exn

let initial =
  (* TODO: duplicated from Typer *)
  let next = Level.(next zero) in
  (* TODO: predef somewhere *)
  (* TODO: rename Type to data *)
  let type_ = Name.make "Type" in
  let names = Name.Map.(empty |> add type_ (false, Level.zero)) in
  Context { names; next }
