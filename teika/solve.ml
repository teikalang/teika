open Utils
open Syntax
open Ctree
open Ttree
open Terror

(* TODO: context vs env *)
type context = Context of { names : Level.t Name.Map.t; next : Level.t }

let enter ctx pat =
  let rec name pat =
    match pat with
    | P_annot { pat; annot = _ } -> name pat
    | P_var { var } -> var
  in
  let name = name pat in
  let (Context { names; next }) = ctx in
  let names = Name.Map.add name next names in
  let next = Level.next next in
  Context { names; next }

let lookup ctx name =
  let (Context { names; next }) = ctx in
  match Name.Map.find_opt name names with
  | Some from -> (
      match Index.of_int @@ ((next :> int) - (from :> int) - 1) with
      | Some var -> var
      | None ->
          (* TODO: proper errors *)
          failwith "invariant lookup")
  | None -> error_unknown_var ~name

let rec solve_term ctx term =
  (* TODO: use this location *)
  let (CTerm { term; loc = _ }) = term in
  match term with
  | CT_parens { content = term } -> solve_term ctx term
  | CT_annot { value = term; annot } ->
      let annot = solve_term ctx annot in
      let term = solve_term ctx term in
      T_annot { term; annot }
  | CT_var { var = name } ->
      let var = lookup ctx name in
      T_var { var }
  | CT_semi { left; right } -> solve_semi ctx ~left ~right
  | CT_extension _ -> error_extensions_not_implemented ()
  | CT_apply { funct; arg } ->
      let funct = solve_term ctx funct in
      let arg = solve_term ctx arg in
      T_apply { funct; arg }
  | CT_lambda { param; body } ->
      let bound = solve_check_pat ctx param in
      let body =
        let ctx = enter ctx bound in
        solve_term ctx body
      in
      T_lambda { bound; body }
  | CT_forall { param; body } ->
      let bound, param = solve_infer_pat ctx param in
      let body =
        let ctx = enter ctx bound in
        solve_term ctx body
      in
      T_forall { bound; param; body }
  | CT_both { left; right } ->
      let bound, left = solve_infer_pat ctx left in
      let right =
        let ctx = enter ctx bound in
        solve_term ctx right
      in
      T_inter { bound; left; right }
  | CT_pair _ | CT_bind _ | CT_number _ | CT_braces _ | CT_string _ ->
      error_invalid_notation ()

and solve_semi ctx ~left ~right =
  let (CTerm { term = left; loc = _ }) = left in
  match left with
  | CT_parens { content = left } -> solve_semi ctx ~left ~right
  | CT_bind { bound; value } ->
      let bound = solve_check_pat ctx bound in
      let arg = solve_term ctx value in
      let body =
        let ctx = enter ctx bound in
        solve_term ctx right
      in
      T_let { bound; arg; body }
  | CT_annot { value = _; annot = _ } -> error_hoist_not_implemented ()
  | CT_var _ | CT_extension _ | CT_forall _ | CT_lambda _ | CT_apply _
  | CT_pair _ | CT_both _ | CT_semi _ | CT_string _ | CT_number _ | CT_braces _
    ->
      error_invalid_notation ()

and solve_infer_pat ctx pat =
  (* TODO: duplicated *)
  (* TODO: to_ here *)
  (* TODO: use this location *)
  let (CTerm { term = pat; loc = _ }) = pat in
  match pat with
  | CT_parens { content = pat } -> solve_infer_pat ctx pat
  | CT_var { var = _ } -> error_missing_annotation ()
  | CT_annot { value = pat; annot } ->
      let annot = solve_term ctx annot in
      let pat = solve_check_pat ctx pat in
      (P_annot { pat; annot }, annot)
  | _ -> error_invalid_notation ()

and solve_check_pat ctx pat =
  (* TODO: to_ here *)
  (* TODO: use this location *)
  let (CTerm { term = pat; loc = _ }) = pat in
  match pat with
  | CT_parens { content = pat } -> solve_check_pat ctx pat
  | CT_var { var } -> P_var { var }
  | CT_annot { value = pat; annot } ->
      let annot = solve_term ctx annot in
      let pat = solve_check_pat ctx pat in
      P_annot { pat; annot }
  | _ -> error_invalid_notation ()

(* external *)
let initial =
  (* TODO: duplicated from Typer *)
  let next = Level.(next zero) in
  (* TODO: predef somewhere *)
  (* TODO: rename Type to data *)
  let type_ = Name.make "Type" in
  let names = Name.Map.(empty |> add type_ Level.zero) in
  Context { names; next }

let solve_term term = try Ok (solve_term initial term) with exn -> Error exn
