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

let () =
  Printexc.register_printer @@ function
  | Solve_error { loc = _; exn } -> Some (Printexc.to_string exn)
  | _ -> None

let split_pat_annot pat =
  let (Pat { struct_ = pat; loc = _ }) = pat in
  match pat with
  | P_annot { pat; annot } -> (pat, annot)
  (* TODO: support tuple here *)
  | P_var _ | P_tuple _ -> error_missing_annotation ()

let rec enter ctx pat =
  let (Pat { struct_ = pat; loc = _ }) = pat in
  (* TODO: use this location? *)
  match pat with
  | P_annot { pat; annot = _ } -> enter ctx pat
  | P_var { var = name } ->
      let (Context { names; next }) = ctx in
      Format.eprintf "hi\n%!";
      let names = Name.Map.add name (false, next) names in
      let next = Level.next next in
      Context { names; next }
  | P_tuple { elements } ->
      List.fold_left (fun ctx el -> enter ctx el) ctx elements

let rec name_of_var_pat pat =
  let (VPat { struct_ = pat; loc = _ }) = pat in
  match pat with
  | VP_annot { pat; annot = _ } -> name_of_var_pat pat
  | VP_var { var } -> var

let open_hoist ctx pat =
  (* TODO: ensure that somehow all the hoists are closed *)
  let name = name_of_var_pat pat in
  let (Context { names; next }) = ctx in
  Format.eprintf "hi\n%!";
  let names = Name.Map.add name (true, next) names in
  let next = Level.next next in
  Context { names; next }

let close_hoist ctx pat =
  (* TODO: this is a bad API *)
  let name = name_of_var_pat pat in
  let (Context { names; next }) = ctx in
  let names =
    match Name.Map.find_opt name names with
    | Some (true, from) -> Name.Map.add name (false, from) names
    | Some (false, _from) -> failwith "compiler bug invalid name"
    | None -> failwith "close_hoist: compiler bug invalid name"
  in
  Context { names; next }

let lookup ctx name =
  let (Context { names; next }) = ctx in
  match Name.Map.find_opt name names with
  | Some (is_open_hoist, from) -> (
      match Level.offset ~from ~to_:next with
      | Some var -> (`hoist is_open_hoist, var)
      | None -> failwith "compiler bug invalid var")
  | None -> error_unknown_var ~name

let is_hoist ctx name =
  let (Context { names; next }) = ctx in
  match Name.Map.find_opt name names with
  | Some (is_open_hoist, from) -> (
      match Level.offset ~from ~to_:next with
      | Some var -> (
          match is_open_hoist with true -> Some var | false -> None)
      | None -> None)
  | None -> None

type meta_pat =
  | MP_simple of var_pat
  | MP_fancy of pat
  | MP_fix of Index.t * var_pat

let rec pat_of_var_pat var_pat =
  let (VPat { struct_ = var_pat; loc }) = var_pat in
  match var_pat with
  | VP_annot { pat; annot } ->
      let pat = pat_of_var_pat pat in
      p_wrap ~loc @@ P_annot { pat; annot }
  | VP_var { var } -> p_wrap ~loc @@ P_var { var }

let pat_not_fix meta_pat =
  match meta_pat with
  | MP_simple pat -> pat_of_var_pat pat
  | MP_fancy pat -> pat
  | MP_fix (_var, _pat) ->
      (* TODO: proper error here *)
      failwith "a variable with the same name is open on a hoist"

let self_pat_simple meta_pat =
  match meta_pat with
  | MP_simple pat -> pat
  | MP_fancy _pat -> failwith "fancy patterns are not supported on self"
  | MP_fix (_var, _pat) ->
      failwith "a variable with the same name is open on a hoist"

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
      (* TODO: this could be treated as forward *)
      let `hoist _, var = lookup ctx name in
      t_wrap ~loc @@ T_var { var }
  | CT_semi { left; right } -> solve_semi ctx ~loc ~left ~right
  | CT_extension _ -> error_extensions_not_implemented ()
  | CT_apply { funct; arg } ->
      let funct = solve_term ctx funct in
      let arg = solve_term ctx arg in
      t_wrap ~loc @@ T_apply { funct; arg }
  | CT_lambda { param; body } ->
      let bound = solve_pat ctx param in
      let bound = pat_not_fix bound in
      let body =
        let ctx = enter ctx bound in
        solve_term ctx body
      in
      t_wrap ~loc @@ T_lambda { bound; body }
  | CT_forall { param; body } ->
      let bound = solve_pat ctx param in
      let bound = pat_not_fix bound in
      let bound, param = split_pat_annot bound in
      let body =
        let ctx = enter ctx bound in
        solve_term ctx body
      in
      t_wrap ~loc @@ T_forall { bound; param; body }
  | CT_pair { left; right } ->
      let left = solve_term ctx left in
      let acc = [ left ] in
      let elements = solve_term_tuple ctx ~acc ~right in
      t_wrap ~loc @@ T_tuple { elements }
  | CT_both { left; right } ->
      let bound = solve_pat ctx left in
      let bound = self_pat_simple bound in
      let body =
        (* TODO: this is hackish *)
        let bound = pat_of_var_pat bound in
        let ctx = enter ctx bound in
        solve_term ctx right
      in
      t_wrap ~loc @@ T_self { bound; body }
  | CT_bind _ | CT_number _ | CT_braces _ | CT_string _ ->
      error_invalid_notation ()

and solve_term_tuple ctx ~acc ~right =
  match
    let (CTerm { term = right; loc = _ }) = right in
    right
  with
  | CT_pair { left; right } ->
      let left = solve_term ctx left in
      let acc = left :: acc in
      solve_term_tuple ctx ~acc ~right
  | CT_var _ | CT_extension _ | CT_forall _ | CT_lambda _ | CT_apply _
  | CT_both _ | CT_bind _ | CT_semi _ | CT_annot _ | CT_string _ | CT_number _
  | CT_parens _ | CT_braces _ ->
      let right = solve_term ctx right in
      List.rev (right :: acc)

and solve_semi ctx ~loc ~left ~right =
  let (CTerm { term = left_desc; loc = _ }) = left in
  match left_desc with
  | CT_bind { bound; value } -> (
      let bound = solve_pat ctx bound in
      (* TODO: just clean this *)
      match bound with
      | MP_simple bound ->
          let bound = pat_of_var_pat bound in
          let arg = solve_term ctx value in
          let body =
            let ctx = enter ctx bound in
            solve_term ctx right
          in
          t_wrap ~loc @@ T_let { bound; arg; body }
      | MP_fancy bound ->
          let arg = solve_term ctx value in
          let body =
            let ctx = enter ctx bound in
            solve_term ctx right
          in
          t_wrap ~loc @@ T_let { bound; arg; body }
      | MP_fix (var, bound) ->
          let arg = solve_term ctx value in
          let body =
            let ctx = close_hoist ctx bound in
            solve_term ctx right
          in
          t_wrap ~loc @@ T_fix { bound; var; arg; body })
  | CT_annot { value = _; annot = _ } ->
      let bound =
        match solve_pat ctx left with
        | MP_simple pat -> pat
        | MP_fancy _pat -> failwith "fancy patterns are not supported on hoist"
        | MP_fix (_var, _pat) ->
            failwith "a variable with the same name is already open"
      in
      let body =
        let ctx = open_hoist ctx bound in
        solve_term ctx right
      in
      t_wrap ~loc @@ T_hoist { bound; body }
  | CT_parens _ | CT_var _ | CT_extension _ | CT_forall _ | CT_lambda _
  | CT_apply _ | CT_pair _ | CT_both _ | CT_semi _ | CT_string _ | CT_number _
  | CT_braces _ ->
      error_invalid_notation ()

(* TODO: this code is kind of ugly *)
and solve_pat ctx pat = solve_pat_simple ctx pat

and solve_pat_simple ctx pat =
  let (CTerm { term = pat_desc; loc }) = pat in
  (* TODO: a bit duplicated *)
  match pat_desc with
  | CT_parens { content = pat } -> solve_pat_simple ctx pat
  | CT_var { var = name } -> (
      match is_hoist ctx name with
      | Some var -> MP_fix (var, vp_wrap ~loc @@ VP_var { var = name })
      | None -> MP_simple (vp_wrap ~loc @@ VP_var { var = name }))
  | CT_annot { value = pat; annot } -> (
      let annot = solve_term ctx annot in
      match solve_pat ctx pat with
      | MP_simple pat -> MP_simple (vp_wrap ~loc @@ VP_annot { pat; annot })
      | MP_fancy pat -> MP_fancy (p_wrap ~loc @@ P_annot { pat; annot })
      | MP_fix (var, pat) ->
          MP_fix (var, vp_wrap ~loc @@ VP_annot { pat; annot }))
  | CT_pair { left = _; right = _ } -> MP_fancy (solve_pat_fancy ctx pat)
  | CT_extension _ | CT_forall _ | CT_lambda _ | CT_apply _ | CT_both _
  | CT_bind _ | CT_semi _ | CT_string _ | CT_number _ | CT_braces _ ->
      error_invalid_notation ()

and solve_pat_fancy ctx pat =
  (* TODO: no duplicated name on pattern *)
  (* TODO: to_ here *)
  let (CTerm { term = pat; loc }) = pat in
  match pat with
  | CT_parens { content = pat } -> solve_pat_fancy ctx pat
  | CT_var { var = name } -> (
      match is_hoist ctx name with
      | Some _var -> failwith "hoist is not supported on fancy patterns"
      | None -> p_wrap ~loc @@ P_var { var = name })
  | CT_annot { value = pat; annot } ->
      let annot = solve_term ctx annot in
      let pat = solve_pat_fancy ctx pat in
      p_wrap ~loc @@ P_annot { pat; annot }
  | CT_pair { left; right } ->
      let left = solve_pat_fancy ctx left in
      let acc = [ left ] in
      let elements = solve_pat_tuple ctx ~acc ~right in
      p_wrap ~loc @@ P_tuple { elements }
  | CT_extension _ | CT_forall _ | CT_lambda _ | CT_apply _ | CT_both _
  | CT_bind _ | CT_semi _ | CT_string _ | CT_number _ | CT_braces _ ->
      error_invalid_notation ()

and solve_pat_tuple ctx ~acc ~right =
  match
    let (CTerm { term = right; loc = _ }) = right in
    right
  with
  | CT_pair { left; right } ->
      let left = solve_pat_fancy ctx left in
      let acc = left :: acc in
      solve_pat_tuple ctx ~acc ~right
  | CT_var _ | CT_extension _ | CT_forall _ | CT_lambda _ | CT_apply _
  | CT_both _ | CT_bind _ | CT_semi _ | CT_annot _ | CT_string _ | CT_number _
  | CT_parens _ | CT_braces _ ->
      let right = solve_pat_fancy ctx right in
      List.rev (right :: acc)

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
