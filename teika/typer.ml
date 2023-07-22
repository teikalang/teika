open Ltree
open Ttree
open Context
open Typer_context
open Escape_check

let unify_term ~expected ~received =
  with_unify_context @@ fun () -> Unify.unify_term ~expected ~received

let split_forall (type a) (type_ : a term) =
  let param = tt_hole () in
  let return = tt_hole () in
  let+ () =
    let expected = TT_forall { param; return } in
    unify_term ~received:type_ ~expected
  in
  (param, return)

let tt_typed ~annot term = TT_typed { term; annot }

let with_tt_loc ~loc f =
  with_loc ~loc @@ fun () ->
  let+ (TT_typed { term; annot }) = f () in
  let term = TT_loc { term; loc } in
  tt_typed ~annot term

(* TODO: does having expected_term also improves inference?
    Maybe with self and fix? But maybe not worth it *)
let rec check_term : type a. _ -> expected:a term -> _ =
 fun term ~expected ->
  (* TODO: propagation through dependent things *)
  let wrapped term =
    let+ () = escape_check expected in
    tt_typed ~annot:expected term
  in
  match term with
  | LT_var { var = name } ->
      let* level, Ex_term received = lookup_var ~name in
      let* () = unify_term ~received ~expected in
      wrapped @@ TT_free_var { level }
  | LT_forall { param; return } ->
      (* TODO: this could in theory be improved by expected term *)
      (* TODO: this could also be checked after the return *)
      let* () = unify_term ~received:tt_type ~expected in
      let expected_param = tt_hole () in
      check_pat param ~expected:expected_param @@ fun (Ex_term param) ->
      let* return = check_annot return in
      wrapped @@ TT_forall { param; return }
  | LT_lambda { param; return } ->
      let* expected_param, return_type = split_forall expected in
      check_pat param ~expected:expected_param @@ fun (Ex_term param) ->
      let* return = check_term return ~expected:return_type in
      wrapped @@ TT_lambda { param; return }
  | LT_apply { lambda; arg } ->
      let lambda_type = tt_hole () in
      let* lambda = check_term lambda ~expected:lambda_type in
      (* TODO: this could be better? avoiding split forall *)
      let* param, return_type = split_forall lambda_type in
      let* arg = check_term arg ~expected:param in
      let* () =
        (* TODO: this technically works here, but bad *)
        let (Ex_term received) =
          Subst.subst_bound ~from:Index.zero ~to_:arg return_type
        in
        unify_term ~received ~expected
      in
      wrapped @@ TT_apply { lambda; arg }
  | LT_exists _ -> error_pairs_not_implemented ()
  | LT_pair _ -> error_pairs_not_implemented ()
  | LT_let { bound; return } ->
      (* TODO: use this loc *)
      let (LBind { loc = _; pat; value }) = bound in
      let value_type = tt_hole () in
      let return_type = tt_hole () in
      let* value = check_term value ~expected:value_type in
      let* return =
        (* TODO: type pattern first? *)
        check_pat pat ~expected:value_type @@ fun _annot ->
        (* TODO: this annotation here is not used *)
        check_term return ~expected:return_type
      in
      let* () =
        (* TODO: this technically works here, but bad *)
        let (Ex_term received) =
          Subst.subst_bound ~from:Index.zero ~to_:value return_type
        in
        unify_term ~received ~expected
      in
      wrapped @@ TT_let { value; return }
  | LT_annot { term; annot } ->
      let* annot = check_annot annot in
      let* term = check_term term ~expected in
      wrapped @@ TT_annot { term; annot }
  | LT_loc { term; loc } ->
      with_tt_loc ~loc @@ fun () -> check_term term ~expected

and check_annot term = check_term term ~expected:tt_type

and check_pat :
    type a k. _ -> expected:k term -> (_ -> a typer_context) -> a typer_context
    =
 fun pat ~expected f ->
  (* TODO: expected should be a pattern, to achieve strictness *)
  match (pat, expected) with
  | LP_var { var = name }, expected ->
      (* TODO: add different names for left and right *)
      with_expected_var @@ fun () ->
      with_received_var ~name ~type_:expected @@ fun () -> f (Ex_term expected)
  | LP_pair _, _ -> error_pairs_not_implemented ()
  | LP_annot { pat; annot }, _expected_desc ->
      let* annot = check_annot annot in
      let* () = unify_term ~expected ~received:annot in
      check_pat pat ~expected:annot f
  | LP_loc { pat; loc }, _expected_desc ->
      with_loc ~loc @@ fun () -> check_pat pat ~expected f

let infer_term term =
  let expected = tt_hole () in
  check_term term ~expected
