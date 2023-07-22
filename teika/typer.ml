open Ltree
open Ttree
open Context
open Typer_context
open Escape_check

let unify_term ~expected ~received =
  with_unify_context @@ fun () -> Unify.unify_term ~expected ~received

let open_term term =
  (* TODO: this opening is weird *)
  let+ level = level () in
  let to_ = TT_free_var { level } in
  tt_subst_bound ~from:Index.zero ~to_ term

let close_term term =
  (* TODO: this closing is weird *)
  let+ from = level () in
  tt_close_free ~from ~to_:Index.zero term

let split_forall (type a) (type_ : a term) =
  let param = tt_hole () in
  (* TODO: this is needed because unification is monomorphic *)
  let* return = open_term (tt_hole ()) in
  let* expected =
    let+ return = close_term return in
    TT_forall { param; return }
  in
  let+ () = unify_term ~received:type_ ~expected in
  (param, return)

let tt_typed ~annot term = TT_typed { term; annot }

let with_tt_loc ~loc f =
  with_loc ~loc @@ fun () ->
  let+ (TT_typed { term; annot }) = f () in
  let term = TT_loc { term; loc } in
  tt_typed ~annot term

(* TODO: does having expected_term also improves inference?
     Maybe with self and fix? But maybe not worth it
   Seems to help with many cases such as expected on annotation *)
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
      let param_type = tt_hole () in
      let* return =
        check_pat param ~expected:param_type @@ fun () -> check_annot return
      in
      let* return = close_term return in
      wrapped @@ TT_forall { param = param_type; return }
  | LT_lambda { param; return } ->
      let* param_type, return_type = split_forall expected in
      let* return =
        check_pat param ~expected:param_type @@ fun () ->
        check_term return ~expected:return_type
      in
      let* return = close_term return in
      wrapped @@ TT_lambda { param = param_type; return }
  | LT_apply { lambda; arg } ->
      let lambda_type = tt_hole () in
      let* lambda = check_term lambda ~expected:lambda_type in
      (* TODO: this could be better? avoiding split forall? *)
      let* arg_type, return_type = split_forall lambda_type in
      let* arg = check_term arg ~expected:arg_type in
      let* () =
        (* TODO: this is clearly a hack *)
        let* return_type = close_term return_type in
        let received = tt_subst_bound ~from:Index.zero ~to_:arg return_type in
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
        check_pat pat ~expected:value_type @@ fun () ->
        check_term return ~expected:return_type
      in
      let* return_type = close_term return_type in
      let* () =
        (* TODO: this technically works here, but bad *)
        (* TODO: tt_subst_free *)
        let received = tt_subst_bound ~from:Index.zero ~to_:value return_type in
        unify_term ~received ~expected
      in
      wrapped @@ TT_let { value; return }
  | LT_annot { term; annot } ->
      (* TODO: expected term could propagate here *)
      let* annot = check_annot annot in
      let* () = unify_term ~received:annot ~expected in
      (* TODO: unify annot before or after check term *)
      let* term = check_term term ~expected:annot in
      wrapped @@ TT_annot { term; annot }
  | LT_loc { term; loc } ->
      with_tt_loc ~loc @@ fun () -> check_term term ~expected

and check_annot term = check_term term ~expected:tt_type

and check_pat :
    type a k.
    _ -> expected:k term -> (unit -> a typer_context) -> a typer_context =
 fun pat ~expected f ->
  (* TODO: expected should be a pattern, to achieve strictness *)
  match (pat, expected) with
  | LP_var { var = name }, expected ->
      (* TODO: add different names for left and right *)
      with_expected_var @@ fun () ->
      with_received_var ~name ~type_:expected @@ fun () -> f ()
  | LP_pair _, _ -> error_pairs_not_implemented ()
  | LP_annot { pat; annot }, _expected_desc ->
      let* annot = check_annot annot in
      let* () = unify_term ~received:annot ~expected in
      check_pat pat ~expected:annot f
  | LP_loc { pat; loc }, _expected_desc ->
      with_loc ~loc @@ fun () -> check_pat pat ~expected f

let infer_term term =
  let expected = tt_hole () in
  check_term term ~expected
