open Ltree
open Ttree
open Context
open Typer_context
open Escape_check

let unify_term ~expected ~received =
  with_unify_context @@ fun () -> Unify.unify_term ~expected ~received

let open_term term =
  (* TODO: this opening is weird *)
  let+ to_ = level () in
  tt_open_bound ~from:Index.zero ~to_ term

let close_term term =
  (* TODO: this closing is weird *)
  let+ from = level () in
  tt_close_free ~from ~to_:Index.zero term

let tt_typed ~annot term = TT_typed { term; annot }
let tp_typed ~annot pat = TP_typed { pat; annot }

let split_forall (type a) (type_ : a term) =
  let param_type = tt_hole () in
  let param = tp_typed ~annot:param_type @@ tp_hole () in
  enter_level @@ fun () ->
  (* TODO: this is needed because unification is monomorphic *)
  let* return = open_term @@ tt_hole () in
  let* expected =
    let+ return = close_term return in
    TT_forall { param; return }
  in
  let+ () = unify_term ~received:type_ ~expected in
  (param_type, return)

let split_self (type a) (type_ : a term) =
  (* TODO: this is needed because unification is monomorphic *)
  enter_level @@ fun () ->
  let* body = open_term @@ tt_hole () in
  let* expected =
    let var = tp_hole () in
    let+ body = close_term body in
    TT_self { var; body }
  in
  let+ () = unify_term ~received:type_ ~expected in
  body

(* TODO: better place for this *)
let rec unfold_fix : type a. a term -> core term =
 fun term ->
  let open Expand_head in
  (* TODO: not ideal to expand head *)
  match expand_head_term term with
  | TT_bound_var _ as term -> term
  | TT_free_var _ as term -> term
  | TT_hole _ as term -> term
  | TT_forall _ as term -> term
  | TT_lambda _ as term -> term
  | TT_apply { lambda; arg } ->
      let lambda = unfold_fix lambda in
      let arg = unfold_fix arg in
      TT_apply { lambda; arg }
  | TT_self _ as term -> term
  | TT_fix _ as term -> term
  | TT_unroll { term = fix } as term -> (
      match expand_head_term fix with
      | TT_fix { var = _; body } as term ->
          expand_head_term @@ tt_subst_bound ~from:Index.zero ~to_:term body
      | _ -> term)

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
    let+ () = escape_check_term expected in
    tt_typed ~annot:expected term
  in
  match term with
  | LT_var { var = name } ->
      let* level, Ex_term received = lookup_var ~name in
      let* () = unify_term ~received ~expected in
      wrapped @@ TT_free_var { level }
  | LT_extension { extension; payload } ->
      check_term_extension ~extension ~payload ~expected
  | LT_forall { param; return } ->
      (* TODO: this could in theory be improved by expected term *)
      (* TODO: this could also be checked after the return *)
      let* () = unify_term ~received:tt_type ~expected in
      let param_type = tt_hole () in
      check_pat param ~expected:param_type @@ fun param ->
      let* return = check_annot return in
      let* return = close_term return in
      wrapped @@ TT_forall { param; return }
  | LT_lambda { param; return } ->
      (* TODO: maybe unify param? *)
      let* param_type, return_type = split_forall expected in
      check_pat param ~expected:param_type @@ fun param ->
      let* return = check_term return ~expected:return_type in
      let* return = close_term return in
      wrapped @@ TT_lambda { param; return }
  | LT_apply { lambda; arg } ->
      let lambda_type = tt_hole () in
      let* lambda = check_term lambda ~expected:lambda_type in
      (* TODO: this could be better? avoiding split forall? *)
      let* arg_type, return_type = split_forall lambda_type in
      let* arg = check_term arg ~expected:arg_type in
      let* () =
        (* TODO: abstract this *)
        let* level = level () in
        let from = Level.next level in
        let received = tt_subst_free ~from ~to_:arg return_type in
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
      (* TODO: type pattern first? *)
      check_pat pat ~expected:value_type @@ fun bound ->
      let* return = check_term return ~expected:return_type in
      let* () =
        (* TODO: abstract this *)
        let* from = level () in
        let received = tt_subst_free ~from ~to_:value return_type in
        unify_term ~received ~expected
      in
      wrapped @@ TT_let { bound; value; return }
  | LT_annot { term; annot } ->
      (* TODO: expected term could propagate here *)
      let* annot = check_annot annot in
      let* () = unify_term ~received:annot ~expected in
      (* TODO: unify annot before or after check term *)
      let* term = check_term term ~expected:annot in
      wrapped @@ TT_annot { term; annot }
  | LT_loc { term; loc } ->
      with_tt_loc ~loc @@ fun () -> check_term term ~expected

and check_term_extension :
    type a. extension:_ -> payload:_ -> expected:a term -> _ =
 fun ~extension ~payload ~expected ->
  let wrapped term =
    let+ () = escape_check_term expected in
    tt_typed ~annot:expected term
  in
  match (Name.repr extension, payload) with
  | _, LT_loc { term = payload; loc } ->
      with_tt_loc ~loc @@ fun () ->
      check_term_extension ~extension ~payload ~expected
  | "@self", LT_forall { param = self; return = body } ->
      (* TODO: this could in theory be improved by expected term *)
      (* TODO: this could also be checked after the return *)
      let* () = unify_term ~received:tt_type ~expected in
      let self_type = tt_hole () in
      let* expected_body = split_self self_type in
      check_pat_core self ~expected:self_type @@ fun var ->
      (* TODO: pattern on self *)
      let* body = check_annot body in
      let* () = unify_term ~received:body ~expected:expected_body in
      let* body = close_term body in
      wrapped @@ TT_self { var; body }
  | "@fix", LT_lambda { param = self; return = body } ->
      let* expected_body_type = split_self expected in
      check_pat_core self ~expected @@ fun var ->
      (* TODO: pattern on fix *)
      let* body = check_term body ~expected:expected_body_type in
      let* body = close_term body in
      wrapped @@ TT_fix { var; body }
  | "@unroll", fix ->
      let fix_type = tt_hole () in
      let* fix = check_term fix ~expected:fix_type in
      (* TODO: this could be better? avoiding split self? *)
      let* body_type = split_self fix_type in
      let* () =
        (* TODO: abstract this *)
        let* level = level () in
        let from = Level.next level in
        let received = tt_subst_free ~from ~to_:fix body_type in
        unify_term ~received ~expected
      in
      wrapped @@ TT_unroll { term = fix }
  | "@unfold", term ->
      (* TODO: breaks propagation *)
      let term_type = tt_hole () in
      let* term = check_term term ~expected:term_type in
      let* () =
        let received = unfold_fix term_type in
        unify_term ~received ~expected
      in
      wrapped @@ TT_unfold { term }
  | _ -> error_typer_unknown_extension ~extension ~payload

and check_annot term = check_term term ~expected:tt_type

and check_pat :
    type a k.
    _ -> expected:k term -> (typed pat -> a typer_context) -> a typer_context =
 fun pat ~expected f ->
  let* () = escape_check_term expected in
  check_pat_core pat ~expected @@ fun pat -> f @@ tp_typed ~annot:expected pat

and check_pat_core :
    type a k.
    _ -> expected:k term -> (core pat -> a typer_context) -> a typer_context =
 fun pat ~expected f ->
  (* TODO: expected should be a pattern, to achieve strictness *)
  match (pat, expected) with
  | LP_var { var = name }, expected ->
      (* TODO: add different names for left and right *)
      with_expected_var @@ fun () ->
      with_received_var ~name ~type_:expected @@ fun () -> f @@ TP_var { name }
  | LP_pair _, _ -> error_pairs_not_implemented ()
  | LP_annot { pat; annot }, _expected_desc ->
      (* TODO: TP_annot *)
      let* annot = check_annot annot in
      let* () = unify_term ~received:annot ~expected in
      check_pat_core pat ~expected:annot f
  | LP_loc { pat; loc }, _expected_desc ->
      with_loc ~loc @@ fun () -> check_pat_core pat ~expected f

let infer_term term =
  let expected = tt_hole () in
  check_term term ~expected
