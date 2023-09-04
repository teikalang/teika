open Ltree
open Ttree
open Context
open Typer_context
open Escape_check
open Unify

let escape_check_term term = with_var_context @@ fun () -> tt_escape_check term

let unify_term ~expected ~received =
  with_unify_context @@ fun () -> tt_unify ~expected ~received

let open_term term =
  (* TODO: this opening is weird *)
  let+ to_ = level () in
  let subst = TS_open_bound { from = Index.zero; to_ } in
  tt_map_desc term @@ fun ~wrap term _desc -> wrap @@ TT_subst { term; subst }

let close_term term =
  (* TODO: this closing is weird *)
  let+ from = level () in
  let subst = TS_close_free { from; to_ = Index.zero } in
  tt_map_desc term @@ fun ~wrap term _desc -> wrap @@ TT_subst { term; subst }

let ttype_subst_free ~from ~to_ term =
  let subst = TS_subst_free { from; to_ } in
  let desc = TT_subst { subst; term } in
  TType { desc }

(* TODO: loc *)
let ttype_hole () = TType { desc = tt_hole () }

let split_forall type_ =
  let param_type = ttype_hole () in
  let param = TPat { pat = tp_hole (); type_ = param_type } in
  enter_level @@ fun () ->
  (* TODO: this is needed because unification is monomorphic *)
  let* return = open_term @@ ttype_hole () in
  let* expected =
    let+ return = close_term return in
    let desc = TT_forall { param; return } in
    TType { desc }
  in
  let+ () = unify_term ~received:type_ ~expected in
  (param_type, return)

let split_self type_ =
  (* TODO: this is needed because unification is monomorphic *)
  enter_level @@ fun () ->
  let* body = open_term @@ ttype_hole () in
  let* expected =
    let var = tp_hole () in
    let+ body = close_term body in
    TType { desc = TT_self { var; body } }
  in
  let+ () = unify_term ~received:type_ ~expected in
  body

(* TODO: better place for this *)
let rec unfold_fix term =
  let open Expand_head in
  (* TODO: not ideal to expand head *)
  tt_map_desc term @@ fun ~wrap term desc ->
  match desc with
  | TT_subst { term; subst } -> unfold_fix @@ tt_expand_subst ~subst term
  | TT_bound_var _ -> term
  | TT_free_var _ -> term
  | TT_hole _ -> term
  | TT_forall _ -> term
  | TT_lambda _ -> term
  | TT_apply { lambda; arg } ->
      let lambda = unfold_fix lambda in
      let arg = unfold_fix arg in
      wrap @@ TT_apply { lambda; arg }
  | TT_self _ -> term
  | TT_fix _ -> term
  | TT_unroll { term = fix } -> (
      match tt_match @@ expand_head_term fix with
      | TT_fix { var = _; body } ->
          let subst = TS_subst_bound { from = Index.zero; to_ = fix } in
          expand_head_term @@ tt_expand_subst ~subst body
      | _ -> term)
  | TT_unfold { term } ->
      let term = unfold_fix term in
      wrap @@ TT_unfold { term }
      (* TODO: unfold under let?  *)
  | TT_let _ -> term
  | TT_annot { term; annot } ->
      let term = unfold_fix term in
      let annot = unfold_fix annot in
      wrap @@ TT_annot { term; annot }
  | TT_string _ -> term
  | TT_native _ -> term

(* TODO: does having expected_term also improves inference?
     Maybe with self and fix? But maybe not worth it
   Seems to help with many cases such as expected on annotation *)
let rec check_term term ~expected =
  (* TODO: propagation through dependent things *)
  let wrapped desc =
    let+ () = escape_check_term expected in
    TTerm { desc; type_ = expected }
  in
  match term with
  | LT_var { var = name } -> (
      let* level, received, alias = lookup_var ~name in
      let* () = unify_term ~received ~expected in
      match alias with
      | Some alias -> wrapped @@ TT_free_var { level; alias = Some alias }
      | None -> wrapped @@ TT_free_var { level; alias = None })
  | LT_extension { extension; payload } ->
      check_term_extension ~extension ~payload ~expected
  | LT_forall { param; return } ->
      (* TODO: this could in theory be improved by expected term *)
      (* TODO: this could also be checked after the return *)
      let* () = unify_term ~received:tt_type ~expected in
      let param_type = ttype_hole () in
      check_typed_pat param ~expected:param_type ~alias:None @@ fun param ->
      let* return = check_annot return in
      let* return = close_term return in
      wrapped @@ TT_forall { param; return }
  | LT_lambda { param; return } ->
      (* TODO: maybe unify param? *)
      let* param_type, return_type = split_forall expected in
      check_typed_pat param ~expected:param_type ~alias:None @@ fun param ->
      let* return = check_term return ~expected:return_type in
      let* return = close_term return in
      wrapped @@ TT_lambda { param; return }
  | LT_apply { lambda; arg } ->
      let lambda_type = ttype_hole () in
      let* lambda = check_term lambda ~expected:lambda_type in
      (* TODO: this could be better? avoiding split forall? *)
      let* arg_type, return_type = split_forall lambda_type in
      let* arg = check_term arg ~expected:arg_type in
      let* () =
        (* TODO: abstract this *)
        let* level = level () in
        let from = Level.next level in
        let received = ttype_subst_free ~from ~to_:arg return_type in
        unify_term ~received ~expected
      in
      wrapped @@ TT_apply { lambda; arg }
  | LT_exists _ -> error_pairs_not_implemented ()
  | LT_pair _ -> error_pairs_not_implemented ()
  | LT_let { bound; return } ->
      (* TODO: use this loc *)
      let (LBind { loc = _; pat; value }) = bound in
      let value_type = ttype_hole () in
      let return_type = ttype_hole () in
      let* value = check_term value ~expected:value_type in
      (* TODO: type pattern first? *)
      check_typed_pat pat ~expected:value_type ~alias:(Some value)
      @@ fun bound ->
      let* return = check_term return ~expected:return_type in
      let* () =
        (* TODO: abstract this *)
        let* from = level () in
        let received = ttype_subst_free ~from ~to_:value return_type in
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
  | LT_string { literal } ->
      let* () = unify_term ~expected ~received:string_type in
      wrapped @@ TT_string { literal }
  | LT_loc { term; loc = _ } ->
      (* TODO: use loc *)
      check_term term ~expected

and check_term_extension ~extension ~payload ~expected =
  let wrapped desc =
    let+ () = escape_check_term expected in
    TTerm { desc; type_ = expected }
  in
  match (Name.repr extension, payload) with
  | _, LT_loc { term = payload; loc = _ } ->
      (* TODO: use location *)
      check_term_extension ~extension ~payload ~expected
  | "@self", LT_forall { param = self; return = body } ->
      (* TODO: this could in theory be improved by expected term *)
      (* TODO: this could also be checked after the return *)
      let* () = unify_term ~received:tt_type ~expected in
      let self_type = ttype_hole () in
      let* expected_body = split_self self_type in
      check_core_pat self ~expected:self_type ~alias:None @@ fun var ->
      (* TODO: pattern on self *)
      let* body = check_annot body in
      let* () = unify_term ~received:body ~expected:expected_body in
      let* body = close_term body in
      wrapped @@ TT_self { var; body }
  | "@fix", LT_lambda { param = self; return = body } ->
      let* expected_body_type = split_self expected in
      check_core_pat self ~expected ~alias:None @@ fun var ->
      (* TODO: pattern on fix *)
      let* body = check_term body ~expected:expected_body_type in
      let* body = close_term body in
      wrapped @@ TT_fix { var; body }
  | "@unroll", fix ->
      let fix_type = ttype_hole () in
      let* fix = check_term fix ~expected:fix_type in
      (* TODO: this could be better? avoiding split self? *)
      let* body_type = split_self fix_type in
      let* () =
        (* TODO: abstract this *)
        let* level = level () in
        let from = Level.next level in
        let received = ttype_subst_free ~from ~to_:fix body_type in
        unify_term ~received ~expected
      in
      wrapped @@ TT_unroll { term = fix }
  | "@unfold", term ->
      (* TODO: breaks propagation *)
      let term_type = ttype_hole () in
      let* term = check_term term ~expected:term_type in
      let* () =
        let received = unfold_fix term_type in
        unify_term ~received ~expected
      in
      wrapped @@ TT_unfold { term }
  | "@native", LT_string { literal = native } ->
      check_term_native ~native ~expected
  | _ -> error_unknown_extension ~extension ~payload

and check_term_native ~native ~expected =
  let wrapped desc =
    let+ () = escape_check_term expected in
    TTerm { desc; type_ = expected }
  in
  match native with
  | "debug" ->
      (* TODO: use this types? *)
      let* _param_type, _return = split_forall expected in
      wrapped @@ TT_native { native = TN_debug }
  | native -> error_unknown_native ~native

and check_annot term = check_term term ~expected:tt_type

and check_typed_pat pat ~expected ~alias f =
  (* TODO: why this escape check? *)
  let* () = escape_check_term expected in
  check_core_pat pat ~expected ~alias @@ fun pat ->
  f @@ TPat { pat; type_ = expected }

and check_core_pat pat ~expected ~alias f =
  (* TODO: expected should be a pattern, to achieve strictness *)
  match (pat, expected) with
  | LP_var { var = name }, expected ->
      (* TODO: add different names for left and right *)
      with_free_vars ~name ~type_:expected ~alias @@ fun () ->
      f @@ TP_var { name }
  | LP_pair _, _ -> error_pairs_not_implemented ()
  | LP_annot { pat; annot }, _expected_desc ->
      (* TODO: TP_annot *)
      let* annot = check_annot annot in
      let* () = unify_term ~received:annot ~expected in
      check_core_pat pat ~expected:annot ~alias f
  | LP_loc { pat; loc }, _expected_desc ->
      with_loc ~loc @@ fun () -> check_core_pat pat ~expected ~alias f

let infer_term term =
  let expected = ttype_hole () in
  check_term term ~expected
