open Ttree
open Context
open Tmachinery

(* TODO: ensure this is eliminated *)

(* TODO: maybe some quality of life, guarantee that unification always
    prefer the top level expected type, by tracking variance *)
(* Reject cases where a variable would be unified with itself,
     this would allow to create negatively recursive types.
   Example: (f => f f)

   Also reject cases where a variable would escape it's scope,
     this would allow to violate abstractions.
   Example: (x => A => (x : A)) *)
(* TODO: if every constructor had max_level,
    this could be short circuited, avoiding traversing
    constructors that have max_level < hole_level *)

(* TODO: optimization, just check, when type is closed *)

(* TODO: occurs should probably be on it's own context *)

(* TODO: diff is a bad name *)

let rec tt_occurs ~aliases hole ~in_ =
  let open Var_context in
  (* TODO: this is needed because of non injective functions
       the unification could succeed only if expand_head happened

     maybe should fail anyway ?
  *)
  let tt_occurs ~in_ = tt_occurs ~aliases hole ~in_ in
  let tpat_occurs ~in_ = tpat_occurs ~aliases hole ~in_ in
  match tt_match @@ tt_expand_head ~aliases in_ with
  (* TODO: frozen and subst *)
  | TT_unfold _ -> error_unfold_found in_
  | TT_annot _ -> error_annot_found in_ (* TODO: escape check *)
  | TT_bound_var { index = _ } -> pure ()
  | TT_free_var { level = _ } -> pure ()
  (* TODO: use this substs? *)
  | TT_hole { hole = in_; subst = _ } -> (
      (* TODO: is it the same hole if substs are different? *)
      match hole == in_ with
      (* TODO: better error *)
      | true -> error_var_occurs ~hole ~in_
      | false -> pure ())
  | TT_forall { param; return } | TT_lambda { param; return } ->
      let* () = tpat_occurs ~in_:param in
      with_free_var @@ fun () -> tt_occurs ~in_:return
  | TT_apply { lambda; arg } ->
      let* () = tt_occurs ~in_:lambda in
      tt_occurs ~in_:arg
  | TT_self { var = _; body } | TT_fix { var = _; body } ->
      with_free_var @@ fun () -> tt_occurs ~in_:body
  | TT_unroll { term } -> tt_occurs ~in_:term
  | TT_let { bound; value; return } ->
      let* () = tpat_occurs ~in_:bound in
      let* () = tt_occurs ~in_:value in
      (* TODO: enter alias? *)
      with_free_var @@ fun () -> tt_occurs ~in_:return
  | TT_string { literal = _ } -> pure ()
  | TT_native { native = _ } -> pure ()

and tpat_occurs hole ~in_ =
  (* TODO: occurs inside of TP_hole *)
  let (TPat { pat = _; type_ }) = in_ in
  tt_occurs hole ~in_:type_

let unify_term_hole ~aliases hole ~subst ~to_ =
  let open Var_context in
  match tt_match to_ with
  (* TODO: what if subst is different *)
  | TT_hole { hole = to_; subst = _ } when hole == to_ -> pure ()
  | _ ->
      let to_ = tt_apply_subst to_ @@ ts_inverse subst in
      (* TODO: prefer a direction when both are holes? *)
      let* () = tt_occurs ~aliases hole ~in_:to_ in
      hole.link <- Some to_;
      pure ()

open Unify_context

(* TODO: for complete terms, there is no need to open during equality *)
let rec tt_unify ~aliases ~expected ~received =
  let tt_unify ~expected ~received = tt_unify ~aliases ~expected ~received in
  let tpat_unify ~expected ~received =
    tpat_unify ~aliases ~expected ~received
  in
  let unify_term_hole hole ~subst ~to_ =
    unify_term_hole ~aliases hole ~subst ~to_
  in
  (* TODO: short circuit physical equality *)
  match
    ( tt_match @@ tt_expand_head ~aliases expected,
      tt_match @@ tt_expand_head ~aliases received )
  with
  (* TODO: annot and unfold equality?  *)
  | TT_unfold _, _ | _, TT_unfold _ -> error_unfold_found ~expected ~received
  | TT_annot _, _ | _, TT_annot _ -> error_annot_found ~expected ~received
  | TT_bound_var { index = expected }, TT_bound_var { index = received } -> (
      match Index.equal expected received with
      | true -> pure ()
      | false -> error_bound_var_clash ~expected ~received)
  | TT_free_var { level = expected }, TT_free_var { level = received } -> (
      match Level.equal expected received with
      | true -> pure ()
      | false -> error_free_var_clash ~expected ~received)
  | TT_hole { hole; subst }, _ ->
      (* TODO: tests if wrong context is used *)
      with_received_var_context @@ fun () ->
      unify_term_hole hole ~subst ~to_:received
  | _, TT_hole { hole; subst } ->
      with_expected_var_context @@ fun () ->
      unify_term_hole hole ~subst ~to_:expected
  (* TODO: track whenever it is unified and locations, visualizing inference *)
  | ( TT_forall { param = expected_param; return = expected_return },
      TT_forall { param = received_param; return = received_return } )
  | ( TT_lambda { param = expected_param; return = expected_return },
      TT_lambda { param = received_param; return = received_return } ) ->
      (* TODO: contravariance *)
      let* () = tpat_unify ~expected:expected_param ~received:received_param in
      with_free_vars @@ fun () ->
      tt_unify ~expected:expected_return ~received:received_return
  | ( TT_apply { lambda = expected_lambda; arg = expected_arg },
      TT_apply { lambda = received_lambda; arg = received_arg } ) ->
      let* () = tt_unify ~expected:expected_lambda ~received:received_lambda in
      tt_unify ~expected:expected_arg ~received:received_arg
  | ( TT_self { var = _; body = expected_body },
      TT_self { var = _; body = received_body } )
  | ( TT_fix { var = _; body = expected_body },
      TT_fix { var = _; body = received_body } ) ->
      with_free_vars @@ fun () ->
      tt_unify ~expected:expected_body ~received:received_body
  | TT_unroll { term = expected }, TT_unroll { term = received } ->
      tt_unify ~expected ~received
  | ( TT_let
        {
          bound = expected_bound;
          value = expected_value;
          return = expected_return;
        },
      TT_let
        {
          bound = received_bound;
          value = received_value;
          return = received_return;
        } ) ->
      (* TODO: contravariance *)
      let* () = tpat_unify ~expected:expected_bound ~received:received_bound in
      let* () = tt_unify ~expected:expected_value ~received:received_value in
      tt_unify ~expected:expected_return ~received:received_return
  | TT_string { literal = expected }, TT_string { literal = received } -> (
      match String.equal expected received with
      | true -> pure ()
      | false -> error_string_clash ~expected ~received)
  | TT_native { native = expected }, TT_native { native = received } ->
      unify_native ~expected ~received
  | ( ( TT_bound_var _ | TT_free_var _ | TT_forall _ | TT_lambda _ | TT_apply _
      | TT_self _ | TT_fix _ | TT_unroll _ | TT_let _ | TT_string _
      | TT_native _ ),
      ( TT_bound_var _ | TT_free_var _ | TT_forall _ | TT_lambda _ | TT_apply _
      | TT_self _ | TT_fix _ | TT_unroll _ | TT_let _ | TT_string _
      | TT_native _ ) ) ->
      error_type_clash ~expected ~received

and tpat_unify ~aliases ~expected ~received =
  (* TODO: pat? *)
  let (TPat { pat = expected_pat; type_ = expected_type }) = expected in
  let (TPat { pat = received_pat; type_ = received_type }) = received in
  let* () = tp_unify ~expected:expected_pat ~received:received_pat in
  tt_unify ~aliases ~expected:expected_type ~received:received_type

and tp_unify ~expected ~received =
  match (tp_repr expected, tp_repr received) with
  | TP_hole { hole }, pat | pat, TP_hole { hole } ->
      unify_pat_hole hole ~to_:pat
  | TP_var _, TP_var _ -> pure ()

and unify_pat_hole hole ~to_ =
  match to_ with
  | TP_hole { hole = to_ } when hole == to_ -> pure ()
  | _ ->
      (* TODO: prefer a direction when both are holes? *)
      (* TODO: occurs_pat? *)
      hole.link <- Some to_;
      pure ()

and unify_native ~expected ~received =
  match (expected, received) with TN_debug, TN_debug -> pure ()
