open Ttree
open Context
open Unify_context
open Expand_head

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

let rec occurs_term hole ~in_ =
  (* TODO: this is needed because of non injective functions
       the unification could succeed only if expand_head happened

     maybe should fail anyway ?
  *)
  let occurs_term ~in_ = occurs_term hole ~in_ in
  let occurs_typed_pat ~in_ = occurs_typed_pat hole ~in_ in
  match tt_match @@ expand_head_term in_ with
  | TT_subst { term; subst } -> occurs_term ~in_:(expand_subst_term ~subst term)
  | TT_bound_var { index = _ } -> return ()
  | TT_free_var { level = _; alias = _ } -> return ()
  (* TODO: use this substs? *)
  | TT_hole { hole = in_ } -> (
      match hole == in_ with
      | true -> error_var_occurs ~hole ~in_
      | false -> return ())
  | TT_forall { param; return } ->
      let* () = occurs_typed_pat ~in_:param in
      occurs_term ~in_:return
  | TT_lambda { param; return } ->
      let* () = occurs_typed_pat ~in_:param in
      occurs_term ~in_:return
  | TT_apply { lambda; arg } ->
      let* () = occurs_term ~in_:lambda in
      occurs_term ~in_:arg
  | TT_self { var = _; body } -> occurs_term ~in_:body
  | TT_fix { var = _; body } -> occurs_term ~in_:body
  | TT_unroll { term } -> occurs_term ~in_:term
  | TT_unfold { term } -> occurs_term ~in_:term
  | TT_let { bound; value; return } ->
      let* () = occurs_typed_pat ~in_:bound in
      let* () = occurs_term ~in_:value in
      occurs_term ~in_:return
  | TT_annot { term; annot } ->
      let* () = occurs_term ~in_:term in
      occurs_term ~in_:annot
  | TT_string { literal = _ } -> return ()
  | TT_native { native = _ } -> return ()

and occurs_typed_pat hole ~in_ =
  (* TODO: occurs inside of TP_hole *)
  let (TPat { pat = _; type_ }) = in_ in
  occurs_term hole ~in_:type_

let rec unify_term ~expected ~received =
  (* TODO: short circuit physical equality *)
  match
    ( tt_match @@ expand_head_term expected,
      tt_match @@ expand_head_term received )
  with
  (* TODO: annot and subst equality?  *)
  | TT_subst _, _ | _, TT_subst _ -> error_subst_found ~expected ~received
  | TT_annot _, _ | _, TT_annot _ -> error_annot_found ~expected ~received
  (* TODO: frozen and subst? *)
  | TT_bound_var { index = expected }, TT_bound_var { index = received } -> (
      match Index.equal expected received with
      | true -> return ()
      | false -> error_bound_var_clash ~expected ~received)
  | ( TT_free_var { level = expected; alias = _ },
      TT_free_var { level = received; alias = _ } ) -> (
      match Level.equal expected received with
      | true -> return ()
      | false -> error_free_var_clash ~expected ~received)
  | TT_hole { hole }, _ -> unify_term_hole hole ~to_:received
  | _, TT_hole { hole } -> unify_term_hole hole ~to_:expected
  (* TODO: track whenever it is unified and locations, visualizing inference *)
  | ( TT_forall { param = expected_param; return = expected_return },
      TT_forall { param = received_param; return = received_return } ) ->
      (* TODO: contravariance *)
      let* () =
        unify_typed_pat ~expected:expected_param ~received:received_param
      in
      unify_term ~expected:expected_return ~received:received_return
  | ( TT_lambda { param = expected_param; return = expected_return },
      TT_lambda { param = received_param; return = received_return } ) ->
      (* TODO: contravariance *)
      let* () =
        unify_typed_pat ~expected:expected_param ~received:received_param
      in
      unify_term ~expected:expected_return ~received:received_return
  | ( TT_apply { lambda = expected_lambda; arg = expected_arg },
      TT_apply { lambda = received_lambda; arg = received_arg } ) ->
      let* () =
        unify_term ~expected:expected_lambda ~received:received_lambda
      in
      unify_term ~expected:expected_arg ~received:received_arg
  | ( TT_self { var = _; body = expected_body },
      TT_self { var = _; body = received_body } ) ->
      unify_term ~expected:expected_body ~received:received_body
  | ( TT_fix { var = _; body = expected_body },
      TT_fix { var = _; body = received_body } ) ->
      unify_term ~expected:expected_body ~received:received_body
  | TT_unroll { term = expected }, TT_unroll { term = received } ->
      unify_term ~expected ~received
  | TT_unfold { term = expected }, TT_unfold { term = received } ->
      (* TODO: does unfold equality makes sense? *)
      unify_term ~expected ~received
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
      let* () =
        unify_typed_pat ~expected:expected_bound ~received:received_bound
      in
      let* () = unify_term ~expected:expected_value ~received:received_value in
      unify_term ~expected:expected_return ~received:received_return
  | TT_string { literal = expected }, TT_string { literal = received } -> (
      match String.equal expected received with
      | true -> return ()
      | false -> error_string_clash ~expected ~received)
  | TT_native { native = expected }, TT_native { native = received } ->
      unify_native ~expected ~received
  | ( ( TT_bound_var _ | TT_free_var _ | TT_forall _ | TT_lambda _ | TT_apply _
      | TT_self _ | TT_fix _ | TT_unroll _ | TT_unfold _ | TT_let _
      | TT_string _ | TT_native _ ),
      ( TT_bound_var _ | TT_free_var _ | TT_forall _ | TT_lambda _ | TT_apply _
      | TT_self _ | TT_fix _ | TT_unroll _ | TT_unfold _ | TT_let _
      | TT_string _ | TT_native _ ) ) ->
      error_type_clash ~expected ~received

and unify_term_hole hole ~to_ =
  match tt_match to_ with
  | TT_hole { hole = to_ } when hole == to_ -> return ()
  | _ ->
      (* TODO: prefer a direction when both are holes? *)
      let* () = occurs_term hole ~in_:to_ in
      hole.link <- Some to_;
      return ()

and unify_typed_pat ~expected ~received =
  (* TODO: pat? *)
  let (TPat { pat = expected_pat; type_ = expected_type }) = expected in
  let (TPat { pat = received_pat; type_ = received_type }) = received in
  let* () = unify_core_pat ~expected:expected_pat ~received:received_pat in
  unify_term ~expected:expected_type ~received:received_type

and unify_core_pat ~expected ~received =
  match (tp_repr expected, tp_repr received) with
  | TP_hole { hole }, pat | pat, TP_hole { hole } ->
      unify_pat_hole hole ~to_:pat
  | TP_var _, TP_var _ -> return ()

and unify_pat_hole hole ~to_ =
  match to_ with
  | TP_hole { hole = to_ } when hole == to_ -> return ()
  | _ ->
      (* TODO: prefer a direction when both are holes? *)
      (* TODO: occurs_pat? *)
      hole.link <- Some to_;
      return ()

and unify_native ~expected ~received =
  match (expected, received) with TN_debug, TN_debug -> return ()
