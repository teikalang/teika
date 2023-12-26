open Ttree
open Context
open Tmachinery

(* TODO: maybe some quality of life, guarantee that unification always
    prefer the top level expected type, by tracking variance *)

(* TODO: optimization, just check, when type is closed *)
(* TODO: if every constructor had max_level,
    this could be short circuited, avoiding traversing
    constructors that have max_level < hole_level *)
(* Reject cases where a variable would be unified with itself,
     this would allow to create negatively recursive types.
   Example: (f => f f)

   Also reject cases where a variable would escape it's scope,
     this would allow to violate abstractions.
   Example: (x => A => (x : A)) *)
(* TODO: occurs should probably be on machinery *)
(* TODO: this is needed because of non injective functions
     the unification could succeed only if expand_head happened

   maybe should fail anyway ?
*)

(* TODO: for complete terms, there is no need to open during equality *)
open Unify_context

let rec tt_unify ~aliases ~expected ~received =
  let tt_unify ~expected ~received = tt_unify ~aliases ~expected ~received in
  let tpat_unify ~expected ~received =
    tpat_unify ~aliases ~expected ~received
  in
  (* TODO: short circuit physical equality *)
  match
    (tt_expand_head ~aliases expected, tt_expand_head ~aliases received)
  with
  (* TODO: annot equality?  *)
  | TT_annot _, _ | _, TT_annot _ -> error_annot_found ~expected ~received
  | TT_bound_var { index = expected }, TT_bound_var { index = received } -> (
      match Index.equal expected received with
      | true -> pure ()
      | false -> error_bound_var_clash ~expected ~received)
  | TT_free_var { level = expected }, TT_free_var { level = received } -> (
      match Level.equal expected received with
      | true -> pure ()
      | false -> error_free_var_clash ~expected ~received)
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
      | TT_let _ | TT_string _ | TT_native _ ),
      ( TT_bound_var _ | TT_free_var _ | TT_forall _ | TT_lambda _ | TT_apply _
      | TT_let _ | TT_string _ | TT_native _ ) ) ->
      error_type_clash ~expected ~received

and tpat_unify ~aliases ~expected ~received =
  (* TODO: pat? *)
  let (TPat { pat = expected_pat; type_ = expected_type }) = expected in
  let (TPat { pat = received_pat; type_ = received_type }) = received in
  let* () = tp_unify ~expected:expected_pat ~received:received_pat in
  tt_unify ~aliases ~expected:expected_type ~received:received_type

and tp_unify ~expected ~received =
  match (expected, received) with TP_var _, TP_var _ -> pure ()

and unify_native ~expected ~received =
  match (expected, received) with TN_debug, TN_debug -> pure ()
