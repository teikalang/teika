open Ttree
module Unify_context = Context.Unify_context (Normalize)
open Unify_context

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
let rec unify_term ~expected ~received =
  (* TODO: use those locations for something? *)
  let (TTerm { loc = _; desc = expected_desc; type_ = expected_type }) =
    expected
  in
  let (TTerm { loc = _; desc = received_desc; type_ = received_type }) =
    received
  in
  (* TODO: why is this needed? *)
  let* () = unify_type ~expected:expected_type ~received:received_type in
  unify_desc ~expected:expected_desc ~received:received_desc

and unify_type ~expected ~received =
  (* TODO: use those locations for something? *)
  let (TType { loc = _; desc = expected }) = expected in
  let (TType { loc = _; desc = received }) = received in
  unify_desc ~expected ~received

and unify_pat ~expected ~received =
  let (TPat { loc = _; desc = expected; type_ = expected_type }) = expected in
  let (TPat { loc = _; desc = received; type_ = received_type }) = received in
  (* TODO: why unify this? *)
  let* () = unify_type ~expected:expected_type ~received:received_type in
  unify_pat_desc ~expected ~received

and unify_pat_desc ~expected ~received =
  match (expected, received) with
  | TP_var { var = _ }, TP_var { var = _ } -> return ()
  | ( TP_pair { left = expected_left; right = expected_right },
      TP_pair { left = received_left; right = received_right } ) ->
      let* () = unify_pat ~expected:expected_left ~received:received_left in
      unify_pat ~expected:expected_right ~received:received_right
  | TP_annot { pat = expected; annot = _ }, received ->
      let (TPat { loc = _; desc = expected; type_ = _ }) = expected in
      unify_pat_desc ~expected ~received
  | expected, TP_annot { pat = received; annot = _ } ->
      let (TPat { loc = _; desc = received; type_ = _ }) = received in
      unify_pat_desc ~expected ~received
  | (TP_var _ | TP_pair _), (TP_var _ | TP_pair _) ->
      error_pat_clash ~expected ~received

and unify_annot ~expected ~received =
  let (TAnnot { loc = _; pat = expected_pat; annot = expected_annot }) =
    expected
  in
  let (TAnnot { loc = _; pat = received_pat; annot = received_annot }) =
    received
  in
  let* () = unify_type ~expected:expected_annot ~received:received_annot in
  unify_pat ~expected:expected_pat ~received:received_pat

and unify_bind ~expected ~received =
  let (TBind { loc = _; pat = expected_pat; value = expected_value }) =
    expected
  in
  let (TBind { loc = _; pat = received_pat; value = received_value }) =
    received
  in
  let* () = unify_term ~expected:expected_value ~received:received_value in
  unify_pat ~expected:expected_pat ~received:received_pat

and unify_desc ~expected ~received =
  match (expected, received) with
  | TT_var { offset = expected }, TT_var { offset = received } -> (
      let* expected = repr_expected_var ~var:expected in
      let* received = repr_received_var ~var:received in
      match Offset.equal expected received with
      | true -> return ()
      | false -> error_var_clash ~expected ~received)
  (* TODO: track whenever it is unified and locations, visualizing inference *)
  | ( TT_forall { param = expected_param; return = expected_return },
      TT_forall { param = received_param; return = received_return } ) ->
      (* TODO: contravariance *)
      let* () = unify_annot ~expected:expected_param ~received:received_param in
      unify_type ~expected:expected_return ~received:received_return
  | ( TT_lambda { param = expected_param; return = expected_return },
      TT_lambda { param = received_param; return = received_return } ) ->
      (* TODO: contravariance *)
      let* () = unify_annot ~expected:expected_param ~received:received_param in
      unify_term ~expected:expected_return ~received:received_return
  | ( TT_apply { lambda = expected_lambda; arg = expected_arg },
      TT_apply { lambda = received_lambda; arg = received_arg } ) ->
      let* () =
        unify_term ~expected:expected_lambda ~received:received_lambda
      in
      unify_term ~expected:expected_arg ~received:received_arg
  | ( TT_exists { left = expected_left; right = expected_right },
      TT_exists { left = received_left; right = received_right } ) ->
      let* () = unify_annot ~expected:expected_left ~received:received_left in
      unify_annot ~expected:expected_right ~received:received_right
  | ( TT_pair { left = expected_left; right = expected_right },
      TT_pair { left = received_left; right = received_right } ) ->
      let* () = unify_bind ~expected:expected_left ~received:received_left in
      unify_bind ~expected:expected_right ~received:received_right
  | ( TT_let { bound = expected_bound; return = expected_return },
      TT_let { bound = received_bound; return = received_return } ) ->
      let* () = unify_bind ~expected:expected_bound ~received:received_bound in
      unify_term ~expected:expected_return ~received:received_return
  (* TODO: why matching patterns? *)
  | ( TT_annot { value = expected_value; annot = expected_annot },
      TT_annot { value = received_value; annot = received_annot } ) ->
      let* () = unify_type ~expected:expected_annot ~received:received_annot in
      unify_term ~expected:expected_value ~received:received_value
  | TT_offset { desc = expected; offset }, received ->
      with_expected_offset ~offset @@ fun () -> unify_desc ~expected ~received
  | expected, TT_offset { desc = received; offset } ->
      with_received_offset ~offset @@ fun () -> unify_desc ~expected ~received
  | ( ( TT_var _ | TT_forall _ | TT_lambda _ | TT_apply _ | TT_exists _
      | TT_pair _ | TT_let _ | TT_annot _ ),
      ( TT_var _ | TT_forall _ | TT_lambda _ | TT_apply _ | TT_exists _
      | TT_pair _ | TT_let _ | TT_annot _ ) ) ->
      error_type_clash ~expected ~received

let unify_term ~expected ~received =
  (* TODO: does it make sense to always normalize? *)
  let* expected = normalize_term expected in
  let* received = normalize_term received in
  unify_term ~expected ~received

let unify_type ~expected ~received =
  let* expected = normalize_type expected in
  let* received = normalize_type received in
  unify_type ~expected ~received
