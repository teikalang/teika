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

(* TODO: should probably be on it's own context *)
let rec occurs_and_escape_check_term ~hole_offset ~to_offset ~to_ =
  let occurs_and_escape_check_term ~to_offset ~to_ =
    occurs_and_escape_check_term ~hole_offset ~to_offset ~to_
  in
  let occurs_and_escape_check_pat ~to_offset ~to_ =
    occurs_and_escape_check_pat ~hole_offset ~to_offset ~to_
  in
  let occurs_and_escape_check_annot ~to_offset ~to_ =
    occurs_and_escape_check_annot ~hole_offset ~to_offset ~to_
  in
  let occurs_and_escape_check_bind ~to_offset ~to_ =
    occurs_and_escape_check_bind ~hole_offset ~to_offset ~to_
  in
  match to_ with
  | TT_var { offset } -> (
      let offset = Offset.(to_offset + offset) in
      match Offset.(offset < hole_offset) with
      | true -> error_var_escape_scope ~var:offset
      | false -> return ())
  | TT_hole { id } -> (
      let* in_repr = repr_hole ~id in
      match in_repr with
      | H_open -> (
          let diff = Offset.(hole_offset - to_offset) in
          match Offset.(zero < diff) with
          | true -> lower_hole ~id ~diff
          | false -> return ())
      | H_link { term = to_ } -> occurs_and_escape_check_term ~to_offset ~to_)
  | TT_forall { param; return } ->
      let* () = occurs_and_escape_check_pat ~to_offset ~to_:param in
      occurs_and_escape_check_term ~to_offset ~to_:return
  | TT_lambda { param; return } ->
      let* () = occurs_and_escape_check_pat ~to_offset ~to_:param in
      occurs_and_escape_check_term ~to_offset ~to_:return
  | TT_apply { lambda; arg } ->
      let* () = occurs_and_escape_check_term ~to_offset ~to_:lambda in
      occurs_and_escape_check_term ~to_offset ~to_:arg
  | TT_exists { left; right } ->
      let* () = occurs_and_escape_check_annot ~to_offset ~to_:left in
      occurs_and_escape_check_annot ~to_offset ~to_:right
  | TT_pair { left; right } ->
      let* () = occurs_and_escape_check_bind ~to_offset ~to_:left in
      occurs_and_escape_check_bind ~to_offset ~to_:right
  | TT_let { bound; return } ->
      let* () = occurs_and_escape_check_bind ~to_offset ~to_:bound in
      occurs_and_escape_check_term ~to_offset ~to_:return
  | TT_annot { term; annot } ->
      (* TODO: check annot? *)
      let* () = occurs_and_escape_check_term ~to_offset ~to_:annot in
      occurs_and_escape_check_term ~to_offset ~to_:term
  | TT_offset { term; offset } ->
      let to_offset = Offset.(to_offset + offset) in
      occurs_and_escape_check_term ~to_offset ~to_:term
  | TT_loc { term; loc = _ } ->
      occurs_and_escape_check_term ~to_offset ~to_:term

and occurs_and_escape_check_pat ~hole_offset ~to_offset ~to_ =
  let occurs_and_escape_check_term ~to_ =
    occurs_and_escape_check_term ~hole_offset ~to_offset ~to_
  in
  let occurs_and_escape_check_pat ~to_ =
    occurs_and_escape_check_pat ~hole_offset ~to_offset ~to_
  in
  match to_ with
  | TP_var { var = _ } -> return ()
  | TP_pair { left; right } ->
      let* () = occurs_and_escape_check_pat ~to_:left in
      occurs_and_escape_check_pat ~to_:right
  | TP_annot { pat; annot } ->
      let* () = occurs_and_escape_check_term ~to_:annot in
      occurs_and_escape_check_pat ~to_:pat
  | TP_loc { pat; loc = _ } -> occurs_and_escape_check_pat ~to_:pat

and occurs_and_escape_check_annot ~hole_offset ~to_offset ~to_ =
  let (TAnnot { loc = _; pat; annot }) = to_ in
  let* () = occurs_and_escape_check_term ~hole_offset ~to_offset ~to_:annot in
  occurs_and_escape_check_pat ~hole_offset ~to_offset ~to_:pat

and occurs_and_escape_check_bind ~hole_offset ~to_offset ~to_ =
  let (TBind { loc = _; pat; value }) = to_ in
  let* () = occurs_and_escape_check_term ~hole_offset ~to_offset ~to_:value in
  occurs_and_escape_check_pat ~hole_offset ~to_offset ~to_:pat

let rec unify_term ~expected ~received =
  match (expected, received) with
  | TT_var { offset = expected }, TT_var { offset = received } -> (
      let* expected_offset = expected_offset () in
      let* received_offset = received_offset () in
      let expected = Offset.(expected + expected_offset) in
      let received = Offset.(received + received_offset) in
      match Offset.equal expected received with
      | true -> return ()
      | false -> error_var_clash ~expected ~received)
  | TT_hole { id = hole }, received -> (
      let* expected = repr_hole ~id:hole in
      match expected with
      | H_open ->
          let* hole_offset = expected_offset () in
          let* to_offset = received_offset () in
          unify_hole ~hole ~hole_offset ~to_:received ~to_offset
      | H_link { term = expected } -> unify_term ~expected ~received)
  | expected, TT_hole { id = hole } -> (
      let* received = repr_hole ~id:hole in
      match received with
      | H_open ->
          let* hole_offset = received_offset () in
          let* to_offset = expected_offset () in
          unify_hole ~hole ~hole_offset ~to_:expected ~to_offset
      | H_link { term = received } -> unify_term ~expected ~received)
  (* TODO: track whenever it is unified and locations, visualizing inference *)
  | ( TT_forall { param = expected_param; return = expected_return },
      TT_forall { param = received_param; return = received_return } ) ->
      (* TODO: contravariance *)
      let* () = unify_pat ~expected:expected_param ~received:received_param in
      unify_term ~expected:expected_return ~received:received_return
  | ( TT_lambda { param = expected_param; return = expected_return },
      TT_lambda { param = received_param; return = received_return } ) ->
      (* TODO: contravariance *)
      let* () = unify_pat ~expected:expected_param ~received:received_param in
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
  | TT_annot { term = expected; annot = _ }, received ->
      unify_term ~expected ~received
  | expected, TT_annot { term = received; annot = _ } ->
      unify_term ~expected ~received
  | TT_loc { term = expected; loc = _ }, received ->
      unify_term ~expected ~received
  | expected, TT_loc { term = received; loc = _ } ->
      unify_term ~expected ~received
      (* TODO: use those locations for something? *)
  | TT_offset { term = expected; offset }, received ->
      with_expected_offset ~offset @@ fun () -> unify_term ~expected ~received
  | expected, TT_offset { term = received; offset } ->
      with_received_offset ~offset @@ fun () -> unify_term ~expected ~received
  | ( ( TT_var _ | TT_forall _ | TT_lambda _ | TT_apply _ | TT_exists _
      | TT_pair _ | TT_let _ ),
      ( TT_var _ | TT_forall _ | TT_lambda _ | TT_apply _ | TT_exists _
      | TT_pair _ | TT_let _ ) ) ->
      error_type_clash ~expected ~received

and unify_pat ~expected ~received =
  match (expected, received) with
  | TP_var { var = _ }, TP_var { var = _ } -> return ()
  | ( TP_pair { left = expected_left; right = expected_right },
      TP_pair { left = received_left; right = received_right } ) ->
      let* () = unify_pat ~expected:expected_left ~received:received_left in
      unify_pat ~expected:expected_right ~received:received_right
  | ( TP_annot { pat = expected; annot = expected_annot },
      TP_annot { pat = received; annot = received_annot } ) ->
      let* () = unify_term ~expected:expected_annot ~received:received_annot in
      unify_pat ~expected ~received
  | TP_loc { pat = expected; loc = _ }, received ->
      unify_pat ~expected ~received
  | expected, TP_loc { pat = received; loc = _ } ->
      unify_pat ~expected ~received
  | (TP_var _ | TP_pair _ | TP_annot _), (TP_var _ | TP_pair _ | TP_annot _) ->
      error_pat_clash ~expected ~received

and unify_annot ~expected ~received =
  let (TAnnot { loc = _; pat = expected_pat; annot = expected_annot }) =
    expected
  in
  let (TAnnot { loc = _; pat = received_pat; annot = received_annot }) =
    received
  in
  let* () = unify_term ~expected:expected_annot ~received:received_annot in
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

and unify_hole ~hole ~hole_offset ~to_ ~to_offset =
  let* () = occurs_and_escape_check_term ~hole_offset ~to_offset ~to_ in
  let offset = Offset.(to_offset - hole_offset) in
  link_hole ~id:hole ~to_ ~offset

let unify_term ~expected ~received =
  (* TODO: does it make sense to always normalize? *)
  let* expected = normalize_term expected in
  let* received = normalize_term received in
  unify_term ~expected ~received
