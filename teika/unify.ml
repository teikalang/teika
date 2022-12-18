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

let rec unify_term : type e r. expected:e term -> received:r term -> _ =
 fun ~expected ~received ->
  match (expand_head expected, expand_head received) with
  | TT_var { offset = expected }, TT_var { offset = received } -> (
      match Offset.equal expected received with
      | true -> return ()
      | false -> error_var_clash ~expected ~received)
  (* TODO: track whenever it is unified and locations, visualizing inference *)
  | ( TT_forall { param = expected_param; return = expected_return },
      TT_forall { param = received_param; return = received_return } ) ->
      (* TODO: contravariance *)
      let* () = unify_param ~expected:expected_param ~received:received_param in
      unify_term ~expected:expected_return ~received:received_return
  | ( TT_lambda { param = expected_param; return = expected_return },
      TT_lambda { param = received_param; return = received_return } ) ->
      (* TODO: contravariance *)
      let* () = unify_param ~expected:expected_param ~received:received_param in
      unify_term ~expected:expected_return ~received:received_return
  | ( TT_apply { lambda = expected_lambda; arg = expected_arg },
      TT_apply { lambda = received_lambda; arg = received_arg } ) ->
      let* () =
        unify_term ~expected:expected_lambda ~received:received_lambda
      in
      unify_term ~expected:expected_arg ~received:received_arg
  | ( ((TT_var _ | TT_forall _ | TT_lambda _ | TT_apply _) as expected_norm),
      ((TT_var _ | TT_forall _ | TT_lambda _ | TT_apply _) as received_norm) )
    ->
      error_type_clash ~expected ~expected_norm ~received ~received_norm

and unify_param ~expected ~received =
  let (TP_annot { pat = expected_pat; annot = expected_annot }) = expected in
  let (TP_annot { pat = received_pat; annot = received_annot }) = received in
  let* () = unify_term ~expected:expected_annot ~received:received_annot in
  unify_pat ~expected:expected_pat ~received:received_pat

and unify_pat : type e r. expected:e pat -> received:r pat -> _ =
 fun ~expected ~received ->
  match (expected, received) with
  | TP_var { var = _ }, TP_var { var = _ } -> return ()
  | ( TP_annot { pat = expected; annot = expected_annot },
      TP_annot { pat = received; annot = received_annot } ) ->
      let* () = unify_term ~expected:expected_annot ~received:received_annot in
      unify_pat ~expected ~received
  | TP_loc { pat = expected; loc = _ }, received ->
      unify_pat ~expected ~received
  | expected, TP_loc { pat = received; loc = _ } ->
      unify_pat ~expected ~received
  | (TP_var _ | TP_annot _), (TP_var _ | TP_annot _) ->
      error_pat_clash ~expected ~received
