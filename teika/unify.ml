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

let rec occurs_term : type a. _ -> in_:a term -> _ =
 fun hole ~in_ ->
  (* TODO: this is needed because of non injective functions
       the unification could succeed only if expand_head happened

     maybe should fail anyway ?
  *)
  let occurs_term ~in_ = occurs_term hole ~in_ in
  let occurs_param ~in_ = occurs_param hole ~in_ in
  match expand_head_term in_ with
  | TT_bound_var { index = _ } -> return ()
  | TT_free_var { level = _ } -> return ()
  (* TODO: use this substs? *)
  | TT_hole { hole = in_ } -> (
      match hole == in_ with
      | true -> error_var_occurs ~hole ~in_
      | false -> return ())
  | TT_forall { param; return } ->
      let* () = occurs_param ~in_:param in
      occurs_term ~in_:return
  | TT_lambda { param; return } ->
      let* () = occurs_param ~in_:param in
      occurs_term ~in_:return
  | TT_apply { lambda; arg } ->
      let* () = occurs_term ~in_:lambda in
      occurs_term ~in_:arg
  | TT_self { var = _; body } -> occurs_term ~in_:body
  | TT_fix { var = _; body } -> occurs_term ~in_:body
  | TT_unroll { term } -> occurs_term ~in_:term

and occurs_param hole ~in_ =
  (* TODO: occurs inside of TP_hole *)
  let (TP_typed { pat = _; annot }) = in_ in
  occurs_term hole ~in_:annot

let rec unify_term : type e r. expected:e term -> received:r term -> _ =
 fun ~expected ~received ->
  (* TODO: short circuit physical equality *)
  match (expand_head_term expected, expand_head_term received) with
  | TT_bound_var { index = expected }, TT_bound_var { index = received } -> (
      match Index.equal expected received with
      | true -> return ()
      | false -> error_bound_var_clash ~expected ~received)
  | TT_free_var { level = expected }, TT_free_var { level = received } -> (
      match Level.equal expected received with
      | true -> return ()
      | false -> error_free_var_clash ~expected ~received)
  | TT_hole { hole }, to_ | to_, TT_hole { hole } ->
      (* TODO: maybe unify against non expanded? *)
      unify_term_hole hole ~to_
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
  | ( TT_self { var = _; body = expected_body },
      TT_self { var = _; body = received_body } ) ->
      unify_term ~expected:expected_body ~received:received_body
  | ( TT_fix { var = _; body = expected_body },
      TT_fix { var = _; body = received_body } ) ->
      unify_term ~expected:expected_body ~received:received_body
  | TT_unroll { term = expected }, TT_unroll { term = received } ->
      unify_term ~expected ~received
  | ( (( TT_bound_var _ | TT_free_var _ | TT_forall _ | TT_lambda _ | TT_apply _
       | TT_self _ | TT_fix _ | TT_unroll _ ) as expected_norm),
      (( TT_bound_var _ | TT_free_var _ | TT_forall _ | TT_lambda _ | TT_apply _
       | TT_self _ | TT_fix _ | TT_unroll _ ) as received_norm) ) ->
      error_type_clash ~expected ~expected_norm ~received ~received_norm

and unify_term_hole hole ~to_ =
  match to_ with
  | TT_hole { hole = to_ } when hole == to_ -> return ()
  | _ ->
      (* TODO: prefer a direction when both are holes? *)
      let* () = occurs_term hole ~in_:to_ in
      hole.link <- Ex_term to_;
      return ()

and unify_param ~expected ~received =
  (* TODO: pat? *)
  let (TP_typed { pat = expected_pat; annot = expected_annot }) = expected in
  let (TP_typed { pat = received_pat; annot = received_annot }) = received in
  let* () = unify_pat ~expected:expected_pat ~received:received_pat in
  unify_term ~expected:expected_annot ~received:received_annot

and unify_pat : type e r. expected:e pat -> received:r pat -> _ =
 fun ~expected ~received ->
  match (expand_head_pat expected, expand_head_pat received) with
  | TP_hole { hole }, pat | pat, TP_hole { hole } ->
      unify_pat_hole hole ~to_:pat
  | TP_var _, TP_var _ -> return ()

and unify_pat_hole hole ~to_ =
  match to_ with
  | TP_hole { hole = to_ } when hole == to_ -> return ()
  | _ ->
      (* TODO: prefer a direction when both are holes? *)
      (* TODO: occurs_pat? *)
      hole.link <- to_;
      return ()
