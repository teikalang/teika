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
  match expand_head_term in_ with
  | TT_bound_var { index = _ } -> return ()
  | TT_free_var { level = _ } -> return ()
  (* TODO: use this substs? *)
  | TT_hole { hole = in_; substs = _ } -> (
      match hole == in_ with
      | true -> error_var_occurs ~hole ~in_
      | false -> return ())
  | TT_forall { param; return } ->
      let* () = occurs_term ~in_:param in
      occurs_term ~in_:return
  | TT_lambda { param; return } ->
      let* () = occurs_term ~in_:param in
      occurs_term ~in_:return
  | TT_apply { lambda; arg } ->
      let* () = occurs_term ~in_:lambda in
      occurs_term ~in_:arg

(* TODO: better place for this *)
let inverse_subst subst =
  match subst with
  (* TODO: subst of this could definitely be inversed *)
  | TS_subst_bound _ -> None
  | TS_subst_free _ -> None
  | TS_open_bound { from; to_ } ->
      Some (TS_close_free { from = to_; to_ = from })
  | TS_close_free { from; to_ } ->
      Some (TS_open_bound { from = to_; to_ = from })

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
  | TT_hole { hole; substs }, to_ | to_, TT_hole { hole; substs } ->
      (* TODO: maybe unify against non expanded? *)
      unify_hole hole ~substs ~to_
  (* TODO: track whenever it is unified and locations, visualizing inference *)
  | ( TT_forall { param = expected_param; return = expected_return },
      TT_forall { param = received_param; return = received_return } ) ->
      (* TODO: contravariance *)
      let* () = unify_term ~expected:expected_param ~received:received_param in
      unify_term ~expected:expected_return ~received:received_return
  | ( TT_lambda { param = expected_param; return = expected_return },
      TT_lambda { param = received_param; return = received_return } ) ->
      (* TODO: contravariance *)
      let* () = unify_term ~expected:expected_param ~received:received_param in
      unify_term ~expected:expected_return ~received:received_return
  | ( TT_apply { lambda = expected_lambda; arg = expected_arg },
      TT_apply { lambda = received_lambda; arg = received_arg } ) ->
      let* () =
        unify_term ~expected:expected_lambda ~received:received_lambda
      in
      unify_term ~expected:expected_arg ~received:received_arg
  | ( ((TT_bound_var _ | TT_free_var _ | TT_forall _ | TT_lambda _ | TT_apply _)
      as expected_norm),
      ((TT_bound_var _ | TT_free_var _ | TT_forall _ | TT_lambda _ | TT_apply _)
      as received_norm) ) ->
      error_type_clash ~expected ~expected_norm ~received ~received_norm

and unify_hole hole ~substs ~to_ =
  let to_ =
    List.fold_right
      (fun subst (Ex_term to_) ->
        match inverse_subst subst with
        | Some subst -> Ex_term (TT_subst { subst; term = to_ })
        | None -> Ex_term to_)
      substs (Ex_term to_)
  in
  (* TODO: prefer a direction when both are holes? *)
  let* () =
    let (Ex_term to_) = to_ in
    occurs_term hole ~in_:to_
  in
  hole.link <- to_;
  return ()
