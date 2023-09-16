open Ttree
open Context
open Var_context

(* TODO: check type whenever substitution is happening? *)
(* TODO: can do better than this *)
let rec repr_bound_var index subst =
  match subst with
  | TS_id -> None
  | TS_open { to_ } -> (
      match Index.equal index Index.zero with
      (* TODO: id subst *)
      | true -> Some (to_, TS_id)
      | false -> None)
  | TS_close { from = _ } -> None
  | TS_lift { subst } -> (
      match Index.previous index with
      | Some index -> (
          match repr_bound_var index subst with
          (* TODO: maybe shouldn't wrap TS_id? *)
          | Some (to_, subst) ->
              let subst = TS_lift { subst } in
              Some (to_, subst)
          | None -> None)
      | None -> None)
  | TS_cons { subst; next } -> (
      match repr_bound_var index subst with
      | Some (to_, subst) -> Some (to_, TS_cons { subst; next })
      | None -> repr_bound_var index next)

let rec repr_free_var level subst =
  match subst with
  | TS_id -> None
  | TS_open { to_ = _ } -> None
  | TS_close { from } -> (
      match Level.equal level from with
      | true -> Some (Index.zero, TS_id)
      | false -> None)
  | TS_lift { subst } -> (
      match repr_free_var level subst with
      | Some (index, subst) ->
          let subst = TS_lift { subst } in
          Some (Index.next index, subst)
      | None -> None)
  | TS_cons { subst; next } -> (
      match repr_free_var level subst with
      | Some (to_, subst) -> Some (to_, TS_cons { subst; next })
      | None -> repr_free_var level next)

let rec tt_apply_subst term subst =
  (* TODO: ignore meaningless substitutions *)
  let with_var subst = TS_lift { subst } in
  match tt_repr term with
  | TT_bound_var { index } -> (
      match repr_bound_var index subst with
      | Some (level, subst) ->
          let to_ = TT_free_var { level } in
          tt_apply_subst to_ subst
      | None -> term)
  | TT_free_var { level } -> (
      match repr_free_var level subst with
      | Some (index, subst) ->
          let to_ = TT_bound_var { index } in
          tt_apply_subst to_ subst
      | None -> term)
  | TT_hole { hole; subst = first } ->
      let subst = TS_cons { subst = first; next = subst } in
      TT_hole { hole; subst }
  | TT_forall { param; return } ->
      let param = tpat_apply_subst param subst in
      let return = tt_apply_subst return @@ with_var subst in
      TT_forall { param; return }
  | TT_lambda { param; return } ->
      let param = tpat_apply_subst param subst in
      let return = tt_apply_subst return @@ with_var subst in
      TT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let lambda = tt_apply_subst lambda subst in
      let arg = tt_apply_subst arg subst in
      TT_apply { lambda; arg }
  | TT_self { var; body } ->
      let body = tt_apply_subst body @@ with_var subst in
      TT_self { var; body }
  | TT_fix { var; body } ->
      let body = tt_apply_subst body @@ with_var subst in
      TT_fix { var; body }
  | TT_unroll { term } ->
      let term = tt_apply_subst term subst in
      TT_unroll { term }
  | TT_unfold { term } ->
      let term = tt_apply_subst term subst in
      TT_unfold { term }
  | TT_let { bound; value; return } ->
      let bound = tpat_apply_subst bound subst in
      let value = tt_apply_subst value subst in
      let return = tt_apply_subst return @@ with_var subst in
      TT_let { bound; value; return }
  | TT_annot { term; annot } ->
      let term = tt_apply_subst term subst in
      let annot = tt_apply_subst annot subst in
      TT_annot { term; annot }
  | TT_string _ -> term
  | TT_native _ -> term

and tpat_apply_subst pat subst =
  let (TPat { pat; type_ }) = pat in
  let type_ = tt_apply_subst type_ subst in
  TPat { pat; type_ }

and tt_repr term =
  (* TODO: path compression *)
  match term with
  | TT_hole { hole; subst } -> (
      match hole.link with
      | Some term ->
          let term = tt_repr term in
          tt_apply_subst term subst
      | None -> term)
  (* TODO: ignore meaningless substitutions *)
  (* TODO: expand cases here  *)
  | _ -> term

let tt_match term = tt_repr term

let rec tt_open term ~from ~to_ =
  (* TODO: ignore meaningless substitutions *)
  let tpat_open pat = tpat_open pat ~from ~to_ in
  let tt_open_flat term = tt_open term ~from ~to_ in
  let tt_open_under term =
    let from = Index.next from in
    tt_open term ~from ~to_
  in
  match tt_repr term with
  | TT_bound_var { index } -> (
      match Index.equal from index with true -> to_ | false -> term)
  | TT_free_var { level = _ } -> term
  | TT_hole { hole; subst } -> (
      (* only capture inversible substitutions *)
      match to_ with
      | TT_free_var { level = to_ } ->
          let subst = TS_cons { subst; next = TS_open { to_ } } in
          TT_hole { hole; subst }
      | _ -> term)
  | TT_forall { param; return } ->
      let param = tpat_open param in
      let return = tt_open_under return in
      TT_forall { param; return }
  | TT_lambda { param; return } ->
      let param = tpat_open param in
      let return = tt_open_under return in
      TT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let lambda = tt_open_flat lambda in
      let arg = tt_open_flat arg in
      TT_apply { lambda; arg }
  | TT_self { var; body } ->
      let body = tt_open_under body in
      TT_self { var; body }
  | TT_fix { var; body } ->
      let body = tt_open_under body in
      TT_fix { var; body }
  | TT_unroll { term } ->
      let term = tt_open_flat term in
      TT_unroll { term }
  | TT_unfold { term } ->
      let term = tt_open_flat term in
      TT_unfold { term }
  | TT_let { bound; value; return } ->
      let bound = tpat_open bound in
      let value = tt_open_flat value in
      let return = tt_open_under return in
      TT_let { bound; value; return }
  | TT_annot { term; annot } ->
      let term = tt_open_flat term in
      let annot = tt_open_flat annot in
      TT_annot { term; annot }
  | TT_string _ -> term
  | TT_native _ -> term

and tpat_open pat ~from ~to_ =
  let (TPat { pat; type_ }) = pat in
  let type_ = tt_open type_ ~from ~to_ in
  TPat { pat; type_ }

let tt_open term ~to_ =
  let from = Index.zero in
  tt_open term ~from ~to_

let tt_close term ~from = tt_apply_subst term @@ TS_close { from }

let rec tt_expand_head term =
  match tt_repr term with
  | TT_bound_var _ -> term
  | TT_free_var _ -> term
  | TT_hole _ -> term
  | TT_forall _ -> term
  | TT_lambda _ -> term
  | TT_apply { lambda; arg } -> (
      (* TODO: use expanded lambda? *)
      match tt_match (tt_expand_head lambda) with
      | TT_lambda { param = _; return } ->
          tt_expand_head @@ tt_open return ~to_:arg
      | TT_native { native } -> expand_head_native native ~arg
      | _lambda -> term)
  | TT_self _ -> term
  | TT_fix _ -> term
  | TT_unroll _ -> term
  | TT_unfold { term } -> tt_expand_head term
  | TT_let { bound = _; value; return } ->
      tt_expand_head @@ tt_open return ~to_:value
  | TT_annot { term; annot = _ } -> tt_expand_head term
  | TT_string _ -> term
  | TT_native _ -> term

and expand_head_native native ~arg =
  match native with TN_debug -> tt_expand_head arg

let rec tt_escape_check term =
  let* current = level () in
  (* TODO: check without expand_head? *)
  (* TODO: this should not be here *)
  match tt_match @@ tt_expand_head term with
  | TT_unfold _ -> error_unfold_found term
  | TT_annot _ -> error_annot_found term
  (* TODO: also check bound var *)
  (* TODO: very very important to check for bound vars, unification
        may unify variables outside of their binders *)
  | TT_bound_var _ -> pure ()
  | TT_free_var { level } -> (
      match Level.(current < level) with
      | true -> error_var_escape ~var:level
      | false -> pure ())
  | TT_hole _hole -> pure ()
  | TT_forall { param; return } | TT_lambda { param; return } ->
      let* () = tpat_escape_check param in
      with_free_var @@ fun () -> tt_escape_check return
  | TT_apply { lambda; arg } ->
      let* () = tt_escape_check lambda in
      tt_escape_check arg
  | TT_self { var = _; body } | TT_fix { var = _; body } ->
      with_free_var @@ fun () -> tt_escape_check body
  | TT_unroll { term } -> tt_escape_check term
  | TT_let { bound; value; return } ->
      let* () = tpat_escape_check bound in
      let* () = tt_escape_check value in
      with_free_var @@ fun () -> tt_escape_check return
  | TT_string { literal = _ } -> pure ()
  | TT_native { native = _ } -> pure ()

and tpat_escape_check term =
  (* TODO: check pat? *)
  let (TPat { pat = _; type_ }) = term in
  tt_escape_check type_

(* TODO: better place for this *)
let rec tt_unfold_fix term =
  (* TODO: not ideal to expand head *)
  match tt_repr term with
  | TT_bound_var _ -> term
  | TT_free_var _ -> term
  | TT_hole _ -> term
  | TT_forall _ -> term
  | TT_lambda _ -> term
  | TT_apply { lambda; arg } ->
      let lambda = tt_unfold_fix lambda in
      let arg = tt_unfold_fix arg in
      TT_apply { lambda; arg }
  | TT_self _ -> term
  | TT_fix _ -> term
  | TT_unroll { term = fix } -> (
      match tt_match @@ tt_expand_head fix with
      | TT_fix { var = _; body } -> tt_expand_head @@ tt_open body ~to_:fix
      | _ -> term)
  | TT_unfold { term } ->
      let term = tt_unfold_fix term in
      TT_unfold { term }
      (* TODO: unfold under let?  *)
  | TT_let _ -> term
  | TT_annot { term; annot } ->
      let term = tt_unfold_fix term in
      let annot = tt_unfold_fix annot in
      TT_annot { term; annot }
  | TT_string _ -> term
  | TT_native _ -> term

let rec ts_inverse subst =
  match subst with
  | TS_id -> TS_id
  | TS_open { to_ = from } -> TS_close { from }
  | TS_close { from = to_ } -> TS_open { to_ }
  | TS_lift { subst } ->
      let subst = ts_inverse subst in
      TS_lift { subst }
  | TS_cons { subst = next; next = subst } ->
      (* also reverses the order *)
      let subst = ts_inverse subst in
      let next = ts_inverse next in
      TS_cons { subst; next }
