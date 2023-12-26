open Ttree

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
  match term with
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

let rec tt_open term ~from ~to_ =
  (* TODO: ignore meaningless substitutions *)
  let tpat_open pat = tpat_open pat ~from ~to_ in
  let tt_open_flat term = tt_open term ~from ~to_ in
  let tt_open_under term =
    let from = Index.next from in
    tt_open term ~from ~to_
  in
  match term with
  | TT_bound_var { index } -> (
      match Index.equal from index with true -> to_ | false -> term)
  | TT_free_var { level = _ } -> term
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

let rec tt_expand_head ~aliases term =
  (* TODO: aliases here is hackish *)
  let tt_expand_head term = tt_expand_head ~aliases term in
  match term with
  | TT_bound_var _ -> term
  | TT_free_var { level } -> (
      (* TODO: possibly infinite loop, consume aliases *)
      match Level.Map.find_opt level aliases with
      | Some alias -> tt_expand_head alias
      | None -> term)
  | TT_forall _ -> term
  | TT_lambda _ -> term
  | TT_apply { lambda; arg } -> (
      (* TODO: use expanded lambda? *)
      match tt_expand_head lambda with
      | TT_lambda { param = _; return } ->
          tt_expand_head @@ tt_open return ~to_:arg
      | TT_native { native } -> expand_head_native ~aliases native ~arg
      | _lambda -> term)
  | TT_let { bound = _; value; return } ->
      tt_expand_head @@ tt_open return ~to_:value
  | TT_annot { term; annot = _ } -> tt_expand_head term
  | TT_string _ -> term
  | TT_native _ -> term

and expand_head_native ~aliases native ~arg =
  match native with TN_debug -> tt_expand_head ~aliases arg

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
