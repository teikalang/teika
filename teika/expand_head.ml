open Ttree

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

let rec tt_expand_subst ~subst term =
  (* TODO: check if term has same type as subst *)
  let tt_subst term subst = TT_subst { term; subst } in
  let with_var subst = TS_lift { subst } in
  match term with
  | TT_subst { term; subst = first } ->
      let subst = TS_cons { subst = first; next = subst } in
      tt_expand_subst ~subst term
  | TT_bound_var { index } -> (
      match repr_bound_var index subst with
      | Some (to_, subst) -> tt_expand_subst ~subst to_
      | None -> term)
  | TT_free_var { level } -> (
      match repr_free_var level subst with
      | Some (index, subst) ->
          let to_ = TT_bound_var { index } in
          tt_expand_subst ~subst to_
      | None -> term)
  (* TODO: subst and hole *)
  | TT_hole { hole = _ } -> term
  | TT_forall { param; return } ->
      let param = tpat_expand_subst ~subst param in
      let return = tt_subst return @@ with_var subst in
      TT_forall { param; return }
  | TT_lambda { param; return } ->
      let param = tpat_expand_subst ~subst param in
      let return = tt_subst return @@ with_var subst in
      TT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let lambda = tt_subst lambda subst in
      let arg = tt_subst arg subst in
      TT_apply { lambda; arg }
  | TT_self { var; body } ->
      let body = tt_subst body @@ with_var subst in
      TT_self { var; body }
  | TT_fix { var; body } ->
      let body = tt_subst body @@ with_var subst in
      TT_fix { var; body }
  | TT_unroll { term } ->
      let term = tt_subst term subst in
      TT_unroll { term }
  | TT_unfold { term } ->
      let term = tt_subst term subst in
      TT_unfold { term }
  | TT_let { bound; value; return } ->
      let bound = tpat_expand_subst ~subst bound in
      let value = tt_subst value subst in
      let return = tt_subst return @@ with_var subst in
      TT_let { bound; value; return }
  | TT_annot { term; annot } ->
      let term = tt_subst term subst in
      let annot = tt_subst annot subst in
      TT_annot { term; annot }
  | TT_string _ -> term
  | TT_native _ -> term

and tpat_expand_subst : subst:subst -> _ -> _ =
 fun ~subst pat ->
  let (TPat { pat; type_ }) = pat in
  let type_ = tt_expand_subst ~subst type_ in
  TPat { pat; type_ }

let rec tt_expand_head term =
  match term with
  | TT_subst { term; subst } -> tt_expand_head @@ tt_expand_subst ~subst term
  | TT_bound_var _ -> term
  | TT_free_var _ -> term
  | TT_hole { hole } -> (
      (* TODO: path compression *)
      (* TODO: move this to machinery *)
      match hole.link with
      | None -> term
      | Some link ->
          (* TODO: path compression *)
          tt_expand_head link)
  | TT_forall _ -> term
  | TT_lambda _ -> term
  | TT_apply { lambda; arg } -> (
      (* TODO: use expanded lambda? *)
      match tt_match (tt_expand_head lambda) with
      | TT_lambda { param = _; return } ->
          (* TODO: param is not used here,
             but it would be cool to check when in debug *)
          (* TODO: this could be done in O(1) with context extending *)
          let subst = TS_open { to_ = arg } in
          tt_expand_head @@ tt_expand_subst ~subst return
      | TT_native { native } -> expand_head_native native ~arg
      | _lambda -> term)
  | TT_self _ -> term
  | TT_fix _ -> term
  | TT_unroll _ -> term
  | TT_unfold { term } -> tt_expand_head term
  | TT_let { bound = _; value; return } ->
      (* TODO: param is not used here,
         but it would be cool to check when in debug *)
      let subst = TS_open { to_ = value } in
      tt_expand_head @@ tt_expand_subst ~subst return
  | TT_annot { term; annot = _ } -> tt_expand_head term
  | TT_string _ -> term
  | TT_native _ -> term

and expand_head_native native ~arg =
  match native with TN_debug -> tt_expand_head arg
