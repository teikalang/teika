module Name = Syntax.Ctree.Name
module Syntax_tree = Syntax.Lparser.Syntax_tree

module Level : sig
  type level
  type t = level

  val ( <= ) : level -> level -> bool
  val zero : level
  val succ : level -> level
  val min : level -> level -> level
  val max : level -> level -> level
end = struct
  type level = int
  type t = level

  let ( <= ) (l : level) (r : level) : bool = l <= r
  let zero = Int.zero
  let succ = Int.succ
  let min = Int.min
  let max = Int.max
end

module Named_tree = struct
  type term = Term of { head : term_head; loc : Location.t }

  and term_head =
    | T_var of { var : var }
    | T_let of { bound : pat; arg : term; body : term }
    | T_apply of { funct : term; arg : term }
    | T_self of { bound : pat }
    | T_fix of { bound : pat; body : term }

  and pat = Pat of { head : pat_head; loc : Location.t }

  and pat_head =
    | P_annot of { pat : pat; annot : term }
    | P_var of { var : var }
    | P_apply of { funct : pat; arg : pat }

  and var

  let t_wrap loc head = Term { head; loc }
  let p_wrap loc head = Pat { head; loc }
end

module Check_tree = struct
  type term =
    | Term of { mutable link : term; head : term_head; mutable at : Level.t }

  and term_head =
    | T_flex
    | T_type
    | T_var of { var : var }
    | T_pi of { param : term; body : term }
    | T_lambda of { bound : var; body : term }
    | T_apply of { funct : term; arg : term }
    | T_self of { body : term }
    | T_fix of { body : term }
    | T_frozen of { body : term }
    | T_unfold of { fix : term }

  and var =
    | Var of {
        mutable to_ : term option;
        mutable type_ : term;
        mutable at : Level.t;
      }

  (* physical identity *)
  let v_same (left : var) (right : var) : bool = left == right
  let v_fresh ~at ~type_ () = Var { to_ = None; type_; at }

  (* physical identity *)
  let t_raw_same (left : term) (right : term) : bool = left == right

  let t_raw_link term ~to_ =
    let (Term inner) = term in
    inner.link <- to_

  let rec t_repr term =
    let (Term { link; head = _; at = _ }) = term in
    match t_raw_same link term with true -> term | false -> t_repr link

  let t_repr term =
    let (Term { link; head = _; at = _ }) = term in
    match t_raw_same link term with
    | true -> term
    | false -> (
        let (Term { link = link_link; head = _; at = _ }) = link in
        match t_raw_same link_link link with
        | true ->
            (* linked but already compressed *)
            link
        | false ->
            (* path compression *)
            let final = t_repr link_link in
            let () = t_raw_link term ~to_:final in
            final)

  let t_at term =
    let (Term { link = _; head = _; at }) = t_repr term in
    at

  let t_head term =
    let (Term { link = _; head; at = _ }) = t_repr term in
    head

  let t_new ~at head =
    let rec self = Term { link = self; head; at } in
    self

  let t_same left right = t_raw_same (t_repr left) (t_repr right)

  let t_lower term ~to_ =
    let (Term inner) = t_repr term in
    (* TODO: is this check right? *)
    assert (Level.(to_ <= inner.at));
    inner.at <- to_

  let t_link term ~to_ = t_raw_link (t_repr term) ~to_

  let v_at var =
    let (Var { type_ = _; at }) = var in
    at

  let v_type var =
    let (Var { type_; at = _ }) = var in
    type_

  let v_with_def var k =
    let (Var var) = var in
    let snapshot = var.at in
    let () = var.at <- Level.zero in
    let result = k () in
    let () = var.at <- snapshot in
    result

  let t_new_flex ~at () = t_new ~at T_flex

  let t_var var =
    let at = v_at var in
    t_new ~at (T_var { var })

  let t_type = t_new ~at:Level.zero T_type

  let t_pi param body =
    let at = Level.max (t_at param) (t_at body) in
    t_new ~at (T_pi { param; body })

  let t_lambda bound body =
    let at = t_at body in
    t_new ~at (T_lambda { bound; body })

  let t_apply funct arg =
    let at = Level.max (t_at funct) (t_at arg) in
    t_new ~at (T_apply { funct; arg })

  let t_self body =
    let at = t_at body in
    t_new ~at (T_self { body })

  let t_fix body =
    let at = t_at body in
    t_new ~at (T_fix { body })

  let t_frozen body =
    let at = t_at body in
    t_new ~at (T_frozen { body })

  let t_unfold fix =
    let at = t_at fix in
    t_new ~at (T_unfold { fix })

  let t_arrow param body =
    let at = t_at body in
    let bound = v_fresh ~at ~type_:param () in
    t_pi param (t_new ~at (T_lambda { bound; body }))

  (* *)
  let t_let bound arg body = t_apply (t_lambda bound body) arg

  (* *)
  let v_fresh_predef () : var =
    let type_ = t_new_flex ~at:Level.zero () in
    v_fresh ~at:Level.zero ~type_ ()

  let vf_unit = v_fresh_predef ()
  let vi_unit = v_fresh_predef ()
  let ve_drop = v_fresh_predef ()
  let tf_unit = t_var vf_unit
  let ti_unit = t_var vi_unit

  let te_drop arg body =
    (* TODO: apply type parameters? *)
    t_apply (t_apply (t_var ve_drop) arg) body

  (* *)
  let vf_sigma = v_fresh_predef ()
  let vi_pair = v_fresh_predef ()
  let ve_fst = v_fresh_predef ()
  let ve_snd = v_fresh_predef ()
  let tf_sigma fst snd = t_apply (t_apply (t_var vf_sigma) fst) snd

  let ti_pair fst snd =
    (* TODO: apply type parameters? *)
    t_apply (t_apply (t_var vi_pair) fst) snd

  let te_fst pair = t_apply (t_var ve_fst) pair
  let te_snd pair = t_apply (t_var ve_snd) pair
end

module Typed_tree = struct
  type term = Term of { head : term_head; type_ : term; loc : Location.t }

  and term_head =
    | T_flex
    | T_type
    | T_var of { var : var }
    | T_let of { bound : var; arg : term; body : term }
    | T_pi of { param : term; body : term }
    | T_lambda of { bound : var; body : term }
    | T_apply of { funct : term; arg : term }
    | T_self of { body : term }
    | T_fix of { body : term }

  and var

  let t_wrap loc ~type_ head = Term { head; type_; loc }
  let t_same : term -> term -> bool = _
  let t_head : term -> term_head = _
  let t_link term ~to_ : unit = _
  let t_pi : term -> term -> term = _
  let t_lambda : var -> term -> term = _
  let t_apply : term -> term -> term = _
  let t_self : term -> term = _
  let t_fix : term -> term = _
  let t_let : var -> term -> term -> term = _
  let v_same : var -> var -> bool = _
  let bump_mark : unit -> unit = _
  let mark : term -> unit = _
  let is_marked : term -> bool = _

  let rec expand term =
    match t_head term with
    | T_flex -> ()
    | T_type -> ()
    | T_var _ -> ()
    | T_let { bound; arg; body } ->
        let to_ = subst bound arg body in
        t_link term ~to_
    | T_pi _ -> ()
    | T_lambda _ -> ()
    | T_apply { funct; arg } -> (
        expand funct;
        match t_head funct with
        | T_lambda { bound; body } ->
            let to_ = t_let bound arg body in
            t_link term ~to_;
            expand to_
        | T_fix { body } -> (
            match is_marked funct with
            | true -> ()
            | false ->
                mark funct;
                let to_ = t_apply (t_apply body funct) arg in
                t_link term ~to_;
                expand to_)
        | T_flex | T_type | T_var _ | T_let _ | T_pi _ | T_apply _ | T_self _ ->
            ())
    | T_self _ -> ()
    | T_fix _ -> ()

  and subst var to_ body =
    match t_head body with
    | T_flex -> body
    | T_type -> body
    | T_var { var = found_var } -> (
        match v_same found_var var with true -> to_ | false -> body)
    | T_let { bound; arg; body } ->
        let final_body = subst bound arg body in
        t_link body ~to_:final_body;
        subst var to_ final_body
    | T_pi { param; body } ->
        let param = t_let var to_ param in
        let body = t_let var to_ body in
        t_pi param body
    | T_lambda { bound; body } -> (
        match v_same bound var with
        | true -> body
        | false ->
            let body = t_let var to_ body in
            t_lambda bound body)
    | T_apply { funct; arg } ->
        let funct = t_let var to_ funct in
        let arg = t_let var to_ arg in
        t_apply funct arg
    | T_self { body } ->
        let body = t_let var to_ body in
        t_self body
    | T_fix { body } ->
        let body = t_let var to_ body in
        t_fix body

  let rec unify received expected =
    match t_same received expected with
    | true -> ()
    | false -> (
        mark received;
        mark expected;
        match is_marked received || is_marked expected with
        | true -> failwith "unify.bug: found cyclical term"
        | false -> unify_head received expected)

  and unify_head received expected = _
end

module Type_check = struct
  open Named_tree
  open Typed_tree

  let unify : term -> term -> unit = _
  let v_type : var -> term = _
  let t_type : term = _
  let t_var loc ~type_ var = t_wrap loc ~type_ (T_var { var })

  let t_let loc ~type_ bound arg body =
    t_wrap loc ~type_ (T_let { bound; arg; body })

  let t_apply loc ~type_ funct arg = t_wrap loc ~type_ (T_apply { funct; arg })

  type context

  let with_var : context -> Named_tree.var -> term -> Typed_tree.var * context =
    _

  let lookup : context -> Named_tree.var -> term * Typed_tree.var = _
  let t_flex ctx () = _

  let rec check_term ctx term expected self =
    let (Term { head; loc } : Named_tree.term) = term in
    match head with
    | T_var { var } ->
        let received, var = lookup ctx var in
        unify (t_var loc ~type_:received var) self;
        unify received expected
    | T_let { bound; arg; body } ->
        let bound, arg, ctx = elab_let ctx bound arg in
        let body_self = t_flex ctx () in
        unify (t_let loc ~type_:expected bound arg body_self) self;
        check_term ctx body expected body_self
    | T_apply { funct; arg } ->
        (* TODO: check on funct *)
        let funct_type = t_flex ctx () in
        let arg_type, body_type = match_pi funct_type in
        let funct_self = t_flex ctx () in
        let arg_self = t_flex ctx () in
        check_term ctx funct funct_type funct_self;
        let type_ = t_apply loc ~type_:t_type body_type arg_self in
        unify (t_apply loc ~type_ funct_self arg_self) self;
        check_term ctx arg arg_type arg_self;
        (* TODO: unify before typing arg? with self? *)
        unify type_ expected
    | T_self { bound } ->
        let bound = check_pat bound self in

        unify (t_self bound) self
    | T_fix { bound; body } ->
        let bound, body, ctx = elab_let ctx bound arg in
        unify (t_fix (t_lambda bound body)) self

  and with_pat ctx pat =
    let (Pat { head; loc }) = pat in
    match head with
    | P_annot { pat; annot = _ } -> with_pat ctx pat
    | P_var { var } ->
        let ctx = _ in
        (ctx, ctx)
    | P_apply { funct; arg } ->
        let inner_funct_ctx, outer_funct_ctx = with_pat ctx funct in
        let inner_arg_ctx, _outer_arg_ctx = with_pat inner_funct_ctx arg in
        (inner_arg_ctx, outer_funct_ctx)

  and infer_pat ctx pat =
    let (Pat { head; loc }) = bound in
    match head with
    | P_annot { pat; annot } ->
        let type_ = infer_pat ctx pat in
        let inner_ctx, _outer_ctx = with_pat ctx pat in
        _
    | P_var _ -> _
    | P_apply { funct; arg } -> _

  and check_fix_pat ctx bound expected =
    let (Pat { head; loc }) = bound in
    match head with
    | P_annot { pat; annot } ->
        let annot_self = _ in
        let pat, ctx = check_fix_pat ctx pat annot_self in
        check_term ctx annot t_type annot_self;
        _
    | P_var { var } ->
        let ctx, var = with_var ctx var expected in
        _
    | P_apply { funct; arg } ->
        let funct_type = _ in
        let param_type, body_type = match_pi funct_type in
        let funct, funct_ctx = check_fix_pat ctx funct funct_type in
        let arg, arg_ctx = check_def_pat ctx arg param_type in
        _

  and infer_pat_let ctx pat = _

  and check_self_pat ctx pat expected =
    let (Pat { head; loc }) = pat in
    match head with
    | P_annot { pat; annot } ->
        let annot_self = _ in
        check_self_pat ctx pat annot_self;
        _
    | P_var _ -> _
    | P_apply { funct; arg } -> _

  and check_pat ctx pat expected =
    let (Pat { head; loc }) = pat in
    match head with
    | P_annot { pat; annot } ->
        let annot_self = _ in
        let pat, inner_ctx, outer_ctx = check_pat ctx pat annot_self in
        (p_annot, inner_ctx, outer_ctx)
    | P_var { var } -> _
    | P_apply { funct; arg } ->
        let funct_type = _ in
        let param_type, body_type = match_pi funct_type in
        let funct, inner_funct_ctx, outer_funct_ctx =
          check_pat ctx funct funct_type
        in
        let arg, inner_arg_ctx, _outer_arg_ctx =
          check_pat inner_funct_ctx arg param_type
        in
        (p_apply, inner_arg_ctx, outer_funct_ctx)

  and infer_pat pat =
    let (Pat { head; loc }) = pat in
    match head with
    | P_annot { pat; annot } -> _
    | P_var { var } -> _
    | P_apply { funct; arg } -> _
end

module Elaborate = struct
  open Syntax_tree
  open Check_tree

  let rec check_term ctx term expected self =
    let (Term { head; loc } : Syntax_tree.term) = term in
    match head with
    | T_var { var } ->
        let received = _ in
        coerce received expected
    | T_block { block; return } ->
        infer_block block;
        check_term return expected
    | T_apply { funct; arg } ->
        (* TODO: check on funct *)
        let funct_type = _ in
        let funct_self = _ in
        let param_type, body_type = match_pi funct_type in
        check_term ctx funct funct_type funct_self;
        let arg_self = _ in
        check_term ctx arg param_type arg_self;

        coerce (apply body_type arg) expected
    | T_self { bound } ->
        let () = match_type expected in
        check_def_pat bound term
    | T_fix { bound; body } ->
        check_def_pat bound expected;
        check_term body expected
    | T_sigma { fst; snd } ->
        let () = match_type expected in
        infer_def_pat fst;
        check_term snd t_type
    | T_pair { fst; snd } ->
        let fst_type, snd_type = match_sigma expected in
        elab_term fst fst_type;
        let snd_type = apply snd_type fst in
        check_term snd snd_type
    | T_unit | T_box _ -> _

  and infer_term ctx term = _

  and infer_pat ctx pat =
    let (Pat { head; loc }) = pat in
    match head with
    | P_annot { pat; annot } ->
        let received, inner_ctx, outer_ctx = infer_pat ctx pat in
        let expected = elab_term inner_ctx annot t_type in
        unify inner_ctx received expected;
        _
    | P_var { var } -> _
    | P_apply { funct; arg } ->
        let funct, funct_inner_ctx, funct_outer_ctx = infer_pat ctx funct in
        let arg, arg_inner_ctx, _arg_outer_ctx =
          infer_pat funct_inner_ctx arg
        in
        (_, arg_inner_ctx, funct_outer_ctx)
    | P_unit -> _
    | P_pair { fst; snd } ->
        let fst, fst_inner_ctx, _fst_outer_ctx = infer_pat ctx fst in
        let snd, _snd_inner_ctx, snd_outer_ctx = infer_pat fst_inner_ctx snd in
        (_, snd_outer_ctx, snd_outer_ctx)
    | P_box _ -> _

  and infer_pat ctx pat =
    let (Pat { head; loc }) = pat in
    match head with
    | P_annot { pat; annot } ->
        let received, inner_ctx = _ in
        let annot = elab_term inner_ctx annot in
        unify received annot
    | P_var { var } -> _
    | P_apply { funct; arg } ->
        let funct, funct_ctx = infer_pat ctx funct in
        let arg, arg_ctx = infer_pat ctx arg in
        _
    | P_unit -> _
    | P_pair _ -> _
    | P_box _ -> _

  and check_let_pat pat expected =
    let (Let_pat { head; loc = _ }) = pat in
    match head with
    | LP_annot { pat = inner; annot } ->
        let received = t_self pat in
        check_let_pat inner received;
        check_term annot t_type;
        unify received expected
    | LP_var { var } -> _
    | LP_unit -> unify tf_unit expected
    | LP_pair { fst; snd } ->
        let fst_type, snd_type = match_sigma expected in
        check_let_pat fst fst_type;
        let snd_type = apply snd_type fst in
        check_let_pat snd snd_type
    | LP_apply { funct; arg } ->
        let funct_type = t_self pat in
        check_def_pat funct funct_type;
        let param_type, body_type = match_pi funct_type in
        check_let_pat arg param_type;
        unify (apply body_type _) expected

  and infer_block block =
    let (Block { head; loc = _ }) = block in
    match head with
    | B_hole -> ()
    | B_let { bound; arg; body } ->
        let arg_type = _ in
        check_let_pat bound arg_type;
        elab_term arg arg_type;
        infer_block body
    | B_def { bound; body } ->
        let bound_type = _ in
        check_def_pat bound bound_type;
        infer_block body
    | B_fix { bound; arg; body } ->
        let arg_type = _ in
        check_def_pat bound arg_type;
        elab_term arg arg_type;
        infer_block body
end

module Named_tree = struct
  type term = Term of { head : term_head; loc : Location.t }

  and term_head =
    | T_var of { var : var }
    | T_block of { block : block; return : term }
    | T_apply of { funct : term; arg : term }
    | T_self of { bound : def_pat }
    | T_fix of { bound : def_pat; body : term }
    | T_sigma of { fst : def_pat; snd : term }
    | T_pair of { fst : term; snd : term }

  and def_pat = Def_pat of { head : def_pat_head; loc : Location.t }

  and def_pat_head =
    | DP_annot of { pat : def_pat; annot : term }
    | DP_var of { var : var }
    | DP_apply of { funct : def_pat; arg : let_pat }

  and let_pat = Let_pat of { head : let_pat_head; loc : Location.t }

  and let_pat_head =
    | LP_annot of { pat : let_pat; annot : term }
    | LP_var of { var : var }
    | LP_unit
    | LP_pair of { fst : let_pat; snd : let_pat }
    | LP_apply of { funct : def_pat; arg : let_pat }

  and block = Block of { head : block_head; loc : Location.t }

  and block_head =
    | B_hole
    | B_let of { bound : let_pat; arg : term; body : block }
    | B_def of { bound : def_pat; body : block }
    | B_fix of { bound : def_pat; arg : term; body : block }

  and var

  let t_wrap : Location.t -> term_head -> term = _
  let p_wrap : Location.t -> pat_head -> pat = _
  let b_wrap : Location.t -> block_head -> block = _
end

module Solve_names = struct
  (* TODO: make this as part of lparser? *)
  (* this also does some elaboration of patterns and blocks *)
  open Syntax_tree
  open Named_tree

  let rec solve_term ctx term =
    let (Term { head; loc } : Syntax_tree.term) = term in
    match head with
    | T_var { var } -> _
    | T_block { block; return } -> _
    | T_apply { funct; arg } ->
        let funct = solve_term ctx funct in
        let arg = solve_term ctx arg in
        t_wrap loc (T_apply { funct; arg })
    | T_self { bound } ->
        let bound, _inner_ctx = solve_pat ctx bound in
        t_wrap loc (T_self { bound })
    | T_fix { bound; body } ->
        let bound, inner_ctx = solve_pat ctx bound in
        let body = solve_term inner_ctx body in
        t_wrap loc (T_fix { bound; body })
    | T_unit -> failwith "solve_term: unit not implemented"
    | T_sigma { fst; snd } ->
        let fst, inner_ctx = solve_pat ctx fst in
        let snd = solve_term inner_ctx snd in
        t_wrap loc (T_sigma { fst; snd })
    | T_pair { fst; snd } ->
        let fst = solve_term ctx fst in
        let snd = solve_term ctx snd in
        t_wrap loc (T_pair { fst; snd })
    | T_box _ -> failwith "solve_term: box not implemented"

  and solve_block ctx block return = _

  and solve_pat ctx pat =
    let (Pat { head; loc } : Syntax_tree.pat) = pat in
    match head with
    | P_annot { pat; annot } ->
        let pat, inner_ctx = solve_pat ctx pat in
        let annot = solve_term inner_ctx annot in
        (p_wrap loc (P_annot { pat; annot }), inner_ctx)
    | P_var { var } -> _
    | P_unit -> (p_wrap loc P_unit, ctx)
    | P_pair { fst; snd } ->
        let fst, fst_ctx = solve_pat ctx fst in
        let snd, snd_ctx = solve_pat fst_ctx snd in
        (p_wrap loc (P_pair { fst; snd }), snd_ctx)
    | P_apply { funct; arg } ->
        let funct, inner_ctx = solve_pat ctx funct in
        let arg, arg_ctx = solve_pat inner_ctx arg in
        (p_wrap loc (P_apply { funct; arg }), arg_ctx)
    | P_box _ -> failwith "solve_pat: box not implemented"
end

module Type_check = struct
  open Named_tree

  let coerce received expected : unit = _
  let t_self (bound : def_pat) : term = _

  let rec check_term term expected =
    let (Term { head; loc } : Named_tree.term) = term in
    match head with
    | T_var { var } ->
        let received = _ in
        coerce received expected
    | T_block { block; return } ->
        infer_block block;
        check_term return expected
    | T_apply { funct; arg } ->
        (* TODO: check on funct *)
        let funct_type = _ in
        let param_type, body_type = match_pi funct_type in
        check_term funct funct_type;
        check_term arg param_type;
        coerce (apply body_type arg) expected
    | T_self { bound } ->
        let () = match_type expected in
        check_def_pat bound term
    | T_fix { bound; body } ->
        check_def_pat bound expected;
        check_term body expected
    | T_sigma { fst; snd } ->
        let () = match_type expected in
        infer_def_pat fst;
        check_term snd t_type
    | T_pair { fst; snd } ->
        let fst_type, snd_type = match_sigma expected in
        check_term fst fst_type;
        let snd_type = apply snd_type fst in
        check_term snd snd_type

  and infer_def_pat pat = check_def_pat pat (t_self pat)

  and check_def_pat pat expected =
    let (Def_pat { head; loc = _ }) = pat in
    match head with
    | DP_annot { pat = inner; annot } ->
        let received = t_self pat in
        check_def_pat inner received;
        check_term annot t_type;
        (* TODO: coerce? *)
        unify received expected
    | DP_var { var } -> check_var var expected
    | DP_apply { funct; arg } ->
        let funct_type = t_self pat in
        check_def_pat funct funct_type;
        (* TODO: coerce *)
        let param_type, body_type = match_pi funct_type in
        check_let_pat arg param_type;
        unify (apply body_type _) expected

  and check_let_pat pat expected =
    let (Let_pat { head; loc = _ }) = pat in
    match head with
    | LP_annot { pat = inner; annot } ->
        let received = t_self pat in
        check_let_pat inner received;
        check_term annot t_type;
        unify received expected
    | LP_var { var } -> _
    | LP_unit -> unify tf_unit expected
    | LP_pair { fst; snd } ->
        let fst_type, snd_type = match_sigma expected in
        check_let_pat fst fst_type;
        let snd_type = apply snd_type fst in
        check_let_pat snd snd_type
    | LP_apply { funct; arg } ->
        let funct_type = t_self pat in
        check_def_pat funct funct_type;
        let param_type, body_type = match_pi funct_type in
        check_let_pat arg param_type;
        unify (apply body_type _) expected

  and infer_block block =
    let (Block { head; loc = _ }) = block in
    match head with
    | B_hole -> ()
    | B_let { bound; arg; body } ->
        let arg_type = _ in
        check_let_pat bound arg_type;
        check_term arg arg_type;
        infer_block body
    | B_def { bound; body } ->
        let bound_type = _ in
        check_def_pat bound bound_type;
        infer_block body
    | B_fix { bound; arg; body } ->
        let arg_type = _ in
        check_def_pat bound arg_type;
        check_term arg arg_type;
        infer_block body
end

module Typed_tree = struct
  type term = Term of { head : term_head; loc : Location.t }

  and term_head =
    | T_var of { var : var }
    | T_block of { block : block; return : term }
    | T_apply of { funct : term; arg : term }
    | T_self of { bound : pat }
    | T_fix of { bound : pat; body : term }
    | T_sigma of { fst : pat; snd : term }
    | T_pair of { fst : term; snd : term }

  and pat = Def_pat of { head : pat_head; loc : Location.t }

  and pat_head =
    | P_annot of { pat : pat; annot : term }
    | P_var of { var : var }
    | P_unit
    | P_pair of { fst : pat; snd : pat }
    | P_apply of { funct : pat; arg : pat }

  and block = Block of { head : block_head; loc : Location.t }

  and block_head =
    | B_hole
    | B_let of { bound : pat; arg : term; body : block }
    | B_def of { bound : pat; body : block }
    | B_fix of { bound : pat; arg : term; body : block }

  and var

  let t_wrap : Location.t -> term_head -> term = _
  let p_wrap : Location.t -> pat_head -> pat = _
  let b_wrap : Location.t -> block_head -> block = _
end

module Elaborate = struct
  open Named_tree

  let coerce received expected : unit = _
  let v_type : var -> term = _
  let t_self (bound : def_pat) : term = _
  let t_type : term = _
  let t_apply : term -> term -> term = _
  let match_type : term -> unit = _
  let match_pi : term -> term * term = _
  let match_sigma : term -> term * term = _

  let rec check_term term expected =
    let (Term { head; loc } : Named_tree.term) = term in
    match head with
    | T_var { var } ->
        let received = _ in
        coerce (v_type var) expected
    | T_block { block; return } ->
        infer_block block;
        check_term return expected
    | T_apply { funct; arg } ->
        (* TODO: check on funct *)
        let funct_type = _ in
        let param_type, body_type = match_pi funct_type in
        check_term funct funct_type;
        check_term arg param_type;
        coerce (t_apply body_type arg) expected
    | T_self { bound } ->
        let () = match_type expected in
        check_def_pat bound term
    | T_fix { bound; body } ->
        check_def_pat bound expected;
        check_term body expected
    | T_sigma { fst; snd } ->
        let () = match_type expected in
        infer_def_pat fst;
        check_term snd t_type
    | T_pair { fst; snd } ->
        let fst_type, snd_type = match_sigma expected in
        check_term fst fst_type;
        let snd_type = t_apply snd_type fst in
        check_term snd snd_type

  and infer_def_pat pat =
    let (Def_pat { head; loc = _ }) = pat in
    match head with
    | DP_annot { pat; annot } ->
        let received = infer_def_pat pat in
        check_term annot t_type;
        unify received annot
    | DP_var { var } -> _
    | DP_apply { funct; arg } ->
        let funct_type = infer_def_pat funct in
        let param_type, body_type = match_pi funct_type in
        check_let_pat arg param_type;
        t_apply body_type _

  and check_def_pat pat expected =
    let received = infer_def_pat pat in
    unify received expected

  and check_let_pat pat expected =
    let (Let_pat { head; loc = _ }) = pat in
    match head with
    | LP_annot { pat = inner; annot } ->
        let received = t_self pat in
        check_let_pat inner received;
        check_term annot t_type;
        unify received expected
    | LP_var { var } -> _
    | LP_unit -> unify tf_unit expected
    | LP_pair { fst; snd } ->
        let fst_type, snd_type = match_sigma expected in
        check_let_pat fst fst_type;
        let snd_type = apply snd_type fst in
        check_let_pat snd snd_type
    | LP_apply { funct; arg } ->
        let funct_type = t_self pat in
        check_def_pat funct funct_type;
        let param_type, body_type = match_pi funct_type in
        check_let_pat arg param_type;
        unify (apply body_type _) expected

  and infer_block block =
    let (Block { head; loc = _ }) = block in
    match head with
    | B_hole -> ()
    | B_let { bound; arg; body } ->
        let arg_type = _ in
        check_any_pat bound arg_type;
        check_term arg arg_type;
        infer_block body
    | B_def { bound; body } ->
        let bound_type = _ in
        check_def_pat bound bound_type;
        infer_block body
    | B_fix { bound; arg; body } ->
        let arg_type = _ in
        check_def_pat bound arg_type;
        check_term arg arg_type;
        infer_block body
end

module Elaborate = struct
  open Named_tree
  open Check_tree

  type context =
    | Context of {
        vars : (Syntax_tree.var * Check_tree.var) list;
        at : Level.t;
      }

  let t_fresh_flex ctx () =
    let (Context { vars = _; at }) = ctx in
    t_new_flex ~at ()

  let v_fresh ctx ~type_ () =
    let (Context { vars = _; at }) = ctx in
    v_fresh ~at ~type_ ()

  let lookup ctx var =
    let (Context { vars }) = ctx in
    let vars = List.find_opt (fun (bound, _to_) -> Name.equal bound var) vars in
    match vars with
    | Some (_bound, to_) -> to_
    | None -> failwith "lookup: unbound variable"

  type val_pat = VP_head of { head : val_pat_head; to_ : Check_tree.var }

  and val_pat_head =
    | VP_annot of { pat : val_pat }
    | VP_var of { var : Syntax_tree.var }
    | VP_unit
    | VP_pair of { fst : val_pat; snd : val_pat }

  let vp_var : context -> Syntax_tree.var -> val_pat = _
  let vp_annot : context -> val_pat -> term -> val_pat = _
  let vp_unit ctx = _
  let vp_pair ctx fst snd = _

  type let_pat =
    | LP_val of { pat : val_pat }
    | LP_annot of { pat : let_pat; annot : term }
    | LP_apply of { funct : let_pat; arg : val_pat }

  type pat_spine =
    | PS_hole
    | PS_annot of { spine : pat_spine; annot : Syntax_tree.term }
    | PA_apply of { spine : pat_spine; arg : Syntax_tree.pat }

  let with_def ctx var to_ =
    let (Context { vars; at }) = ctx in
    (* TODO: put all binders in the same level *)
    let at = Level.succ at in
    Context { vars = (var, to_) :: vars; at }

  let rec with_val_pat ctx pat =
    let (VP_head { head; to_ }) = pat in
    match head with
    | VP_annot { pat } -> with_val_pat ctx pat
    | VP_var { var } -> with_def ctx var to_
    | VP_unit -> ctx
    | VP_pair { fst; snd } ->
        let ctx = with_val_pat ctx fst in
        with_val_pat ctx snd

  let rec with_let_pat ctx pat =
    match pat with
    | LP_val { pat } -> with_val_pat ctx pat
    | LP_annot { pat; annot = _ } -> with_let_pat ctx pat
    | LP_apply { funct; arg } ->
        let ctx = with_let_pat ctx funct in
        with_val_pat ctx arg

  let rec t_lambda_val_pat_let bound arg body =
    let (VP_head { head; to_ }) = bound in
    match head with
    | VP_annot { pat } -> t_lambda_val_pat_let pat (t_var to_) body
    | VP_var { var = _ } -> body
    | VP_unit -> te_drop arg body
    | VP_pair { fst; snd } ->
        t_lambda_val_pat_let fst (te_fst arg)
          (t_lambda_val_pat_let snd (te_snd arg) body)

  let t_lambda_val_pat bound body =
    let (VP_head { head = _; to_ }) = bound in
    (* TODO: this seems wrong *)
    let body = t_lambda_val_pat_let bound (t_var to_) body in
    t_lambda to_ body

  let rec t_lambda_let_pat_let bound arg body =
    match bound with
    | LP_val { pat } -> t_lambda_val_pat_let pat arg body
    | LP_annot { pat; annot } -> _
    | LP_apply { funct; arg } -> _

  let t_lambda_let_pat bound body = _

  let match_annot_val_pat pat =
    let (VP_head { head; to_ }) = pat in
    match head with
    | VP_annot { pat } -> (pat, v_type to_)
    | VP_var _ | VP_unit | VP_pair _ -> (pat, v_type to_)

  let rec elab_term term =
    let (Term { head; loc } : Syntax_tree.term) = term in
    match head with
    | T_var { var } -> _
    | T_block { block; return } -> elab_block block return
    | T_apply { funct; arg } ->
        let funct = elab_term funct in
        let arg = elab_term arg in
        t_apply funct arg
    | T_self { bound } ->
        let bound, body = _ in
        let body = elab_def_pat bound body in
        t_self body
    | T_fix { bound; body } ->
        let body = elab_def_pat bound body in
        t_fix body
    | T_sigma { fst; snd } ->
        let bound, fst = _ in
        let snd = elab_def_pat bound snd in
        tf_sigma fst snd
    | T_pair { fst; snd } ->
        let fst = elab_term fst in
        let snd = elab_term snd in
        ti_pair fst snd
    | T_unit | T_box _ -> failwith "elab_term: unit and box not implemented"

  and elab_block block return =
    let (Block { head; loc } : Syntax_tree.block) = block in
    _

  and elab_def_pat pat body =
    (* TODO: this is running out of order *)
    let (Def_pat { head; loc } : Named_tree.def_pat) = pat in
    match head with
    | DP_annot { pat; annot } ->
        let annot = elab_term annot in
        elab_def_pat_annot pat annot
    | DP_var { var } -> _
    | DP_apply { funct; arg } -> _

  and elab_def_pat_annot pat annot =
    let (Def_pat { head; loc } : Named_tree.def_pat) = pat in
    match head with
    | DP_annot { pat; annot } -> _
    | DP_var { var } -> _
    | DP_apply { funct; arg } ->
        let param, body = elab_val_pat arg annot in
        let annot = t_pi param body in
        elab_def_pat_annot funct annot

  and elab_any_pat pat =
    match pat with AP_def { pat } -> _ | AP_val { pat } -> _

  and elab_val_pat pat body = _
  and elab = _
end

module M = struct
  type term
  and term_head
end

module Check = struct
  open Check_tree

  let rec expand term =
    match t_head term with
    | T_apply { funct; arg } -> (
        expand funct;
        match t_head funct with
        | T_lambda { bound; body } ->
            expand body;
            let to_ = subst bound ~arg body in
            let () = t_link term ~to_ in
            expand to_
        | T_flex | T_type | T_var _ | T_pi _ | T_apply _ | T_self _ | T_fix _
        | T_frozen _ | T_unfold _ ->
            ())
    | T_unfold { fix } -> (
        expand fix;
        match t_head fix with
        | T_fix { body } ->
            let arg = t_frozen body in
            let to_ = t_apply body arg in
            t_link term ~to_;
            expand to_
        | T_flex | T_type | T_var _ | T_pi _ | T_lambda _ | T_apply _ | T_self _
        | T_frozen _ | T_unfold _ ->
            ())
    | T_flex | T_type | T_var _ | T_pi _ | T_lambda _ | T_self _ | T_fix _
    | T_frozen _ ->
        ()

  and subst var ~arg term =
    match t_head term with
    | T_flex -> term
    | T_type -> term
    | T_var { var = found } -> (
        match v_same var found with true -> arg | false -> term)
    | T_pi { param; body } -> (
        let param_aft = subst var ~arg param in
        let body_aft = subst var ~arg body in
        match t_same param param_aft && t_same body body_aft with
        | true -> term
        | false -> t_pi param_aft body_aft)
    | T_lambda { bound; body } -> (
        match v_same var bound with
        | true -> term
        | false -> (
            let body_aft = subst var ~arg body in
            match t_same body body_aft with
            | true -> term
            | false -> t_lambda bound body_aft))
    | T_apply { funct; arg } -> (
        let funct_aft = subst var ~arg funct in
        let arg_aft = subst var ~arg arg in
        match t_same funct funct_aft && t_same arg arg_aft with
        | true -> term
        | false -> t_apply funct_aft arg_aft)
    | T_self { body } -> (
        let body_aft = subst var ~arg body in
        match t_same body body_aft with
        | true -> term
        | false -> t_self body_aft)
    | T_fix { body } -> (
        let body_aft = subst var ~arg body in
        match t_same body body_aft with true -> term | false -> t_fix body_aft)
    | T_frozen { body } -> (
        let body_aft = subst var ~arg body in
        match t_same body body_aft with
        | true -> term
        | false -> t_frozen body_aft)
    | T_unfold { fix } -> (
        let fix_aft = subst var ~arg fix in
        match t_same fix fix_aft with true -> term | false -> t_unfold fix_aft)

  (* *)
  type head =
    | H_flex of { flex : term; spine : spine }
    | H_type
    | H_var of { var : var; spine : spine }
    | H_pi of { param : term; body : term }
    | H_lambda of { bound : var; body : term }
    | H_self of { body : term }
    | H_fix of { body : term }
    | H_frozen of { body : term; spine : spine }

  and spine =
    | S_hole
    | S_apply of { spine : spine; arg : term }
    | S_unfold of { spine : spine }

  let rec split term spine =
    match t_head term with
    | T_flex -> H_flex { flex = term; spine }
    | T_type ->
        assert (spine = S_hole);
        H_type
    | T_var { var } -> H_var { var; spine }
    | T_pi { param; body } ->
        assert (spine = S_hole);
        H_pi { param; body }
    | T_lambda { bound; body } ->
        assert (spine = S_hole);
        H_lambda { bound; body }
    | T_apply { funct; arg } -> split funct (S_apply { spine; arg })
    | T_self { body } ->
        assert (spine = S_hole);
        H_self { body }
    | T_fix { body } ->
        assert (spine = S_hole);
        H_fix { body }
    | T_frozen { body } -> H_frozen { body; spine }
    | T_unfold { fix } -> split fix (S_unfold { spine })

  let expand term = split term S_hole

  let rec unify_check ~flex to_ =
    match t_same flex to_ with
    | true -> failwith "unify_check: occurs check"
    | false ->
        let () = unify_check_head ~flex to_ in
        t_lower to_ ~to_:(t_at flex)

  and unify_check_head ~flex to_ =
    match t_head to_ with
    | T_flex -> ()
    | T_var { var } -> (
        match Level.(t_at flex < v_at var) with
        | true -> ()
        | false -> failwith "unify_check: escape check")
    | T_type -> ()
    | T_pi { param; body } ->
        unify_check ~flex param;
        unify_check ~flex body
    | T_lambda { bound; body } ->
        v_with_def bound (fun () -> unify_check ~flex body)
    | T_apply { funct; arg } ->
        unify_check ~flex funct;
        unify_check ~flex arg
    | T_self { body } -> unify_check ~flex body
    | T_fix { body } -> unify_check ~flex body
    | T_frozen { body } -> unify_check ~flex body
    | T_unfold { fix } -> unify_check ~flex fix

  let rec unify received expected =
    match t_same received expected with
    | true -> ()
    | false ->
        let () = unify_head received expected in
        t_link received ~to_:expected

  and unify_flex flex spine to_ =
    let rec extract spine vars =
      match spine with
      | S_hole -> List.rev vars
      | S_apply { spine; arg } ->
          let bound =
            match t_head arg with
            | T_var { var } -> var
            | T_flex | T_type | T_pi _ | T_lambda _ | T_apply _ | T_self _
            | T_fix _ | T_frozen _ | T_unfold _ ->
                failwith "unify_flex: not an unification pattern"
          in
          let () =
            match List.exists (fun var -> v_same var bound) vars with
            | true -> failwith "unify_flex: not an unification pattern"
            | false -> ()
          in
          extract spine (bound :: vars)
      | S_unfold _ -> failwith "unify_flex: unfold found"
    in
    let vars = extract spine [] in
    let to_ = List.fold_left (fun to_ bound -> t_lambda bound to_) to_ vars in
    unify_check ~flex to_

  and unify_head received expected =
    match (expand received, expand expected) with
    | ( H_flex { flex = received_flex; spine = received_spine },
        H_flex { flex = expected_flex; spine = expected_spine } ) -> (
        match t_same received_flex expected_flex with
        | true -> unify_spine received_spine expected_spine
        | false -> unify_flex received_flex received_spine expected)
    | H_flex { flex; spine }, _ -> unify_flex flex spine expected
    | _, H_flex { flex; spine } -> unify_flex flex spine received
    | H_type, H_type -> ()
    | ( H_var { var = received_var; spine = received_spine },
        H_var { var = expected_var; spine = expected_spine } ) -> (
        match received_var == expected_var with
        | true -> unify_spine received_spine expected_spine
        | false -> failwith "unify.bug: cannot unify different variables")
    | ( H_pi { param = received_param; body = received_body },
        H_pi { param = expected_param; body = expected_body } ) ->
        unify received_param expected_param;
        unify received_body expected_body
    | H_lambda { bound; body = received_body }, _ ->
        let expected_body = t_apply expected (t_var bound) in
        unify received_body expected_body
    | _, H_lambda { bound; body = expected_body } ->
        let received_body = t_apply received (t_var bound) in
        unify received_body expected_body
    | H_self { body = received_body }, H_self { body = expected_body } ->
        unify received_body expected_body
    | H_fix { body = received_body }, H_fix { body = expected_body } ->
        (* TODO: eta fix *)
        unify received_body expected_body
    | H_fix { body = received_body }, _ ->
        (* TODO: document on why this is safe *)
        (* TODO: unfold frozen here? *)
        let received = t_apply received_body expected in
        let expected = t_unfold received in
        unify received expected
    | _, H_fix { body = expected_body } ->
        let received = t_unfold received in
        let expected = t_apply expected_body received in
        unify received expected
    | ( H_frozen { body = received_body; spine = received_spine },
        H_frozen { body = expected_body; spine = expected_spine } ) ->
        unify received_body expected_body;
        unify_spine received_spine expected_spine
    | H_frozen _, _ | _, H_frozen _ -> failwith "unify: clash with frozen"
    | ( (H_type | H_var _ | H_pi _ | H_self _),
        (H_type | H_var _ | H_pi _ | H_self _) ) ->
        failwith "unify: type clash"

  and unify_spine received_spine expected_spine =
    match (received_spine, expected_spine) with
    | S_hole, S_hole -> ()
    | ( S_apply { spine = received_spine; arg = received_arg },
        S_apply { spine = expected_spine; arg = expected_arg } ) ->
        unify received_arg expected_arg;
        unify_spine received_spine expected_spine
    | S_unfold { spine = received_spine }, S_unfold { spine = expected_spine }
      ->
        unify_spine received_spine expected_spine
    | (S_hole | S_apply _ | S_unfold _), (S_hole | S_apply _ | S_unfold _) ->
        (* TODO: can this actually happens, not with isorecursion I think *)
        failwith "unify_spine: spine clash"

  (* *)
  let rec unfreeze term =
    match t_head term with
    | T_apply { funct; arg } -> unfreeze funct
    | T_unfold { fix } -> unfreeze fix
    (* TODO: this being done in a mutable way is annoying *)
    | T_frozen { body } -> t_link term ~to_:(t_fix body)
    | T_flex | T_type | T_var _ | T_pi _ | T_lambda _ | T_self _ | T_fix _ -> ()

  let unify received expected =
    unfreeze received;
    unfreeze expected;
    unify received expected

  let match_type type_ = unify t_type type_

  let match_pi type_ =
    let at = t_at type_ in
    let param = t_new_flex ~at () in
    let body = t_new_flex ~at () in
    unify (t_pi param body) type_;
    (param, body)

  let match_self type_ =
    let at = t_at type_ in
    let body = t_new_flex ~at () in
    unify (t_self body) type_;
    body

  (* *)

  type spine_item = S_apply of { arg : term } | S_unfold

  let rec check term expected =
    match t_head term with
    | T_type -> match_type expected
    | T_var { var } -> unify (v_type var) expected
    | T_pi { param; body } ->
        match_type expected;
        check param t_type;
        check body (t_arrow param t_type)
    | T_lambda { bound; body } ->
        let param_type, body_type = match_pi expected in
        unify (v_type bound) param_type;
        check body (t_apply body_type (t_var bound))
    | T_self { body } ->
        match_type expected;
        check body (t_arrow term t_type)
    | T_fix { body } ->
        let body_type = match_self expected in
        check_elim body (S_apply { arg = term } :: []) (t_apply body_type term)
    | T_apply _ -> check_elim term [] expected
    | T_unfold _ -> check_elim term [] expected
    | T_flex -> failwith "check.bug: flex term found"
    | T_frozen _ -> failwith "check.bug: frozen term found"

  and check_elim head spine expected =
    match t_head head with
    | T_apply { funct; arg } ->
        check_elim funct (S_apply { arg } :: spine) expected
    | T_lambda { bound; body } -> (
        match spine with
        | [] -> check head expected
        | S_apply { arg } :: spine ->
            (* TODO: check arg after typing the body? *)
            check arg (v_type bound);
            let () =
              let (Var var) = bound in
              assert (var.to_ = None);
              var.to_ <- Some arg
            in
            check_elim body spine expected
        | S_unfold :: spine ->
            (* TODO: support this? *)
            failwith "check_elim: cannot unfold lambda")
    | T_fix { body } -> (
        match spine with
        | [] -> check head expected
        | S_unfold :: spine ->
            check_elim body (S_apply { arg = head } :: spine) expected
        | S_apply { arg } :: spine ->
            (* TODO: support this? *)
            failwith "check_elim: cannot apply to fix")
    | T_unfold { fix } -> check_elim fix (S_unfold :: spine) expected
    | T_var { var } ->
        let received = v_type var in
        check_elim_coerce head received spine expected
    | T_type | T_pi _ | T_self _ -> (
        match spine with
        | [] -> check head expected
        | S_apply { arg } :: spine -> failwith "check_elim: cannot apply a type"
        | S_unfold :: spine -> failwith "check_elim: cannot unfold a type")
    | T_flex -> failwith "check.bug: flex term found"
    | T_frozen _ -> failwith "check.bug: frozen term found"

  and check_elim_coerce head received spine expected =
    match spine with
    | [] -> unify received expected
    | S_apply { arg } :: spine ->
        let param_type, body_type = match_pi received in
        check arg param_type;
        (* TODO: recreating the head is bad *)
        let received = t_apply body_type arg in
        let head = t_apply head arg in
        check_elim_coerce head received spine body_type
    | S_unfold :: spine ->
        let body_type = match_self received in
        let received = t_apply body_type head in
        let head = t_unfold head in
        check_elim_coerce head received spine expected
end
