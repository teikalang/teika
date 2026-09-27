module Name = Lsp_demo_syntax.Name
module Syntax_tree = Lsp_demo_syntax.Lparser.Syntax_tree

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
    | T_annot of { value : term; annot : term }
    | T_self of { body : term }
    | T_fix of { body : term }
    | T_frozen of { body : term }
    | T_unfold of { fix : term }
    | T_unit_type
    | T_unit
    | T_drop of { arg : term; body : term }
    | T_sigma of { fst : term; snd : term }
    | T_pair of { fst : term; snd : term }
    | T_fst of { pair : term }
    | T_snd of { pair : term }

  and var = Var of { mutable type_ : term; mutable at : Level.t }

  (* physical identity *)
  let v_same (left : var) (right : var) : bool = left == right
  let v_fresh ~at ~type_ () = Var { type_; at }

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

  let t_annot value annot =
    let at = Level.max (t_at value) (t_at annot) in
    t_new ~at (T_annot { value; annot })

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

  let tf_unit = t_new ~at:Level.zero T_unit_type
  let ti_unit = t_new ~at:Level.zero T_unit

  let te_drop arg body =
    let at = Level.max (t_at arg) (t_at body) in
    t_new ~at (T_drop { arg; body })

  let tf_sigma fst snd =
    let at = Level.max (t_at fst) (t_at snd) in
    t_new ~at (T_sigma { fst; snd })

  let ti_pair fst snd =
    let at = Level.max (t_at fst) (t_at snd) in
    t_new ~at (T_pair { fst; snd })

  let te_fst pair = t_new ~at:(t_at pair) (T_fst { pair })
  let te_snd pair = t_new ~at:(t_at pair) (T_snd { pair })
end

module Elaborate = struct
  open Syntax_tree
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

  type pat =
    | Pat of { var : Syntax_tree.var; to_ : Check_tree.var; type_ : term }

  type pat_result = Syntax of Syntax_tree.term | Elaborated of term | Fresh
  type declaration = { name : Syntax_tree.var; pattern : pat }

  let rec split_apply bound args =
    let (Syntax_tree.Pat { head; _ }) = bound in
    match head with
    | P_apply { funct; arg } -> split_apply funct (arg :: args)
    | P_annot _ | P_var _ | P_wildcard | P_unit | P_pair _ -> (bound, args)

  let rec syntax_pat_name bound =
    let (Syntax_tree.Pat { head; _ }) = bound in
    match head with
    | P_annot { pat; _ } -> syntax_pat_name pat
    | P_apply { funct; _ } -> syntax_pat_name funct
    | P_var { var } -> var
    | P_wildcard | P_unit | P_pair _ ->
        failwith "syntax_pat_name: pattern does not bind one name"

  let match_annot bound =
    let (Pat { var = _; to_ = _; type_ }) = bound in
    (bound, type_)

  let match_bound bound =
    let (Pat { var = _; to_; type_ = _ }) = bound in
    to_

  let with_def ctx pat =
    let (Context { vars; at }) = ctx in
    let (Pat { var; to_; type_ = _ }) = pat in
    let vars = (var, to_) :: vars in
    let at = Level.succ at in
    Context { vars; at }

  let rec elab_term ctx term =
    let (Term { head; loc } : Syntax_tree.term) = term in
    match head with
    | T_hole -> t_fresh_flex ctx ()
    | T_type -> t_type
    | T_var { var } ->
        let var = lookup ctx var in
        t_var var
    | T_def _ -> elab_block ctx [] term
    | T_let { bound; arg; body } -> elab_let ctx bound arg body
    | TF_pi { bound; body } ->
        let bound = elab_pat ctx bound in
        let bound, param = match_annot bound in
        let body = elab_lambda ctx bound body in
        t_pi param body
    | TI_lambda { bound; body } ->
        let bound = elab_pat ctx bound in
        elab_lambda ctx bound body
    | TE_apply { funct; arg } ->
        let funct = elab_term ctx funct in
        let arg = elab_term ctx arg in
        t_apply funct arg
    | TE_annot { value; annot } ->
        let value = elab_term ctx value in
        let annot = elab_term ctx annot in
        t_annot value annot
    | TF_self { bound; body } ->
        let bound = elab_pat ctx bound in
        let body = elab_lambda ctx bound body in
        t_self body
    | TI_fix { bound; body } ->
        let bound = elab_pat ctx bound in
        let body = elab_lambda ctx bound body in
        t_fix body
    | TE_unfold { fix } ->
        let fix = elab_term ctx fix in
        t_unfold fix
    | TF_unit -> tf_unit
    | TI_unit -> ti_unit
    | TE_drop { arg; body } ->
        let arg = elab_term ctx arg in
        let body = elab_term ctx body in
        te_drop arg body
    | TF_sigma { fst; snd } ->
        (* TODO: this elab_pat is bad imo *)
        let fst = elab_pat ctx fst in
        let bound, fst = match_annot fst in
        let snd = elab_lambda ctx bound snd in
        tf_sigma fst snd
    | TI_pair { fst; snd } ->
        let fst = elab_term ctx fst in
        let snd = elab_term ctx snd in
        ti_pair fst snd
    | TE_fst { pair } ->
        let pair = elab_term ctx pair in
        te_fst pair
    | TE_snd { pair } ->
        let pair = elab_term ctx pair in
        te_snd pair

  and elab_pat ctx bound : pat =
    let (Pat { head; loc } : Syntax_tree.pat) = bound in
    match head with
    | P_annot { pat; annot } -> (
        let (Syntax_tree.Pat { head; _ }) = pat in
        match head with
        | P_apply _ -> elab_pat_apply ctx pat (Syntax annot)
        | P_annot _ | P_var _ | P_wildcard | P_unit | P_pair _ ->
            let type_ = elab_term ctx annot in
            elab_pat_annot ctx pat type_)
    | P_var { var } ->
        let type_ = t_fresh_flex ctx () in
        let to_ = v_fresh ctx ~type_ () in
        Pat { var; to_; type_ }
    | P_wildcard ->
        let type_ = t_fresh_flex ctx () in
        let var = Name.make "\000" in
        let to_ = v_fresh ctx ~type_ () in
        Pat { var; to_; type_ }
    | P_apply _ -> elab_pat_apply ctx bound Fresh
    | P_unit | P_pair _ ->
        failwith "elab_pat: unit and pair patterns not supported"

  and elab_pat_annot ctx bound type_ =
    let (Pat { head; loc } : Syntax_tree.pat) = bound in
    match head with
    | P_var { var } ->
        let to_ = v_fresh ctx ~type_ () in
        Pat { var; to_; type_ }
    | P_wildcard ->
        let var = Name.make "\000" in
        let to_ = v_fresh ctx ~type_ () in
        Pat { var; to_; type_ }
    | P_apply _ -> elab_pat_apply ctx bound (Elaborated type_)
    | P_annot _ -> failwith "elab_pat_annot: nested annotations not supported"
    | P_unit | P_pair _ ->
        failwith "elab_pat_annot: unit and pair patterns not supported"

  and elab_pat_apply ctx bound result =
    let funct, args = split_apply bound [] in
    let rec elaborate_args ctx elaborated = function
      | [] -> (ctx, List.rev elaborated)
      | arg :: args ->
          let arg = elab_pat ctx arg in
          let arg, param = match_annot arg in
          let inner_ctx = with_def ctx arg in
          elaborate_args inner_ctx ((arg, param) :: elaborated) args
    in
    let inner_ctx, args = elaborate_args ctx [] args in
    let result =
      match result with
      | Syntax result -> elab_term inner_ctx result
      | Elaborated result -> result
      | Fresh -> t_fresh_flex inner_ctx ()
    in
    let function_type =
      List.fold_right
        (fun (arg, param) body ->
          let bound = match_bound arg in
          t_pi param (t_lambda bound body))
        args result
    in
    elab_pat_annot ctx funct function_type

  and elab_definition ctx declared bound arg =
    let funct, args = split_apply bound [] in
    let rec elaborate_args ctx expected elaborated = function
      | [] -> (ctx, List.rev elaborated)
      | arg :: args ->
          let arg, expected =
            match expected with
            | None -> (elab_pat ctx arg, None)
            | Some expected -> (
                match t_head expected with
                | T_pi { param; body } -> (
                    match t_head body with
                    | T_lambda { bound; body } ->
                        let var = syntax_pat_name arg in
                        (Pat { var; to_ = bound; type_ = param }, Some body)
                    | T_flex | T_type | T_var _ | T_pi _ | T_apply _ | T_annot _
                    | T_self _ | T_fix _ | T_frozen _ | T_unfold _ | T_unit_type
                    | T_unit | T_drop _ | T_sigma _ | T_pair _ | T_fst _
                    | T_snd _ ->
                        failwith "elab_definition: pi body is not a lambda")
                | T_flex | T_type | T_var _ | T_lambda _ | T_apply _ | T_annot _
                | T_self _ | T_fix _ | T_frozen _ | T_unfold _ | T_unit_type
                | T_unit | T_drop _ | T_sigma _ | T_pair _ | T_fst _ | T_snd _
                  ->
                    failwith "elab_definition: too many applied parameters")
          in
          let inner_ctx = with_def ctx arg in
          elaborate_args inner_ctx expected (arg :: elaborated) args
    in
    let expected =
      Option.map (fun pattern -> v_type (match_bound pattern)) declared
    in
    let inner_ctx, args = elaborate_args ctx expected [] args in
    let arg = elab_term inner_ctx arg in
    let arg =
      List.fold_right
        (fun pattern body -> t_lambda (match_bound pattern) body)
        args arg
    in
    (syntax_pat_name funct, funct, arg)

  and elab_let ctx bound arg body =
    let _name, funct, arg = elab_definition ctx None bound arg in
    let funct = elab_pat ctx funct in
    let body = elab_lambda ctx funct body in
    t_apply body arg

  and elab_block ctx declarations term =
    let (Term { head; _ } : Syntax_tree.term) = term in
    match head with
    | T_def { bound; body } ->
        let name = syntax_pat_name bound in
        if
          List.exists
            (fun declaration -> Name.equal declaration.name name)
            declarations
        then failwith "elab_block: duplicate declaration";
        let pattern = elab_pat ctx bound in
        let ctx = with_def ctx pattern in
        elab_block ctx ({ name; pattern } :: declarations) body
    | T_let { bound; arg; body } -> (
        let name = syntax_pat_name bound in
        let rec take reversed = function
          | [] -> None
          | declaration :: declarations ->
              if Name.equal declaration.name name then
                Some (declaration, List.rev_append reversed declarations)
              else take (declaration :: reversed) declarations
        in
        match take [] declarations with
        | Some (declaration, declarations) ->
            let _name, _funct, arg =
              elab_definition ctx (Some declaration.pattern) bound arg
            in
            let body = elab_block ctx declarations body in
            let bound = match_bound declaration.pattern in
            t_apply (t_lambda bound body) arg
        | None ->
            let _name, funct, arg = elab_definition ctx None bound arg in
            let pattern = elab_pat ctx funct in
            let inner_ctx = with_def ctx pattern in
            let body = elab_block inner_ctx declarations body in
            let bound = match_bound pattern in
            t_apply (t_lambda bound body) arg)
    | T_hole | T_type | T_var _ | TF_pi _ | TI_lambda _ | TE_apply _
    | TE_annot _ | TF_self _ | TI_fix _ | TE_unfold _ | TF_unit | TI_unit
    | TE_drop _ | TF_sigma _ | TI_pair _ | TE_fst _ | TE_snd _ ->
        if declarations <> [] then
          failwith "elab_block: declaration has no definition";
        elab_term ctx term

  and elab_lambda ctx bound body =
    let ctx = with_def ctx bound in
    let body = elab_term ctx body in
    let bound = match_bound bound in
    t_lambda bound body
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
        | T_flex | T_type | T_var _ | T_pi _ | T_apply _ | T_annot _ | T_self _
        | T_fix _ | T_frozen _ | T_unfold _ | T_unit_type | T_unit | T_drop _
        | T_sigma _ | T_pair _ | T_fst _ | T_snd _ ->
            ())
    | T_unfold { fix } -> (
        expand fix;
        match t_head fix with
        | T_fix { body } ->
            let arg = t_frozen body in
            let to_ = t_apply body arg in
            t_link term ~to_;
            expand to_
        | T_flex | T_type | T_var _ | T_pi _ | T_lambda _ | T_apply _
        | T_annot _ | T_self _ | T_frozen _ | T_unfold _ | T_unit_type | T_unit
        | T_drop _ | T_sigma _ | T_pair _ | T_fst _ | T_snd _ ->
            ())
    | T_annot { value; annot } ->
        expand value;
        expand annot
    | T_drop { arg; body } ->
        expand arg;
        expand body
    | T_sigma { fst; snd } | T_pair { fst; snd } ->
        expand fst;
        expand snd
    | T_fst { pair } | T_snd { pair } -> expand pair
    | T_flex | T_type | T_var _ | T_pi _ | T_lambda _ | T_self _ | T_fix _
    | T_frozen _ | T_unit_type | T_unit ->
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
    | T_annot { value; annot } ->
        let value_aft = subst var ~arg value in
        let annot_aft = subst var ~arg annot in
        if t_same value value_aft && t_same annot annot_aft then term
        else t_annot value_aft annot_aft
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
    | T_unit_type | T_unit -> term
    | T_drop { arg = dropped; body } ->
        let dropped_aft = subst var ~arg dropped in
        let body_aft = subst var ~arg body in
        if t_same dropped dropped_aft && t_same body body_aft then term
        else te_drop dropped_aft body_aft
    | T_sigma { fst; snd } ->
        let fst_aft = subst var ~arg fst in
        let snd_aft = subst var ~arg snd in
        if t_same fst fst_aft && t_same snd snd_aft then term
        else tf_sigma fst_aft snd_aft
    | T_pair { fst; snd } ->
        let fst_aft = subst var ~arg fst in
        let snd_aft = subst var ~arg snd in
        if t_same fst fst_aft && t_same snd snd_aft then term
        else ti_pair fst_aft snd_aft
    | T_fst { pair } ->
        let pair_aft = subst var ~arg pair in
        if t_same pair pair_aft then term else te_fst pair_aft
    | T_snd { pair } ->
        let pair_aft = subst var ~arg pair in
        if t_same pair pair_aft then term else te_snd pair_aft

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
    | H_unit_type
    | H_unit
    | H_sigma of { fst : term; snd : term }
    | H_pair of { fst : term; snd : term }
    | H_fst of { pair : term; spine : spine }
    | H_snd of { pair : term; spine : spine }

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
    | T_lambda { bound; body } -> (
        match spine with
        | S_hole -> H_lambda { bound; body }
        | S_apply { spine; arg } -> split (subst bound ~arg body) spine
        | S_unfold _ -> failwith "split: cannot unfold lambda")
    | T_apply { funct; arg } -> split funct (S_apply { spine; arg })
    | T_annot { value; _ } -> split value spine
    | T_self { body } ->
        assert (spine = S_hole);
        H_self { body }
    | T_fix { body } ->
        assert (spine = S_hole);
        H_fix { body }
    | T_frozen { body } -> H_frozen { body; spine }
    | T_unfold { fix } -> split fix (S_unfold { spine })
    | T_unit_type ->
        assert (spine = S_hole);
        H_unit_type
    | T_unit ->
        assert (spine = S_hole);
        H_unit
    | T_drop { body; _ } -> split body spine
    | T_sigma { fst; snd } ->
        assert (spine = S_hole);
        H_sigma { fst; snd }
    | T_pair { fst; snd } ->
        assert (spine = S_hole);
        H_pair { fst; snd }
    | T_fst { pair } -> (
        match t_head pair with
        | T_pair { fst; _ } -> split fst spine
        | T_annot { value; _ } -> split (te_fst value) spine
        | T_flex | T_type | T_var _ | T_pi _ | T_lambda _ | T_apply _ | T_self _
        | T_fix _ | T_frozen _ | T_unfold _ | T_unit_type | T_unit | T_drop _
        | T_sigma _ | T_fst _ | T_snd _ ->
            H_fst { pair; spine })
    | T_snd { pair } -> (
        match t_head pair with
        | T_pair { snd; _ } -> split snd spine
        | T_annot { value; _ } -> split (te_snd value) spine
        | T_flex | T_type | T_var _ | T_pi _ | T_lambda _ | T_apply _ | T_self _
        | T_fix _ | T_frozen _ | T_unfold _ | T_unit_type | T_unit | T_drop _
        | T_sigma _ | T_fst _ | T_snd _ ->
            H_snd { pair; spine })

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
        match Level.(v_at var <= t_at flex) with
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
    | T_annot { value; annot } ->
        unify_check ~flex value;
        unify_check ~flex annot
    | T_self { body } -> unify_check ~flex body
    | T_fix { body } -> unify_check ~flex body
    | T_frozen { body } -> unify_check ~flex body
    | T_unfold { fix } -> unify_check ~flex fix
    | T_unit_type | T_unit -> ()
    | T_drop { arg; body } ->
        unify_check ~flex arg;
        unify_check ~flex body
    | T_sigma { fst; snd } | T_pair { fst; snd } ->
        unify_check ~flex fst;
        unify_check ~flex snd
    | T_fst { pair } | T_snd { pair } -> unify_check ~flex pair

  let rec unify received expected =
    match t_same received expected with
    | true -> ()
    | false ->
        let received_is_flexible =
          match expand received with H_flex _ -> true | _ -> false
        in
        let expected_is_flexible =
          match expand expected with H_flex _ -> true | _ -> false
        in
        let () = unify_head received expected in
        if received_is_flexible then t_link received ~to_:expected
        else if expected_is_flexible then t_link expected ~to_:received

  and unify_flex flex spine to_ =
    let rec extract spine vars =
      match spine with
      | S_hole -> List.rev vars
      | S_apply { spine; arg } ->
          let bound =
            match t_head arg with
            | T_var { var } -> var
            | T_flex | T_type | T_pi _ | T_lambda _ | T_apply _ | T_annot _
            | T_self _ | T_fix _ | T_frozen _ | T_unfold _ | T_unit_type
            | T_unit | T_drop _ | T_sigma _ | T_pair _ | T_fst _ | T_snd _ ->
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
    | H_unit_type, H_unit_type | H_unit, H_unit -> ()
    | ( H_sigma { fst = received_fst; snd = received_snd },
        H_sigma { fst = expected_fst; snd = expected_snd } )
    | ( H_pair { fst = received_fst; snd = received_snd },
        H_pair { fst = expected_fst; snd = expected_snd } ) ->
        unify received_fst expected_fst;
        unify received_snd expected_snd
    | ( H_fst { pair = received_pair; spine = received_spine },
        H_fst { pair = expected_pair; spine = expected_spine } )
    | ( H_snd { pair = received_pair; spine = received_spine },
        H_snd { pair = expected_pair; spine = expected_spine } ) ->
        unify received_pair expected_pair;
        unify_spine received_spine expected_spine
    | H_frozen _, _ | _, H_frozen _ -> failwith "unify: clash with frozen"
    | ( ( H_type | H_var _ | H_pi _ | H_self _ | H_unit_type | H_unit
        | H_sigma _ | H_pair _ | H_fst _ | H_snd _ ),
        ( H_type | H_var _ | H_pi _ | H_self _ | H_unit_type | H_unit
        | H_sigma _ | H_pair _ | H_fst _ | H_snd _ ) ) ->
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
    | T_annot { value; annot } ->
        unfreeze value;
        unfreeze annot
    | T_unfold { fix } -> unfreeze fix
    | T_drop { arg; body } ->
        unfreeze arg;
        unfreeze body
    | T_sigma { fst; snd } | T_pair { fst; snd } ->
        unfreeze fst;
        unfreeze snd
    | T_fst { pair } | T_snd { pair } -> unfreeze pair
    (* TODO: this being done in a mutable way is annoying *)
    | T_frozen { body } -> t_link term ~to_:(t_fix body)
    | T_flex | T_type | T_var _ | T_pi _ | T_lambda _ | T_self _ | T_fix _
    | T_unit_type | T_unit ->
        ()

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

  let match_sigma type_ =
    let at = t_at type_ in
    let fst = t_new_flex ~at () in
    let bound = v_fresh ~at ~type_:fst () in
    let snd_body = t_new_flex ~at () in
    let snd = t_lambda bound snd_body in
    unify (tf_sigma fst snd) type_;
    (fst, snd)

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
    | T_annot { value; annot } ->
        check annot t_type;
        check value annot;
        unify annot expected
    | T_self { body } ->
        match_type expected;
        check body (t_arrow term t_type)
    | T_fix { body } ->
        let body_type = match_self expected in
        let bound, definition =
          match t_head body with
          | T_lambda { bound; body } -> (bound, body)
          | T_flex | T_type | T_var _ | T_pi _ | T_apply _ | T_annot _
          | T_self _ | T_fix _ | T_frozen _ | T_unfold _ | T_unit_type | T_unit
          | T_drop _ | T_sigma _ | T_pair _ | T_fst _ | T_snd _ ->
              failwith "check: fix body is not a lambda"
        in
        unify (v_type bound) expected;
        check definition (t_apply body_type term)
    | T_apply _ -> check_elim term [] expected
    | T_unfold _ -> check_elim term [] expected
    | T_unit_type -> match_type expected
    | T_unit -> unify tf_unit expected
    | T_drop { arg; body } ->
        check arg tf_unit;
        check body expected
    | T_sigma { fst; snd } ->
        match_type expected;
        check fst t_type;
        check snd (t_arrow fst t_type)
    | T_pair { fst; snd } ->
        let fst_type, snd_type = match_sigma expected in
        check fst fst_type;
        check snd (t_apply snd_type fst)
    | T_fst { pair } ->
        let pair_type = t_new_flex ~at:(t_at pair) () in
        let fst_type, _snd_type = match_sigma pair_type in
        check pair pair_type;
        unify fst_type expected
    | T_snd { pair } ->
        let pair_type = t_new_flex ~at:(t_at pair) () in
        let _fst_type, snd_type = match_sigma pair_type in
        check pair pair_type;
        unify (t_apply snd_type (te_fst pair)) expected
    | T_flex -> ()
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
    | T_annot { value; annot } ->
        check annot t_type;
        check value annot;
        check_elim_coerce value annot spine expected
    | T_var { var } ->
        let received = v_type var in
        check_elim_coerce head received spine expected
    | T_type | T_pi _ | T_self _ | T_unit_type | T_unit | T_drop _ | T_sigma _
    | T_pair _ | T_fst _ | T_snd _ -> (
        match spine with
        | [] -> check head expected
        | _ ->
            let received = t_new_flex ~at:(t_at head) () in
            check head received;
            check_elim_coerce head received spine expected)
    | T_flex -> ()
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
        check_elim_coerce head received spine expected
    | S_unfold :: spine ->
        let body_type = match_self received in
        let received = t_apply body_type head in
        let head = t_unfold head in
        check_elim_coerce head received spine expected
end
