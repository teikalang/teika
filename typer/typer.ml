module Name = Syntax.Ctree.Name
module Syntax_tree = Syntax.Lparser.Syntax_tree

module Level : sig
  type level
  type t = level

  val ( <= ) : level -> level -> bool
  val ( < ) : level -> level -> bool
  val zero : level
  val succ : level -> level
  val equal : level -> level -> bool
  val min : level -> level -> level
  val max : level -> level -> level
end = struct
  type level = int
  type t = level

  let ( <= ) (l : level) (r : level) : bool = l <= r
  let ( < ) (l : level) (r : level) : bool = l < r
  let zero = Int.zero
  let succ = Int.succ
  let equal = Int.equal
  let min = Int.min
  let max = Int.max
end

module Core_tree = struct
  type term =
    | Term of {
        mutable head : term_head;
        mutable at : Level.t;
        mutable link : term;
      }

  and term_head =
    | T_flex of { mutable pending : constraint_ list }
    | T_subst of { flex : term; args : term list }
    (* *)
    | T_var
    | T_lambda of { bound : var; body : term }
    | T_apply of { funct : term; arg : term }
    | T_fix of { body : term }
    (* *)
    | T_type
    | T_pi of { param : term; body : term }
    | T_self of { body : term }

  and var

  and constraint_ =
    | C_true
    | C_eq of { left : term; right : term; next : constraint_ }

  let t_head : term -> term_head = _

  let rec expand term =
    match t_head term with
    | T_flex _ -> _
    | T_subst _ -> _
    | T_var -> _
    | T_lambda _ -> _
    | T_apply { funct; arg } -> _
    | T_fix _ -> _
    | T_type -> _
    | T_pi _ -> _
    | T_self _ -> _

  let t_same : term -> term -> bool = _
  let apply funct arg = _

  type unify_result = UR_ok | UR_delayed of constraint_

  let rec unify received expected =
    match t_same received expected with
    | true -> UR_ok
    | false ->
        expand received;
        expand expected;
        unify_head received expected

  and unify_flex flex args to_ = _

  and unify_head received expected =
    match (t_head received, t_head expected) with
    | T_flex _, _ -> unify_flex received [] expected
    | _, T_flex _ -> unify_flex expected [] received
    | T_subst { flex; args }, _ -> unify_flex flex args expected
    | _, T_subst { flex; args } -> unify_flex flex args received
    | T_lambda _, _ | _, T_lambda _ ->
        let arg = _ in
        let received = apply received arg in
        let expected = apply expected arg in
        unify received expected
    | T_fix { body = received_body }, T_fix { body = expected_body } ->
        unify received_body expected_body
    | T_fix _, _ | _, T_fix _ -> _
    | ( T_apply { funct = received_funct; arg = received_arg },
        T_apply { funct = expected_funct; arg = expected_arg } ) ->
        unify_pair (received_funct, received_arg) (expected_funct, expected_arg)
    | T_type, T_type -> ()
    | ( T_pi { param = received_param; body = received_body },
        T_pi { param = expected_param; body = expected_body } ) ->
        unify_pair
          (received_param, received_body)
          (expected_param, expected_body)
    | T_self { body = received_body }, T_self { body = expected_body } ->
        unify received_body expected_body
    | ( (T_var | T_apply _ | T_type | T_pi _ | T_self _),
        (T_var | T_apply _ | T_type | T_pi _ | T_self _) ) ->
        failwith "unify: clash"

  and unify_pair (received_fst, received_snd) (expected_fst, expected_snd) =
    match unify received_fst expected_fst with
    | UR_ok -> unify received_snd expected_snd
    | UR_delayed dep -> _
end

module Core_tree = struct
  type term =
    | Term of {
        head : term_head;
        mutable at : Level.t;
        mutable mark : bool;
        mutable link : term;
      }

  and term_head =
    | T_flex
    | T_subst of { flex : term; spine : spine }
    | T_var
    | T_lambda of { bound : var; body : term }
    | T_apply of { funct : term; arg : term }

  and var
  and spine = S_hole | S_apply of { head : spine; arg : spine }

  let t_head : term -> term_head = _

  let rec expand term =
    match t_head term with
    | T_subst { flex; spine } -> (
        match t_head flex with
        | T_flex -> ()
        | T_subst _ | T_var | T_lambda _ | T_apply _ ->
            let to_ = expand_spine flex spine in
            t_link term ~to_)
    | T_apply { funct; arg } -> (
        expand funct;
        match t_head funct with
        | T_lambda { bound; body } ->
            let to_ = expand_let bound arg body in
            t_link term ~to_
        | T_var | T_apply _ -> ()
        | T_flex | T_subst _ ->
            failwith "expand.bug: flex and subst under apply")
    | T_flex | T_var | T_lambda _ -> ()

  and expand_spine term spine =
    match spine with
    | S_hole -> term
    | S_apply { head; arg } -> (
        match t_head term with
        | T_flex -> _
        | T_subst _ -> _
        | T_var -> _
        | T_lambda _ -> _
        | T_apply _ -> _)

  let t_repr : term -> term = _
  let t_at term = _
  let t_same : term -> term -> bool = _

  let t_link received expected =
    let received = t_repr received in
    let expected = t_repr expected in
    match Level.(t_at received <= t_at expected) with
    | true ->
        let (Term expected) = expected in
        expected.link <- received
    | false ->
        let (Term received) = received in
        received.link <- expected

  let t_link term ~to_ = _

  let rec unify received expected =
    match t_same received expected with
    | true -> ()
    | false -> (
        t_link received ~to_:expected;
        expand received;
        expand expected;
        match t_same received expected with
        | true -> ()
        | false -> unify_head received expected)

  and unify_flex flex spine to_ =
    match spine with
    | S_hole -> _
    | S_apply { head; arg } -> ( match is_var arg with true -> _ | false -> _)

  and unify_head received expected =
    let received_head = t_head received in
    let expected_head = t_head expected in
    match (received_head, expected_head) with
    | T_flex, _ -> t_link received ~to_:expected
    | _, T_flex -> t_link expected ~to_:received
    | T_subst { flex; spine }, _ -> unify_flex flex spine expected
    | _, T_subst { flex; spine } -> unify_flex flex spine received
    | T_lambda _, _ | _, T_lambda _ ->
        t_link received ~to_:expected;
        let arg = _ in
        (* TODO: creating nodes here *)
        let received = apply received arg in
        let expected = apply expected arg in
        unify received expected
    | ( T_apply { funct = received_funct; arg = received_arg },
        T_apply { funct = expected_funct; arg = expected_arg } ) ->
        unify_pair (received_funct, received_arg) (expected_funct, expected_arg)
    | (T_var | T_apply _), (T_var | T_apply _) -> failwith "unify: clash"

  and unify_pair (received_fst, received_snd) (expected_fst, expected_snd) = _

  let t_type : term = _
  let t_self : term -> term = _
  let t_pi : term -> term -> term = _
  let t_apply : term -> term -> term = _
  let match_pi type_ : term * term = _
end

module Kernel_tree = struct
  type term = Term of { head : term_head; loc : Location.t }

  and term_head =
    (* (_ : A = N) *)
    | T_annot of { annot : term; term : term }
    | T_var of { var : var }
    | T_let of { bound : var; arg : term; body : term }
    | T_pi of { param : term; body : term }
    | T_lambda of { bound : var; body : term }
    | T_apply of { funct : term; arg : term }
    | T_self of { body : term }
    | T_fix of { bound : var; body : term }

  and var = Var of { to_ : value }
  and value = Value of { core : Core_tree.term; type_ : Core_tree.term }

  let unify received expected =
    let (Value { core = received_core; type_ = received_type }) = received in
    let (Value { core = expected_core; type_ = expected_type }) = expected in
    Core_tree.unify_pair
      (received_type, received_core)
      (expected_type, expected_core)

  let wrap ~type_ core = Value { core; type_ }

  let typeof value =
    let (Value { core = _; type_ }) = value in
    Value { core = type_; type_ = Core_tree.t_type }

  let eval var =
    let (Var { to_ }) = var in
    to_

  let v_self body =
    let open Core_tree in
    let (Value { core = body_core; type_ = body_type }) = body in
    unify t_type body_type;
    wrap ~type_:t_type (t_self body_core)

  let v_pi param body =
    let open Core_tree in
    let (Value { core = param_core; type_ = param_type }) = param in
    let (Value { core = body_core; type_ = body_type }) = body in
    unify t_type param_type;
    unify t_type body_type;
    wrap ~type_:t_type (t_pi param_core body_core)

  let v_apply funct arg =
    let open Core_tree in
    let (Value { core = funct_core; type_ = funct_type }) = funct in
    let (Value { core = arg_core; type_ = arg_type }) = arg in
    let param_type, body_type = match_pi funct_type in
    unify arg_type param_type;
    let type_ = t_apply body_type arg_core in
    wrap ~type_ (t_apply funct_core arg_core)

  let match_self self =
    let body = flex _ in
    unify (v_self body) self;
    body

  let match_pi self =
    let param = flex _ in
    let body = flex _ in
    unify (v_pi param body) self;
    (param, body)

  let match_apply self =
    let funct = flex _ in
    let arg = flex _ in
    unify (apply funct arg) self;
    (funct, arg)

  let unfold self =
    let open Core_tree in
    let (Value { core; type_ }) = self in
    let body_type = match_self type_ in
    let type_ = t_apply body_type core in
    wrap ~type_ (unfold core)
end

module Type_check = struct
  open Kernel_tree

  let rec check term self =
    let (Term { head; loc }) = term in
    match head with
    | T_annot { annot; term } ->
        check annot (typeof self);
        check term self
    | T_var { var } -> unify (eval var) self
    | T_let { bound; arg; body } ->
        check arg (eval bound);
        check body self
    | T_pi { param; body } ->
        let param_self, body_self = match_pi self in
        check param param_self;
        check body body_self
    | T_lambda { bound; body } ->
        let body_self = v_apply self (eval bound) in
        check body body_self
    | T_apply { funct; arg } ->
        let funct_self, arg_self = match_apply self in
        check funct funct_self;
        check arg arg_self
    | T_self { body } ->
        let body_self = match_self self in
        check body body_self
    | T_fix { bound; body } ->
        unify (eval bound) self;
        check body (unfold self)
end

module Elaborate = struct
  open Syntax_tree
  open Kernel_tree

  type context

  let with_def : context -> Syntax_tree.var -> _ = _
  let lookup : context -> Syntax_tree.var -> var = _

  type pat = Pat of { head : pat_head; loc : Location.t }

  and pat_head =
    | P_annot of { bound : pat; annot : term }
    | P_var of { var : var }
    | P_apply of { funct : pat; arg : pat }

  let p_wrap loc head = Pat { head; loc }
  let rec expand_lambda_annot bound annot body = _

  let rec expand_lambda bound body =
    let (Pat { head; loc }) = bound in
    match head with
    | P_annot { bound; annot } ->
        let bound, annot = expand_self bound body in
        _
    | P_var { var } -> t_lambda var body
    | P_apply { funct; arg } -> _

  and expand_self bound body =
    let (Pat { head; loc }) = bound in
    match head with
    | P_annot { bound; annot } ->
        let body = t_unify annot body in
        expand_self bound body
    | P_var { var } -> _
    | P_apply { funct; arg } -> _

  let rec expand_fix bound body =
    let (Pat { head; loc }) = bound in
    match head with
    | P_annot { bound; annot } ->
        let body = t_annot ~annot body in
        expand_fix bound body
    | P_var { var } -> _
    | P_apply { funct; arg } ->
        let body = expand_lambda arg body in
        _

  let rec expand_let bound arg = _
  let t_wrap loc head = Term { head; loc }

  let rec elab_term ctx term =
    let (Term { head; loc }) = (term : Syntax_tree.term) in
    match head with
    | T_var { var } ->
        let var = lookup ctx var in
        t_wrap loc (T_var { var })
    | T_block { block; return } -> elab_block ctx block return
    | T_apply { funct; arg } ->
        let funct = elab_term ctx funct in
        let arg = elab_term ctx arg in
        t_wrap loc (T_apply { funct; arg })
    | T_self { bound; body } ->
        let inner_ctx, _outer_ctx, bound = elab_pat ctx bound in
        let body = elab_term inner_ctx body in
        let body = expand_self bound body in
        t_wrap loc (T_self { body })
    | T_fix { bound; body } ->
        let x = _ in
        _
    | T_unit | T_sigma _ | T_pair _ | T_box _ -> failwith "not implemented"

  and elab_pat ctx bound =
    let (Pat { head; loc }) = (bound : Syntax_tree.pat) in
    match head with
    | P_annot { pat; annot } ->
        let inner_ctx, outer_ctx, bound = elab_pat ctx pat in
        let annot = elab_term inner_ctx annot in
        (inner_ctx, outer_ctx, p_wrap loc (P_annot { bound; annot }))
    | P_var { var } ->
        let ctx = _ in
        (ctx, ctx, p_wrap loc (P_var { var }))
    | P_apply { funct; arg } ->
        let inner_ctx, outer_ctx, funct = elab_pat ctx funct in
        let inner_ctx, _arg_ctx, arg = elab_pat outer_ctx arg in
        (inner_ctx, outer_ctx, p_wrap loc (P_apply { funct; arg }))
    | P_unit | P_pair _ | P_box _ -> failwith "not implemented"

  and elab_block ctx block return =
    let (Block { head; loc }) = (block : Syntax_tree.block) in
    match head with
    | B_hole -> elab_term ctx return
    | B_fix { bound; arg; body } ->
        let inner_ctx, outer_ctx, bound = elab_pat ctx bound in
        let arg = elab_term inner_ctx arg in
        let body = elab_block outer_ctx body return in
        let bound, arg = expand_let bound arg in
        t_wrap loc (T_let { bound; arg; body })
    | B_def _ -> failwith "not implemented"
end
