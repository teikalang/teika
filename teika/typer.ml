open Ttree

module Subst : sig
  val subst_term : from:Var.t -> to_:term_desc -> term -> term
  val subst_type : from:Var.t -> to_:term_desc -> type_ -> type_
  val subst_desc : from:Var.t -> to_:term_desc -> term_desc -> term_desc
  val subst_annot : from:Var.t -> to_:term_desc -> annot -> annot
  val subst_bind : from:Var.t -> to_:term_desc -> bind -> bind
end = struct
  let rec subst_term ~from ~to_ term =
    let (TTerm { loc; desc; type_ }) = term in
    let desc = subst_desc ~from ~to_ desc in
    TTerm { loc; desc; type_ }

  and subst_type ~from ~to_ type_ =
    let (TType { loc; desc }) = type_ in
    let desc = subst_desc ~from ~to_ desc in
    TType { loc; desc }

  and subst_annot ~from ~to_ annot =
    let (TAnnot { loc; var; annot }) = annot in
    let annot = subst_type ~from ~to_ annot in
    (var, tannot loc ~var ~annot)

  and subst_bind ~from ~to_ bind =
    let (TBind { loc; var; value }) = bind in
    let value = subst_term ~from ~to_ value in
    (var, tbind loc ~var ~value)

  and subst_desc ~from ~to_ desc =
    let subst_term term = subst_term ~from ~to_ term in
    let subst_type type_ = subst_type ~from ~to_ type_ in
    let subst_annot annot = subst_annot ~from ~to_ annot in
    let subst_bind bind = subst_bind ~from ~to_ bind in
    match desc with
    | TT_var { var } -> (
        match Var.equal from var with true -> to_ | false -> TT_var { var })
    | TT_forall { param; return } ->
        let var, param = subst_annot param in
        let return =
          match Var.equal from var with
          | true -> return
          | false -> subst_type return
        in
        TT_forall { param; return }
    | TT_lambda { param; return } ->
        let var, param = subst_annot param in
        let return =
          match Var.equal from var with
          | true -> return
          | false -> subst_term return
        in
        TT_lambda { param; return }
    | TT_apply { lambda; arg } ->
        let lambda = subst_term lambda in
        let arg = subst_term arg in
        TT_apply { lambda; arg }
    | TT_exists { left; right } ->
        let var, left = subst_annot left in
        let right =
          match Var.equal from var with
          | true -> right
          | false ->
              let _var, right = subst_annot right in
              right
        in
        TT_exists { left; right }
    | TT_pair { left; right } ->
        let var, left = subst_bind left in
        let right =
          match Var.equal from var with
          | true -> right
          | false ->
              let _var, right = subst_bind right in
              right
        in
        TT_pair { left; right }
    | TT_unpair { left; right; pair; return } ->
        let pair = subst_term pair in
        let return =
          (* TODO: is this guard needed? *)
          match Var.equal from left || Var.equal from right with
          | true -> return
          | false -> subst_term return
        in
        TT_unpair { left; right; pair; return }
    | TT_let { bound; return } ->
        let var, bound = subst_bind bound in
        let return =
          match Var.equal from var with
          | true -> return
          | false -> subst_term return
        in
        TT_let { bound; return }
    | TT_annot { value; annot } ->
        let annot = subst_type annot in
        let value = subst_term value in
        TT_annot { value; annot }

  let subst_annot ~from ~to_ annot =
    let _var, annot = subst_annot ~from ~to_ annot in
    annot

  let subst_bind ~from ~to_ bind =
    let _var, bind = subst_bind ~from ~to_ bind in
    bind
end

module Normalize : sig
  val normalize_term : term -> term
  val normalize_type : type_ -> type_
end = struct
  open Subst

  let rec normalize_term term =
    let (TTerm { loc; desc; type_ }) = term in
    let type_ = normalize_type type_ in
    let desc = normalize_desc desc in
    TTerm { loc; desc; type_ }

  and normalize_type type_ =
    let (TType { loc; desc }) = type_ in
    let desc = normalize_desc desc in
    TType { loc; desc }

  and normalize_annot annot =
    let (TAnnot { loc; var; annot }) = annot in
    let annot = normalize_type annot in
    tannot loc ~var ~annot

  and normalize_bind bind =
    let (TBind { loc; var; value }) = bind in
    let value = normalize_term value in
    let (TTerm { loc = _; desc = value_desc; type_ = _ }) = value in
    (var, value_desc, tbind loc ~var ~value)

  and normalize_desc desc =
    match desc with
    | TT_var { var } -> TT_var { var }
    | TT_forall { param; return } ->
        let param = normalize_annot param in
        let return = normalize_type return in
        TT_forall { param; return }
    | TT_lambda { param; return } ->
        let param = normalize_annot param in
        let return = normalize_term return in
        TT_lambda { param; return }
    | TT_apply { lambda; arg } -> (
        let lambda = normalize_term lambda in
        let arg = normalize_term arg in
        match
          let (TTerm { loc = _; desc; type_ = _ }) = lambda in
          desc
        with
        | TT_lambda { param; return } ->
            let (TTerm { loc = _; desc = return; type_ = _ }) = return in
            let (TTerm { loc = _; desc = arg; type_ = _ }) = arg in
            let (TAnnot { loc = _; var; annot = _ }) = param in
            normalize_desc @@ subst_desc ~from:var ~to_:arg return
        | _ -> TT_apply { lambda; arg })
    | TT_exists { left; right } ->
        let left = normalize_annot left in
        let right = normalize_annot right in
        TT_exists { left; right }
    | TT_pair { left; right } ->
        let var, desc, left = normalize_bind left in
        let right = subst_bind ~from:var ~to_:desc right in
        let _var, _desc, right = normalize_bind right in
        TT_pair { left; right }
    | TT_unpair { left; right; pair; return } -> (
        let pair = normalize_term pair in
        let return = normalize_term return in
        match
          let (TTerm { loc = _; desc; type_ = _ }) = pair in
          desc
        with
        | TT_pair { left = left_bind; right = right_bind } ->
            let (TBind { loc = _; var = _; value = left_value }) = left_bind in
            let (TTerm { loc = _; desc = left_desc; type_ = _ }) = left_value in
            let (TBind { loc = _; var = _; value = right_value }) =
              right_bind
            in
            let (TTerm { loc = _; desc = right_desc; type_ = _ }) =
              right_value
            in
            let (TTerm { loc = _; desc = return; type_ = _ }) = return in
            let return = subst_desc ~from:left ~to_:left_desc return in
            subst_desc ~from:right ~to_:right_desc return
        | _ -> TT_unpair { left; right; pair; return })
    | TT_let { bound; return } ->
        let var, desc, _bound = normalize_bind bound in
        let (TTerm { loc = _; desc = return; type_ = _ }) = return in
        normalize_desc @@ subst_desc ~from:var ~to_:desc return
    | TT_annot { value; annot = _ } ->
        let (TTerm { loc = _; desc = value; type_ = _ }) = value in
        normalize_desc value
end

module Unify : sig
  val unify_term : expected:term -> received:term -> unit Context.t
  val unify_type : expected:type_ -> received:type_ -> unit Context.t
end = struct
  open Normalize
  open Context

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
  let rec occurs_and_escape_check_term ~hole_level ~hole_var ~in_ =
    let (TTerm { loc = _; desc; type_ }) = in_ in
    (* TODO: why is this needed? *)
    occurs_and_escape_check_type ~hole_level ~hole_var ~in_:type_ >>= fun () ->
    occurs_and_escape_check_desc ~hole_level ~hole_var ~in_:desc

  and occurs_and_escape_check_type ~hole_level ~hole_var ~in_ =
    let (TType { loc = _; desc }) = in_ in
    occurs_and_escape_check_desc ~hole_level ~hole_var ~in_:desc

  and occurs_and_escape_check_annot ~hole_level ~hole_var ~in_ f =
    let (TAnnot { loc = _; var; annot }) = in_ in
    occurs_and_escape_check_type ~hole_level ~hole_var ~in_:annot >>= fun () ->
    (* TODO: is region needed here? *)
    with_region @@ fun () -> with_var_rigid var f

  and occurs_and_escape_check_bind ~hole_level ~hole_var ~in_ f =
    let (TBind { loc = _; var; value }) = in_ in
    occurs_and_escape_check_term ~hole_level ~hole_var ~in_:value >>= fun () ->
    (* TODO: is region needed here? *)
    with_region @@ fun () -> with_var_rigid var f

  and occurs_and_escape_check_desc ~hole_level ~hole_var ~in_ =
    match in_ with
    | TT_var { var } ->
        occurs_and_escape_check_var ~hole_level ~hole_var ~in_:var
    | TT_forall { param; return } ->
        occurs_and_escape_check_annot ~hole_level ~hole_var ~in_:param
        @@ fun () ->
        occurs_and_escape_check_type ~hole_level ~hole_var ~in_:return
    | TT_lambda { param; return } ->
        occurs_and_escape_check_annot ~hole_level ~hole_var ~in_:param
        @@ fun () ->
        occurs_and_escape_check_term ~hole_level ~hole_var ~in_:return
    | TT_apply { lambda; arg } ->
        occurs_and_escape_check_term ~hole_level ~hole_var ~in_:lambda
        >>= fun () ->
        occurs_and_escape_check_term ~hole_level ~hole_var ~in_:arg
    | TT_exists { left; right } ->
        occurs_and_escape_check_annot ~hole_level ~hole_var ~in_:left
        @@ fun () ->
        occurs_and_escape_check_annot ~hole_level ~hole_var ~in_:right
        @@ fun () -> return ()
    | TT_pair { left; right } ->
        occurs_and_escape_check_bind ~hole_level ~hole_var ~in_:left
        @@ fun () ->
        occurs_and_escape_check_bind ~hole_level ~hole_var ~in_:right
        @@ fun () -> return ()
    | TT_unpair { left; right; pair; return } ->
        occurs_and_escape_check_term ~hole_level ~hole_var ~in_:pair
        >>= fun () ->
        (* TODO: is region needed here? *)
        with_region @@ fun () ->
        with_var_rigid left @@ fun () ->
        (* TODO: is region needed here? *)
        with_region @@ fun () ->
        with_var_rigid right @@ fun () ->
        occurs_and_escape_check_term ~hole_level ~hole_var ~in_:return
    | TT_let { bound; return } ->
        occurs_and_escape_check_bind ~hole_level ~hole_var ~in_:bound
        @@ fun () ->
        occurs_and_escape_check_term ~hole_level ~hole_var ~in_:return
    | TT_annot { value; annot } ->
        occurs_and_escape_check_type ~hole_level ~hole_var ~in_:annot
        >>= fun () ->
        occurs_and_escape_check_term ~hole_level ~hole_var ~in_:value

  and occurs_and_escape_check_var ~hole_level ~hole_var ~in_ =
    match Var.equal hole_var in_ with
    | true -> fail_occurs_check hole_var ~in_
    | false -> (
        repr in_ >>= function
        | V_rigid { level } -> (
            match Level.(hole_level < level) with
            | true -> fail_escape_check in_ ~to_:hole_var
            | false -> return ())
        | V_alias { var = in_ } ->
            occurs_and_escape_check_var ~hole_level ~hole_var ~in_
        | V_link { type_ = in_ } ->
            occurs_and_escape_check_desc ~hole_level ~hole_var ~in_
        | V_hole { level } -> (
            match Level.(hole_level < level) with
            | true -> var_lower in_ ~to_:hole_level
            | false -> return ()))

  let rec unify_term ~expected ~received =
    (* TODO: use those locations for something? *)
    let (TTerm { loc = _; desc = expected_desc; type_ = expected_type }) =
      expected
    in
    let (TTerm { loc = _; desc = received_desc; type_ = received_type }) =
      received
    in
    (* TODO: why is this needed? *)
    unify_type ~expected:expected_type ~received:received_type >>= fun () ->
    unify_desc ~expected:expected_desc ~received:received_desc

  and unify_type ~expected ~received =
    (* TODO: use those locations for something? *)
    let (TType { loc = _; desc = expected }) = expected in
    let (TType { loc = _; desc = received }) = received in
    unify_desc ~expected ~received

  and unify_annot ~expected ~received f =
    let (TAnnot { loc = _; var = expected_var; annot = expected }) = expected in
    let (TAnnot { loc = _; var = received_var; annot = received }) = received in
    unify_type ~expected ~received >>= fun () ->
    with_region @@ fun () ->
    with_var_rigid expected_var @@ fun () ->
    with_var_alias received_var ~of_:expected_var f

  and unify_bind ~expected ~received f =
    let (TBind { loc = _; var = expected_var; value = expected }) = expected in
    let (TBind { loc = _; var = received_var; value = received }) = received in
    unify_term ~expected ~received >>= fun () ->
    with_region @@ fun () ->
    with_var_rigid expected_var @@ fun () ->
    with_var_alias received_var ~of_:expected_var f

  and unify_desc ~expected ~received =
    match (expected, received) with
    | TT_var { var = expected }, _ -> unify_expected_var ~expected ~received
    | _, TT_var { var = received } -> unify_received_var ~expected ~received
    (* TODO: track whenever it is unified and locations, visualizing inference *)
    | ( TT_forall { param = expected_param; return = expected_return },
        TT_forall { param = received_param; return = received_return } ) ->
        unify_annot ~expected:received_param ~received:expected_param
        @@ fun () ->
        unify_type ~expected:expected_return ~received:received_return
    | ( TT_lambda { param = expected_param; return = expected_return },
        TT_lambda { param = received_param; return = received_return } ) ->
        unify_annot ~expected:received_param ~received:expected_param
        @@ fun () ->
        unify_term ~expected:expected_return ~received:received_return
    | ( TT_apply { lambda = expected_lambda; arg = expected_arg },
        TT_apply { lambda = received_lambda; arg = received_arg } ) ->
        unify_term ~expected:expected_lambda ~received:received_lambda
        >>= fun () -> unify_term ~expected:expected_arg ~received:received_arg
    | ( TT_exists { left = expected_left; right = expected_right },
        TT_exists { left = received_left; right = received_right } ) ->
        unify_annot ~expected:expected_left ~received:received_left @@ fun () ->
        unify_annot ~expected:expected_right ~received:received_right
        @@ fun () -> return ()
    | ( TT_pair { left = expected_left; right = expected_right },
        TT_pair { left = received_left; right = received_right } ) ->
        unify_bind ~expected:expected_left ~received:received_left @@ fun () ->
        unify_bind ~expected:expected_right ~received:received_right
        @@ fun () -> return ()
    | ( TT_unpair
          {
            left = expected_left;
            right = expected_right;
            pair = expected_pair;
            return = expected_return;
          },
        TT_unpair
          {
            left = received_left;
            right = received_right;
            pair = received_pair;
            return = received_return;
          } ) ->
        with_region @@ fun () ->
        with_var_rigid expected_left @@ fun () ->
        with_var_alias received_left ~of_:expected_left @@ fun () ->
        with_region @@ fun () ->
        with_var_rigid expected_right @@ fun () ->
        with_var_alias received_right ~of_:expected_right @@ fun () ->
        unify_term ~expected:expected_pair ~received:received_pair >>= fun () ->
        unify_term ~expected:expected_return ~received:received_return
    | ( TT_let { bound = expected_bound; return = expected_return },
        TT_let { bound = received_bound; return = received_return } ) ->
        unify_bind ~expected:expected_bound ~received:received_bound
        @@ fun () ->
        unify_term ~expected:expected_return ~received:received_return
    | ( TT_annot { value = expected_value; annot = expected_annot },
        TT_annot { value = received_value; annot = received_annot } ) ->
        unify_type ~expected:expected_annot ~received:received_annot
        >>= fun () ->
        unify_term ~expected:expected_value ~received:received_value
    | ( ( TT_forall _ | TT_lambda _ | TT_apply _ | TT_exists _ | TT_pair _
        | TT_unpair _ | TT_let _ | TT_annot _ ),
        ( TT_forall _ | TT_lambda _ | TT_apply _ | TT_exists _ | TT_pair _
        | TT_unpair _ | TT_let _ | TT_annot _ ) ) ->
        fail_type_clash ~expected ~received

  and unify_expected_var ~expected ~received =
    match received with
    | TT_var { var = received } -> unify_var ~expected ~received
    | TT_forall _ | TT_lambda _ | TT_apply _ | TT_exists _ | TT_pair _
    | TT_unpair _ | TT_let _ | TT_annot _ -> (
        repr expected >>= function
        | V_rigid { level = _ } -> fail_var_constrained expected ~by_:received
        | V_alias { var = expected } -> unify_expected_var ~expected ~received
        | V_link { type_ = expected } -> unify_desc ~expected ~received
        | V_hole { level = hole_level } ->
            occurs_and_escape_check_desc ~hole_level ~hole_var:expected
              ~in_:received
            >>= fun () -> var_link expected ~to_:received)

  and unify_received_var ~expected ~received =
    match expected with
    | TT_var { var = expected } -> unify_var ~expected ~received
    | TT_forall _ | TT_lambda _ | TT_apply _ | TT_exists _ | TT_pair _
    | TT_unpair _ | TT_let _ | TT_annot _ -> (
        repr received >>= function
        | V_rigid { level = _ } -> fail_var_constrained received ~by_:expected
        | V_alias { var = received } -> unify_received_var ~expected ~received
        | V_link { type_ = received } -> unify_desc ~expected ~received
        | V_hole { level = hole_level } ->
            occurs_and_escape_check_desc ~hole_level ~hole_var:received
              ~in_:expected
            >>= fun () -> var_link received ~to_:expected)

  and unify_var ~expected ~received =
    match Var.equal expected received with
    | true -> return ()
    | false -> (
        repr expected >>= fun expected_info ->
        repr received >>= fun received_info ->
        match (expected_info, received_info) with
        | V_rigid { level = _ }, V_rigid { level = _ } ->
            fail_var_clash ~expected ~received
        | V_alias { var = expected }, _ -> unify_var ~expected ~received
        | _, V_alias { var = received } -> unify_var ~expected ~received
        | V_link { type_ = expected }, _ ->
            unify_received_var ~expected ~received
        | _, V_link { type_ = received } ->
            unify_expected_var ~expected ~received
        (* TODO: is it okay when expected_level == received_level? *)
        | V_hole { level = expected_level }, V_hole { level = received_level }
          -> (
            match Level.(received_level < expected_level) with
            | true -> var_alias expected ~of_:received
            | false -> var_alias received ~of_:expected)
        | V_hole { level = expected_level }, V_rigid { level = received_level }
          -> (
            match Level.(expected_level < received_level) with
            | true -> fail_escape_check received ~to_:expected
            | false -> var_alias expected ~of_:received)
        | V_rigid { level = expected_level }, V_hole { level = received_level }
          -> (
            match Level.(received_level < expected_level) with
            | true -> fail_escape_check expected ~to_:received
            | false -> var_alias received ~of_:expected))

  let unify_term ~expected ~received =
    (* TODO: does it make sense to always normalize? *)
    let expected = normalize_term expected in
    let received = normalize_term received in
    unify_term ~expected ~received

  let unify_type ~expected ~received =
    let expected = normalize_type expected in
    let received = normalize_type received in
    unify_type ~expected ~received
end
