open Term

exception Var_clash of { expected : Var.t; received : Var.t }
exception Type_clash of { expected : term; received : term }
exception Not_a_function of { lambda : term }
exception Not_a_pair of { pair : term }

let rec subst ~from ~to_ term =
  let subst term = subst ~from ~to_ term in
  match term with
  | T_type { var = _ } -> t_type
  | T_var { var; type_ } -> (
      let type_ = subst type_ in
      match Var.equal var from with
      | true -> to_ ~type_
      | false -> t_var ~var ~type_)
  | T_arrow { var; param; return } ->
      let param = subst param in
      let return =
        match Var.equal var from with true -> return | false -> subst return
      in
      t_arrow ~var ~param ~return
  | T_lambda { var; param; return } ->
      let param = subst param in
      let return =
        match Var.equal var from with true -> return | false -> subst return
      in
      t_lambda ~var ~param ~return
  | T_apply { lambda; arg } ->
      let lambda = subst lambda in
      let arg = subst arg in
      t_apply ~lambda ~arg
  | T_sigma { var; left; right } ->
      let left = subst left in
      let right =
        match Var.equal var from with true -> right | false -> subst right
      in
      t_sigma ~var ~left ~right
  | T_pair { var; left; right; annot } ->
      let left = subst left in
      let right = subst right in
      let annot = if Var.equal var from then annot else subst annot in
      t_pair ~var ~left ~right ~annot
  | T_unpair { left; right; pair; return } ->
      let pair = subst pair in
      let return =
        match Var.equal left from || Var.equal right from with
        | true -> return
        | false -> subst return
      in
      t_unpair ~left ~right ~pair ~return

let rename ~from ~to_ term =
  subst ~from ~to_:(fun ~type_ -> t_var ~var:to_ ~type_) term

let expand ~from ~to_ term =
  subst ~from
    ~to_:(fun ~type_:_ ->
      (* TODO: check type? *)
      to_)
    term

let rec normalize term =
  match term with
  | T_type { var = _ } -> t_type
  | T_var { var; type_ } ->
      let type_ = normalize type_ in
      t_var ~var ~type_
  | T_arrow { var; param; return } ->
      let param = normalize param in
      let return = normalize return in
      t_arrow ~var ~param ~return
  | T_lambda { var; param; return } ->
      let param = normalize param in
      t_lambda ~var ~param ~return
  | T_apply { lambda; arg } -> (
      let lambda = normalize lambda in
      let arg = normalize arg in
      match lambda with
      | T_lambda { var; param = _; return } ->
          let return = expand ~from:var ~to_:arg return in
          normalize return
      (* TODO: primitives *)
      | T_var { var; type_ = _ } when Var.(equal var debug) ->
          Format.eprintf "debug: %a\n%!" Term.pp arg;
          arg
      | lambda -> t_apply ~lambda ~arg)
  | T_sigma { var; left; right } ->
      let left = normalize left in
      let right = normalize right in
      t_sigma ~var ~left ~right
  | T_pair { var; left; right; annot } ->
      let left = normalize left in
      let right = normalize right in
      let annot = normalize annot in
      t_pair ~var ~left ~right ~annot
  | T_unpair { left; right; pair; return } -> (
      let return = normalize return in
      match normalize pair with
      | T_pair { var = _; left = left_value; right = right_value; annot = _ } ->
          let return = expand ~from:left ~to_:left_value return in
          let return = expand ~from:right ~to_:right_value return in
          normalize return
      | pair -> t_unpair ~left ~right ~pair ~return)

let rec equal ~expected ~received =
  match (expected, received) with
  | T_type { var = expected }, T_type { var = received }
  | T_var { var = expected; type_ = _ }, T_var { var = received; type_ = _ } ->
      if Var.equal expected received then ()
      else raise (Var_clash { expected; received })
  | ( T_arrow
        { var = expected_var; param = expected_param; return = expected_return },
      T_arrow
        { var = received_var; param = received_param; return = received_return }
    )
  | ( T_lambda
        { var = expected_var; param = expected_param; return = expected_return },
      T_lambda
        { var = received_var; param = received_param; return = received_return }
    ) ->
      equal ~expected:received_param ~received:expected_param;

      let received_return =
        rename ~from:received_var ~to_:expected_var received_return
      in
      equal ~expected:expected_return ~received:received_return
  | ( T_apply { lambda = expected_lambda; arg = expected_arg },
      T_apply { lambda = received_lambda; arg = received_arg } ) ->
      equal ~expected:expected_lambda ~received:received_lambda;
      equal ~expected:expected_arg ~received:received_arg
  | ( T_sigma
        { var = expected_var; left = expected_left; right = expected_right },
      T_sigma
        { var = received_var; left = received_left; right = received_right } )
  | ( T_pair
        {
          var = expected_var;
          left = expected_left;
          right = _;
          annot = expected_right;
        },
      T_pair
        {
          var = received_var;
          left = received_left;
          right = _;
          annot = received_right;
        } ) ->
      equal ~expected:expected_left ~received:received_left;

      let received_right =
        rename ~from:received_var ~to_:expected_var received_right
      in
      equal ~expected:expected_right ~received:received_right
  | ( T_unpair
        {
          left = expected_left;
          right = expected_right;
          pair = expected_pair;
          return = expected_return;
        },
      T_unpair
        {
          left = received_left;
          right = received_right;
          pair = received_pair;
          return = received_return;
        } ) ->
      equal ~expected:expected_pair ~received:received_pair;

      let received_return =
        rename ~from:received_left ~to_:expected_left received_return
      in
      let received_return =
        rename ~from:received_right ~to_:expected_right received_return
      in
      equal ~expected:expected_return ~received:received_return
  | ( ( T_type _ | T_var _ | T_arrow _ | T_lambda _ | T_apply _ | T_sigma _
      | T_pair _ | T_unpair _ ),
      ( T_type _ | T_var _ | T_arrow _ | T_lambda _ | T_apply _ | T_sigma _
      | T_pair _ | T_unpair _ ) ) ->
      raise (Type_clash { expected; received })

let equal ~expected ~received =
  let expected = normalize expected in
  let received = normalize received in
  equal ~expected ~received

let rec typeof term =
  match term with
  | T_type { var = _ } -> t_type
  | T_var { var = _; type_ } -> type_
  | T_arrow { var = _; param = _; return = _ } -> t_type
  | T_lambda { var; param; return } ->
      let return = typeof return in
      t_arrow ~var ~param ~return
  | T_apply { lambda; arg } -> (
      match typeof lambda with
      | T_arrow { var; param = _; return } -> expand ~from:var ~to_:arg return
      | _ -> failwith "bug")
  | T_sigma { var = _; left = _; right = _ } -> t_type
  | T_pair { var; left; right = _; annot } ->
      (* TODO: is this right? *)
      let left = typeof left in
      t_sigma ~var ~left ~right:annot
  | T_unpair { left = _; right = _; pair = _; return } -> typeof return

let typeof term =
  let term = normalize term in
  typeof term

let apply ~lambda ~arg =
  match typeof lambda with
  | T_arrow { var = _; param; return = _ } ->
      let arg = typeof arg in
      equal ~expected:param ~received:arg
  | _ -> raise (Not_a_function { lambda })

let pair ~var ~left ~right ~annot =
  let right = typeof right in
  let annot = expand ~from:var ~to_:left annot in
  equal ~expected:annot ~received:right

let unpair ~left ~pair =
  let var, left_type, right_type =
    match
      let pair = typeof pair in
      normalize pair
    with
    | T_sigma { var; left; right } -> (var, left, right)
    | _ -> raise (Not_a_pair { pair })
  in
  let right_type = rename ~from:var ~to_:left right_type in
  (left_type, right_type)

let annot ~value ~type_ =
  let value = typeof value in
  equal ~expected:type_ ~received:value
