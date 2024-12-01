open Utils

type funct_ptr = Level.t
type var = Level.t
type field = int

type block =
  | B_let of { annot : var; arg : code; next : block }
  | B_unify of { received : var; expected : var; next : block }
  | B_return of { code : code }

and code =
  | C_var of { var : var }
  | C_apply of { funct : var; arg : var }
  | C_lambda of { body : block }
  | C_forall of { param : var; body : block }
  | C_self of { body : block }

module Value : sig
  type value =
    | V_hole of { hole : hole }
    | V_var of { var : var; args : value list }
    | V_lambda of { env : env; body : block }
    | V_univ
    | V_forall of { param : value; env : env; body : block }
    | V_self of { env : env; body : block }
    | V_thunk of { mutable funct_or_value : value; mutable arg_or_tag : value }

  and hole = { mutable at : Level.t; mutable link : value }
  and env

  (* constructors *)
  val v_univ : value
  val v_skolem : var:Level.t -> value

  (* env *)
  val append : env -> value -> env
  val lookup : env -> var -> value

  (* value *)
  val same : value -> value -> bool
  val same_hole : hole -> hole -> bool
  val repr : value -> value

  (* eval *)
  val eval_lazy : env -> code -> value
  val eval_block : env -> block -> value
  val eval_code : env -> code -> value
  val force_head : value -> value
end = struct
  type value =
    | V_hole of { hole : hole }
    | V_var of { var : var; args : value list }
    | V_lambda of { env : env; body : block }
    (* TODO: is univ actually needed or useful here? *)
    | V_univ
    | V_forall of { param : value; env : env; body : block }
    | V_self of { env : env; body : block }
    | V_thunk of { mutable funct_or_value : value; mutable arg_or_tag : value }

  and hole = { mutable at : Level.t; mutable link : value }

  (* TODO: next vs at *)
  and env = { values : value Level.Map.t; next : Level.t }

  let v_univ = V_univ
  let v_skolem ~var = V_var { var; args = [] }

  let append env value =
    let { values; next } = env in
    let values = Level.Map.add next value values in
    let next = Level.next next in
    { values; next }

  let lookup env var =
    let { values; next = _ } = env in
    match Level.Map.find_opt var values with
    | Some value -> value
    | None -> failwith "lookup: not found"

  let same (left : value) (right : value) = left == right
  let same_hole (left : hole) (right : hole) = left == right
  let l_null = Level.zero
  let v_null = V_var { var = l_null; args = [] }
  let is_v_null value = same value v_null

  (* TODO: path compression *)
  let rec repr value =
    match value with
    | V_hole { hole } -> (
        let { at = _; link } = hole in
        match is_v_null link with true -> value | false -> repr link)
    | V_var _ | V_lambda _ | V_univ | V_forall _ | V_self _ | V_thunk _ -> value

  (* TODO: inline repr? *)
  let repr value =
    match value with
    | V_hole { hole } -> (
        let { at = _; link } = hole in
        match is_v_null link with
        | true -> value
        | false ->
            let value = repr link in
            hole.link <- value;
            value)
    | V_var _ | V_lambda _ | V_univ | V_forall _ | V_self _ | V_thunk _ -> value

  (* TODO: this is slightly duplicated from eval *)
  let eval_lazy env code =
    match code with
    | C_var { var } -> repr @@ lookup env var
    | C_apply { funct; arg } ->
        let funct_or_value = lookup env funct in
        let arg_or_tag = lookup env arg in
        V_thunk { funct_or_value; arg_or_tag }
    | C_lambda { body } -> V_lambda { env; body }
    | C_forall { param; body } ->
        let param = lookup env param in
        V_forall { param; env; body }
    | C_self { body } -> V_self { env; body }

  let rec eval_block env block =
    match block with
    | B_let { annot = _; arg; next } ->
        let arg = eval_code env arg in
        let env = append env arg in
        eval_block env next
    | B_unify { received = _; expected = _; next } -> eval_block env next
    | B_return { code } -> eval_code env code

  and eval_code env code =
    match code with
    | C_var { var } -> force_head @@ lookup env var
    | C_apply { funct; arg } ->
        let funct = lookup env funct in
        let arg = lookup env arg in
        eval_apply funct arg
    | C_lambda { body } -> V_lambda { env; body }
    | C_forall { param; body } ->
        let param = lookup env param in
        V_forall { param; env; body }
    | C_self { body } -> V_self { env; body }

  and eval_apply funct arg =
    match force_head @@ funct with
    | V_var { var; args } ->
        let args = arg :: args in
        V_var { var; args }
    | V_lambda { env; body } ->
        let env = append env arg in
        eval_block env body
    | V_hole _ | V_univ | V_forall _ | V_self _ ->
        failwith "eval_apply: type mismatch"
    | V_thunk _ -> failwith "eval_apply: thunk reached"

  and force_head value =
    let value = repr value in
    match value with
    | V_hole _ | V_var _ | V_lambda _ | V_univ | V_forall _ | V_self _ -> value
    | V_thunk ({ funct_or_value; arg_or_tag } as thunk) -> (
        match is_v_null arg_or_tag with
        | true ->
            (* TODO: path compression *)
            force_head funct_or_value
        | false ->
            let value = eval_apply funct_or_value arg_or_tag in
            thunk.funct_or_value <- value;
            thunk.arg_or_tag <- v_null;
            value)
end

module Unify = struct
  open Value

  let rec unify_check ~at ~hole in_ =
    (* TODO: force_head? Why? *)
    let in_ = force_head in_ in
    match in_ with
    | V_hole { hole = in_ } -> (
        match same_hole hole in_ with
        | true -> failwith "occurs check"
        | false ->
            (* TODO: is this lowering correct? *)
            in_.at <- Level.min in_.at at)
    | V_var { var; args } ->
        (* TODO: poly comparison *)
        (match var >= at with true -> failwith "escape check" | false -> ());
        List.iter (fun in_ -> unify_check ~at ~hole in_) args
    | V_univ -> ()
    | V_lambda { env; body } -> unify_check_under ~at ~hole env body
    | V_forall { param; env; body } ->
        unify_check ~at ~hole param;
        unify_check_under ~at ~hole env body
    | V_self { env; body } -> unify_check_under ~at ~hole env body
    | V_thunk _ -> failwith "unify_check: thunk reacehed"

  and unify_check_under ~at ~hole env body =
    let skolem = v_skolem ~var:at in
    let at = Level.next at in
    let body =
      let env = append env skolem in
      eval_block env body
    in
    unify_check ~at ~hole body

  let rec unify_hole ~at ~hole ~to_ =
    let to_ = force_head to_ in
    match to_ with
    | V_hole { hole = to_ } -> (
        match same_hole hole to_ with true -> () | false -> _)
    | V_var _ | V_lambda _ | V_univ | V_forall _ | V_self _ | V_thunk _ ->
        unify_check ~at ~hole to_;
        hole.link <- to_

  let rec unify ~at lhs rhs =
    let lhs = repr lhs in
    let rhs = repr rhs in
    match same lhs rhs with true -> () | false -> unify_struct ~at lhs rhs

  and unify_struct ~at lhs rhs =
    let lhs = force_head lhs in
    let rhs = force_head rhs in
    match (lhs, rhs) with
    | V_hole { hole }, to_ -> unify_hole ~at ~hole ~to_
    | to_, V_hole { hole } -> unify_hole ~at ~hole ~to_
    | V_var { var = lhs; args = lhs_args }, V_var { var = rhs; args = rhs_args }
      ->
        (match Level.equal lhs rhs with
        | true -> ()
        | false -> failwith "var clash");
        unify_args ~at lhs_args rhs_args
    | ( V_lambda { env = lhs_env; body = lhs_body },
        V_lambda { env = rhs_env; body = rhs_body } ) ->
        unify_under ~at lhs_env lhs_body rhs_env rhs_body
    | V_univ, V_univ -> ()
    | ( V_forall { param = lhs_param; env = lhs_env; body = lhs_body },
        V_forall { param = rhs_param; env = rhs_env; body = rhs_body } ) ->
        unify ~at lhs_param rhs_param;
        unify_under ~at lhs_env lhs_body rhs_env rhs_body
    | _, _ -> _

  and unify_args ~at lhs_args rhs_args =
    (* TODO: iter2 clash *)
    (* TODO: this can happen for sink types *)
    List.iter2 (fun lhs rhs -> unify ~at lhs rhs) lhs_args rhs_args

  and unify_under ~at lhs_env lhs_body rhs_env rhs_body =
    let skolem = v_skolem ~var:at in
    let at = Level.next at in
    let lhs =
      let lhs_env = append lhs_env skolem in
      eval_block lhs_env lhs_body
    in
    let rhs =
      let rhs_env = append rhs_env skolem in
      eval_block rhs_env rhs_body
    in
    unify ~at lhs rhs

  let unify ~at ~received ~expected = unify ~at received expected
  let split_forall : value -> value * env * block = assert false
  let subst : arg:value -> env -> block -> value = assert false
end

module Check = struct
  open Value
  open Unify

  let unify : env -> received:var -> expected:var -> unit = assert false
  let enter : env -> env = assert false

  let rec check_block env block =
    match block with
    | B_let { annot = _; arg; next } ->
        let () = check_code env arg in
        let arg = eval_code env arg in
        let env = append env arg in
        check_block env next
    | B_unify { received; expected; next } ->
        unify env ~received ~expected;
        check_block env next
    | B_return { code } -> check_code env code

  and check_code env code =
    match code with
    | C_var { var = _ } -> ()
    | C_apply { funct = _; arg = _ } -> ()
    | C_lambda { body } | C_forall { param = _; body } | C_self { body } ->
        let env = enter env in
        check_block env body
end

module Typer = struct
  open Value
  open Unify

  type context

  let enter : context -> value -> context = assert false
  let inst : context -> var -> value = assert false

  (* let inst : self:var -> type_:value -> value = assert false *)
  let rec check_block ctx env ~at block ~self ~expected : unit =
    match block with
    | B_let { annot; arg; next } ->
        let annot = lookup env annot in
        let () =
          let self = Some at in
          let x = assert false in
          check_code ctx env ~at arg ~self ~expected:annot
        in
        let arg = eval_lazy env arg in
        let ctx = enter ctx expected in
        let env = append env arg in
        let at = Level.next at in
        check_block ctx env ~at next ~self ~expected
    | B_return { code } -> check_code ctx env ~at code ~self ~expected

  and check_code ctx env ~at code ~self ~expected =
    match code with
    | C_var { var } ->
        (* TODO: which self to use here? *)
        let received = inst ctx var in
        unify ~at ~received ~expected
    | C_apply { funct; arg } ->
        let funct = inst ctx funct in
        let param, expected_env, expected_body = split_forall funct in
        let () =
          let arg_type = inst ctx arg in
          unify ~at ~received:arg_type ~expected:param
        in
        let received =
          let arg = lookup env arg in
          subst ~arg expected_env expected_body
        in
        unify ~at ~received ~expected
    | C_lambda { body } ->
        let param, expected_env, expected_body = split_forall expected in
        let arg = v_skolem ~var:at in
        let ctx = enter ctx param in
        let env = append env arg in
        let at = Level.next at in
        let expected = subst ~arg expected_env expected_body in
        let self =
          match self with
          | Some self ->
              let self = eval_apply ~self ~arg in
              Some self
          | None -> None
        in
        check_block ctx env ~at body ~self ~expected
    | C_forall { param; body } ->
        unify ~at ~received:v_univ ~expected;
        let () =
          let param_type = inst ctx param in
          unify ~at ~received:param_type ~expected:v_univ
        in
        let param = lookup env param in
        let arg = v_skolem ~var:at in
        let ctx = enter ctx param in
        let env = append env arg in
        let at = Level.next at in
        let self = None in
        check_block ctx env ~at body ~self ~expected
end

type context

let repr : value -> value = assert false
let same : value -> value -> bool = assert false

let rec check_block (ctx : context) block ~self ~expected =
  match block with
  | B_let { annot; arg; next } ->
      let () =
        let annot = lookup ctx annot in
        assert false
      in
      let () = check_code ctx arg ~self ~expected in
      let ctx = append ctx arg in
      check_block ctx next ~self ~expected
  | B_return { code } -> check_code ctx code ~self ~expected

and check_code ctx code ~self ~expected =
  match code with
  | C_var { var } ->
      let received = inst ctx var in
      unify ~received ~expected
  | C_apply { funct; arg } ->
      let funct = inst ctx funct in
      let param, expected_env, expected_body = split_forall funct in
      let () =
        let arg_type = inst ctx arg in
        unify ~received:arg_type ~expected:param
      in
      let received = subst ~arg expected_env expected_body in
      unify ~received ~expected
  | C_lambda { body } ->
      let param, expected_env, expected_body = split_forall expected in
      let ctx = enter_opaque ctx in
      let arg = assert false in
      let expected = subst ~arg expected_env expected_body in
      let self = eval_apply in
      check_block ctx body ~self ~expected
  | C_forall { param; body } ->
      unify ~received:v_univ ~expected;
      let () =
        let param_type = inst ctx param in
        unify ~received:param_type ~expected:v_univ
      in
      (* TODO: this *)
      let ctx = enter_opaque ctx in
      let self = None in
      check_block ctx body ~self ~expected
