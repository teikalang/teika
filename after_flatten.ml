open Utils
open Ttree

type var = Level.t

(* TODO: field as int is bad? *)
type field = int

type block =
  | B_let of { arg : code; next : block }
  | B_hoist of { next : block }
  | B_fix of { var : var; arg : code; next : block }
  | B_return of { code : code }

(* control flow *)
and code =
  | C_var of { var : var }
  | C_apply of { funct : var; arg : var }
  | C_lambda of { body : block }
  | C_forall of { param : var; body : block }
  | C_prim of { prim : prim }

(* value operations *)
and prim =
  | P_type_stub
  | P_tuple of { elements : var array }
  | P_int32 of { lit : int32 }
  | P_string of { lit : string }
  | P_field of { tuple : var; field : field }
  | P_int32_add of { left : var; right : var }
  | P_int32_sub of { left : var; right : var }
  | P_int32_mul of { left : var; right : var }
  | P_string_length of { string : var }
  | P_string_concat of { left : var; right : var }
  | P_world_log of { world : var; message : var }

module Flatten = struct
  module Context : sig
    type 'a context
    type 'a t = 'a context

    val pure : 'a -> 'a context
    val ( let* ) : 'a context -> ('a -> 'b context) -> 'b context
    val solve : Index.t -> var context
    val let_or_var : code -> var context
    val enter_let : var:var -> (unit -> 'a context) -> 'a context
    val enter_hoist : (unit -> 'a context) -> 'a context
    val enter_fix : var:var -> arg:code -> (unit -> 'a context) -> 'a context
    val fork_body : (var -> code context) -> block context
  end = struct
    type block_entry =
      | BE_let of { arg : code }
      | BE_hoist
      | BE_fix of { var : Level.t; arg : code }

    type 'a context =
      env:Level.t list ->
      block:block_entry list ->
      next:Level.t ->
      'a * block_entry list * Level.t

    type 'a t = 'a context

    let pure x ~env:_ ~block ~next = (x, block, next)

    let ( let* ) v f ~env ~block ~next =
      let x, block, next = v ~env ~block ~next in
      f x ~env ~block ~next

    let solve var ~env ~block ~next =
      match List.nth_opt env ((var : Index.t) :> int) with
      | Some var -> (var, block, next)
      | None -> failwith "unknown var somehow"

    let let_or_var code ~env:_ ~block ~next =
      match code with
      | C_var { var } -> (var, block, next)
      | C_apply _ | C_lambda _ | C_prim _ ->
          let var = next in
          let block = BE_let { arg = code } :: block in
          let next = Level.next next in
          (var, block, next)

    let enter_let =
     fun ~var k ~env ~block ~next ->
      let env = var :: env in
      k () ~env ~block ~next

    let enter_hoist k ~env ~block ~next =
      let var = next in
      let env = var :: env in
      let block = BE_hoist :: block in
      let next = Level.next next in
      k () ~env ~block ~next

    let enter_fix ~var ~arg k ~env ~block ~next =
      let block = BE_fix { var; arg } :: block in
      k () ~env ~block ~next

    let emit_block block last =
      let last = B_return { code = last } in
      List.fold_left
        (fun next entry ->
          match entry with
          | BE_let { arg } -> B_let { arg; next }
          | BE_hoist -> B_hoist { next }
          | BE_fix { var; arg } -> B_fix { var; arg; next })
        last block

    let fork_body k ~env ~block ~next =
      let body =
        let param = next in
        let env = param :: env in
        let next = Level.next next in
        let last, block, _lambda_next = k param ~env ~block:[] ~next in
        emit_block block last
      in
      (body, block, next)
  end

  open Context

  let rec flatten_term term : code context =
    (* TODO: locations here *)
    let (Term { struct_ = term; type_ = _; loc = _ }) = term in
    match term with
    | T_annot { term; annot = _ } -> flatten_term term
    | T_var { var } ->
        let* var = solve var in
        pure @@ C_var { var }
    | T_let { bound; arg; body } ->
        let* arg = flatten_term arg in
        let* arg = let_or_var arg in
        flatten_pat ~src:arg bound @@ fun () -> flatten_term body
    | T_hoist { name = _; annot = _; body } ->
        enter_hoist @@ fun () -> flatten_term body
    | T_fix { name = _; var; arg; body } ->
        let* var = solve var in
        let* arg = flatten_term arg in
        enter_fix ~var ~arg @@ fun () -> flatten_term body
    | T_apply { funct; arg } ->
        let* funct = flatten_term funct in
        let* funct = let_or_var funct in
        let* arg = flatten_term arg in
        let* arg = let_or_var arg in
        pure @@ C_apply { funct; arg }
    | T_lambda { param; body } ->
        let* body =
          fork_body @@ fun param ->
          flatten_pat ~src:param bound @@ fun () -> flatten_term body
        in
        pure @@ C_lambda { body }
    | T_tuple { elements } ->
        (* TODO: this is stupid, just put it on the monad *)
        let rec flatten_elements elements =
          match elements with
          | [] -> pure []
          | el :: elements ->
              let* el = flatten_term el in
              let* el = let_or_var el in
              let* elements = flatten_elements elements in
              pure @@ (el :: elements)
        in
        let* elements = flatten_elements elements in
        let elements = Array.of_list elements in
        pure @@ C_prim { prim = P_tuple { elements } }
    | T_int32 { lit } -> pure @@ C_prim { prim = P_int32 { lit } }
    | T_string { lit } -> pure @@ C_prim { prim = P_string { lit } }
    | T_forall { param; body } ->
        let param = flatten_pat param in
        let* body =
          fork_body @@ fun param ->
          flatten_pat ~src:param param @@ fun () -> flatten_term body
        in
        pure @@ C_lambda { body }
    | T_self _ | T_exists _ -> pure @@ C_prim { prim = P_type_stub }

  and flatten_pat ~src pat k =
    match pat with
    | P_annot { pat; annot = _ } -> flatten_pat ~src pat k
    | P_var { var = _ } ->
        (* TODO: something about the name? *)
        enter_let ~var:src k
    | P_tuple { elements } ->
        (* TODO: this is not great *)
        let tuple = src in
        let rec shallow_dump elements field =
          match elements with
          | [] -> pure @@ []
          | el :: elements ->
              let* var =
                let_or_var @@ C_prim { prim = P_field { tuple; field } }
              in
              let* elements =
                (* TODO: index type? *)
                let field = 1 + field in
                shallow_dump elements field
              in
              pure @@ ((var, el) :: elements)
        in
        let* elements = shallow_dump elements 0 in
        let rec deep_dump elements k =
          match elements with
          | [] -> k ()
          | (var, el) :: elements ->
              flatten_pat ~src:var el @@ fun () -> deep_dump elements k
        in
        deep_dump elements k
end

module Flatten = struct
  type value =
    | V_forward of { mutable init : bool; mutable value : value }
    | V_type_stub
    | V_closure of { env : env; body : block }
    | V_tuple of { elements : value array }
    | V_world of { mutable used : bool }
    | V_int32 of { value : int32 }
    | V_string of { value : string }

  and env

  let v_unit : value = V_tuple { elements = [||] }
  let v_null : value = V_tuple { elements = [||] }
  let v_forward () = V_forward { init = false; value = v_null }
  let lookup : env -> var -> value = assert false
  let append : env -> value -> env = fun _env _value -> assert false

  exception Not_initialized
  exception World_duplicated
  exception Index_out_of_bounds
  exception Invalid_initialization
  exception Expected_a_function
  exception Expected_a_tuple
  exception Expected_a_pair
  exception Expected_a_world
  exception Expected_an_int32
  exception Expected_a_string

  (* helpers *)
  let rec init_forward value ~to_ =
    match value with
    | V_forward ({ init = false; value = _ } as forward) ->
        forward.init <- true;
        forward.value <- to_
    | V_forward { init = true; value = _ } -> raise Invalid_initialization
    | V_world { used = true } -> raise World_duplicated
    | V_world ({ used = false } as world) ->
        world.used <- true;
        (* TODO: should touch the value? *)
        raise Invalid_initialization
    | V_type_stub
    | V_closure { env = _; body = _ }
    | V_tuple { elements = _ }
    | V_int32 { value = _ }
    | V_string { value = _ } ->
        raise Invalid_initialization

  let rec unpack_funct value =
    match value with
    | V_closure { env; body } -> (env, body)
    | V_forward { init = true; value } -> unpack_funct value
    | V_forward { init = false; value = _ } -> raise Not_initialized
    | V_world { used = true } -> raise World_duplicated
    | V_world ({ used = false } as world) ->
        world.used <- true;
        (* TODO: should touch the value? *)
        raise Expected_a_function
    | V_type_stub
    | V_tuple { elements = _ }
    | V_int32 { value = _ }
    | V_string { value = _ } ->
        raise Expected_a_function

  let rec unpack_tuple value =
    match value with
    | V_tuple { elements } -> elements
    | V_forward { init = true; value } -> unpack_tuple value
    | V_forward { init = false; value = _ } -> raise Not_initialized
    | V_world { used = true } -> raise World_duplicated
    | V_world ({ used = false } as world) ->
        world.used <- true;
        (* TODO: should touch the value? *)
        raise Expected_a_tuple
    | V_type_stub
    | V_closure { env = _; body = _ }
    | V_int32 { value = _ }
    | V_string { value = _ } ->
        raise Expected_a_tuple

  let rec unpack_pair value =
    match value with
    | V_tuple { elements = [| left; right |] } -> (left, right)
    | V_forward { init = true; value } -> unpack_pair value
    | V_forward { init = false; value = _ } -> raise Not_initialized
    | V_world { used = true } -> raise World_duplicated
    | V_world ({ used = false } as world) ->
        world.used <- true;
        (* TODO: should touch the value? *)
        raise Expected_a_pair
    | V_type_stub
    | V_closure { env = _; body = _ }
    | V_tuple { elements = _ }
    | V_int32 { value = _ }
    | V_string { value = _ } ->
        raise Expected_a_pair

  let rec unpack_world value =
    match value with
    | V_world { used = true } -> raise World_duplicated
    | V_world ({ used = false } as world) ->
        world.used <- true;
        V_world { used = false }
    | V_forward { init = true; value } -> unpack_world value
    | V_forward { init = false; value = _ } -> raise Not_initialized
    | V_type_stub
    | V_closure { env = _; body = _ }
    | V_tuple { elements = _ }
    | V_int32 { value = _ }
    | V_string _ ->
        raise Expected_a_world

  let rec unpack_int32 value =
    match value with
    | V_int32 { value } -> value
    | V_forward { init = true; value } -> unpack_int32 value
    | V_forward { init = false; value = _ } -> raise Not_initialized
    | V_world { used = true } -> raise World_duplicated
    | V_world ({ used = false } as world) ->
        world.used <- true;
        (* TODO: should touch the value? *)
        raise Expected_an_int32
    | V_type_stub
    | V_closure { env = _; body = _ }
    | V_tuple { elements = _ }
    | V_string { value = _ } ->
        raise Expected_an_int32

  let rec unpack_string value =
    match value with
    | V_string { value } -> value
    | V_forward { init = true; value } -> unpack_string value
    | V_forward { init = false; value = _ } -> raise Not_initialized
    | V_world { used = true } -> raise World_duplicated
    | V_world ({ used = false } as world) ->
        world.used <- true;
        (* TODO: should touch the value? *)
        raise Expected_a_string
    | V_type_stub
    | V_closure { env = _; body = _ }
    | V_tuple { elements = _ }
    | V_int32 { value = _ } ->
        raise Expected_a_string

  (* interpreter *)
  let rec eval_block env block =
    match block with
    | B_let { arg; next } ->
        let arg = eval_code env arg in
        let env = append env arg in
        eval_block env next
    | B_hoist { next } ->
        let arg = v_forward () in
        let env = append env arg in
        eval_block env next
    | B_fix { var; arg; next } ->
        let forward = lookup env var in
        (* TODO: fix should be prim? *)
        let arg = eval_code env arg in
        let () = init_forward forward ~to_:arg in
        eval_block env next
    | B_return { code } -> eval_code env code

  and eval_code env code =
    match code with
    | C_var { var } -> lookup env var
    | C_apply { funct; arg } ->
        let funct = lookup env funct in
        let arg = lookup env arg in
        let env, body = unpack_funct funct in
        let env = append env arg in
        eval_block env body
    | C_lambda { body } -> V_closure { env; body }
    | C_prim { prim } -> eval_prim env prim

  and eval_prim env prim =
    match prim with
    | P_type_stub -> V_type_stub
    | P_tuple { elements } ->
        (* TODO: unit and single element *)
        let elements = Array.map (fun el -> lookup env el) elements in
        V_tuple { elements }
    | P_int32 { lit } -> V_int32 { value = lit }
    | P_string { lit } -> V_string { value = lit }
    | P_field { tuple; field } -> (
        let tuple = lookup env tuple in
        let els = unpack_tuple tuple in
        match Array.get els field with
        | el -> el
        | exception Invalid_argument _ -> raise Index_out_of_bounds)
    | P_int32_add { left; right } ->
        let left = lookup env left in
        let right = lookup env right in
        let left = unpack_int32 left in
        let right = unpack_int32 right in
        let value = Int32.add left right in
        V_int32 { value }
    | P_int32_sub { left; right } ->
        let left = lookup env left in
        let right = lookup env right in
        let left = unpack_int32 left in
        let right = unpack_int32 right in
        let value = Int32.sub left right in
        V_int32 { value }
    | P_int32_mul { left; right } ->
        let left = lookup env left in
        let right = lookup env right in
        let left = unpack_int32 left in
        let right = unpack_int32 right in
        let value = Int32.mul left right in
        V_int32 { value }
    | P_string_length { string } ->
        let string = lookup env string in
        let string = unpack_string string in
        let value = String.length string in
        V_int32 { value = Int32.of_int value }
    | P_string_concat { left; right } ->
        let left = lookup env left in
        let right = lookup env right in
        let left = unpack_string left in
        let right = unpack_string right in
        let value = left ^ right in
        V_string { value }
    | P_world_log { world; message } ->
        let world = lookup env world in
        let message = lookup env message in
        let world = unpack_world world in
        let message = unpack_string message in
        (* TODO: message *)
        print_endline message;
        world
end

type var = Level.t

type block =
  | B_let of { annot : var; arg : code; next : block }
  | B_return of { code : code }

and code =
  | C_var of { var : var }
  | C_apply of { funct : var; arg : var }
  | C_lambda of { body : block }
  | C_forall of { param : var; body : block }
  | C_self of { self : var; body : block }

module Machinery = struct
  type value = { struct_ : value_struct; mutable var : var }

  and value_struct =
    (* | V_hole of { env : env; hole : hole } *)
    | V_var of { var : var; args : value list }
    | V_lambda of { env : env; body : block }
    | V_forall of { param : value; env : env; body : block }
  (* | V_self of { env : env; body : block }
     | V_thunk of { mutable funct_or_value : value; mutable arg_or_tag : value } *)

  and env = value list

  let var_null : var = assert false
  let is_var_null : var -> bool = assert false
  let repr : value -> value = assert false
  let struct_ : value -> value_struct = assert false
  let let_or_var block code = assert false

  let rec reify ~at block value =
    let value = repr value in
    let var = value.var in
    match is_var_null value.var with
    | true ->
        let x = reify_struct in
        assert false
    | false -> (block, C_var { var })

  and reify_struct ~at block value =
    match struct_ value with
    | V_var { var; args } ->
        let funct = C_var { var } in
        reify_args ~at block funct args
    | V_lambda { env; body } ->
        let block, body = reify_under ~at block env body in
        (block, C_lambda { body })
    | V_forall { param; env; body } ->
        let block, param = let_or_var block param in
        let block, body = reify_under ~at block env body in
        (block, C_forall { param; body })

  and reify_under = assert false

  and reify_args ~at block funct args =
    match args with
    | [] -> (block, funct)
    | arg :: args ->
        let block, funct = let_or_var block funct in
        let block, arg = reify ~at block arg in
        let block, arg = let_or_var block arg in
        let funct = C_apply { funct; arg } in
        reify_args ~at block funct args

  let append : env -> value -> env = fun _env _value -> assert false
  let lookup : env -> var -> value = fun _env _var -> assert false
  let v_null : value = assert false
  let is_null : value -> bool = assert false

  let rec eval_block env block =
    match block with
    | B_let { annot = _; arg; next } ->
        let arg = eval_code env arg in
        let env = append env arg in
        eval_block env next
    | B_return { code } -> eval_code env code

  and eval_code env code =
    match code with
    | C_var { var } -> force_head @@ lookup env var
    | C_apply { funct; arg } ->
        let funct = lookup env funct in
        let arg = lookup env arg in
        eval_apply env funct arg
    | C_lambda { body } -> V_lambda { env; body }
    | C_forall { param; body } ->
        let param = lookup env param in
        V_forall { param; env; body }
    | C_self { self = _; body } -> V_self { env; body }

  and eval_apply env funct arg =
    match force_head @@ funct with
    | V_hole { env; hole } -> _
    | V_var { var; args } ->
        let args = arg :: args in
        V_var { var; args }
    | V_lambda { env; body } ->
        let env = append env arg in
        eval_block env body
    | V_forall { param = _; env = _; body = _ } | V_self { env = _; body = _ }
      ->
        failwith "eval_apply: type mismatch"
    | V_thunk { funct_or_value = _; arg_or_tag = _ } ->
        failwith "eval_apply: thunk reached"

  and force_head value =
    match value with
    | V_hole _ | V_var _ | V_lambda _ | V_forall _ | V_self _ -> assert false
    | V_thunk ({ funct_or_value; arg_or_tag } as thunk) -> (
        match is_null arg_or_tag with
        | true ->
            (* TODO: path compression *)
            force_head funct_or_value
        | false ->
            let value = eval_apply funct_or_value arg_or_tag in
            thunk.funct_or_value <- value;
            thunk.arg_or_tag <- v_null;
            value)

  let rec unify lhs_env lhs rhs_env rhs =
    (* TODO: hydrate *)
    match (lhs, rhs) with
    | V_hole { env = lhs_env; hole = lhs }, rhs ->
        unify_hole lhs_env lhs rhs_env rhs
    | lhs, V_hole { env = rhs_env; hole = rhs } ->
        unify_hole rhs_env rhs lhs_env lhs
    | V_var { var = lhs; args = lhs_args }, V_var { var = rhs; args = rhs_args }
      -> (
        match Level.equal lhs rhs with
        | true -> ()
        | false -> failwith "var clash")
    | V_lambda { body = lhs_body }, V_lambda { body = rhs_body } ->
        (* unify_under lhs_env lhs_body rhs_env rhs_body *)
        assert false
    | ( V_forall { param = lhs_param; body = lhs_body },
        V_forall { param = rhs_param; body = rhs_body } ) ->
        unify lhs_env lhs_param rhs_env rhs_param;
        unify_under lhs_env lhs_body rhs_env rhs_body
    | lhs, rhs -> assert false

  and unify_hole hole_env hole to_env to_ = assert false
end

module M = struct
  type value =
    | V_hole of { env : env; hole : hole }
    | V_var of { var : var; args : value list }
    | V_lambda of { env : env; body : block }
    | V_forall of { param : value; env : env; body : block }

  and hole
  and env = value list

  let force_lambda =
    let body_hole = assert false in
    let env = [ body_hole ] in
    let body = C_var { var = Index.zero } in
    let body = B_return { code = body } in
    assert false

  let append : env -> value -> env = fun _env _value -> assert false
  let lookup : env -> var -> value = fun _env _var -> assert false

  let rec eval_block env block =
    match block with
    | B_let { annot = _; arg; next } ->
        let arg = eval_code env arg in
        let env = append env arg in
        eval_block env next
    | B_return { code } -> eval_code env code

  and eval_code env code =
    match code with
    | C_hole _ -> assert false
    | C_var { var } -> lookup env var
    | C_apply { funct; arg } ->
        let funct = lookup env funct in
        let arg = lookup env arg in
        eval_apply funct arg
    | C_lambda { body } -> V_lambda { body }
    | C_forall { param; body } ->
        let param = lookup env param in
        V_forall { param; body }

  and eval_apply funct arg =
    match funct with
    | V_hole { env; hole } -> assert false
    | V_var { var; args } ->
        let args = arg :: args in
        V_var { var; args }
    | V_lambda { env; body } ->
        let env = append env arg in
        eval_block env body
    | V_forall { param = _; env = _; body = _ } ->
        failwith "eval_apply: type mismatch"

  let map : env -> (value -> value) -> env = fun _env _value -> assert false

  (* TODO: not great but useful for holes *)
  let rec hydrate external_env value =
    (* TODO: repr *)
    match value with
    | V_hole { env; hole } ->
        let env = hydrate_env external_env env in
        V_hole { env; hole }
    | V_var { var; args } ->
        let funct = lookup external_env var in
        List.fold_left (fun funct arg -> eval_apply funct arg) funct args
    | V_lambda { env; body } ->
        let env = hydrate_env external_env env in
        V_lambda { env; body }
    | V_forall { param; env; body } ->
        let param = hydrate external_env param in
        let env = hydrate_env external_env env in
        V_forall { param; env; body }

  and hydrate_env external_env env =
    map env (fun value -> hydrate external_env value)

  type context

  let inst : context -> Index.t -> value = assert false
  let unify : received:value -> expected:value -> unit = assert false
  let fresh_hole = assert false

  let split_forall type_ =
    match type_ with
    | V_hole { env; hole } -> assert false
    | V_forall { param; env; body } -> (param, env, body)
    | _ -> assert false

  let v_univ : value = assert false
  let subst : arg:Index.t -> env -> block -> value = assert false
  let skolem : context -> value = assert false
  let enter_opaque : context -> context = assert false
  let enter_alias : context -> value -> context = assert false
  let lookup : context -> Index.t -> value = assert false

  let thunk env code =
    match code with
    | C_hole { hole } -> V_hole { env; hole }
    | C_var { var } -> lookup env var
    | C_apply { funct; arg } ->
        let funct = lookup env funct in
        let arg = lookup env arg in
        V_thunk { funct; arg }
    | C_lambda { body } -> V_lambda { env; body }
    | C_forall { param; body } ->
        let param = lookup env param in
        V_forall { param; env; body }
    | C_self { self = _; body } -> V_self { env; body }

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
    | C_self { self; body } -> _

  let struct_ : value -> value_struct = assert false

  let let_or_var block funct =
    match funct with
    | C_var { var } -> (block, var)
    | C_hole _
    | C_apply { funct = _; arg = _ }
    | C_lambda { body = _ }
    | C_forall { param = _; body = _ } ->
        write_let ctx funct

  module Context : sig
    type 'a context
    type 'a t = 'a context

    val pure : 'a -> 'a context
    val ( let* ) : 'a context -> ('a -> 'b context) -> 'b context
    val let_or_var : code -> var context
    val fork_block : (unit -> code context) -> block context
  end = struct
    type 'a context =
      block:code list -> next:Level.t -> 'a * code list * Level.t

    type 'a t = 'a context

    let pure x ~block ~next = (x, block, next)

    let ( let* ) v f ~block ~next =
      let x, block, next = v ~block ~next in
      f x ~block ~next

    let let_or_var code ~block ~next =
      match code with
      | C_var { var } -> (var, block, next)
      | ( C_hole _
        | C_apply { funct = _; arg = _ }
        | C_lambda { body = _ }
        | C_forall { param = _; body = _ } ) as code ->
          let var = next in
          let block = code :: block in
          let next = Level.next next in
          (var, block, next)

    let fork_block f ~block ~next = assert false
    (* let run f ~next = f ~block:[] ~next:Level.zero *)
  end

  open Context

  (* TODO: this also does unify check *)
  let rec reify value =
    match struct_ value with
    | V_var { var; args } ->
        (* TODO: check level of var *)
        let funct = C_var { var } in
        reify_args funct args
    | V_lambda { env; body } ->
        let* body = reify_under env body in
        pure @@ C_lambda { body }
    | V_forall { env; param; body } ->
        let* param = reify param in
        let* param = let_or_var param in
        let* body = reify_under env body in
        pure @@ C_forall { param; body }

  and reify_args funct args =
    match args with
    | [] -> pure @@ funct
    | arg :: args ->
        let* funct = let_or_var funct in
        let* arg = reify arg in
        let* arg = let_or_var arg in
        let funct = C_apply { funct; arg } in
        reify_args funct args

  and reify_under env body = C_lambda { body }

  let append : env -> value -> env = fun _env _value -> assert false
  let lookup : env -> var -> value = fun _env _var -> assert false

  let rec reify block value =
    match struct_ value with
    | V_var { var; args } ->
        let block = assert false in
        (block, C_var { var })
    | V_lambda { env; body } ->
        let block, body = reify_under block env body in
        (block, C_lambda { body })
    | V_forall { param; env; body } ->
        let block, param = reify block param in
        let block = param :: block in
        (* TODO: let or var *)
        let param = assert false in
        let block, body = reify_under block env body in
        (block, C_forall { param; body })

  and reify_under env body = assert false
  and reify_env block env = assert false

  let append : env -> value -> env = fun _env _value -> assert false
  let lookup : env -> var -> value = fun _env _var -> assert false
  let unify_check hole env code = assert false

  let rec unify lhs_env lhs rhs_env rhs =
    let lhs = lookup lhs_env lhs in
    let rhs = lookup rhs_env rhs in
    unify_value lhs_env lhs rhs_env rhs

  and unify_value lhs_env lhs rhs_env rhs =
    match (lhs, rhs) with
    | V_hole { hole = lhs }, rhs -> unify_hole lhs_env lhs rhs_env rhs
    | lhs, V_hole { hole = rhs } -> unify_hole rhs_env rhs lhs_env lhs
    | V_var { var = lhs }, V_var { var = rhs } -> (
        match Level.equal lhs rhs with
        | true -> ()
        | false -> failwith "var clash")
    | ( V_apply { funct = lhs_funct; arg = lhs_arg },
        V_apply { funct = rhs_funct; arg = rhs_arg } ) ->
        unify lhs_env lhs_funct rhs_env rhs_funct;
        unify lhs_env lhs_arg rhs_env rhs_arg
    | V_lambda { body = lhs_body }, V_lambda { body = rhs_body } ->
        unify_under lhs_env lhs_body rhs_env rhs_body
    | ( V_forall { param = lhs_param; body = lhs_body },
        V_forall { param = rhs_param; body = rhs_body } ) ->
        unify lhs_env lhs_param rhs_env rhs_param;
        unify_under lhs_env lhs_body rhs_env rhs_body
    | lhs, rhs -> assert false

  and unify_hole hole_env hole to_env to_ = assert false

  and unify_under lhs_env lhs_body rhs_env rhs_body =
    let skolem = _ in
    let lhs_env, lhs =
      let lhs_env = append lhs_env skolem in
      eval_block lhs_env lhs_body
    in
    let rhs_env, rhs =
      let rhs_env = append rhs_env skolem in
      eval_block rhs_env rhs_body
    in
    unify_value lhs_env lhs rhs_env rhs

  let reify block value = assert false

  let rec equal lhs_env lhs rhs_env rhs = assert false

  and equal_code lhs_env lhs rhs_env rhs =
    match (lhs, rhs) with
    | C_var _, C_var _ -> assert false
    | C_apply _, C_apply _ -> assert false
    | C_lambda { body = lhs_body }, C_lambda { body = rhs_body } ->
        equal_under lhs_env lhs_body rhs_env rhs_body
    | ( C_forall { param = lhs_param; body = lhs_body },
        C_forall { param = rhs_param; body = rhs_body } ) ->
        equal lhs_env lhs_param rhs_env rhs_param;
        equal_under lhs_env lhs_body rhs_env rhs_body
    | _ -> assert false

  and equal_under lhs_env lhs_body rhs_env rhs_body = assert false

  type value =
    | V_hole of { hole : hole }
    | V_var of { var : var; args : value list }
    | V_lambda of { env : env; body : block }
    | V_forall of { param : value; env : env; body : block }

  and hole = { mutable level : Level.t; mutable link : value }
  and env = { at : Level.t; values : value Level.Map.t }

  let append : env -> value -> env = fun _env _value -> assert false
  let lookup : env -> var -> value = fun _env _var -> assert false

  let rec eval_block env block =
    match block with
    | B_let { annot = _; arg; next } ->
        let arg = eval_code env arg in
        let env = append env arg in
        eval_block env next
    | B_return { code } -> eval_code env code

  and eval_code env code =
    match code with
    | C_var { var } -> lookup env var
    | C_apply { funct; arg } ->
        let funct = lookup env funct in
        let arg = lookup env arg in
        eval_apply funct arg
    | C_lambda { body } -> V_lambda { env; body }
    | C_forall { param; body } ->
        let param = lookup env param in
        V_forall { param; env; body }
    | C_self { self = _; body } -> V_self { env; body }

  let unify received_env received expected_env expected = assert false
  let current : env -> Level.t = assert false
  let append : env -> value -> env = fun _env _value -> assert false
  let lookup : env -> var -> value = fun _env _var -> assert false
  let infer : env -> var -> value = assert false
  let equal : value -> value -> unit = fun _left _right -> assert false
  let subst : arg:value -> env -> block -> value = assert false

  let destruct_forall : value -> value * env * block =
   fun _value -> assert false

  let v_univ : value = assert false
  let v_null : value = assert false
  let is_null : value -> bool = assert false

  let rec quote value =
    (* TODO: size explosion *)
    match value with
    | V_hole _ -> _
    | V_var { var; args } -> _
    | V_lambda { env; body } ->
        let block = quote_env env in
        (block, C_lambda { body })
    | V_forall { param; env; body } -> _
    | V_self { env; body } -> _
    | V_thunk _ -> _

  and quote_env block env = assert false

  let rec eval_block env block =
    match block with
    | B_let { annot = _; arg; next } ->
        let arg = eval_code env arg in
        let env = append env arg in
        eval_block env next
    | B_return { code } -> eval_code env code

  and eval_code env code =
    match code with
    | C_hole { hole } -> V_hole { env; hole }
    | C_var { var } -> lookup env var
    | C_apply { funct; arg } ->
        let funct = lookup env funct in
        let arg = lookup env arg in
        eval_apply funct arg
    | C_lambda { body } -> V_lambda { env; body }
    | C_forall { param; body } ->
        let param = lookup env param in
        V_forall { param; env; body }
    | C_self { self = _; body } -> V_self { env; body }

  and eval_apply funct arg =
    match head @@ funct with
    | V_hole _ -> _
    | V_var { var; args } ->
        let args = arg :: args in
        V_var { var; args }
    | V_lambda { env; body } ->
        let env = append env arg in
        eval_block env body
    | V_forall { param = _; env = _; body = _ } | V_self { env = _; body = _ }
      ->
        failwith "eval_apply: type mismatch"
    | V_thunk { funct = _; arg = _ } ->
        failwith "eval_apply: thunk not recheable"

  and head value =
    match value with
    | V_thunk ({ funct; arg } as thunk) -> (
        (* TODO: path compression *)
        match is_null arg with
        | true -> head funct
        | false ->
            let funct = eval_apply funct arg in
            thunk.funct <- funct;
            thunk.arg <- v_null;
            head funct)
    | V_hole _ | V_var _ | V_lambda _ | V_forall _ | V_self _ -> value

  and head_loop = assert false

  (* TODO: this is duplicated from eval *)
  let thunk env code =
    match code with
    | C_hole { hole } -> V_hole { env; hole }
    | C_var { var } -> lookup env var
    | C_apply { funct; arg } ->
        let funct = lookup env funct in
        let arg = lookup env arg in
        V_thunk { funct; arg }
    | C_lambda { body } -> V_lambda { env; body }
    | C_forall { param; body } ->
        let param = lookup env param in
        V_forall { param; env; body }
    | C_self { self = _; body } -> V_self { env; body }

  let rec check_block env block ~self ~expected =
    match block with
    | B_let { annot; arg; next } ->
        let () =
          let self = assert false in
          let expected = assert false in
          check_code env arg ~self ~expected
        in
        let env =
          let arg = assert false in
          append env arg
        in
        check_block env next ~self ~expected
    | B_return { code } -> check_code env code ~self ~expected

  and check_code env code ~self ~expected =
    (* TODO: coerce *)
    match code with
    | C_hole { hole } -> _
    | C_var { var } ->
        let received = infer env var in
        equal received expected
    | C_apply { funct; arg } ->
        let funct = infer env funct in
        let param, received_env, received_body = destruct_forall funct in
        let arg_type = infer env arg in
        equal param arg_type;
        let received =
          let arg = lookup env arg in
          subst ~arg received_env received_body
        in
        equal received expected
    | C_lambda { body } ->
        let param, expected_env, expected_body = destruct_forall expected in
        let arg = assert false in
        let expected = subst ~arg expected_env expected_body in
        let self = assert false in
        check_block env body ~self ~expected
    | C_forall { param; body } ->
        let received = univ in
        equal received expected;

        assert false
    | C_self { self = received_self; body } ->
        let received = univ in
        equal received expected;
        let funct = infer env body in
        let param, received_env, received_body = destruct_forall funct in
        let () = equal param self in
        let body = lookup env body in
        assert false

  let fresh_hole : unit -> hole = fun () -> assert false

  let force_lambda hole =
    let hole = fresh_hole () in
    let body = B_return { code = C_hole { hole } } in
    C_lambda { body }

  let rec eval_block env block =
    match block with
    | B_let { arg; next } -> assert false
    | B_return { code } -> eval_code env code

  and eval_code env code =
    match code with
    | C_var { var } -> lookup env var
    | C_apply { funct; arg } ->
        let funct = lookup env funct in
        let arg = lookup env arg in
        eval_apply funct arg
    | C_hole { hole } -> V_hole { env; hole }
    | C_lambda { body } -> V_lambda { env; body }

  and eval_apply funct arg =
    match funct with
    | V_var { var; args } ->
        let args = arg :: args in
        V_var { var; args }
    | V_hole { env; hole } ->
        let x =
          let body =
            let hole = fresh_hole () in
            B_return { code = C_hole { hole } }
          in
          V_lambda { env; body }
        in
        let body =
          let fresh_hole () = assert false in
          let env = empty in

          assert false
        in
        let env = append env arg in
        V_hole { env; hole }
    | V_lambda { env; body } ->
        let env = append env arg in
        eval_block env body
end
