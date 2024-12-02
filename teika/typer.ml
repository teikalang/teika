open Utils
open Ttree
open Terror

module Value : sig
  type value

  and value_struct =
    | V_hole
    (* TODO: name on var? *)
    | V_var of { name : Name.t }
    | V_forward of { name : Name.t; mutable inner : value }
    | V_apply of { funct : value; arg : value }
    | V_lambda of { env : env; bound : pat; body : term }
    | V_univ
    | V_forall of { param : value; env : env; bound : pat; body : term }
    | V_self of { env : env; bound : var_pat; body : term }
    | V_thunk of { env : env; term : term }
    | V_link of { mutable value : value }

  and env [@@deriving show]

  (* environment *)
  val empty : env
  val access : env -> Index.t -> value
  val append : env -> value -> env

  (* constructors *)
  val v_null : value
  val v_var : at:Level.t -> name:Name.t -> value
  val fresh_v_hole : at:Level.t -> value
  val fresh_v_forward : name:Name.t -> value
  val v_apply : funct:value -> arg:value -> value
  val v_lambda : env:env -> bound:pat -> body:term -> value
  val v_univ : value
  val v_forall : param:value -> env:env -> bound:pat -> body:term -> value
  val v_self : env:env -> bound:var_pat -> body:term -> value
  val v_thunk : env:env -> term:term -> value

  (* utilities *)
  val repr : value -> value
  val struct_ : value -> value_struct
  val level : value -> Level.t
  val same : value -> value -> bool
  val assert_forward : value -> unit
  val init_forward : value -> to_:value -> unit
  val lock_forward : value -> (unit -> 'a) -> 'a
  val hole_lower : value -> to_:Level.t -> unit
  val hole_link : value -> to_:value -> unit
  val thunk_link : value -> to_:value -> unit
end = struct
  type value = { mutable struct_ : value_struct; mutable at : Level.t }

  and value_struct =
    | V_hole
    | V_var of { name : Name.t }
    | V_forward of { name : Name.t; mutable inner : value [@opaque] }
    | V_apply of { funct : value; arg : value }
    | V_lambda of { env : env; [@opaque] bound : pat; body : term }
      (* TODO: is univ actually needed or useful here? *)
    | V_univ
    (* TODO: non dependent version of types and function *)
    | V_forall of {
        param : value;
        env : env; [@opaque]
        bound : pat;
        body : term;
      }
    | V_self of { env : env; [@opaque] bound : var_pat; body : term }
    | V_thunk of { env : env; [@opaque] term : term }
    | V_link of { mutable value : value }

  and env = value list [@@deriving show { with_path = false }]

  let v_new ~at struct_ = { struct_; at }

  let v_null =
    let name = Name.make "**null**" in
    v_new ~at:Level.zero @@ V_var { name }

  let v_var ~at ~name = v_new ~at @@ V_var { name }
  let fresh_v_hole ~at = v_new ~at @@ V_hole

  let fresh_v_forward ~name =
    (* TODO: proper level here *)
    let at = Level.zero in
    v_new ~at @@ V_forward { name; inner = v_null }

  let v_apply ~funct ~arg =
    let at = Level.max funct.at arg.at in
    v_new ~at @@ V_apply { funct; arg }

  let v_lambda ~env ~bound ~body =
    (* TODO: proper level for lambdas *)
    let at = Level.zero in
    v_new ~at @@ V_lambda { env; bound; body }

  let v_univ = v_new ~at:Level.zero @@ V_univ

  let v_forall ~param ~env ~bound ~body =
    (* TODO: proper level for forall *)
    let at = Level.zero in
    v_new ~at @@ V_forall { param; env; bound; body }

  let v_self ~env ~bound ~body =
    (* TODO: proper level for self *)
    let at = Level.zero in
    v_new ~at @@ V_self { env; bound; body }

  let v_thunk ~env ~term =
    (* TODO: proper level here *)
    let at = Level.zero in
    v_new ~at @@ V_thunk { env; term }

  let rec repr value =
    match value.struct_ with
    | V_link { value } -> repr value
    | V_hole | V_var _ | V_forward _ | V_apply _ | V_lambda _ | V_univ
    | V_forall _ | V_self _ | V_thunk _ ->
        value

  (* TODO: inline repr? *)
  let repr value =
    match value.struct_ with
    | V_link ({ value } as link) ->
        (* path compression *)
        let value = repr value in
        link.value <- value;
        value
    | V_hole | V_var _ | V_forward _ | V_apply _ | V_lambda _ | V_univ
    | V_forall _ | V_self _ | V_thunk _ ->
        value

  let struct_ value = (repr value).struct_

  (* TODO: level vs at *)
  let level value = (repr value).at
  let same (left : value) (right : value) = left == right

  let assert_forward value =
    match value.struct_ with
    | V_forward { name = _; inner = _ } -> ()
    | V_hole | V_var _ | V_apply _ | V_lambda _ | V_univ | V_forall _ | V_self _
    | V_thunk _ | V_link _ ->
        failwith "assert_forward: not a forward"

  let init_forward value ~to_ =
    let value = repr value in
    match value.struct_ with
    | V_forward ({ name = _; inner } as forward) -> (
        match same inner v_null with
        | true -> forward.inner <- to_
        | false -> failwith "init_forward: already initialized")
    | V_hole | V_var _ | V_apply _ | V_lambda _ | V_univ | V_forall _ | V_self _
    | V_thunk _ | V_link _ ->
        failwith "init_forward: not a forward"

  let lock_forward value f =
    match struct_ value with
    | V_forward ({ name = _; inner } as forward) ->
        forward.inner <- v_null;
        let finally () = forward.inner <- inner in
        Fun.protect ~finally f
    | V_hole | V_var _ | V_apply _ | V_lambda _ | V_univ | V_forall _ | V_self _
    | V_thunk _ | V_link _ ->
        failwith "lock_forward: not a forward"

  let hole_lower hole ~to_ =
    let hole = repr hole in
    (match hole.struct_ with
    | V_hole -> ()
    | _ -> failwith "hole_lower: not a hole");
    hole.at <- Level.min hole.at to_

  let hole_link hole ~to_ =
    let hole = repr hole in
    (match hole.struct_ with
    | V_hole -> ()
    | _ -> failwith "link_hole: not a hole");
    hole.struct_ <- V_link { value = to_ }

  let thunk_link thunk ~to_ =
    let thunk = repr thunk in
    (match thunk.struct_ with
    | V_thunk _ -> ()
    | _ -> failwith "link_thunk: not a thunk");
    thunk.struct_ <- V_link { value = to_ }

  let empty = []

  let access env var =
    let var = (var : Index.t :> int) in
    match List.nth_opt env var with
    | Some value -> value
    | None -> failwith "lookup: unknown variable"

  let append env value = value :: env
end

module Eval = struct
  open Value

  let rec with_var_pat env bound ~arg =
    let (VPat { struct_ = bound; loc = _ }) = bound in
    match bound with
    | VP_annot { pat; annot = _ } -> with_var_pat env pat ~arg
    | VP_var { var = _ } ->
        (* TODO: name and maybe type here? *)
        append env arg

  let rec with_pat env bound ~arg =
    let (Pat { struct_ = bound; loc = _ }) = bound in
    match bound with
    | P_annot { pat; annot = _ } -> with_pat env pat ~arg
    | P_var { var = _ } ->
        (* TODO: name and maybe type here? *)
        append env arg
    | P_tuple { elements = _ } -> failwith "not implemented"

  let rec fresh_v_forward_of_var_pat pat =
    let (VPat { struct_ = pat; loc = _ }) = pat in
    match pat with
    | VP_annot { pat; annot = _ } -> fresh_v_forward_of_var_pat pat
    | VP_var { var = name } -> fresh_v_forward ~name

  let rec eval env term =
    let (Term { struct_ = term; loc = _ }) = term in
    match term with
    | T_annot { term; annot = _ } -> eval env term
    | T_var { var } -> weak_force @@ access env var
    | T_hoist { bound; body } ->
        let env =
          let arg = fresh_v_forward_of_var_pat bound in
          with_var_pat env bound ~arg
        in
        eval env body
    | T_fix { bound = _; var; arg; body } ->
        let forward = access env var in
        let () = assert_forward forward in
        let () =
          let arg = eval env arg in
          init_forward forward ~to_:arg
        in
        eval env body
    | T_let { bound; arg; body } ->
        let env =
          let arg = eval env arg in
          with_pat env bound ~arg
        in
        eval env body
    | T_apply { funct; arg } ->
        let funct = eval env funct in
        let arg = eval env arg in
        eval_apply ~funct ~arg
    | T_lambda { bound; body } -> v_lambda ~env ~bound ~body
    | T_forall { bound; param; body } ->
        let param = eval env param in
        v_forall ~param ~env ~bound ~body
    | T_self { bound; body } -> v_self ~env ~bound ~body
    | T_tuple _ | T_exists _ -> failwith "not implemented"

  and eval_apply ~funct ~arg =
    let funct = weak_force funct in
    match struct_ funct with
    | V_lambda { env; bound; body } ->
        let env = with_pat env bound ~arg in
        eval env body
    | V_var _ | V_forward _ | V_apply _ -> v_apply ~funct ~arg
    | V_hole | V_univ | V_forall _ | V_self _ ->
        failwith "eval_apply: type clash"
    | V_link _ | V_thunk _ -> failwith "eval_apply: unreacheable"

  and weak_force value =
    (* TODO: forcing every time removes some short circuits *)
    let value = repr value in
    match struct_ value with
    | V_thunk { env; term } ->
        (* TODO: detect recursive force? *)
        let final = eval env term in
        thunk_link value ~to_:final;
        final
    | V_hole | V_var _ | V_forward _ | V_apply _ | V_lambda _ | V_univ
    | V_forall _ | V_self _ | V_link _ ->
        value

  let rec strong_force value =
    (* TODO: forcing every time removes some short circuits *)
    (* TODO: path compression is bad for reasons *)
    let value = weak_force value in
    match struct_ value with
    | V_forward { name = _; inner } -> (
        match same inner v_null with
        | true -> value
        | false -> strong_force inner)
    | V_apply { funct; arg } ->
        let funct = strong_force funct in
        strong_eval_apply ~funct ~arg
    | V_hole | V_var _ | V_lambda _ | V_univ | V_forall _ | V_self _ | V_link _
    | V_thunk _ ->
        value

  and strong_eval_apply ~funct ~arg =
    match struct_ funct with
    | V_lambda { env; bound; body } ->
        let env = with_pat env bound ~arg in
        strong_force @@ eval env body
    | V_var _ | V_forward _ | V_apply _ -> v_apply ~funct ~arg
    | V_hole | V_univ | V_forall _ | V_self _ ->
        failwith "strong_eval_apply: type clash"
    | V_link _ | V_thunk _ -> failwith "strong_eval_apply: unreacheable"
end

module Unify = struct
  open Value
  open Eval

  let rec unify_check ~at ~hole in_ =
    (* TODO: short circuit on level *)
    (* TODO: color to avoid size explosion *)
    let in_ = weak_force in_ in
    match struct_ in_ with
    | V_hole -> (
        match same hole in_ with
        | true -> failwith "occurs check"
        | false -> hole_lower in_ ~to_:at)
    | V_var { name = _ } -> (
        (* TODO: poly comparison *)
        match level in_ >= at with
        | true -> failwith "escape check"
        | false -> ())
    | V_forward { name = _; inner } ->
        lock_forward in_ @@ fun () -> unify_check ~at ~hole inner
    | V_apply { funct; arg } ->
        unify_check ~at ~hole funct;
        unify_check ~at ~hole arg
    | V_univ -> ()
    | V_lambda { env; bound = _; body } -> unify_check_under ~at ~hole env body
    | V_forall { param; env; bound = _; body } ->
        unify_check ~at ~hole param;
        unify_check_under ~at ~hole env body
    | V_self { env; bound = _; body } -> unify_check_under ~at ~hole env body
    | V_thunk _ | V_link _ -> failwith "unify_check: unreacheable"

  and unify_check_under ~at ~hole env body =
    (* TODO: fill this *)
    let name = Name.make "**unify_check_under**" in
    let skolem = v_var ~at ~name in
    let at = Level.next at in
    let body =
      let env = append env skolem in
      eval env body
    in
    unify_check ~at ~hole body

  let unify_hole ~at ~hole ~to_ =
    match same hole to_ with
    | true -> ()
    | false ->
        unify_check ~at ~hole to_;
        hole_link hole ~to_

  let rec unify ~at lhs rhs =
    (* TODO: do repr shortcircuit first *)
    let lhs = weak_force lhs in
    let rhs = weak_force rhs in
    match same lhs rhs with true -> () | false -> unify_struct ~at lhs rhs

  and unify_struct ~at lhs rhs =
    match (struct_ lhs, struct_ rhs) with
    | V_hole, _ -> unify_hole ~at ~hole:lhs ~to_:rhs
    | _, V_hole -> unify_hole ~at ~hole:rhs ~to_:lhs
    | V_var { name = _ }, V_var { name = _ } -> failwith "var clash"
    | ( V_forward { name = lhs_name; inner = lhs_inner },
        V_forward { name = rhs_name; inner = rhs_inner } ) -> (
        match same lhs_inner v_null || same rhs_inner v_null with
        | true ->
            failwith
            @@ Format.asprintf "forward clash: %s == %s" (Name.repr lhs_name)
                 (Name.repr rhs_name)
        | false ->
            (* TODO: is this a good idea? *)
            lock_forward lhs @@ fun () ->
            lock_forward rhs @@ fun () -> unify ~at lhs_inner rhs_inner)
    | ( V_apply { funct = lhs_funct; arg = lhs_arg },
        V_apply { funct = rhs_funct; arg = rhs_arg } ) ->
        unify ~at lhs_funct rhs_funct;
        unify ~at lhs_arg rhs_arg
    | ( V_lambda { env = lhs_env; bound = _; body = lhs_body },
        V_lambda { env = rhs_env; bound = _; body = rhs_body } ) ->
        unify_under ~at lhs_env lhs_body rhs_env rhs_body
    | V_univ, V_univ -> ()
    | ( V_forall { param = lhs_param; env = lhs_env; bound = _; body = lhs_body },
        V_forall
          { param = rhs_param; env = rhs_env; bound = _; body = rhs_body } ) ->
        unify ~at lhs_param rhs_param;
        unify_under ~at lhs_env lhs_body rhs_env rhs_body
    | ( V_self { env = lhs_env; bound = _; body = lhs_body },
        V_self { env = rhs_env; bound = _; body = rhs_body } ) ->
        (* TODO: check only bound? *)
        unify_under ~at lhs_env lhs_body rhs_env rhs_body
    | ( ( V_var _ | V_forward _ | V_apply _ | V_lambda _ | V_univ | V_forall _
        | V_self _ | V_thunk _ | V_link _ ),
        ( V_var _ | V_forward _ | V_apply _ | V_lambda _ | V_univ | V_forall _
        | V_self _ | V_thunk _ | V_link _ ) ) ->
        error_type_clash ()

  and unify_under ~at lhs_env lhs rhs_env rhs =
    (* TODO: should use pattern *)
    (* TODO: fill this *)
    let name = Name.make "**unify_check_under**" in
    let skolem = v_var ~at ~name in
    let at = Level.next at in
    let lhs =
      let lhs_env = append lhs_env skolem in
      eval lhs_env lhs
    in
    let rhs =
      let rhs_env = append rhs_env skolem in
      eval rhs_env rhs
    in
    unify ~at lhs rhs
end

module Machinery = struct
  open Value
  open Eval

  let rec inst_self ~self type_ =
    let type_ = strong_force type_ in
    match struct_ @@ type_ with
    | V_self { env; bound; body } ->
        let type_ =
          let env = with_var_pat env bound ~arg:self in
          eval env body
        in
        inst_self ~self type_
    | V_hole | V_var _ | V_forward _ | V_apply _ | V_lambda _ | V_univ
    | V_forall _ | V_thunk _ | V_link _ ->
        type_

  let split_forall value =
    let value = strong_force value in
    match struct_ value with
    | V_forall { param; env; bound; body } -> (param, env, bound, body)
    | V_hole -> failwith "hole is not a forall"
    | V_var _ | V_forward _ | V_apply _ | V_lambda _ | V_univ | V_self _
    | V_thunk _ | V_link _ ->
        failwith "not a forall"

  let coerce ~at ~self lhs rhs =
    let lhs = inst_self ~self lhs in
    (* TODO: this is really bad *)
    let rhs = inst_self ~self rhs in
    Format.eprintf "%a == %a\n%!" pp_value lhs pp_value rhs;
    Unify.unify ~at lhs rhs
end

open Value
open Eval
open Unify
open Machinery

type value = Value.value

(* infer *)
type vars = Vars of { types : (Name.t * value) list }
[@@ocaml.unboxed] [@@deriving show { with_path = false }]

let rec v_skolem ~at pat =
  let (Pat { struct_ = pat; loc = _ }) = pat in
  match pat with
  | P_annot { pat; annot = _ } -> v_skolem ~at pat
  | P_var { var = name } -> v_var ~at ~name
  | P_tuple _ -> failwith "not implemented"

let rec v_skolem_of_var_pat ~at pat =
  let (VPat { struct_ = pat; loc = _ }) = pat in
  match pat with
  | VP_annot { pat; annot = _ } -> v_skolem_of_var_pat ~at pat
  | VP_var { var = name } -> v_var ~at ~name

let rec enter vars pat ~type_ =
  let (Pat { struct_ = pat; loc = _ }) = pat in
  match pat with
  | P_annot { pat; annot = _ } -> enter vars pat ~type_
  | P_var { var = name } ->
      let (Vars { types }) = vars in
      (* TODO: why this *)
      let type_ =
        (* TODO: thunk strong force *)
        strong_force type_
      in
      let types = (name, type_) :: types in
      Vars { types }
  | P_tuple _ -> failwith "not implemented"

let rec enter_var_pat vars pat ~type_ =
  let (VPat { struct_ = pat; loc = _ }) = pat in
  match pat with
  | VP_annot { pat; annot = _ } -> enter_var_pat vars pat ~type_
  | VP_var { var = name } ->
      let (Vars { types }) = vars in
      (* TODO: why this *)
      let type_ =
        (* TODO: thunk strong force *)
        strong_force type_
      in
      let types = (name, type_) :: types in
      Vars { types }

let solve vars env var =
  let rec solve types var =
    match (types, var) with
    | (_name, type_) :: _types, 0 -> type_
    | (_name, _type) :: types, var -> solve types (var - 1)
    | [], _var ->
        (* TODO: this is a problem *)
        failwith "unexpected unbound variable"
  in
  let (Vars { types }) = vars in
  let self = access env var in
  let var = ((var : Index.t) :> int) in
  let type_ = solve types var in
  inst_self ~self type_

let rec type_of_pat env pat ~type_ =
  let (Pat { struct_ = pat; loc = _ }) = pat in
  match pat with
  | P_annot { pat; annot } ->
      let type_ = v_thunk ~env ~term:annot in
      type_of_pat env pat ~type_
  | P_var { var = _ } -> type_
  | P_tuple _ -> failwith "not implemented"

let rec type_of_var_pat env pat ~type_ =
  let (VPat { struct_ = pat; loc = _ }) = pat in
  match pat with
  | VP_annot { pat; annot } ->
      let type_ = v_thunk ~env ~term:annot in
      type_of_var_pat env pat ~type_
  | VP_var { var = _ } -> type_

(* TODO: ideally ensure that infer_term returns head normalized type *)
let rec infer_term vars env ~at term =
  let expected_self = None in
  let expected = fresh_v_hole ~at in
  check_term vars env ~at term ~expected_self ~expected;
  (* TODO: is this correct or a good idea? *)
  let self = v_thunk ~env ~term in
  inst_self ~self expected

and check_term vars env ~at term ~expected_self ~expected =
  (* TODO: not principled, let and annot will break this *)
  let (Term { struct_ = term; loc = _ }) = term in
  match term with
  | T_annot { term; annot } ->
      let annot = check_annot vars env ~at annot ~expected_self ~expected in
      check_term vars env ~at term ~expected_self ~expected:annot
  | T_var { var } ->
      (* TODO: use expected_self? *)
      let received = solve vars env var in
      let self = access env var in
      coerce ~at ~self received expected
  | T_hoist { bound; body } ->
      (* TODO: ensure it's eventually bound *)
      let type_ = infer_var_pat vars env ~at bound in
      let vars = enter_var_pat vars bound ~type_ in
      let arg = fresh_v_forward_of_var_pat bound in
      let env = with_var_pat env bound ~arg in
      check_term vars env ~at body ~expected_self ~expected
  | T_fix { bound; var; arg; body } ->
      (* TODO: ensure it's not trivially recursive? A = M(A) *)
      let forward = access env var in
      let () = assert_forward forward in
      let () =
        let expected = solve vars env var in
        check_var_pat vars env ~at bound ~expected;
        let self = forward in
        let expected_self = Some self in
        let expected = type_of_var_pat env bound ~type_:expected in
        check_term vars env ~at arg ~expected_self ~expected
      in
      let () =
        let arg = v_thunk ~env ~term:arg in
        init_forward forward ~to_:arg
      in
      let expected =
        (* TODO: this could unlock some reductions *)
        match expected_self with
        | Some self -> inst_self ~self expected
        | None -> expected
      in
      check_term vars env ~at body ~expected_self ~expected
  | T_let { bound; arg; body } ->
      let type_ = infer_pat vars env ~at bound in
      let () =
        check_term vars env ~at arg ~expected_self:None ~expected:type_
      in
      let arg = v_thunk ~env ~term:arg in
      let vars = enter vars bound ~type_ in
      let env = with_pat env bound ~arg in
      check_term vars env ~at body ~expected_self ~expected
  | T_lambda { bound; body } ->
      let expected_param, expected_env, expected_bound, expected_body =
        split_forall expected
      in
      let () = check_pat vars env ~at bound ~expected:expected_param in
      let param = type_of_pat env bound ~type_:expected_param in
      let skolem = v_skolem ~at bound in
      let vars = enter vars bound ~type_:param in
      let env = with_pat env bound ~arg:skolem in
      let at = Level.next at in
      let expected =
        let env = with_pat expected_env expected_bound ~arg:skolem in
        eval env expected_body
      in
      let expected = strong_force expected in
      let expected, expected_self =
        match expected_self with
        | Some self ->
            let self = strong_force self in
            let self = eval_apply ~funct:self ~arg:skolem in
            let expected = inst_self ~self expected in
            (expected, Some self)
        | None -> (expected, None)
      in
      check_term vars env ~at body ~expected_self ~expected
  | T_apply { funct; arg } ->
      let funct_type = infer_term vars env ~at funct in
      let param, body_env, bound, body_type = split_forall funct_type in
      let () =
        check_term vars env ~at arg ~expected_self:None ~expected:param
      in
      let received =
        let arg = v_thunk ~env ~term:arg in
        let body_env = with_pat body_env bound ~arg in
        eval body_env body_type
      in
      (* TODO: coerce? *)
      unify ~at received expected
  | T_forall { bound; param; body } ->
      unify ~at v_univ expected;
      let () =
        check_term vars env ~at param ~expected_self:None ~expected:v_univ
      in
      let param = eval env param in
      check_pat vars env ~at bound ~expected:param;
      let skolem = v_skolem ~at bound in
      let at = Level.next at in
      let vars = enter vars bound ~type_:param in
      let env = append env skolem in
      check_term vars env ~at body ~expected_self:None ~expected:v_univ
  | T_self { bound; body } ->
      (* TODO: this is really ugly *)
      unify ~at v_univ expected;
      let expected_self =
        match expected_self with
        | Some expected_self -> expected_self
        | None -> failwith "self is only supported in a fixpoint"
      in
      check_var_pat vars env ~at bound ~expected:expected_self;
      let type_ = type_of_var_pat env bound ~type_:expected_self in
      let skolem = v_skolem_of_var_pat ~at bound in
      let at = Level.next at in
      let vars = enter_var_pat vars bound ~type_ in
      let env = with_var_pat env bound ~arg:skolem in
      check_term vars env ~at body ~expected_self:None ~expected:v_univ
  | T_tuple _ | T_exists _ -> failwith "not implemented"

and check_annot vars env ~at annot ~expected_self ~expected =
  check_term vars env ~at annot ~expected_self:None ~expected:v_univ;
  let received = eval env annot in
  match expected_self with
  | Some self ->
      let received = inst_self ~self received in
      coerce ~at ~self received expected;
      received
  | None ->
      unify ~at received expected;
      received

and infer_var_pat vars env ~at pat =
  let expected = fresh_v_hole ~at in
  check_var_pat vars env ~at pat ~expected;
  expected

and check_var_pat vars env ~at pat ~expected =
  let (VPat { struct_ = pat_struct; loc = _ }) = pat in
  match pat_struct with
  | VP_annot { pat; annot } ->
      let annot =
        check_annot vars env ~at annot ~expected_self:None ~expected
      in
      check_var_pat vars env ~at pat ~expected:annot
  | VP_var { var = _ } -> ()

and infer_pat vars env ~at pat =
  let expected = fresh_v_hole ~at in
  check_pat vars env ~at pat ~expected;
  expected

and check_pat vars env ~at pat ~expected =
  let (Pat { struct_ = pat_struct; loc = _ }) = pat in
  match pat_struct with
  | P_annot { pat; annot } ->
      let annot =
        check_annot vars env ~at annot ~expected_self:None ~expected
      in
      check_pat vars env ~at pat ~expected:annot
  | P_var { var = _ } -> ()
  | P_tuple _ -> failwith "not implemented"

(* external *)
let infer_term term =
  let at = Level.(next zero) in
  let vars =
    let types = [ (Name.make "Type", v_univ) ] in
    Vars { types }
  in
  let env = append empty v_univ in
  try Ok (infer_term vars env ~at term) with exn -> Error exn
