open Utils
open Ttree
open Terror

module Value : sig
  (* TODO: write docs for this *)
  type value

  and value_struct =
    (* inference *)
    | V_hole of { hole : hole }
    (* equality *)
    | V_var of { at : Level.t; args : value list }
    (* loops *)
    | V_forward of { forward : forward; args : value list }
    (* functions *)
    | V_lambda of { env : env; body : term }
    (* types *)
    | V_univ
    | V_forall of { param : value; env : env; body : term }
    | V_self of { env : env; body : term }
    (* laziness *)
    | V_thunk of { thunk : value Lazy.t }

  and env

  (* TODO: exposing this is clearly bad *)
  and hole = { mutable level : Level.t; mutable link : value }

  (* TODO: not ideal to expose this either *)
  and forward = { mutable inner : value } [@@deriving show]

  val struct_ : value -> value_struct
  val same_value : value -> value -> bool
  val same_hole : hole -> hole -> bool
  val same_forward : forward -> forward -> bool
  val fix : env -> Index.index -> arg:value -> unit
  val empty : env
  val v_null : value
  val v_univ : value
  val v_hole : at:Level.t -> unit -> value
  val v_forward : unit -> value
  val skolem : at:Level.t -> value
  val access : env -> Index.t -> value
  val append : env -> value -> env
  val thunk : env -> term -> value
  val eval : env -> term -> value
  val eval_apply : funct:value -> arg:value -> value
  val weak_head : value -> value
  val strong_head : value -> value
  val link : hole -> to_:value -> unit
  val link_hole : hole -> to_:hole -> unit
end = struct
  (* TODO: write docs for this *)
  (* TODO: non dependent version of this *)
  type value = value_struct

  and value_struct =
    | V_hole of { hole : hole }
    | V_var of { at : Level.t; args : value list }
    | V_forward of { forward : forward; args : value list }
    | V_lambda of { env : env; [@opaque] body : term }
    | V_univ
    | V_forall of { param : value; env : env; [@opaque] body : term }
    | V_self of { env : env; [@opaque] body : term }
    | V_thunk of { thunk : value Lazy.t }

  and env = Env of { values : value list } [@@ocaml.unboxed]
  and hole = { mutable level : Level.t; mutable link : value }

  and forward = { mutable inner : value [@opaque] }
  [@@deriving show { with_path = false }]

  let same_value (left : value) (right : value) = left == right
  let same_hole (left : hole) (right : hole) = left == right
  let same_forward (left : forward) (right : forward) = left == right
  let v_null = V_var { at = Level.zero; args = [] }
  let is_null value = same_value value v_null

  let rec repr value =
    match value with
    | V_hole { hole } when not (is_null hole.link) -> repr hole.link
    | V_hole _ | V_forward _ | V_var _ | V_lambda _ | V_univ | V_forall _
    | V_self _ | V_thunk _ ->
        value

  let repr value =
    match value with
    | V_hole { hole } when not (is_null hole.link) ->
        (* path compression *)
        let value = repr hole.link in
        hole.link <- value;
        value
    | V_hole _ | V_forward _ | V_var _ | V_lambda _ | V_univ | V_forall _
    | V_self _ | V_thunk _ ->
        value

  let link hole ~to_ =
    (* TODO: better error here *)
    (match is_null hole.link with
    | true -> ()
    | false -> failwith "linked twice");
    hole.link <- to_

  let link_hole hole ~to_ =
    let to_ = V_hole { hole = to_ } in
    link hole ~to_

  let struct_ value = repr value

  (* TODO: ideally env should be somewhere else *)
  let empty = Env { values = [] }
  let v_univ = V_univ

  let v_hole ~at () =
    let hole = { level = at; link = v_null } in
    V_hole { hole }

  let v_forward () =
    let forward = { inner = v_null } in
    V_forward { forward; args = [] }

  let skolem ~at = V_var { at; args = [] }

  let access env var =
    let rec access values var =
      match (values, var) with
      | value :: _values, 0 -> value
      | _value :: values, var -> access values (var - 1)
      | [], _var -> failwith "invalid access"
    in
    let (Env { values }) = env in
    let var = ((var : Index.t) :> int) in
    access values var

  let append env value =
    let (Env { values }) = env in
    let values = value :: values in
    Env { values }

  let fix env var ~arg =
    match access env var with
    | V_forward { forward; args = [] } -> forward.inner <- arg
    | V_forward _ | V_hole _ | V_var _ | V_lambda _ | V_univ | V_forall _
    | V_self _ | V_thunk _ ->
        failwith "invalid fix"

  let rec eval env term =
    match term with
    | T_annot { term; annot = _ } -> eval env term
    | T_var { var } -> weak_head @@ access env var
    | T_hoist { bound = _; annot = _; body } ->
        let arg = v_forward () in
        let env = append env arg in
        eval env body
    | T_fix { bound = _; var; arg; body } ->
        let arg = thunk env arg in
        fix env var ~arg;
        eval env body
    | T_let { bound = _; arg; body } ->
        let arg = thunk env arg in
        let env = append env arg in
        eval env body
    | T_apply { funct; arg } ->
        let funct = eval env funct in
        let arg = thunk env arg in
        eval_apply ~funct ~arg
    | T_lambda { bound = _; body } -> V_lambda { env; body }
    | T_forall { bound = _; param; body } ->
        let param = thunk env param in
        V_forall { param; env; body }
    | T_self { bound = _; self = _; body } -> V_self { env; body }

  and eval_apply ~funct ~arg =
    (* TODO: this is super hackish *)
    let funct = weak_head funct in
    match funct with
    | V_lambda { env; body } ->
        let env = append env arg in
        eval env body
    | V_hole { hole = _ } -> failwith "not implemented hole apply"
    | V_var { at; args } ->
        let args = arg :: args in
        V_var { at; args }
    | V_forward { forward; args } ->
        let args = arg :: args in
        V_forward { forward; args }
    | V_univ | V_forall _ | V_self _ | V_thunk _ ->
        failwith "should be unrecheable"

  and weak_head initial =
    let initial = repr initial in
    match initial with
    | V_thunk { thunk } -> Lazy.force thunk
    | V_var _ | V_hole _ | V_forward _ | V_lambda _ | V_univ | V_forall _
    | V_self _ ->
        initial

  and strong_head initial =
    let initial = weak_head initial in
    match initial with
    | V_thunk { thunk } -> strong_head @@ Lazy.force thunk
    | V_forward { forward; args } -> (
        let { inner } = forward in
        match is_null inner with
        | true -> initial
        | false ->
            (* TODO: head_and_fix? *)
            strong_head
            @@ List.fold_right
                 (fun arg funct -> eval_apply ~funct ~arg)
                 args inner)
    | V_hole _ | V_var _ | V_lambda _ | V_univ | V_forall _ | V_self _ ->
        initial

  and thunk env term =
    let thunk = lazy (eval env term) in
    V_thunk { thunk }
end

module Machinery = struct
  open Value

  let rec unify_check ~hole ~max_level in_ =
    match struct_ in_ with
    | V_hole { hole = in_ } -> (
        match same_hole hole in_ with
        | true -> failwith "occurs check"
        | false ->
            (* TODO: is this lowering correct? *)
            in_.level <- Level.min in_.level max_level)
    | V_var { at; args } ->
        (* TODO: poly comparison *)
        (match at >= max_level with
        | true -> failwith "escape check"
        | false -> ());
        List.iter (fun in_ -> unify_check ~hole ~max_level in_) args
    | V_forward { forward; args } ->
        (* TODO: forward level *)
        (* TODO: this is not perfect, it will give false positives *)
        (* TODO: this is super hackish *)
        let { inner } = forward in
        forward.inner <- v_null;
        unify_check ~hole ~max_level inner;
        (* TODO: undo on exception *)
        forward.inner <- inner;
        List.iter (fun in_ -> unify_check ~hole ~max_level in_) args
    | V_univ -> ()
    | V_lambda { env; body } -> unify_check_under ~hole ~max_level env body
    | V_forall { param; env; body } ->
        unify_check ~hole ~max_level param;
        unify_check_under ~hole ~max_level env body
    | V_self { env; body } -> unify_check_under ~hole ~max_level env body
    | V_thunk { thunk } -> unify_check ~hole ~max_level @@ Lazy.force thunk

  and unify_check_under ~hole ~max_level env body =
    let body =
      let skolem = skolem ~at:max_level in
      let env = append env skolem in
      eval env body
    in
    let max_level = Level.next max_level in
    unify_check ~hole ~max_level body

  let rec unify ~at lhs rhs =
    let lhs = weak_head lhs in
    let rhs = weak_head rhs in
    match same_value lhs rhs with
    | true -> ()
    | false -> unify_struct ~at lhs rhs

  and unify_struct ~at lhs rhs =
    match (struct_ @@ lhs, struct_ @@ rhs) with
    | V_hole { hole }, _ -> unify_hole ~hole rhs
    | _, V_hole { hole } -> unify_hole ~hole lhs
    | V_var { at = lhs; args = lhs_args }, V_var { at = rhs; args = rhs_args }
      ->
        (match Level.equal lhs rhs with
        | true -> ()
        | false -> failwith "var clash");
        unify_args ~at lhs_args rhs_args
    | ( V_forward { forward = lhs; args = lhs_args },
        V_forward { forward = rhs; args = rhs_args } ) ->
        (match same_forward lhs rhs with
        | true -> ()
        | false ->
            (* TODO: check if they're literally the same *)
            failwith "forward clash");
        unify_args ~at lhs_args rhs_args
    | ( V_lambda { env = lhs_env; body = lhs_body },
        V_lambda { env = rhs_env; body = rhs_body } ) ->
        unify_under ~at lhs_env lhs_body rhs_env rhs_body
    | V_univ, V_univ -> ()
    | ( V_forall { param = lhs_param; env = lhs_env; body = lhs_body },
        V_forall { param = rhs_param; env = rhs_env; body = rhs_body } ) ->
        unify ~at lhs_param rhs_param;
        unify_under ~at lhs_env lhs_body rhs_env rhs_body
    | ( V_self { env = lhs_env; body = lhs_body },
        V_self { env = rhs_env; body = rhs_body } ) ->
        unify_under ~at lhs_env lhs_body rhs_env rhs_body
    | ( ( V_var _ | V_forward _ | V_lambda _ | V_univ | V_forall _ | V_self _
        | V_thunk _ ),
        ( V_var _ | V_forward _ | V_lambda _ | V_univ | V_forall _ | V_self _
        | V_thunk _ ) ) ->
        error_type_clash ()

  and unify_args ~at lhs_args rhs_args =
    (* TODO: iter2 clash *)
    (* TODO: this can happen for sink types *)
    List.iter2 (fun lhs rhs -> unify ~at lhs rhs) lhs_args rhs_args

  and unify_under ~at lhs_env lhs rhs_env rhs =
    let skolem = skolem ~at in
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

  and unify_hole ~hole to_ =
    match struct_ to_ with
    | V_hole { hole = to_ } -> (
        match same_hole hole to_ with
        | true -> ()
        | false -> (
            (* TODO: poly > *)
            (* TODO: unecessary allocation *)
            match hole.level > to_.level with
            | true -> link_hole to_ ~to_:hole
            | false ->
                (* TODO: which direction when both are the same level *)
                link_hole hole ~to_))
    | V_forward _ | V_var _ | V_lambda _ | V_univ | V_forall _ | V_self _
    | V_thunk _ ->
        unify_check ~hole ~max_level:hole.level to_;
        link hole ~to_

  let unify ~at lhs rhs =
    let lhs = strong_head lhs in
    let rhs = strong_head rhs in
    unify ~at lhs rhs

  let rec inst_self ~self type_ =
    let type_ = strong_head type_ in
    match struct_ @@ type_ with
    | V_self { env; body } ->
        let type_ =
          let env = append env self in
          eval env body
        in
        inst_self ~self type_
    | V_hole _ | V_var _ | V_forward _ | V_lambda _ | V_univ | V_forall _
    | V_thunk _ ->
        type_

  let coerce ~at term lhs rhs =
    let lhs = inst_self ~self:term lhs in
    let rhs = inst_self ~self:term rhs in
    (* TODO: this is garbage *)
    unify ~at lhs rhs
  (* TODO: where to do path compression? *)

  let split_forall value =
    match struct_ @@ strong_head value with
    | V_forall { param; env; body } -> (param, env, body)
    | V_hole { hole = _ } -> failwith "hole is not a forall"
    | V_var _ | V_forward _ | V_lambda _ | V_univ | V_self _ | V_thunk _ ->
        Format.eprintf "value: %a\n%!" pp_value (strong_head value);
        failwith "not a forall"

  let split_forall_with_self ~self value = split_forall @@ inst_self ~self value
end

open Value
open Machinery

type nonrec value = value

(* infer *)
type vars = Vars of { types : value list } [@@ocaml.unboxed]

let enter vars ~type_ =
  let (Vars { types }) = vars in
  let types = type_ :: types in
  Vars { types }

let solve vars var =
  let rec solve types var =
    match (types, var) with
    | type_ :: _types, 0 -> type_
    | _type :: types, var -> solve types (var - 1)
    | [], _var ->
        (* TODO: this is a problem *)
        failwith "unexpected unbound variable"
  in
  let (Vars { types }) = vars in
  let var = ((var : Index.t) :> int) in
  solve types var

(* TODO: ideally ensure that infer_term returns head normalized type *)
let rec infer_term ~at vars env term =
  let self = None in
  let expected = v_hole ~at () in
  check_term ~at vars env term ~self ~expected;
  expected

and check_term ~at vars env term ~self ~expected =
  (* TODO: not principled, let and annot will break this *)
  match term with
  | T_annot { term; annot } ->
      let annot = check_annot ~at vars env annot ~expected in
      check_term ~at vars env term ~self ~expected:annot
  | T_var { var } ->
      let received = solve vars var in
      let var = access env var in
      (* TODO: coerce here? *)
      coerce ~at var received expected
  | T_hoist { bound; annot; body } ->
      (* TODO: ensure it's eventually bound *)
      let annot = infer_annot ~at vars env annot in
      check_pat ~at vars env bound ~expected:annot;
      let vars = enter vars ~type_:annot in
      let env =
        let value = v_forward () in
        append env value
      in
      check_term ~at vars env body ~self ~expected
  | T_fix { bound; var; arg; body } ->
      (* TODO: ensure it's not trivially recursive; A = M(A) *)
      let annot = solve vars var in
      check_pat ~at vars env bound ~expected:annot;
      let () =
        let self = Some (access env var) in
        check_term ~at vars env arg ~self ~expected:annot
      in
      let () =
        let arg = thunk env arg in
        fix env var ~arg
      in
      check_term ~at vars env body ~self ~expected
  | T_let { bound; arg; body } ->
      let value_type =
        match infer_pat ~at vars env bound with
        | Some value_type ->
            (* TODO: self here? *)
            let self = None in
            check_term ~at vars env arg ~self ~expected:value_type;
            value_type
        | None -> infer_term ~at vars env arg
      in
      let vars = enter vars ~type_:value_type in
      let env =
        let value = thunk env arg in
        append env value
      in
      check_term ~at vars env body ~self ~expected
  | T_lambda { bound; body } ->
      let param, expected_env, expected_body =
        match self with
        | None -> split_forall expected
        | Some self -> split_forall_with_self ~self expected
      in
      check_pat ~at vars env bound ~expected:param;
      let skolem = skolem ~at in
      let at = Level.next at in
      let vars = enter vars ~type_:param in
      let env = append env skolem in
      let expected_body =
        let expected_env = append expected_env skolem in
        eval expected_env expected_body
      in
      (* TODO: apply null *)
      let self =
        match self with
        | Some self -> Some (eval_apply ~funct:self ~arg:skolem)
        | None -> None
      in
      check_term ~at vars env body ~self ~expected:expected_body
  | T_apply { funct; arg } ->
      let funct_type = infer_term ~at vars env funct in
      let param_type, body_env, body_type =
        let self = thunk env funct in
        split_forall_with_self ~self funct_type
      in
      let () =
        let self = None in
        check_term ~at vars env arg ~self ~expected:param_type
      in
      let body_env =
        let thunk = thunk env arg in
        append body_env thunk
      in
      let received = eval body_env body_type in
      (* TODO: drop unify *)
      unify ~at received expected
  | T_forall { bound; param; body } ->
      unify ~at v_univ expected;
      let param = infer_annot ~at vars env param in
      check_pat ~at vars env bound ~expected:param;
      let skolem = skolem ~at in
      let at = Level.next at in
      let vars = enter vars ~type_:param in
      let env = append env skolem in
      (* TODO: self here? *)
      let self = None in
      check_term ~at vars env body ~self ~expected:v_univ
  | T_self { bound; self = term_self; body } ->
      (* TODO: this is really ugly *)
      unify ~at v_univ expected;
      let self_type =
        match self with
        | Some self -> check_annot ~at vars env term_self ~expected:self
        | None -> failwith "self is only supported in a fixpoint"
      in
      check_pat ~at vars env bound ~expected:self_type;
      let skolem = skolem ~at in
      let at = Level.next at in
      let vars = enter vars ~type_:self_type in
      let env = append env skolem in
      check_term ~at vars env body ~self ~expected:v_univ

and infer_annot ~at vars env annot =
  let self = None in
  check_term ~at vars env annot ~self ~expected:v_univ;
  eval env annot

and check_annot ~at vars env annot ~expected =
  let received = infer_annot ~at vars env annot in
  unify ~at received expected;
  received

and infer_pat ~at vars env pat =
  match pat with
  | P_annot { pat; annot } ->
      let annot = infer_annot ~at vars env annot in
      check_pat ~at vars env pat ~expected:annot;
      Some annot
  | P_var { var = _ } -> None

and check_pat ~at vars env pat ~(expected : value) =
  match pat with
  | P_annot { pat; annot } ->
      let annot = check_annot ~at vars env annot ~expected in
      check_pat ~at vars env pat ~expected:annot
  | P_var { var = _ } -> ()

(* external *)
let infer_term term =
  let at = Level.(next zero) in
  let vars =
    let types = [ v_univ ] in
    Vars { types }
  in
  let env = append empty v_univ in
  try Ok (infer_term ~at vars env term) with exn -> Error exn
