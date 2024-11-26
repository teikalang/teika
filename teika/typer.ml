open Utils
open Ttree
open Terror

module Value : sig
  (* TODO: write docs for this *)
  type value = private
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

  and forward = private
    | Forward of { mutable value : value; mutable init : bool }
  [@@deriving show]

  val same : value -> value -> bool
  val same_forward : forward -> forward -> bool
  val fix : env -> Index.index -> arg:value -> unit
  val empty : env
  val v_univ : value
  val v_forward : unit -> value
  val skolem : at:Level.t -> value
  val access : env -> Index.t -> value
  val append : env -> value -> env
  val thunk : env -> term -> value
  val eval : env -> term -> value
  val weak_head : value -> value
  val strong_head : value -> value
  val eta_lambda : skolem:value -> value -> value
end = struct
  (* TODO: write docs for this *)
  (* TODO: non dependent version of this *)
  type value =
    | V_var of { at : Level.t; args : value list }
    | V_forward of { forward : forward; args : value list }
    | V_lambda of { env : env; [@opaque] body : term }
    | V_univ
    | V_forall of { param : value; env : env; [@opaque] body : term }
    | V_self of { env : env; [@opaque] body : term }
    | V_thunk of { thunk : value Lazy.t }

  and env = Env of { values : value list } [@@ocaml.unboxed]

  and forward =
    | Forward of { mutable value : value; [@opaque] mutable init : bool }
  [@@deriving show { with_path = false }]

  let same (left : value) (right : value) = left == right
  let same_forward (left : forward) (right : forward) = left == right
  let v_nil = V_var { at = Level.zero; args = [] }

  (* TODO: ideally env should be somewhere else *)
  let empty = Env { values = [] }
  let v_univ = V_univ

  let v_forward () =
    let forward = Forward { value = v_nil; init = false } in
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
    | V_forward { forward; args = [] } ->
        let (Forward forward) = forward in
        forward.value <- arg;
        forward.init <- true
    | V_forward _ | V_var _ | V_lambda _ | V_univ | V_forall _ | V_self _
    | V_thunk _ ->
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
    match weak_head funct with
    | V_lambda { env; body } ->
        let env = append env arg in
        eval env body
    | V_var { at; args } ->
        let args = arg :: args in
        V_var { at; args }
    | V_forward { forward; args } ->
        let args = arg :: args in
        V_forward { forward; args }
    | V_univ | V_forall _ | V_self _ | V_thunk _ ->
        failwith "should be unrecheable"

  and weak_head initial =
    match initial with
    | V_thunk { thunk } -> Lazy.force thunk
    | V_var _ | V_forward _ | V_lambda _ | V_univ | V_forall _ | V_self _ ->
        initial

  and strong_head initial =
    let initial = weak_head initial in
    match initial with
    | V_thunk { thunk } -> strong_head @@ Lazy.force thunk
    | V_forward { forward; args } -> (
        let (Forward { value; init }) = forward in
        match init with
        | false -> initial
        | true ->
            (* TODO: head_and_fix? *)
            strong_head
            @@ List.fold_right
                 (fun arg funct -> eval_apply ~funct ~arg)
                 args value)
    | V_var _ | V_lambda _ | V_univ | V_forall _ | V_self _ -> initial

  and thunk env term =
    let thunk = lazy (eval env term) in
    V_thunk { thunk }

  let eta_lambda ~skolem lambda =
    (* TODO: this will do eta *)
    match weak_head lambda with
    | V_lambda { env; body } ->
        let env = append env skolem in
        eval env body
    | V_var { at; args } ->
        let args = skolem :: args in
        V_var { at; args }
    | V_forward { forward; args } ->
        let args = skolem :: args in
        V_forward { forward; args }
    | V_univ | V_forall _ | V_self _ | V_thunk _ ->
        failwith "not a lambda during eta"
end

module Machinery = struct
  open Value

  let rec equal ~at lhs rhs =
    match same lhs rhs with true -> () | false -> equal_struct ~at lhs rhs

  and equal_struct ~at lhs rhs =
    match (weak_head lhs, weak_head rhs) with
    | V_var { at = lhs; args = lhs_args }, V_var { at = rhs; args = rhs_args }
      ->
        (match Level.equal lhs rhs with
        | true -> ()
        | false -> failwith "var clash");
        equal_args ~at lhs_args rhs_args
    | ( V_forward { forward = lhs; args = lhs_args },
        V_forward { forward = rhs; args = rhs_args } ) ->
        (match same_forward lhs rhs with
        | true -> ()
        | false ->
            (* TODO: check if they're literally the same *)
            failwith "forward clash");
        equal_args ~at lhs_args rhs_args
    | ( V_lambda { env = lhs_env; body = lhs_body },
        V_lambda { env = rhs_env; body = rhs_body } ) ->
        equal_under ~at lhs_env lhs_body rhs_env rhs_body
    | V_univ, V_univ -> ()
    | ( V_forall { param = lhs_param; env = lhs_env; body = lhs_body },
        V_forall { param = rhs_param; env = rhs_env; body = rhs_body } ) ->
        equal ~at lhs_param rhs_param;
        equal_under ~at lhs_env lhs_body rhs_env rhs_body
    | ( V_self { env = lhs_env; body = lhs_body },
        V_self { env = rhs_env; body = rhs_body } ) ->
        equal_under ~at lhs_env lhs_body rhs_env rhs_body
    | ( ( V_var _ | V_forward _ | V_lambda _ | V_univ | V_forall _ | V_self _
        | V_thunk _ ),
        ( V_var _ | V_forward _ | V_lambda _ | V_univ | V_forall _ | V_self _
        | V_thunk _ ) ) ->
        error_type_clash ()

  and equal_args ~at lhs_args rhs_args =
    (* TODO: iter2 clash *)
    List.iter2 (fun lhs rhs -> equal ~at lhs rhs) lhs_args rhs_args

  and equal_under ~at lhs_env lhs rhs_env rhs =
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
    equal ~at lhs rhs

  let equal ~at lhs rhs =
    let lhs = strong_head lhs in
    let rhs = strong_head rhs in
    equal ~at lhs rhs

  let rec inst_self ~self type_ =
    match strong_head type_ with
    | V_self { env; body } ->
        let type_ =
          let env = append env self in
          eval env body
        in
        inst_self ~self type_
    | V_var _ | V_forward _ | V_lambda _ | V_univ | V_forall _ | V_thunk _ ->
        type_

  let coerce ~at term lhs rhs =
    let lhs = inst_self ~self:term lhs in
    let rhs = inst_self ~self:term rhs in
    (* TODO: this is garbage *)
    equal ~at lhs rhs
  (* TODO: where to do path compression? *)

  let split_forall value =
    match strong_head value with
    | V_forall { param; env; body } -> (param, env, body)
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
  match term with
  | T_annot { term; annot } ->
      let annot = check_annot ~at vars env annot in
      check_term ~at vars env term ~expected:annot;
      annot
  | T_var { var } -> solve vars var
  | T_hoist { bound; annot; body } ->
      (* TODO: ensure it's eventually bound *)
      let annot = check_annot ~at vars env annot in
      check_pat ~at vars env bound ~expected:annot;
      let vars = enter vars ~type_:annot in
      let env =
        let value = v_forward () in
        append env value
      in
      infer_term ~at vars env body
  | T_fix { bound; var; arg; body } ->
      (* TODO: ensure it's not trivially recursive; A = M(A) *)
      let annot = solve vars var in
      check_pat ~at vars env bound ~expected:annot;
      let self = access env var in
      check_self ~at vars env arg ~self ~expected:annot;
      let () =
        let arg = thunk env arg in
        fix env var ~arg
      in
      infer_term ~at vars env body
  | T_let { bound; arg; body } ->
      let value_type =
        match infer_pat ~at vars env bound with
        | Some value_type ->
            check_term ~at vars env arg ~expected:value_type;
            value_type
        | None -> infer_term ~at vars env arg
      in
      let vars = enter vars ~type_:value_type in
      let env =
        let value = thunk env arg in
        append env value
      in
      infer_term ~at vars env body
  | T_lambda { bound = _; body = _ } -> failwith "infer not supported"
  | T_apply { funct; arg } ->
      let funct_type = infer_term ~at vars env funct in
      let param_type, body_env, body_type =
        let self = thunk env funct in
        split_forall_with_self ~self funct_type
      in
      check_term ~at vars env arg ~expected:param_type;
      let body_env =
        let thunk = thunk env arg in
        append body_env thunk
      in
      eval body_env body_type
  | T_forall { bound; param; body } ->
      let param = check_annot ~at vars env param in
      check_pat ~at vars env bound ~expected:param;
      let () =
        let skolem = skolem ~at in
        let at = Level.next at in
        let vars = enter vars ~type_:param in
        let env = append env skolem in
        check_term ~at vars env body ~expected:v_univ
      in
      v_univ
  | T_self { bound; self; body } ->
      let self = check_annot ~at vars env self in
      check_pat ~at vars env bound ~expected:self;
      let () =
        let skolem = skolem ~at in
        let at = Level.next at in
        let vars = enter vars ~type_:self in
        let env = append env skolem in
        check_term ~at vars env body ~expected:v_univ
      in
      v_univ

and check_term ~at vars env term ~expected =
  (* TODO: not principled, let and annot will break this *)
  match term with
  | T_lambda { bound; body } ->
      let param, expected_env, expected_body = split_forall expected in
      check_pat ~at vars env bound ~expected:param;
      let skolem = skolem ~at in
      let at = Level.next at in
      let vars = enter vars ~type_:param in
      let env = append env skolem in
      let expected_body =
        let expected_env = append expected_env skolem in
        eval expected_env expected_body
      in
      check_term ~at vars env body ~expected:expected_body
  | term ->
      let received = infer_term ~at vars env term in
      let term = eval env term in
      coerce ~at term received expected

and check_self ~at vars env term ~self ~expected =
  (* TODO: not principled, let and annot will break this *)
  let expected = strong_head expected in
  match term with
  | T_lambda { bound; body } ->
      let param, expected_env, expected_body =
        split_forall_with_self ~self expected
      in
      (* TODO: this is almost a copy check_term *)
      check_pat ~at vars env bound ~expected:param;
      let skolem = skolem ~at in
      let at = Level.next at in
      let vars = enter vars ~type_:param in
      let env = append env skolem in
      let expected_body =
        let expected_env = append expected_env skolem in
        eval expected_env expected_body
      in
      let self = eta_lambda ~skolem self in
      check_self ~at vars env body ~self ~expected:expected_body
  | term -> check_term ~at vars env term ~expected

and check_annot ~at vars env term =
  check_term ~at vars env term ~expected:v_univ;
  eval env term

and infer_pat ~at vars env pat =
  match pat with
  | P_annot { pat; annot } ->
      let annot = check_annot ~at vars env annot in
      check_pat ~at vars env pat ~expected:annot;
      Some annot
  | P_var { var = _ } -> None

and check_pat ~at vars env pat ~expected =
  match pat with
  | P_annot { pat; annot } ->
      let annot = check_annot ~at vars env annot in
      (* TODO: coercion here? *)
      equal ~at annot expected;
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
