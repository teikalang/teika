open Utils
open Ttree
open Terror

module Eval = struct
  let rec eval env term =
    let (Term { struct_ = term; loc = _ }) = term in
    match term with
    | T_hole { hole } -> weak_force hole
    | T_annot { term; annot = _ } -> eval env term
    | T_var { var } -> weak_force @@ access env var
    | T_hoist { bound = _; body } ->
        let arg = fresh_v_forward () in
        let env = append env arg in
        eval env body
    | T_fix { bound = _; var; arg; body } ->
        let arg = eval env arg in
        let forward = access env var in
        init_forward forward ~to_:arg;
        eval env body
    | T_let { bound = _; arg; body } ->
        let arg = eval env arg in
        let env = append env arg in
        eval env body
    | T_apply { funct; arg } ->
        let funct = eval env funct in
        let arg = eval env arg in
        eval_apply ~funct ~arg
    | T_lambda { param; body } ->
        let param = eval_pat env param in
        v_lambda ~param ~env ~body
    | T_forall { param; body } ->
        let param = eval_pat env param in
        v_forall ~param ~env ~body
    | T_self { self; body } ->
        let self = eval_pat env self in
        v_self ~self ~env ~body

  and eval_pat env bound =
    let (Pat { struct_ = pat; annot; loc = _ }) = bound in
    match pat with
    (* | P_annot { pat; annot = _ } -> eval_pat env pat *)
    | P_var { var } ->
        let type_ = eval env annot in
        VPat { name = var; type_ }

  and with_pat env bound ~arg =
    let (VPat { name = _; type_ = _ }) = bound in
    (* TODO: name and maybe type here? *)
    append env arg

  and eval_apply ~funct ~arg =
    (* TODO: this is super hackish *)
    let funct = repr funct in
    match struct_ funct with
    | V_lambda { param; env; body } ->
        let env = with_pat env param ~arg in
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
    let value = weak_force value in
    match struct_ value with
    | V_forward { inner } -> (
        (* TODO: detect recursive force? *)
        (* TODO: this is not ideal *)
        match same inner v_null with
        | true -> value
        | false -> strong_force inner)
    | V_apply { funct; arg } ->
        (* TODO: avoid unneeded allocation when funct doesn't change *)
        let funct = strong_force funct in
        (* TODO: strong force on return of apply? *)
        eval_apply ~funct ~arg
    | V_hole | V_var _ | V_lambda _ | V_univ | V_forall _ | V_self _ | V_link _
    | V_thunk _ ->
        value
end

module Unify = struct
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
    | V_forward { inner } ->
        lock_forward in_ @@ fun () -> unify_check ~at ~hole inner
    | V_apply { funct; arg } ->
        unify_check ~at ~hole funct;
        unify_check ~at ~hole arg
    | V_univ -> ()
    | V_lambda { param = _; env; body } -> unify_check_under ~at ~hole env body
    | V_forall { param; env; body } ->
        unify_check_pat ~at ~hole param;
        unify_check_under ~at ~hole env body
    | V_self { self; env = _; body = _ } ->
        (* TODO: is check on both sides needed? *)
        unify_check_pat ~at ~hole self
    | V_thunk _ | V_link _ -> failwith "unify_check: unreacheable"

  and unify_check_pat ~at ~hole in_ =
    let (VPat { name = _; type_ }) = in_ in
    unify_check ~at ~hole type_

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
    | V_forward { inner = lhs_inner }, V_forward { inner = rhs_inner } -> (
        match same lhs_inner v_null || same rhs_inner v_null with
        | true -> failwith "forward clash"
        | false ->
            lock_forward lhs @@ fun () ->
            lock_forward rhs @@ fun () -> unify ~at lhs_inner rhs_inner)
    | ( V_apply { funct = lhs_funct; arg = lhs_arg },
        V_apply { funct = rhs_funct; arg = rhs_arg } ) ->
        unify ~at lhs_funct rhs_funct;
        unify ~at lhs_arg rhs_arg
    | ( V_lambda { env = lhs_env; param = _; body = lhs_body },
        V_lambda { env = rhs_env; param = _; body = rhs_body } ) ->
        unify_under ~at lhs_env lhs_body rhs_env rhs_body
    | V_univ, V_univ -> ()
    | ( V_forall { param = lhs_param; env = lhs_env; body = lhs_body },
        V_forall { param = rhs_param; env = rhs_env; body = rhs_body } ) ->
        unify_pat ~at lhs_param rhs_param;
        unify_under ~at lhs_env lhs_body rhs_env rhs_body
    | ( V_self { self = lhs_self; env = _; body = _ },
        V_self { self = rhs_self; env = _; body = _ } ) ->
        (* TODO: check body?? *)
        unify_pat ~at lhs_self rhs_self
    | ( ( V_var _ | V_forward _ | V_apply _ | V_lambda _ | V_univ | V_forall _
        | V_self _ | V_thunk _ | V_link _ ),
        ( V_var _ | V_forward _ | V_apply _ | V_lambda _ | V_univ | V_forall _
        | V_self _ | V_thunk _ | V_link _ ) ) ->
        error_type_clash ()

  and unify_pat ~at lhs rhs =
    let (VPat { name = _; type_ = lhs_type }) = lhs in
    let (VPat { name = _; type_ = rhs_type }) = rhs in
    unify ~at lhs_type rhs_type

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

  let unify ~at lhs rhs =
    Format.eprintf "%a == %a\n%!" pp_value lhs pp_value rhs;
    unify ~at lhs rhs
end

module Machinery = struct
  open Eval

  let rec inst_self ~self type_ =
    let type_ = strong_force type_ in
    match struct_ @@ type_ with
    | V_self { self = self_pat; env; body } ->
        let type_ =
          let env = with_pat env self_pat ~arg:self in
          eval env body
        in
        inst_self ~self type_
    | V_hole | V_var _ | V_forward _ | V_apply _ | V_lambda _ | V_univ
    | V_forall _ | V_thunk _ | V_link _ ->
        type_

  let split_forall value =
    let value = strong_force value in
    match struct_ value with
    | V_forall { param; env; body } -> (param, env, body)
    | V_hole -> failwith "hole is not a forall"
    | V_var _ | V_forward _ | V_apply _ | V_lambda _ | V_univ | V_self _
    | V_thunk _ | V_link _ ->
        failwith "not a forall"
end

open Eval
open Unify
open Machinery

type nonrec value = value

(* infer *)
type vars = Vars of { types : (Name.t * value) list } [@@ocaml.unboxed]

let v_skolem ~at pat =
  let (VPat { name; type_ = _ }) = pat in
  v_var ~at ~name

let type_of_pat pat =
  let (VPat { name = _; type_ }) = pat in
  match same type_ v_null with
  | true -> failwith "pat not initialized"
  | false -> type_

let enter vars pat =
  let (VPat { name; type_ }) = pat in
  let (Vars { types }) = vars in
  (* TODO: why this *)
  let type_ = strong_force type_ in
  let types = (name, type_) :: types in
  Vars { types }

let solve vars var =
  let rec solve types var =
    match (types, var) with
    | (_name, type_) :: _types, 0 ->
        (* Format.eprintf "var : %s\n%!" @@ Name.repr name; *)
        type_
    | (_name, _type) :: types, var -> solve types (var - 1)
    | [], _var ->
        (* TODO: this is a problem *)
        failwith "unexpected unbound variable"
  in
  let (Vars { types }) = vars in
  let var = ((var : Index.t) :> int) in
  solve types var

(* TODO: ideally ensure that infer_term returns head normalized type *)
let rec infer_term vars env ~at term =
  let self = None in
  let expected = fresh_v_hole ~at in
  check_term vars env ~at term ~self ~expected;
  (* TODO: is this correct or a good idea? *)
  let self = v_thunk ~env ~term in
  inst_self ~self expected

and check_term vars env ~at term ~self ~expected =
  (* TODO: not principled, let and annot will break this *)
  let (Term { struct_ = term; loc = _ }) = term in
  (* TODO: this is bad, a proper implementation would not need it *)
  let expected =
    match self with
    | Some self -> inst_self ~self expected
    | None -> strong_force expected
  in
  match term with
  | T_hole { hole = _ } -> failwith "check_term: hole should never happen"
  | T_annot { term; annot } ->
      let annot = check_annot vars env ~at annot ~self ~expected in
      check_term vars env ~at term ~self ~expected:annot
  | T_var { var } ->
      let received = solve vars var in
      let received =
        match self with
        | Some self -> inst_self ~self received
        | None -> strong_force received
      in
      (* Format.eprintf "%a == %a\n%!" pp_value received pp_value expected; *)
      unify ~at received expected
  | T_hoist { bound; body } ->
      (* TODO: ensure it's eventually bound *)
      let _bound = infer_pat vars env ~at bound in
      let bound = eval_pat env bound in
      let vars = enter vars bound in
      let arg = fresh_v_forward () in
      let env = with_pat env bound ~arg in
      check_term vars env ~at body ~self ~expected
  | T_fix { bound; var; arg; body } ->
      (* TODO: ensure it's not trivially recursive? A = M(A) *)
      let annot = solve vars var in
      check_pat vars env ~at bound ~expected:annot;
      let () =
        let self = access env var in
        (* TODO: this should also include the pattern? *)
        let expected = inst_self ~self annot in
        let self = Some self in
        check_term vars env ~at arg ~self ~expected
      in
      let () =
        let var = access env var in
        let arg = v_thunk ~env ~term:arg in
        init_forward var ~to_:arg
      in
      check_term vars env ~at body ~self ~expected
  | T_let { bound; arg; body } ->
      let () =
        let value_type = infer_pat vars env ~at bound in
        check_term vars env ~at arg ~self ~expected:value_type
      in
      let bound = eval_pat env bound in
      let arg = v_thunk ~env ~term:arg in
      let vars = enter vars bound in
      let env = with_pat env bound ~arg in
      check_term vars env ~at body ~self ~expected
  | T_lambda { param; body } ->
      Format.eprintf "forall: %a\n%!" pp_value (strong_force @@ expected);
      let expected_param, expected_env, expected_body = split_forall expected in
      let () =
        (* TODO: is this one okay? *)
        let expected = type_of_pat expected_param in
        check_pat vars env ~at param ~expected
      in
      let param = eval_pat env param in
      let skolem = v_skolem ~at param in
      let vars = enter vars param in
      let env = with_pat env param ~arg:skolem in
      let at = Level.next at in
      let expected =
        let expected_env = append expected_env skolem in
        eval expected_env expected_body
      in
      Format.eprintf "param: %a\n%!" pp_value (strong_force @@ type_of_pat param);
      Format.eprintf "expected: %a\n%!" pp_value expected;
      (* TODO: apply null *)
      let self =
        match self with
        | Some self ->
            let self = eval_apply ~funct:self ~arg:skolem in
            Some self
        | None -> None
      in
      check_term vars env ~at body ~self ~expected
  | T_apply { funct; arg } ->
      let funct_type = infer_term vars env ~at funct in
      let param, body_env, body_type = split_forall funct_type in
      let () =
        let self = None in
        let param_type = type_of_pat param in
        check_term vars env ~at arg ~self ~expected:param_type
      in
      let received =
        let arg = v_thunk ~env ~term:arg in
        let body_env = with_pat body_env param ~arg in
        eval body_env body_type
      in
      unify ~at received expected
  | T_forall { param; body } ->
      unify ~at v_univ expected;
      let _param_type = infer_pat vars env ~at param in
      let param = eval_pat env param in
      let skolem = v_skolem ~at param in
      let at = Level.next at in
      let vars = enter vars param in
      let env = append env skolem in
      (* TODO: self here? *)
      let self = None in
      check_term vars env ~at body ~self ~expected:v_univ
  | T_self { self = received_self; body } ->
      (* TODO: this is really ugly *)
      unify ~at v_univ expected;
      let self =
        match self with
        | Some self -> self
        | None -> failwith "self is only supported in a fixpoint"
      in
      check_pat vars env ~at received_self ~expected:self;
      let self = eval_pat env received_self in
      let skolem = v_skolem ~at self in
      let at = Level.next at in
      let vars = enter vars self in
      let env = with_pat env self ~arg:skolem in
      (* TODO: this self:None seems weird *)
      check_term vars env ~at body ~self:None ~expected:v_univ

and infer_annot vars env ~at annot =
  let self = None in
  check_term vars env ~at annot ~self ~expected:v_univ;
  eval env annot

and check_annot vars env ~at annot ~self ~expected =
  let received = infer_annot vars env ~at annot in
  let received =
    match self with Some self -> inst_self ~self received | None -> received
  in
  unify ~at received expected;
  received

and infer_pat vars env ~at pat : value =
  let expected = fresh_v_hole ~at in
  check_pat vars env ~at pat ~expected;
  expected

and check_pat vars env ~at pat ~expected =
  (* TODO: this duplicated the checking of some annotations *)
  let (Pat { struct_ = pat_struct; annot; loc = _ }) = pat in
  (* TODO: this is hackish *)
  let _received =
    match same_term annot t_null with
    | true -> p_init_hole ~at pat
    | false -> check_annot vars env ~at annot ~self:None ~expected
  in
  (* TODO: use this received *)
  match pat_struct with
  | P_var { var = _ } -> ()

(* external *)
let infer_term term =
  let at = Level.(next zero) in
  let vars =
    let types = [ (Name.make "Type", v_univ) ] in
    Vars { types }
  in
  let env = append empty v_univ in
  try Ok (infer_term vars env ~at term) with exn -> Error exn
