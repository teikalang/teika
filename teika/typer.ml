open Ttree
open Terror

let rec equal ~at lhs rhs =
  match (head lhs, head rhs) with
  | V_var { at = lhs }, V_var { at = rhs } -> (
      match Level.equal lhs rhs with
      | true -> ()
      | false -> failwith "var clash")
  | ( V_apply { funct = lhs_funct; arg = lhs_arg },
      V_apply { funct = rhs_funct; arg = rhs_arg } ) ->
      equal ~at lhs_funct rhs_funct;
      equal ~at lhs_arg rhs_arg
  | ( V_lambda { env = lhs_env; body = lhs_body },
      V_lambda { env = rhs_env; body = rhs_body } ) ->
      equal_under ~at lhs_env lhs_body rhs_env rhs_body
  | V_univ, V_univ -> ()
  | ( V_forall { param = lhs_param; env = lhs_env; body = lhs_body },
      V_forall { param = rhs_param; env = rhs_env; body = rhs_body } ) ->
      equal ~at lhs_param rhs_param;
      equal_under ~at lhs_env lhs_body rhs_env rhs_body
  | ( V_inter { left = lhs_left; env = lhs_env; right = lhs_right },
      V_inter { left = rhs_left; env = rhs_env; right = rhs_right } ) ->
      equal ~at lhs_left rhs_left;
      equal_under ~at lhs_env lhs_right rhs_env rhs_right
  | ( ( V_var _ | V_apply _ | V_lambda _ | V_univ | V_forall _ | V_inter _
      | V_thunk _ ),
      ( V_var _ | V_apply _ | V_lambda _ | V_univ | V_forall _ | V_inter _
      | V_thunk _ ) ) ->
      error_type_clash ()

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

(* (M : LHS) :> RHS*)
(* TODO: short circuits *)
let rec coerce ~at term lhs rhs =
  match (head lhs, head rhs) with
  | V_inter { left = lhs_left; env = lhs_env; right = lhs_right }, rhs -> (
      try coerce ~at term lhs_left rhs
      with _exn ->
        (* TODO: properly handle this exception *)
        let lhs_right =
          let lhs_env = append lhs_env term in
          eval lhs_env lhs_right
        in
        coerce ~at term lhs_right rhs)
  | lhs, V_inter { left = rhs_left; env = rhs_env; right = rhs_right } ->
      coerce ~at term lhs rhs_left;
      let rhs_right =
        let rhs_env = append rhs_env term in
        eval rhs_env rhs_right
      in
      coerce ~at term lhs rhs_right
  | ( (V_var _ | V_apply _ | V_lambda _ | V_univ | V_forall _ | V_thunk _),
      (V_var _ | V_apply _ | V_lambda _ | V_univ | V_forall _ | V_thunk _) ) ->
      equal ~at lhs rhs

(* TODO: where to do path compression? *)
let split_forall value =
  match head value with
  | V_forall { param; env; body } ->
      let param = head param in
      (param, env, body)
  | V_var _ | V_apply _ | V_lambda _ | V_univ | V_inter _ | V_thunk _ ->
      failwith "not a forall"

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

let rec infer_term ~at vars env term =
  match term with
  | T_annot { term; annot } ->
      let annot = check_annot ~at vars env annot in
      check_term ~at vars env term ~expected:annot;
      annot
  | T_var { var } -> solve vars var
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
      let param_type, body_env, body_type = split_forall funct_type in
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
  | T_inter { bound; left; right } ->
      let left = check_annot ~at vars env left in
      check_pat ~at vars env bound ~expected:left;
      let () =
        let skolem = skolem ~at in
        let at = Level.next at in
        let vars = enter vars ~type_:left in
        let env = append env skolem in
        check_term ~at vars env right ~expected:v_univ
      in
      v_univ

and check_term ~at vars env term ~expected =
  match (term, expected) with
  | ( T_lambda { bound; body },
      V_forall { param; env = expected_env; body = expected_body } ) ->
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
  | term, expected ->
      let received = infer_term ~at vars env term in
      let term = eval env term in
      coerce ~at term received expected

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
