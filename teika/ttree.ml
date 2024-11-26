open Utils

type term =
  (* (M : A) *)
  | T_annot of { term : term; annot : term }
  (* \n *)
  | T_var of { var : Index.t }
  (* P = N; M *)
  | T_let of { bound : pat; arg : term; body : term }
  (* P : A; M *)
  | T_hoist of { bound : pat; annot : term; body : term }
  (* P : A; ...; P = N; M *)
  | T_fix of { bound : pat; var : Index.t; arg : term; body : term }
  (* P => M *)
  | T_lambda of { bound : pat; body : term }
  (* M N *)
  | T_apply of { funct : term; arg : term }
  (* (P : A) -> B *)
  | T_forall of { bound : pat; param : term; body : term }
  (* (P : A) & B *)
  | T_self of { bound : pat; self : term; body : term }

and pat =
  (* (P : A) *)
  | P_annot of { pat : pat; annot : term }
  (* x *)
  (* TODO: drop names and uses receipts *)
  | P_var of { var : Name.t }
[@@deriving show { with_path = false }]

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

let lazy_apply ~funct ~arg =
  let thunk = lazy (eval_apply ~funct ~arg) in
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
