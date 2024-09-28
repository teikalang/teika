open Utils

type term =
  (* (M : A) *)
  | T_annot of { term : term; annot : term }
  (* \n *)
  | T_var of { var : Index.t }
  (* P = N; M *)
  | T_let of { bound : pat; arg : term; body : term }
  (* P => M *)
  | T_lambda of { bound : pat; body : term }
  (* M N *)
  | T_apply of { funct : term; arg : term }
  (* (P : A) -> B *)
  | T_forall of { bound : pat; param : term; body : term }
  (* (P : A) & B *)
  | T_inter of { bound : pat; left : term; right : term }

and pat =
  (* (P : A) *)
  | P_annot of { pat : pat; annot : term }
  (* x *)
  | P_var of { var : Name.t }
[@@deriving show { with_path = false }]

(* TODO: write docs for this *)
(* TODO: non dependent version of this *)
type value =
  | V_var of { at : Level.t }
  | V_apply of { funct : value; arg : value }
  | V_lambda of { env : env; body : term }
  | V_univ
  | V_forall of { param : value; env : env; body : term }
  | V_inter of { left : value; env : env; right : term }
  | V_thunk of { thunk : value Lazy.t }

and env = Env of { values : value list }
[@@ocaml.unboxed] [@@deriving show { with_path = false }]

(* TODO: ideally env should be somewhere else *)
let empty = Env { values = [] }
let v_univ = V_univ
let skolem ~at = V_var { at }

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

let shift env ~by =
  let rec shift values by =
    match (values, by) with
    | _value :: values, 0 -> values
    | _value :: values, by -> shift values (by - 1)
    | [], _var -> failwith "invalid shift"
  in
  let (Env { values }) = env in
  let by = ((by : Index.t) :> int) in
  let values = shift values by in
  Env { values }

let rec eval env term =
  match term with
  | T_annot { term; annot = _ } -> eval env term
  | T_var { var } -> head @@ access env var
  | T_let { bound = _; arg; body } ->
      let arg = thunk env arg in
      let env = append env arg in
      eval env body
  | T_apply { funct; arg } -> (
      let funct = eval env funct in
      let arg = thunk env arg in
      match funct with
      | V_lambda { env; body } ->
          let env = append env arg in
          eval env body
      | V_var _ | V_apply _ | V_univ | V_forall _ | V_inter _ | V_thunk _ ->
          V_apply { funct; arg })
  | T_lambda { bound = _; body } -> V_lambda { env; body }
  | T_forall { bound = _; param; body } ->
      let param = thunk env param in
      V_forall { param; env; body }
  | T_inter { bound = _; left; right } ->
      let left = thunk env left in
      V_inter { left; env; right }

and head value =
  match value with
  | V_thunk { thunk } -> Lazy.force thunk
  | V_var _ | V_apply _ | V_lambda _ | V_univ | V_forall _ | V_inter _ -> value

and thunk env term =
  let thunk = lazy (eval env term) in
  V_thunk { thunk }
