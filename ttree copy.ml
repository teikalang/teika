open Utils

type term = Term of { struct_ : term_struct; type_ : value; loc : Location.t }

and term_struct =
  (* (M : A) *)
  | T_annot of { term : term; annot : term }
  (* \n *)
  | T_var of { var : Index.t }
  (* P = N; M *)
  | T_let of { bound : pat; arg : term; body : term }
  (* P : A; M *)
  | T_hoist of { bound : var_pat; body : term }
  (* P : A; ...; P = N; M *)
  (* TODO: pattern on fix *)
  | T_fix of { bound : var_pat; var : Index.t; arg : term; body : term }
  (* P => M *)
  | T_lambda of { param : pat; body : term }
  (* M N *)
  | T_apply of { funct : term; arg : term }
  (* (P : A) -> B *)
  | T_forall of { param : pat; body : term }
  (* TODO: part of fix *)
  (* (P : A) & B *)
  | T_self of { self : var_pat; body : term }
  (* (x = M; ...) *)
  | T_tuple of { elements : term list }
  (* (x : A, ...) *)
  | T_exists of { elements : pat list }
  (* -2 *)
  | T_int32 of { lit : int32 }
  (* "a" *)
  | T_string of { lit : string }

and var_pat =
  (* (P : A) *)
  | VP_annot of { pat : var_pat; annot : term }
  (* x *)
  | VP_var of { var : Name.t }

and pat = Pat of { struct_ : pat_struct; type_ : value; loc : Location.t }

and pat_struct =
  (* (P : A) *)
  | P_annot of { pat : pat; annot : term }
  (* x *)
  (* TODO: drop names and uses receipts *)
  | P_var of { var : Name.t }
  (* (x, ...) *)
  | P_tuple of { elements : pat list }

and value = { mutable struct_ : value_struct; mutable at : Level.t }

and value_struct =
  (* typer *)
  | V_hole
  | V_thunk of { env : env; [@opaque] term : term }
  | V_link of { mutable value : value }
  (* core *)
  | V_var of { var : Level.t; args : value list }
  | V_forward of { value : value; [@opaque] args : value list }
  | V_lambda of { env : env; [@opaque] param : pat; body : term }
  (* TODO: is univ actually needed or useful here? *)
  | V_univ
  | V_forall of { env : env; [@opaque] param : pat; body : term }
  | V_self of { env : env; [@opaque] self : var_pat; body : term }

and env = value list [@@deriving show { with_path = false }]

let rec repr value =
  match value.struct_ with
  | V_link { value } -> repr value
  | V_hole | V_var _ | V_forward _ | V_lambda _ | V_univ | V_forall _ | V_self _
  | V_thunk _ ->
      value

(* TODO: inline repr? *)
let repr value =
  match value.struct_ with
  | V_link ({ value } as link) ->
      (* path compression *)
      let value = repr value in
      link.value <- value;
      value
  | V_hole | V_var _ | V_forward _ | V_lambda _ | V_univ | V_forall _ | V_self _
  | V_thunk _ ->
      value

let lookup : env -> Index.t -> value = assert false
let append : env -> value -> env = assert false

let rec eval env term ~args =
  let (Term { struct_ = term; type_ = _; loc = _ }) = term in
  match term with
  | T_annot { term; annot } -> eval env term ~args
  | T_var { var } -> lookup env var
  | T_let { bound; arg; body } -> assert false
