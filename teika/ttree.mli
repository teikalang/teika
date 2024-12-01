open Utils

type term = private Term of { struct_ : term_struct; loc : Location.t }

and term_struct =
  (* _A : Type *)
  | T_hole of { hole : value }
  (* (M : A) *)
  | T_annot of { term : term; annot : term }
  (* \n *)
  | T_var of { var : Index.t }
  (* P = N; M *)
  | T_let of { bound : pat; arg : term; body : term }
  (* P : A; M *)
  | T_hoist of { bound : pat; body : term }
  (* P : A; ...; P = N; M *)
  (* TODO: pattern on fix? *)
  | T_fix of { bound : pat; var : Index.t; arg : term; body : term }
  (* P => M *)
  | T_lambda of { param : pat; body : term }
  (* M N *)
  | T_apply of { funct : term; arg : term }
  (* (P : A) -> B *)
  | T_forall of { param : pat; body : term }
  (* TODO: part of fix *)
  (* (P : A) & B *)
  | T_self of { self : pat; body : term }

and pat = private
  | Pat of { struct_ : pat_struct; mutable annot : term; loc : Location.t }

and pat_struct =
  (* x *)
  (* TODO: drop names and uses receipts *)
  | P_var of { var : Name.t }

and value

and value_struct =
  | V_hole
  (* TODO: name on var? *)
  | V_var of { name : Name.t }
  | V_forward of { mutable inner : value [@opaque] }
  | V_apply of { funct : value; arg : value }
  | V_lambda of { param : value_pat; env : env; [@opaque] body : term }
  | V_univ
  | V_forall of { param : value_pat; env : env; [@opaque] body : term }
  | V_self of { self : value_pat; env : env; [@opaque] body : term }
  | V_thunk of { env : env; [@opaque] term : term }
  | V_link of { mutable value : value }

and value_pat = VPat of { name : Name.t; type_ : value }
and env [@@deriving show]

(* term *)
val t_null : term
val same_term : term -> term -> bool
val t_wrap : loc:Location.t -> term_struct -> term
val p_wrap : loc:Location.t -> annot:term -> pat_struct -> pat
val p_init_hole : at:Level.t -> pat -> value
(* values *)

(* environment *)
val empty : env
val access : env -> Index.t -> value
val append : env -> value -> env

(* constructors *)
val v_null : value
val v_var : at:Level.t -> name:Name.t -> value
val fresh_v_hole : at:Level.t -> value
val fresh_v_forward : unit -> value
val v_apply : funct:value -> arg:value -> value
val v_lambda : param:value_pat -> env:env -> body:term -> value
val v_univ : value
val v_forall : param:value_pat -> env:env -> body:term -> value
val v_self : self:value_pat -> env:env -> body:term -> value
val v_thunk : env:env -> term:term -> value

(* utilities *)
val repr : value -> value
val struct_ : value -> value_struct
val level : value -> Level.t
val same : value -> value -> bool
val init_forward : value -> to_:value -> unit
val lock_forward : value -> (unit -> 'a) -> 'a
val hole_lower : value -> to_:Level.t -> unit
val hole_link : value -> to_:value -> unit
val thunk_link : value -> to_:value -> unit
