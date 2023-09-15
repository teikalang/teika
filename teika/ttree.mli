type loc = Loc
type typed = Typed
type core = Core
type sugar = Sugar

type term =
  (* M[S]*)
  | TT_subst of { term : term; subst : subst }
  (* x/-n *)
  | TT_bound_var of { index : Index.t }
  (* x/+n *)
  | TT_free_var of { level : Level.t }
  (* TODO: I really don't like this ex_term *)
  (* _x/+n *)
  | TT_hole of { hole : term hole }
  (* (x : A) -> B *)
  | TT_forall of { param : typed_pat; return : term }
  (* (x : A) => e *)
  | TT_lambda of { param : typed_pat; return : term }
  (* l a *)
  | TT_apply of { lambda : term; arg : term }
  (* @self(x -> e)*)
  (* TODO: why core pat? *)
  | TT_self of { var : core_pat; body : term }
  (* @fix(x => e)*)
  (* TODO: why core pat? *)
  | TT_fix of { var : core_pat; body : term }
  (* @unroll(e)*)
  | TT_unroll of { term : term }
  (* @unfold(e)*)
  (* TODO: technically not sugar *)
  | TT_unfold of { term : term }
  (* x = t; u *)
  | TT_let of { bound : typed_pat; value : term; return : term }
  (* (v : T) *)
  | TT_annot of { term : term; annot : term }
  (* ".." *)
  | TT_string of { literal : string }
  (* @native("debug") *)
  | TT_native of { native : native }

(* TODO: this could probably be avoided if there were dependent types *)
and typed_pat = TPat of { pat : core_pat; type_ : term }

and core_pat =
  | TP_hole of { hole : core_pat hole }
  (* x *)
  | TP_var of { name : Name.t }

and 'a hole = { mutable link : 'a option }

and subst =
  (* id *)
  | TS_id
  (* open *)
  | TS_open of { to_ : term }
  (* close +l *)
  | TS_close of { from : Level.t }
  (* lift s *)
  | TS_lift of { subst : subst }
  (* s :: n *)
  | TS_cons of { subst : subst; next : subst }

and native = TN_debug [@@deriving show]

val nil_level : Level.t
val type_level : Level.t
val string_level : Level.t

(* utils *)
val tt_repr : term -> term
val tt_match : term -> term
val tp_repr : core_pat -> core_pat

(* constructors *)
val tt_type : term
val string_type : term
val tt_hole : unit -> term
val tp_hole : unit -> core_pat
