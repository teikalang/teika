type loc = Loc
type typed = Typed
type core = Core
type sugar = Sugar

type term =
  | TTerm of { desc : term_desc; type_ : term }
  | TType of { desc : term_desc }

and term_desc =
  (* M[S]*)
  | TT_subst of { term : term; subst : subst }
  (* x/-n *)
  | TT_bound_var of { index : Index.t }
  (* x/+n *)
  (* TODO: this alias is a hack *)
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
  (* -f := N *)
  | TS_subst_bound of { to_ : term }
  (* +f `open` -t *)
  | TS_close_free of { from : Level.t; to_ : Index.t }

and native = TN_debug

val nil_level : Level.t
val type_level : Level.t
val string_level : Level.t

(* utils *)
val tt_match : term -> term_desc
val tp_repr : core_pat -> core_pat

val tt_map_desc :
  term -> (wrap:(term_desc -> term) -> term -> term_desc -> 'a) -> 'a

(* constructors *)
val tt_type : term
val string_type : term
val tt_hole : unit -> term_desc
val tp_hole : unit -> core_pat
