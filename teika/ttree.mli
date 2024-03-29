open Syntax

type term =
  (* x/-n *)
  | TT_bound_var of { index : Index.t }
  (* x/+n *)
  | TT_free_var of { level : Level.t }
  (* TODO: I really don't like this ex_term *)
  (* (x : A) -> B *)
  | TT_forall of { param : typed_pat; return : term }
  (* (x : A) => e *)
  | TT_lambda of { param : typed_pat; return : term }
  (* l a *)
  | TT_apply of { lambda : term; arg : term }
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

and core_pat = (* x *)
  | TP_var of { name : Name.t }

and subst =
  (* id *)
  | TS_id
  (* open -t *)
  | TS_open of { to_ : Level.t }
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

(* constructors *)
val tt_type : term
val string_type : term
