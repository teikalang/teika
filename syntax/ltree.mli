open Utils

(* TODO: location stack? *)
type term = LTerm of { term : term_syntax; loc : Location.t }

and term_syntax =
  (* x *)
  | LT_var of { var : Name.t }
  (* @x(e) *)
  | LT_extension of { extension : Name.t; payload : term }
  (* (x : A) -> (z : B) *)
  | LT_forall of { param : pat; return : term }
  (* (x : A) => e *)
  | LT_lambda of { param : pat; return : term }
  (* l a *)
  | LT_apply of { lambda : term; arg : term }
  (* p : A; r *)
  | LT_hoist of { bound : pat; annot : term; return : term }
  (* p = v; r *)
  | LT_let of { bound : pat; value : term; return : term }
  (* (v : T) *)
  | LT_annot of { term : term; annot : term }
  (* ".." *)
  | LT_string of { literal : string }

and pat = LPat of { pat : pat_syntax; loc : Location.t }

and pat_syntax =
  (* x *)
  | LP_var of { var : Name.t }
  (* (p : T) *)
  | LP_annot of { pat : pat; annot : term }
[@@deriving show]

val lterm : loc:Location.t -> term_syntax -> term
val lpat : loc:Location.t -> pat_syntax -> pat
