type term =
  | LT_loc of { term : term; loc : Location.t }
  (* x *)
  | LT_var of { var : Name.t }
  (* P -> T *)
  | LT_forall of { param : pat; return : term }
  (* P => m *)
  | LT_lambda of { param : pat; return : term }
  (* (m n) *)
  | LT_apply of { lambda : term; arg : term }
  (* P @-> m *)
  | LT_self of { bound : pat; body : term }
  (* P @=> m *)
  | LT_fix of { bound : pat; body : term }
  (* @m *)
  | LT_unroll of { term : term }
  (* %expand m *)
  | LT_expand of { term : term }
  (* P === m; n *)
  | LT_alias of { bound : pat; value : term; return : term }
  (* (m : T) *)
  | LT_annot of { term : term; annot : term }

and pat =
  | LP_loc of { pat : pat; loc : Location.t }
  (* x *)
  | LP_var of { var : Name.t }
  (* x : T *)
  | LP_annot of { pat : pat; annot : term }
[@@deriving show]
