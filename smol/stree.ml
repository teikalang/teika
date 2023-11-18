type ty_term = ST_typed of { term : term; type_ : term }

and term =
  | ST_loc of { term : term; loc : Location.t [@opaque] }
  | ST_free_var of { level : Level.t }
  | ST_bound_var of { index : Index.t }
  | ST_forall of { param : ty_pat; return : term }
  | ST_lambda of { param : ty_pat; return : term }
  | ST_apply of { lambda : term; arg : term }
  (* TODO: self being only pat is weird *)
  | ST_self of { self : pat; body : term }
  | ST_fix of { self : ty_pat; body : term }
  | ST_unroll of { term : term }
  | ST_let of { bound : ty_pat; value : term; return : term }
  | ST_annot of { term : term; annot : term }

and ty_pat = SP_typed of { pat : pat; type_ : term }

and pat =
  | SP_loc of { pat : pat; loc : Location.t [@opaque] }
  (* TODO: extract Syntax.Name *)
  | SP_var of { var : Syntax.Name.t }
  | SP_erasable of { pat : pat }
  (* TODO: SP_unroll *)
  | SP_annot of { pat : pat; annot : term }
[@@deriving show]
