type term =
  | LT_loc of { term : term; loc : Location.t [@opaque] }
  | LT_var of { var : Name.t }
  | LT_forall of { param : pat; return : term }
  | LT_lambda of { param : pat; return : term }
  | LT_apply of { lambda : term; arg : term }
  | LT_self of { bound : pat; body : term }
  | LT_fix of { bound : pat; body : term }
  | LT_unroll of { term : term }
  | LT_expand of { term : term }
  | LT_alias of { bound : pat; value : term; return : term }
  | LT_annot of { term : term; annot : term }

and pat =
  | LP_loc of { pat : pat; loc : Location.t [@opaque] }
  | LP_var of { var : Name.t }
  | LP_annot of { pat : pat; annot : term }
[@@deriving show { with_path = false }]
