type term =
  | ST_loc of { term : term; loc : Location.t }
  | ST_parens of { term : term }
  | ST_var of { var : Name.t }
  | ST_arrow of { param : term; return : term }
  | ST_lambda of { param : term; return : term }
  | ST_apply of { lambda : term; arg : term }
  | ST_alias of { bound : term; value : term; return : term }
  | ST_annot of { term : term; annot : term }
[@@deriving show]
