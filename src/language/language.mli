type identifier = string
type number = string

type expr = { le_loc : Location.t; le_desc : expr_desc }

and expr_desc =
  | LE_var of identifier
  | LE_number of number
  | LE_arrow of { param : pat; body : expr }
  | LE_lambda of { param : pat; body : expr }
  | LE_apply of { lambda : expr; arg : expr }
  | LE_let of { bound : pat; value : expr; body : expr }
  | LE_record of le_record_bind list
  | LE_asterisk
  | LE_annot of { value : expr; type_ : expr }

and le_record_bind =
  | LE_record_bind of { loc : Location.t; bound : pat; value : expr option }

and pat = { lp_loc : Location.t; lp_desc : pat_desc }

and pat_desc =
  | LP_var of identifier
  | LP_record of lp_record_bind list
  | LP_annot of { pat : pat; type_ : expr }

and lp_record_bind = LP_record_bind of { loc : Location.t; bound : pat }

val interpret_expr : Syntax.term -> expr
val interpret_pat : Syntax.term -> pat
