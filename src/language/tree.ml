type identifier = string
type number = string

type expr = { le_loc : Location.t; le_desc : expr_desc }

and expr_desc =
  (* TODO: *)
  (* _ *)
  (* | LE_hole *)
  (* x *)
  | LE_var of identifier
  (* 123 *)
  | LE_number of number
  (* pat -> expr *)
  | LE_arrow of { param : pat; body : expr }
  (* pat => expr *)
  | LE_lambda of { param : pat; body : expr }
  (* expr expr *)
  | LE_apply of { lambda : expr; arg : expr }
  (* pat = expr; expr *)
  | LE_let of { bound : pat; value : expr; body : expr }
  (* { pat = expr; } *)
  | LE_record of le_record_bind list
  (* * *)
  | LE_asterisk
  (* expr : type *)
  | LE_annot of { value : expr; type_ : expr }

and le_record_bind =
  | LE_record_bind of { loc : Location.t; bound : pat; value : expr option }

(* TODO: can pattern be unified back on expr? *)
and pat = { lp_loc : Location.t; lp_desc : pat_desc }

and pat_desc =
  (* TODO: any*)
  (* _ *)
  (* | LP_any *)
  (* x *)
  | LP_var of identifier
  | LP_record of lp_record_bind list
  (* pat : type *)
  | LP_annot of { pat : pat; type_ : expr }

and lp_record_bind = LP_record_bind of { loc : Location.t; bound : pat }
