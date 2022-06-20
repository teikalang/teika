type identifier = string
type number = string

type expr = LE of { loc : Location.t; desc : expr_desc }

and expr_desc =
  (* TODO: *)
  (* _ *)
  (* | LE_hole *)
  (* x *)
  | LE_var of identifier
  (* 123 *)
  | LE_number of number
  (* pat => expr *)
  | LE_lambda of { param : pat; body : expr }
  (* expr expr *)
  | LE_apply of { lambda : expr; arg : expr }
  (* pat = expr; expr *)
  | LE_let of { bind : expr_bind; body : expr }
  (* { pat = expr; } *)
  | LE_record of expr_bind list
  (* expr : type *)
  | LE_annot of { value : expr; annot : annot }
  (* type_ *)
  | LE_type of type_

(* TODO: bind directly on parser? *)
and expr_bind = LE_bind of { loc : Location.t; bound : pat; value : expr }
and type_ = LT of { loc : Location.t; desc : type_desc }

and type_desc =
  (* TODO: *)
  (* _ *)
  (* | LE_hole *)
  (* X *)
  | LT_var of identifier
  (* (X: kind) -> typ*)
  | LT_forall of { var : identifier; kind : kind; body : type_ }
  (* typ -> typ *)
  | LT_arrow of { param : type_; body : type_ }
  (* TODO: should LT_record allow any pattern? *)
  (* { X: typ; } *)
  | LT_record of type_bind list

and type_bind =
  | LT_bind of { loc : Location.t; var : identifier; type_ : type_ }

(* TODO: can pattern be unified back on expr? *)
and pat = LP of { loc : Location.t; desc : pat_desc }

and pat_desc =
  (* TODO: any*)
  (* _ *)
  (* | LP_any *)
  (* x *)
  | LP_var of identifier
  | LP_record of pat list
  (* pat : type *)
  | LP_annot of { pat : pat; annot : annot }

(* TODO: is this syntatic difference really needed? *)
and annot = LA_type of expr | LA_kind of kind
and kind = LK of { loc : Location.t; desc : kind_desc }

and kind_desc =
  (* * *)
  | LK_type
  (* kind -> kind *)
  | LK_arrow of { param : kind; body : kind }
