type expr = private TE of { type_ : Type.t; desc : expr_desc }

and expr_desc = private
  | TE_var of { var : Var.t }
  | TE_lambda of { var : Var.t; param : type_; return : expr }
  | TE_forall of { var : Var.t; return : expr }
  | TE_apply of { funct : expr; arg : expr }
  | TE_pair of { left : bind; right : bind }
  | TE_exits of { var : Var.t; right : bind }
  | TE_type of { type_ : type_ }
  | TE_let of { bind : bind; return : expr }
  | TE_annot of { expr : expr; type_ : type_ }

and bind = private TB of { type_ : Type.t; var : Var.t; value : expr }
and type_ = private TT of { type_ : Type.t; desc : type_desc }

and type_desc = private
  | TT_var of { var : Var.t }
  | TT_arrow of { param : type_; return : type_ }
  | TT_forall of { var : Var.t; return : type_ }
  | TT_pair of { left : type_; right : type_ }
  | TT_exists of { var : Var.t; right : type_ }

(* expr *)
val te_var : Type.t -> var:Var.t -> expr
val te_lambda : var:Var.t -> param:type_ -> return:expr -> expr
val te_forall : var:Var.t -> return:expr -> expr
val te_apply : Type.t -> funct:expr -> arg:expr -> expr
val te_pair : left:bind -> right:bind -> expr
val te_exists : var:Var.t -> right:bind -> expr
val te_type : type_:type_ -> expr
val te_let : bind:bind -> return:expr -> expr
val te_annot : expr:expr -> type_:type_ -> expr

(* bind *)
val tb : var:Var.t -> value:expr -> bind

(* type *)
val tt_var : Type.t -> var:Var.t -> type_
val tt_arrow : param:type_ -> return:type_ -> type_
val tt_forall : var:Var.t -> return:type_ -> type_
val tt_pair : left:type_ -> right:type_ -> type_
val tt_exists : var:Var.t -> right:type_ -> type_
