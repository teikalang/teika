type term = private
  | T_type of { var : Var.t }
  | T_var of { var : Var.t; type_ : term }
  | T_arrow of { var : Var.t; param : term; return : term }
  | T_lambda of { var : Var.t; param : term; return : term }
  | T_apply of { lambda : term; arg : term }
  | T_sigma of { var : Var.t; left : term; right : term }
  | T_pair of { var : Var.t; left : term; right : term; annot : term }
  | T_unpair of { left : Var.t; right : Var.t; pair : term; return : term }

type t = term [@@deriving show]

val t_type : term
val t_var : var:Var.t -> type_:term -> term
val t_arrow : var:Var.t -> param:term -> return:term -> term
val t_lambda : var:Var.t -> param:term -> return:term -> term
val t_apply : lambda:term -> arg:term -> term
val t_sigma : var:Var.t -> left:term -> right:term -> term
val t_pair : var:Var.t -> left:term -> right:term -> annot:term -> term
val t_unpair : left:Var.t -> right:Var.t -> pair:term -> return:term -> term
