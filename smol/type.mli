type type_ = private
  | T_type
  | T_var of { var : Var.t }
  | T_arrow of { param : type_; return : type_ }
  | T_forall of { var : Var.t; return : type_ }
  | T_pair of { left : type_; right : type_ }
  | T_exists of { var : Var.t; right : type_ }
  | T_alias of { type_ : type_ }

type t = type_ [@@deriving show]

val t_type : type_
val t_var : var:Var.t -> type_
val t_arrow : param:type_ -> return:type_ -> type_
val t_forall : var:Var.t -> return:type_ -> type_
val t_pair : left:type_ -> right:type_ -> type_
val t_exists : var:Var.t -> right:type_ -> type_
val t_alias : type_:type_ -> type_
