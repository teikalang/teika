type type_ = private
  | T_type
  | T_var of { var : Var.t }
  | T_arrow of { var : Var.t; param : type_; return : type_ }
  | T_pair of { var : Var.t; left : type_; right : type_ }
  | T_alias of { type_ : type_ }

type t = type_ [@@deriving show]

val t_type : type_
val t_var : var:Var.t -> type_
val t_arrow : var:Var.t -> param:type_ -> return:type_ -> type_
val t_pair : var:Var.t -> left:type_ -> right:type_ -> type_
val t_alias : type_:type_ -> type_
