type type_ =
  | T_type
  | T_var of { var : Var.t }
  | T_arrow of { var : Var.t; param : type_; return : type_ }
  | T_pair of { var : Var.t; left : type_; right : type_ }
  | T_alias of { type_ : type_ }
[@@deriving show]

type t = type_ [@@deriving show]

let t_type = T_type
let t_var ~var = T_var { var }
let t_arrow ~var ~param ~return = T_arrow { var; param; return }
let t_pair ~var ~left ~right = T_pair { var; left; right }
let t_alias ~type_ = T_alias { type_ }
