type type_ =
  | T_type
  | T_var of { var : Var.t }
  | T_arrow of { param : type_; return : type_ }
  | T_forall of { var : Var.t; return : type_ }
  | T_pair of { left : type_; right : type_ }
  | T_exists of { var : Var.t; right : type_ }
  | T_alias of { type_ : type_ }
[@@deriving show]

type t = type_ [@@deriving show]

let t_type = T_type
let t_var ~var = T_var { var }
let t_arrow ~param ~return = T_arrow { param; return }
let t_forall ~var ~return = T_forall { var; return }
let t_pair ~left ~right = T_pair { left; right }
let t_exists ~var ~right = T_exists { var; right }
let t_alias ~type_ = T_alias { type_ }
