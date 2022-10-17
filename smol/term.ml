type term =
  | T_type
  | T_var of { var : Var.t; type_ : term }
  | T_arrow of { var : Var.t; param : term; return : term }
  | T_lambda of { var : Var.t; param : term; return : term }
  | T_apply of { lambda : term; arg : term }
  | T_sigma of { var : Var.t; left : term; right : term }
  | T_pair of { var : Var.t; left : term; right : term; annot : term }
  | T_unpair of { left : Var.t; right : Var.t; pair : term; return : term }
[@@deriving show { with_path = false }]

type t = term [@@deriving show]

let t_type = T_type
let t_var ~var ~type_ = T_var { var; type_ }
let t_arrow ~var ~param ~return = T_arrow { var; param; return }
let t_lambda ~var ~param ~return = T_lambda { var; param; return }
let t_apply ~lambda ~arg = T_apply { lambda; arg }
let t_sigma ~var ~left ~right = T_sigma { var; left; right }
let t_pair ~var ~left ~right ~annot = T_pair { var; left; right; annot }
let t_unpair ~left ~right ~pair ~return = T_unpair { left; right; pair; return }
