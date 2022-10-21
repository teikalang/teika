type term =
  | T_type of { var : Var.t }
  | T_var of { var : Var.t; type_ : term }
  | T_arrow of { var : Var.t; param : term; return : term }
  | T_lambda of { var : Var.t; param : term; return : term }
  | T_apply of { lambda : term; arg : term }
  | T_sigma of { var : Var.t; left : term; right : term }
  | T_pair of { var : Var.t; left : term; right : term; annot : term }
  | T_unpair of { left : Var.t; right : Var.t; pair : term; return : term }

type t = term

let t_type =
  let var = Var.type_ in
  T_type { var }

let t_var ~var ~type_ = T_var { var; type_ }
let t_arrow ~var ~param ~return = T_arrow { var; param; return }
let t_lambda ~var ~param ~return = T_lambda { var; param; return }
let t_apply ~lambda ~arg = T_apply { lambda; arg }
let t_sigma ~var ~left ~right = T_sigma { var; left; right }
let t_pair ~var ~left ~right ~annot = T_pair { var; left; right; annot }
let t_unpair ~left ~right ~pair ~return = T_unpair { left; right; pair; return }

let rec pp fmt term =
  let open Format in
  (* TODO: this doesn't handle shadowing *)
  let var_pp fmt var =
    let name = Var.name var in
    fprintf fmt "%s" (Name.repr name)
  in
  match term with
  | T_type { var } | T_var { var; type_ = _ } -> fprintf fmt "%a" var_pp var
  | T_arrow { var; param; return } ->
      fprintf fmt "(%a : %a) -> %a" var_pp var pp param pp return
  | T_lambda { var; param; return } ->
      fprintf fmt "(%a : %a) => %a" var_pp var pp param pp return
  | T_apply { lambda; arg } -> (
      (match lambda with
      | T_type _ | T_var _ | T_sigma _ | T_pair _ | T_apply _ ->
          fprintf fmt "%a" pp lambda
      | T_arrow _ | T_lambda _ | T_unpair _ -> fprintf fmt "(%a)" pp lambda);
      match arg with
      | T_type _ | T_var _ | T_sigma _ | T_pair _ -> fprintf fmt " %a" pp arg
      | T_arrow _ | T_lambda _ | T_apply _ | T_unpair _ ->
          fprintf fmt " (%a)" pp arg)
  | T_sigma { var; left; right } ->
      fprintf fmt "(%a : %a, %a)" var_pp var pp left pp right
  | T_pair { var; left; right; annot } ->
      fprintf fmt "(%a : %a, %a : %a)" var_pp var pp left pp right pp annot
  | T_unpair { left; right; pair; return } -> (
      match return with
      | T_type _ | T_var _ | T_arrow _ | T_lambda _ | T_apply _ | T_sigma _
      | T_pair _ ->
          fprintf fmt "(%a, %a) = %a; %a" var_pp left var_pp right pp pair pp
            return
      | T_unpair _ ->
          fprintf fmt "(%a, %a) = (%a); %a" var_pp left var_pp right pp pair pp
            return)

let show term = Format.asprintf "%a" pp term
