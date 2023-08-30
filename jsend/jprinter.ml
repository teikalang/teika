open Jtree
open Format

(* TODO: identation *)
let rec pp_expression_syntax ~pp_wrapped ~pp_call ~pp_atom fmt expression =
  let pp_expression_syntax fmt expression =
    pp_expression_syntax ~pp_wrapped ~pp_call ~pp_atom fmt expression
  in
  match expression with
  | JE_loc { expression; loc = _ } -> pp_expression_syntax fmt expression
  | JE_var { var } -> Var.pp fmt var
  | JE_generator { param; return } ->
      (* TODO: names on functions? *)
      fprintf fmt "function* (%a) { return %a; }" Var.pp param pp_wrapped return
  | JE_call { lambda; arg } ->
      fprintf fmt "%a(%a)" pp_call lambda pp_wrapped arg
  | JE_yield { expression } -> fprintf fmt "yield %a" pp_call expression
  | JE_string { literal } ->
      (* TODO: proper JS escaping *)
      fprintf fmt "%S" literal

type prec = Wrapped | Call | Atom

let rec pp_expression prec fmt expression =
  let pp_wrapped fmt term = pp_expression Wrapped fmt term in
  let pp_call fmt term = pp_expression Call fmt term in
  let pp_atom fmt term = pp_expression Atom fmt term in
  match (expression, prec) with
  | JE_loc { expression; loc = _ }, prec -> pp_expression prec fmt expression
  | (JE_var _ | JE_string _), (Wrapped | Call | Atom)
  | JE_call _, (Wrapped | Call)
  | (JE_generator _ | JE_yield _), Wrapped ->
      pp_expression_syntax ~pp_wrapped ~pp_call ~pp_atom fmt expression
  | JE_call _, Atom | (JE_generator _ | JE_yield _), (Call | Atom) ->
      fprintf fmt "(%a)" pp_wrapped expression

let pp_expression fmt expression = pp_expression Wrapped fmt expression
