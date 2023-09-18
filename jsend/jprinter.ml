open Jtree
open Format

(* TODO: identation *)
let pp_block_syntax ~pp_wrapped_expression fmt block =
  let (JBlock { consts; return }) = block in
  List.iter
    (fun (var, value) ->
      fprintf fmt "const %a = %a;" Var.pp var pp_wrapped_expression value)
    consts;
  fprintf fmt "return %a;" pp_wrapped_expression return

let rec pp_expression_syntax ~pp_wrapped ~pp_call ~pp_atom ~pp_block fmt
    expression =
  let pp_expression_syntax fmt expression =
    pp_expression_syntax ~pp_wrapped ~pp_call ~pp_atom ~pp_block fmt expression
  in
  match expression with
  | JE_loc { expression; loc = _ } -> pp_expression_syntax fmt expression
  | JE_var { var } -> Var.pp fmt var
  | JE_generator { params; block } ->
      (* TODO: names on functions? *)
      let rec pp_params fmt params =
        match params with
        | [] -> ()
        | [ param ] -> fprintf fmt "%a" Var.pp param
        | param :: params -> fprintf fmt "%a, %a" Var.pp param pp_params params
      in
      fprintf fmt "function* (%a) { %a }" pp_params params pp_block block
  | JE_call { lambda; args } ->
      (* TODO: almost duplicated from params *)
      let rec pp_args fmt args =
        match args with
        | [] -> ()
        | [ arg ] -> fprintf fmt "%a" pp_wrapped arg
        | arg :: args -> fprintf fmt "%a, %a" pp_wrapped arg pp_args args
      in
      fprintf fmt "%a(%a)" pp_call lambda pp_args args
  | JE_yield { expression } -> fprintf fmt "yield %a" pp_call expression
  | JE_string { literal } ->
      (* TODO: proper JS escaping *)
      fprintf fmt "%S" literal

type prec = Wrapped | Call | Atom

let rec pp_expression prec fmt expression =
  let pp_wrapped fmt term = pp_expression Wrapped fmt term in
  let pp_call fmt term = pp_expression Call fmt term in
  let pp_atom fmt term = pp_expression Atom fmt term in
  let pp_block fmt block =
    pp_block_syntax ~pp_wrapped_expression:pp_wrapped fmt block
  in
  match (expression, prec) with
  | JE_loc { expression; loc = _ }, prec -> pp_expression prec fmt expression
  | (JE_var _ | JE_string _), (Wrapped | Call | Atom)
  | JE_call _, (Wrapped | Call)
  | (JE_generator _ | JE_yield _), Wrapped ->
      pp_expression_syntax ~pp_wrapped ~pp_call ~pp_atom ~pp_block fmt
        expression
  | JE_call _, Atom | (JE_generator _ | JE_yield _), (Call | Atom) ->
      fprintf fmt "(%a)" pp_wrapped expression

let pp_expression fmt expression = pp_expression Wrapped fmt expression
