open Utils
open Jtree
open Format

let pp_var fmt var =
  (* TODO: those pp here are not prepared for codegen *)
  match var with
  | Var.Global name -> fprintf fmt "%s" @@ Name.repr name
  | Var.Funct level -> fprintf fmt "f_%a" Level.pp level
  | Var.Local level -> fprintf fmt "x_%a" Level.pp level

(* TODO: check those precedence *)
(* TODO: identation *)
let pp_statements_syntax ~pp_wrapped_expression fmt program =
  List.iter
    (fun const ->
      let (JS_const { name = var; init = value }) = const in
      fprintf fmt "const %a = %a;" pp_var var pp_wrapped_expression value)
    program

let pp_block_syntax ~pp_wrapped_expression fmt block =
  let (JBlock { statements; return }) = block in
  pp_statements_syntax ~pp_wrapped_expression fmt statements;
  fprintf fmt "return %a;" pp_wrapped_expression return

let rec pp_params fmt params =
  match params with
  | [] -> ()
  | [ param ] -> fprintf fmt "%a" pp_var param
  | param :: params -> fprintf fmt "%a, %a" pp_var param pp_params params

let rec pp_expression_syntax ~pp_wrapped ~pp_call ~pp_atom ~pp_block fmt
    expression =
  let pp_expression_syntax fmt expression =
    pp_expression_syntax ~pp_wrapped ~pp_call ~pp_atom ~pp_block fmt expression
  in
  match expression with
  | JE_loc { expression; loc = _ } -> pp_expression_syntax fmt expression
  | JE_var { var } -> pp_var fmt var
  | JE_arrow { params; body } ->
      fprintf fmt "(%a) => %a" pp_params params pp_call body
  | JE_generator { params; body } ->
      (* TODO: names on functions? *)
      fprintf fmt "function* (%a) { %a }" pp_params params pp_block body
  (* TODO: new precedence is the same as call? *)
  | JE_new { constructor } -> fprintf fmt "new %a" pp_call constructor
  | JE_call { lambda; args } ->
      (* TODO: almost duplicated from params *)
      let rec pp_args fmt args =
        match args with
        | [] -> ()
        | [ arg ] -> fprintf fmt "%a" pp_wrapped arg
        | arg :: args -> fprintf fmt "%a, %a" pp_wrapped arg pp_args args
      in
      fprintf fmt "%a(%a)" pp_call lambda pp_args args
  | JE_yield { expression } -> fprintf fmt "yield(%a)" pp_wrapped expression
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
  | (JE_new _ | JE_call _), (Wrapped | Call)
  | (JE_arrow _ | JE_generator _ | JE_yield _), Wrapped ->
      pp_expression_syntax ~pp_wrapped ~pp_call ~pp_atom ~pp_block fmt
        expression
  | (JE_new _ | JE_call _), Atom
  | (JE_arrow _ | JE_generator _ | JE_yield _), (Call | Atom) ->
      fprintf fmt "(%a)" pp_wrapped expression

let pp_wrapped_expression fmt expression = pp_expression Wrapped fmt expression

let pp_program fmt program =
  pp_statements_syntax ~pp_wrapped_expression fmt program

let pp_block fmt block = pp_block_syntax ~pp_wrapped_expression fmt block
let pp_expression fmt expression = pp_expression Wrapped fmt expression
