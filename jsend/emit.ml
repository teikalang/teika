open Utree
open Jtree

let rec emit_term : Utree.term -> expression =
 fun term ->
  match term with
  (* TODO: sourcemap *)
  | UT_loc { term; loc = _ } -> emit_term term
  | UT_var { var } -> JE_var { var }
  | UT_lambda { param; return } ->
      let return = emit_term return in
      JE_generator { param; return }
  | UT_apply { lambda; arg } ->
      let lambda = emit_term lambda in
      let arg = emit_term arg in
      let call = JE_call { lambda; arg } in
      (* TODO: test optimization, if instanceof before yield *)
      JE_yield { expression = call }
  | UT_string { literal } -> JE_string { literal }
  | UT_external { external_ } -> translate_external external_

and translate_external : external_ -> expression =
 fun external_ ->
  let var =
    match external_ with
    | UE_type -> Var.type_
    | UE_fix -> Var.fix
    | UE_unit -> Var.unit
    | UE_debug -> Var.debug
  in
  JE_var { var }
