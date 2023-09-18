open Utree
open Jtree

let rec emit_term : Utree.term -> expression =
 fun term ->
  match term with
  (* TODO: sourcemap *)
  | UT_loc { term; loc = _ } -> emit_term term
  | UT_var { var } -> JE_var { var }
  | UT_lambda { param; return } ->
      let params = [ param ] in
      let block = emit_block ~consts:[] return in
      JE_generator { params; block }
  | UT_apply { lambda; arg } ->
      let lambda = emit_term lambda in
      let args = [ emit_term arg ] in
      let call = JE_call { lambda; args } in
      (* TODO: test optimization, if instanceof before yield *)
      JE_yield { expression = call }
  | UT_let _ ->
      (* TODO: weird to ignore UT_let like this *)
      let block = emit_block ~consts:[] term in
      let wrapper = JE_generator { params = []; block } in
      let call = JE_call { lambda = wrapper; args = [] } in
      JE_yield { expression = call }
  | UT_string { literal } -> JE_string { literal }
  | UT_external { external_ } -> translate_external external_

and emit_block ~consts return =
  match return with
  | UT_loc { term = return; loc = _ } -> emit_block ~consts return
  | UT_let { var; value; return } ->
      let value = emit_term value in
      let consts = (var, value) :: consts in
      emit_block ~consts return
  | UT_var _ | UT_lambda _ | UT_apply _ | UT_string _ | UT_external _ ->
      let return = emit_term return in
      let consts = List.rev consts in
      JBlock { consts; return }

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
