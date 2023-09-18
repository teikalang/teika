open Utree
open Jtree

let emit_curry function_ =
  JE_call { lambda = JE_var { var = Var.curry }; args = [ function_ ] }

let rec emit_term : Utree.term -> expression =
 fun term ->
  match term with
  (* TODO: sourcemap *)
  | UT_loc { term; loc = _ } -> emit_term term
  | UT_var { var } -> JE_var { var }
  | UT_lambda _ ->
      (* TODO: weird to ignore UT_lambda like this *)
      emit_curry @@ emit_generator ~params:[] term
  | UT_apply _ ->
      (* TODO: weird to ignore UT_apply like this *)
      let call = emit_call ~args:[] term in
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

and emit_generator ~params return =
  (* TODO: is this transformation desired?
      Does it changes performance behaviour *)
  (* TODO: too many params *)
  match return with
  | UT_loc { term = return; loc = _ } -> emit_generator ~params return
  | UT_lambda { param; return } ->
      let params = param :: params in
      emit_generator ~params return
  | UT_var _ | UT_apply _ | UT_let _ | UT_string _ | UT_external _ ->
      let params = List.rev params in
      let block = emit_block ~consts:[] return in
      JE_generator { params; block }

and emit_call ~args lambda =
  (* TODO: too many args? *)
  match lambda with
  | UT_loc { term = lambda; loc = _ } -> emit_call ~args lambda
  | UT_apply { lambda; arg } ->
      let arg = emit_term arg in
      let args = arg :: args in
      emit_call ~args lambda
  | UT_var _ | UT_lambda _ | UT_let _ | UT_string _ | UT_external _ ->
      let lambda = emit_term lambda in
      JE_call { lambda; args }

and emit_block ~consts return =
  match return with
  | UT_loc { term = return; loc = _ } -> emit_block ~consts return
  | UT_let { var; value; return } ->
      let value = emit_term value in
      let consts = (var, value) :: consts in
      emit_block ~consts return
  | UT_apply _ ->
      (* tco *)
      let return =
        let return = emit_call ~args:[] return in
        let constructor =
          JE_call { lambda = JE_var { var = Var.jmp }; args = [ return ] }
        in
        JE_new { constructor }
      in
      let consts = List.rev consts in
      JBlock { consts; return }
  | UT_var _ | UT_lambda _ | UT_string _ | UT_external _ ->
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
