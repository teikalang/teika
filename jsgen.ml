open Utils
open Ftree
open Jtree

(* TODO: generate minified already? *)
(* TODO: TMC *)
let rec gen_program next_funct program =
  match program with
  | P_lambda { context; body; next } ->
      let name = Var.funct @@ next_funct in
      let init = gen_lambda context body in
      let next_funct = Level.next next_funct in
      let body = gen_program next_funct next in
      JS_const { name; init } :: body
  | P_main { body } ->
      (* TODO: this is weird and hard coded from the Type global thing *)
      let next_var = Level.(next zero) in
      let name = Var.main in
      let block = gen_block next_var body in
      [ JS_const { name; init = JE_generator { params = []; body = block } } ]

and gen_lambda context body =
  let params = Level.init context (fun var -> Var.local var) in
  let param = Var.local context in
  let next_var = Level.next context in
  let body = gen_block next_var body in
  JE_arrow { params; body = JE_generator { params = [ param ]; body } }

and gen_block next_var block =
  match block with
  | B_define { arg; next } ->
      let name = Var.local next_var in
      let arg = gen_code arg in
      let next_var = Level.next next_var in
      let statement = JS_const { name; init = arg } in
      let (JBlock { statements; return }) = gen_block next_var next in
      JBlock { statements = statement :: statements; return }
  | B_return { code } ->
      let return = gen_code code in
      JBlock { statements = []; return }

and gen_code code =
  match code with
  | C_var { var } ->
      let var = Var.local var in
      JE_var { var }
  | C_apply { funct; arg } ->
      let funct = Var.funct funct in
      let funct = JE_var { var = funct } in
      let arg = Var.local arg in
      let arg = JE_var { var = arg } in
      (* TODO: too many args *)
      let call = JE_call { lambda = funct; args = [ arg ] } in
      JE_yield { expression = call }
  | C_closure { ptr; context } -> gen_closure context ptr
  | C_type_stub ->
      let var = Var.type_stub in
      JE_var { var }

and gen_closure context ptr =
  (* TODO: too many args *)
  let funct = Var.funct ptr in
  let funct = JE_var { var = funct } in
  let args = Level.init context (fun var -> JE_var { var = Var.local var }) in
  JE_call { lambda = funct; args }

(* TODO: this is bad *)
let initial = Level.(next zero)
