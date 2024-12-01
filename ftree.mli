open Utils

type funct_ptr = Level.t
type var = Level.t

type program =
  (* f = (...ctx) => x => B; P *)
  | P_lambda of { context : Level.t; body : block; next : program }
  (* main = () => B; main() *)
  | P_main of { body : block }

and block =
  (* x := C; B *)
  | B_define of { arg : code; next : block }
  (* C *)
  | B_return of { code : code }

and code =
  (* x *)
  | C_var of { var : var }
  (* x(y) *)
  | C_apply of { funct : var; arg : var }
  (* f(...ctx) *)
  | C_closure of { ptr : funct_ptr; context : Level.t }
  (* _ : Type *)
  | C_type_stub
