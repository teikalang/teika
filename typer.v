Definition Index := nat.
Definition Level := nat.

Inductive Code : Type := | X
with Instr : Type :=
  | I_var (var : Index)
  | I_forall
  | I_lambda
  | I_apply
  | I_return
with Closure : Type :=

with Value : Type :=
  | V_var (var : Level) (args : list Value)
  | V_forall (param : Value) (env : list Value) ()
  | V_closure (env : list Value) (body : Code).

