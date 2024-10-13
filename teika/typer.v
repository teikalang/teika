Definition Index := nat.
Definition Level := nat.

Inductive Term : Type :=
  | T_annot : forall (term : Term) (annot : Term), Term 
  | T_forall : forall (param : Pat) (body : Term), Term
  | T_inter : forall (left : Pat) (right : Term), Term
  | T_var : forall (var : Index), Term
  | T_let : forall (bound : Pat) (arg : Term) (body : Term), Term
  | T_apply : forall (funct : Term) (arg : Term), Term
  | T_lambda : forall (param : Pat) (body : Term), Term
with Pat : Type :=
  | P_annot : forall (pat : Pat) (annot : Term), Pat
  | P_var : Pat.

Module Inner.
  Definition Name := nat.
  Inductive Value :=
    | V_apply : forall (var : Level) (args : list Value), Value
    | V_forall : forall (param : Value) (body : Closure), Value
    | V_inter : forall (left : Value) (right : Closure), Value
    | V_lambda : forall (body : Closure), Value
    | V_fix : forall (body : Closure), Value
  with Closure :=
    | Clos : forall (env : list Value) (body : Term), Closure.

  Lemma open : Value -> Closure -> Value.
    admit.
  Admitted.
  
  
  Inductive Equal : Value -> Value -> Prop :=
    | E_refl : forall value,
      Equal value value.

  Inductive Coerce : Value -> Value -> Value -> Prop :=
    | C_refl : forall term type, Coerce term type type
    | C_unfold_self : forall term left right expected,
      Equal left (V_inter left right) ->
      Coerce term (open term right) expected ->
      Coerce term (V_inter left right) expected.

  Inductive Equal_result :=
    | E_equal : forall (gas : nat), Equal_result
    | E_not_equal : Equal_result
    | E_out_of_gas : Equal_result.

  Fixpoint equal gas left right :=
    match gas with
    | 0 => E_out_of_gas
    | S gas =>
      match left, right with
      | V_univ, V_univ => E_equal gas
      | V_forall left_param left_body, V_forall right_param right_body =>
        match equal gas left_param right_param with
        | E_equal gas => equal gas left_body right_body
        | E_not_equal => E_not_equal
        | E_out_of_gas => E_out_of_gas
        end
      | V_inter left_param left_body, V_inter right_param right_body =>
        match equal gas left_param right_param with
        | E_equal gas => equal gas left_body right_body
        | E_not_equal => E_not_equal
        | E_out_of_gas => E_out_of_gas
        end
      | V_univ, (V_forall _ _ | V_inter _ _) => E_not_equal
      | V_forall _ _, (V_univ | V_inter _ _) => E_not_equal
      | V_inter _ _, (V_univ | V_forall _ _) => E_not_equal
      end
    end.
End Inner.

Inductive Context : Prop.
  (* | C_annot : forall (term : Term) (annot : Term), Term 
  | C_univ : Term
  | C_forall : forall (param : Term) (body : Term), Term
  | C_inter : forall (left : Term) (right : Term), Term
  | C_var : forall (var : Index), Term
  | C_let : forall (arg : Term) (body : Term), Term
  | C_apply : forall (funct : Term) (arg : Term), Term
  | C_lambda : forall (body : Term), Term. *)
Definition plug (ctx : Context) (term : Term) : Term.
  admit.
Admitted.

Inductive Reduction : Context -> Term -> Term -> Prop :=
  (* | R_subst : forall arg body,
    Reduction (T_let arg (body x)) (T_let arg body) *)
  | R_beta : forall ctx body arg,
    Reduction ctx (plug ctx (T_apply (T_lambda body) arg))
      (plug ctx (T_let arg body)).
  
Inductive Env := 
  | Empty
  | Bound : forall (head : Term) (tail : Env), Env.

Inductive Equal : Term -> Term -> Prop :=
  | E_refl : forall term, Equal term term
  | E_left :
    forall left_ctx left_bef left_aft right,
    Reduction left_ctx left_bef left_aft ->
    Equal (plug left_ctx left_aft) right ->
    Equal (plug left_ctx left_bef) right.


Inductive Infer : Env -> Term -> Term -> Prop :=
  | I_univ : forall env,
    Infer env T_univ T_univ
  | I_forall : forall env param body body_type,
    Infer env param T_univ -> Infer (Bound param env) body body_type ->
    Infer env (T_forall param body) T_univ
  | I_inter : forall env left right left_type right_type,
    Infer env left left_type -> Infer env right right_type ->
    Infer env (T_inter left right) T_univ
  (* | Valid_var_head : forall head_env head tail,
    Valid (Cons head_env head tail) (T_var 0)
  | Valid_var_succ : forall head_env head tail rest,
    Valid tail (T_var rest) ->
    Valid (Cons head_env head tail) (T_var (1 + rest)) *)
  | I_let : forall env arg body arg_type body_type,
    Infer env arg arg_type -> Infer (Cons env arg_type env) body body_type ->
    Infer env (T_let arg body) (T_let arg_type body_type)
  | I_apply : forall env funct arg param body_type,
    Infer env funct (T_forall param body_type) -> Infer env arg param ->
    Infer env (T_apply funct arg) (T_let arg body_type)
  | I_lambda : forall env body param body_type,
    Infer (Cons env param env) body body_type ->
    Infer env (T_lambda body) (T_forall param body_type).

(* TODO: better than Valid *)
Inductive Valid : Env -> Term -> Prop :=
  (* | Valid_annot : forall env term annot,
    Valid env term -> Valid env annot ->
    Valid env (T_annot term annot) *)
  | Valid_var_head : forall head_env head tail,
    Valid (Cons head_env head tail) (T_var 0)
  | Valid_var_succ : forall head_env head tail rest,
    Valid tail (T_var rest) ->
    Valid (Cons head_env head tail) (T_var (1 + rest))
  | Valid_let : forall env arg body,
    Valid (Cons env arg env) body ->
    Valid env (T_let arg body)
  | Valid_apply : forall env funct arg,
    Valid env funct -> Valid env arg ->
    Valid env (T_apply funct arg)
  | Valid_lambda : forall env body,
    Valid (Cons Empty (T_var 0) env) body ->
    Valid env (T_lambda body).

Lemma lookup_empty_var_false {var} (H : Valid Empty (T_var var)) : False.
  inversion H.
Qed.

Fixpoint lookup env var : Valid env (T_var var) -> (Env * Term) :=
  match env with
  | Empty =>  
    fun valid =>
      match lookup_empty_var_false valid with
      end
  | Cons head_env head tail =>
      match var with
      | 0 => fun valid => (head_env, head)
      | S rest => fun valid =>
        lookup tail rest (ltac:(inversion valid; intuition))
      end
  end.

(* Inductive Is_value : Index -> Term -> Prop :=
  | V_var : forall d var, var < d ->
    Is_value d (T_var var)
  | V_apply : forall d funct arg,
    Is_value d funct -> Is_value d arg ->
    Is_value d (T_apply funct arg)
  | V_lambda : forall d body,
    Is_value (1 + d) body ->
    Is_value d (T_lambda body)
  | V_univ : forall d,
    Is_value d T_univ
  | V_forall : forall d param body,
    Is_value d param -> Is_value (1 + d) body ->
    Is_value d (T_forall param body)
  | V_inter : forall d left right,
    Is_value d left -> Is_value (1 + d) right ->
    Is_value d (T_inter left right)
  | V_value : forall d var,
    Is_value d (T_value var).
Definition Value := {t : Term | Is_value 0 t}. *)

Fixpoint step env term : Valid env term -> (Env * Term) :=
  match term with
  | T_var var => fun valid => lookup env var valid 
  | T_let arg body => fun valid =>
    match valid with
    | Valid_let _ _ _ valid_body =>
      step (Cons env arg env) body valid_body
    end
  | T_apply funct arg => fun valid =>

    (env, funct)
  | T_lambda body => _
  end.

(* Definition eval (gas : nat) (env : Env) (term : Term) := *)
