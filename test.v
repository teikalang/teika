Require Import String.
Require Import List.

Import ListNotations.
Open Scope string_scope.

Inductive Term : Type :=
  | T_var (var : string)
  | T_lam (param : string) (body : Term)
  | T_app (lam : Term) (arg : Term).

Fixpoint subst from to_ term :=
  match term with
  | T_var var =>
    match String.eqb from var with
    | true => to_
    | false => T_var var
    end
  | T_lam param body =>
    let body :=
      match String.eqb from param with
      | true => body
      | false => subst from to_ body
      end in
    T_lam param body
  | T_app lam arg =>
    let lam := subst from to_ lam in
    let arg := subst from to_ arg in
    T_app lam arg
  end.

Definition Context := list string.
Fixpoint append (l r : Context) : Context :=
  match l with
  | [] => r
  | hd :: tl => hd :: append tl r
  end.

Fixpoint free_in_ctx var (ctx : Context) :=
  match ctx with
  | [] => false
  | hd :: tl =>
    match String.eqb hd var with
    | true => true
    | false => free_in_ctx var tl
    end
  end.

Lemma append_eq_empty_split {ctx ctx'}
  : append ctx ctx' = [] -> ctx = [] /\ ctx' = [].
  intros eq.
  induction ctx.
  exact (conj eq_refl eq).
  inversion eq.
Qed.

Lemma free_x_is_false_but_y_ctx_implies_y_neq_x {ctx x y}
  : free_in_ctx x (y :: ctx) = false -> String.eqb y x = false.
  intros x_not_free_in_y_ctx.
  simpl in x_not_free_in_y_ctx.
  destruct (String.eqb_spec y x).
  inversion x_not_free_in_y_ctx.
  reflexivity.
Qed.
Lemma adding_irrelevant_stil_free {ctx x y}
  : free_in_ctx x ctx = false -> x <> y ->
    free_in_ctx x (y :: ctx) = false.
  intros x_not_free_in_ctx x_neq_y.
  simpl; rewrite String.eqb_sym.
  destruct (String.eqb_neq x y).
  rewrite (H0 x_neq_y).
  exact x_not_free_in_ctx.
Qed.
Lemma free_x_is_false_in_y_ctx_but_without_y {ctx x y}
  : free_in_ctx x (y :: ctx) = false -> free_in_ctx x ctx = false.
  intros x_not_free_in_y_ctx.
  simpl in x_not_free_in_y_ctx.
  destruct (String.eqb_spec y x).
  inversion x_not_free_in_y_ctx.
  exact x_not_free_in_y_ctx.
Qed.
Lemma split_not_free_in_both {ctx ctx' x}
  : free_in_ctx x (append ctx ctx') = false ->
    free_in_ctx x ctx = false /\ free_in_ctx x ctx' = false.
    intros x_free_in_ctx_and_ctx'.
    generalize dependent x.
    generalize dependent ctx'.
    induction ctx.
    intros ctx' x x_free_in_ctx'.
    exact (conj eq_refl x_free_in_ctx').
    intros ctx' x x_free_in_a_ctx_and_ctx'.
    simpl; rewrite (
      free_x_is_false_but_y_ctx_implies_y_neq_x x_free_in_a_ctx_and_ctx'
    ).
    apply IHctx.
    exact (free_x_is_false_in_y_ctx_but_without_y x_free_in_a_ctx_and_ctx').
Qed.

Inductive Linear : Context -> Term -> Type :=
  | L_var var : Linear [var] (T_var var)
  | L_lam {lam_ctx} param
    {body_term} (body : Linear (param :: lam_ctx) body_term)
    (param_not_free_in_lam_ctx : free_in_ctx param lam_ctx = false)
    : Linear lam_ctx (T_lam param body_term)
  | L_app {lam_ctx lam_term} (lam : Linear lam_ctx lam_term)
    {arg_ctx arg_term} (arg : Linear arg_ctx arg_term)
    : Linear (append lam_ctx arg_ctx) (T_app lam_term arg_term).

Inductive Value : Term -> Type :=
  (* TODO: make body also value *)
  | V_lam param body : Value (T_lam param body).

Lemma exchange_preserve_linear ctx {ctx' x term}
  : Linear (append ctx (x :: ctx')) term ->
    Linear (x :: append ctx ctx') term.
  intros linear.
  generalize dependent x.
  generalize dependent ctx'.
  generalize dependent ctx.
  induction term.
  -
    intros ctx ctx' x linear.
    destruct ctx.
    exact linear.
    destruct ctx.
    inversion linear.
    inversion linear.
  -
    intros ctx ctx' x linear; refine (L_lam _ _ _).
    apply (IHterm (x :: ctx) ctx' param); simpl in *.
    simpl i.
    
    exact linear.
    
  intros ctx' x term linear; exact linear.
  generalize dependent ctx.
  induction ctx'.
  intros x term linear; simpl in *.
  apply IHctx.
  si
  induction 
  intros ctx' x term linear.
  generalize dependent x.
  generalize dependent ctx'.
  induction term.
  -
    intros ctx' x linear.
    induction ctx.
    inversion linear.
    inversion linear.
  -
    intros ctx' x lam; refine (L_lam _ _ _).
    induction ctx.
    inversion lam; destruct H2.
    simpl in *.

  
    intros ctx ctx' x linear.
    induction ctx.
    exact linear.
    induction ctx.
    inversion linear.
    inversion linear.
  -
    intros ctx ctx' x linear.
    induction ctx.
    exact linear.
    refine (L_lam _ _ _).
    
  -
    intros ctx' x lam; clear IHterm; refine (L_lam _ _ _).
    inversion 
    induction ctx.
    simpl in *; inversion linear; exact body.
    simpl in *.
    intros IHterm ctx' x linear; simpl in *.
    epose (IHctx).
    intros ctx' x linear; simpl in *.
    refine (L_lam _ _ _).
    inversion linear; destruct H2; clear H lam_ctx H1 param0.
    intros ctx' x linear; exact linear.
  -
  intros ctx' x linear; simpl in *.

    induction ctx.
    simpl in *; exact linear.
    (* TODO: double induction *)
    induction ctx.
    simpl in *; inversion linear.
    inversion linear.
  -
    intros ctx ctx' x lam.
    inversion lam; destruct H2; clear H lam_ctx H1 param0 lam.
    refine (L_lam _ _ _).
    generalize dependent param.
    generalize dependent x.
    generalize dependent ctx'.
    induction ctx; simpl in *.
    intros ctx' x param body x_not_free_in_param_ctx; exact body.
    intros x param body x_not_free_in_param_ctx.
    apply IHctx.


    induction ctx.
    
    inversion linear; rewrite H in *; clear H var0.

Admitted.

Lemma subst_but_not_in_ctx {ctx from to term}
  : free_in_ctx from ctx = false -> Linear ctx term ->
    Linear ctx (subst from to term).
  generalize dependent ctx.
  induction term.
  -
    intros ctx from_not_in_ctx linear.
    inversion linear; destruct H; rewrite H1 in *; clear H1; simpl.
    rewrite String.eqb_sym.
    rewrite (
      free_x_is_false_but_y_ctx_implies_y_neq_x from_not_in_ctx
    ).
    exact linear.
  -
    intros ctx from_not_in_ctx lam.
    simpl in *; destruct (String.eqb_spec from param).
    exact lam.
    inversion lam; destruct H, H2; clear H1 param0.
    refine (L_lam _ _ _).
    apply IHterm.
    refine (adding_irrelevant_stil_free _ _).
    exact from_not_in_ctx.
    exact n.
    exact body.
    exact param_not_free_in_lam_ctx.
  -
    intros ctx from_not_in_ctx app.
    simpl in *; inversion app; destruct H, H1, H2.
    destruct (split_not_free_in_both from_not_in_ctx).
    refine (L_app _ _).
    apply IHterm1.
    exact H.
    exact lam.
    apply IHterm2.
    exact H0.
    exact arg.
Qed.

Lemma subst_preserve_linear {ctx param to_term body_term}
  : Linear [] to_term -> Linear (param :: ctx) body_term ->
    Linear ctx (subst param to_term body_term).
  intros to body.
  generalize dependent ctx.
  induction body_term.
  -
    intros ctx body; simpl; inversion body; destruct H; clear var0 H0.
    rewrite (String.eqb_refl param); exact to.
  -
    intros ctx lam; simpl.
    inversion lam; clear H lam_ctx H1 param1 H2 body_term0 lam.
    rewrite (
      free_x_is_false_but_y_ctx_implies_y_neq_x param_not_free_in_lam_ctx
    ).
    refine (L_lam _ _ _).
    refine (IHbody_term _ _); clear IHbody_term.
    exact (exchange_preserve_linear [param0] body).
    exact (free_x_is_false_in_y_ctx_but_without_y param_not_free_in_lam_ctx).
  -
    intros ctx app; simpl.
    inversion app; destruct H, H2; clear app.
    induction lam_ctx.
    simpl in *; rewrite H0 in *; clear H0.
    rewrite <- (eq_refl : (append [] ctx = ctx)).
    apply L_app.
    refine (subst_but_not_in_ctx _ lam).
    reflexivity.
    apply IHbody_term2.
    exact arg.
    simpl in *.
    inversion H0; destruct H1, H2; clear H0.
    apply L_app.

    apply IHlam_ctx.
    simpl in H
    
    epose (L_app lam arg).

    epose (IHbody_term1 lam_ctx).
    epose (L_app _ _).
    refine (L_app _ _ ).
    refine 
    simpl.
Lemma normalize {term} (linear : Linear [] term)
  : { term : Term & Linear [] term & Value term }.
  induction term.
  -
    inversion linear.
  -
    exact (existT2 _ _ _ linear (V_lam _ _)).
  -
    inversion linear; destruct H, H2; clear linear.
    (* prepare context *)
    destruct (append_eq_empty_split H0) as [lam_ctx_empty arg_ctx_empty].
    rewrite lam_ctx_empty, arg_ctx_empty in *.
    clear H0 lam_ctx arg_ctx lam_ctx_empty arg_ctx_empty; simpl in *.
    (* extract inductions *)
    rename lam into lam_base; rename lam_term into lam_base_term.
    rename arg into arg_base; rename arg_term into arg_base_term.
    destruct (IHterm1 lam_base) as [lam_term lam lam_value].
    destruct (IHterm2 arg_base) as [arg_term arg arg_value].
    clear lam_base_term lam_base arg_base_term arg_base IHterm1 IHterm2.
    (* eliminate lam *)
    destruct lam_value as [param body_term].
    inversion lam; clear H lam_ctx H1 param0 H2 body_term0 lam.
    refine (existT2 _ _ (subst param arg_term body_term) _ _).
    induction body_term.
    --
      inversion body; destruct H; clear var0 H0.
      




Definition a : 1 + 2 = 3 := eq_refl.

Definition es_sucks P (y z : nat) (f : forall x : nat, P _) :=
  let _ : P y := f y in
  (f z : P y).
Check es_sucks. 
Proof.
  
Qed.

Definition a : 1 + 2 = 3 := eq_refl.
Definition b : 1 + 2 = 4 := eq_refl.

Print a.
Definition assert : forall {A}, forall x y : A, {_ : x = y} -> () := _.
Definition () := assert (1 + 2) 3.

Definition fuzz : 
  forall generator : (int -> 'a),
  forall check : ('a -> bool),
  (exists cases : list 'a, {cases = random generator & List.every f cases = true}) -> ().

Definition _ := fuzz generator check. 

  auto.
Qed.

Definition bool_to_int (b : bool) : nat.
  refine (
    match b with
    | true => _
    | false => _
    end).
  exact 1.
  exact 0.
Qed.


Definition x := 1.

Check x.

Print x.

