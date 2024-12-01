Require Import List.
Import ListNotations.
Require Import Bool.
Require Import PeanoNat.
Require Import Lia.

Inductive Term :=
  | T_var : forall (n : nat), Term
  | T_lambda : forall (body : Term), Term
  | T_apply : forall (funct : Term) (arg : Term), Term.

Fixpoint shift by_ depth term :=
  match term with
  | T_var var => 
    match Nat.ltb var depth with
    | true => T_var var
    | false => T_var (by_ + var)
    end
  | T_lambda body => 
    let body := shift by_ (1 + depth) body in
    T_lambda body
  | T_apply funct arg =>
    let funct := shift by_ depth funct in
    let arg := shift by_ depth arg in
    T_apply funct arg
  end.
Fixpoint subst to_ depth term  :=
  match term with
  | T_var var => 
    match Nat.eqb var depth with
    | true => shift depth 0 to_
    | false => 
      match Nat.ltb var depth with
      | true => T_var var
      | false => T_var (pred var)
      end
    end
  | T_lambda body => 
    let body := subst to_ (1 + depth) body in
    T_lambda body
  | T_apply funct arg =>
    let funct := subst to_ depth funct in
    let arg := subst to_ depth arg in
    T_apply funct arg
  end.
Fixpoint count var term :=
  match term with
  | T_var n =>
    match Nat.eqb n var with
    | true => 1
    | false => 0
    end
  | T_lambda body => count (1 + var) body
  | T_apply funct arg => count var funct + count var arg
  end.
Fixpoint size term :=
  match term with
  | T_var _ => 0
  | T_lambda body => 1 + size body
  | T_apply funct arg => size funct + size arg
  end.

Lemma shift_preserves_size {by_ depth term}
  : size term = size (shift by_ depth term).
  revert depth.
  induction term; intros depth; simpl in *; intuition.
  destruct (n <? depth); reflexivity.
Qed.
  
(* TODO: for which size metrics this works? *)
Lemma subst_multiplies_size {to_ term var}
  : size (subst to_ var term) = size term + (count var term * size to_).
  revert var.
  induction term; intros var; simpl in *.
  -
    destruct (Nat.eqb_spec n var); simpl.
    rewrite <- shift_preserves_size.
    apply plus_n_O.
    destruct (n <? var); reflexivity.
  -
    apply eq_S.
    apply IHterm.
  -
    rewrite IHterm1.
    rewrite IHterm2.
    lia.
Qed.

Fixpoint beta term  :=
  match term with
  | T_var var => T_var var
  | T_lambda body => 
    let body := beta body in
    T_lambda body
  | T_apply funct arg =>
    let funct := beta funct in
    let arg := beta arg in
    match funct with
    | T_lambda body => subst arg 0 body
    | T_var _ | T_apply _ _ => T_apply funct arg
    end
  end.

Definition Context := list nat.
 
Inductive Ordered : Context -> Term -> Type :=
  | O_var : forall n,
      Ordered [n] (T_var n)
  | O_lambda : forall ctx body (o_body : Ordered (0 :: ctx) body),
      Ordered ctx (T_lambda body)
  | O_apply : forall ctx_funct funct (o_funct : Ordered ctx_funct funct)
      ctx_arg arg (o_arg : Ordered ctx_arg arg)
      (unique : forall var, count var funct = 1 -> count var arg = 0),
      Ordered (app ctx_funct ctx_arg) (T_apply funct arg).

Lemma ordered_is_affine {ctx term} (o_term : Ordered ctx term)
  : forall var, count var term <= 1.
  induction o_term; intros var; simpl.
  -
    destruct (n =? var); auto.
  -
    apply IHo_term.
  -
    specialize (IHo_term1 var).
    specialize (IHo_term2 var).
    specialize (unique var).
    destruct (count var funct).
    apply IHo_term2.
    destruct n; lia.
Qed.

Lemma head_beta_preserve_ordered {ctx body arg}
  (o_term : Ordered ctx (T_apply (T_lambda body) arg))
  : Ordered ctx (subst arg 0 body).
  inversion o_term; clear o_term funct H1 arg0 H2 ctx H.
  inversion o_funct; clear o_funct ctx H body0 H1.
  revert ctx_funct o_body ctx_arg o_arg unique.
  induction body; intros.
  -
    simpl.
    assert (ctx_funct = []).
    inversion o_body; reflexivity.
    inversion o_funct; inversion o_body; reflexivity.
    rewrite H.
    destruct n.
    exact o_arg.
    inversion o_funct; inversion o_body.
  -
    
    

    

Fixpoint size term :=
  match term with
  | T_var _ => 0
  | T_lambda body => 1 + size body
  | T_apply funct arg => size funct + size arg
  end.
Fixpoint has_beta (term : Term) :=
  match term with
  | T_var _ => false
  | T_lambda body => has_beta body
  | T_apply funct arg =>
    match funct with
    | T_lambda _ => true
    | T_var _ | T_apply _ _ => has_beta funct || has_beta arg
    end
  end. 
Theorem ordered_beta_reduces {ctx term} (o_term : Ordered ctx term)
  : has_beta term = true -> size (beta term) < size term.
  intros H.
  induction o_term; simpl in *.
  -
    discriminate.
  -
    rename o_term into o_body, IHo_term into IHo_body.
    apply le_n_S. 
    exact (IHo_body H).
  -
    rename o_term1 into o_funct, IHo_term1 into IHo_funct.
    rename o_term2 into o_arg, IHo_term2 into IHo_arg.
    induction o_funct; simpl in *.
    --
      exact (IHo_arg H).
    --
      

  lia.

  
  
Lemma sized_fixpoint {T} (f : forall n, (forall m, m < n -> T) -> T)
  : forall (n : nat), T.
Proof.
  fix sized_fixpoint 1; intros n.
  refine (f n _); intros m H.
  destruct n.
  destruct (Nat.nlt_0_r _ H).
  exact (sized_fixpoint n).
Defined.
