Require Import String.
Open Scope string_scope.

Require Import List.
Import ListNotations.


Inductive term : Type :=
  | T_var (var : string)
  | T_abs (var : string) (body : term)
  | T_app (lambda : term) (arg : term).

Definition intersection (l1 l2 : list string) :=
  List.filter (fun n => List.existsb (String.eqb n) l2) l1.

Definition remove := List.remove string_dec.
Definition count_occ := List.count_occ string_dec.

Inductive free_vars : term -> list string -> Type :=
  | FV_var : forall var,
    free_vars (T_var var) [var]
  | FV_abs : forall var body fv_body,
    free_vars body fv_body ->
    free_vars (T_abs var body) (remove var fv_body)
  | FV_app : forall lambda arg fv_lambda fv_arg,
    free_vars lambda fv_lambda -> free_vars arg fv_arg ->
    free_vars (T_app lambda arg) (List.rev_append fv_lambda fv_arg).

Inductive affine : term -> Type :=
  | A_var : forall var, affine (T_var var)
  | A_abs : forall var body, affine body -> affine (T_abs var body)
  | A_app : forall lambda arg fv_lambda fv_arg,
    free_vars lambda fv_lambda -> free_vars arg fv_arg ->
    (intersection fv_lambda fv_arg = []) ->
    affine lambda -> affine arg -> affine (T_app lambda arg).

Theorem string_dec_refl x : {eq & string_dec x x = left eq}.
  induction string_dec.
  apply (existT _ a).
  auto.
  destruct b.
  reflexivity.
Qed.

Theorem succ_n_eq_succ_m :
  forall (n m : nat), 1 + n = 1 + m.
Proof.
  intros n m.
  induction n.
  -
Qed.

Theorem removing_never_decreases fv x y :
  count_occ fv x = 0 \/ count_occ fv x = 1 ->
  count_occ (remove y fv) x = 0 \/ count_occ (remove y fv) x = 1.
  destruct (string_dec x y).
  destruct e.
  induction fv.
  auto.
  simpl in *; intros occurs.
  destruct (string_dec a x).
  destruct e.
  inversion occurs; inversion H; clear H.
  destruct (IHfv y (or_introl H1)).
  destruct (string_dec y a).
  destruct e.
  rewrite <- H.
  
  destruct H, H1.
  auto.
  rewrite H, H1; auto.
  simpl.
  destruct (string_dec a a).
  destruct (IHfv y (or_introl H1)).
  rewrite H, H1; auto.
  rewrite H.
  
  inversion occurs.
  auto.
  
  destruct e, e0.
  destruct (string_dec a a).
  inversion occurs; inversion H; clear H.
  apply or_intror.
  destruct (IHfv a (or_introl H1)).
  rewrite H.
  destruct H1.
  
  
  inversion occurs.
  inversion H.
  inversion H.
  epose (IHfv y (or_introl H1)).
  rewrite H1 in *.
  epose
  destruct e.
  inversion occurs.
  epose (IHfv y).
  
  
  induction fv.
  auto.
  intros x y occurs.
  inversion occurs.
  epose (IHfv _ _).
  induction remove.
  auto.
  induction (remove).
  auto.
  intros x occurs.
  simpl.
  destruct (string_dec a x).
  destruct e.
  destruct (IHl a occurs).
  auto.
  simpl.
  

  auto.
  intros occurs.
  destruct (IHn occurs).
  auto.
  destruct IHn.
  auto.
  destruct (String.string_dec x y).
  intros fv occurs; destruct e.
  -
    induction fv.
    auto.
    destruct (String.string_dec x a).
    destruct e.
    apply or_introl.
    simpl.
    simpl.
    destruct (String.string_dec x x).
    Search (string_dec _ _ = true).
    enough (occurs' : count_occ fv x = 0 \/ count_occ fv x = 1).
    epose (IHfv occurs'); destruct o.
    destruct H.
    destruct occurs.
  destruc
  induction fv.
  - auto.
  - intros x y occurs.
    epose (IHfv x y).

  intros zero_or_one; destruct zero_or_one.
  destruct (String.string_dec x y).
  destruct e; apply or_introl.
  destruct H.
  
  destruct e.
  
  unfold count_occ, List.count_occ, remove, List.remove.
  .
  induction fv.
  - auto.
  - intros zero_or_one; inversion zero_or_one; clear zero_or_one.
    destruct (String.string_dec a x).
    cbn; destruct e.
    destruct e.
    inversion H.
    epose (IHfv (or_introl H)); clear H IHfv.
    rewrite H0.
    cbn.
    apply IHfv.
    destruct e
  ; inversion zero_or_one; clear zero_or_one; destruct H.
    cbn in *.
    destruct (String.string_dec y a).
    destruct e.
    unfold count_occ, List.count_occ in *.
    
    inversion H.
    destruct H.
    s
    destruct e.
    epose (IHfv ).
    
    destruct zero_or_one.

Theorem affine_is_affine : forall term, affine term -> forall fv, free_vars term fv ->
  forall var, count_occ fv var = 0 \/ count_occ fv var = 1.
Proof.
  intros term affine.
  induction affine.
  - intros fv ind_fv var'.
    inversion ind_fv; clear ind_fv; destruct H0, H1.
    cbn; destruct (String.string_dec var0 var').
    auto.
    auto.
  - intros fv ind_fv var'.
    inversion ind_fv; clear ind_fv; destruct H, H0, H1.
    destruct (IHaffine fv_body H2 var'); clear IHaffine H2.
    destruct (String.string_dec var0 var').
    destruct e.
    apply or_introl.
    unfold count_occ.
    destruct 

  -
Qed.
