Set Universe Polymorphism.
Set Definitional UIP.
Set Primitive Projections.

Axiom K : forall A (x : A) (H : x = x), eq_refl = H.
Lemma uip {A} {x y : A} (p q : x = y) : p = q.
  revert q.
  rewrite p.
  intros q.
  apply K.
Defined.

Definition not b :=
  match b with
  | true => false
  | false => true
  end.
 
Axiom bad_eq : bool = bool.
Axiom bad_eq_elim : forall b,
  match bad_eq with
  | eq_refl _ => b
  end = not b.


Lemma false : False.
  pose (bad_eq_elim true).
  rewrite (uip bad_eq eq_refl) in e.
  unfold not.
  destruct bad_eq.
Lemma false : false = true -> False.
  intros H.
  epose (eq_ind false (fun b =>
  match b with
  | true => False
  | false => True
  end) I).
  destruct H.

  pose (bad_eq_elim true).
  rewrite (uip bad_eq eq_refl) in e.
  unfold not.
  destruct bad_eq.

Lemma false : False.
  pose (bad_eq_elim true).
  rewrite (uip bad_eq eq_refl) in e.
  unfold not.
  destruct bad_eq.

Definition C_Bool : Type := forall A, A -> A -> A.
Definition c_true : C_Bool := fun A x y => x.
Definition c_false : C_Bool := fun A x y => y.

Definition I_Bool (c_b : C_Bool) : Type :=
  forall P : C_Bool -> Type, P c_true -> P c_false -> P c_b.
Definition i_true : I_Bool c_true := fun A x y => x.
Definition i_false : I_Bool c_false := fun A x y => y.

Definition Bool0 : Type := sigT I_Bool.
Definition true0 : Bool0 := existT _ _ i_true.
Definition false0 : Bool0 := existT _ _ i_false.

Definition Bool : Type := sigT (fun b : Bool0 =>
  projT1 b Prop (b = true0) (b = false0)).
Definition true : Bool := existT _ true0 eq_refl.
Definition false : Bool := existT _ false0 eq_refl.

Lemma ind_bool : forall P b, P true -> P false -> P b.
  intros P b t f.
  destruct b as [[c_b i_b] w]; simpl in *.
  epose (i_b (fun c_b => forall (i_b : I_Bool c_b), P (existT _ (existT I_Bool c_b i_b)))).
  apply i_b.
  unfold existT.

(* Counterexample by Thierry Coquand and Christine Paulin
   Translated into Coq by Vilhelm Sjöberg *)

(* Phi is a positive, but not strictly positive, operator. *)
Definition Phi (a : Type) := (a -> Prop) -> Prop.

Unset Positivity Checking.
Inductive A : Type :=
  introA : Phi A -> A.
Set Positivity Checking.
Definition matchA (a : A) : Phi A :=
  match a with | introA x => x end.
Definition beta x : matchA (introA x) = x := eq_refl.

Lemma introA_injective : forall p p', introA p = introA p' -> p = p'.
Proof.
  intros.
  assert (matchA (introA p) = (matchA (introA p'))) as H1 by congruence.
  now repeat rewrite beta in H1.
Qed.

(* However, ... *) 

(* Proposition: For any type A, there cannot be an injection
   from Phi(A) to A. *)

(* For any type X, there is an injection from X to (X->Prop),
   which is λx.(λy.x=y) . *)
Definition i {X:Type} : X -> (X -> Prop) := 
  fun x y => x = y.

Lemma i_injective : forall X (x x' :X), i x = i x' -> x = x'.
Proof.
  intros.
  assert (i x x = i x' x) as H1 by congruence.
  compute in H1.
  symmetry.
  rewrite <- H1.
  reflexivity.
Qed.  

(* Hence, by composition, we get an injection f from A->Prop to A. *)
Definition f : (A -> Prop) -> A 
  := fun p => introA (i p).

Lemma f_injective : forall p p', f p = f p' -> p = p'.
Proof.
  unfold f. intros.
  apply introA_injective in H. apply i_injective in H. assumption.
Qed.

(* We are now back to the usual Cantor-Russel paradox. *)
(* We can define *)
Definition P0 : A -> Prop
  := fun x => 
       exists (P:A->Prop), f P = x /\ ~ P x.
(* i.e., P0 x := x codes a set P such that x∉P. *)

Definition x0 := f P0.

(* We have (P0 x0) iff ~(P0 x0) *)
Lemma bad : (P0 x0) <-> ~(P0 x0).
Proof.
split.
  * intros [P [H1 H2]] H.
    change x0 with (f P0) in H1.
    apply f_injective in H1. rewrite H1 in H2.
    auto.
  * intros.
    exists P0. auto.
Qed.

(* Hence a contradiction. *)
Theorem worse : False.
  pose bad. tauto.
Qed.




Lemma symm_of_symm {A} {x y : A}
  (H : x = y) : eq_sym (eq_sym H) = H.
  destruct H.
  simpl.
  reflexivity.
Require Import List.
Import ListNotations.
Require Import PeanoNat.
Require Import Lia.

Definition Index := nat.
Definition Level := nat.

Inductive Sort :=
  | S_prop
  | S_type.

Inductive Term : Type :=
  | T_type : Term
  | T_prop : Term
  | T_free_var : forall (l : Level), Term
  | T_bound_var : forall (i : Index), Term
  | T_forall : forall (param : Term) (body : Term), Term
  | T_lambda : forall (body : Term), Term
  | T_apply : forall (lambda : Term) (arg : Term), Term.
  (* | T_bool : Term
  | T_true : Term
  | T_false : Term
  | T_if : forall (pred : Term) (then_ : Term) (else_ : Term), Term. *)

Fixpoint open_at to_ depth term :=
  match term with
  | T_type => T_type
  | T_prop => T_prop
  | T_free_var l => T_free_var l
  | T_bound_var i =>
    match Nat.eqb i depth with
    | true => to_
    | false => T_bound_var i
    end
  | T_forall param body =>
    let param := open_at to_ depth param in
    let body := open_at to_ (1 + depth) body in
    T_forall param body
  | T_lambda body =>
    let body := open_at to_ (1 + depth) body in
    T_lambda body
  | T_apply lambda arg =>
    let lambda := open_at to_ depth lambda in
    let arg := open_at to_ depth arg in
    T_apply lambda arg
  (* | T_bool => T_bool
  | T_true => T_true
  | T_false => T_false
  | T_if pred then_ else_ =>
    let pred := open to_ depth pred in
    let then_ := open to_ depth then_ in
    let else_ := open to_ depth else_ in
    T_if pred then_ else_ *)
  end.

Definition open to_ term := open_at to_ 0 term.
Fixpoint close from depth term :=
  match term with
  | T_type => T_type
  | T_prop => T_prop
  | T_free_var l =>
    match Nat.eqb l from with
    | true => T_bound_var depth
    | false => T_free_var l
    end
  | T_bound_var i => T_bound_var i
  | T_forall param body =>
    let param := close from depth param in
    let body := close from (1 + depth) body in
    T_forall param body
  | T_lambda body =>
    let body := close from (1 + depth) body in
    T_lambda body
  | T_apply lambda arg =>
    let lambda := close from depth lambda in
    let arg := close from depth arg in
    T_apply lambda arg
  (* | T_bool => T_bool
  | T_true => T_true
  | T_false => T_false
  | T_if pred then_ else_ =>
    let pred := close from depth pred in
    let then_ := close from depth then_ in
    let else_ := close from depth else_ in
    T_if pred then_ else_ *)
  end.

(* TODO: better names here *)
Inductive LC_at (k : Level) : Term -> Type :=
  | LC_type : LC_at k T_type
  | LC_prop : LC_at k T_prop
  | LC_free_var : forall l, l >= k -> LC_at k (T_free_var l)
  | LC_bound_var : forall i, i < k -> LC_at k (T_bound_var i)
  | LC_forall : forall param body,
    LC_at k param -> LC_at (1 + k) body ->
    LC_at k (T_forall param body)
  | LC_lambda : forall body,
    LC_at (1 + k) body ->
    LC_at k (T_lambda body)
  | LC_apply : forall lambda arg,
    LC_at k lambda -> LC_at k arg ->
    LC_at k (T_apply lambda arg).

Definition LC := LC_at 0.
(* 
Definition open_lower_lc {t k} (lc : LC_at (1 + k) t)
  : LC_at (1 + k) (open (T_free_var (1 + k)) t).
  revert k lc; induction t; intros k lc.
  (* TODO: how to make it autocomplete? *)
  apply LC_type.
  apply LC_prop.
  apply LC_free_var.
  inversion lc.
  auto. *)



Definition x :
  forall P : Term -> Type,
  P T_type -> P T_prop ->
  (forall l,
    P (T_free_var l)) ->
  (forall k param body,
    P param ->
    (forall x, x > k -> P (open (T_free_var x) body))
    -> P (T_forall param body)) ->
  (forall k body,
    (forall x, x > k -> P (open (T_free_var x) body)) -> P (T_lambda body)) ->
  (forall lambda arg,
    P lambda -> P arg -> P (T_apply lambda arg)) ->
  (* P T_bool ->
  P T_true ->
  P T_false ->
  (forall pred then_ else_,
    P pred -> P then_ -> P else_ -> P (T_if pred then_ else_)) -> *)
  forall t, LC t -> P t.
  intros P p_type p_prop p_free p_forall p_lambda p_apply
    (* p_bool p_true p_false p_if *) t lc.
  induction t.
  auto.
  auto.
  auto.'
  inversion lc; lia.
  inversion lc; clear param body H H0.
  epose (p_forall _ t1 _ (IHt1 H1)). 


Check Term_rect.
(* TODO: maybe locally nameless *)
Inductive Term :=
  | T_type
  | T_prop
  | T_var (n : Index) (type : Term)
  | T_forall (param : Term) (body : Term)
  | T_lambda (body : Term) (type : Term)
  | T_apply (lambda : Term) (arg : Term) (type : Term)
  | T_bool
  | T_true
  | T_false
  | T_if (pred : Term) (then_ : Term) (else_ : Term) (type : Term).

Fixpoint shift by_ depth term :=
  match term with
  | T_type => T_type
  | T_prop => T_prop
  | T_var n type => 
    let type := shift by_ depth type in
    match Nat.leb n depth with
    | false => T_var n type
    | true => T_var (n + by_) type
    end
  | T_forall param body =>
    let param := shift by_ depth param in
    let body := shift by_ (1 + depth) body in
    T_forall param body
  | T_lambda body type =>
    let body := shift by_ (1 + depth) body in
    let type := shift by_ depth type in
    T_lambda body type
  | T_apply lambda arg type =>
    let lambda := shift by_ depth lambda in
    let arg := shift by_ depth arg in
    let type := shift by_ depth type in
    T_apply lambda arg type
  | T_bool => T_bool
  | T_true => T_true
  | T_false => T_false
  | T_if pred then_ else_ type =>
    let pred := shift by_ depth pred in
    let then_ := shift by_ depth then_ in
    let else_ := shift by_ depth else_ in
    let type := shift by_ depth type in
    T_if pred then_ else_ type
  end.

Fixpoint subst to_ depth term :=
  match term with
  | T_type => T_type
  | T_prop => T_prop
  | T_var n type => 
    let type := subst to_ depth type in
    (* TODO: maybe subtraction *)
    match Nat.leb n depth with
    | false => T_var n type
    | true =>
      match Nat.eqb n depth with
      | true => shift depth 0 to_
      | false => T_var (n - 1) type
      end
    end
  | T_forall param body =>
    let param := subst to_ depth param in
    let body := subst to_ (1 + depth) body in
    T_forall param body
  | T_lambda body type =>
    let body := subst to_ (1 + depth) body in
    let type := subst to_ depth type in
    T_lambda body type
  | T_apply lambda arg type =>
    let lambda := subst to_ depth lambda in
    let arg := subst to_ depth arg in
    let type := subst to_ depth type in
    T_apply lambda arg type
  | T_bool => T_bool
  | T_true => T_true
  | T_false => T_false
  | T_if pred then_ else_ type =>
    let pred := subst to_ depth pred in
    let then_ := subst to_ depth then_ in
    let else_ := subst to_ depth else_ in
    let type := subst to_ depth type in
    T_if pred then_ else_ type
  end.

Fixpoint type_of (term : Term) :=
  match term with
  (* not reachable *)
  | T_type => T_type
  | T_prop => T_type
  | T_var _n type => type
  | T_forall _param body => type_of body
  | T_lambda _body type => type
  | T_apply _lambda _arg type => type
  | T_bool => T_type
  | T_true => T_bool
  | T_false => T_bool
  | T_if _pred _then _else type => type 
  end.

Fixpoint erase_bool term :=
  match term with
  | T_type => T_type
  | T_prop => T_prop
  | T_var n type =>
    let type := erase_bool type in
    T_var n type
  | T_forall param body =>
    let param := erase_bool param in
    let body := erase_bool body in
    T_forall param body
  | T_lambda body type =>
    let body := erase_bool body in
    let type := erase_bool type in
    T_lambda body type
  | T_apply lambda arg type =>
    let lambda := erase_bool lambda in
    let arg := erase_bool arg in
    let type := erase_bool type in
    T_apply lambda arg
  | T_bool =>
    T_forall T_prop (T_forall T_prop T_prop)
  | T_true =>
    T_lambda (T_lambda (T_var 1))
  | T_false =>
    T_lambda (T_lambda (T_var 0))
  | T_if pred then_ else_ T_prop =>
    T_apply (T_apply pred then_ (T_forall T_prop T_prop)) else_ T_prop
  | T_if pred then_ else_ type =>
    let pred := erase_bool pred in
    let then_ := erase_bool then_ in
    let else_ := erase_bool else_ in
    let type := erase_bool type in
    T_if pred then_ else_ type
  end.

Definition Context := list Term.
Inductive Lookup_free : Context -> Index -> Term -> Type :=
Inductive Lookup_bound : Context -> Index -> Term -> Type :=
  | LB_head : forall head ctx,
    Lookup_bound (head :: ctx) 0 head
  | LB_

Inductive Normalize : Term -> Term -> Type :=
  | Trivial : forall term, Normalize term term.
Inductive Equal : Term -> Term -> Type :=
  | Eq_mod_normal : forall l r x,
    Normalize l x -> Normalize r x -> Equal l r.

(* TODO: unicode *)
(* TODO: actually Check *)
Inductive Infer : Term -> Type :=
  | I_prop : forall ctx, Infer ctx T_prop
  | I_bound_var_head : forall ctx n type,
    Infer ctx type -> Infer (T_bound_var 0 type)
  | I_bound_var_succ : forall head ctx n type sort,
    Infer ctx (T_bound_var n type) sort ->
    Infer (head :: ctx) (T_bound_var (1 + n) type) sort
  | I_forall : forall ctx param param_kind body body_kind,
    Infer ctx param S_type ->
    Infer (param :: ctx) body S_type ->
    Infer ctx (T_forall param body ) S_type
  | I_lambda : forall ctx param param_kind body body_type,
    Infer ctx param param_kind ->
    Infer (param :: ctx) body body_type ->
    Infer ctx (T_lambda body (T_forall param body_type))
  | I_apply : forall ctx lambda param body_type arg arg_type,
    Infer ctx lambda (T_forall param body_type) ->
    Infer ctx arg arg_type ->
    Infer ctx (T_apply lambda arg) body_type
  | I_bool : forall ctx,
    Infer ctx T_bool T_type
  | I_true : forall ctx,
    Infer ctx T_true T_bool
  | I_false : forall ctx,
    Infer ctx T_false T_bool
  | I_if : forall ctx pred then_ else_ result,
    Infer ctx result T_type ->
    Infer ctx pred T_bool ->
    Infer ctx then_ result ->
    Infer ctx else_ result ->
    Infer ctx (T_if pred then_ else_) result.

Inductive Valid_context : Context -> Type :=
  | VC_empty : Valid_context []
  | VC_cons : forall ctx type kind,
    Sort kind -> Infer ctx type kind ->
    Valid_context ctx ->
    Valid_context (type :: ctx).

Inductive Typed :=
  |

Lemma no_t_type_in_valid_ctx ctx n
  : Valid_context ctx -> Lookup ctx n T_type -> False.
  revert ctx; induction n.
  -
    intros ctx ctx_is_valid H.
    induction ctx_is_valid.
    (* TODO: why contradiction doesn't work? *)
    inversion H.
    inversion H; clear ctx0 H2 type0 H1.
    rewrite H0 in *; clear H0.
    inversion i.
  -
    intros ctx ctx_is_valid H.
    induction ctx_is_valid.
    inversion H.
    inversion H; clear head H0 type0 H1 ctx0 H2 n0 H3.
    exact (IHn ctx ctx_is_valid H4).
Qed.
Fixpoint erase_bool {ctx term type_} (ctx_is_valid : Valid_context ctx)
  (term_has_type : Infer ctx term type_) : Term.
  (* TODO: this should probably just be gallina *)
  destruct term_has_type.
  exact T_prop.
  exact (T_bound_var n).
  exact (T_forall (erase_bool _ _ _ term_has_type1)
    (erase_bool _ _ _ term_has_type2)).
  exact (T_lambda (erase_bool _ _ _ term_has_type2)).
  exact (T_apply (erase_bool _ _ _ term_has_type1)
    (erase_bool _ _ _ term_has_type2)).
  exact (T_forall T_prop (T_forall T_prop T_prop)).
  exact (T_lambda (T_lambda (T_bound_var 1))).
  exact (T_lambda (T_lambda (T_bound_var 0))).
  destruct result.
  inversion term_has_type1.
  exact (T_apply (T_apply pred then_) else_).

      T_forall T_prop (T_forall T_prop T_prop)
    | T_true =>
      T_lambda (T_lambda (T_bound_var 1))
    | T_false =>
      T_lambda (T_lambda (T_bound_var 0))
    | T_if pred then_ else_ => _
    end
  ).
:=
  match term_has_type with
  | I_prop _ => T_prop
  | I_bound_var n => _
  end.
Fixpoint erase_bool {ctx term type_}
  (term_has_type : Infer ctx term type_) :=
  match term with
  | T_type => T_type
  | T_prop => T_prop
  | T_bound_var n => T_bound_var n
  | T_forall param body =>
    let param := erase_bool param in
    let body := erase_bool body in
    T_forall param body
  | T_lambda body =>
    let body := erase_bool body in
    T_lambda body
  | T_apply lambda arg =>
    let lambda := erase_bool lambda in
    let arg := erase_bool arg in
    T_apply lambda arg
  | T_bool =>
    T_forall T_prop (T_forall T_prop T_prop) 
  | T_true =>
    T_lambda (T_lambda (T_bound_var 1))
  | T_false =>
    T_lambda (T_lambda (T_bound_var 0))
  | T_if pred then_ else_ =>
    match 
  end.


Theorem type_is_also_valid ctx term type
  : type <> T_type -> Valid_context ctx ->
    Infer ctx term type -> {kind & Infer ctx type kind}.
  intros not_in_type ctx_is_valid term_has_type.
  induction type.
  contradiction.
  exists T_type; exact (I_prop _).
  exists T_prop.
  induction term_has_type; intuition.
  
  
  intuition.
  induction n.
  induction ctx_is_valid.
  (* TODO: use intuition to refute *)
  inversion l.
  inversion l.
  exists 

  
  intuition.

Inductive F_kind :=
  | K_linear
  | K_data.
Inductive F_type :=
  | T_var (n : Index)
  | T_arrow (param : F_type) (body : F_type) (kind : F_kind)
  | T_forall (param : F_kind) (body : F_type).
Inductive F_expr :=
  | E_var (n : Index)
  | E_lambda (param : F_type) (body : F_expr) (kind : F_kind)
  | E_apply (lambda : F_expr) (arg : F_expr)
  | E_type_lambda (param : F_kind) (body : F_expr)
  | E_type_apply (lambda : F_expr) (arg : F_type).

  
Inductive F_kind :=
  | K_linear
  | K_data.
Inductive F_type :=
  | T_var (n : Index)
  | T_arrow (param : F_type) (body : F_type) (kind : F_kind)
  | T_forall (param : F_kind) (body : F_type).
Inductive F_expr :=
  | E_var (n : Index)
  | E_lambda (param : F_type) (body : F_expr) (kind : F_kind)
  | E_apply (lambda : F_expr) (arg : F_expr)
  | E_type_lambda (param : F_kind) (body : F_expr)
  | E_type_apply (lambda : F_expr) (arg : F_type).

Fixpoint shift_type by_ depth type :=
  match type with
  | T_var n =>
    match Nat.ltb n depth with
    | true => T_var n
    | false => T_var (by_ + n)
    end
  | T_arrow param body kind =>
    let param := shift_type by_ depth param in
    let body := shift_type (1 + depth) by_ body  in
    T_arrow param body kind
  | T_forall param body =>
    let body := shift_type by_ (1 + depth) body in
    T_forall param body
  end.

Fixpoint subst_type to_ depth type :=
  match type with
  | T_var n =>
    match Nat.ltb n depth with
    | true => T_var n
    | false =>
      match Nat.eqb depth n with
      | true => shift_type depth 0 to_ 
      | false => T_var (n - depth)
      end
    end
  | T_arrow param body kind =>
    let param := subst_type to_ depth param in
    let body := subst_type to_ (1 + depth) body in
    T_arrow param body kind
  | T_forall param body =>
    let body := subst_type to_ (1 + depth) body in
    T_forall param body
  end.

Inductive Context_entry :=
  | CE_value (type : F_type)
  | CE_type (kind : F_kind).

Definition Context := list Context_entry.
(* TODO: unicode *)

Inductive Lookup_type : Context -> Index -> F_type -> Type :=
  | LT_head : forall type ctx,
    Lookup_type (CE_value type :: ctx) 0 type
  | LT_succ : forall head ctx type n,
    Lookup_type ctx n type ->
    Lookup_type (head :: ctx) (1 + n) type.

Inductive Lookup_kind : Context -> Index -> F_kind -> Type :=
  | LK_head : forall kind ctx,
    Lookup_kind (CE_type kind :: ctx) 0 kind
  | LK_succ : forall head ctx kind n,
    Lookup_kind ctx n kind ->
    Lookup_kind (head :: ctx) (1 + n) kind.

Inductive F_infer_type : Context -> F_type -> F_kind -> Type :=
  | FT_infer_var : forall ctx n kind,
    Lookup_kind ctx n kind ->
    F_infer_type ctx (T_var n) kind
  | FT_infer_arrow : forall ctx param param_kind body body_kind kind,
    F_infer_type ctx param param_kind ->
    F_infer_type (CE_value param :: ctx) param body_kind ->
    F_infer_type ctx (T_arrow param body kind) kind
  | FT_infer_forall : forall ctx param body kind,
    F_infer_type (CE_type param :: ctx) body kind ->
    F_infer_type ctx (T_forall param body) kind.

Inductive F_infer_expr : Context -> F_expr -> F_type -> Type :=
  | FE_var : forall ctx n type,
    Lookup_type ctx n type ->
    F_infer_expr ctx (E_var n) type
  | FE_lambda : forall ctx param param_kind body body_type kind,  
    F_infer_type ctx param param_kind ->
    F_infer_expr (CE_value param :: ctx) body body_type ->
    F_infer_expr ctx (E_lambda param body kind)
      (T_arrow param body_type kind)
  | FE_apply : forall ctx lambda param kind body_type arg,
    F_infer_expr ctx lambda (T_arrow param body_type kind) ->
    F_infer_expr ctx arg param ->
    F_infer_expr ctx (E_apply lambda arg) body_type
  | FE_type_lambda : forall ctx param body body_type,
    F_infer_expr (CE_type param :: ctx) body body_type ->
    F_infer_expr ctx (E_type_lambda param body) (T_forall param body_type)
  | FE_type_apply : forall ctx lambda param body_type arg,
    F_infer_expr ctx lambda (T_forall param body_type) ->
    F_infer_type ctx arg param ->
    F_infer_expr ctx (E_type_apply lambda arg)
      (subst_type arg 0 body_type).

  
Definition C_Bool := Prop -> Prop -> Prop.
Definition c_true : C_Bool := fun x y => x.
Definition c_false : C_Bool := fun x y => y.

Definition I_Bool (c_b : C_Bool) :=
  forall P : C_Bool -> Prop,
    P c_true -> P c_false -> P c_b.
Definition i_true : I_Bool c_true := fun P x y => x.
Definition i_false : I_Bool c_false := fun P x y => y.

Definition EqC_b (x y : C_Bool) := forall P : C_Bool -> Prop, P x -> P y.
Definition reflC_b (x : C_Bool) : EqC_b x x := fun P x => x.

Definition Eq {A : Type} (x y : A) := forall P : A -> Prop, P x -> P y.
Definition refl {A} (x : A) : Eq x x := fun P x => x.

Definition W_Bool {c_b} (i_b : I_Bool c_b) :=
  c_b
    (forall H : EqC_b c_b c_true,
      Eq i_true (H _ i_b))
    (forall H : EqC_b c_b c_false,
      Eq i_false (H _ i_b)).

Lemma ind {c_b : C_Bool} (i_b : I_Bool c_b) (w : W_Bool i_b)
  : forall P : forall {c_b}, I_Bool c_b -> Prop,
    P i_true -> P i_false -> P i_b.
  intros P x y.
  refine (i_b (fun i_c_b => forall (eq : EqC_b i_c_b c_b), P c_b i_b)
    _ _ (reflC_b _)
  ); intros eq.
  refine (eq (fun c_b =>
    forall (i_b : I_Bool c_b) (w : W_Bool i_b), P c_b i_b) _ i_b w).
  clear eq; intros i_b0 w0.
  exact (w0 (reflC_b _) _ x).
  refine (eq (fun c_b =>
    forall (i_b : I_Bool c_b) (w : W_Bool i_b), P c_b i_b) _ i_b w).
  clear eq; intros i_b0 w0.
  exact (w0 (reflC_b _) _ y).
Defined.

Lemma w_true : W_Bool i_true.
  intros H.
  
Lemma cdec_cbool (H : c_true = c_true) : H = eq_refl.l
  epose (
    match H in (_ = c_b)
      return forall (i_b : I_Bool c_b),
        i_b (fun c_b => forall h : c_true = c_b, Prop)
          (fun h => h = eq_refl)
          (fun h => IDProp) H
    with
    | eq_refl _ => _
    end i_true
  ).
  intros i_b.
  cbv.
  unfold I_Bool in i_b.
  epose (i_b (fun _ => _)).
  finduction c_b using i_b.
  epose 
unfold I_Bool in i_true.
  epose (i_true.

Lemma ind_true : forall P : forall {c_b}, I_Bool c_b -> Prop,
  P i_true -> P i_false -> P i_true.
  apply ind.
  intros H.
  destruct H.


Print i_ind.
Set Printing Universes.

Definition C_Nat : Type :=
    forall A, A -> (A -> A) -> A.
Definition c_zero : C_Nat :=
  fun A z s => z.
Definition c_succ (pred : C_Nat) : C_Nat :=
  fun A z s => s (pred A z s).

Definition I_Nat (c_n : C_Nat) :=
  forall P : C_Nat -> Type, P c_zero ->
    (forall c_pred, P c_pred -> P (c_succ c_pred)) -> P c_n.
Definition i_zero : I_Nat c_zero :=
  fun P z s => z.
Definition i_succ {c_pred} (pred : I_Nat c_pred)
  : I_Nat (c_succ c_pred) :=
  fun P z s => s _ (pred P z s).

Inductive Nat : Type := {
  c_n : C_Nat;
  i_n : I_Nat c_n;
  (* TODO: I think I can drop heterogenous equality *)
  w : c_n SProp
    (heqP _ i_n _ i_zero)
    (fun H => heqP _ i_n _ i_zero)
}.
Definition zero : Nat :=
  {|
    c_n := c_zero;
    i_n := i_zero;
    w := heqP_refl _ _;
  |}.
Definition succ (pred : Nat) : Nat :=
  {|
    c_n := c_succ (c_n pred);
    i_n := i_succ (i_n pred);
    w := _;
  |}.

Lemma ind b : forall P : Bool -> Type, P true -> P false -> P b.
  intros P x y.
  destruct b as [c_b i_b w].
  refine (i_b (fun i_c_b =>
    forall (eq : eqP _ c_b i_c_b) w,
    P {|
      c_b := i_c_b;
      i_b := 
        match eq with
        | eqP_refl _ _ =>
          i_b
        end;
      w := w
    |}
  ) _ _ (eqP_refl _ _) w).
  intros eq w0; unfold c_true in *; simpl in *.
  destruct (eqP_sym (heqP_to_eqP w0)).
  exact x.
  intros eq w0; unfold c_false in *; simpl in *.
  destruct (eqP_sym (heqP_to_eqP w0)).
  exact y.
Defined.


Theorem eqP_sym {A x y} (H : eqP A x y) : eqP A y x.
  destruct H.
  exact (eqP_refl _ _).
Defined.

Record sigP (A : Type) (P : A -> SProp) : Type := { l : A; r : P l }.

Lemma w
  (Bool0 : Type) (true0 : Bool0) (false0 : Bool0)
  (H0 : eqP _ {b0 : Bool0 & forall P, P true0 -> P false0 -> P b0} Bool0) :
    let Bool := sigP _ (fun b0 => eqP _
      match H0 in (eqP _ _ r) return r with
      | eqP_refl _ _ => b0
      end
      (projT1 b0)) in
    forall (true : Bool) (false : Bool),
    forall b, forall P, P true -> P false -> P b.
  intros Bool true false b P x y.
  destruct b as [b0 w].
  destruct b0 as [b0' i].
  destruct H0.
  simpl in *.
  rewrite <- w.
  apply i.
  rewrite H0 in b.
  refine (eqP_rect _ {b0 : Bool0 & forall P, P true0 -> P false0 -> P b0}
    (fun T eq =>
      forall H0 : eqP _ Bool0 T,
      let Bool := sigP _ (fun b0 => eqP T
        match eq in (eqP _ _ r) return r with
        | eqP_refl _ _ => b0
      end
        match H0 in (eqP _ _ r) return r with
        | eqP_refl _ _ => projT1 b0
        end) in
      forall (true : Bool) (false : Bool),
      forall b, forall P, P true -> P false -> P b
    ) _ Bool0 H0 (eqP_refl _ _)).
  clear H0; intros H0 Bool true false.
  simpl in *.
  simpl in *.
  refine (
    match H0 as (eqP _ _ T) return (
      let Bool :=
        sigP Bool0
          (fun b0 : Bool0 =>
          eqP Bool0 b0
            (projT1 match H0 in (eqP _ _ r) return r with
                    | eqP_refl _ _ => b0
                    end)) in
      forall (true false b : Bool) (P : Bool -> Type), P true -> P false -> P b
    ) with
    | eqP_refl _ _ =>
    end
  ).
  intros Bool true false.
  intros b.
  destruct b as [b0 w].
  Check 
  refine (
    match w as (eqP with
    | eqP_refl _ _ =>
    end
  ).
  
  refine (
    match H1 in (eqP _ _ r)
      return _ with
    | eqP_refl _ _ => _
    end
  ).
  revert true false.
  destruct H0.
  rewrite H0 in true0.
  refine (
    match H0 with
    | eqP_refl _ _ => _
    end
  ).
  intros b.
  destruct b as [b0 w].
  generalize dependent b0.
  epose (eqP_rect _ Bool0 (fun T H =>
    forall (b0 : T) (w : eqP T b0
      (projT1 _)), P {| l := b0; r := w |})).
      simpl in p.
  destruct H0.
  rewrite H0.
  destruct H0.
  destruct H0.
  destruct (eq_sym H1); clear H1.
  simpl in *.
  rewrite H1 in b.
Unset Positivity Checking.


Set Definitional UIP.
Set Primitive Projections.
Set Definitional UIP.

Record sigP (A : Type) (P : A -> SProp) : Type := { l : A; r : P l }.
Inductive eqP (A : Type) (x : A) : A -> SProp := eqP_refl : eqP A x x.

Definition C_Bool : Type := forall A, A -> A -> A.
Definition c_true : C_Bool := fun A x y => x.
Definition c_false : C_Bool := fun A x y => y.

Definition I_Bool (c_b : C_Bool) :=
  forall P : C_Bool -> SProp, P c_true -> P c_false -> P c_b.
Definition i_true : I_Bool c_true := fun A x y => x.
Definition i_false : I_Bool c_false := fun A x y => y.

Definition Bool : Type := sigP C_Bool I_Bool.


CoFixpoint x := {x : }
Lemma eq {A B} {l r : {x : A & B x}} (H1 : projT1 l = projT1 r)
  : projT2 l =
    match eq_sym H1 with
    | eq_refl => projT2 r
    end -> l = r.
  intros H2.
  destruct l.
  destruct r.
  simpl in *.
  destruct H1.
  simpl.
  unfold eq_sym in H2.
  rewrite H2.
  reflexivity.
Defined.
  rewrite <- H1 in *.

Require Import PeanoNat.
Require Import String.
Require Import Lia.

Fixpoint step_fixpoint (n : nat)
  : forall {T}, (forall n, (forall n', 1 + n' = n -> T n') -> T n) -> T n.
  intros T f.
  apply f; clear f.
  destruct n.
  lia.
  intros n' S_n'_eq_S_n.
  rewrite (eq_add_S _ _ S_n'_eq_S_n).
  destruct n.
  apply f; lia.
  refine (f (S n) _).
  intros n' S_n'_eq_S_n.
  rewrite (eq_add_S _ _ S_n'_eq_S_n).
  exact (step_fixpoint n _ f).
Defined.

Fixpoint step_fixpoint (n : nat)
  : forall {T}, (forall n, (forall n', 1 + n' = n -> T n') -> T n) -> T n.
  intros T f.
  destruct n.
  apply f; lia.
  refine (f (S n) _).
  intros n' S_n'_eq_S_n.
  rewrite (eq_add_S _ _ S_n'_eq_S_n).
  exact (step_fixpoint n _ f).
Defined.

Fixpoint sized_fixpoint (n : nat)
  : forall {T}, (forall n, (forall n', n' < n -> T n') -> T n) -> T n.
  intros T f.
  destruct n.
  apply f; lia.
  refine (f (S n) _).
  intros n' n'_lt_S_n.
  destruct n.
  destruct (Nat.le_0_r n' ); clear H0.
  epose (H (le_S_n _ _ n'_lt_S_n)).
  
  
  Search (_ <= 0).
  Search (S _ <= S _ -> _ <= _).
  unfold "<" in n'_lt_S_n.
  unfold n'_lt_S_n.
  refine (f (S n) _).

  epose (sized_fixpoint n T f).
  
    intros n' n'_lt_0.
    lia.
    Search (_ < 0 -> _).

  := fun T f => f n (fun n' n_lt_n' => sized_fixpoint n' n_lt_n' T). *)

CoInductive CNat : nat -> Type :=
  | czero : CNat 0
  | csucc : forall pred (cpred : CNat pred), CNat (1 + pred).

Definition fold {a} (n : CNat a) : forall A, A -> (A -> A) -> A.
  intros A z s.
  refine (
    match n with
    | czero => z
    | csucc pred cpred => _
    end
  ).
  destruct n.
  exact z.
  exact (s (fold pred n A z s)).
Defined.

Definition Index := nat.

Inductive F_kind :=
  | K_linear
  | K_data.
Inductive F_type :=
  | T_arrow (param : F_type) (body : F_type)
  | T_var (n : Index)
  | T_forall (param : F_kind) (body : F_type).
Inductive F_expr :=
  | E_var (n : Index)
  | E_lambda (param : F_type) (body : F_expr)
  | E_apply (lambda : F_expr) (arg : F_expr)
  | E_type_lambda (param : F_kind) (body : F_expr)
  | E_type_apply (lambda : F_expr) (arg : F_type).

Inductive Context :=
  | C_empty
  | C_value (type : F_type) (next : Context)
  | C_type (kind : F_kind) (next : Context).

Fixpoint lookup_kind ctx (var : Index) : option F_kind :=
  match ctx with
  | C_empty => None
  | C_value type ctx => 
    match var with
    | 0 => None
    | S var => lookup_kind ctx var
    end
  | C_type kind ctx => 
    match var with
    | 0 => Some kind
    | S var => lookup_kind ctx var
    end
  end.

Inductive FK_eq : F_kind -> F_kind -> Type :=
  | FK_eq_linear : FK_eq (K_linear) (K_linear)
  | FK_eq_.
Inductive F_rule_type :=
  | FT_

Inductive FRule  :=
  | R_var ()

Definition False := {n : nat & n < 0}.

Theorem ind_false (f : False) : forall P, P f.
Proof.
  intros P.
  destruct f as [n n_lt_0].
  induction n.
  inversion n_lt_0.
  Search (0 < 0 -> _).
Qed.

Set Universe Polymorphism.
Set Definitional UIP.
Set Primitive Projections.
Check sigT.
Record sigP (A : Type) (P : A -> SProp) : Type := { l : A; r : P l }.
Inductive eqP (A : Type) (x : A) : A -> SProp := eqP_refl : eqP A x x.

Definition C_Bool : Type := forall A, A -> A -> A.
Definition c_true : C_Bool := fun A x y => x.
Definition c_false : C_Bool := fun A x y => y.

Definition I_Bool (c_b : C_Bool) : Type :=
  forall P : C_Bool -> Type, P c_true -> P c_false -> P c_b.
Definition i_true : I_Bool c_true := fun A x y => x.
Definition i_false : I_Bool c_false := fun A x y => y.

Definition Bool : Type := sigP C_Bool I_Bool.

Lemma true : Bool.
  refine (existT _ c_true _).
  refine ({| l := i_true; r := _ |}).
  intros oc_b oi_b H.
  destruct H.
Definition true : Bool := {| l := c_true; r := i_true |}.
Definition false : Bool := {| l := c_false; r := i_false |}.

Theorem sind_bool b : forall P : Bool -> SProp, P true -> P false -> P b.
  destruct b as [b_l b_r].
  intros P p_t p_f.
  refine (b_r (fun x_l => forall x_r : I_Bool x_l, _) _ _ b_r).
  intros x_r; exact p_t.
  intros x_r; exact p_f.
Qed.

Theorem ind_bool b : forall P : Bool -> Type, P true -> P false -> P b.
  destruct b as [b_l b_r].
  intros P.
  refine (b_l _ _ _).
  
  epose (b_l ).
  
  epose (b_l ).
  refine (b_r (fun x_l => forall x_r : I_Bool x_l, _) _ _ b_r).
  intros x_r; exact p_t.
  intros x_r; exact p_f.
Qed.

Require Import PeanoNat.
Require Import String.
Require Import Lia.

Require Import List.
Import ListNotations.

Definition Level := nat.
Definition Index := nat.

Print le.
Print lt.
Inductive Term :=
  | T_global_var (x : Level)
  | T_local_var (n : Index)
  | T_apply (lambda : Term) (arg : Term)
  | T_lambda (body : Term)
  | T_let (value : Term) (body : Term).

Fixpoint pack (level : Level) (depth : Index) (term : Term) :=
  match term with
  | T_global_var x =>
    _
  | T_local_var n =>
    _
  | T_apply lambda arg => 
    T_apply (pack level depth lambda) (pack level depth arg)
  | T_lambda body =>
    T_lambda (pack level (1 + depth) body)
  | T_let value body =>
    T_let (pack level depth value) (pack level (1 + depth) body)
  end.

(* substitution context *)
Inductive S_Context : Type :=
  | SC_hole
  | SC_let (value : Term) (body : S_Context).

Fixpoint sc_plug (ctx : S_Context) (term : Term) :=
  match ctx with
  | SC_hole => term
  | SC_let value body => T_let value (sc_plug body term)
  end.

(* reduction context *)
Inductive R_Context :=
  | RC_hole
  | RC_apply_l (lambda : R_Context) (arg : Term)
  | RC_apply_r (lambda : Term) (arg : R_Context)
  | RC_lambda (body : R_Context)
  | RC_let_v (value : R_Context) (body : Term)
  | RC_let_b (value : Term) (body : R_Context).

Fixpoint rc_plug (ctx : R_Context) (term : Term) :=
  match ctx with
  | RC_hole => term
  | RC_apply_l lambda arg => T_apply (rc_plug lambda term) arg
  | RC_apply_r lambda arg => T_apply lambda (rc_plug arg term)
  | RC_lambda body => T_lambda (rc_plug body term)
  | RC_let_v value body => T_let (rc_plug value term) body
  | RC_let_b value body => T_let value (rc_plug body term)
  end.

Fixpoint rc_bound (ctx : R_Context) : Index :=
  match ctx with
  | RC_hole => 0
  | RC_apply_l lambda arg => rc_bound lambda
  | RC_apply_r lambda arg => rc_bound arg
  | RC_lambda body => 1 + rc_bound body
  | RC_let_v value _ => rc_bound value
  | RC_let_b value body => 1 + rc_bound body
  end.

(* TODO: introduce notation *)
(* TODO: reduction modulo LC *)
Inductive Reduction : Term -> Term -> Type :=
  | R_beta : forall substs body arg,
    Reduction (T_apply (sc_plug substs (T_lambda body)) arg)
      (sc_plug substs (T_let arg body))
  | R_subst : forall value ctx,
    Reduction (T_let value (rc_plug ctx (T_bound_var (rc_bound ctx))))
      (T_let value (rc_plug ctx value)).


Lemma symm {A} {x y : A} (H : x = y) : y = x.
Proof.
  exact (eq_ind x (fun z => z = x) eq_refl y H).
Qed.


Print nat.
Definition four := S (S (S (S O))).
Print four.
Definition x := 1 + 2.
Definition f a := x + a.


Definition g {x'} (f' : forall ) a := f a
Definition f' {x'} (eq : x = x') : nat.
  epose (
    match eq with
    | eq_refl => f
    end
  ).
  unfold f.
  epose (f)




Definition incr : (
  let T := nat in
  T -> T
) := fun x => x.
Definition x := incr 1.

Check incr.

Definition a : 1 + 2 = 3 := eq_refl.

Inductive Term :=
  | T_free_var (var : nat)
  | T_bound_var (var : nat)
  | T_subst (body : Term) (value : Term)
  | T_shift (body : Term).

Fixpoint shift term depth :=
  match term with
  | T_free_var var => T_free_var var
  | T_bound_var var =>
    match Nat.leb var depth with
    | true => T_bound_var var
    | false => T_bound_var (1 + var)
    end
  | T_subst body value =>
    let value := shift value depth in
    let body := shift body (1 + depth) in
    T_subst body value
  | T_shift body =>
    let body := shift body depth in
    T_shift body
  end.

Fixpoint subst term from to_ :=
  match term with
  | T_free_var var => T_free_var var
  | T_bound_var var =>
    match Nat.eqb from var with
    | true => shift to_ 0 var
    | false => T_bound_var var
    end
  | T_subst body value =>
    let value := subst value from to_ in
    let body := subst body (1 + from) to_ in
    T_subst body value
  end.

Fixpoint normalize fuel term :=
  match fuel with
  | 0 => None
  | S fuel =>
    match term with
    | T_free_var var => Some (T_free_var var)
    | T_bound_var var => Some (T_bound_var var)
    | T_subst body value => 
      normalize fuel (subst body 0 value)
    end
  end.

Definition has_subst term :=
  match term with
  | T_free_var _ => false
  | T_bound_var _ => false
  | T_subst body value => true
  end.

Fixpoint normalize_no_subst fuel from to_ :
  normalize fuel from = Some to_ -> has_subst to_ = false.
  intros H.
  destruct fuel; simpl in H.
  inversion H.
  destruct from; inversion H; intuition.
  exact (normalize_no_subst fuel (subst from1 0 from2) to_ H).
Defined.
  
Fixpoint shift term depth by_ :=
  match term with
  | T_free_var var => T_free_var var
  | T_bound_var var =>
    match Nat.leb var depth with
    | true => T_bound_var var
    | false => T_bound_var (by_ + var)
    end
  | T_lambda body =>
    let body := shift body (1 + depth) by_ in
    T_lambda body
  | T_apply lambda arg =>
    let lambda := shift lambda depth by_ in
    let arg := shift arg depth by_ in
    T_apply lambda arg
  end.
  (* | T_lambda (body : Term)
  | T_apply (lambda : Term) (arg : Term). *)
  (* | T_subst (body : Term) (value : Term). *)
  (* | T_shift (body : Term) (by_ : nat). *)


Fixpoint shift term depth by_ :=
  match term with
  | T_free_var var => T_free_var var
  | T_bound_var var =>
    match Nat.leb var depth with
    | true => T_bound_var var
    | false => T_bound_var (by_ + var)
    end
  | T_lambda body =>
    let body := shift body (1 + depth) by_ in
    T_lambda body
  | T_apply lambda arg =>
    let lambda := shift lambda depth by_ in
    let arg := shift arg depth by_ in
    T_apply lambda arg
  end.
Fixpoint subst term from to_ :=
  match term with
  | T_free_var var => T_free_var var
  | T_bound_var var =>
    match Nat.eqb from var with
    | true => shift to_ 0 var
    | false => T_bound_var var
    end
  | T_lambda body =>
    let body := subst body (1 + from) to_ in
    T_lambda body
  | T_apply lambda arg =>
    let lambda := subst lambda from to_ in
    let arg := subst arg from to_ in
    T_apply lambda arg
  end.

Fixpoint normalize fuel term :=
  match fuel with
  | 0 => None
  | S fuel =>
    match term with
    | T_free_var var => Some (T_free_var var)
    | T_bound_var var => Some (T_bound_var var)
    | T_lambda body => 
      match normalize fuel body with
      | None => None
      | Some body => Some (T_lambda body)
      end
    | T_apply lambda arg =>
      match normalize fuel lambda, normalize fuel arg with
      | None, None => None
      | Some _, None => None
      | None, Some _ => None
      | Some lambda, Some arg =>
        match lambda with
        | T_lambda body => normalize fuel (subst body 0 arg)
        | lambda => Some (T_apply lambda arg)
        end
      end
    end
  end.

Fixpoint has_beta term :=
  match term with
  | T_free_var _ => false
  | T_bound_var _ => false
  | T_lambda body => has_beta body
  | T_apply lambda arg => 
    match lambda with
    | T_lambda _ => true
    | lambda => orb (has_beta lambda) (has_beta arg)
    end
  end.

Fixpoint normalize_no_beta fuel from to_ :
  normalize fuel from = Some to_ ->
  has_beta to_ = false.
  intros H; destruct fuel.
  inversion H.
  destruct from.
  simpl in H; inversion H; auto.
  simpl in H; inversion H; auto.
  simpl in H; destruct from.
  destruct fuel.
  epose (normalize_no_beta fuel _ t).
  apply (normalize_no_beta fuel from to_).

  induction fuel; simpl in *.
  inversion H.
  induction from; simpl in *.
  
  simpl in H.
  destruct (normalize fuel from).
  epose (normalize_no_beta fuel from).
  destruct (normalize )

  destruct 
  
  destruct from.
  inversion H; intuition.
  inversion H; intuition.
  apply IHfuel.
  simpl.
  
  
  
  simpl in H.
  
  unfold normalize in *.
  
Definition shift_var to_ var shifts :=
  match shifts with
  | nil => var
  | cons (by_, at_) shifts => (
      let substs := to_ - at_ in
      match var > substs with
      | true =>
          let var := var + by_ in
          shift_var at_ var shifts
      | false => var
      end)
  end

Require Import PeanoNat.
Require Import String.
Require Import Lia.

Require Import List.
Import ListNotations.

From Coq Require Import Program.

Program Fixpoint sized_fixpoint (n : nat) {measure n}
  : forall {T}, (forall n, (forall n', n' < n -> T n') -> T n) -> T n
  := fun T f => f n (fun n' n_lt_n' => sized_fixpoint n' n_lt_n' T).

Definition Fuel := nat.

Definition Fueled A :=
  forall (bef : Fuel), option (A * {aft : Fuel | aft <= bef}).

Definition fueled_fixpoint {T} (f : Fueled T -> Fueled T) : Fueled T.
  intros fuel; refine (@sized_fixpoint fuel (fun fuel => _) _).
  clear fuel; intros fuel self.
  
  

   intros fuel self; refine (f _ fuel).
  intros fuel'.
  intros fuel'; refine (self fuel' _).
  
Definition pure {A} x : Fueled A :=
  fun bef => Some (x, exist _ bef (le_n _)).

Lemma fueled_fixpoint {T} (f : Fueled T -> Fueled T) : Fueled T.
  let self :=
    match bef with;
    | 0 => None
    | S bef =>
      match fueled_fixpoint bef f with
      | Some (value, aft_and_aft_le_bef) => 
        let (aft, aft_le_bef) := aft_and_aft_le_bef in
        Some (value, exist _ aft (le_S _ _ aft_le_bef))
      | None => None
      end
    end in
  f bef self.

(* TODO: fast numbers *)
Definition Name := string.
Definition Level := nat.
Definition Index := nat.
Definition Fuel := nat.

Inductive Term : Set :=
  | T_annot (term : Term) (annot : Term)
  | T_free_var (var : Level)
  | T_bound_var (var : Index)
  | T_forall (param : Term) (body : Term)
  | T_lambda (param : Term) (body : Term)
  | T_apply (lambda : Term) (arg : Term).

(* with Pat : Set :=
  | P_annot (pat : Pat) (annot : Term)
  | P_var (name : Name). *)

(* Globals *)
Definition T_univ := T_free_var 0.

(* Monad *)
Definition bind {A B} (m : option A) f : option B :=
  match m with
  | Some value => f value
  | None => None
  end.
Notation "'let*' x ':=' c1 'in' c2" := (bind c1 (fun x => c2))
  (right associativity, at level 84, c1 at next level).

Inductive Size : Prop := suc : Size -> Size.

Inductive Fueled_data A : Fuel -> Type :=
  | Next : forall aft bef, aft <= bef -> A -> Fueled_data A bef
  | Fail : forall fuel, Fueled_data A fuel.

Definition Fueled A := forall fuel, Fueled_data A fuel.

Definition fueled_bind {A B} : Fueled A -> (A -> Fueled B) -> Fueled B.
  intros m f bef; refine (
    match m bef with
    | Next _ aft bef aft_le_bef value => _
    | Fail _ fuel => Fail _ fuel
    end
  ).
  epose (f value aft).
  epose (f a ).
  
  epose (Next _ _ _ _ a).
  



Inductive Fueled A : Type :=
  | Fueled_W : forall fuel, option (A * Fueled_prop A fuel) -> Fueled A
with Fueled_prop A : Fuel -> Prop :=
  | .
(* TODO: move to CPS *)
Inductive Fueled A : Type :=
  | Pure : forall (fuel : Fuel), (x : A)  -> Fueled A
  | Bind (B : Type) (m : Fueled B) (f : B -> Fueled A).
Definition pure {A} (x : A) : Fueled A :=
  fun fuel => exist (fun m => Fueled_prop A fuel m) (I_pure x) (Pure A fuel x).
Definition fail {A} : Fueled A :=
  fun fuel => exist (fun m => Fueled_prop A fuel m) None (Fail A fuel).

Definition bind_fuel {A B} (m : Fueled A) : (A -> Fueled B) -> Fueled B.
  intros f fuel; destruct (m fuel) as [m_i m_prop]; clear m.
  destruct m_i.

  destruct p as [value [inter inter_le_fuel]]. 
  destruct (f value inter) as [f_i f_prop].

  

  destruct f_i.
  destruct p as [result [rest rest_le_inter]].

  assert (rest <= fuel).
  exact (Nat.le_trans _ _ _ rest_le_inter inter_le_fuel).

  exists (I_pure result).
  epose (Bind_pure B rest A fuel _ value (Pure _ _ value) H).

  

  
  exist (I_return result).
  exist (Some (result, exist _ rest H)).

  

  

  epose
  epose (Some (result, exist _ rest H)).

  refine (Bind_some B A rest fuel _ value _ _).
  refine (Bind_some B fuel A rest _ value _).
  apply Return.

  epose (Bind_some B fuel).
  apply Bind_some.
  refine (
    match i with
    | Some result => _
    | None => None
    end
  ).
  destruct i_m as [value [inter inter_le_fuel]].

  intros a b A B m f.



Definition fueled_fixpoint2 {A}
  (f : Fueled A) : Fueled A.


(* Machinery *)
Fixpoint open (from : Index) to_ term :=
  match term with 
  | T_annot term annot => 
    let annot := open from to_ annot in
    let term := open from to_ term in
    T_annot term annot
  | T_free_var var => T_free_var var
  | T_bound_var var => 
    match Nat.eqb var from with
    | true => to_
    | false => T_bound_var var
    end
  | T_forall param body =>
    let param := open from to_ param in
    let body := open (1 + from) to_ body in
    T_forall param body
  | T_lambda param body =>
    let param := open from to_ param in
    let body := open (1 + from) to_ body in
    T_lambda param body
  | T_apply lambda arg =>
    let lambda := open from to_ lambda in
    let arg := open from to_ arg in
    T_apply lambda arg
  end.
(* with open_pat (from : Index) to_ pat :=
  (* TODO: fancy pattern *)
  match pat with
  | P_annot pat annot =>
    let annot := open from to_ annot in
    let pat := open_pat from to_ pat in
    P_annot pat annot
  | P_var name => P_var name
  end. *)

Fixpoint close (from : Level) to_ term :=
  match term with 
  | T_annot term annot => 
    let annot := close from to_ annot in
    let term := close from to_ term in
    T_annot term annot
  | T_free_var var =>
    match Nat.eqb var from with
    | true => T_bound_var to_
    | false => T_free_var var
    end
  | T_bound_var var => T_bound_var var
    
  | T_forall param body =>
    let param := close from to_ param in
    let body := close from (1 + to_) body in
    T_forall param body
  | T_lambda param body =>
    let param := close from to_ param in
    let body := close from (1 + to_) body in
    T_lambda param body
  | T_apply lambda arg =>
    let lambda := close from to_ lambda in
    let arg := close from to_ arg in
    T_apply lambda arg
  end.
(* with close_pat (from : Level) to_ pat :=
  (* TODO: fancy pattern *)
  match pat with
  | P_annot pat annot =>
    let annot := close from to_ annot in
    let pat := close_pat from to_ pat in
    P_annot pat annot
  | P_var name => P_var name
  end. *)

Fixpoint equal left right :=
  match left, right with
  | T_annot left_term left_annot, T_annot right_term right_annot =>
    let* tt := equal left_annot right_annot in
    equal left_term right_term
  | T_free_var left_var, T_free_var right_var =>
  match Nat.eqb left_var right_var with
    | true => Some tt
    | false => None
    end
  | T_bound_var left_var, T_bound_var right_var =>
    match Nat.eqb left_var right_var with
    | true => Some tt
    | false => None
    end
  | T_forall left_param left_body, T_forall right_param right_body =>
    let* tt := equal left_param right_param in
    equal left_body right_body
  | T_lambda left_param left_body, T_lambda right_param right_body =>
    let* tt := equal left_param right_param in
    equal left_body right_body
  | T_apply left_lambda left_arg, T_apply right_lambda right_arg =>
    let* tt := equal left_lambda right_lambda in
    equal left_arg right_arg
  | _, (T_annot _ _
    | T_free_var _
    | T_bound_var _
    | T_forall _ _
    | T_lambda _ _
    | T_apply _ _
  ) => None
  end.
(* with equal_pat left right :=
  match left, right with
  | P_annot left_pat left_annot, P_annot right_pat right_annot =>
    let* tt := equal left_annot right_annot in
    equal_pat left_pat right_pat
  | P_var _left_name, P_var _right_name => Some tt
  | _, (P_annot _ _ | P_var _) => None
  end. *)

(* Typer *)

Definition Context := list Term.
Definition current_level (ctx : Context) := List.length ctx.
Definition open_level (ctx : Context) term :=
  let from := 0 in
  let to := T_free_var (current_level ctx) in
  open from to term.

Definition split_forall term :=
  (* TODO: expand head *)
  match term with
  | T_forall param body =>
    Some (param, body)
  | _ => None
  end.

Fixpoint infer_var (ctx : Context) (var : Level) :=
  match ctx with
  | [] => None
  | head :: ctx =>
    match Nat.eqb (1 + List.length ctx) var with
    | true => Some head
    | false => infer_var ctx var
    end
  end.

Notation "'let@@' x ':=' c1 'in' c2" := (c1 (fun x => c2))
  (right associativity, at level 84, c1 at next level).


Fixpoint infer (ctx : Context) term :=
  match term with
  | T_annot term annot =>
    let* annot_type := infer ctx annot in
    let* tt := equal annot_type T_univ in
    let* term_type := infer ctx term in
    let* tt := equal term_type annot_type in
    Some annot_type
  | T_free_var var => infer_var ctx var
  | T_bound_var var => None
  | T_forall param body =>
    let* param_type := infer ctx param in
    let* tt := equal param_type T_univ in
    let* body_type :=
      let body := open_level ctx body in
      infer (param :: ctx) body in
    let* tt := equal body_type T_univ in
    Some T_univ
  | T_lambda param body =>
    let* param_type := infer ctx param in
    let* tt := equal param_type T_univ in
    let* body_type := infer (param :: ctx) body in
    Some (T_forall param body_type)
  | T_apply lambda arg =>
    let* lambda := infer ctx lambda in
    let* forall_ := split_forall lambda in
    let (param, return_type) := forall_ in
    let* arg_type := infer ctx arg in
    let* tt := equal arg_type param in
    Some (open 0 arg return_type)
  end.

Definition bind {A B} (M : Fueled A) (f : A -> Fueled B) : Fueled B.
  intros fuel0; refine (
    match M fuel0 with
    | Some (value, fuel1) => _
    | None => None
    end
  ).
  destruct fuel1 as [fuel1 fuel1_lt_fuel0].
  refine (
    match f value fuel1 with
    | Some (value, fuel2) => _
    | None => _
    end
  ).

  fun fuel =>
    match M fuel with
    | Some (value, rest) => f value rest
    | None => _
    end.

Definition Fueled (A : Type) :=
  forall (bef : nat), option  (A * {aft | bef < aft}).



Class Bind (M : Type -> Type) := {
  bind : forall {A B}, M A -> (A -> M B) -> M B;
}.
Notation "'let*' x ':=' c1 'in' c2" := (bind c1 (fun x => c2))
  (right associativity, at level 84, c1 at next level).

Definition Fueled (A : Type) :=
  forall (bef : nat), option (A * {aft | bef < aft}).


Lemma unfold {A} : forall f n,
  step_fixpoint (1 + n) f = f n _.

From Coq Require Import Program.

Program Fixpoint sized_fixpoint (n : nat) {measure n}
  : forall {T}, (forall n, (forall n', n' < n -> T n') -> T n) -> T n
  := fun T f => f n (fun n' n_lt_n' => sized_fixpoint n' n_lt_n' T).
Check sized_fixpoint.
Lemma add (x y : nat) : nat.
  refine (sized_fixpoint x (fun n => forall x, n = x -> nat) _ x eq_refl).
  clear x; intros n add x n_eq_x; destruct n_eq_x.
  destruct n.
  exact y.
  exact (S (add n (Nat.lt_succ_diag_r _) n eq_refl)).
Defined.

Lemma add_2 : add 1 2 = 3.
  unfold add, sized_fixpoint, Fix_sub, Fix_F_sub.
  rewrite Wf.WfExtensionality.fix_sub_eq_ext.
  unfold_sub.
  epose (Wf.F_unfold nat).
  simpl.
  intros 
  sized_fixpoint x (fun x')
Print sized_fixpoint.

  refine (step_fixpoint n _); clear n.
  intros n sized_fixpoint.
  intros A f; refine (f n _); intros n' n_lt_n'.
  refine (sized_fixpoint)
  destruct n.
  destruct (Nat.neq_0_succ _ n_eq_S_n').
  exact (step_fixpoint n _ f).
Defined.

Lemma fueled_fixpoint {A B} (f : (A -> Fueled B) -> A -> Fueled B)
  : A -> Fueled B.
  intros x fuel.
  refine (f _ x fuel).
  
  apply step_f
  epose (f _).
  intros fuel.
  destruct (f fuel).
  induction fuel.
  exact None.
  destruct (f fuel).

  
Instance Fueled_bind : Bind Fueled := {
  bind := fun {A B} x f =>
    fun bef =>
      match bef with
      | 0 =>
    match x with
    | None => None
    | Some y => f y
    end
}.

Instance Option_monad : Monad option := {
  pure := fun {A} x => Some x;
  bind := fun {A B} x f =>
    match x with
    | None => None
    | Some y => f y
    end
}.

Module Fueled.
  
End M.
Definition bind 
Notation "x <- c1 ;; c2" := (bind c1 (fun x => c2))
                        (right associativity, at level 84, c1 at next level)
Fixpoint expand_head fuel term {struct fuel} :=
  match fuel with
  | 0 => term
  | S fuel =>
    match term with
    | T_annot term _annot => expand_head term
    | T_var var => T_var var
    | T_forall param body => T_forall param body
    | T_lambda param body => T_lambda param body
    | T_apply lambda arg =>
        match expand_head lambda with
      | T_lambda _param body, fuel =>
        (* TODO: proper eliminate param *)
        expand_head body (arg :: substs)
      | lambda, fuel => (T_apply lambda arg)
      end
    end
  end.




Definition Substs := list Term.


Fixpoint lookup_bound_substs var substs : option (Term * Substs) :=
  match substs with
  | [] => None
  | to_ :: substs =>
    match var with
    | 0 => Some (to_, substs)
    | S var => lookup_bound_substs var substs
    end
  end.



Fixpoint t_fold_vars {A} free_var bound_var binder (acc : A) term :=
  let t_fold_vars acc term :=
    t_fold_vars free_var bound_var binder acc term in
  let p_fold_vars acc pat := 
    p_fold_vars free_var bound_var binder acc pat in
  match term with
  | T_annot term annot => 
    let acc := t_fold_vars acc annot in
    t_fold_vars acc term
  | T_free_var var => free_var acc var
  | T_bound_var var => bound_var acc var
  | T_forall param body => 
    let acc := p_fold_vars acc param in
    t_fold_vars acc body
  | T_lambda param body =>
    let acc := p_fold_vars acc param in
    t_fold_vars acc body
  | T_apply lambda arg =>
    let acc := t_fold_vars acc lambda in
    t_fold_vars acc arg
  end

with p_fold_vars {A} free_var bound_var binder (acc : A) pat :=
  let t_fold_vars acc term :=
    t_fold_vars free_var bound_var binder acc term in
  let p_fold_vars acc pat := 
    p_fold_vars free_var bound_var binder acc pat in
  match pat with
  | P_annot pat annot => 
    let acc := t_fold_vars acc annot in
    p_fold_vars acc pat
  | P_var name => binder acc name
  end.

Fixpoint is_lc_term max_level max_index term : bool :=
  let under_is_lc_term term := is_lc_term max_level (1 + max_index) term in
  let is_lc_term term := is_lc_term max_level max_index term in
  let is_lc_pat pat := is_lc_pat max_level max_index pat in
  match term with
  | T_annot term annot => andb (is_lc_term term) (is_lc_term annot)
  | T_free_var var => Nat.leb var max_level
  | T_bound_var var => 
    match max_index with
    | 0 => false
    | S max_index => Nat.ltb var max_index
    end
  | T_forall param body => andb (is_lc_pat param) (under_is_lc_term body)
  | T_lambda param body => andb (is_lc_pat param) (under_is_lc_term body)
  | T_apply lambda arg => andb (is_lc_term lambda) (is_lc_term arg)
  end

with is_lc_pat max_level max_index pat :=
  let is_lc_term term := is_lc_term max_level max_index term in
  let is_lc_pat pat := is_lc_pat max_level max_index pat in
  match pat with
  | P_annot pat annot => andb (is_lc_pat pat) (is_lc_term annot)
  | P_var name => true
  end.


(* TODO: SProp? *)
Definition V_Term := {term | is_lc_term max_level term = true}.
Definition expand_head_step (term : Term) (substs : Substs) :=
  match term with
  | T_wrapped term _type => (term, substs)
  | T_free_var var => (T_free_var var, substs)
  | T_bound_var var => (T_bound_var var, substs)
  | T_forall param body => 

  end.

Fixpoint expand_head (fuel : Fuel) :
  forall (term : Term) (substs : Substs), (Term * Substs).
  intros term substs.
  refine (
    match fuel with
    | O => (term, substs)
    | S fuel => 
  )


Inductive Term : Set :=
  | T_univ
  | T_var (n : nat)
  | T_forall (param : Term) (body : Term)
  | T_lambda (param : Term) (body : Term)
  | T_apply (lambda : Term) (arg : Term)
  | T_subst (body : Term) (value : Term)
  | T_coerce (body : Term) (path : Path).

Inductive Step : Set :=
  | Subst
  | Beta.
Definition Path := list Step.

Inductive Term : Set :=
  | T_univ
  | T_var (n : nat)
  | T_forall (param : Term) (body : Term)
  | T_lambda (param : Term) (body : Term)
  | T_apply (lambda : Term) (arg : Term)
  | T_subst (body : Term) (value : Term)
  | T_coerce (body : Term) (path : Path).

(* TODO: notation *)
Fixpoint subst (term : Term) (from : nat) (to_ : Term) :=
  match term with
  | T_univ => T_univ
  | T_var n =>
    match Nat.eqb n from with
    | true => to_
    | false => T_var n
    end
  | T_forall param body =>
    T_forall (subst param from to_) (subst body (1 + from) to_)
  | T_lambda param body =>
    T_lambda (subst param from to_) (subst body (1 + from) to_)
  | T_apply lambda arg =>
    T_apply (subst lambda from to_) (subst arg from to_)
  | T_subst body value =>  
    T_subst (subst body (1 + from) to_) (subst value from to_)
  | T_coerce term path =>
    T_coerce (subst term from to_) path
  end.

Fixpoint step (op : Term -> Term) (term : Term) :=
  match term with 
  | T_univ => op T_univ
  | T_var n => op (T_var n)
  | T_forall param body =>
    T_forall (op (step op param)) (op (step op body))
  | T_lambda param body =>
    T_lambda (op (step op param)) (op (step op body))
  | T_apply lambda arg =>
    T_apply (op (step op lambda)) (op (step op arg))
  | T_subst body value =>  
    T_subst (op (step op body)) (op (step op value))
  | T_coerce term path =>
    T_coerce (op (step op term)) path
  end.

Definition head_step_subst term :=
  match term with
  | T_subst body value => subst body 0 value 
  | term => term
  end.
Definition idem (f : Term -> Term) := forall term, f term = f (f term).

Theorem head_subst_idem : idem head_step_subst.
  intros term; induction term; intuition.
  simpl; induction term1; intuition.
  simpl; destruct (Nat.eqb n 0).
  simpl; induction term2; intuition; simpl in *.
  intuition.
  intros term2 eq.
  intuition.
  induction term1; intuition; simpl in *.
  destruct (Nat.eqb n 0).

  unfold head_step_subst in *; simpl in *.
Definition step_subst term :=
  step head_step_subst term.
Theorem step_subst_idem {term} : step_subst term = step_subst (step_subst term).
  induction term; intuition.
  unfold step_subst; simpl.
  

Fixpoint reduce (term : Term) (path : Path) :=
  match path with
  | [] => []
  | step :: path =>
    match step with
    | Subst => reduce (step subst 
    end
  end.
  

Definition Context := list Term.
Fixpoint find (ctx : Context) (n : nat) :=
  match ctx with
  | [] => None
  | hd :: ctx =>
    match n with
    | 0 => Some hd
    | S n' => find ctx n'
    end
  end.

Inductive Check : Context -> Term -> Term -> Set :=
  | C_univ : forall ctx, Check ctx T_univ T_univ
  | C_var : forall ctx n A,
    find ctx n = Some A ->
    Check ctx (T_var n) A
  | C_forall : forall ctx A B,
    Check ctx A T_univ -> Check ctx B T_univ ->
    Check ctx (T_forall A B) T_univ
  | C_lambda : forall ctx A M B,
    Check ctx (T_forall A B) T_univ -> Check (A :: ctx) M B ->
    Check ctx (T_lambda A M) (T_forall A B)
  | C_apply : forall ctx M N A B,
    Check ctx M (T_forall A B) -> Check ctx N A ->
    Check ctx (T_apply M N) (T_subst B N).
Fixpoint add1 n m :=
  match n with
  | O => m
  | S n' => S (add1 n' m)
  end.

Fixpoint add2 n m :=
  match m with
  | O => n
  | S m' => S (add2 n m')
  end.

Fixpoint add3 n m :=
  match n with
  | O => m
  | S n' => S (add3 n' m)
  end.

Theorem add1_eq_add2 {n m} : add1 n m = add2 m n.
  unfold add1, add2.
  

  epose (e := (eq_refl : (add1 n m = match n with | O => _ | S _ => _ end))).
  cbv.
  reflexivity.



Require Import PeanoNat.
Require Import Lia.

Definition true : forall [A : Type], A -> A -> A := fun A x y => x.

Definition id : forall [A : Type], A -> A := fun A x => x.

Definition x := true id.
Check x.


Fixpoint step_fixpoint T a :
  (forall a, (forall b, 1 + b = a -> T b) -> T a) -> T a.
  intros f; refine (f a _).
  intros b S_b_eq_a.
  destruct a.
  inversion S_b_eq_a.
  inversion S_b_eq_a; rewrite H0 in *.
  exact (step_fixpoint T a f).
Defined.

Definition multi_step_fixpoint T a :
  (forall a, (forall x b, 1 + x + b = a -> T b) -> T a) -> T a.
  intros f; refine (
    step_fixpoint (fun a => forall x b, x + b = a -> T b)
      a _ 0 a eq_refl
  ).
  clear a; intros a self x b b_eq_a.
  destruct b_eq_a; destruct x; simpl in *.
  refine (f b _); intros x b0 rel.
  exact (self (x + b0) rel _ _ eq_refl).
  exact (self (x + b) eq_refl _ _ eq_refl).
Defined.

Lemma le_diff b a : b <= a -> {x | x + b = a}.
  intros b_le_a; exists (a - b); lia.
Qed.

Definition sized_fixpoint T a :
  (forall a, (forall b, b < a -> T b) -> T a) -> T a.
  intros f; refine (multi_step_fixpoint T a _).
  clear a; intros a self; refine (f a _).
  intros b b_lt_a.
  destruct (le_diff _ _ b_lt_a) as [x diff]; clear b_lt_a.
  refine (self x b _); lia.
Defined.

Inductive Term : Set :=
  | Var (n : nat)
  | Lam (body : Term)
  | App (lam : Term) (arg : Term).

Fixpoint subst (term : Term) (var : nat) (to_ : Term) :=
  match term with
  | Var n => if (Nat.eqb var n) then to_ else Var n
  | Lam body => Lam (subst body (1 + var) to_)
  | App lam arg => App (subst lam var to_) (subst arg var to_)
  end.

Fixpoint count (term : Term) (var : nat) :=
  match term with
  | Var n => if (Nat.eqb var n) then 1 else 0
  | Lam body => count body (1 + var)
  | App lam arg => count lam var + count arg var
  end.

Fixpoint beta (term : Term) :=
  match term with
  | Var n => Var n
  | Lam body => Lam (beta body)
  | App lam arg =>
    match lam with
    | Lam body => subst body 0 (beta arg)
    | lam => App (beta lam) (beta arg)
    end
  end.

Fixpoint has_beta (term : Term) :=
  match term with
  | Var n => false
  | Lam body => has_beta body
  | App lam arg =>
    match lam with
    | Lam body => true
    | lam => orb (has_beta lam) (has_beta arg)
    end
  end.

Fixpoint size (term : Term) :=
  match term with
  | Var n => 0
  | Lam body => 1 + size body
  | App lam arg => size lam + size arg
  end.

Lemma subst_multiplies_value {term var to_}
  : size term + (count term var * size to_) = size (subst term var to_).
  revert var; induction term; intros var; simpl; auto.
  destruct (Nat.eqb var n); auto.
  epose (IHterm1 var); epose (IHterm2 var); lia.
Defined.

Inductive Affine : Term -> Set :=
  | A_Var : forall n, Affine (Var n)
  | A_Lam : forall body,
    Affine body -> Affine (Lam body)
  | A_App : forall lam arg,
    Affine lam -> Affine arg -> Affine (App lam arg).

Inductive Affine : Term -> Set :=
  | A_Var : forall n, Affine (Var n)
  | A_Lam : forall body,
    Affine body -> Affine (Lam body)
  | A_App : forall lam arg,
    Affine lam -> Affine arg -> Affine (App lam arg).

Lemma affine_is_affine {term} : Affine term -> forall n, count term n <= 1.
  admit.
Admitted.

Lemma affine_beta_is_affine {term} : Affine term -> Affine (beta term).
  admit.
Admitted.

Lemma affine_beta_reduces_size {term}
  : Affine term -> true = has_beta term -> size (beta term) < size term.
  intros is_affine_term term_has_beta.
  induction is_affine_term; simpl in *.
  inversion term_has_beta.
  exact (le_n_S _ _ (IHis_affine_term term_has_beta)).
  admit.
Admitted.

Definition affine_normalize_open  a
  (self : forall b, b < a ->
    forall term, Affine term -> b = size term -> Term)
  : forall term, Affine term -> a = size term -> Term.
  intros term term_is_affine a_is_size.
  rewrite a_is_size in *; clear a a_is_size.
  remember (has_beta term).
  destruct b.
  exact (self _ (affine_beta_reduces_size term_is_affine Heqb)
    (beta term) (affine_beta_is_affine term_is_affine) eq_refl).
  exact term.
Defined.

Definition affine_normalize {term} : Affine term -> Term.
  intros term_is_affine; exact (sized_fixpoint
    (fun a => forall term, Affine term -> a = size term -> _)
    _ affine_normalize_open _ term_is_affine eq_refl
  ).
Defined.

Lemma affine_normalize_has_no_beta {term} (term_is_affine : Affine term)
  : false = has_beta (affine_normalize term_is_affine).
  induction term_is_affine; simpl; auto.
  unfold affine_normalize, sized_fixpoint, multi_step_fixpoint, step_fixpoint.

  destruct (unroll_sized_fixpoint (fun a : nat =>
  forall term0 : Term,
  Affine term0 -> a = size term0 -> Term) (size term) affine_normalize_open)
    as [k eq].
  rewrite eq; clear eq.
  induction term; simpl; auto.
  unfold affine_normalize_open.
  simpl.
  unfold affine_normalize_open.
  destruct (has_beta term).
  forall term0 : Term,
  Affine term0 -> a = size term0 -> Term) (size term) _).
  induction term_is_affine; simpl; intuition.
  unfold affine_normalize.
  destruct (unroll_sized_fixpoint _ _ _) as [H].
  epose (sized_fixpoint).
  simpl in *.
  fold (affine_normalize term_is_affine) in *
  fold affine_normalize in *.
  r

Definition x : 1 + 2 = 4 := eq_refl.

Require Import List.
Import ListNotations.

Inductive Forest a : Type
  := Empty    : Forest a
  |  WithTree : Tree a -> Forest a -> Forest a
with Tree a : Type
  := Branch : bool -> a -> Forest a -> Tree a.

Inductive Typ :=
  | T_ctx (typ : Typ) (body : list Entry)
  | T_var (n : nat)
  | T_arrow (param : Typ) (body : Typ)
  | T_forall (body : Typ)
with Expr :=
  | E_ctx (expr : Expr) (body : list Entry)
  | E_var (n : nat)
  | E_lam (param : Typ) (body : Expr)
  | E_app (lam : Expr) (arg : Expr)
  | ET_lam (body : Expr)
  | ET_app (lam : Expr) (arg : Typ)
with Entry :=
  | C_lam (annot : Typ)
  | C_subst (annot : Typ) (value : Expr)
  | CT_lam
  | CT_subst (value : Typ) (body : Expr).

Inductive Has_type : Expr -> Typ -> Type :=
  | I_var_zero_abs : forall Γ A,
    Has_type (E_ctx (E_var 0) (C_lam A :: Γ)) (T_ctx A Γ)
  | I_var_succ_abs : forall Γ Δ A B n,
    Has_type (E_ctx (E_var n) Γ) (T_ctx B Δ) ->
    Has_type (E_ctx (E_var (1 + n)) (C_lam A :: Γ)) (T_ctx B Δ)
  | I_lambda : forall Γ A B M,
    Has_type (E_ctx M (C_lam A :: Γ)) B ->
    Has_type (E_ctx (E_lam A M) Γ) (T_arrow (T_ctx A Γ) B)
  .

Fixpoint add n m :=
  match n with
  | 0 => m
  | S n' => S (add n' m)
  end.

Theorem uip (t : True) (eq : t = t) : eq = eq_refl.
Proof.
  destruct t.
  exact (
    match eq with
    | eq_refl _ => eq_refl
    end
  ).
  reflexivity.
Qed.

Inductive sFalse : SProp :=.
Inductive sTrue : SProp := | sI.

Set Primitive Projections.
Record ssig (A : Type) (P : A -> SProp) : Type := { l : A; r : P l }.

Definition C_Bool : Type := forall A, A -> A -> A.
Definition c_true : C_Bool := fun A x y => x.
Definition c_false : C_Bool := fun A x y => y.

Definition I_Bool (c_b : C_Bool) : SProp :=
  forall P : C_Bool -> SProp, P c_true -> P c_false -> P c_b.
Definition i_true : I_Bool c_true := fun A x y => x.
Definition i_false : I_Bool c_false := fun A x y => y.

Definition Bool : Type := ssig C_Bool I_Bool.
Definition true : Bool := {| l := c_true; r := i_true |}.
Definition false : Bool := {| l := c_false; r := i_false |}.

Theorem ind_bool b : forall P : Bool -> SProp, P true -> P false -> P b.
  intros P p_t p_f.
  destruct b as [b_l b_r].
  refine (b_r (fun x_l => forall x_r : I_Bool x_l, _) _ _ b_r).
  intros x_r; exact p_t.
  intros x_r; exact p_f.
Qed.

(* TODO: this is a very lazy definition d*)
Definition Mu (G : _ -> Type) :=
  forall (C_Nat_R : Type), (G C_Nat_R -> C_Nat_R) -> C_Nat_R.
Definition C_Nat_G (C_Nat_R : Type) : Type :=
    forall A, A -> (C_Nat_R -> A) -> A.
Definition C_Nat : Type := Mu C_Nat_G.
Definition c_zero : C_Nat :=
    fun C_Nat_R w => w (fun A z s => z).
Definition c_succ (pred : C_Nat) : C_Nat :=
    fun C_Nat_R w => w (fun A z s => s (pred _ w)).

Definition Mu_Nat (G : _ -> _ -> SProp) c_n :=
  forall (I_Nat_R : C_Nat -> SProp),
    (forall c_n, G I_Nat_R c_n -> I_Nat_R c_n) -> I_Nat_R c_n.
Definition I_Nat_G (I_Nat_R : C_Nat -> SProp) (c_n : C_Nat) : SProp :=
  forall P : C_Nat -> SProp, P c_zero ->
    (forall c_pred, I_Nat_R c_pred -> P (c_succ c_pred)) -> P c_n.
Definition I_Nat : C_Nat -> SProp := Mu_Nat I_Nat_G.
Definition i_zero : I_Nat c_zero.
  intros I_Nat_R w; apply w.
  intros P p_z p_s; exact p_z.
Defined.
Definition i_succ {c_pred} (pred : I_Nat c_pred) : I_Nat (c_succ c_pred).
  intros I_Nat_R w; apply w.
  intros P p_z p_s; apply (p_s _ (pred _ w)).
Defined.

Theorem weak_ind_nat {c_n} (i_n : I_Nat c_n) :
  forall P : C_Nat -> SProp, P c_zero ->
  (forall c_pred, P c_pred -> P (c_succ c_pred)) -> P c_n.
  intros P p_z p_s.
  apply i_n; clear c_n i_n.
  intros c_n w; apply w.
  exact p_z.
  exact p_s.
Qed.

Definition Nat : Type := ssig C_Nat I_Nat.
Definition zero : Nat := {| l := _; r := i_zero |}.
Definition succ (pred : Nat) : Nat := {| l := _; r := i_succ (pred.(r _ _)) |}.

Theorem i_pred {c_pred} (i_s_pred : I_Nat (c_succ c_pred)) : I_Nat c_pred.
  intros I_Nat_R w; apply w.
  intros P p_z p_s.
  apply (i_s_pred
    (fun c_n => P (c_n _ (fun c => c SProp sTrue (fun c_pred => c_pred))))).
  intros c_n w2; apply w2.
  exact sI.
  intros c_pred2 H; unfold c_succ.
  epose (p_s )
  epose (i_s_pred _ w).
  apply i_s_pred.

(* TODO: this is a lazy proof *)
Theorem ind_nat n : forall P : Nat -> SProp, P zero ->
  (forall pred, P pred -> P (succ pred)) -> P n.
  intros P p_z p_s.
  unfold zero, succ in *.
  destruct n as [n_l n_r].
  refine (weak_ind_nat n_r (fun x_l => _) _ _ n_r).
  intros x_r; exact p_z.
  intros c_pred H x_r.
  refine (p_s {| l := c_pred; r := _ |} _).
  apply H.
  Unshelve.
  unfold I_Nat, Mu_Nat in x_r.
  epose (weak_ind_nat _).

  epose (weak_ind_nat).
  epose H.
  simpl y.
  clear n_l n_r; intros x_l w x_r.
  refine (w (fun x_l => forall x_r : I_Nat x_l, _) _ _ x_r).
  intros x_r2; intros P p_z p_s; exact p_z.
  intros pred_l x pred_r P p_z p_s.
  refine (p_s {| l := pred_l; r := _ |} _).
  simpl in p.
  simpl in p.
  unfold I_Nat, I_Nat_G, Mu_Nat in n_r .
  refine (n_r _).
  
  refine (n_r (fun x_l => forall x_r : I_Nat x_l, _) _ _ n_r).
  intros c_l _ _ x_r. ; exact p_z.
  intros c_pred H x_r.
  refine (p_s _ (H _)); clear n_l n_r P p_z p_s H.
  intros P p_z p_s.
  enough (forall A : Prop, A -> A -> A) as id.
  epose (x_r (fun x_l => x_l _ _ (fun c_pred => P c_pred ))).
  unfold I_Nat in *.
  intros P p_z p_s.
  
  
  apply x_r.
  epose (p_s ({| l := _; r := x_r |})).
  simpl in p.

  simpl in y.
  assert (forall K : SProp,
    (seq c_zero n_l -> K) ->
    (forall c_pred, seq (c_succ c_pred) n_l -> K) -> K
  ).
  intros K; apply n_r.
  intros left right; exact (left (srefl _)).
  intros c_pred left right eq.
  epose ().
  epose (right).

  apply H; clear H.
  intros eq; destruct eq.
  exact p_z.
  intros c_pred eq.
  epose (n_r _ p_z).
  apply (b_r (fun x_l => forall K : SProp,
    (seq c_true x_l -> K) -> (seq c_false x_l -> K) -> K)).
  intros K left right; exact (left (srefl _)).
  intros K left right; exact (right (srefl _)).
Qed.


Definition C_Bool : Type := forall A, A -> A -> A.
Definition c_true : C_Bool := fun A x y => x.
Definition c_false : C_Bool := fun A x y => y.

Definition I_Bool (c_b : C_Bool) : SProp :=
  forall P : C_Bool -> SProp, P c_true -> P c_false -> P c_b.
Definition i_true : I_Bool c_true := fun A x y => x.
Definition i_false : I_Bool c_false := fun A x y => y.

Definition Bool : Type := ssig C_Bool I_Bool.
Definition true : Bool := {| l := c_true; r := i_true |}.
Definition false : Bool := {| l := c_false; r := i_false |}.

(* TODO: this is a lazy proof *)
Theorem ind_bool b : forall P : Bool -> SProp, P true -> P false -> P b.
  
  intros P p_t p_f.
  unfold true, false in *.
  destruct b as [b_l b_r].
  enough (forall K : SProp,
    (seq c_true b_l -> K) ->
    (seq c_false b_l -> K) -> K
  ).
  apply H; clear H.
  intros eq; destruct eq.
  exact p_t.
  intros eq; destruct eq.
  exact p_f.
  apply (b_r (fun x_l => forall K : SProp,
    (seq c_true x_l -> K) -> (seq c_false x_l -> K) -> K)).
  intros K left right; exact (left (srefl _)).
  intros K left right; exact (right (srefl _)).
Qed.

Definition Mu G := forall X, (G X -> X) -> X.

Definition Bool : Prop := forall A : Prop, A -> A -> A.
Definition true : Bool := fun A x y => x.
Definition false : Bool := fun A x y => y.

Axiom ind_prop :
  forall (P : Bool -> Prop),
  P true -> P false -> forall b, P b.

Axiom ind_type :
  forall (P : Bool -> Type),
  P true -> P false -> forall b, P b.



Definition K_Bool := Prop -> Prop -> Prop.
Definition k_true : K_Bool := fun x y => x.
Definition k_false : K_Bool := fun x y => y.


Definition Bool := {k_b & V_Bool k_b}.
Definition true : Bool := existT _ _ v_true.
Definition false : Bool := existT _ _ v_false.

Inductive Color :=
  | Red
  | Green
  | Blue.

Theorem dup_eq {A} (x y : A) (eq : x = y) : ((x = y) * (x = y)).
  rewrite eq.
  exact (eq_refl, eq_refl).
Qed.

Definition Secondary : Type := (Color * Color).
Definition Valid (b : Secondary) : Type := fst b <> snd b.
Definition f (both : Secondary | Valid both) : option nat.
  exact (
    match color with
    | Red => None
    | Green => Some 1
    | Blue => Some 2
    end
  ).
Qed.

Require Import Coq.Logic.Eqdep_dec.


Definition K_Bool := Prop -> Prop -> Prop.
Definition k_true : K_Bool := fun x y => x.
Definition k_false : K_Bool := fun x y => y.

Definition V_Bool k_b : Prop :=
    forall P : K_Bool -> Prop, P k_true -> P k_false -> P k_b.
Definition v_true : V_Bool k_true := fun A x y => x.
Definition v_false : V_Bool k_false := fun A x y => y.

Definition Bool := {k_b & V_Bool k_b}.
Definition true : Bool := existT _ _ v_true.
Definition false : Bool := existT _ _ v_false.

Axiom ind_bool :
  forall (P : Bool -> Type),
  P true -> P false ->
  forall b, P b.

Theorem true_neq_false (eq : true = false) : False.
  exact (
    match eq in (_ = b)
      return let (k_b, _) := b in k_b True False
    with
    | eq_refl => I
    end
  ).
Qed.

Theorem bool_uip (b : Bool) (eq : b = b) : eq = eq_refl.
  induction b using ind_bool.
  refine (
    match eq in (_ = b)
      return 
    with | eq_refl => _ end).
  simpl.
  apply b_1.
  refine (existT _ true_0 eq_refl).
  refine (existT _ false_0 eq_refl).
Qed.

Definition Bool_0 := forall A, A -> A -> A.
Definition true_0 : Bool_0 := fun A x y => x.
Definition false_0 : Bool_0 := fun A x y => y.

Definition Bool_1 b_0 := forall P, P true_0 -> P false_0 -> P b_0.
Definition true_1 : Bool_1 true_0 := fun P x y => x.
Definition false_1 : Bool_1 false_0 := fun P x y => y.

Theorem b_0_is_erasable {b_0} (b_1 : Bool_1 b_0) : {b_0' & b_0 = b_0'}.
  apply b_1.
  refine (existT _ true_0 eq_refl).
  refine (existT _ false_0 eq_refl).
Qed.

Set Universe Polymorphism.

Definition Weird : Prop := forall A : Type, True.
Inductive SFalse : SProp :=.
Inductive SBool : SProp := | sTrue | sFalse.
Inductive SEq {A : SProp} (x : A) : A -> SProp := seq_refl : SEq x x.

Definition id {A} (x : A) := x.
Definition x : Prop := @id Prop True.

Theorem weird : SEq sTrue sFalse.
  exact (seq_refl _).
Qed.

Theorem uip_nat (n : nat) (eq : n = n) : eq = eq_refl.
  apply eq_proofs_unicity.
  clear eq n; intros x.
  induction x.
  -
    intros y; induction y.
    auto.
    auto.
  -
    intros y; induction y.
    auto.
    destruct IHy.
    destruct H; auto.
    destruct (IHx y).
    destruct H0.
    auto.
    auto.
Qed.

Check eq_proofs_unicity.
Theorem uip_bool (b : bool) (eq : b = b) : eq = eq_refl.
  destruct b.
  refine (match eq with | eq_refl => _ end).
  reflexivity.
  refine (match eq with | eq_refl => _ end).
  reflexivity.
Qed.
Print uip_bool.
Theorem uip_nat (n : nat) (eq : n = n) : eq = eq_refl.
  generalize dependent n.
  induction n.
  -
    intros eq.
    refine (match eq with | eq_refl => _ end).
    reflexivity.
  -
    intros eq.
    inversion eq.

  intros x.
  destruct (f x).
  reflexivity.
Qed.
Definition add a b := a + b.
Definition x : 1 = add 0 1 := eq_refl.
Definition y : 3 = 1 + 2 := eq_refl.
Definition f : forall n, n = 0 + n := fun n => eq_refl.
Definition g : forall n, n = n + 0.
  intros n.
  induction n.
  reflexivity.
  simpl.
  rewrite <- IHn.
  reflexivity.
Qed.

Definition x := add 1 2.
Print x.

Theorem add_comm n m : n + m = m + n.
Proof.
  induction n.
  -
    induction m.
    reflexivity.
    simpl; rewrite <- IHm.
    reflexivity.
  -
    simpl; rewrite IHn; clear IHn.
    induction m.
    reflexivity.
    simpl; rewrite <- IHm.
    reflexivity.
Qed.

Axiom ind_type :
  forall (A : Type),
  forall (P : Type -> Type),
  P Type ->
  (forall A B (A_ind : P A)
    (B_ind : forall x, P (B x)),
    P (forall x : A, B x)) ->
  P A.

Axiom ind_type_when_type :
  forall P p_type p_forall,
  ind_type Type P p_type p_forall = p_type.
Axiom ind_type_when_forall :
  forall P p_type p_forall A B p_A p_B,
  ind_type (forall x : A, B x) P p_type p_forall
    = p_forall A B p_A p_B.

Lemma false_is_not_type : Type = (forall A, A) -> False.
  intros eq.
  epose (eq_rect Type
    (fun T => ind_type T (fun _ => Type) True
      (fun _ _ _ _ => False))).
  simpl in i.
  rewrite ind_type_when_type in i.
  epose (i I (forall A, A)).
  rewrite ind_type_when_forall in i0.
  exact (i0 eq).
  exact nat.
  intros x.
  exact nat.
Qed.


Axiom ind_term :
  forall {A} (t : A),
  forall (P : forall {A}, A -> Type),
  P Type ->
  (forall A B (A_ind : P A)
    (B_ind : forall x, P x -> P (B x)),
    P (forall x : A, B x)) ->
  (forall A B M (A_ind : P A)
    (B_ind : forall x, P x -> P (B x))
    (M_ind : forall x, P x -> P (M x)),
    P (fun x : A => (M x : B x))) ->
  (forall A B M N (A_ind : P A)
    (B_ind : forall x, P x -> P (B x))
    (M_ind : P M) (N_ind : P N),
    P ((M : forall x : A, B x) (N : A))) ->
  P t.


Definition Unit := forall A, A -> A.
Lemma not_id : Unit.
Proof.
  intros T x.
  induction x using ind_term.
  exact nat.
  exact nat.
  exact M.
  exact (M N).
Qed.

Definition Eq {A} (x y : A) :=
  forall P : A -> Type, P x -> P y.
Definition refl {A} (x : A) : Eq x x := fun P p_x => p_x.

Definition id := fun (x : nat) => x.

Definition Heq {A B} (x : A) (y : B) :=
  forall P : forall A, A -> Type, P A x -> P B y.

(* Lemma heq_to_eq_P {A} (x : A) : Type.
  refine (forall y, Heq x y -> Eq x y).
  assert 
  refine (ind_term A ). *)
  
Lemma heq_to_eq (x : Type) : Heq Type x -> Eq Type x.
  intros heq P p_type.
  apply (ind_term p_type (fun A _ =>
    forall P : A -> Type, P y)).
  revert y.
  apply (ind_term x (fun A x =>
    forall y : A, Heq x y -> Eq x y)).
  -
    intros y Heq.
    apply (ind_term y (fun A x =>
      forall y : A, Heq x y -> Eq x y)).
    intros P p_x.

Lemma funext (id' : nat -> nat)
  : (forall x, id x = id' x) -> id = id'.
  intros eq.
  unfold id.
  epose (ind_term id' (fun T id' =>
    forall 
    _)).


Lemma funext {A B} (f g : A -> B)
  : (forall x, Eq (f x) (g x)) -> Eq f g.
  revert B f g.
  induction A using ind_term.
  intros eq.
  unfold Eq.
  intros P.
  epose (ind_term P (fun T P =>
    forall eq : T = (nat -> nat) -> Type,
      (match eq ) f -> )).
  epose (eq_rect f (fun f' => P f') _ g).
  
Axiom ind_term_when_type :
  forall P p_type p_forall p_lambda p_apply,
  ind_term Type P p_type p_forall p_lambda p_apply = p_type.
Axiom ind_term_when_forall :
  forall P p_type p_forall p_lambda p_apply A B p_A p_B,
  ind_term (forall x : A, B x) P p_type p_forall p_lambda p_apply
    = p_forall A B p_A p_B.

(* Lemma type_is_not_forall {A B} : Type = (forall x : A, B x) -> False.
  intros eq.
  epose (inv_0 := eq_rect Type
    (fun t =>
      ind_term t (fun _ _ => Type)
        True
        (fun _ _ _ _ => False)
        (fun _ _ _ _ _ _ => False)
        (fun _ _ _ _ _ _ _ _ => False)
        )).
  rewrite ind_term_when_type in inv_0.
  epose (inv_1 := inv_0 I _ eq).
  rewrite ind_term_when_forall in inv_1.
  exact inv_1.
  (* TODO: makes no sense *)
  exact A.
  intros x l.
  exact l.
Qed. *)

Lemma type_is_not_forall {A B} : Type = (forall x : A, B x) -> False.
Admitted.

Definition Eq {A} (x y : A) :=
  forall P : A -> Type, P x -> P y.
Definition refl {A} (x : A) : Eq x x := fun P p_x => p_x.

(* Axiom K *)

(* Lemma uip {A} (x : A) : forall eq : Eq x x, Eq (refl x) eq.
  intros eq.
  intros P p_refl.
  epose (ind_term u (fun A u =>
    forall eq : Unit = A, (match eq return A -> Type with | eq_refl => P end) u)). *)


(* 
Lemma dumb (id : Unit)
  : (fun A x => id A x) = id.

Lemma funext {A B} (f g : A -> B)
  : (forall x : A, f x = g x) -> f = g. *)


  
Lemma uniq_unit (a b : Unit) : forall A x, Eq (a A x) (b A x).
Proof.
  intros A x.
  epose (ind_term (P (b A x)) (fun A r =>
    A = Type -> forall l, forall P, P l -> P r) _ _ _ _ eq_refl (P (a A x))).
  
  

Lemma ind_unit (u : Unit)
  : forall P : Unit -> Type, P (fun A x => x) -> P u.
Proof.
  refine (ind_term u (fun A u =>
    A = Unit -> forall unit, Eq unit u) _ _ _ _ eq_refl _).
  -
    clear u; intros eq unit; destruct (type_is_not_forall eq).
  -
    clear u; intros A B IHA IHB eq; destruct (type_is_not_forall eq).
  -
    clear u; intros A.
    enough (A = Type).
    rewrite H in *; clear H A.
    intros B.
    enough (B = fun A => A -> A).
    rewrite H in *; clear H B.
    intros M _ _ _ _ unit.
    induction A using
    intros A B M.
    epose (ind_term A (fun A_T A =>
      A = Type -> _) _ _ _ _ eq_refl _).
    induction A using ind_term.


  
  simpl in p.
  intros P p_unit.
  fold 
  induction u using ind_term.
  revert A u HeqA.
  apply (ind_term (fun A u =>
    A = (forall A : Type, A -> A) -> forall id, u = id)).
  -
    intros HeqA id; destruct (type_is_not_forall HeqA).
  -
    intros A B IHA IHB HeqA; destruct (type_is_not_forall HeqA).
  -
    intros A B M IHA IHB IHM HeqA id.
    enough (A = Type).
    assert (B = fun x => x -> x).
  
  induction u using ((fun A u =>
  forall id, A = (forall A : Type, A -> A) -> u = id)).
  -
    intros id.
Qed.


Require Import String.
Open Scope string_scope.

Require Import List.
Import ListNotations.

Print List.map.

Set Universe Polymorphism.

Definition Bool_0 := forall A, A -> A.
Definition true_0 : Bool_0 := fun A x => x.
Definition false_0 : Bool_0 := fun A x => x.

Definition Bool_1 b_0 := forall P, P true_0 -> P false_0 -> P b_0.
Definition true_1 : Bool_1 true_0 := fun A x y => x.
Definition false_1 : Bool_1 false_0 := fun A x y => y.

Definition Bool_2 {b_0} b_1 :=
  forall P : forall b_0, Bool_1 b_0 -> Type, P true_0 true_1 -> P false_0 false_1 -> P b_0 b_1.
Definition true_2 : Bool_2 true_1 := fun A x y => x.
Definition false_2 : Bool_2 false_1 := fun A x y => y.


Lemma true_neq_false : true_1 <> false_1.
Proof.
  intros eq.
  exact (eq_rect true_1
    (fun b_1 => b_1 (fun _ => Type) True False)
      I false_1 eq
    ).
Qed.


Inductive Ind_0 : Type :=
  | Type_0 : Ind_0
  | Arrow_0 (A_ind : Ind_0) (B_ind : Ind_0) : Ind_0
  | Forall_0 {A}
    (A_ind : Ind_0)
    (B_ind : A -> Ind_0)
    : Ind_0
  | Lambda_0 {A}
    (A_ind : Ind_0)
    (B_ind : A -> Ind_0)
    (M_ind : A -> Ind_0)
    : Ind_0.

Inductive Ind_1 : forall A, A -> Type :=
  | Type_1 : Ind_1 Type Type
  | Arrow_1 {A B}
    (A_ind : Ind_1 Type A)
    (B_ind : Ind_1 Type B)
    : Ind_1 Type (A -> B)
  | Forall_1 {A B}
    (A_ind : Ind_1 Type A)
    (B_ind : forall x : A, Ind_0 -> Ind_0)
    : Ind_1 Type (forall x : A, B x)
  | Lambda_1 {A B M}
    (A_ind : Ind_1 Type A)
    (B_ind : forall x : A, Ind_0 -> Ind_0)
    (M_ind : forall x : A, Ind_0 -> Ind_0)
    : Ind_1 (forall x : A, B x) (fun x => M x).
Inductive Ind_2 : forall A x, Ind_1 A x -> Type :=
  | Type_2 : Ind_2 Type Type Type_1
  | Arrow_2 {A B}
    (A_ind : Ind_1 Type A)
    (B_ind : Ind_1 Type B)
    : Ind_2 Type (A -> B) (Arrow_1 A_ind B_ind)
  | Forall_2 {A A_ind_1 B B_ind_0}
    (A_ind : Ind_2 Type A A_ind_1)
    (B_ind : forall x : A, Ind_1 A x -> Ind_1 Type (B x))
    : Ind_2 Type (forall x : A, B x) (Forall_1 A_ind_1 B_ind_0)
  | Lambda_2 {A A_ind_1 B B_ind_0 M M_ind_0}
    (A_ind : Ind_2 Type A A_ind_1)
    (B_ind : forall x : A, Ind_1 A x -> Ind_1 Type (B x))
    (M_ind : forall x : A, Ind_1 A x -> Ind_1 (B x) (M x))
    : Ind_2 (forall x : A, B x) (fun x => M x)
      (Lambda_1 A_ind_1 B_ind_0 M_ind_0).

Lemma ind_1_term {A} (t : A) (ind : Ind_1 A t)
  : forall (P : forall A, A -> Type),
    P Type Type ->
    (forall A B,
      P Type A -> P Type B ->
      P Type (A -> B)) ->
    (forall A B
      (A_ind : P Type A)
      (B_ind : forall x : A, Ind_0 -> Ind_0),
      P Type (forall x : A, B x)) ->
    (forall A B M
      (A_ind : P Type A)
      (B_ind : forall x : A, Ind_0 -> Ind_0)
      (M_ind : forall x : A, Ind_0 -> Ind_0),
      P (forall x : A, B x) (fun x => M x)) ->
    P A t.
  induction ind.
  auto.
  exact (
    fun P p_type p_arrow p_forall p_lambda =>
      p_arrow A B
        (IHind1 P p_type p_arrow p_forall p_lambda)
        (IHind2 P p_type p_arrow p_forall p_lambda)).
  exact (
    fun P p_type p_arrow p_forall p_lambda =>
      p_forall A B
        (IHind P p_type p_arrow p_forall p_lambda)
        (fun x x_ind_0 => B_ind x x_ind_0)).
  exact (
    fun P p_type p_arrow p_forall p_lambda =>
      p_lambda A B M
        (IHind P p_type p_arrow p_forall p_lambda)
        (fun x x_ind_0 => B_ind x x_ind_0)
        (fun x x_ind_0 => M_ind x x_ind_0)).
Qed.
Lemma ind_2_term {A} (t : A) {ind_1} (ind : Ind_2 A t ind_1)
  : forall (P : forall A, A -> Type),
    P Type Type ->
    (forall A B,
      P Type A -> P Type B ->
      P Type (A -> B)) ->
    (forall A B
      (A_ind : P Type A)
      (B_ind : forall x, Ind_1 A x -> P Type (B x)),
      P Type (forall x : A, B x)) ->
    (forall A B M
      (A_ind : P Type A)
      (B_ind : forall x, Ind_1 A x -> P Type (B x))
      (M_ind : forall x, Ind_1 A x -> P (B x) (M x)),
      P (forall x : A, B x) (fun x => M x)) ->
    P A t.
  induction ind.
  auto.
  refine (
    fun P p_type p_arrow p_forall p_lambda =>
      p_arrow A B
        _
        _).
  apply (ind_1_term _ A_ind P p_type p_arrow).

  refine (
    fun P p_type p_arrow p_forall =>
      p_arrow A B _ _).
  induction A_ind.
      
  exact (
    fun P p_type p_arrow p_forall =>
      p_arrow A B
        (A_ind P p_type p_arrow p_forall)
        (B_ind P p_type p_arrow p_forall)).
  refine (
    fun P p_type p_arrow p_forall p_lambda =>
      p_forall A B
        (IHind P p_type p_arrow p_forall p_lambda)
        (fun x x_ind_0 => _)).
    apply
  exact (
    fun P p_type p_arrow p_forall p_lambda =>
      p_lambda A B M
        (IHind P p_type p_arrow p_forall p_lambda)
        (fun x x_ind_0 => ind_0_term _ (B_ind x x_ind_0) P p_type p_arrow)
        (fun x x_ind_0 => ind_0_term _ (M_ind x x_ind_0) P p_type p_arrow)).
Qed.
Definition ind_2_id {id} 
  : Ind_2 (forall (A : Type), A -> A) id -> id = (fun A x => x).
  intros id_ind.
  induction id using id_ind.
  apply Forall_2.
  apply Type_2.
  intros A A_ind.
  apply Arrow_1.
  exact A_ind.
  exact A_ind.
Qed.

Definition ind_2_id : Ind_2 Type (forall (A : Type), A -> A).
  apply Forall_2.
  apply Type_2.
  intros A A_ind.
  apply Arrow_1.
  exact A_ind.
  exact A_ind.
Qed.

Definition C_nat := forall A, A -> (A -> A) -> A.
Definition C_zero : C_nat := fun A z s => z.
Definition C_succ (n : C_nat) : C_nat := fun A z s => s (n A z s).

Definition Ind_B {A} (t : A) :=
  forall P : forall {A}, A -> Type,
    P Type -> P t.
Definition ind_b_type : Ind_B Type :=
  fun P p_type => p_type.
Definition Ind_0 {A} (t : A) :=
  forall P : forall {A}, A -> Type,
    P Type ->
    (forall A B, P A ->
      (forall x, Ind_B x -> P (B x)) ->
      P (forall (x : A), B x)) ->
    (forall A B M, P A ->
      (forall x, Ind_B x -> P (B x)) ->
      (forall x, Ind_B x -> P (M x)) ->
      P (fun (x : A) => (M x : B x))) ->
    P t.
Definition ind_0_type : Ind_0 Type :=
  fun P p_type p_forall p_lambda => p_type.
Definition ind_0_forall {A} (A_ind : Ind_0 A)
  {B} (B_ind : forall x, Ind_B x -> Ind_0 (B x))
  : Ind_0 (forall x : A, B x) :=
  fun P p_type p_forall p_lambda =>
    p_forall A B
      (A_ind P p_type p_forall p_lambda)
      (fun x x_ind => B_ind x x_ind P p_type p_forall p_lambda).
Definition ind_0_lambda {A} (A_ind : Ind_0 A)
  {B} (B_ind : forall x, Ind_B x -> Ind_0 (B x))
  {M} (M_ind : forall x, Ind_B x -> Ind_0 (M x))
  : Ind_0 (fun (x : A) => (M x : B x)) :=
  fun P p_type p_forall p_lambda =>
    p_lambda A B M
      (A_ind P p_type p_forall p_lambda)
      (fun x x_ind => B_ind x x_ind P p_type p_forall p_lambda)
      (fun x x_ind => M_ind x x_ind P p_type p_forall p_lambda).

Definition Ind_1 {A} (t : A) (ind : Ind_0 t) :=
  forall P : forall {A} (t : A), Ind_0 t -> Type,
    P Type ind_0_type ->
    (forall A A_ind B B_ind, P A A_ind -> 
      (forall x x_ind_b, Ind_0 x -> P (B x) (B_ind x x_ind_b)) ->
      P (forall x, B x) (ind_0_forall A_ind B_ind)) ->
    (forall A A_ind B B_ind M M_ind, P A A_ind ->
      (forall x x_ind_b, Ind_0 x -> P (B x) (B_ind x x_ind_b)) ->
      (forall x x_ind_b, Ind_0 x -> P (M x) (M_ind x x_ind_b)) ->
      P (fun (x : A) => (M x : B x)) (ind_0_lambda A_ind B_ind M_ind)) ->
    P t ind.
Definition ind_1_type : Ind_1 Type ind_0_type :=
  fun P p_type p_forall p_lambda => p_type.
Definition ind_1_forall {A} {A_ind_0} (A_ind : Ind_1 A A_ind_0)
  {B} {B_ind_0} (B_ind : forall x x_ind_b x_ind, Ind_1 (B x) (B_ind_0 x x_ind_b))
  : Ind_1 (forall x : A, B x) (ind_0_forall A_ind_0 B_ind_0) :=
  fun P p_type p_forall p_lambda =>
    p_forall A A_ind_0 B B_ind_0
      (A_ind P p_type p_forall p_lambda)
      (fun x x_ind_b x_ind => B_ind x x_ind_b x_ind P p_type p_forall p_lambda).
Definition ind_1_lambda {A} {A_ind_0} (A_ind : Ind_1 A A_ind_0)
  {B} {B_ind_0} (B_ind : forall x x_ind_b x_ind, Ind_1 (B x) (B_ind_0 x x_ind_b))
  {M} {M_ind_0} (M_ind : forall x x_ind_b x_ind, Ind_1 (M x) (M_ind_0 x x_ind_b))
  : Ind_1 (fun (x : A) => (M x : B x)) (ind_0_lambda A_ind_0 B_ind_0 M_ind_0) :=
  fun P p_type p_forall p_lambda =>
    p_lambda A A_ind_0 B B_ind_0 M M_ind_0
      (A_ind P p_type p_forall p_lambda)
      (fun x x_ind_b x_ind => B_ind x x_ind_b x_ind P p_type p_forall p_lambda)
      (fun x x_ind_b x_ind => M_ind x x_ind_b x_ind P p_type p_forall p_lambda).

Definition ind_0_false : Ind_0 (forall (A : Type), A) :=
  ind_0_forall ind_0_type
    (fun x x_ind => x_ind (fun A x => Ind_0 x) ind_0_type).


Definition ind_0_id : Ind_0 (forall (A : Type), A -> A).
  apply (ind_0_forall ind_0_type).
  intros x x_ind_b.
  apply (ind_0_forall).
  
  ind_0_forall ind_0_type
    (fun x x_ind => x_ind (fun A x => Ind_0 x) ind_0_type).
    

Lemma ind_1_false : Ind_1 (forall (A : Type), A) ind_0_false.
  refine (ind_1_forall ind_1_type (fun x x_ind_b x_ind => _)).
  epose (x_ind (fun A x x_ind => Ind_1 x x_ind)).
  -
    clear x_ind_b x_ind x.
    unfold Ind_1; intros x_ind P p_type p_forall p_lambda.

  epose (x_ind _ x).
  
  exact ind_0_type.
  intros x x_ind.
  unfold Ind_0.
  intros P p_type p_forall p_lambda.
  apply x_ind.
  exact p_type.
Qed.

Definition ind_1_type : Ind_1 Type ind_0_type :=
  fun P p_type p_forall p_lambda => p_type.
Definition ind_1_forall {A} {A_ind_0} (A_ind : Ind_1 A A_ind_0)
  {B} {B_ind_0} (B_ind : forall x, Ind_0 x -> Ind_1 (B x) (B_ind_0 x))
  : Ind_1 (forall x : A, B x) (ind_0_forall A_ind_0 B_ind_0) :=
  fun P p_type p_forall p_lambda =>
    p_forall A A_ind_0 B B_ind_0
      (A_ind P p_type p_forall p_lambda)
      (fun x x_ind => B_ind x x_ind P p_type p_forall p_lambda).
Definition ind_1_lambda {A} {A_ind_0} (A_ind : Ind_1 A A_ind_0)
  {B} {B_ind_0} (B_ind : forall x, Ind_0 x -> Ind_1 (B x) (B_ind_0 x))
  {M} {M_ind_0} (M_ind : forall x, Ind_0 x -> Ind_1 (M x) (M_ind_0 x))
  : Ind_1 (fun (x : A) => (M x : B x)) (ind_0_lambda A_ind_0 B_ind_0 M_ind_0) :=
  fun P p_type p_forall p_lambda =>
    p_lambda A A_ind_0 B B_ind_0 M M_ind_0
      (A_ind P p_type p_forall p_lambda)
      (fun x x_ind => B_ind x x_ind P p_type p_forall p_lambda)
      (fun x x_ind => M_ind x x_ind P p_type p_forall p_lambda).

Lemma ind_0_false : Ind_0 (forall (A : Type), A).
Qed.
  apply ind_0_forall.
  apply ind_0_type.
  intros; unfold Ind_0.
  intros P p_type p_forall p_lambda.
  
  unfold Dyn_1; intros P p_type p_forall p_lambda.
  apply p_forall.
  exact p_type.
  intros x p_x.
  apply p_x.
  -
    exact p_type.
  -
    intros A B p_a p_b.
    apply p_forall.
    exact p_a.
    intros x0 dyn.
    exact p_b.
  -
    intros A B M p_a p_m.
    apply p_lambda.
    exact p_a.
    intros x0 dyn.
    exact p_m.
Qed.

Lemma ind_type (t : Type) (ind : Dyn_1 t)
  : forall P : forall {A}, A -> Type,
      P Type ->
      (forall A B, P A -> 
        (forall x, Dyn_0 x -> P (B x)) ->
        P (forall (x : A), B x)) ->
      P t.
  intros P p_type p_forall.
  apply ind; clear ind.
  -
    exact p_type.
  -
    intros A B p_of_a p_of_b.
    apply p_forall.
    exact p_of_a.
    intros x dyn.
    apply p_of_b.
    exact dyn.
  -
    clear p_type p_forall.
    intros A B M p_of_a p_of_m.
    apply 

Lemma ind_false : Dyn_1 (forall (A : Type), A).
  unfold Dyn_1; intros P p_type p_forall p_lambda.
  apply p_forall.
  exact p_type.
  intros x p_x.
  apply p_x.
  exact p_type.
  intros A B p_of_a p_of_b.
  apply p_forall.
  exact p_of_a.
  intros x0 dyn.
  exact p_of_b.
  intros A B M p_of_a p_of_m.
  apply p_forall.
  
Qed.

Lemma ind_type : Dyn_1 (forall (A : Type), A).
  unfold Dyn_1; intros P p_type p_forall.
  apply p_forall.
  exact p_type.
  intros x p_x.
  apply p_x.
  exact p_type.
  intros A B p_of_a p_of_b.
  apply p_forall.
  exact p_of_a.
  intros x0 dyn.
  exact p_of_b.
Qed.

Lemma ind_id : Dyn_1 (forall (A : Type), A -> A).
  unfold Dyn_1; intros P p_type p_forall.
  apply p_forall.
  exact p_type.
  intros x p_x.
  epose (p_forall)
  apply p_forall.
  apply p_forall.
  apply p_forall.
  intros x p_x.
  exact p_x.
Qed.
  exact Univ.
  intros x.
  intros P p_type p_forall.
  apply p_forall.
  exact p_type.
  intros x.
Qed.

Definition Dyn (l : C_nat) {A} (x : A) : Type :=
  l _


Notation Ind T :=
  (
    forall P : Type -> Type,
      P Type ->
      (forall A B, P A ->
        (forall x, P (B x)) ->
        P (forall (x : A), B x)) ->
      P T
  ).
Definition IType := {T & Ind T}.

Definition T (x : IType) : Type :=
  let (raw, _) := x in
  raw.


Inductive Dyn : forall A, A -> Type :=
  | Univ : Dyn Type Type
  | Forall : forall A B, Dyn Type A ->
    (forall x, Dyn A x -> Dyn Type (B x)) ->
    Dyn Type (forall x, B x).
Lemma ind_false : Dyn (forall (A : Type), A).
  apply Forall.
  exact Univ.
  intros x.
  intros P p_type p_forall.
  apply p_forall.
  exact p_type.
  intros x.
Qed.
Lemma ind_id : Ind (forall A, A -> A).
  intros P p_type p_forall.
  apply p_forall.
  exact p_type.
Qed.

Lemma ind_id : Ind (Type -> Type).
  intros P p_type p_forall.
  apply p_forall.
  exact p_type.
  exact p_type.
Qed.
Lemma ind_apply : Ind ((Type -> Type) -> Type -> Type).
  intros P p_type p_forall.
  apply p_forall.
  apply (ind_id _ p_type p_forall).
  apply (ind_id _ p_type p_forall).
Qed.
  intros x.
  apply p_forall.


Ind (T : Prop) =
  (P : Prop -> Type) ->
  P Type ->
  ((A : Type) -> (B : (x : A) -> Type) ->
    P A -> ((x : A) -> P (B x)) -> P ((x : A) -> B x)) ->
  P T;

Require Import String.
Open Scope string_scope.

Require Import List.
Import ListNotations.


Notation Context := (list string).

(* Inductive Term : Type :=
  | T_var (var : nat)
  | T_lam (param : string) (body : Term)
  | T_app (lam : Term) (arg : Term). *)

Definition nat_to_ascii (n : nat) : string.
  (* TODO: this is clearly cheatting *)
  exact (String.string_of_list_ascii [Ascii.ascii_of_nat n]).
Qed.

Inductive Term : nat -> Type :=
  | T_var var : Term (S var)
  (* TODO: this is just anotation *)
  | T_lam {ctx} (body : Term (S ctx)) : Term ctx
  | T_app {lam_ctx} (lam : Term lam_ctx)
      {arg_ctx} (arg : Term (lam_ctx + arg_ctx))
    : Term (lam_ctx + arg_ctx).

Fixpoint count var (ctx : Context) : nat :=  
  match ctx with
  | [] => 0
  | hd :: tl => 
    match String.eqb var hd with
    | true => 1 + count var tl
    | false => count var tl
    end
  end.

Fixpoint names {n} (term : Term n) : Context :=  
  match term with
  | T_var var => [nat_to_ascii var]
  | T_lam body => names body
  | T_app lam arg => names lam ++ names arg
  end.

Notation uniq ctx := (forall x,
  {count x ctx = 0} + {count x ctx = 1}).

Lemma count_app_comm {l r x}
  : count x (l ++ r) = count x (r ++ l).
  induction l.
  rewrite <- List.app_nil_end.
  reflexivity.
  simpl.
  rewrite IHl; clear IHl.
  induction r.
  reflexivity.
  destruct String.eqb.
  simpl in *.
  destruct String.eqb.
  rewrite IHr.
  reflexivity.
  exact IHr.
  simpl in *.
  destruct String.eqb.
  rewrite IHr.
  reflexivity.
  exact IHr.
Qed.

Lemma count_app_split {l r x}
  : count x (l ++ r) = count x l + count x r.
  induction l.
  intros; reflexivity.
  intros; simpl.
  destruct (String.eqb x a).
  rewrite IHl.
  reflexivity.
  exact IHl.
Qed.
  
Lemma all_names_are_uniq {n} (term : Term n) : uniq (names term).  
  induction term.
  -
    simpl; intros.
    destruct String.eqb.
    right; reflexivity.
    left; reflexivity.
  -
    exact IHterm.
  -
    simpl; intros.
    rename term1 into lam; rename IHterm1 into IHlam.
    rename term2 into arg; rename IHterm2 into IHarg.
    assert (count x (names lam) = 1 -> count x (names arg) = 1 -> False).
    intros; remember (lam_ctx + arg_ctx).
    induction lam.
    induction arg.
    --

      simpl in *.
      induction arg.
      ---
        inversion Heqn; rewrite H2 in *; clear var0 H2 Heqn.
        simpl in *.
        destruct (String.eqb_spec x (nat_to_ascii var)).
        rewrite e in H0.
        assert (nat_to_ascii var =? nat_to_ascii (var + arg_ctx) = false).
        induction var.
        simpl.
        simpl.
        
        
        simpl in *.
        
      destruct (IHterm1 x).

    rewrite count_app_split.
    destruct (IHterm1 x).
    --
      destruct (IHterm2 x).
      rewrite e.
      rewrite e0.
      left; reflexivity.
      rewrite e.
      rewrite e0.
      right; reflexivity.
    --
      destruct (IHterm2 x).
      rewrite e.
      rewrite e0.
      right; reflexivity.
      clear IHterm1 IHterm2.
      induction term2.
      --- 

    
       
    

Qed.

Inductive XTerm : Context -> Type :=
  | X_var var : XTerm [var]
  | X_lam {ctx} param (body : XTerm (param :: ctx)) : XTerm ctx
  | X_app {lam_ctx} (lam : XTerm lam_ctx) {arg_ctx} (arg : XTerm arg_ctx)
    (no_inter : forall x, count x lam_ctx = 1 -> count x arg_ctx = 0)
    : XTerm (lam_ctx ++ arg_ctx).




Lemma all_names_are_uniq {names n} (term : Term names n) : uniq names.
  revert n term.
  induction names.
  -
    intros; left; reflexivity.
  - 
    intros; simpl.
    
    dependent induction term.
    --
      destruct (String.eqb x a).
      right; reflexivity.
      left; reflexivity.
  -
    intros x; simpl in *.
    destruct (IHterm x).
    destruct (String.eqb x param).
    inversion e.
    left; exact e.
    destruct (String.eqb x param).
    inversion e.
    left; reflexivity.
    right; exact e.
  -
    rename term1 into lam; rename IHterm1 into IHlam.
    rename term2 into arg; rename IHterm2 into IHarg.
    intros x.
Admitted.

Definition to_x_term {ctx n} (term : Term ctx n) : XTerm ctx.
  eassert _ as ctx_uniq.
  exact (all_names_are_uniq term).
  induction term.
  -
    apply X_var.
  -
    assert (count param names = 0).
    --
      clear IHterm.
      induction term eqn : eq_term.
      inversion Heqn; destruct H0; clear Heqn.
      
    --
    apply (X_lam param).
    apply IHterm.
    intros; simpl.
    destruct (ctx_uniq param).
    --
      destruct (String.eqb_spec x param).
      destruct e0.
      right; rewrite e; reflexivity.
      apply ctx_uniq.
    --
      

  -
    apply (X_app).
    exact IHterm1.
    exact IHterm2.
    exact ()
    intros.

Lemma simple_exchange {ctx0 x ctx1}
  (term : Term (ctx0 ++ x :: ctx1)) : Term (x :: ctx0 ++ ctx1).
  assert (Term (x :: ctx0)) as lam.
  assert (Term (rev ctx1 ++ x :: ctx0)) as app.
  assert (Term (rev ctx1)) as app.
  assert (Term []) as lam.
  -
    (* TODO: why it is easier to assert first? *)
    assert (Term ctx1) as term'.
    induction ctx0.
    exact (T_lam x term).
    exact (IHctx0 (T_lam a term)).
    clear term; induction ctx1.
    exact term'.
    exact (IHctx1 (T_lam a term')).
  -
    (* TODO: better than epose? *)
    eassert _ as uniq_ctx.
    exact (all_ctx_are_uniq term).
    clear term; induction ctx1.
    exact lam.
    refine (T_app (IHctx1 _) (T_var a) _).
    destruct (uniq_ctx ).
  -
    clear term; induction ctx0 using rev_context_rect.
    exact (T_app app (T_var x)).
    epose (app' := T_app IHctx0 (T_var a)).
    rewrite <- app_assoc in app'.
    exact app'.
  -
    clear term; induction ctx1 using rev_context_rect.
    exact app.
    apply IHctx1.
    refine (T_lam a _).
    rewrite rev_unit in app.
    exact app.
  -
    clear term; induction ctx1 using rev_context_rect.
    rewrite <- app_nil_end.
    exact lam.
    rewrite app_assoc.
    rewrite app_comm_cons.
    exact (T_app IHctx1 (T_var a)).
Qed.


(* TODO: find this in *)
Fixpoint rev_context_rect (l : Context) :
  forall P, P [] -> (forall a tl, P tl -> P (tl ++ [a])) -> P l.
  intros P nil append.
  refine (
    match l with
    | [] => nil
    | a :: tl => _
    end
  ).
  apply (rev_context_rect tl).
  exact (append a [] nil).
  intros.
  apply (append a0 (a :: tl0)).
  exact X.
Qed.
Fixpoint count var (ctx : Context) : nat :=  
  match ctx with
  | [] => 0
  | hd :: tl => 
    match Nat.eqb var hd with
    | true => 1 + count var tl
    | false => count var tl
    end
  end.


  
Definition uniq ctx := (forall x,
  {count x ctx = 0} + {count x ctx = 1}).

Notation Context := (list nat).

Inductive Term : Context -> Type :=
  | T_var var : Term [var + 1]
  | T_lam {ctx} param (body : Term (param :: ctx)) : Term ctx
  | T_app {lam_ctx} (lam : Term lam_ctx) {arg_ctx} (arg : Term arg_ctx)
    (no_inter : forall x, count x lam_ctx = 1 -> count x arg_ctx = 0)
    : Term (lam_ctx ++ arg_ctx).

(* Inductive Term : Context -> Type :=
  | T_var var : Term [var]
  | T_lam {ctx} param (body : Term (param :: ctx)) : Term ctx
  | T_app {lam_ctx} (lam : Term lam_ctx) {arg_ctx} (arg : Term arg_ctx)
    (no_inter : forall x, count x lam_ctx = 1 -> count x arg_ctx = 0)
    : Term (lam_ctx ++ arg_ctx). *)

Lemma new_var ctx : {var & count var ctx = 0}.
Admitted.

Lemma all_ctx_are_uniq {ctx} (term : Term ctx) : uniq ctx.
  unfold uniq.
  induction term.
  -
    intros; simpl.
    destruct (String.eqb x var).
    exact (right eq_refl).
    exact (left eq_refl).
  -
    intros; simpl in *.
    destruct (IHterm x).
    destruct (String.eqb x param).
    inversion e.
    left; exact e.
    destruct (String.eqb x param).
    inversion e.
    left; reflexivity.
    right; exact e.
  -
    intros.
    induction lam_ctx.
    --
      apply IHterm2.
    --
      simpl in *; destruct (String.eqb_spec x a).
      destruct e.
      

    --
    exact uniq0.
Qed.

Lemma simple_exchange {ctx0 x ctx1}
  (term : Term (ctx0 ++ x :: ctx1)) : Term (x :: ctx0 ++ ctx1).
  assert (Term (x :: ctx0)) as lam.
  assert (Term (rev ctx1 ++ x :: ctx0)) as app.
  assert (Term (rev ctx1)) as app.
  assert (Term []) as lam.
  -
    (* TODO: why it is easier to assert first? *)
    assert (Term ctx1) as term'.
    induction ctx0.
    exact (T_lam x term).
    exact (IHctx0 (T_lam a term)).
    clear term; induction ctx1.
    exact term'.
    exact (IHctx1 (T_lam a term')).
  -
    (* TODO: better than epose? *)
    eassert _ as uniq_ctx.
    exact (all_ctx_are_uniq term).
    clear term; induction ctx1.
    exact lam.
    refine (T_app (IHctx1 _) (T_var a) _).
    destruct (uniq_ctx ).
  -
    clear term; induction ctx0 using rev_context_rect.
    exact (T_app app (T_var x)).
    epose (app' := T_app IHctx0 (T_var a)).
    rewrite <- app_assoc in app'.
    exact app'.
  -
    clear term; induction ctx1 using rev_context_rect.
    exact app.
    apply IHctx1.
    refine (T_lam a _).
    rewrite rev_unit in app.
    exact app.
  -
    clear term; induction ctx1 using rev_context_rect.
    rewrite <- app_nil_end.
    exact lam.
    rewrite app_assoc.
    rewrite app_comm_cons.
    exact (T_app IHctx1 (T_var a)).
Qed.

Lemma exchange {ctx0 x ctx1 y ctx2}
  : Term (ctx0 ++ x :: ctx1 ++ y :: ctx2) -> Term (x :: ctx0 ++ ctx1)
Lemma exchange {ctx0 x ctx1 y ctx2} :
  Term (append ctx0 (x :: (append ctx1 (y :: ctx2)))) ->
  Term (append ctx0 (y :: (append ctx1 (x :: ctx2)))).
  intros term.
  induction ctx0.
  induction ctx1.
  induction ctx2.
  -
    refine

  assert (Term []) as lam.
  -
    induction ctx0.
    exact (T_lam x term).
    exact (IHctx0 (T_lam a term)).
  -
    clear term.
    apply rev_context_rect.
    exact lam.
    clear x ctx0 lam; intros a ctx0 lam.
    exact (T_app lam (T_var a)).
Qed.
  unfold uniq; intros; simpl.
  destruct (String.eqb_spec x0 k).
  destruct e.
  destruct (String.eqb_spec x0 r).
  destruct e.
  simpl in k_free; rewrite (String.eqb_refl x0) in *; inversion k_free.
  right; reflexivity.
  destruct (String.eqb_spec x0 r).
  right; reflexivity.
  left; reflexivity.
  unfold uniq; intros; simpl.
  destruct (String.eqb_spec x0 k).
  destruct e.
  destruct (String.eqb_spec x0 r).
  destruct e.
  simpl in k_free; rewrite (String.eqb_refl x0) in *; inversion k_free.
  destruct (String.eqb_spec x0 l).
  destruct e.
  simpl in k_free; rewrite (String.eqb_refl x0) in *.
  destruct (String.eqb_neq x0 r).
  rewrite (H0 n) in k_free; inversion k_free.
  right; reflexivity.
  destruct (String.eqb_spec x0 r).
  destruct e.
  destruct (String.eqb_spec x0 l).
  destruct e.
  simpl in r_free; rewrite (String.eqb_refl x0) in *; inversion r_free.
  right; reflexivity.
  destruct (String.eqb_spec x0 l).
  right; reflexivity.
  left; reflexivity.
  
  induction term.
  -
    apply T_var.
  -
    refine (T_lam param _).
    apply IHterm.
    simpl; destruct (String.eqb_spec l param).


Lemma exchange {x y ctx2} : Term (x :: y :: ctx2) -> Term (y :: x :: ctx2).
  intros term.
  destruct (new_var (x :: y :: ctx2)) as [l l_free].
  destruct (new_var (l :: x :: y :: ctx2)) as [r r_free].
  destruct (new_var (r :: l :: x :: y :: ctx2)) as [k k_free].
  assert (Term []) as swap.
  refine (T_lam l (T_lam r (T_lam k (T_app (T_app (T_var k) (T_var r) _) (T_var l) _)))).
  unfold uniq; intros; simpl.
  destruct (String.eqb_spec x0 k).
  destruct e.
  destruct (String.eqb_spec x0 r).
  destruct e.
  simpl in k_free; rewrite (String.eqb_refl x0) in *; inversion k_free.
  right; reflexivity.
  destruct (String.eqb_spec x0 r).
  right; reflexivity.
  left; reflexivity.
  unfold uniq; intros; simpl.
  destruct (String.eqb_spec x0 k).
  destruct e.
  destruct (String.eqb_spec x0 r).
  destruct e.
  simpl in k_free; rewrite (String.eqb_refl x0) in *; inversion k_free.
  destruct (String.eqb_spec x0 l).
  destruct e.
  simpl in k_free; rewrite (String.eqb_refl x0) in *.
  destruct (String.eqb_neq x0 r).
  rewrite (H0 n) in k_free; inversion k_free.
  right; reflexivity.
  destruct (String.eqb_spec x0 r).
  destruct e.
  destruct (String.eqb_spec x0 l).
  destruct e.
  simpl in r_free; rewrite (String.eqb_refl x0) in *; inversion r_free.
  right; reflexivity.
  destruct (String.eqb_spec x0 l).
  right; reflexivity.
  left; reflexivity.

  induction term.
  induction ctx2.
  -
    intros.
    refine (T_app (T_app swap (T_var y) _) (T_var x) _).
    unfold uniq; intros; simpl.
    destruct (String.eqb_spec x0 y).
    right; reflexivity.
    left; reflexivity.
    unfold uniq; intros; simpl.
    destruct (String.eqb_spec x0 y).
    destruct e.
    destruct (String.eqb_spec x0 x).
    destruct e.
    destruct (all_ctx_are_uniq term x0).
    simpl in e; rewrite (String.eqb_refl x0) in *; inversion e.
    simpl in e; rewrite (String.eqb_refl x0) in *; inversion e.
    right; reflexivity.
    destruct (String.eqb_spec x0 x).
    right; reflexivity.
    left; reflexivity.
  -
    induction term.
    destruct
    epose 
Qed.



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

Definition Context : Type := list string.
Fixpoint append (l r : Context) : Context :=
  match l with
  | [] => r
  | hd :: tl => hd :: append tl r
  end.
Fixpoint count var (ctx : Context) : nat :=  
  match ctx with
  | [] => 0
  | hd :: tl => 
    match String.eqb var hd with
    | true => 1 + count var tl
    | false => count var tl
    end
  end.
Fixpoint free var (ctx : Context) :=
  match ctx with
  | [] => false
  | hd :: tl =>
    match String.eqb hd var with
    | true => true
    | false => free var tl
    end
  end.

Definition uniq ctx := (forall x,
  {count x ctx = 0} + {count x ctx = 1}).

(* Inductive System : Context -> Term -> Type :=
  | S_var var : System [var] (T_var var)
  | S_lam {lam_ctx} param
    {body_term} (body : System (param :: lam_ctx) body_term)
    : System lam_ctx (T_lam param body_term)
  | S_app {lam_ctx lam_term} (lam : System lam_ctx lam_term)
    {arg_ctx arg_term} (arg : System arg_ctx arg_term)
    (uniq : uniq (append lam_ctx arg_ctx))
    : System (append lam_ctx arg_ctx) (T_app lam_term arg_term). *)
Inductive System : Context -> Term -> Type :=
  | S_var var : System [var] (T_var var)
  | S_lam {lam_ctx} param
    {body_term} (body : System (param :: lam_ctx) body_term)
    : System lam_ctx (T_lam param body_term)
  | S_app {lam_ctx lam_term} (lam : System lam_ctx lam_term)
    {arg_ctx arg_term} (arg : System arg_ctx arg_term)
    (uniq : uniq (append lam_ctx arg_ctx))
    : System (append lam_ctx arg_ctx) (T_app lam_term arg_term).
  (* TODO: weaker version of exchange *)
  (* | L_exchange {ctx0 ctx1 ctx2 x y term}
    (linear : Linear (append ctx0 (append (x :: ctx1) (y :: ctx2))) term)
    : Linear (append ctx0 (append (y :: ctx1) (x :: ctx2))) term. *)

Lemma x_not_in_ctx0_and_ctx1 {x ctx0 ctx1}
  (x_not_in_append_ctx0_ctx1 : count x (append ctx0 ctx1) = 0)
  : count x ctx0 = 0 /\ count x ctx1 = 0.
  induction ctx0.
  -
    apply conj.
    reflexivity.
    exact x_not_in_append_ctx0_ctx1.
  -
    simpl in *; destruct (String.eqb x a).
    inversion x_not_in_append_ctx0_ctx1.
    apply IHctx0.
    exact x_not_in_append_ctx0_ctx1.
Qed.

Lemma x_in_ctx0_or_ctx1 {x ctx0 ctx1}
  (x_in_append_ctx0_ctx1 : count x (append ctx0 ctx1) = 1)
  : {count x ctx0 = 1 /\ count x ctx1 = 0} +
    {count x ctx0 = 0 /\ count x ctx1 = 1}
  .
  induction ctx0.
  -
    apply right.
    apply conj.
    reflexivity.
    exact x_in_append_ctx0_ctx1.
  -
    simpl in *; destruct (String.eqb x a).
    inversion x_in_append_ctx0_ctx1; rewrite H0 in *.
    destruct (x_not_in_ctx0_and_ctx1 H0) as [x_not_in_ctx0 x_not_in_ctx1];
    clear H0 x_in_append_ctx0_ctx1.
    rewrite x_not_in_ctx0 in *; rewrite x_not_in_ctx1 in *.
    apply left.
    apply conj.
    reflexivity.
    reflexivity.
    apply IHctx0.
    exact x_in_append_ctx0_ctx1.
Qed.

(* Lemma ind_system
  (P : forall {ctx term}, System ctx term -> Type)
  (p_var : forall var, P (S_var var))
  (p_lam : forall {lam_ctx} param {body_term} body,
    P (@S_lam lam_ctx param body_term body))
  (p_app : forall {lam_ctx lam_term} lam {arg_ctx arg_term} arg uniq,
    P (@S_app lam_ctx lam_term lam arg_ctx arg_term arg uniq))
  : forall ctx term (system : System ctx term), P system.
  intros.
  induction system.
  apply p_var. *)

Lemma x_y_ctx_so_neq {x term} (system : System [x; x] term) : False.
  remember [x; x] as ctx.
  induction system.
  inversion Heqctx.
  inversion system.

  
  intros eq; inversion eq.
  inversion system.
  induction system eqn : system_eq.

Lemma x_y_ctx_so_neq {x y ctx0 ctx1 ctx2 term}
  (system : System (append ctx0 (append (x :: ctx1) (y :: ctx2))) term)
  : x <> y.
  induction system eqn : system_eq.
  -
    intros; inversion system; clear system.
    subst H.
    substitue
  destruct (String.eqb_spec x y).
  destruct e.
  destruct (all_ctx_are_uniq system x).
  -
    (* TODO: makes induction easier *)
    clear term system.
    induction ctx0.
    simpl in *; rewrite (String.eqb_refl x) in *; inversion e.
    apply IHctx0.
    simpl in *; destruct (String.eqb x a).
    inversion e.
    exact e.
  -
    clear term system.
    induction ctx0.
    induction ctx1.
    simpl in *; rewrite (String.eqb_refl x) in *; inversion e.
    apply IHctx1; clear IHctx1.
    simpl in *; destruct (String.eqb x a).
    simpl in *; rewrite (String.eqb_refl x) in *; inversion e.
    simpl in *; rewrite (String.eqb_refl x) in *.
    exact e.
    apply IHctx0; clear IHctx0.
    simpl in *; destruct (String.eqb x a); clear a.
    --
      induction ctx0.
      simpl in *; rewrite (String.eqb_refl x) in *; inversion e.
      simpl in *; destruct (String.eqb x a).
      inversion e.
      apply IHctx0; clear IHctx0.
      exact e.
    --
      exact e.
  -
    exact n.
Qed.

Lemma all_ctx_are_uniq {ctx term} (system : System ctx term) : uniq ctx.
  unfold uniq.
  revert ctx system.
  induction term.
  -
    intros; inversion system; destruct H, H1; clear system; simpl.
    destruct (String.eqb x var0).
    exact (right eq_refl).
    exact (left eq_refl).
  -
    intros; inversion system; destruct H, H2; clear system param0 H1.
    destruct (IHterm _ body x).
    simpl in *; destruct (String.eqb x param).
    inversion e.
    exact (left e).
    simpl in *; destruct (String.eqb x param).
    inversion e.
    exact (left eq_refl).
    exact (right e).
  -
    intros; inversion system; destruct H, H1, H2; clear system.
    rename IHterm1 into IHlam; rename IHterm2 into IHarg.
    revert x.
    exact uniq0.
Qed.


Lemma x_y_ctx_so_neq {x y ctx0 ctx1 ctx2 term}
  (system : System (append ctx0 (append (x :: ctx1) (y :: ctx2))) term)
  : x <> y.
  destruct (String.eqb_spec x y).
  destruct e.
  destruct (all_ctx_are_uniq system x).
  -
    (* TODO: makes induction easier *)
    clear term system.
    induction ctx0.
    simpl in *; rewrite (String.eqb_refl x) in *; inversion e.
    apply IHctx0.
    simpl in *; destruct (String.eqb x a).
    inversion e.
    exact e.
  -
    clear term system.
    induction ctx0.
    induction ctx1.
    simpl in *; rewrite (String.eqb_refl x) in *; inversion e.
    apply IHctx1; clear IHctx1.
    simpl in *; destruct (String.eqb x a).
    simpl in *; rewrite (String.eqb_refl x) in *; inversion e.
    simpl in *; rewrite (String.eqb_refl x) in *.
    exact e.
    apply IHctx0; clear IHctx0.
    simpl in *; destruct (String.eqb x a); clear a.
    --
      induction ctx0.
      simpl in *; rewrite (String.eqb_refl x) in *; inversion e.
      simpl in *; destruct (String.eqb x a).
      inversion e.
      apply IHctx0; clear IHctx0.
      exact e.
    --
      exact e.
  -
    exact n.
Qed.

  
Lemma subst_preserve_system {lam_ctx arg_ctx param arg_term body_term}
  (body : System (param :: lam_ctx) body_term) (arg : System arg_ctx arg_term)
  (lam_ctx_plus_arg_ctx_is_uniq : uniq (append lam_ctx arg_ctx))
  : System (append lam_ctx arg_ctx) (subst param arg_term body_term).
  revert param lam_ctx body arg_ctx arg_term arg
    lam_ctx_plus_arg_ctx_is_uniq.
  induction body_term.
  -
    intros; simpl.
    inversion body; destruct H1, H; clear var0 H0.
    simpl; rewrite String.eqb_refl.
    exact arg.
  -
    intros; simpl; apply S_lam.
    inversion body; clear body param1 H1 body_term0 H2 lam_ctx0 H.

    

Lemma t_var_context_is_singleton {a b a}
  (linear : Linear [a; b] (T_var a)) : False.
  destruct linear.
  remember (T_var c) as term.
  

Lemma t_var_context_is_singleton {ctx var}
  (linear : Linear (var :: ctx) (T_var var))
  : ctx = [].
  revert var linear.
  induction ctx.
  intros; reflexivity.
  intros.
  inversion linear; clear linear term H1.
  enough (ctx = []).
  rewrite H in *; clear H.
  enough (y = var)
  destruct
  apply IHctx.
  reflexivity.

  destruct ctx.
  -
    inversion linear.
    (* exchange *)
    destruct ctx0.
    inversion H0.
    inversion H0.
  -
    enough (ctx = []).
    --
      rewrite H in *; clear H.
      inversion linear.
      reflexivity.
      (* exchange *)
      destruct ctx0.
      destruct ctx1.
      inversion H0.
      inversion H0.
      destruct ctx0.
      inversion H0.
      inversion H0.
    --
      induction ctx.
      reflexivity.
      inversion linear; clear linear.
      destruct ctx0.
      destruct ctx1.
      simpl in *.
      
      
      intros s0 var linear.
      inversion linear; clear linear.
      
    
      


      reflexivity.


    induction  
    induction ctx.

    inversion linear.
    reflexivity.
    (* exchange *)
    destruct (String.eqb_spec a var).
    destruct e.
    induction ctx.
    reflexivity.

    destruct ctx.
    reflexivity.
    inversion linear.
    
    --
      intros s var linear.
      inversion linear.
      reflexivity.
      (* exchange*)
      destruct ctx0.
      destruct ctx1.
      inversion H0.
      inversion H0.
      destruct ctx0.
      inversion H0.
      inversion H0.
    --
      intros s var linear.
      inversion linear; clear linear term H1.
      epose (IHctx var).
      induction ctx.
      inversion linear.
      



       ctx.

      
Qed.
(* TODO: stop using inversion? *)
Lemma subst_preserve_linear {lam_ctx arg_ctx param arg_term body_term}
  (body : Linear (param :: lam_ctx) body_term) (arg : Linear arg_ctx arg_term)
  (lam_ctx_plus_arg_ctx_is_uniq : uniq (append lam_ctx arg_ctx))
  : Linear (append lam_ctx arg_ctx) (subst param arg_term body_term).
  induction body_term.
  -
    assert (lam_ctx = []).
    induction lam_ctx.
    reflexivity.
    inversion body.
    simpl.
    destruct (param =? var).
    inversion body; simpl.
    rewrite (String.eqb_refl var).
    exact arg.
    (* exchange *)
    induction ctx0.
    
  simp


    

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
Lemma exchange_preserve_free {ctx0}
  : forall {ctx1 ctx2 var x y b},
    free_in_ctx var (append ctx0 (append (x :: ctx1) (y :: ctx2))) = b ->
    free_in_ctx var (append ctx0 (append (y :: ctx1) (x :: ctx2))) = b.
  induction ctx0.
  -
    intros ctx1.
    induction ctx1.
    intros ctx2 var x y b var_free; simpl in *.
    destruct (String.eqb y var).
    destruct (String.eqb x var).
    exact var_free.
    exact var_free.
    exact var_free.
    intros ctx2 var x y b var_free; simpl in *.
    destruct (String.eqb a var).
    --
      destruct (String.eqb x var).
      destruct (String.eqb y var).
      exact var_free.
      exact var_free.
      destruct (String.eqb y var).
      exact var_free.
      exact var_free.
    --
      apply IHctx1; clear IHctx1.
      exact var_free.
  -
    intros ctx1 ctx2 var x y b var_free; simpl in *.
    destruct (String.eqb a var).
    exact var_free.
    apply IHctx0; clear IHctx0.
    exact var_free.
Qed.
    
Inductive Linear : Context -> Term -> Type :=
  | L_var var : Linear [var] (T_var var)
  | L_lam {lam_ctx} param
    {body_term} (body : Linear (param :: lam_ctx) body_term)
    (param_not_free_in_lam_ctx : free_in_ctx param lam_ctx = false)
    : Linear lam_ctx (T_lam param body_term)
  | L_app {lam_ctx lam_term} (lam : Linear lam_ctx lam_term)
    {arg_ctx arg_term} (arg : Linear arg_ctx arg_term)
    (no_inter : forall {x},
      free_in_ctx x lam_ctx = true -> free_in_ctx x arg_ctx = false)
    : Linear (append lam_ctx arg_ctx) (T_app lam_term arg_term).

Inductive Value : Term -> Type :=
  (* TODO: make body also value *)
  | V_lam param body : Value (T_lam param body).

Lemma exchange_preserve_linear {term}
  : forall {ctx0 ctx1 ctx2 : Context}, forall x y,
    Linear (append ctx0 (append (x :: ctx1) (y :: ctx2))) term ->
    Linear (append ctx0 (append (y :: ctx1) (x :: ctx2))) term.
  induction term.
  -
    intros ctx0 ctx1 ctx2 x y linear_var.
    induction ctx0.
    induction ctx1.
    inversion linear_var.
    inversion linear_var.
    induction ctx0.
    inversion linear_var.
    inversion linear_var.
  -
    intros ctx0 ctx1 ctx2 x y lam;
    inversion lam; destruct H2; clear H lam_ctx H1 param0 lam.
    refine (L_lam _ _ _).
    exact (IHterm (param :: ctx0) _ _ _ _ body).
    clear IHterm body.
    apply exchange_preserve_free.
    exact param_not_free_in_lam_ctx.
  -
    intros ctx0 ctx1 ctx2 x y app.
    inversion app; destruct H, H2; clear app.
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
    refine (L_app _ _ _).
    apply IHterm1.
    exact H.
    exact lam.
    apply IHterm2.
    exact H0.
    exact arg.
    exact no_inter.
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
    refine (L_lam _ _ _).
    rewrite (
      free_x_is_false_but_y_ctx_implies_y_neq_x param_not_free_in_lam_ctx
    ).
    apply IHbody_term.
    exact (@exchange_preserve_linear _ [] [] _ _ _ body).
    exact (free_x_is_false_in_y_ctx_but_without_y param_not_free_in_lam_ctx).
  -
    intros ctx app; simpl.
    inversion app; destruct H, H2; clear app.
    rename IHbody_term1 into IHlam; rename IHbody_term2 into IHarg.
    enough (
      param_in_lam_or_arg :
      {free_in_ctx param lam_ctx = true /\ free_in_ctx param arg_ctx = false} +
      {free_in_ctx param lam_ctx = false /\ free_in_ctx param arg_ctx = true}
    ).
    destruct param_in_lam_or_arg.
    destruct a.
    destruct lam_ctx.
    inversion H.
    inversion H0; clear H0; rewrite H3 in *; clear s H3.
    apply L_app.
    apply IHlam.
    exact lam.
    apply subst_but_not_in_ctx.
    exact H1.
    exact arg.
Admitted.

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
    apply subst_preserve_linear.
    exact arg.
    exact body.
    clear param_not_free_in_lam_ctx.
    generalize dependent arg_term.
    generalize dependent param.
    induction body_term.
    --
      intros param body arg_term arg arg_value.
      inversion body; destruct H; clear var0 H0; simpl in *.
      rewrite (String.eqb_refl).
      exact arg_value.
    --
      intros param0 body arg_term arg arg_value; simpl in *.
      inversion body; destruct H1, H2; clear lam_ctx H.
      apply V_lam.
    --
      intros param0 body arg_term arg arg_value; simpl in *.
      inversion body; destruct H, H2.
      enough (Value (subst param0 arg_term lam_term)).
      destruct H.
      simpl.
      epose (fun x => IHbody_term1 param0 _ _ arg arg_value).
      simpl.






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

