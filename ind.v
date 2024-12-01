Set Universe Polymorphism.

Definition Eq_0 {A : Type} (x y : A) := forall (P : A -> Type), P x -> P y.
Definition refl_0 {A : Type} {x : A} : Eq_0 x x := fun P p_x => p_x.

Definition Eq_1 {T_0 : Type} {T_1 : T_0 -> Type} {x_0 y_0 : T_0} (x_1 : T_1 x_0) (y_1 : T_1 y_0)
  := forall (P : forall z_0, T_1 z_0 -> Type), P x_0 x_1 -> P y_0 y_1.
Definition refl_1 {T_0 : Type} {T_1 : T_0 -> Type} {x_0 : T_0} {x_1 : T_1 x_0} : Eq_1 x_1 x_1 := fun P p_x => p_x.

Definition Nat_0 := forall (A : Type), A -> (A -> A) -> A.
Definition zero_0 : Nat_0 := fun A z s => z.
Definition succ_0 (n : Nat_0) : Nat_0 := fun A z s => s (n A z s).

Definition Nat_1 n_0 :=
  forall (P : Nat_0 -> Type),
  P zero_0 -> (forall n, P n -> P (succ_0 n)) -> P n_0.
Definition zero_1 : Nat_1 zero_0 := fun P p_z p_s => p_z.
Definition succ_1 {n_0} (n_1 : Nat_1 n_0) : Nat_1 (succ_0 n_0)
  := fun P p_z p_s => p_s n_0 (n_1 P p_z p_s).

Definition Nat_2 {n_0} (n_1 : Nat_1 n_0) :=
  forall (P : forall n, Nat_1 n -> Type),
  P zero_0 zero_1 -> (forall n_0 (n_1 : Nat_1 n_0), 
    P n_0 n_1 -> P (succ_0 n_0) (succ_1 n_1)) -> P n_0 n_1.
Definition zero_2 : Nat_2 zero_1 := fun P p_z p_s => p_z.
Definition succ_2 {n_0} {n_1 : Nat_1 n_0} (n_2 : Nat_2 n_1) : Nat_2 (succ_1 n_1)
  := fun P p_z p_s => p_s n_0 n_1 (n_2 P p_z p_s).

Definition add_0 (n_0 m_0 : Nat_0) : Nat_0 :=
  fun A z s => n_0 A (m_0 A z s) s.

Lemma add_0_lift_succ_left n_0 m_0
  : Eq_0 (succ_0 (add_0 n_0 m_0)) (add_0 (succ_0 n_0) m_0).
Proof. exact refl_0. Defined.
Lemma add_0_lift_succ_right {n_0}  (n_1 : Nat_1 n_0) m_0
  : Eq_0 (succ_0 (add_0 n_0 m_0)) (add_0 n_0 (succ_0 m_0)).
Proof.
  apply n_1.
  - exact refl_0.
  - clear n_1 n_0; intros n_0 IHn_0.
    apply add_0_lift_succ_left.
    apply add_0_lift_succ_left.
    intros P p.
    apply IHn_0.
    exact p.
Defined.
Lemma n_eq_n_add_0_zero {n_0}
  (n_1 : Nat_1 n_0) : Eq_0 n_0 (add_0 n_0 zero_0).
Proof.
  apply n_1.
  - exact refl_0.
  - clear n_1 n_0; intros n_0 IHn_0.
    apply (add_0_lift_succ_left n_0 zero_0).
Defined.

Lemma add_0_comm {n_0 m_0} (n_1 : Nat_1 n_0) (m_1 : Nat_1 m_0)
  : Eq_0 (add_0 n_0 m_0) (add_0 m_0 n_0).
  apply n_1.
  - apply n_eq_n_add_0_zero.
    exact m_1.
  - clear n_1 n_0; intros n_0 IHn_0.
    apply (add_0_lift_succ_left n_0 m_0).
    apply (add_0_lift_succ_right m_1 n_0).
    intros P p.
    apply IHn_0.
    exact p.
Defined.

Definition add_1 {n_0 m_0}
  (n_1 : Nat_1 n_0) (m_1 : Nat_1 m_0) : Nat_1 (add_0 n_0 m_0).
  intros P z s.
  apply n_1.
  - exact (m_1 P z s).
  - clear n_1 n_0; intros n_0.
    apply (s _).
Defined.
Lemma add_1_lift_succ_left {n_0 m_0} (n_1 : Nat_1 n_0) (m_1 : Nat_1 m_0)
  : Eq_1 (succ_1 (add_1 n_1 m_1)) (add_1 (succ_1 n_1) m_1).
Proof. exact refl_1. Defined.
Lemma add_1_lift_succ_right {n_0 m_0}
  {n_1 : Nat_1 n_0} (n_2 : Nat_2 n_1) (m_1 : Nat_1 m_0)
  : Eq_1 (succ_1 (add_1 n_1 m_1)) (add_1 n_1 (succ_1 m_1)).
Proof.
  apply n_2.
  - exact refl_1.
  - clear n_2 n_1 n_0; intros n_0 n_1 IHn_1.
    apply add_1_lift_succ_left.
    apply add_1_lift_succ_left.
    intros P p.
    apply IHn_1.
    exact p.
Defined.
Lemma n_eq_n_add_1_zero {n_0} {n_1 : Nat_1 n_0}
  (n_2 : Nat_2 n_1) : Eq_1 n_1 (add_1 n_1 zero_1).
Proof.
  apply n_2.
  - exact refl_1.
  - clear n_2 n_1 n_0; intros n_0 n_1 IHn_1.
    apply (add_1_lift_succ_left n_1 zero_1).
Defined.
Lemma add_1_comm {n_0 m_0} {n_1 : Nat_1 n_0} {m_1 : Nat_1 m_0}
  (n_2 : Nat_2 n_1) (m_2 : Nat_2 m_1) : Eq_1 (add_1 n_1 m_1) (add_1 m_1 n_1).
  apply n_2.
  - apply n_eq_n_add_1_zero.
    exact m_2.
  - clear n_2 n_1 n_0; intros n_0 n_1 IHn_1.
    apply (add_1_lift_succ_left n_1 m_1).
    apply (add_1_lift_succ_right m_2 n_1).
    intros P p.
    apply IHn_1.
    exact p.
Defined.
