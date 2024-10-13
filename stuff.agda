{-
  This is a demo of how self types can be erased to
    coinduction + induction-induction + UIP

  The following code is equivalent to

  Bool : Type;
  true : Bool;
  false : Bool;

  Bool = (b : Bool) & (P : (b : Bool) -> Type) ->
    (then : P true) -> (else : P false) -> P B;
  true = P => then => else => then
  false = P => then => else => else
  ```

  But it does a detour by stratified induction, which can be done
    by having coinductive pairs

  This encodes self types in Agda
  There is a couple ideas,
    - self types can be seen as a recursive dependent intersection
      A : Type;
      A = (x : A) & B; // this is the same as a self
    - self types are like coinductive pairs equipped with an equality
        stating that the p = fst p
      A0 : Type;
      A0 = (x : A0, y : B);

      A = (x0 : A0, w : A0 == fst x0);
    - pairs where the right side is irrelevant are
      defined by the left side, such that fst a = fst b -> a = b

  The equality needs to be a strict equality as it needs to be irrelevant.
  
  Induction-induction is likely not needed if there is sigma
  Sigma is likely not needed if there is induction-induction
  Agda doesn't have universe lifting so large elimination
    will fail without --type-in-type
  
  The major insight is that coinduction is how you introduce
    infinite self references in types.
  The equality needs to be a strict equality as it needs to be irrelevant.

  If I'm correct and sigma is not needed
    this leads to a very nice proof of
    strong normalization of a bunch of things.
    
    - Church-style annotated self types
    - Church-style dependent intersections
    - A bunch of PTS + Fix + ADTs, like the CIC
      are reduced to the PTS + Fix side.

    As you can then erase those to a theory with 
    a very powerful fixpoint, strict equality 
      and universes with lifting for large elimination.
-}
{-# OPTIONS --guardedness #-}

module stuff where

open import Agda.Builtin.Equality

symm : ∀ {l} {A : Set l} {n m : A} → n ≡ m → m ≡ n
symm refl = refl

record Pack {l} (A : Set l) (fst : A → A) : Set l where
  constructor pack
  field
    x : A
    w : x ≡ fst x

uip : ∀ {l} {A : Set l} {m n : A} (a b : m ≡ n) → a ≡ b
uip refl refl = refl

eq_x_is_enough : ∀ {l} {A : Set l} {fst} (l r : Pack A fst) →
  Pack.x l ≡ Pack.x r → l ≡ r
eq_x_is_enough (pack l l_w) (pack r r_w) H
  rewrite H rewrite uip l_w r_w = refl

lower_w : ∀ {l} {A : Set l} (fst : A → A) {x} →
  x ≡ fst x → fst x ≡ fst (fst x)
lower_w {_} {_} fst H rewrite symm H = H

lower : ∀ {l} {A : Set l} {fst} → Pack A fst → Pack A fst
lower {l} {A} {fst} (pack x w) = pack (fst x) (lower_w fst w)

lower_is_same : ∀ {l} {A : Set l} {fst} (P : Pack A fst) → P ≡ lower P
lower_is_same (pack x w) = eq_x_is_enough _ _ w

record W_Bool : Set₁
w_true : W_Bool
w_false : W_Bool

{-# NO_POSITIVITY_CHECK #-}
{-# NO_UNIVERSE_CHECK #-}
record W_Bool where
  coinductive
  field
    -- ideally this should also be erasable
    b : W_Bool
    i : ∀ {l} (P : W_Bool → Set l) → P w_true → P w_false → P b
W_Bool.b w_true = w_true
W_Bool.i w_true P then else = then

W_Bool.b w_false = w_false
W_Bool.i w_false P then else = else

Bool : Set₁
Bool = Pack W_Bool W_Bool.b

true : Bool
true = pack w_true refl

false : Bool
false = pack w_false refl

ind_lower : ∀ b {l} (P : Bool → Set l) → P true → P false → P (lower b)
ind_lower (pack x w) P then else = W_Bool.i x
  (λ { x → ∀ w → P (pack x w) })
  (λ { refl → then }) (λ { refl → else }) _

ind : ∀ b {l} (P : Bool → Set l) → P true → P false → P b
ind b P then else rewrite lower_is_same b = ind_lower b P then else
