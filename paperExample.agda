postulate A : Set
-- head : {@0 A : Set} {@0 n : Nat} → Vec A (suc n) → A
-- head (con x xs) = x
data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

  
data Vector (A : Set) : Nat → Set where
  []  : Vector A zero
  _∷_ : {n : Nat} → A → Vector A n → Vector A (suc n)



head : {A : Set} {n : Nat} → Vector A (suc n) → A
head (x ∷ xs) = x