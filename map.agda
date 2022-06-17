variable A B : Set


data Nat : Set where
  zero : Nat
  suc : Nat -> Nat


data Bool : Set where
  True : Bool
  False : Bool

data List (A : Set) : Set where
  Empty : List A
  Succ : A → List A → List A

map : {A B : Set} -> (A -> B) -> List A -> List B
map f Empty = Empty
map f (Succ x xs) = (Succ (f x) (map f xs))

plusOne : Nat -> Nat
plusOne x = suc x

test = map (plusOne) (Succ zero (Succ (suc zero) (Succ (suc (suc zero)) Empty)))