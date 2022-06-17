postulate A : Set

id : A → A
id x = x

data Bool : Set where
  True False : Bool

not : Bool → Bool
not True  = False
not False = True

ite : {A : Set} → Bool → A → A → A
ite True x y = x
ite False x y = y

-- {-# NON_TERMINATING #-}
-- loop : Bool
-- loop = loop

-- test1 = ite False loop True

data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

one = suc zero
two = suc one
three = suc two

pred : Nat → Nat
pred zero = zero
pred (suc n) = n


plus : Nat → Nat → Nat
plus zero n = n
plus (suc m) n = suc (plus m n)

twice : Nat → Nat
twice zero = zero
twice (suc n) = suc (suc (twice n))

pow2 : Nat → Nat
pow2 zero = suc zero
pow2 (suc n) = twice (pow2 n)

consume : Nat → Nat
consume zero = zero
consume (suc n) = consume n

test2 = consume (pow2 (twice (twice (twice three))))


data Vec (@0 A : Set) : @0 Nat → Set where
  nil : Vec A zero
  con : {@0 n : Nat} → A → Vec A n → Vec A (suc n)

head : {@0 A : Set} {@0 n : Nat} → Vec A (suc n) → A
head (con x xs) = x

tail : {@0 A : Set} {@0 n : Nat} → Vec A (suc n) → Vec A n
tail (con x xs) = xs

map : {@0 A B : Set} {@0 n : Nat} → (A → B) → Vec A n → Vec B n
map f nil = nil
map f (con x xs) = con (f x) (map f xs)

test3 = head (tail (map suc (con zero (con (suc zero) (con (suc (suc zero)) nil)))))

-- Testing that names are properly sanitized
asdf = zero

test4 = asdf

module M (n : Nat) where
  fie : Nat
  fie = suc n

  foe : Nat
  foe = suc fie

open M (suc (suc zero))

fun : Nat
fun = plus fie foe