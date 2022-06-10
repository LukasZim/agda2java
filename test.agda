data Bool : Set where
  True : Bool
  False : Bool

-- not : Bool → Bool
-- not True  = False
-- not False = True

data Nat : Set where
  zero : Nat
  suc : Nat -> Nat

minusOne : Nat -> Nat
minusOne zero = zero
minusOne (suc n) = n

ans = minusOne (suc (suc zero))


-- notFalse = not False

-- test : Bool -> Bool -> Bool -> Bool -> Bool -> Bool
-- test _ _ _ _ x = x

-- and : Bool -> Bool -> Bool
-- and True True = True
-- and False True = False
-- and True False = False
-- and False False = False

-- or : Bool -> Bool -> Bool
-- or False False = False
-- or _ _ = True

-- id : Bool -> Bool
-- id x = x




-- minus : Nat -> Nat -> Nat
-- minus zero x = x
-- minus x zero = zero
-- minus (suc y) (suc x) = minus y x

-- minusOneAlt : Nat -> Nat
-- minusOneAlt = minus (suc zero)



-- postulate A : Set

-- id : A → A
-- id x = x

-- a = not True
-- b = True
-- c = False

-- halve : Nat -> Nat
-- halve (suc (suc n)) = suc (halve n)
-- halve _ = zero




data Vector (A : Set) : Nat → Set where
  Empty  : Vector A zero
  Next : {n : Nat} → A → Vector A n → Vector A (suc n)