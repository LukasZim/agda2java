data Bool : Set where
  True : Bool
  False : Bool

not : Bool → Bool
not True  = False
not False = True

-- data Nat : Set where
--   zero : Nat
--   suc : Nat -> Nat

-- halve : Nat -> Nat
-- halve (suc (suc n)) = suc (halve n)
-- halve _ = zero

-- postulate A : Set

-- id : A → A
-- id x = x

-- a = not True
-- b = True
-- c = False

-- tripleAnd : Bool → Bool → Bool → Bool
-- tripleAnd True True True = True
-- tripleAnd _ _ _ = False

-- weirdOp : Bool -> Bool -> Bool -> Bool
-- weirdOp True _ true = true
-- weirdOp true false false = true
-- weirdOp false true true = true
-- weirdOp false false true = true
-- weirdOp _ _ _ = false