data Bool : Set where
  True : Bool
  False : Bool

data Nat : Set where
  zero : Nat
  suc : Nat -> Nat

not : Bool → Bool
not True  = False
not False = True

-- notFalse = not False

test : Bool -> Bool -> Bool -> Bool -> Bool -> Bool
test _ _ _ _ x = x

and : Bool -> Bool -> Bool
and True True = True
and False True = False
and True False = False
and False False = False

id : Bool -> Bool
id x = x

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