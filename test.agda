data Bool : Set where
  True : Bool
  False : Bool



data Nat : Set where
  zero : Nat
  suc : Nat -> Nat


plus : Nat → Nat → Nat
plus zero n = n
plus (suc m) n = suc (plus m n)

mult : Nat -> Nat -> Nat
mult zero y = zero
mult (suc x) y = plus y (mult x y)

twice : Nat → Nat
twice zero = zero
twice (suc n) = suc (suc (twice n))

pow2 : Nat → Nat
pow2 zero = suc zero
pow2 (suc n) = twice (pow2 n)


consume : Nat → Nat
consume zero = zero
consume (suc n) = consume n

one = suc zero
two = suc one
three = suc two

ans = plus (suc zero) (suc zero)
ans2 = mult (suc (suc (suc zero))) (suc (suc zero))
test2 = consume (pow2 (twice (twice (twice three))))

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