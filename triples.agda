variable A B : Set


data Nat : Set where
  zero : Nat
  suc : Nat -> Nat


data Bool : Set where
  True : Bool
  False : Bool

equals : Nat -> Nat -> Bool
equals zero zero = True
equals (suc n) (suc m) = equals n m
equals zero (suc n) = False
equals (suc n) zero = False

plus : Nat -> Nat -> Nat
plus zero m = m
plus (suc n) m = suc (plus n m)

minus : Nat -> Nat -> Nat
minus zero zero = zero
minus (suc n) zero = suc n
minus zero (suc n) = zero
minus (suc n) (suc m) = minus n m

mult : Nat -> Nat -> Nat
mult zero m = zero
mult (suc n) m = plus m (mult n m)

data List (A : Set) : Set where
  Empty : List A
  listsuc : A → List A → List A





if : Bool → A → A → A
if True x y = x
if False x y = y

id : A → A
id x = x


concat : List A → List A → List A
concat Empty ys = ys
concat (listsuc x xs)  ys = (listsuc x (concat xs ys))




range : Nat → Nat → List Nat
range x y = go (minus (suc y)  x) x 
    where
        go : Nat → Nat → List Nat
        go zero    _ = Empty
        go (suc m) n = listsuc n (go m (suc n))
  


-- record Triple : Set where
--   constructor triple
--   field fst snd trd : Nat

data Triple : Set where
  triple : Nat -> Nat -> Nat -> Triple

bind1 : Nat → Nat → List Nat → List Triple
bind1 y z Empty = Empty
bind1 y z (listsuc x xs) = listsuc (triple x y z) (bind1 y z xs)

bind2 : Nat → List Nat → List Triple
bind2 z Empty = Empty
bind2 z (listsuc y ys) = concat (bind1 y z (range (suc zero) y)) (bind2 z ys)

bind3 : List Nat → List Triple
bind3 Empty = Empty
bind3 (listsuc z zs) = concat (bind2 z (range (suc zero) z)) (bind3 zs)

alltriples : Nat → List Triple
alltriples top = bind3 (range (suc zero) top)

pythagorean : Triple → Bool
pythagorean (triple x y z) = (equals (plus (mult x x) (mult y y)) (mult z z))


filterP : List Triple → List Triple
filterP Empty = Empty
filterP (listsuc x xs) = (if (pythagorean x) (listsuc x (filterP xs)) (id (filterP xs))) 


triples : Nat → List Triple
triples top = filterP (alltriples top)


sumThree : Triple -> Nat
sumThree (triple x y z) = (plus (plus x y) z)

sumall : List Triple → Nat
sumall Empty = zero
sumall (listsuc x xs) = (plus (sumThree x) (sumall xs))
-- sumall Empty = zero
-- sumall (listsuc (triple x y z)  xs) = (plus (plus (plus x y) z) (sumall xs))

-- -- should be 200
-- -- ten = zero

test1 = sumall (triples ( ( ( ( ( (suc (suc (suc (suc (suc zero))))))))))) -- evaluates to 33638 