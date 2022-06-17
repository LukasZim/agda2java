data Nat : Set where
  zero : Nat
  suc : Nat -> Nat


data Bool : Set where
  True : Bool
  False : Bool

data List (A : Set) : Set where
  Empty : List A
  listsuc : A → List A → List A


data U : Set where
    natType : U
    boolType : U
    listType : U → U
    

Universe : U -> Set
Universe natType = Nat
Universe boolType = Bool
Universe (listType t) = List (Universe t)


f : (c : U) -> Universe c -> Universe c
f natType zero = zero
f natType (suc x) = x
f boolType False = True
f boolType True = False
f (listType c) Empty = Empty
f (listType c) (listsuc x xs) = (listsuc (f c x) (f (listType c) xs))


test = f natType zero
test2 = f (listType boolType) (listsuc False Empty)
test3 = f (listType (listType boolType)) (listsuc (listsuc False Empty) (listsuc Empty (listsuc (listsuc True Empty) Empty)))