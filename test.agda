data Bool : Set where
  true : Bool
  false : Bool

not : Bool → Bool
not true  = false
not false = true

a = not true
b = true
c = false

tripleAnd : Bool → Bool → Bool → Bool
tripleAnd true true true = true
tripleAnd _ _ _ = false 