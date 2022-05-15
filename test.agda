data Bool : Set where
  true : Bool
  false : Bool

not : Bool → Bool
not true  = false
not false = true

a = not true
b = true
c = false
