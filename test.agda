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

weirdOp : Bool -> Bool -> Bool -> Bool
weirdOp true _ true = true
weirdOp true false false = true
weirdOp false true true = true
weirdOp false false true = true
weirdOp _ _ _ = false