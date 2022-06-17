-- open import Agda.Builtin.Nat

data Nat : Set where
  zero : Nat
  suc : Nat -> Nat








plus : Nat -> Nat -> Nat
plus zero m = m
plus (suc n) m = suc (plus n m)













fib : Nat -> Nat
fib zero = zero
fib (suc zero) = suc zero
fib (suc (suc m)) = plus (fib (suc m)) (fib m)

main : Nat -> Nat
main n = fib n
