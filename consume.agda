{-# OPTIONS --guardedness #-}

module consume where

-- open import IO

data Nat : Set where
  zero : Nat
  suc : Nat -> Nat


main = zero