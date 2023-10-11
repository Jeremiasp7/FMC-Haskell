module Nat where
import Prelude hiding (fact, fib, min, max, div, quot, rem, gcd, lcm, sum, mult, pow, pred)

data Nat = O | S Nat
    deriving ( Eq , Show )

--Soma
sum :: Nat -> Nat -> Nat
sum m O = m
sum m (S n) = S(sum m n)

--Multiplicação
mult :: Nat -> Nat -> Nat
mult m O = O 
mult m (S n) = sum m (mult m n)

--Potenciação
pow :: Nat -> Nat -> Nat
pow m O = S O
pow m (S n) = mult m (pow m n)

--Predecessor
pred :: Nat -> Nat
pred O = O
pred (S m) = m

--Fatorial
fact :: Nat -> Nat
fact O = S O
fact (S m) = mult (S m) (fact m)

--Fibonacci
fib :: Nat -> Nat
fib O = O 
fib (S O) = S O
fib (S(S m)) = sum (fib (S m)) (fib m)

--Máximo
max :: Nat -> Nat -> Nat
max m O = m 
max O n = n
max m n = S(max (pred m) (pred n))

--Mínimo 
min :: Nat -> Nat -> Nat
min m O = O
min O n = O
min m n = S(min (pred m) (pred n))
