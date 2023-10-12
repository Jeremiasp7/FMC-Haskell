module Nat where
import Prelude hiding (fact, fib, min, max, div, quot, rem, gcd, lcm, sum, mult, pow, pred, double, (<=), (<), (>=), (>), sub)

data Nat = O | S Nat
    deriving ( Eq , Show )

--OPERADORES DE ORDEM:

--Menor ou igual que
(<=) :: Nat -> Nat -> Bool
O <= m = True
m <= O = False
(S m) <= (S n) = m <= n

--Menor que
(<) :: Nat -> Nat -> Bool
m < O = False
O < m = True
(S m) < (S n) = m < n

--Maior ou igual que
(>=) :: Nat -> Nat -> Bool
m >= O = True
O >= m = False
(S m) >= (S n) = m >= n

--Maior que
(>) :: Nat -> Nat -> Bool
O > m = False
m > O = True
(S m) > (S n) = m > n

--OPERADORES DOS NATURAIS:

--Double
double :: Nat -> Nat
double m = mult m (S(S O))

--Soma
sum :: Nat -> Nat -> Nat
sum m O = m
sum m (S n) = S(sum m n)

--Subtração
sub :: Nat -> Nat -> Nat
sub O m = O
sub m O = m 
sub (S m) (S n) = sub m n

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

--Divisão
div :: (Nat, Nat) -> (Nat, Nat)
div (m, n)
    |m < n = (O, m)
    |otherwise = (sum q' (S O), r')
    where 
        (q', r') = div(sub m n, n)

--Quociente
quot :: Nat -> Nat -> Nat
quot m n
    |m < n = O
    |otherwise = sum q' (S O)
    where
        (q', r') = div(sub m n, n)

--Resto
rem :: Nat -> Nat -> Nat
rem m n
    |m < n = O
    |otherwise = r' 
    where
        (q', r') = div(sub m n, n)

--Máximo divisor comum
gcd :: Nat -> Nat -> Nat
gcd m O = m
gcd m n 
    |m > n = gcd n (rem m n)    
    |otherwise = gcd m (rem n m)
    
--Mínimo múltiplo comum
lcm :: Nat -> Nat -> Nat
lcm m n = q'
    where
        (q', r') = div(mult m n, gcd m n)

--OPERADORES BOOLEANOS:

-- Menor ou igual que
leq :: Nat -> Nat -> Bool
leq O m = True
leq m O = False
leq (S m) (S n) = leq m n

-- É par
ev :: Nat -> Bool
ev O = True
ev (S O) = False
ev (S(S m)) = ev m

-- É ímpar
od :: Nat -> Bool
od O = False
od (S O) = True
od (S(S m)) = od m

--É múltiplo de 3
isMul3 :: Nat -> Bool
isMul3 O = True
isMul3 (S O) = False
isMul3 (S(S O)) = False
isMul3 (S(S(S m))) = isMul3 m

--Divides
divides :: Nat -> Nat -> Bool
divides m O = True
divides O m = False
divides m n 
    |m > n = rem m n == O
    |otherwise = rem n m == O

--isZero
isZero :: Nat -> Bool
isZero O = True
isZero (S O) = False
isZero (S m) = isZero m


