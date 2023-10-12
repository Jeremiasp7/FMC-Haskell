module Bool where
import Nat ( Nat(..) )
import Prelude hiding (leq, ev, od, isMul3, divides, isZero, Bool, True, False, ifthenelse)

data Bool = True | False
    deriving ( Eq , Show )

--If then else
ifthenelse :: Bool -> Nat -> Nat -> Nat
ifthenelse True m n = m
ifthenelse False m n = n