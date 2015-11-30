import Data.List
import Data.Char

data Nat = Zero | Succ Nat deriving Show

natToInteger :: Nat -> Integer
-- natToInteger = head . m
--   where m Zero = [0]
--         m (Succ n) = [sum [x | x <- (1 : m n)]]
natToInteger = \n -> genericLength [c | c <- show n, c == 'S']


integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat n = Succ (integerToNat $ n - 1)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat ->  Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

check a b = (a*b, natToInteger $ integerToNat a `mult` integerToNat b)
