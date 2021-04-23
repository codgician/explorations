module Haskell.Codewars.Peano where
import Prelude hiding (even, odd, div, compare, Num, Int, Integer, Float, Double, Rational, Word)

data Peano = Zero | Succ Peano deriving (Eq, Show)

add, sub, mul, div :: Peano -> Peano -> Peano
-- Addition
add Zero      = id
add (Succ x)  = Succ . add x

-- Subtraction
sub x Zero            = x
sub Zero _            = error "negative number"
sub (Succ x) (Succ y) = sub x y 

-- Multiplication
mul Zero _      = Zero
mul (Succ x) y  = mul x y `add` y

-- Integer division
div _ Zero  = error "divide by 0" 
div Zero _  = Zero
div x y | x `compare` y == LT = Zero
        | otherwise           = Succ $ div (x `sub` y) y

even, odd :: Peano -> Bool
-- Even
even Zero = True
even (Succ x) = not $ even x
-- Odd
odd = not . even

compare :: Peano -> Peano -> Ordering
-- Compare
compare Zero Zero         = EQ 
compare Zero _            = LT
compare _ Zero            = GT
compare (Succ x) (Succ y) = compare x y
