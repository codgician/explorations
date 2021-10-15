module PolynomField where

import Data.List ( intercalate, sortBy )

newtype BinaryPolynom = BinaryPolynom { unwrap :: [Int] } deriving Eq

zero, one, m :: BinaryPolynom
zero = BinaryPolynom []
one  = BinaryPolynom [0]
m    = BinaryPolynom [8, 4, 3, 1, 0]

deg :: BinaryPolynom -> Int
deg (BinaryPolynom []) = -1
deg (BinaryPolynom (x:_)) = x

-- | Constructs a monom with the given degree.
polyFromDeg :: Int -> BinaryPolynom
polyFromDeg (-1)  = BinaryPolynom []
polyFromDeg x     = BinaryPolynom [x]

polyFromPowers :: [Int] -> BinaryPolynom
polyFromPowers = BinaryPolynom . sortBy (flip compare)

instance Show BinaryPolynom where
    show (BinaryPolynom []) = "0"
    show (BinaryPolynom xs) = intercalate " + " $ map (\x -> if x == 0 then "1" else "x^" ++ show x) xs
    
-- | Multiplication in the polynom ring.
multiply :: BinaryPolynom -> BinaryPolynom -> BinaryPolynom
multiply (BinaryPolynom xs) (BinaryPolynom ys) = BinaryPolynom $
                                                 removeRepeated $
                                                 foldl mergeSortedLists [] [[x + y | x <- xs] | y <- ys]
        
-- | Addition and multiplication in the polynom field.
(.+.), (.*.) :: BinaryPolynom -> BinaryPolynom -> BinaryPolynom
x .+. y =  BinaryPolynom $ removeRepeated $ mergeSortedLists (unwrap x) (unwrap y)
x .*. y = snd $ polyDivMod (x `multiply` y) m

polyDivMod :: BinaryPolynom -> BinaryPolynom -> (BinaryPolynom, BinaryPolynom)
polyDivMod x y = if degDelta < 0 then (BinaryPolynom [], x)
                 else (q' .+. polyFromDeg degDelta, r)
                 where
                    degDelta = deg x - deg y
                    (q', r) = polyDivMod x' y
                    x' = x .+. (y `multiply` polyFromDeg degDelta)

-- | Helper functions
removeRepeated :: Eq a => [a] -> [a]
removeRepeated [] = []
removeRepeated [x] = [x]
removeRepeated (x:y:r) = if x == y then removeRepeated r else x:removeRepeated (y:r)

mergeSortedLists :: Ord a => [a] -> [a] -> [a]
mergeSortedLists xs [] = xs
mergeSortedLists [] ys = ys
mergeSortedLists xs@(x:xs') ys@(y:ys') = if x > y then x:mergeSortedLists xs' ys
                                    else y:mergeSortedLists xs ys'
