{-# LANGUAGE RankNTypes #-}

module Haskell.Codewars.Church where

import Prelude hiding (succ)

newtype Number = Nr (forall a. (a -> a) -> a -> a)

zero :: Number
zero = Nr (\ _ z -> z)

succ :: Number -> Number
succ (Nr a) = Nr (\ s z -> s (a s z))

one :: Number
one = succ zero

add :: Number -> Number -> Number
add (Nr a) = a succ

mult :: Number -> Number -> Number
mult (Nr a) b = a (add b) zero

pow :: Number -> Number -> Number
pow x (Nr n) = n (mult x) one
