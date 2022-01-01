{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Kata.AdditionCommutes
  ( plusCommutes ) where

import Kata.AdditionCommutes.Definitions
  ( Z, S
  , Natural(..), Equal(..)
  , (:+:))

-- These are some lemmas that may be helpful.
-- They will *not* be tested, so rename them
-- if you so desire. Good luck!

-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive (NumS n) = EqlS $ reflexive n

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS eq) = EqlS $ symmetric eq

-- | if a = b and b = c, then a = c.
transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ = EqlZ
transitive (EqlS eq1) (EqlS eq2) = EqlS $ transitive eq1 eq2  

-- This is the proof that the kata requires.
-- | a + b = b + a
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
-- 0 + 0 = 0 + 0
plusCommutes NumZ NumZ = EqlZ 
-- 0 + a = a + 0
plusCommutes NumZ (NumS a) = EqlS $ plusCommutes NumZ a
plusCommutes (NumS a) NumZ = EqlS $ plusCommutes a NumZ
-- S a + S b 
-- = S (a + S b)
-- = S (S b + a)
-- = S (S (b + a))
-- = S (S (a + b))
-- = S (S a + b)
-- = S (b + S a)
-- = S b + S a
plusCommutes (NumS a) (NumS b) = EqlS $ plusCommutes a (NumS b) 
  `transitive` EqlS (plusCommutes b a) 
  `transitive` plusCommutes (NumS a) b

-- For reference, here are the definitions, if you
-- want to copy them into an IDE:
{-

-- | The natural numbers, encoded in types.
data Z
data S n

-- | Predicate describing natural numbers.
-- | This allows us to reason with `Nat`s.
data Natural :: * -> * where
    NumZ :: Natural Z
    NumS :: Natural n -> Natural (S n)

-- | Predicate describing equality of natural numbers.
data Equal :: * -> * -> * where
    EqlZ :: Equal Z Z
    EqlS :: Equal n m -> Equal (S n) (S m)

-- | Peano definition of addition.
type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

-}