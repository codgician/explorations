{-# LANGUAGE TypeOperators, TypeFamilies, GADTs, UndecidableInstances #-}

module Kata.TimesComm where

import Kata.TimesComm.Definitions

{- Preloaded code. Maybe helpful for local editing.

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

-- | Peano definition of multiplication.
type family (:*:) (n :: *) (m :: *) :: *
type instance Z :*: m = Z
type instance S n :*: m = m :+: (n :*: m)

-}
-- | Peano definition of addition.
(|+|) :: Natural a -> Natural b -> Natural (a :+: b)
(|+|) NumZ a = a
(|+|) (NumS a) b = NumS (a |+| b)

-- | Peano definition of multiplication.
(|*|) :: Natural a -> Natural b -> Natural (a :*: b)
(|*|) NumZ _ = NumZ
(|*|) (NumS a) b = b |+| (a |*| b) 

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

-- This will be helpful
plus' :: Equal n1 m1 -> Equal n2 m2 -> Equal (n1 :+: n2) (m1 :+: m2)
-- 0 + 0 = 0 + 0
plus' EqlZ EqlZ = EqlZ
-- 0 + eq = 0 + eq
plus' EqlZ (EqlS eq) = EqlS $ plus' EqlZ eq
-- eq1 + eq2 = eq1 + eq2
plus' (EqlS eq1) eq2 = EqlS $ plus' eq1 eq2

-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-plus-c-equals-a-plus-b-plus-c-prove-it/haskell
plusAssoc :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
plusAssoc NumZ NumZ c  = reflexive c 
plusAssoc NumZ (NumS b) c = EqlS $ plusAssoc NumZ b c
plusAssoc (NumS a) b c = EqlS $ plusAssoc a b c

-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-equals-b-plus-a-prove-it/haskell
plusComm :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
-- 0 + 0 = 0 + 0
plusComm NumZ NumZ = EqlZ 
-- 0 + a = a + 0
plusComm NumZ (NumS a) = EqlS $ plusComm NumZ a
plusComm a NumZ = symmetric $ plusComm NumZ a
-- S a + S b = S b + S a
plusComm (NumS a) (NumS b) = EqlS $ plusComm a (NumS b) 
  `transitive` EqlS (plusComm b a) 
  `transitive` plusComm (NumS a) b

-- This will also be helpful
zeroComm :: Natural a -> Equal Z (a :*: Z)
zeroComm NumZ = EqlZ
zeroComm (NumS a) = zeroComm a

-- This is the proof that the kata requires.
-- | a * b = b * a
timesComm :: Natural a -> Natural b -> Equal (a :*: b) (b :*: a)
-- 0 * 0 = 0 * 0
timesComm NumZ NumZ = EqlZ
-- 0 * a = a * 0
timesComm NumZ (NumS a) = zeroComm a
timesComm a NumZ = symmetric $ timesComm NumZ a
-- S a * S b = S b * S a
timesComm (NumS a) (NumS b) = (reflexive (NumS b) `plus'` timesComm a (NumS b))  -- S a * S b <=> S b + (a * S b) = S b + (S b * a)
  `transitive` plusAssoc (NumS b) a (b |*| a) -- S b + (S b * a) <=> S b + (a + (b * a)) = (S b + a) + (b * a) 
  `transitive` (EqlS (plusComm b a) `plus'` timesComm b a)  -- (S b + a) + (b * a) = (S a + b) + (a * b)
  `transitive` symmetric (plusAssoc (NumS a) b (a |*| b)) -- (S a + b) + (a * b) = S a + (b + (a * b))
  `transitive` (reflexive (NumS a) `plus'` (NumS a `timesComm` b)) -- S a + (b + (a * b)) <=> S a + (S a * b) = S a + (b * S a) <=> S b * S a