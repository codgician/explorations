{-# LANGUAGE TypeOperators, TypeFamilies, GADTs, UndecidableInstances, TemplateHaskell #-}
module InvertAdd where

import InvertAddPreload

{- Imported code:
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

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS eq) = EqlS $ symmetric eq

-- | if a = b and b = c, then a = c.
transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ = EqlZ
transitive (EqlS eq1) (EqlS eq2) = EqlS $ transitive eq1 eq2

-- | a + S b = S a + b
lemma :: Natural a -> Natural b -> Equal (a :+: S b) (S a :+: b)
-- S 0 + 0 = 0 + S 0
lemma NumZ NumZ = EqlS EqlZ
-- 0 + S a = S 0 + a
lemma NumZ (NumS a) = EqlS $ lemma NumZ a
-- a + S b = S a + b
lemma (NumS a) b = EqlS $ lemma a b

-- | if a + a = b + b, then a = b
invert :: Natural a -> Natural b -> Equal (a :+: a) (b :+: b) -> Equal a b
-- 0 + 0 = 0 + 0 => 0 = 0
invert NumZ NumZ EqlZ = EqlZ
-- S a + S a = S b + S b
invert (NumS a) (NumS b) (EqlS eq) = EqlS $ invert a b eq'
  where (EqlS eq') = symmetric (lemma a a) `transitive` eq `transitive` lemma b b
