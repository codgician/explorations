module Huffman
    ( frequencies
    , encode
    , decode
    , Bit (..)
    ) where

import Data.Bifunctor
import Data.List ( insertBy, sort, sortOn )
import Data.Ord
import Data.Maybe
import qualified Data.Map as M

data Bit        = Z | O deriving (Eq, Show)
data TreeNode a = Leaf a Int
                | Fork (TreeNode a) (TreeNode a) Int
                deriving (Show)
type CodeMap a  = M.Map a [Bit]

-- | Calculate symbol frequencies of a text.
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = sortOn snd . foldl work [] . sort 
  where
    work [] x             = [(x, 1)]
    work (p@(v, c):ps) x  = if v == x
                            then (x, c + 1):ps
                            else (x, 1):p:ps

-- | Build Huffman Tree
buildTree :: Ord a => [(a, Int)] -> Maybe (TreeNode a)
buildTree [] = Nothing
buildTree freq = Just (work $ map (uncurry Leaf) freq)
  where
    work [t]            = t
    work (x:y:ts)       = work $ insertBy (comparing value) (merge x y) ts
    merge t1 t2         = Fork t1 t2 (value t1 + value t2)
    value (Leaf _ x)    = x
    value (Fork _ _ x)  = x

buildCodeMap :: Ord a => Maybe (TreeNode a) -> CodeMap a
buildCodeMap Nothing = M.empty 
buildCodeMap (Just t) = (M.fromList . buildCodeList) t
  where
    buildCodeList (Leaf a _)    = [(a, [])]
    buildCodeList (Fork l r _)  = map (second (Z:)) (buildCodeList l) 
                                ++ map (second (O:)) (buildCodeList r)

-- | Encode a sequence using the given frequencies.
encode :: Ord a => [(a, Int)] -> [a] -> Maybe [Bit]
encode [] _   = Nothing
encode [_] _  = Nothing
encode _ []   = Just []
encode freq xs = Just $ concatMap (codeMap M.!) xs
  where 
    codeMap = (buildCodeMap . buildTree) freq

-- | Decode a bit sequence using the given frequencies.
decode :: Ord a => [(a, Int)] -> [Bit] -> Maybe [a]
decode [] _     = Nothing
decode [_] _    = Nothing
decode _ []     = Just []
decode freq xs  = Just $ work tree xs
  where
    tree = fromJust $ buildTree freq
    work (Leaf a _) []        = [a]
    work (Leaf a _) xs        = a:work tree xs
    work (Fork l r _) (Z:xs)  = work l xs
    work (Fork l r _) (O:xs)  = work r xs
