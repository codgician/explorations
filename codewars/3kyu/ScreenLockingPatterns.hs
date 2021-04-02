module ScreenLockingPatterns.Solution where

-- import ScreenLockingPatterns.Shared
import Data.List ((\\))

-- module ScreenLockingPatterns.Shared where
data Vertex
  = A | B | C | D | E | F | G | H | I
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

diag :: Vertex -> Vertex -> Maybe Vertex
diag x y 
  | isValid x y = Just . toEnum . (`div` 2) $ fromEnum x + fromEnum y
  | otherwise = Nothing
  where
    row = (`div` 3) . fromEnum
    col = (`mod` 3) . fromEnum
    isValid x y = even (row x + row y) && even (col x + col y)

trans :: [Vertex] -> Vertex -> [Vertex]
trans vis cur = filter checker nexts
  where 
    nexts = [A ..] \\ vis
    checker x = case diag x cur of
              Just y  -> y `notElem` nexts
              Nothing -> True

dfs :: Int -> Int -> [Vertex] -> Vertex -> Int
dfs step lim vis cur
  | step > lim  = 0
  | step == lim = 1
  | otherwise   = (sum . map (dfs (step + 1) lim (cur:vis))) $ trans vis cur 

countPatternsFrom :: Vertex -> Int -> Int
countPatternsFrom startPt lim 
  | lim > 9   = 0
  | otherwise = dfs 1 lim [startPt] startPt
