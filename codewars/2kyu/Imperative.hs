module Imperative (
  def, var, lit, while, (+=), (-=), (*=)
) where

import Control.Monad.State
import qualified Data.Map as M

type Index = Int              -- Variable index
type Env = M.Map Int Integer  -- Global variable map
type Expr = State Env         -- State monad for expressions

newtype Literal = Literal Integer   -- Literal
newtype Variable = Variable Index   -- Variable

class Value a where
  eval :: Env -> a -> Integer

instance Value Literal where
  eval _ (Literal x)  = x

instance Value Variable where
  eval env (Variable i) = env M.! i

lit :: Integer -> Literal
lit = Literal

var :: Integer -> Expr Variable
var x = do
  env <- get
  let i = length env
  put $ M.insert i x env
  return $ Variable i

def :: Value a => Expr a -> Integer
def m = let (v, env) = runState m M.empty 
        in eval env v

while :: Value a => a -> (Integer -> Bool) -> Expr () -> Expr ()
while x f act = do
  env <- get
  let cond = f $ eval env x
  when cond $ do
    put $ execState act env
    while x f act

opAssign :: Value a => (Integer -> Integer -> Integer) -> Variable -> a -> Expr ()
opAssign op (Variable i) x = do
  env <- get
  let res = op (env M.! i) (eval env x)
  put $ M.insert i res env

(+=), (-=), (*=) :: Value a => Variable -> a -> Expr ()
(+=) = opAssign (+)
(-=) = opAssign (-)
(*=) = opAssign (*)
