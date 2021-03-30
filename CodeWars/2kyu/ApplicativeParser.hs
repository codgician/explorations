{-# LANGUAGE LambdaCase #-}

module ApplicativeParser where

import Data.Char
import Prelude hiding (fmap)

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = P (\inp -> case unP p inp of
            []  -> []
            r   -> (\(out, v) -> (out, f v)) `map` r)

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
(<#) = pmap . const

infixl 4 <#>
infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP f = P (\case
          []      -> []
          (x:xs)  -> [(xs, x) | f x])

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP = predP . (==)

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = P (\inp -> [(inp, x)])

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
pf <@> px = P (\inp -> case unP pf inp of
            []  -> []
            rs  -> concat [unP (f <#> px) out | (out, f) <- rs])

(<@) :: Parser a -> Parser b -> Parser a
px <@ py = P (\inp -> case unP px inp of
            []  -> []
            rx  -> concat [f out vx | (out, vx) <- rx]
              where f out vx = case unP py out of
                          []  -> [(out, vx)]
                          ry  -> (\(out', _) -> (out', vx)) `map` ry)
         
(@>) :: Parser a -> Parser b -> Parser b
px @> py = P (\inp -> case unP px inp of
            []  -> unP py inp
            rx  -> concat [unP py out | (out, vx) <- rx])

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP [] = inject []
stringP (x:xs) = inject (:) <@> charP x <@> stringP xs

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P $ const []

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
(<<>>) px py = P (\inp -> case unP px inp of
            []  -> unP py inp
            rx -> rx ++ unP py inp)

infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = some p <<>> inject []

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = inject (:) <@> p <@> many p

-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = snd `map` filter (\(x, _) -> x == "") (unP p cs)

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs = case runParser p cs of
                      [x] -> Just x
                      _   -> Nothing

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
          | BinOpE BinOp Expr Expr
          | NegE Expr
          | ZeroE
          deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr (ConstE x) = x
evalExpr (BinOpE AddBO ea eb) = evalExpr ea + evalExpr eb
evalExpr (BinOpE MulBO ea eb) = evalExpr ea * evalExpr eb
evalExpr (NegE e) = -evalExpr e
evalExpr _ = 0

-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
--
parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique expr

expr :: Parser Expr
expr = cnst <<>> binOpExpr <<>> neg <<>> zero

cnst :: Parser Expr
cnst = inject ConstE <@> int

binOpExpr :: Parser Expr
binOpExpr = inject f <@> charP '(' <@> expr <@> space <@> binOp <@> space <@> expr <@> charP ')'
  where 
    f _ ex _ op _ ey _ = BinOpE op ex ey 
    space = predP isSpace

binOp :: Parser BinOp
binOp = inject (const AddBO) <@> charP '+' 
  <<>> inject (const MulBO) <@> charP '*'

neg :: Parser Expr
neg = inject (const NegE) <@> charP '-' <@> expr

zero :: Parser Expr
zero = const ZeroE <#> charP 'z'

int :: Parser Int
int = inject read <@> some digit

digit :: Parser Char
digit = predP isDigit
