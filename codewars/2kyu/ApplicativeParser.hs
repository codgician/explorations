{-# LANGUAGE LambdaCase #-}

module ApplicativeParser where

import Data.Char
import Prelude hiding (fmap)

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = P (\inp -> (\(out, v) -> (out, f v)) `map` unP p inp)

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
pf <@> px = P (\inp -> concat [unP (f <#> px) out | (out, f) <- unP pf inp])

(<@) :: Parser a -> Parser b -> Parser a
px <@ py = (const <#> px) <@> py
         
(@>) :: Parser a -> Parser b -> Parser b
px @> py = (flip const <#> px) <@> py

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
(<<>>) px py = P (\inp -> unP px inp  ++ unP py inp)

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
cnst = ConstE <#> int

binOpExpr :: Parser Expr
binOpExpr = flip BinOpE <#> (charP '(' @> expr <@ space) <@> binOp <@> (space @> expr <@ charP ')')
  where space = predP isSpace

binOp :: Parser BinOp
binOp = AddBO <# charP '+'
  <<>> MulBO <# charP '*'

neg :: Parser Expr
neg = NegE <#> (charP '-' @> expr)

zero :: Parser Expr
zero = ZeroE <# charP 'z'

int :: Parser Int
int = read <#> some digit

digit :: Parser Char
digit = predP isDigit
