module Parser where

import MParserCore
import Data.Char

mapP :: (a -> b) -> Parser a -> Parser b
mapP = fmap

liftP :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
liftP f p q = p ++> \x -> q ++> \y -> yield (f x y) 

infixr 4 +++ 

(+++) :: Parser a -> Parser b -> Parser b
p +++ q = p ++> const q

sat :: (Char -> Bool) -> Parser Char
sat p = item ++> \x -> if p x then yield x else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser ()
char x = sat (== x) +++ yield ()

many :: Parser a -> Parser [a]
many p = many1 p ||| yield []

many1 :: Parser a -> Parser [a]
many1 p = liftP (:) p (many p)
