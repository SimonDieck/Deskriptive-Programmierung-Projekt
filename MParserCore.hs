module MParserCore (Parser, parse, item, (|||), failure, (++>), yield, 
                    module Control.Applicative) where

import qualified LParserCore as ParserCore
import Control.Applicative hiding (Alternative(..), optional)

newtype Parser a = P {unP :: ParserCore.Parser a}

parse :: Parser a -> String -> [a]
parse = ParserCore.parse . unP

item :: Parser Char
item = P ParserCore.item

infixr 3 |||

(|||) :: Parser a -> Parser a -> Parser a
(P p) ||| (P q) = P (p ParserCore.||| q)

failure :: Parser a
failure = P ParserCore.failure

infixr 4 ++>

(++>) :: Parser a -> (a -> Parser b) -> Parser b
(P p) ++> f = P (p ParserCore.++> (unP . f))

yield :: a -> Parser a
yield = P . ParserCore.yield

instance Functor Parser where
  fmap f p = p ++> \x -> yield (f x)

instance Applicative Parser where
  pure = yield
  p <*> q = p ++> \x -> fmap x q

instance Monad Parser where
  return = yield
  (>>=)  = (++>)
  fail _ = failure
