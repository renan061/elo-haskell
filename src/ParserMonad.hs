module ParserMonad where

import Control.Monad (ap, liftM, (>=>))
import Data.List (uncons)
import Control.Applicative (Alternative, empty, (<|>))

newtype Parser a = Parser (String -> Maybe (a, String))

apply :: Parser a -> String -> Maybe (a, String)
apply (Parser p) = p

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  p >>= f = Parser $ apply p >=> (\(a, s) -> apply (f a) s)
  p1 >> p2 = p1 >>= const p2

instance Alternative Parser where
  empty = failure
  Parser f <|> Parser g = Parser $ \s -> f s <|> g s

-- basic parsers

failure :: Parser a
failure = Parser (const Nothing)

many0 :: Parser a -> Parser [a]
many0 p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p =
  do x <- p
     xs <- many0 p
     return (x : xs)

item :: Parser Char
item = Parser uncons

satisfies :: (Char -> Bool) -> Parser Char
satisfies f = do
  c <- item
  if f c
    then return c
    else failure
