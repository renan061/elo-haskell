module Scanner where

import Data.Char (isAlphaNum, isDigit, isLower, isUpper, isSpace, ord)
import Control.Applicative ((<|>))

import ParserMonad

char :: Char -> Parser Char
char c = satisfies (== c)

match :: String -> Parser ()
match [] = return ()
match (x : xs) =
  do _ <- char x
     match xs
     return ()

digit :: Parser Int
digit = do
  c <- satisfies isDigit
  return (charToDigit c)
  where charToDigit c = ord c - ord '0'

space :: Parser Char
space = satisfies isSpace

spaces :: Parser ()
spaces =
  do _ <- many0 space
     _ <- comment <|> many0 space
     return ()

comment :: Parser String
comment =
  do _ <- char '#'
     _ <- many0 (satisfies (/= '\n'))
     many0 space

lower :: Parser Char
lower = satisfies isLower

upper :: Parser Char
upper = satisfies isUpper

-- tokens

data Token
  = Nil
  | Num Int
  | Id String


nil :: Parser Token
nil = do match "nil"
         spaces
         return Nil

num :: Parser Token
num = do n <- f 0
         spaces
         return n
  where f acc = do { c <- digit
                   ; let x = acc * 10 + c in
                     f x <|> return (Num x)
                   }

id :: Parser Token
id = do x <- lower
        xs <- many0 (satisfies isAlphaNum)
        spaces
        return (Id (x : xs))
