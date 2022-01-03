module Parser (parse, Result(Ok, Err)) where

import Control.Monad (ap, liftM)
import Data.Char (isAlphaNum, isDigit, isLower, isUpper, isSpace, ord)

import Term

-- parser monad

newtype Parser a = Parser (String -> Maybe (a, String))

apply :: Parser a -> String -> Maybe (a, String)
apply (Parser p) = p

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  p >>= f = Parser $ \s ->
    (apply p s) >>= (\(a, s') -> apply (f a) s')

-- basic parsers

failure :: Parser a
failure = Parser (const Nothing)

item :: Parser Char
item = Parser f
  where
    f [] = Nothing
    f (x : xs) = Just (x, xs)

(\/) :: Parser a -> Parser a -> Parser a
(\/) (Parser f) (Parser g) = Parser $ \s ->
  case f s of
    Nothing -> g s
    x -> x

infixl 2 \/

satisfies :: (Char -> Bool) -> Parser Char
satisfies f = do c <- item
                 if f c
                  then return c
                  else failure

digit :: Parser Int
digit = do c <- satisfies isDigit
           return (charToDigit c)
  where charToDigit c = ord c - ord '0'

number :: Parser Int
number = f 0
  where f acc = do c <- digit
                   let x = acc * 10 + c in f x \/ return x

many0 :: Parser a -> Parser [a]
many0 p = many1 p \/ return []

many1 :: Parser a -> Parser [a]
many1 p = do x <- p
             xs <- many0 p
             return (x : xs)

char :: Char -> Parser Char
char c = satisfies (== c)

string :: String -> Parser ()
string [] = return ()
string (x : xs) = do { _ <- char x ; string xs }

space :: Parser Char
space = satisfies isSpace

token :: Parser a -> Parser a
token t = do x <- t
             _ <- space
             return x

symbol :: String -> Parser ()
symbol = token . string

comment :: Parser String
comment = do _ <- char '#'
             _ <- many0 (satisfies (/= '\n'))
             many0 space

spaces :: Parser String
spaces = do many0 space
            comment \/ many0 space

lower :: Parser Char
lower = satisfies isLower

upper :: Parser Char
upper = satisfies isUpper

-- elo parsers

termNil :: Parser Term
termNil = do symbol "nil"
             return TermNil

termNum :: Parser Term
termNum = do n <- number
             return (TermNum n)

termId :: Parser Term
termId = do x <- lower
            xs <- many1 (satisfies isAlphaNum)
            return (TermId (x : xs))

termAsg :: Parser Term
termAsg = do lhs <- term
             spaces
             symbol "="
             spaces
             rhs <- term
             return (TermAsg lhs rhs)

termSeq :: Parser Term
termSeq = do t1 <- stmt
             spaces
             symbol ";"
             spaces
             t2 <- stmt
             return (TermSeq t1 t2)

termSpawn :: Parser Term
termSpawn = do symbol "spawn"
               spaces
               block <- term
               return (TermSpawn block)

exp :: Parser Term
exp = termNil \/ termNum \/ termId

stmt :: Parser Term
stmt = termSeq \/ termAsg \/ termSpawn

term :: Parser Term
term = do t <- stmt
          return t

data Result
  = Ok Term
  | Err String

-- main function

parse :: String -> Result
parse [] = Err "input is empty"
parse s = case apply term s of
  Nothing -> Err "could not parse"
  Just (t, []) -> Ok t
  Just (t, err) -> Err err
