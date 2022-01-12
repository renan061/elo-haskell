module Parser (parse, Result(Ok, Err)) where

import ParserMonad
import Data.Char (isAlphaNum, isDigit, isLower, isUpper, isSpace, ord)

import Term

-- elo parsers

termNil :: Parser Term
termNil = do symbol "nil"
             return TermNil

termNum :: Parser Term
termNum = do n <- number
             return (TermNum n)

termId :: Parser Term
termId = do x <- lower
            xs <- many0 (satisfies isAlphaNum)
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
