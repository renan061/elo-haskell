module Main where

import Parser

main :: IO ()
main = case parse "a = 32" of
  Err err -> putStrLn err
  Ok t -> print t
