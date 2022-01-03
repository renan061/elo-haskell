module Term where

data Typ
 -- primitive types
 = TypVoid
 | TypNum
 | TypArr Typ
 | TypIArr Typ
  -- internal types
 | TypRef Typ
 | TypIRef Typ
 | TypFun Typ Typ

data Term
  -- expressions
  = TermNil
  | TermNum Int
  | TermArrNew Typ Term
  | TermArrIdx Term Term
  | TermId String
  -- statements
  | TermAsg Term Term
  | TermArrAsg Term Term Term
  | TermCall Term Term
  | TermSeq Term Term
  | TermSpawn Term
  -- definitions
  | TermLetVal String Typ Term Term
  | TermLetVar String Typ Term Term
  | TermLetFun String Typ Term Term
  -- internal terms
  | TermLoc Int
  | TermArr Typ Int
  | TermLoad Term
  | TermFun String Typ Term Typ

instance Show Term where
  show TermNil = "nil"
  show (TermNum n) = show n
  show (TermId s) = s
  show (TermAsg lhs rhs) = show lhs ++ " = " ++ show rhs
  show (TermSeq t1 t2) = show t1 ++ "; " ++ show t2
  show (TermSpawn block) = "spawn {" ++ show block ++ "}"
  show _ = "TODO"
