module Type where

data Value = VInt Int
    | VSyntax String
    deriving Show

data Expression = EInt Int
    | EList [Expression]
    | EVar String
    deriving Show

type Environment = [[(String, Value)]]
