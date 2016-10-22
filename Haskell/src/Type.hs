module Type where

data Value = VInt Int
    | VNil
    | VSyntax String
    | VSubroutine String
    | VSymbol String
    | VCons Value Value
    deriving Show

data Expression = EInt Int
    | EList [Expression]
    | EVar String
    deriving Show

type Environment = [[(String, Value)]]

valueToString :: Value -> String
valueToString (VInt i) = show i
valueToString VNil = "()"
valueToString (VSyntax s) = "#<syntax " ++ s ++ ">"
valueToString (VSubroutine s) = "#<subroutine " ++ s ++ ">"
valueToString (VSymbol s) = s
valueToString (VCons v0 v1) = "(" ++ sub v0 v1 ++ ")"
    where
        sub :: Value -> Value -> String
        sub v0 VNil = valueToString v0
        sub v0 (VCons v1 v2) = valueToString v0 ++ " " ++ sub v1 v2
        sub v0 v1 = valueToString v0 ++ " . " ++ valueToString v1
