module Type where

import qualified Data.Ratio
import qualified Data.Complex

data SchemeNumber = SchemeInt Integer
    | SchemeFloat Double
    | SchemeRational Data.Ratio.Rational Integer
    | SchemeComplex Data.Complex.Complex Double
    deriving Show

data Value = VNumber SchemeNumber
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

instance Num SchemeNumber where
    (SchemeInt x0) + (SchemeInt x1) = SchemeInt $ x0 + x1
    (SchemeFloat x) + (SchemeFloat y) = SchemeFloat $ x + y
    (SchemeRational x0) + (SchemeRational x1) = SchemeRational $ x0 + x1
    (SchemeComplex x0) + (SchemeComplex x0) = SchemeComplex $ x0 + x1
    (SchemeInt x0) - (SchemeInt x1) = SchemeInt $ x0 - x1
