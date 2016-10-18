module Parser (parse) where

import Tokenizer
import Type
import qualified Data.Tuple

parse :: (Monad m) => [Token] -> m Expression
parse ts = scan ts >>= return . Data.Tuple.fst

scan :: (Monad m) => [Token] -> m (Expression, [Token])
scan [] = fail "no token"
scan (TInt i : xs) = return (EInt i, xs)
scan (TSymbol s : xs) = return (EVar s, xs)
scan (TLPar : xs) = do
    (es, ts) <- scanList xs
    return (EList es, ts)
scan (TRPar : xs) = fail "unexpected )"

scanList :: (Monad m) => [Token] -> m ([Expression], [Token])
scanList [] = fail "unexpected EOF before )"
scanList (TRPar : xs) = return ([], xs)
scanList t@(x : xs) = do
    (e, t1) <- scan t
    (es, t2) <- scanList t1
    return (e : es, t2)
