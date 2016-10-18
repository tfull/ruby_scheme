module Tokenizer (Token (..), tokenize) where

import Data.Char as C

data Token = TInt Int
    | TLPar
    | TRPar
    | TSymbol String
    deriving (Show, Eq)

data State = SPlain
    | SInt
    | SSymbol

tokenize :: (Monad m) => String -> m [Token]
tokenize = scan SPlain []

scan :: (Monad m) => State -> String -> String -> m [Token]
scan SPlain _ [] = return []
scan SPlain _ (c : cs)
    | C.isDigit c = scan SInt [c] cs
    | C.isSpace c = scan SPlain [] cs
    | c == '(' = scan SPlain [] cs >>= (\ts -> return $ TLPar : ts)
    | c == ')' = scan SPlain [] cs >>= (\ts -> return $ TRPar : ts)
    | otherwise = scan SSymbol [c] cs
scan SInt store [] = return [TInt $ read store]
scan SInt store (c : cs)
    | C.isDigit c = scan SInt (store ++ [c]) cs
    | C.isSpace c = scan SPlain [] cs >>= (\ts -> return $ TInt (read store) : ts)
    | c == '(' = scan SPlain [] cs >>= (\ts -> return $ TInt (read store) : TLPar : ts)
    | c == ')' = scan SPlain [] cs >>= (\ts -> return $ TInt (read store) : TRPar : ts)
    | otherwise = scan SSymbol (store ++ [c]) cs
scan SSymbol store [] = return [TSymbol store]
scan SSymbol store (c : cs)
    | C.isSpace c = scan SPlain [] cs >>= (\ts -> return $ TSymbol store : ts)
    | c == '(' = scan SPlain [] cs >>= (\ts -> return $ TSymbol store : TLPar : ts)
    | c == ')' = scan SPlain [] cs >>= (\ts -> return $ TSymbol store : TRPar : ts)
    | otherwise = scan SSymbol (store ++ [c]) cs
