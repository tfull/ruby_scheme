module Evaluator (evaluate, firstEnvironment) where

import Type
import qualified Data.List

evaluate :: Monad m => Environment -> Expression -> m (Environment, Value)
evaluate env (EInt i) = return (env, VInt i)
evaluate env (EVar v) = findValue v env >>= (\val -> return (env, val))
evaluate env (EList []) = return (env, VNull)
evaluate env (EList (x:xs)) = do
    (e1, v1) <- evaluate env x
    evaluateList v1 env xs

evaluateList :: Monad m => Value -> Environment -> [Expression] -> m (Environment, Value)
evaluateList (VSyntax syntax) env exps = case Data.List.lookup syntax syntaxList of
    Just f -> f env exps
    Nothing -> fail "not implemented syntax"
evaluateList (VSubroutine subroutine) env exps = case Data.List.lookup subroutine subroutineList of
    Just f -> do
        (e0, v0) <- evaluateSome env exps
        v <- f v0
        return (e0, v)
    Nothing -> fail "not implemented subroutine"
evaluateList _ _ _ = fail "not applicable"

evaluateSome :: Monad m => Environment -> [Expression] -> m (Environment, [Value])
evaluateSome env [] = return (env, [])
evaluateSome env (e:es) = do
    (e0, v0) <- evaluate env e
    (e1, v1) <- evaluateSome e0 es
    return (e1, v0 : v1)

findValue :: Monad m => String -> Environment -> m Value
findValue s [] = fail $ "no such variable " ++ s
findValue s ([] : xs) = findValue s xs
findValue s (((k, v) : kvs) : xs)
    | s == k = return v
    | otherwise = findValue s (kvs : xs)

syntaxDefine :: Monad m => Environment -> [Expression] -> m (Environment, Value)
syntaxDefine env (EVar v:e:_) = do
    (envh:envt, value) <- evaluate env e
    return (((v, value) : envh) : envt, VDefined v)
syntaxDefine _ _ = fail "error: define"

subroutinePlus :: Monad m => [Value] -> m Value
subroutinePlus [] = return $ VInt 0
subroutinePlus (VInt i : vs) = do
    y <- subroutinePlus vs
    case y of
        VInt j -> return $ VInt $ i + j
        _ -> fail "not a number"

syntaxList :: Monad m => [(String, Environment -> [Expression] -> m (Environment, Value))]
syntaxList = [("define", syntaxDefine)]

subroutineList :: Monad m => [(String, [Value] -> m Value)]
subroutineList = [("+", subroutinePlus)]

firstEnvironment :: Environment
firstEnvironment = [[ ("define", VSyntax "define"), ("+", VSubroutine "+") ]]
