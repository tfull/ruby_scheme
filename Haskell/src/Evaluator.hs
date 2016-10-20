module Evaluator (evaluate, firstEnvironment) where

import Type

evaluate :: Monad m => Environment -> Expression -> m (Environment, Value)
evaluate env (EInt i) = return (env, VInt i)
evaluate env (EVar v) = findValue v env >>= (\val -> return (env, val))
evaluate env (EList []) = return (env, VNull)
evaluate env (EList (x:xs)) = do
    (e1, v1) <- evaluate env x
    evaluateList v1 env xs

evaluateList :: Monad m => Value -> Environment -> [Expression] -> m (Environment, Value)
evaluateList (VSyntax syntax) env exps
    | syntax == "define" = syntaxDefine env exps
    | otherwise = fail "not implemented syntax"

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

firstEnvironment :: Environment
firstEnvironment = [[ ("define", VSyntax "define") ]]
