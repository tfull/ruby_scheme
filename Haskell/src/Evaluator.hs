module Evaluator where

import Type

evaluate :: (Monad m) => Expression -> m Value
evaluate (EInt i) = return $ VInt i
evaluate _ = undefined
