module Main where

import Result
import Type
import qualified Tokenizer as T
import qualified Parser as P
import qualified Evaluator as E
import qualified Data.List

getBlock :: IO String
getBlock = do
    line <- getLine
    if Data.List.length line > 0
        then do
            b <- getBlock
            return $ line ++ "\n" ++ b
        else return ""

main :: IO ()
main = loop E.firstEnvironment

loop :: Environment -> IO ()
loop env = do
    string <- getBlock
    case T.tokenize string >>= P.parse >>= E.evaluate env of
        Reject s -> putStrLn ("Error: " ++ s) >> loop env
        Accept (e, v) -> putStrLn (show v) >> loop e
