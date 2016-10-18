module Main where

import Tokenizer as T
import Parser as P
import Evaluator as E
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
main = do
    string <- getBlock
    tokens <- T.tokenize string
    putStrLn $ show tokens
    expression <- P.parse tokens
    putStrLn $ show expression
    value <- E.evaluate expression
    putStrLn $ show value
    main
