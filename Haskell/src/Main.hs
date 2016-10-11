module Main where

import Tokenizer as T

main :: IO ()
main = do
    line <- getLine
    tokens <- T.tokenize line
    putStrLn $ show tokens
