module Main where

import Text.Parsers.IRC

loop :: IO ()
loop = do
  line <- putStrLn "Line:" >> getLine
  let parse = parseIrcMsg line
  case parse of
    Left e -> putStrLn $ show e
    Right x -> putStrLn $ show x
  loop

main :: IO ()
main = do
  loop

