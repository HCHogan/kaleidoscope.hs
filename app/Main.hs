module Main (main) where

import KaleidoscopeHs.Lexer (addInts)

main :: IO ()
main = do
  let a = 1
  let b = 2
  result <- addInts a b
  putStrLn $ "Result: " ++ show result
