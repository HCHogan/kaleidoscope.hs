module KaleidoscopeHs.Lexer (addInts) where

import KaleidoscopeHs (add)

import Foreign.C.Types

addInts :: Int -> Int -> IO Int
addInts x y = do
    let x' = fromIntegral x :: CLong
    let y' = fromIntegral y :: CLong
    result <- add x' y'
    return (fromIntegral result)
