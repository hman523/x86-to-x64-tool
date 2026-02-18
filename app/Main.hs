module Main where

import qualified X86_to_X64 (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  X86_to_X64.someFunc
