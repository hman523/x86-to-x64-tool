module Main where

import Test.Hspec
import Parser.ParserTests (parserSpec)

main :: IO ()
main = hspec $ do
  parserSpec