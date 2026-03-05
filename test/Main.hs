module Main where

import Test.Hspec
import Parser.ParserTests (parserSpec)
import Analysis.AnalysisTests (analysisSpec)

main :: IO ()
main = hspec $ do
  parserSpec
  analysisSpec