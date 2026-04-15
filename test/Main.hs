module Main where

import Test.Hspec
import Parser.ParserTests (parserSpec)
import Analysis.AnalysisTests (analysisSpec)
import Linter.LinterTests (linterSpec)

main :: IO ()
main = hspec $ do
  parserSpec
  analysisSpec
  linterSpec