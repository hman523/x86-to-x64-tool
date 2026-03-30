module Main where

import Test.Hspec
import Parser.ParserTests (parserSpec)
import Analysis.AnalysisTests (analysisSpec)
import Transformation.TransformationTests (transformationSpec)

main :: IO ()
main = hspec $ do
  parserSpec
  analysisSpec
  transformationSpec