module Main where

import Test.Hspec
import Parser.ParserTests (parserSpec)
import Analysis.AnalysisTests (analysisSpec)
import Linter.LinterTests (linterSpec)
import Transformer.TransformerTests (transformerSpec)
import Transformer.IntegrationTests (transformerIntegrationSpec)
import CrossArch.CrossArchTests (crossArchSpec)

main :: IO ()
main = hspec $ do
  parserSpec
  analysisSpec
  linterSpec
  transformerSpec
  transformerIntegrationSpec
  crossArchSpec