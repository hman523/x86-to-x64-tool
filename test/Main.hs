module Main where

import Test.Hspec
import Parser.ParserTests (parserSpec)
import Parser.FormatSpecParserTests (formatSpecParserSpec)
import Analysis.AnalysisTests (analysisSpec)
import Linter.LinterTests (linterSpec)
import Transformer.TransformerTests (transformerSpec)
import Transformer.IntegrationTests (transformerIntegrationSpec)
import Transformer.UsageClassifierTests (usageClassifierSpec)
import CrossArch.CrossArchTests (crossArchSpec)

main :: IO ()
main = hspec $ do
  parserSpec
  formatSpecParserSpec
  analysisSpec
  linterSpec
  transformerSpec
  transformerIntegrationSpec
  usageClassifierSpec
  crossArchSpec