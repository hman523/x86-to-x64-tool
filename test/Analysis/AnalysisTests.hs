module Analysis.AnalysisTests where

import Test.Hspec
import Test.HUnit
import qualified Data.ByteString.Char8 as BS
import Language.C

import Analysis.TypeSizeTests (typeSizeSpec)
import Analysis.TypeCheckerTests (typecheckerSpec)

analysisSpec :: Spec
analysisSpec = describe "Analysis Tests" $ do
  typeSizeSpec
  typecheckerSpec
