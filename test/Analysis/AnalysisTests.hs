module Analysis.AnalysisTests where

import Test.Hspec
import Test.HUnit
import qualified Data.ByteString.Char8 as BS
import Language.C

import Analysis.TypeSizeTests (typeSizeSpec)

analysisSpec :: Spec
analysisSpec = do
  typeSizeSpec
