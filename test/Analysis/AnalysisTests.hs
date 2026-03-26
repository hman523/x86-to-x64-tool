module Analysis.AnalysisTests where

import Test.Hspec
import Test.HUnit
import qualified Data.ByteString.Char8 as BS
import Language.C

import Analysis.TypeSizeTests (typeSizeSpec)
import Analysis.TypeCheckerTests (typecheckerSpec)
import Analysis.FunctionSignaturesTests (functionSignaturesSpec)
import Analysis.PointerMathTests (pointerMathSpec)
import Analysis.MemoryAllocationTests (memoryAllocationSpec)
import Analysis.PlatformSpecificsTests (platformSpecificsSpec)
import Analysis.BitManipulationTests (bitManipulationSpec)
import Analysis.ComparisonTests (comparisonSpec)
import Analysis.FormatStringsTests (formatStringsSpec)
import Analysis.ConstantsLiteralsTests (constantsLiteralsSpec)
import Analysis.AlignmentTests (alignmentSpec)
import Analysis.SerializationTests (serializationSpec)

analysisSpec :: Spec
analysisSpec = describe "Analysis Tests" $ do
  typeSizeSpec
  typecheckerSpec
  functionSignaturesSpec
  pointerMathSpec
  memoryAllocationSpec
  platformSpecificsSpec
  bitManipulationSpec
  comparisonSpec
  formatStringsSpec
  constantsLiteralsSpec
  alignmentSpec
  serializationSpec
