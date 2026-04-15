module Linter.LinterTests where

import Test.Hspec
import Linter.AlignmentTests
import Linter.BitManipulationTests
import Linter.ComparisonTests
import Linter.ConstantsLiteralsTests
import Linter.FormatStringsTests
import Linter.FunctionSignaturesTests
import Linter.MemoryAllocationTests
import Linter.PlatformSpecificsTests
import Linter.PointerMathTests
import Linter.SerializationTests
import Linter.TypeSizeTests
import Linter.IntegrationTests

linterSpec :: Spec
linterSpec = describe "Linter Tests" $ do
  alignmentLintSpec
  bitManipulationLintSpec
  comparisonLintSpec
  constantsLiteralsLintSpec
  formatStringsLintSpec
  functionSignaturesLintSpec
  memoryAllocationLintSpec
  platformSpecificsLintSpec
  pointerMathLintSpec
  serializationLintSpec
  typeSizeLintSpec
  linterIntegrationSpec