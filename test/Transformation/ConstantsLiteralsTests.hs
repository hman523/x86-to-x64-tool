module Transformation.ConstantsLiteralsTests where

import Test.Hspec
import Transformation.TransformationTestsUtils
import Transformation.ConstantsLiterals
import Analysis.ConstantsLiterals (analyzeConstantsLiteralsIssues)
import Analysis.IssueTypes

constantsLiteralsTransformSpec :: Spec
constantsLiteralsTransformSpec = describe "ConstantsLiterals Transformations" $ do
  testTransformMagicValuesUsed
  testTransformBitMaskingAssuming32bitPts
  testTransformHardCodedAddressValues
  testTransformConstantsUsedForSizeCalcs

testTransformMagicValuesUsed :: Spec
testTransformMagicValuesUsed =
  describe "transformMagicValuesUsed" $ do
    shouldLeaveUnresolved "leaves MagicValuesUsed unresolved"
      "void f() { void *p = malloc(4); }"
      analyzeConstantsLiteralsIssues
      transformConstantsLiteralsIssues
      [MagicValuesUsed]

testTransformBitMaskingAssuming32bitPts :: Spec
testTransformBitMaskingAssuming32bitPts =
  describe "transformBitMaskingAssuming32bitPts" $ do
    shouldLeaveUnresolved "leaves BitMaskingAssuming32bitPts unresolved"
      "void f() { int *p = 0; int x = p & 0xFFFFFFFF; }"
      analyzeConstantsLiteralsIssues
      transformConstantsLiteralsIssues
      [BitMaskingAssuming32bitPts]

testTransformHardCodedAddressValues :: Spec
testTransformHardCodedAddressValues =
  describe "transformHardCodedAddressValues" $ do
    shouldLeaveUnresolved "leaves HardCodedAddressValues unresolved"
      "void f() { int *p = (int *)0xDEADBEEF; }"
      analyzeConstantsLiteralsIssues
      transformConstantsLiteralsIssues
      [HardCodedAddressValues]

testTransformConstantsUsedForSizeCalcs :: Spec
testTransformConstantsUsedForSizeCalcs =
  describe "transformConstantsUsedForSizeCalcs" $ do
    shouldLeaveUnresolved "leaves ConstantsUsedForSizeCalcs unresolved"
      "void f() { unsigned int n; n = 42; }"
      analyzeConstantsLiteralsIssues
      transformConstantsLiteralsIssues
      [ConstantsUsedForSizeCalcs]
