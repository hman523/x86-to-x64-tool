module Transformation.ConstantsLiteralsTests where

import Test.Hspec
import Transformation.TransformationTestsUtils
import Transformation.ConstantsLiterals
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
    it "TODO: implement transformation tests" pending

testTransformBitMaskingAssuming32bitPts :: Spec
testTransformBitMaskingAssuming32bitPts =
  describe "transformBitMaskingAssuming32bitPts" $ do
    it "TODO: implement transformation tests" pending

testTransformHardCodedAddressValues :: Spec
testTransformHardCodedAddressValues =
  describe "transformHardCodedAddressValues" $ do
    it "TODO: implement transformation tests" pending

testTransformConstantsUsedForSizeCalcs :: Spec
testTransformConstantsUsedForSizeCalcs =
  describe "transformConstantsUsedForSizeCalcs" $ do
    it "TODO: implement transformation tests" pending
