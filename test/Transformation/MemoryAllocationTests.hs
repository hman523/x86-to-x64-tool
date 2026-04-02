module Transformation.MemoryAllocationTests where

import Test.Hspec
import Transformation.TransformationTestsUtils
import Transformation.MemoryAllocation
import Analysis.IssueTypes

memoryAllocationTransformSpec :: Spec
memoryAllocationTransformSpec = describe "MemoryAllocation Transformations" $ do
  testTransformAllocationSizeCalcsMayOverflow
  testTransformMallocWithoutOverflowChecking
  testTransformUsingIntToStoreAllocationSizes

testTransformAllocationSizeCalcsMayOverflow :: Spec
testTransformAllocationSizeCalcsMayOverflow =
  describe "transformAllocationSizeCalcsMayOverflow" $ do
    it "TODO: implement transformation tests" pending

testTransformMallocWithoutOverflowChecking :: Spec
testTransformMallocWithoutOverflowChecking =
  describe "transformMallocWithoutOverflowChecking" $ do
    it "TODO: implement transformation tests" pending

testTransformUsingIntToStoreAllocationSizes :: Spec
testTransformUsingIntToStoreAllocationSizes =
  describe "transformUsingIntToStoreAllocationSizes" $ do
    it "TODO: implement transformation tests" pending
