module Transformation.BitManipulationTests where

import Test.Hspec
import Transformation.TransformationTestsUtils
import Transformation.BitManipulation
import Analysis.IssueTypes

bitManipulationTransformSpec :: Spec
bitManipulationTransformSpec = describe "BitManipulation Transformations" $ do
  testTransformPackingPtrsWithFlagsInInt
  testTransformBitShiftsOnPtr
  testTransformExtractingPtrBitsIn32BitVar

testTransformPackingPtrsWithFlagsInInt :: Spec
testTransformPackingPtrsWithFlagsInInt =
  describe "transformPackingPtrsWithFlagsInInt" $ do
    it "TODO: implement transformation tests" pending

testTransformBitShiftsOnPtr :: Spec
testTransformBitShiftsOnPtr =
  describe "transformBitShiftsOnPtr" $ do
    it "TODO: implement transformation tests" pending

testTransformExtractingPtrBitsIn32BitVar :: Spec
testTransformExtractingPtrBitsIn32BitVar =
  describe "transformExtractingPtrBitsIn32BitVar" $ do
    it "TODO: implement transformation tests" pending
