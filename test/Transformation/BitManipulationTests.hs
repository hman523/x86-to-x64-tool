module Transformation.BitManipulationTests where

import Test.Hspec
import Transformation.TransformationTestsUtils
import Transformation.BitManipulation
import Analysis.BitManipulation (analyzeBitManipulationIssues)
import Analysis.IssueTypes

bitManipulationTransformSpec :: Spec
bitManipulationTransformSpec = describe "BitManipulation Transformations" $ do
  testTransformPackingPtrsWithFlagsInInt
  testTransformBitShiftsOnPtr
  testTransformExtractingPtrBitsIn32BitVar

testTransformPackingPtrsWithFlagsInInt :: Spec
testTransformPackingPtrsWithFlagsInInt =
  describe "transformPackingPtrsWithFlagsInInt" $ do
    shouldLeaveUnresolved "leaves PackingPtrsWithFlagsInInt unresolved"
      "void f() { int *p = 0; int x = (int)(p | 1); }"
      analyzeBitManipulationIssues
      transformBitManipulationIssues
      [PackingPtrsWithFlagsInInt]

testTransformBitShiftsOnPtr :: Spec
testTransformBitShiftsOnPtr =
  describe "transformBitShiftsOnPtr" $ do
    shouldLeaveUnresolved "leaves BitShiftsOnPtr unresolved"
      "void f() { int *p = 0; int *q = p >> 1; }"
      analyzeBitManipulationIssues
      transformBitManipulationIssues
      [BitShiftsOnPtr]

testTransformExtractingPtrBitsIn32BitVar :: Spec
testTransformExtractingPtrBitsIn32BitVar =
  describe "transformExtractingPtrBitsIn32BitVar" $ do
    shouldLeaveUnresolved "leaves ExtractingPtrBitsIn32BitVar unresolved"
      "void f() { int *p = 0; int x = (int)(p >> 1); }"
      analyzeBitManipulationIssues
      transformBitManipulationIssues
      [BitShiftsOnPtr, ExtractingPtrBitsIn32BitVar]
