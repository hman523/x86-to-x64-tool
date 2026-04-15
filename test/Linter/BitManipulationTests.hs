module Linter.BitManipulationTests where

import Test.Hspec
import Linter.LinterTestsUtils
import Linter.BitManipulation
import Analysis.BitManipulation (analyzeBitManipulationIssues)
import Analysis.IssueTypes

bitManipulationLintSpec :: Spec
bitManipulationLintSpec = describe "BitManipulation Linting" $ do
  testLintPackingPtrsWithFlagsInInt
  testLintBitShiftsOnPtr
  testLintExtractingPtrBitsIn32BitVar

testLintPackingPtrsWithFlagsInInt :: Spec
testLintPackingPtrsWithFlagsInInt =
  describe "lintPackingPtrsWithFlagsInInt" $ do
    shouldLeaveUnresolved "leaves PackingPtrsWithFlagsInInt unresolved"
      "void f() { int *p = 0; int x = (int)(p | 1); }"
      analyzeBitManipulationIssues
      lintBitManipulationIssues
      [PackingPtrsWithFlagsInInt]

testLintBitShiftsOnPtr :: Spec
testLintBitShiftsOnPtr =
  describe "lintBitShiftsOnPtr" $ do
    shouldLeaveUnresolved "leaves BitShiftsOnPtr unresolved"
      "void f() { int *p = 0; int *q = p >> 1; }"
      analyzeBitManipulationIssues
      lintBitManipulationIssues
      [BitShiftsOnPtr]

testLintExtractingPtrBitsIn32BitVar :: Spec
testLintExtractingPtrBitsIn32BitVar =
  describe "lintExtractingPtrBitsIn32BitVar" $ do
    shouldLeaveUnresolved "leaves ExtractingPtrBitsIn32BitVar unresolved"
      "void f() { int *p = 0; int x = (int)(p >> 1); }"
      analyzeBitManipulationIssues
      lintBitManipulationIssues
      [BitShiftsOnPtr, ExtractingPtrBitsIn32BitVar]
