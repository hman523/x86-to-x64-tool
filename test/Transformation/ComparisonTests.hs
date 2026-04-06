module Transformation.ComparisonTests where

import Test.Hspec
import Transformation.TransformationTestsUtils
import Transformation.Comparison
import Analysis.Comparison (analyzeComparisonIssues)
import Analysis.IssueTypes

comparisonTransformSpec :: Spec
comparisonTransformSpec = describe "Comparison Transformations" $ do
  testTransformLoopCounterAsIntWhenIteratingOverPtrArrays
  testTransformPtrComparisonWithIntConsts
  testTransformUsingIntForFileOffsets

testTransformLoopCounterAsIntWhenIteratingOverPtrArrays :: Spec
testTransformLoopCounterAsIntWhenIteratingOverPtrArrays =
  describe "transformLoopCounterAsIntWhenIteratingOverPtrArrays" $ do
    shouldTransformTo "rewrites int loop counter to ptrdiff_t"
      "void f() { int *p = 0; int *q = 0; for (int i = 0; i < (p - q); i++) { } }"
      analyzeComparisonIssues
      transformComparisonIssues
      "ptrdiff_t"

testTransformPtrComparisonWithIntConsts :: Spec
testTransformPtrComparisonWithIntConsts =
  describe "transformPtrComparisonWithIntConsts" $ do
    shouldLeaveUnresolved "leaves PtrComparisonWithIntConsts unresolved"
      "void f() { int *p = 0; if (p > 1) { } }"
      analyzeComparisonIssues
      transformComparisonIssues
      [PtrComparisonWithIntConsts]

testTransformUsingIntForFileOffsets :: Spec
testTransformUsingIntForFileOffsets =
  describe "transformUsingIntForFileOffsets" $ do
    shouldTransformTo "rewrites int offset variable passed to fseek to off_t"
      "void f() { int offset = 0; fseek(0, offset, 0); }"
      analyzeComparisonIssues
      transformComparisonIssues
      "off_t"
