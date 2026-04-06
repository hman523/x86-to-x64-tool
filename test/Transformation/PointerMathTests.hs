module Transformation.PointerMathTests where

import Test.Hspec
import Transformation.TransformationTestsUtils
import Transformation.PointerMath
import Analysis.PointerMath (analyzePointerMathIssues)
import Analysis.IssueTypes

pointerMathTransformSpec :: Spec
pointerMathTransformSpec = describe "PointerMath Transformations" $ do
  testTransformPtrDiffStoredAs32bit
  testTransformPointerAddOverflow
  testTransformPtrSubUnderflow
  testTransformArrayIndexingIntInArrayOver2tothe31size

testTransformPtrDiffStoredAs32bit :: Spec
testTransformPtrDiffStoredAs32bit =
  describe "transformPtrDiffStoredAs32bit" $ do
    shouldTransformTo "rewrites int variable storing ptr diff to ptrdiff_t"
      "void f() { int *p = 0; int *q = 0; int d; d = p - q; }"
      analyzePointerMathIssues
      transformPointerMathIssues
      "ptrdiff_t"

testTransformPointerAddOverflow :: Spec
testTransformPointerAddOverflow =
  describe "transformPointerAddOverflow" $ do
    shouldLeaveUnresolved "leaves PointerAddOverflow unresolved"
      "void f() { int *p = 0; int n = 1; int *q = p + n; }"
      analyzePointerMathIssues
      transformPointerMathIssues
      [PointerAddOverflow]

testTransformPtrSubUnderflow :: Spec
testTransformPtrSubUnderflow =
  describe "transformPtrSubUnderflow" $ do
    shouldLeaveUnresolved "leaves PtrSubUnderflow unresolved"
      "void f() { int *p = 0; unsigned int n = 1; int *q = p - n; }"
      analyzePointerMathIssues
      transformPointerMathIssues
      [PtrSubUnderflow]

testTransformArrayIndexingIntInArrayOver2tothe31size :: Spec
testTransformArrayIndexingIntInArrayOver2tothe31size =
  describe "transformArrayIndexingIntInArrayOver2tothe31size" $ do
    shouldTransformTo "rewrites int index variable to ptrdiff_t"
      "void f() { int arr[10]; int i = 0; int x = arr[i]; }"
      analyzePointerMathIssues
      transformPointerMathIssues
      "ptrdiff_t"
