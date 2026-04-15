module Linter.PointerMathTests where

import Test.Hspec
import Linter.LinterTestsUtils
import Linter.PointerMath
import Analysis.PointerMath (analyzePointerMathIssues)
import Analysis.IssueTypes

pointerMathLintSpec :: Spec
pointerMathLintSpec = describe "PointerMath Linting" $ do
  testLintPtrDiffStoredAs32bit
  testLintPointerAddOverflow
  testLintPtrSubUnderflow
  testLintArrayIndexingIntInArrayOver2tothe31size

testLintPtrDiffStoredAs32bit :: Spec
testLintPtrDiffStoredAs32bit =
  describe "lintPtrDiffStoredAs32bit" $ do
    shouldLintTo "rewrites int variable storing ptr diff to ptrdiff_t"
      "void f() { int *p = 0; int *q = 0; int d; d = p - q; }"
      analyzePointerMathIssues
      lintPointerMathIssues
      "ptrdiff_t"

testLintPointerAddOverflow :: Spec
testLintPointerAddOverflow =
  describe "lintPointerAddOverflow" $ do
    shouldLeaveUnresolved "leaves PointerAddOverflow unresolved"
      "void f() { int *p = 0; int n = 1; int *q = p + n; }"
      analyzePointerMathIssues
      lintPointerMathIssues
      [PointerAddOverflow]

testLintPtrSubUnderflow :: Spec
testLintPtrSubUnderflow =
  describe "lintPtrSubUnderflow" $ do
    shouldLeaveUnresolved "leaves PtrSubUnderflow unresolved"
      "void f() { int *p = 0; unsigned int n = 1; int *q = p - n; }"
      analyzePointerMathIssues
      lintPointerMathIssues
      [PtrSubUnderflow]

testLintArrayIndexingIntInArrayOver2tothe31size :: Spec
testLintArrayIndexingIntInArrayOver2tothe31size =
  describe "lintArrayIndexingIntInArrayOver2tothe31size" $ do
    shouldLintTo "rewrites int index variable to ptrdiff_t"
      "void f() { int arr[10]; int i = 0; int x = arr[i]; }"
      analyzePointerMathIssues
      lintPointerMathIssues
      "ptrdiff_t"
